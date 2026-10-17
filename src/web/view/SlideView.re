open Util;

/* What a slide shows of its program: pins, the zoomed module, and
   whether the pins are parked behind the whole; [realize] opens cells
   to match. Zoom is only a view: evaluation and scope are unchanged. */

[@deriving (show({with_path: false}), sexp, yojson)]
type pin = {
  p_id: Id.t,
  /* one cell for the whole test run starting at [p_id] */
  p_run: bool,
};

/* The outline's two modes, each with pins of its own. Dev (the
   architecture) shows what was picked, nothing to start; UX (the art
   made in it) shows the livelits' GUIs to start: every row whose
   definition is a livelit's use. Either can show anything: a row
   pinned in UX mode is pinned there only. */
[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Dev
  | UX;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  /* module rows from the top level down to the zoomed module; [] is
     the whole program */
  zoom: list(Id.t),
  /* the current mode's pins */
  pins: list(pin),
  /* the current level's whole is shown even though pins exist */
  parked: bool,
  mode,
  /* the other mode's pins and parked, kept while this one shows; UX's
     are None until first shown or changed (then: its livelit rows) */
  away_pins: option(list(pin)),
  away_parked: bool,
};

let init: t = {
  zoom: [],
  pins: [],
  parked: false,
  mode: Dev,
  away_pins: None,
  away_parked: false,
};

let zoom_root = (v: t): option(Id.t) => ListUtil.last_opt(v.zoom);

/* the header symbol of a headerless cell, from the outline's kind */
let sym_of = (fid: Id.t, term: Language.Exp.t): option(string) =>
  switch (OutlineTree.kind_of(fid, term)) {
  | Some(OutlineTree.KTrail) => Some({js|⇒|js})
  | Some(OutlineTree.KTest)
  | Some(OutlineTree.KStmt) => Some({js|;|js})
  | _ => None
  };

let inside = (~term, root: option(Id.t), id: Id.t): bool =>
  switch (root) {
  | None => true
  | Some(m) => List.mem(id, OutlineTree.descendant_ids(m, term))
  };

let visible = (~term, v: t): list(pin) =>
  List.filter((p: pin) => inside(~term, zoom_root(v), p.p_id), v.pins);

/* what a cell shows: a pin, or a zoomed module's members */
type slot =
  | Pin(pin)
  | Members(Id.t);

/* the cells to show: the visible pins, or the level's whole: one
   editor at the program root, the module's members below it */
let shown = (~term, v: t): list(slot) =>
  switch (v.parked ? [] : visible(~term, v)) {
  | [] =>
    switch (zoom_root(v)) {
    | None => []
    | Some(m) => [Members(m)]
    }
  | pins => List.map(p => Pin(p), pins)
  };

/* the zoomed module's members are on screen (the breadcrumb names it) */
let showing_zoom_cell = (~term, v: t): bool =>
  switch (shown(~term, v)) {
  | [Members(_)] => true
  | _ => false
  };

/* pins to items that are gone drop; the zoom follows its module
   through moves, or falls back to its deepest surviving ancestor */
let normalize = (~term, v: t): t => {
  let exists = id => OutlineTree.kind_of(id, term) != None;
  let zoom =
    List.find_map(
      m =>
        OutlineTree.kind_of(m, term) == Some(OutlineTree.KModule)
          ? OutlineTree.trail_of(m, term) : None,
      List.rev(v.zoom),
    )
    |> Option.value(~default=[]);
  {
    ...v,
    zoom,
    pins: List.filter((p: pin) => exists(p.p_id), v.pins),
    away_pins:
      Option.map(List.filter((p: pin) => exists(p.p_id)), v.away_pins),
  };
};

/* an open cell shows [w]: a module row's pin shows as its members, so
   one members cell serves the pin and the zoom alike */
let satisfies = (e: ScratchCell.t, w: slot): bool =>
  switch (w) {
  | Pin(p) => e.e_id == p.p_id && ScratchCell.is_run(e) == p.p_run
  | Members(m) => e.e_id == m && e.e_kind == Members
  };

let slot_id =
  fun
  | Pin(p) => p.p_id
  | Members(m) => m;

/* headerless cells follow an edited expression to its new root, and
   their pins with them, so normalize doesn't drop them */
let follow_headless = (~term, v: t, p: Program.t): (t, Program.t) =>
  switch (p) {
  | Whole(_) => (v, p)
  | Divided(d) =>
    let (rows, headless) = (
      OutlineTree.row_ids(term),
      OutlineTree.headless_row_ids(term),
    );
    switch (Divided.follow_headless(~rows, ~headless, d)) {
    | (_, []) => (v, p)
    | (d, moves) => (
        {
          ...v,
          pins:
            List.map(
              (pin: pin) =>
                switch (List.assoc_opt(pin.p_id, moves)) {
                | Some(id) => {
                    ...pin,
                    p_id: id,
                  }
                | None => pin
                },
              v.pins,
            ),
        },
        Divided(d),
      )
    };
  };

/* the program with exactly [shown] open. Opens go first, so replacing
   the last cell doesn't join; one blocked by an open cell retries after
   the closes. Pins that can't open drop; from one editor, the caret
   moves into the cell holding it. The focused cell keeps the focus while
   it stays open; else the last cell to open takes it. */
let realize = (~info_map, ~term, v: t, p: Program.t): (t, Program.t) => {
  let (v, p) = follow_headless(~term, v, p);
  let before = p;
  let v = normalize(~term, v);
  let want = shown(~term, v);
  let cells =
    switch (p) {
    | Whole(_) => []
    | Divided(d) => Divided.cells(d)
    };
  let to_open =
    List.filter(w => !List.exists(e => satisfies(e, w), cells), want);
  let to_close =
    List.filter_map(
      (e: ScratchCell.t) =>
        List.exists(satisfies(e), want) ? None : Some(e.e_id),
      cells,
    );
  let open_one = (p: Program.t, w: slot): option(Program.t) => {
    let id = slot_id(w);
    let d =
      switch (p) {
      | Whole(e) => Divided.of_whole(e)
      | Divided(d) => d
      };
    let sym = sym_of(id, term);
    let open_cell = (~inner) =>
      Divided.open_(~info_map, ~term, ~sym?, ~inner, id, d);
    (
      switch (w) {
      | Pin({p_run: true, _}) => Divided.open_run(~info_map, ~term, id, d)
      /* a module row opens as its members; `{}` has none to show */
      | Pin(_)
          when OutlineTree.kind_of(id, term) == Some(OutlineTree.KModule) =>
        switch (open_cell(~inner=true)) {
        | Some(d) => Some(d)
        | None => open_cell(~inner=false)
        }
      | Pin(_) => open_cell(~inner=false)
      | Members(_) => open_cell(~inner=true)
      }
    )
    |> Option.map(d => Program.Divided(d));
  };
  let open_all = (p, ws) =>
    List.fold_left(
      ((p, missed), w) =>
        switch (open_one(p, w)) {
        | Some(p) => (p, missed)
        | None => (p, missed @ [w])
        },
      (p, []),
      ws,
    );
  let whole_caret =
    switch (p) {
    | Whole(e) => Divided.anchor_of(e.editor.editor.state.zipper)
    | Divided(_) => None
    };
  let (p, blocked) = open_all(p, to_open);
  /* the active cell closes last, so the join keeps its caret */
  let to_close =
    switch (p) {
    | Divided(d) =>
      switch (Divided.active(d)) {
      | Some((a, _)) =>
        let (last, rest) = List.partition(id => id == a, to_close);
        rest @ last;
      | None => to_close
      }
    | Whole(_) => to_close
    };
  let p =
    List.fold_left(
      (p: Program.t, id) =>
        switch (p) {
        | Divided(d) => Program.of_close(Divided.close(id, d))
        | Whole(_) => p
        },
      p,
      to_close,
    );
  let (p, failed) = open_all(p, blocked);
  let p =
    switch (p, whole_caret, before) {
    | (Divided(d), Some(a), _) => Program.Divided(Divided.place_caret(a, d))
    | (Divided(d), None, Divided(prev)) =>
      Program.Divided(Divided.keep_focus(~prev, d))
    | _ => p
    };
  (
    {
      ...v,
      pins: List.filter(q => !List.mem(Pin(q), failed), v.pins),
    },
    p,
  );
};

/* view changes; the caller realizes */

/* zoom out until [id] is in view */
let reveal = (~term, id: Id.t, v: t): t => {
  let rec go = (zoom: list(Id.t)) =>
    switch (ListUtil.split_last_opt(zoom)) {
    | None => []
    | Some((rest, m)) => inside(~term, Some(m), id) ? zoom : go(rest)
    };
  {
    ...v,
    zoom: go(v.zoom),
  };
};

/* pin [id]; pins inside it fold into it, and it shows */
let pin = (~term, ~run=false, id: Id.t, v: t): t => {
  let v = reveal(~term, id, v);
  let desc = OutlineTree.descendant_ids(id, term);
  {
    ...v,
    pins:
      List.filter(
        (p: pin) => p.p_id != id && !List.mem(p.p_id, desc),
        v.pins,
      )
      @ [
        {
          p_id: id,
          p_run: run,
        },
      ],
    parked: false,
  };
};

let unpin = (id: Id.t, v: t): t => {
  ...v,
  pins: List.filter((p: pin) => p.p_id != id, v.pins),
};

/* drop the pins shown at this level */
let discard = (~term, v: t): t => {
  let root = zoom_root(v);
  {
    ...v,
    pins: List.filter((p: pin) => !inside(~term, root, p.p_id), v.pins),
    parked: false,
  };
};

let zoom_in = (~term, m: Id.t, v: t): t =>
  switch (OutlineTree.trail_of(m, term)) {
  | Some(trail)
      when OutlineTree.kind_of(m, term) == Some(OutlineTree.KModule) => {
      ...v,
      zoom: trail,
    }
  | _ => v
  };

let zoom_out = (v: t): t => {
  ...v,
  zoom: Option.fold(~none=[], ~some=fst, ListUtil.split_last_opt(v.zoom)),
};

/* zoom to [m], an entry of the current trail (None: the program) */
let zoom_to = (m: option(Id.t), v: t): t => {
  ...v,
  zoom:
    switch (m) {
    | None => []
    | Some(m) =>
      let rec upto = (acc, trail) =>
        switch (trail) {
        | [] => List.rev(acc)
        | [x, ...rest] =>
          x == m ? List.rev([x, ...acc]) : upto([x, ...acc], rest)
        };
      upto([], v.zoom);
    },
};

let park = (parked: bool, v: t): t => {
  ...v,
  parked,
};

/* UX mode's pins to start: a pin for each livelit row */
let ux_default = (~term): list(pin) =>
  List.map(
    id =>
      {
        p_id: id,
        p_run: false,
      },
    OutlineTree.livelit_row_ids(term),
  );

/* the other mode's pins, its default if never set */
let away = (~term, v: t): list(pin) =>
  switch (v.away_pins) {
  | Some(ps) => ps
  | None => v.mode == Dev ? ux_default(~term) : []
  };

/* show the other mode: its pins become the current ones, and these are
   kept for coming back */
let set_mode = (~term, mode: mode, v: t): t =>
  mode == v.mode
    ? v
    : {
      ...v,
      mode,
      pins: away(~term, v),
      parked: v.away_parked,
      away_pins: Some(v.pins),
      away_parked: v.parked,
    };

/* UX mode's pins, whichever mode shows */
let ux_pins = (~term, v: t): list(pin) =>
  v.mode == UX ? v.pins : away(~term, v);

/* UX mode's pins replaced, whichever mode shows */
let set_ux_pins = (pins: list(pin), v: t): t =>
  v.mode == UX
    ? {
      ...v,
      pins,
    }
    : {
      ...v,
      away_pins: Some(pins),
    };

/* pins for the rows whose labels, through [norm], are [labels]: each the
   first such row in outline order, at any depth; a label naming no row
   is dropped */
let rows_labeled =
    (~term, ~norm: string => string, labels: list(string)): list(pin) => {
  let rec find = (wanted, ns: list(OutlineTree.node)) =>
    List.fold_left(
      (found, n: OutlineTree.node) =>
        switch (found) {
        | Some(_) => found
        | None =>
          n.o_id != None && norm(n.o_label) == wanted
            ? n.o_id : find(wanted, n.o_children)
        },
      None,
      ns,
    );
  let outline = OutlineTree.of_term(term);
  labels
  |> List.filter_map(wanted => find(wanted, outline))
  |> List.fold_left(
       (seen, id) => List.mem(id, seen) ? seen : seen @ [id],
       [],
     )
  |> List.map(id =>
       {
         p_id: id,
         p_run: false,
       }
     );
};

let in_ux = (~term, id: Id.t, v: t): bool =>
  List.exists((p: pin) => p.p_id == id, ux_pins(~term, v));

/* show or hide [id] in UX mode, from either mode: in UX mode it is a
   pin like any other; in dev mode it changes the pins kept for UX */
let toggle_ux = (~term, id: Id.t, v: t): t => {
  let toggled = ps =>
    List.exists((p: pin) => p.p_id == id, ps)
      ? List.filter((p: pin) => p.p_id != id, ps)
      : ps
        @ [
          {
            p_id: id,
            p_run: false,
          },
        ];
  v.mode == UX
    ? {
      ...v,
      pins: toggled(v.pins),
    }
    : {
      ...v,
      away_pins: Some(toggled(away(~term, v))),
    };
};
