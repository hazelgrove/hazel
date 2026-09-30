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

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  /* module rows from the top level down to the zoomed module; [] is
     the whole program */
  zoom: list(Id.t),
  pins: list(pin),
  /* the current level's whole is shown even though pins exist */
  parked: bool,
};

let init: t = {
  zoom: [],
  pins: [],
  parked: false,
};

let zoom_root = (v: t): option(Id.t) =>
  switch (List.rev(v.zoom)) {
  | [m, ..._] => Some(m)
  | [] => None
  };

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
    List.fold_left(
      (found, m) =>
        switch (found) {
        | Some(_) => found
        | None =>
          OutlineTree.kind_of(m, term) == Some(OutlineTree.KModule)
            ? OutlineTree.trail_of(m, term) : None
        },
      None,
      List.rev(v.zoom),
    )
    |> Option.value(~default=[]);
  {
    ...v,
    zoom,
    pins: List.filter((p: pin) => exists(p.p_id), v.pins),
  };
};

let slot_of_cell = (e: ScratchCell.t): slot =>
  e.e_inner
    ? Members(e.e_id)
    : Pin({
        p_id: e.e_id,
        p_run: e.e_run,
      });

let slot_id =
  fun
  | Pin(p) => p.p_id
  | Members(m) => m;

/* the program with exactly [shown] open. Opens go first, so replacing
   the last cell doesn't join; one blocked by an open cell retries after
   the closes. Pins that can't open drop; from one editor, the caret
   moves into the cell holding it. */
let realize = (~info_map, ~term, v: t, p: Program.t): (t, Program.t) => {
  let v = normalize(~term, v);
  let want = shown(~term, v);
  let current =
    switch (p) {
    | Whole(_) => []
    | Divided(d) => List.map(slot_of_cell, Divided.cells(d))
    };
  let to_open = List.filter(w => !List.mem(w, current), want);
  let to_close = List.filter(c => !List.mem(c, want), current);
  let open_one = (p: Program.t, w: slot): option(Program.t) => {
    let id = slot_id(w);
    let sym = sym_of(id, term);
    let (run, inner) =
      switch (w) {
      | Pin(p) => (p.p_run, false)
      | Members(_) => (false, true)
      };
    switch (p) {
    | Whole(e) =>
      (
        run
          ? Divided.split_run(~info_map, e, id)
          : Divided.split(~info_map, ~sym?, ~inner, e, id)
      )
      |> Option.map(d => Program.Divided(d))
    | Divided(d) =>
      (
        run
          ? Divided.open_run(~info_map, ~term, id, d)
          : Divided.open_(~info_map, ~term, ~sym?, ~inner, id, d)
      )
      |> Option.map(d => Program.Divided(d))
    };
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
        let (last, rest) = List.partition(c => slot_id(c) == a, to_close);
        rest @ last;
      | None => to_close
      }
    | Whole(_) => to_close
    };
  let p =
    List.fold_left(
      (p: Program.t, c: slot) =>
        switch (p) {
        | Divided(d) => Program.of_close(Divided.close(slot_id(c), d))
        | Whole(_) => p
        },
      p,
      to_close,
    );
  let (p, failed) = open_all(p, blocked);
  let p =
    switch (whole_caret, p) {
    | (Some(a), Divided(d)) => Program.Divided(Divided.place_caret(a, d))
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

let pinned = (id: Id.t, v: t): bool =>
  List.exists((p: pin) => p.p_id == id, v.pins);

/* zoom out until [id] is in view */
let reveal = (~term, id: Id.t, v: t): t => {
  let rec go = (zoom: list(Id.t)) =>
    switch (List.rev(zoom)) {
    | [] => []
    | [m, ...rest] =>
      inside(~term, Some(m), id) ? zoom : go(List.rev(rest))
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
  zoom:
    switch (List.rev(v.zoom)) {
    | [] => []
    | [_, ...rest] => List.rev(rest)
    },
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
