open Util;

/* What a slide shows of its program: pinned items, the module it is
   zoomed into, and whether the pins are parked behind the whole.
   [realize] makes the program's open cells match. Zoom is only a view:
   the program, its evaluation and its scope are unchanged. */

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

/* [id] lies under [root] (anything does at the program root) */
let inside = (~term, root: option(Id.t), id: Id.t): bool =>
  switch (root) {
  | None => true
  | Some(m) => List.mem(id, OutlineTree.descendant_ids(m, term))
  };

let visible = (~term, v: t): list(pin) =>
  List.filter((p: pin) => inside(~term, zoom_root(v), p.p_id), v.pins);

/* the cells to show: the visible pins, or the level's whole: one
   editor at the program root, the module as one cell below it */
let shown = (~term, v: t): list(pin) =>
  switch (v.parked ? [] : visible(~term, v)) {
  | [] =>
    switch (zoom_root(v)) {
    | None => []
    | Some(m) => [
        {
          p_id: m,
          p_run: false,
        },
      ]
    }
  | pins => pins
  };

/* the zoomed module's own cell is on screen (its header band is the
   breadcrumb's job) */
let showing_zoom_cell = (~term, v: t): bool =>
  switch (zoom_root(v), shown(~term, v)) {
  | (Some(m), [p]) => p.p_id == m
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

let pin_of_cell = (e: ScratchCell.t): pin => {
  p_id: e.e_id,
  p_run: e.e_run,
};

/* the program with exactly [shown] open. What collides with an open
   cell (members of the zoomed module's cell) opens after the closes;
   the rest opens first, so replacing the last cell doesn't join the
   whole program. Pins that can't open (unfinished items) drop. Going
   from one editor to cells, the caret moves into the cell holding it. */
let realize = (~info_map, ~term, v: t, p: Program.t): (t, Program.t) => {
  let v = normalize(~term, v);
  let want = shown(~term, v);
  let current =
    switch (p) {
    | Whole(_) => []
    | Divided(d) => List.map(pin_of_cell, Divided.cells(d))
    };
  let to_open = List.filter(w => !List.mem(w, current), want);
  let to_close = List.filter(c => !List.mem(c, want), current);
  let open_one = (p: Program.t, w: pin): option(Program.t) => {
    let sym = sym_of(w.p_id, term);
    switch (p) {
    | Whole(e) =>
      (
        w.p_run
          ? Divided.split_run(~info_map, e, w.p_id)
          : Divided.split(~info_map, ~sym?, e, w.p_id)
      )
      |> Option.map(d => Program.Divided(d))
    | Divided(d) =>
      (
        w.p_run
          ? Divided.open_run(~info_map, ~term, w.p_id, d)
          : Divided.open_(~info_map, ~term, ~sym?, w.p_id, d)
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
  let p =
    List.fold_left(
      (p: Program.t, c: pin) =>
        switch (p) {
        | Divided(d) => Program.of_close(Divided.close(c.p_id, d))
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
      pins: List.filter(q => !List.mem(q, failed), v.pins),
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
