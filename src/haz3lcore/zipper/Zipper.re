open Sexplib.Std;
open Util;
open OptUtil.Syntax;

module Caret = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Outer
    | Inner(int, int);

  let decrement: t => t =
    fun
    | Outer
    | Inner(_, 0) => Outer
    | Inner(d, c) => Inner(d, c - 1);

  let offset: t => int =
    fun
    | Outer => 0
    | Inner(_, c) => c + 1;
};

// assuming single backpack, shards may appear in selection, backpack, or siblings
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  selection: Selection.t,
  // backpack: Backpack.t,
  relatives: Relatives.t,
  caret: Caret.t,
  // col_target: int,
};

let init: int => t =
  id => {
    selection: {
      focus: Left,
      content: [],
    },
    // backpack: [],
    relatives: {
      siblings: ([], [Grout({id, sort: Exp, shape: Convex})]),
      ancestors: [],
    },
    caret: Outer,
    // col_target: 0,
  };

let next_blank = id => {
  (id + 1, init(id));
};

[@deriving (show({with_path: false}), sexp, yojson)]
type state = (t, IdGen.state);

[@deriving (show({with_path: false}), sexp, yojson)]
type chunkiness =
  | ByChar
  | MonoByChar
  | ByToken;

[@deriving (show({with_path: false}), sexp, yojson)]
type planar =
  | Up
  | Down
  | Left(chunkiness)
  | Right(chunkiness);

let from_plane: planar => Direction.t =
  fun
  | Left(_) => Left
  | Right(_) => Right
  | Up => Left
  | Down => Right;

let update_caret = (f: Caret.t => Caret.t, z: t): t => {
  ...z,
  caret: f(z.caret),
};
let set_caret = (caret: Caret.t): (t => t) => update_caret(_ => caret);

let update_relatives = (f: Relatives.t => Relatives.t, z: t): t => {
  ...z,
  relatives: f(z.relatives),
};

let update_siblings: (Siblings.t => Siblings.t, t) => t =
  f => update_relatives(rs => {...rs, siblings: f(rs.siblings)});

let parent = (z: t): option(Piece.t) =>
  Relatives.parent(~sel=z.selection.content, z.relatives);

let zip = (z: t): Segment.t =>
  Relatives.zip(~sel=z.selection.content, z.relatives);

let sibs_with_sel =
    (
      {
        selection: {content, focus},
        relatives: {siblings: (l_sibs, r_sibs), _},
        _,
      }: t,
    )
    : Siblings.t =>
  switch (focus) {
  | Left => (l_sibs, content @ r_sibs)
  | Right => (l_sibs @ content, r_sibs)
  };

// let pop_backpack = (z: t) =>
//   Backpack.pop(Relatives.local_incomplete_tiles(z.relatives), z.backpack);

let neighbor_monotiles: Siblings.t => (option(Token.t), option(Token.t)) =
  siblings =>
    switch (Siblings.neighbors(siblings)) {
    | (Some(l), Some(r)) => (Piece.monotile(l), Piece.monotile(r))
    | (Some(l), None) => (Piece.monotile(l), None)
    | (None, Some(r)) => (None, Piece.monotile(r))
    | (None, None) => (None, None)
    };

let remold_regrout = (d: Direction.t, z: t): IdGen.t(t) => {
  assert(Selection.is_empty(z.selection));
  open IdGen.Syntax;
  let+ relatives = Relatives.regrout(d, Relatives.remold(z.relatives));
  {...z, relatives};
};

let unselect = (z: t): t => {
  let relatives =
    z.relatives
    |> Relatives.prepend(z.selection.focus, z.selection.content)
    |> Relatives.reassemble;
  let selection = Selection.clear(z.selection);
  {...z, selection, relatives};
};
let unselect_and_zip = (z: t): Segment.t => z |> unselect |> zip;

let update_selection = (selection: Selection.t, z: t): (Selection.t, t) => {
  let old = z.selection;
  // used to be necessary to unselect when selection update
  // included remold/regrout, now no longer necessary if needs
  // to be changed but keeping for now to minimize change
  let z = unselect({...z, selection});
  (old, z);
};

let put_selection = (sel: Selection.t, z: t): t =>
  snd(update_selection(sel, z));

let grow_selection = (z: t): option(t) => {
  let+ (p, relatives) = Relatives.pop(z.selection.focus, z.relatives);
  let selection = Selection.push(p, z.selection);
  {...z, selection, relatives};
};

// toggles focus and grows if selection is empty
let shrink_selection = (z: t): option(t) => {
  switch (Selection.pop(z.selection)) {
  | None =>
    let selection = Selection.toggle_focus(z.selection);
    grow_selection({...z, selection});
  | Some((p, selection)) =>
    let relatives =
      z.relatives
      |> Relatives.push(selection.focus, p)
      |> Relatives.reassemble;
    Some({...z, selection, relatives});
  };
};

let directional_unselect = (d: Direction.t, z: t): t => {
  let selection = {...z.selection, focus: Direction.toggle(d)};
  unselect({...z, selection});
};

let move = (d: Direction.t, z: t, id_gen): option((t, IdGen.state)) =>
  if (Selection.is_empty(z.selection)) {
    // let balanced = !Backpack.is_balanced(z.backpack);
    let b = Direction.toggle(d);
    let sort = Ancestors.sort(z.relatives.ancestors);
    let+ (p, relatives) = Relatives.pop(d, z.relatives);
    let (relatives, id_gen) =
      switch (p) {
      | Whitespace(_)
      | Grout(_) => (relatives, id_gen)
      | Tile(t) =>
        let (trim, relatives) = Relatives.pop_trim(b, relatives);
        let (rel_l, rel_r) = Relatives.nibs(relatives);
        let (t_l, t_r) = Tile.nibs(t);
        let nibs =
          switch (d) {
          | Left => (t_r, rel_r)
          | Right => (rel_l, t_l)
          };
        let (trim, id_gen) =
          Segment.Trim.is_linted(nibs, trim, sort)
            ? (trim, id_gen)
            : {
              let ((_, trim), id_gen) =
                Segment.Trim.regrout(nibs, trim, sort, id_gen);
              (trim, id_gen);
            };
        // let relatives =
        //   relatives
        //   |> Relatives.push(Direction.toggle(d), p)
        //   |> Relatives.reassemble;
        let pushed = Relatives.push_trim(b, trim, relatives);
        (pushed, id_gen);
      };
    let relatives = relatives |> Relatives.push(b, p) |> Relatives.reassemble;
    ({...z, relatives}, id_gen);
  } else {
    Some((directional_unselect(d, z), id_gen));
  };

let select = (d: Direction.t, z: t): option(t) =>
  d == z.selection.focus ? grow_selection(z) : shrink_selection(z);

let (let$) = (z, o, f) =>
  switch (o) {
  | None => z
  | Some(x) => f(x)
  };

let merge = (z: t): t => {
  let (let$) = (let$)(z);
  let$ (l, rs) = Relatives.pop(Left, z.relatives);
  let$ (r, rs) = Relatives.pop(Right, rs);
  let$ p = Piece.merge(l, r);
  // push Right since caret indexes into right token
  let relatives = Relatives.push(Right, p, rs);
  let caret = z.caret + Piece.length(l);
  {...z, caret, relatives};
};

let (let&) = (z, o, f) =>
  switch (o) {
  | None => IdGen.return(z)
  | Some(x) => f(x)
  };

let split = (z: t): IdGen.t(t) => {
  open IdGen.Syntax;
  let (let&) = (let&)(z);
  let& (p, rs) = Relatives.pop(z.relatives);
  let* split = Piece.split(z.caret, p);
  let& (l, r) = split;
  let relatives = rs |> Relatives.push(Left, l) |> Relatives.push(Right, r);
  let caret = z.caret - Piece.length(l);
  return({...z, caret, relatives});
};

// let pick_up = (z: t): t => {
//   let (selected, z) = update_selection(Selection.empty, z);
//   let selection =
//     selected.content
//     |> Segment.trim_grout_around_whitespace(Left)
//     |> Segment.trim_grout_around_whitespace(Right)
//     |> Selection.mk(selected.focus);
//   Segment.tiles(selection.content)
//   |> List.map((t: Tile.t) => t.id)
//   |> Effect.s_touch;
//   let backpack = Backpack.push(selection, z.backpack);
//   {...z, backpack};
// };

let delete_selection = (d: Direction.t, z: t): IdGen.t(t) => {
  open IdGen.Syntax;
  let (selected, z) = update_selection(Selection.empty, z);
  switch (
    Segment.edge_shape_of(Left, selected.content),
    Segment.edge_shape_of(Right, selected.content),
  ) {
  | (Some(l), Some(r)) when !Nib.Shape.fits(l, r) =>
    // use any sort to insert grout anchor with assumption
    // that it will be regrouted to proper sort
    let+ g = Grout.mk_fits_shape(l, Any);
    let b = Direction.toggle(d);
    {...z, relatives: Relatives.push(b, Grout(g), z.relatives)};
  | _ => return(z)
  };
};

let pop = (d, z) => {
  open OptUtil.Syntax;
  let+ (p, relatives) = Relatives.pop(d, z.relatives);
  (p, {...z, relatives});
};
let push = (d, p) => update_relatives(Relatives.push(d, p));

let regrold = (z: t): IdGen.t => {
  open IdGen.Syntax;
  let+ relatives = Relatives.regrold(z.relatives);
  {...z, relatives};
};
let reassemble = update_relatives(Relatives.reassemble);

let delete = (d: Direction.t, z: t): IdGen.t(t) =>
  if (!Segment.is_empty(selected.content)) {
    delete_selection(d, z);
  } else {
    open IdGen.Syntax;
    let (let&) = (let&)(z);
    let* z = split(z);
    let& (p, z) = pop(d, z);
    return(
      switch (p) {
      | Whitespace(_) => z
      | Grout(_) => z |> push(Direction.toggle(d), p)
      | Tile(t) =>
        let s = Tile.hd(t);
        let token = Token.(d == Left ? rm_last : rm_first)(s.form.token);
        let t = Tile.mono({...s, token});
        z |> (Token.is_empty(token) ? Fun.id : push(d, Tile(t))) |> merge;
      },
    );
  };

let insert = (onto: Direction.t, token: Token.t, z: t): IdGen.t(t) => {
  open IdGen.Syntax;
  let* z = delete_selection(Left, z);
  let* id = IdGen.fresh;
  let t = Tile.mono({
            id,
            form: {
              token,
              mold: Mold.null,
            },
          });
  let* z = split(z);
  z |> push(onto, Tile(t)) |> merge;
};

let representative_piece = (z: t): option((Piece.t, Direction.t)) => {
  /* The piece to the left of the caret, or if none exists, the piece to the right */
  switch (Siblings.neighbors(sibs_with_sel(z))) {
  | (Some(l), _) => Some((l, Left))
  | (_, Some(r)) => Some((r, Right))
  | _ => None
  };
};

let caret_direction = (z: t): option(Direction.t) =>
  /* Direction the caret is facing in */
  switch (z.caret) {
  | Inner(_) => None
  | Outer =>
    switch (Siblings.neighbors(sibs_with_sel(z))) {
    | (Some(l), Some(r))
        when Piece.is_whitespace(l) && Piece.is_whitespace(r) =>
      None
    | _ => Siblings.direction_between(sibs_with_sel(z))
    }
  };

let base_point = (measured: Measured.t, z: t): Measured.Point.t => {
  switch (representative_piece(z)) {
  | Some((p, d)) =>
    let seg = Piece.disassemble(p);
    switch (d) {
    | Left =>
      let p = ListUtil.last(seg);
      let m = Measured.find_p(p, measured);
      m.last;
    | Right =>
      let p = List.hd(seg);
      let m = Measured.find_p(p, measured);
      m.origin;
    };
  | None => {row: 0, col: 0}
  };
};
let caret_point = (measured, z: t): Measured.Point.t => {
  let Measured.Point.{row, col} = base_point(measured, z);
  {row, col: col + Caret.offset(z.caret)};
};

let serialize = (z: t): string => {
  sexp_of_t(z) |> Sexplib.Sexp.to_string;
};

let deserialize = (data: string): t => {
  Sexplib.Sexp.of_string(data) |> t_of_sexp;
};
