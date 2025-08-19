open Util;
open OptUtil.Syntax;
include ZipperBase;

let init: unit => t =
  () => {
    selection: Selection.mk([]),
    relatives: {
      siblings: (
        [],
        [
          Grout({
            id: Id.mk(),
            shape: Convex,
          }),
        ],
      ),
      ancestors: [],
    },
    caret: Outer,
  };

let next_blank = _ => Id.mk();

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type chunkiness =
  | ByChar
  | ByToken;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
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

let delete_parent = (z: t): t => {
  ...z,
  relatives: Relatives.delete_parent(z.relatives),
};

let zip = (z: t): Segment.t =>
  Relatives.zip(~sel=z.selection.content, z.relatives);

let unzip = (seg: Segment.t): t => {
  selection: Selection.mk([]),
  relatives: {
    siblings: (seg, []),
    ancestors: [],
  },
  caret: Outer,
};

let regrout = (d: Direction.t, z: t): t => {
  assert(Selection.is_empty(z.selection));
  let relatives = Relatives.regrout(d, z.relatives);
  {
    ...z,
    relatives,
  };
};

let remold = (z: t): t => {
  assert(Selection.is_empty(z.selection));
  {
    ...z,
    relatives: Relatives.remold(z.relatives),
  };
};

let remold_regrout = (d: Direction.t, z: t): t => z |> remold |> regrout(d);

let clear_unparsed_buffer = (z: t) =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) => {
      ...z,
      selection: Selection.empty,
    }
  | _ => z
  };

let unselect = (~erase_buffer=false, z: t): t => {
  /* NOTE(andrew): Erase buffer flag only applies to unparsed buffer,
   * that is, the buffer style that just contains a single flat token.
   * Erasing a buffer that contains arbitrary tiles would be more complex
   * as we can't just empty the selection without regrouting */
  let z = erase_buffer ? clear_unparsed_buffer(z) : z;
  let relatives =
    z.relatives
    |> Relatives.prepend(z.selection.focus, z.selection.content)
    |> Relatives.reassemble;
  let selection = Selection.empty;
  {
    ...z,
    selection,
    relatives,
  };
};

let destroy_selection: t => t =
  z =>
    unselect({
      ...z,
      selection: Selection.empty,
    });

let unselect_and_zip = (~erase_buffer=false, z: t): Segment.t =>
  z |> unselect(~erase_buffer) |> zip;

let replace_selection = (focus, segment, z: t): t => {
  ...z,
  selection: Selection.mk(~focus, segment),
};

let grow_selection = (z: t): option(t) => {
  let+ (p, relatives) = Relatives.pop(z.selection.focus, z.relatives);
  let selection = Selection.push(p, z.selection);
  {
    ...z,
    selection,
    relatives,
  };
};

// toggles focus and grows if selection is empty
let shrink_selection = (z: t): option(t) => {
  switch (Selection.pop(z.selection)) {
  | None =>
    let selection = Selection.toggle_focus(z.selection);
    grow_selection({
      ...z,
      selection,
    });
  | Some((p, selection)) =>
    let relatives =
      z.relatives
      |> Relatives.push(selection.focus, p)
      |> Relatives.reassemble;
    Some({
      ...z,
      selection,
      relatives,
    });
  };
};

let toggle_focus = (z: t): t => {
  ...z,
  selection: Selection.toggle_focus(z.selection),
};

let set_focus = (z: t, d: Direction.t): t => {
  let selection = {
    ...z.selection,
    focus: d,
  };
  {
    ...z,
    selection,
  };
};

let directional_unselect = (d: Direction.t, z: t): t => {
  let selection = {
    ...z.selection,
    focus: Direction.toggle(d),
  };
  unselect({
    ...z,
    selection,
  });
};

let unselect = (z: t): t =>
  z.selection.content == [] ? z : directional_unselect(z.selection.focus, z);

let move = (d: Direction.t, z: t): option(t) =>
  if (Selection.is_empty(z.selection)) {
    let+ (p, relatives) = Relatives.pop(d, z.relatives);
    let relatives =
      relatives
      |> Relatives.push(Direction.toggle(d), p)
      |> Relatives.reassemble;
    {
      ...z,
      relatives,
    };
  } else {
    Some(directional_unselect(d, z));
  };

let select = (d: Direction.t, z: t): option(t) =>
  d == z.selection.focus ? grow_selection(z) : shrink_selection(z);

let singleton_shard_selection = (seg: Segment.t): option(Token.t) =>
  switch (seg) {
  | [Tile(t)] =>
    switch (Tile.effective_label(t)) {
    | [tok] => Some(tok)
    | _ => None
    }
  | _ => None
  };

let neighbor_shard = (d: Direction.t, z: t): option(Token.t) =>
  switch (Siblings.neighbor(d, z.relatives.siblings)) {
  | Some(p) when Piece.monotile(p) != None => Piece.monotile(p)
  | _ =>
    let* z = select(d, z);
    singleton_shard_selection(z.selection.content);
  };

let neighbor_shards = (z: t): (option(Token.t), option(Token.t)) => (
  neighbor_shard(Left, z),
  neighbor_shard(Right, z),
);

let adj_pos = (d: Direction.t, z: t): t =>
  switch (d) {
  | Left => z
  | Right =>
    switch (move(Left, z)) {
    | None => z
    | Some(z) => z
    }
  };

let put_down_core = (seg: Segment.t, z: t): t =>
  z |> replace_selection(Right, seg) |> unselect;

let put_down_seg = (d: Direction.t, seg: Segment.t, z: t): t =>
  z |> put_down_core(seg) |> adj_pos(d);

let local_backpack = (z: t): list(Tile.t) =>
  Relatives.local_missing_shards(z.relatives);

let backpack_hd = (z: t): option(Tile.t) =>
  z |> local_backpack |> ListUtil.hd_opt;

let backpack_find = (tok: Token.t, z: t): option(Tile.t) =>
  if (Form.is_ambiguous_polymorph(tok)) {
    /* Special case for ambiguous polymorphs. These tokens
       occur both on their own as infix ops and as delimiters of
       multi-delimiter forms. To give the singleton form a chance, we
       only match these to incomplete tiles to form their multi forms
       when they're on the top of the stack */
    backpack_hd(z) |> Option.map(Tile.effective_label) == Some([tok])
      ? backpack_hd(z) : None;
  } else {
    List.find_map(
      t => Tile.effective_label(t) == [tok] ? Some(t) : None,
      local_backpack(z),
    );
  };

let put_down_tok = (d: Direction.t, tok: Token.t, z: t): option(t) => {
  /* Does not regrout/remold on its own. */
  let+ target = backpack_find(tok, z);
  put_down_seg(d, [Tile(target)], z);
};

let put_down = (d: Direction.t, z: t): option(t) => {
  /* Does not regrout/remold on its own. */
  let+ target = backpack_hd(z);
  put_down_seg(d, [Tile(target)], z);
};

let can_put_down = z =>
  switch (local_backpack(z)) {
  | [] => false
  | _ => z.caret == Outer
  };

let put_down_regrout_target = (d: Direction.t, target: Tile.t, z: t): t => {
  let z = put_down_core([Tile(target)], z);
  let z = z |> regrout(Left) |> remold;
  adj_pos(d, z);
};

let put_down_regrout_remold = (d: Direction.t, z: t): option(t) => {
  let+ target = backpack_hd(z);
  put_down_regrout_target(d, target, z);
};

let will_glom = (tok: Token.t, z: t): bool =>
  put_down_tok(Right, tok, z) != None;

let glom = (d: Direction.t, tok: Token.t, z: t): option(t) => {
  let+ target = backpack_find(tok, z);
  put_down_regrout_target(d, target, z);
};

let rec get_leaf_pieces =
        (syntaxNode: Piece.t, ~ignored_labels: list(list(string)))
        : list(Piece.t) =>
  switch (syntaxNode) {
  | Tile(tile) =>
    /* Check if this tile's label is in the ignored labels */
    let should_ignore =
      List.exists(label => label == tile.label, ignored_labels);
    if (should_ignore) {
      [];
        /* Ignore this tile */
    } else if (tile.children == []) {
      [
        /* It's a leaf piece */
        Tile(tile),
      ];
    } else {
      /* Recurse into the children */
      tile.children
      |> List.concat_map(segment =>
           segment |> List.concat_map(get_leaf_pieces(~ignored_labels))
         );
    };
  | _ => []
  };

let remove_projector = (id: Id.t, syntax: Piece.t) =>
  switch (syntax) {
  | Projector(pr) when pr.id == id =>
    // just get the label, found as first leaf piece
    get_leaf_pieces(pr.syntax, ~ignored_labels=[[","]]) |> List.hd
  | x => x
  };

let delete = (d: Direction.t, z: t): option(t) => {
  let to_delete = z |> select(d);
  switch (to_delete) {
  | Some({selection: {content: [Projector(p)], _}, _}) =>
    switch (p.kind) {
    | Livelit =>
      Some(ZipperBase.MapPiece.fast_local(remove_projector(p.id), p.id, z))
    | _ => to_delete |> Option.map(destroy_selection)
    }
  | _ => to_delete |> Option.map(destroy_selection)
  };
};

let glom_prev = (z: t) =>
  switch (neighbor_shard(Left, z)) {
  | Some(t) when will_glom(t, z) =>
    switch (delete(Left, z)) {
    | Some(z) => glom(Left, t, z)
    | None => Some(z)
    }
  | _ => None
  };

let adjacent_monotile_id = (d: Direction.t, z: t): option(Id.t) =>
  switch (Siblings.neighbors(z.relatives.siblings)) {
  | (Some(Tile({id, label: [_], _})), _) when d == Left => Some(id)
  | (_, Some(Tile({id, label: [_], _}))) when d == Right => Some(id)
  | _ => None
  };

let adjacent_monotile_or_new_id = (d, z) =>
  switch (adjacent_monotile_id(d, z)) {
  | Some(id) => id
  | None => Id.mk()
  };

let representative_piece = (z: t): option((Piece.t, Direction.t)) => {
  /* The piece to the left of the caret, or if none exists, the piece to the right */
  switch (Siblings.neighbors(sibs_with_sel(z))) {
  | (Some(l), _) => Some((l, Left))
  | (_, Some(r)) => Some((r, Right))
  | _ => None
  };
};

let base_point = (measured: Measured.t, z: t): Point.t => {
  switch (representative_piece(z)) {
  | Some((p, d)) =>
    let seg = Piece.disassemble(p);
    switch (d) {
    | Left =>
      let p = ListUtil.last(seg);
      let m = Measured.find_p(~msg="base_point", p, measured);
      m.last;
    | Right =>
      let p = List.hd(seg);
      let m = Measured.find_p(~msg="base_point", p, measured);
      m.origin;
    };
  | None => {
      row: 0,
      col: 0,
    }
  };
};

module Caret = {
  let offset: caret => int =
    fun
    | Outer => 0
    | Inner(idx) => idx + 1;

  let set = (caret: caret, z: t): t => {
    ...z,
    caret,
  };

  /* Max internal index of the shard the caret is adjacent to */
  let nhbr_max_idx = (d: Direction.t, z: t): option(int) => {
    let* t =
      switch (d, neighbor_shards(z)) {
      | (Left, (Some(t), _)) => Some(t)
      | (Right, (_, Some(t))) => Some(t)
      | _ => None
      };
    let max_idx = Token.length(t) - 2;
    max_idx < 0 ? None : Some(max_idx);
  };

  /* Returns the delimiter index that the caret is adjacent to.
   * For non-tiles and monotiles this is always zero */
  let delim_idx = (z: t) =>
    switch (snd(z.relatives.siblings), z.relatives.ancestors) {
    | ([], [({shards: (l, _), _}, _), ..._]) => List.length(l)
    | _ => 0
    };

  /* Direction the caret is facing in */
  let direction = (z: t): option(Direction.t) =>
    switch (z.caret) {
    | Inner(_) => None
    | Outer =>
      switch (Siblings.neighbors(sibs_with_sel(z))) {
      | (Some(l), Some(r))
          when
            Piece.is_secondary(l)
            && Piece.is_secondary(r)
            && Selection.is_empty(z.selection) =>
        None
      | _ => Siblings.direction_between(sibs_with_sel(z))
      }
    };

  /* Grid position of the caret */
  let point = (measured: Measured.t, z: t): Point.t => {
    let Point.{row, col} = base_point(measured, z);
    {
      row,
      col: col + offset(z.caret),
    };
  };

  type t = ZipperBase.caret;
};

let selection_anchor_point = (measured, z: t): option(Point.t) => {
  switch (z.selection) {
  | {content: [], _} => None
  | {content, focus: Right, _} =>
    Some(
      Measured.find_p(
        ~msg="selection_anchor_point",
        List.hd(content),
        measured,
      ).
        origin,
    )
  | {content, focus: Left, _} =>
    Some(
      Measured.find_p(
        ~msg="selection_anchor_point",
        ListUtil.last(content),
        measured,
      ).
        last,
    )
  };
};

let serialize = (z: t): string => {
  sexp_of_t(z) |> Sexplib.Sexp.to_string;
};

let to_sexp = (z: t): Sexplib.Sexp.t => sexp_of_t(z);

let deserialize = (data: string): t => {
  Sexplib.Sexp.of_string(data) |> t_of_sexp;
};

let set_buffer = (z: t, ~mode: Selection.buffer, ~content: Segment.t): t => {
  ...z,
  selection: Selection.mk_buffer(mode, content),
};

let is_linebreak_to_right_of_caret =
    ({relatives: {siblings: (_, r), _}, _}: t): bool => {
  switch (r) {
  | [Secondary(s), ..._] when Secondary.is_linebreak(s) => true
  | _ => false
  };
};

/* Try to complete the syntax to give better semantic feeback.
 * This is a best-effort approach focussed on adding new definitions
 * as opposed to restructuring; it does not complete the syntax in
 * all cases.
 *
 * NOTE: Setting the caret to outer was necessary to 'get it past'
 * string literals, i.e. offer live feeback when typing inside a
 * string; not sure if this is a hack or not, it may be compensating
 * for the put_down logic not working right with string lits. To test,
 * try to look at live evaluation while typing inside a string lit with
 * stuff left to drop in backpack with below set: Outer disabled. */
let try_to_dump_backpack = (zipper: t) => {
  switch (local_backpack(zipper)) {
  | [] => zipper
  | _ =>
    let zipper = {
      ...zipper,
      caret: Outer,
    };
    let rec move_until_cant_put_down = (z_last, z: t) =>
      if (can_put_down(z) && !is_linebreak_to_right_of_caret(z)) {
        switch (move(Right, z)) {
        | None => z
        | Some(z_new) => move_until_cant_put_down(z, z_new)
        };
      } else {
        z_last;
      };
    let rec move_until_can_put_down = (z: t) =>
      if (!can_put_down(z)) {
        switch (move(Right, z)) {
        | None => z
        | Some(z_new) => move_until_can_put_down(z_new)
        };
      } else {
        z;
      };
    let rec go = (z: t): t => {
      let z_can = can_put_down(z) ? z : move_until_can_put_down(z);
      let z_cant = move_until_cant_put_down(z_can, z_can);
      switch (put_down_regrout_remold(Right, z_cant)) {
      | None => z_cant
      | Some(z) => go(z)
      };
    };
    go(zipper);
  };
};

let smart_seg = (~dump_backpack: bool, ~erase_buffer: bool, z: t) => {
  let z = erase_buffer ? clear_unparsed_buffer(z) : z;
  let z = dump_backpack ? try_to_dump_backpack(z) : z;
  unselect_and_zip(~erase_buffer, z);
};

let seg_without_buffer = smart_seg(~erase_buffer=true, ~dump_backpack=false);
