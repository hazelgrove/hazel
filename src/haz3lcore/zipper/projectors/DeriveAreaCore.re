open ZipperBase;
open Util.SimulDom;
open Util.Tree;

let rec mk_virtual_node_tree = (Node({jdmt, rule}, c)) =>
  Node(
    Branch(Column, Start),
    [
      Node(
        Branch(Row, End),
        [
          Node(Branch(Row, End), List.map(mk_virtual_node_tree, c)),
          Node(Leaf(rule), []),
        ],
      ),
      Node(Leaf(jdmt), []),
    ],
  );

let is_tile =
  fun
  | Piece.Tile(_) => true
  | Secondary(_)
  | Grout(_) => false;

let split_tiles_of_segment = List.partition(is_tile);

let tile_of_piece =
  fun
  | Piece.Tile(t) => t
  | Secondary(_)
  | Grout(_) => failwith("tile_of_piece: expect tile piece");

let width_of_segment = (seg: Piece.segment): int => {
  let (tiles, _) = seg |> split_tiles_of_segment;
  let tiles = tiles |> List.map(tile_of_piece);
  if (List.length(tiles) == 0) {
    0;
  } else {
    let first_tile = List.hd(tiles);
    let last_tile = List.hd(List.rev(tiles));
    let {Measured.tiles, _} = Measured.of_segment(seg);
    let {Measured.origin: {row: x1, col: y1}, _} =
      Id.Map.find(first_tile.id, tiles) |> List.hd |> snd;
    let {Measured.last: {row: x2, col: y2}, _} =
      Id.Map.find(last_tile.id, tiles) |> List.rev |> List.hd |> snd;
    let width = x1 == x2 ? y2 - y1 : failwith("expected single row");
    width;
  };
};

// TODO(zhiyao): for now we assume all segments are single row
let size_of_segment = (seg: Piece.segment): size => {
  w: seg |> width_of_segment,
  h: 1,
};

let rec mk_derive_tree =
        (seg: Piece.segment): (tree(derive(Piece.segment)), list(Piece.t)) => {
  let (tiles, others) = seg |> split_tiles_of_segment;
  let tiles = tiles |> List.map(tile_of_piece);
  // List.iter(t => "####" ++ (t |> Piece.show_tile) |> print_endline, tiles);
  let tile =
    switch (tiles) {
    | [tile] => tile
    | l =>
      failwith("expect 1 tile, got " ++ (l |> List.length |> string_of_int))
    };
  switch (tile.label) {
  | ["from", "to", "by", "end"] => ()
  | _ =>
    failwith(
      "unexpected label: ["
      ++ (tile.label |> String.concat(","))
      ++ "]"
      ++ "\n"
      ++ (tile |> Piece.show_tile),
    )
  };
  switch (List.length(tile.children)) {
  | 3 => ()
  | n => failwith("unexpected number of children: " ++ (n |> string_of_int))
  };
  let (prems, jdmt, rule) =
    switch (tile.children) {
    | [prems, jdmt, rule] => (prems, jdmt, rule)
    | _ => failwith("impossible")
    };
  let (prems, others') = prems |> split_tiles_of_segment;
  let others = others |> List.append(others');
  let prems = prems |> List.map(tile_of_piece);
  let prems =
    switch (prems) {
    | [prems] => prems
    | l =>
      failwith(
        "expect 1 .premise, got" ++ (l |> List.length |> string_of_int),
      )
    };
  let prems = prems.children |> List.concat;
  let rec get_as_of_aba =
    fun
    | [a, b, ...tl] => {
        print_endline(Piece.show_piece(b));
        [[a], ...get_as_of_aba(tl)];
      }
    | [a] => [[a]]
    | [] => [];
  let prems = prems |> get_as_of_aba;
  let (children_derive_tree, other_list) =
    List.map(mk_derive_tree, prems) |> List.split;
  // TODO: temp
  let others = other_list |> List.concat |> List.append(others);
  (Node({jdmt, rule}, children_derive_tree), others);
};

// let mk_ghost_comma = (): Piece.t =>
//   Piece.Tile({
//     id: Id.mk(),
//     label: [","],
//     mold: Mold.mk_bin'(19, Exp, Exp, [], Exp),
//     shards: [0],
//     children: [],
//   });

let mk_ghost = (label, children): Piece.t =>
  Piece.Tile({
    id: Id.mk(),
    label,
    mold: Mold.mk_op(Exp, List.init(List.length(children), _ => Sort.Exp)),
    shards: List.init(List.length(children) + 1, _ => 0),
    children,
  });

let rec reduce_derive_tree =
        (Node({jdmt, rule}, children): tree(derive(Piece.segment)))
        : Piece.t => {
  let prems = children |> List.map(reduce_derive_tree);
  let rec put_as_to_aba =
    fun
    | [a] => [a]
    | [a, ...tl] => [a, mk_ghost([","], []), ...put_as_to_aba(tl)]
    | [] => [];
  let prems = prems |> put_as_to_aba;
  let prems = [mk_ghost(["[", "]"], [prems])];
  let children = [prems, jdmt, rule];
  mk_ghost(["from", "to", "by", "end"], children);
};

// let bind_ghost_tiles =
//     (others: list(Piece.t), seg: Piece.segment): Piece.segment => {
//   let mk_ghost_tile =
//     fun
//     | Piece.Tile({id, _}) =>
//       Piece.Tile({
//         id,
//         label: [""],
//         mold: Mold.mk_op(Any, []),
//         shards: [0],
//         children: [],
//       })
//     | _ as t => t;
//   others |> List.map(mk_ghost_tile) |> List.append(seg);
// };

let mk = (syntax: Piece.t, model): projector_core =>
  try(
    (module
     {
       [@deriving (show({with_path: false}), sexp, yojson)]
       type model = ZipperBase.derivearea;
       [@deriving (show({with_path: false}), sexp, yojson)]
       type action = ZipperBase.derivearea_action;
       let model =
         switch (model.tree) {
         | Some(_) => model
         | None => {...model, tree: Some(mk_derive_tree([syntax]) |> fst)}
         };
       let tree = model.tree |> Option.get;
       //  let reduced = tree |> reduce_derive_tree;
       //  print_endline(Piece.show(reduced));
       let projector = DeriveArea(model);
       let can_project = _ => true;
       let placeholder = () =>
         tree
         |> mk_virtual_node_tree
         |> outer_size(size_of_segment)
         |> (({w, h}) => ZipperBase.Block({col: w, row: h}));
       //  let virtual_node_tree = mk_virtual_node_tree(tree);
       //  let loc_tiles =
       //    virtual_node_tree
       //    |> mk_loc_list({x: 0, y: 0}, size_of_segment)
       //    |> flatloc2tiles(size_of_segment);
       //  let placeholders =
       //    loc_tiles
       //    |> List.filter_map(
       //         fun
       //         | Inline(x) => Some(ZipperBase.Inline(x))
       //         | Block({w, h}) =>
       //           Some(ZipperBase.Block({col: w, row: h}))
       //         | _ => None,
       //       );
       //  let children =
       //    loc_tiles
       //    |> List.filter_map(
       //         fun
       //         | Inline(_) => None
       //         | Block(_) => None
       //         | Just(tile) => Some(tile),
       //       );
       //  let child =
       //    switch (children) {
       //    | [child, _] => child
       //    | _ => failwith("expect at least one child")
       //    };
       //  let child = child |> bind_ghost_tiles(others);
       //  let children = [child, ...List.tl(children)];
       //  (children, () => ZipperBase.Multi(placeholders));
       //  //  List.iter(
       //  //    seg => List.iter(piece => piece |> Piece.show |> print_endline, seg),
       //  //    children,
       //  //  );
       // let placeholder = () => ZipperBase.Multi([]);
       let auto_update = _: projector => DeriveArea(model);
       let update = _action => DeriveArea(model);
     })
  ) {
  | e => failwith("DeriveAreaCore: " ++ (e |> Printexc.to_string))
  };
