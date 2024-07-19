open ZipperBase;
open Util.StringDom;
open Util.Tree;

type derive('code) = {
  jdmt: 'code,
  rule: 'code,
};

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

// TODO(zhiyao): Not a good way to verify projected syntax
let verify_projected = (syntax: Piece.t): bool => {
  let (tiles, _) = [syntax] |> split_tiles_of_segment;
  let tiles = tiles |> List.map(tile_of_piece);
  if (List.length(tiles) == 0) {
    true;
  } else {
    let tile = tiles |> List.hd;
    switch (tile.label) {
    | ["from", "to", "by", "end"] => false
    | _ => true
    };
  };
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
  // assert(tile.label == ["from", "to", "by", "end"]);
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
  // assert(List.length(tile.children) == 3);
  switch (List.length(tile.children)) {
  | 3 => ()
  | n => failwith("unexpected number of children: " ++ (n |> string_of_int))
  };
  // let prems = List.nth(tile.children, 0);
  // let jdmt = List.nth(tile.children, 1);
  // let rule = List.nth(tile.children, 2);
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
  // let get_only =
  //   fun
  //   | [x] => List.concat([x])
  //   | _ => failwith("expect 1 child");
  let prems = prems.children |> List.concat;
  let rec get_as_of_aba =
    fun
    | [a, _b, ...tl] => [[a], ...get_as_of_aba(tl)]
    | [a] => [[a]]
    | [] => [];
  let prems = prems |> get_as_of_aba;
  // let n = List.length(prems);
  // TODO(zhiyao): handle n = 0
  // let label = n == 0 ? ["[]"] : ["["] @ List.init(n - 1, _ => ",") @ ["]"];
  // // assert(prems.label == label\);
  // if (prems.label == label) {
  //   ();
  // } else {
  //   failwith(
  //     "unexpected label: [" ++ (prems.label |> String.concat(",")) ++ "]",
  //   );
  // };
  let (children_derive_tree, other_list) =
    List.map(mk_derive_tree, prems) |> List.split;
  // TODO: temp
  let others = other_list |> List.concat |> List.append(others);
  (Node({jdmt, rule}, children_derive_tree), others);
};

let bind_ghost_tiles =
    (others: list(Piece.t), seg: Piece.segment): Piece.segment => {
  let mk_ghost_tile =
    fun
    | Piece.Tile({id, _}) =>
      Piece.Tile({
        id,
        label: [""],
        mold: Mold.mk_op(Any, []),
        shards: [0],
        children: [],
      })
    | _ as t => t;
  others |> List.map(mk_ghost_tile) |> List.append(seg);
};

let mk = (syntax: Piece.t, _model): projector_core =>
  try(
    (module
     {
       [@deriving (show({with_path: false}), sexp, yojson)]
       type model = ZipperBase.derivearea;
       [@deriving (show({with_path: false}), sexp, yojson)]
       type action = ZipperBase.derivearea_action;
       let model = ();
       let projector = DeriveArea(model);
       let can_project = _ => true;
       //TODO(andrew): cleanup
       let verified = verify_projected(syntax);
       let (children, placeholder) =
         if (verified) {
           ((syntax |> tile_of_piece).children, () => ZipperBase.Multi([]));
         } else {
           let (tree, _others) = mk_derive_tree([syntax]);
           let virtual_node_tree = mk_virtual_node_tree(tree);
           let loc_tiles =
             virtual_node_tree
             |> mk_loc_list({x: 0, y: 0}, size_of_segment)
             |> flatloc2tiles(size_of_segment);
           let placeholders =
             loc_tiles
             |> List.filter_map(
                  fun
                  | Inline(x) => Some(ZipperBase.Inline(x))
                  | Block({w, h}) =>
                    Some(ZipperBase.Block({col: w, row: h}))
                  | _ => None,
                );
           let children =
             loc_tiles
             |> List.filter_map(
                  fun
                  | Inline(_) => None
                  | Block(_) => None
                  | Just(tile) => Some(tile),
                );
           //  let child =
           //    switch (children) {
           //    | [child, _] => child
           //    | _ => failwith("expect at least one child")
           //    };
           //  let child = child |> bind_ghost_tiles(others);
           //  let children = [child, ...List.tl(children)];
           (children, () => ZipperBase.Multi(placeholders));
         };
       //  List.iter(
       //    seg => List.iter(piece => piece |> Piece.show |> print_endline, seg),
       //    children,
       //  );
       let auto_update = _: projector => DeriveArea();
       let update = _action => DeriveArea();
     })
  ) {
  | e => failwith("DeriveAreaCore: " ++ (e |> Printexc.to_string))
  };
