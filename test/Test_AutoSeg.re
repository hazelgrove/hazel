// open Alcotest;
  // open Haz3lcore;
  // open Base;
  // // Helper function to create simple segments for testing
  // let mk_simple_tile =
  //     (id: Id.t, label: list(string), children: list(Segment.t)): Piece.t => {
  //   Tile({
  //     id,
  //     label,
  //     mold: Mold.mk_op(Exp, []),
  //     shards: List.mapi((i, _) => i, label),
  //     children,
  //   });
  // };
  // let mk_grout = (shape: Grout.shape): Piece.t => {
  //   Grout({
  //     id: Id.mk(),
  //     shape,
  //   });
  // };
  // let mk_secondary = (content: string): Piece.t => {
  //   Secondary({
  //     id: Id.mk(),
  //     content: Secondary.Whitespace(content),
  //   });
  // };
  // // Test cases for seg_to_auto_seg
  // let test_empty_segment = () => {
  //   let empty_seg: Segment.t = [];
  //   let result = AutoSeg.seg_to_auto_seg(empty_seg);
  //   check(
  //     bool,
  //     "Empty segment should contain root entry",
  //     true,
  //     AutoSeg.IdMap.mem(AutoSeg.root, result),
  //   );
  //   let root_seg = AutoSeg.IdMap.find(AutoSeg.root, result);
  //   check(int, "Root segment should be empty", 0, List.length(root_seg));
  // };
  // let test_single_tile = () => {
  //   let tile_id = Id.mk();
  //   let seg: Segment.t = [mk_simple_tile(tile_id, ["1"], [])];
  //   let result = AutoSeg.seg_to_auto_seg(seg);
  //   let root_id = AutoSeg.root;
  //   check(
  //     bool,
  //     "Should contain root segment",
  //     true,
  //     AutoSeg.IdMap.mem(root_id, result),
  //   );
  //   let root_seg = AutoSeg.IdMap.find(root_id, result);
  //   check(int, "Root segment should have one piece", 1, List.length(root_seg));
  //   switch (List.hd(root_seg)) {
  //   | AutoSeg.Tile(tile) =>
  //     // The label should be preserved from the original tile
  //     check(list(string), "Tile should preserve label", ["1"], tile.label);
  //     check(
  //       int,
  //       "Tile should have empty children",
  //       0,
  //       List.length(tile.children),
  //     );
  //   | _ => fail("Expected Tile piece")
  //   };
  // };
  // let test_tile_with_children = () => {
  //   let parent_id = Id.mk();
  //   let child_seg: Segment.t = [mk_simple_tile(Id.mk(), ["2"], [])];
  //   let seg: Segment.t = [
  //     mk_simple_tile(parent_id, ["(", ")"], [child_seg]),
  //   ];
  //   let result = AutoSeg.seg_to_auto_seg(seg);
  //   let root_id = AutoSeg.root;
  //   // Check root segment
  //   let root_seg = AutoSeg.IdMap.find(root_id, result);
  //   check(int, "Root segment should have one piece", 1, List.length(root_seg));
  //   switch (List.hd(root_seg)) {
  //   | AutoSeg.Tile(tile) =>
  //     // Debug: print what we're actually getting
  //     print_endline(
  //       "Debug: Expected label: [\"(\", \")\"], Got: ["
  //       ++ String.concat(", ", List.map(s => "\"" ++ s ++ "\"", tile.label))
  //       ++ "]",
  //     );
  //     check(
  //       list(string),
  //       "Tile should preserve label",
  //       ["(", ")"],
  //       tile.label,
  //     );
  //     check(int, "Tile should have one child", 1, List.length(tile.children));
  //     // Check child segment
  //     let child_id = List.hd(tile.children);
  //     check(
  //       bool,
  //       "Child segment should exist in IdMap",
  //       true,
  //       AutoSeg.IdMap.mem(child_id, result),
  //     );
  //     let child_seg = AutoSeg.IdMap.find(child_id, result);
  //     check(
  //       int,
  //       "Child segment should have one piece",
  //       1,
  //       List.length(child_seg),
  //     );
  //     switch (List.hd(child_seg)) {
  //     | AutoSeg.Tile(child_tile) =>
  //       check(
  //         list(string),
  //         "Child tile should preserve label",
  //         ["2"],
  //         child_tile.label,
  //       )
  //     | _ => fail("Expected Tile piece in child")
  //     };
  //   | _ => fail("Expected Tile piece")
  //   };
  // };
  // let test_mixed_pieces = () => {
  //   let seg: Segment.t = [
  //     mk_simple_tile(Id.mk(), ["foo"], []),
  //     mk_grout(Grout.Convex),
  //     mk_secondary(" "),
  //     mk_simple_tile(Id.mk(), ["bar"], []),
  //   ];
  //   let result = AutoSeg.seg_to_auto_seg(seg);
  //   let root_id = AutoSeg.root;
  //   let root_seg = AutoSeg.IdMap.find(root_id, result);
  //   check(
  //     int,
  //     "Root segment should have four pieces",
  //     4,
  //     List.length(root_seg),
  //   );
  //   // Check first piece (tile)
  //   switch (List.nth(root_seg, 0)) {
  //   | AutoSeg.Tile(tile) =>
  //     check(
  //       list(string),
  //       "First tile should preserve label",
  //       ["foo"],
  //       tile.label,
  //     )
  //   | _ => fail("Expected Tile piece at index 0")
  //   };
  //   // Check second piece (grout)
  //   switch (List.nth(root_seg, 1)) {
  //   | AutoSeg.Grout(grout) =>
  //     check(
  //       bool,
  //       "Grout should preserve shape",
  //       true,
  //       grout.shape == Grout.Convex,
  //     )
  //   | _ => fail("Expected Grout piece at index 1")
  //   };
  //   // Check third piece (secondary)
  //   switch (List.nth(root_seg, 2)) {
  //   | AutoSeg.Secondary(secondary) =>
  //     check(
  //       bool,
  //       "Secondary should preserve content",
  //       true,
  //       secondary.content == Secondary.Whitespace(" "),
  //     )
  //   | _ => fail("Expected Secondary piece at index 2")
  //   };
  //   // Check fourth piece (tile)
  //   switch (List.nth(root_seg, 3)) {
  //   | AutoSeg.Tile(tile) =>
  //     check(
  //       list(string),
  //       "Second tile should preserve label",
  //       ["bar"],
  //       tile.label,
  //     )
  //   | _ => fail("Expected Tile piece at index 3")
  //   };
  // };
  // let test_nested_tiles = () => {
  //   let inner_child: Segment.t = [mk_simple_tile(Id.mk(), ["3"], [])];
  //   let middle_child: Segment.t = [
  //     mk_simple_tile(Id.mk(), ["(", ")"], [inner_child]),
  //   ];
  //   let seg: Segment.t = [
  //     mk_simple_tile(Id.mk(), ["(", ")"], [middle_child]),
  //   ];
  //   let result = AutoSeg.seg_to_auto_seg(seg);
  //   let root_id = AutoSeg.root;
  //   // Check root segment
  //   let root_seg = AutoSeg.IdMap.find(root_id, result);
  //   switch (List.hd(root_seg)) {
  //   | AutoSeg.Tile(root_tile) =>
  //     check(
  //       int,
  //       "Root tile should have one child",
  //       1,
  //       List.length(root_tile.children),
  //     );
  //     // Check middle child
  //     let middle_id = List.hd(root_tile.children);
  //     let middle_seg = AutoSeg.IdMap.find(middle_id, result);
  //     switch (List.hd(middle_seg)) {
  //     | AutoSeg.Tile(middle_tile) =>
  //       check(
  //         int,
  //         "Middle tile should have one child",
  //         1,
  //         List.length(middle_tile.children),
  //       );
  //       // Check inner child
  //       let inner_id = List.hd(middle_tile.children);
  //       let inner_seg = AutoSeg.IdMap.find(inner_id, result);
  //       switch (List.hd(inner_seg)) {
  //       | AutoSeg.Tile(inner_tile) =>
  //         check(
  //           list(string),
  //           "Inner tile should preserve label",
  //           ["3"],
  //           inner_tile.label,
  //         )
  //       | _ => fail("Expected Tile piece in inner child")
  //       };
  //     | _ => fail("Expected Tile piece in middle child")
  //     };
  //   | _ => fail("Expected Tile piece in root")
  //   };
  // };
  // let test_multiple_children = () => {
  //   let child1: Segment.t = [mk_simple_tile(Id.mk(), ["a"], [])];
  //   let child2: Segment.t = [mk_simple_tile(Id.mk(), ["b"], [])];
  //   let child3: Segment.t = [mk_simple_tile(Id.mk(), ["c"], [])];
  //   let seg: Segment.t = [
  //     mk_simple_tile(Id.mk(), ["(", ",", ",", ")"], [child1, child2, child3]),
  //   ];
  //   let result = AutoSeg.seg_to_auto_seg(seg);
  //   let root_id = AutoSeg.root;
  //   let root_seg = AutoSeg.IdMap.find(root_id, result);
  //   switch (List.hd(root_seg)) {
  //   | AutoSeg.Tile(tile) =>
  //     check(
  //       int,
  //       "Tile should have three children",
  //       3,
  //       List.length(tile.children),
  //     );
  //     // Check that all children exist in the IdMap
  //     List.iteri(
  //       (i, child_id) => {
  //         check(
  //           bool,
  //           "Child " ++ string_of_int(i) ++ " should exist in IdMap",
  //           true,
  //           AutoSeg.IdMap.mem(child_id, result),
  //         )
  //       },
  //       tile.children,
  //     );
  //   | _ => fail("Expected Tile piece")
  //   };
  // };
  // let test_custom_root_id = () => {
  //   let custom_id: AutoSeg.id = {
  //     uuid: Id.mk(),
  //     index: 42,
  //   };
  //   let seg: Segment.t = [mk_simple_tile(Id.mk(), ["test"], [])];
  //   let result = AutoSeg.seg_to_auto_seg(~id=custom_id, seg);
  //   check(
  //     bool,
  //     "Should contain custom root id",
  //     true,
  //     AutoSeg.IdMap.mem(custom_id, result),
  //   );
  //   check(
  //     bool,
  //     "Should not contain default root id",
  //     false,
  //     AutoSeg.IdMap.mem(AutoSeg.root, result),
  //   );
  // };
  // let test_projector_conversion = () => {
  //   // Create a segment with a projector (which should be converted to Secondary)
  //   let projector_piece: Piece.t = {
  //     let projector_id = Id.mk();
  //     Projector({
  //       id: projector_id,
  //       kind: ProjectorCore.Kind.Fold,
  //       syntax: mk_simple_tile(Id.mk(), ["syntax"], []),
  //       model: "model",
  //     });
  //   };
  //   let seg: Segment.t = [projector_piece];
  //   let result = AutoSeg.seg_to_auto_seg(seg);
  //   let root_id = AutoSeg.root;
  //   let root_seg = AutoSeg.IdMap.find(root_id, result);
  //   check(int, "Root segment should have one piece", 1, List.length(root_seg));
  //   switch (List.hd(root_seg)) {
  //   | AutoSeg.Secondary(secondary) =>
  //     check(
  //       bool,
  //       "Projector should be converted to Secondary with Comment",
  //       true,
  //       secondary.content == Secondary.Comment("WHOOPS"),
  //     )
  //   | _ => fail("Expected Secondary piece from projector conversion")
  //   };
  // };
  // let test_debug = () => {
  //   let tile_id = Id.mk();
  //   let seg: Segment.t = [mk_simple_tile(tile_id, ["debug"], [])];
  //   let result = AutoSeg.seg_to_auto_seg(seg);
  //   let root_id = AutoSeg.root;
  //   let root_seg = AutoSeg.IdMap.find(root_id, result);
  //   switch (List.hd(root_seg)) {
  //   | AutoSeg.Tile(tile) =>
  //     // Just print the label to see what we're getting
  //     print_endline("Debug: Got label: " ++ String.concat(", ", tile.label));
  //     check(bool, "Debug test", true, true);
  //   | _ => fail("Expected Tile piece")
  //   };
  // };
  // let tests = (
  //   "AutoSeg",
  //   [
  //     test_case("Empty segment", `Quick, test_empty_segment),
  //     test_case("Single tile", `Quick, test_single_tile),
  //     test_case("Tile with children", `Quick, test_tile_with_children),
  //     test_case("Mixed pieces", `Quick, test_mixed_pieces),
  //     test_case("Nested tiles", `Quick, test_nested_tiles),
  //     test_case("Multiple children", `Quick, test_multiple_children),
  //     test_case("Custom root id", `Quick, test_custom_root_id),
  //     test_case("Projector conversion", `Quick, test_projector_conversion),
  //     test_case("Debug", `Quick, test_debug),
  //   ],
  // );
