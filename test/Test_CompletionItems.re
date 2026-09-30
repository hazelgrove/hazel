open Alcotest;
open Haz3lcore;
open Language;

/* Parity gate for per-item canonical completion
   (CanonicalCompletion.complete_items vs complete_segment_deep): on the
   mega corpus, on small edge programs, and on edit-derived incomplete
   states, the per-item reading must equal the whole-segment reading
   (segment modulo grout ids, plus the same shard records), items with
   no incomplete tile must come back physically unchanged, and a second
   call on the same segment must complete nothing.
     bash test/run_node.sh test 'CompletionItems' */

let settings = CoreSettings.on;

let records_sorted = (rs: list(CanonicalCompletion.shard_record)) =>
  List.sort(
    (a: CanonicalCompletion.shard_record, b: CanonicalCompletion.shard_record) =>
      compare(a.tile_id, b.tile_id),
    rs,
  );

let check_parity = (name: string, seg: Segment.t): unit => {
  let whole = CanonicalCompletion.complete_segment_deep(~sort=Exp, seg);
  let w0 = CanonicalCompletion.items_widened^;
  let items = CanonicalCompletion.complete_items(~sort=Exp, seg);
  let widened = CanonicalCompletion.items_widened^ - w0;
  check(
    bool,
    name ++ ": completed segments equivalent",
    true,
    Segment.equiv_mod_grout(whole.completed_seg, items.completed_seg),
  );
  let grout_count = sg =>
    List.length(
      List.filter(
        (p: Piece.t) =>
          switch (p) {
          | Grout(_) => true
          | _ => false
          },
        sg,
      ),
    );
  check(
    int,
    name ++ ": same top-level grout count",
    grout_count(whole.completed_seg),
    grout_count(items.completed_seg),
  );
  check(
    bool,
    name ++ ": same shard records",
    true,
    records_sorted(whole.shard_records)
    == records_sorted(items.shard_records),
  );
  /* items without incomplete tiles keep their pieces, except the ones a
     widening merged into an incomplete predecessor's block */
  let complete_items =
    Segment.top_items(seg)
    |> List.filter(item => Segment.incomplete_tiles_deep(item) == []);
  let kept =
    complete_items
    |> List.filter(item =>
         List.for_all(p => List.memq(p, items.completed_seg), item)
       )
    |> List.length;
  check(
    bool,
    Printf.sprintf(
      "%s: complete items physically kept (%d of %d, %d widened)",
      name,
      kept,
      List.length(complete_items),
      widened,
    ),
    true,
    kept >= List.length(complete_items) - widened,
  );
  /* another call: nothing recompletes */
  let n0 = CanonicalCompletion.items_completed^;
  let again = CanonicalCompletion.complete_items(~sort=Exp, seg);
  check(int, name ++ ": memo hit", n0, CanonicalCompletion.items_completed^);
  check(
    bool,
    name ++ ": memo returns the same reading",
    true,
    Segment.ptr_eq(again.completed_seg, items.completed_seg),
  );
};

/* type [keys] on a fresh line after the k-th item (caret to the right of
   its last piece, then Enter), the way a new item is actually typed */
let type_after_item =
    (k: int, keys: string, seg: Segment.t): option(Zipper.t) => {
  let z0 = Zipper.unzip(seg);
  let last =
    switch (List.nth_opt(Segment.top_items(seg), k)) {
    | Some(item) => Option.map(Piece.id, Util.ListUtil.last_opt(item))
    | None => None
    };
  let keys = "\n" ++ keys;
  Option.bind(last, id => Move.jump_to_side_of_id(Right, z0, id))
  |> Option.map(z => {
       let syntax = CachedSyntax.init(~root=Exp, z);
       let statics =
         CachedStatics.init_from_term(
           ~settings,
           ~is_dynamic_term=true,
           MakeTerm.from_zip_for_sem(z, ~root=Exp).term,
         );
       String.fold_left(
         (z, c) =>
           switch (
             Perform.go(
               ~settings,
               ~statics,
               ~syntax,
               ~root=Exp,
               Action.Insert(String.make(1, c)),
               {
                 zipper: z,
                 col_target: None,
               },
             )
           ) {
           | Ok(z) => z
           | Error(_) => z
           },
         z,
         keys,
       );
     });
};

let corpus_case = (file: string, ()) =>
  switch (CorpusUtil.corpus_seg(~root=Exp, file)) {
  | None => fail("corpus unreadable/unparseable: " ++ file)
  | Some(seg) =>
    check_parity(file ++ " (complete)", seg);
    let n = List.length(Segment.top_items(seg));
    List.iter(
      ((k, keys)) =>
        switch (type_after_item(k, keys, seg)) {
        | None =>
          fail(Printf.sprintf("%s: could not type after item %d", file, k))
        | Some(z) =>
          check_parity(
            Printf.sprintf("%s item %d + %S", file, k, keys),
            Zipper.unselect_and_zip(~erase_buffer=true, z),
          )
        },
      [
        (n / 2, "let q = 1"),
        (n / 3, "case x "),
        (2 * n / 3, "(1 + "),
        (0, "let f = fun x ->"),
        (n - 2, "if true then"),
      ],
    );
  };

let edge_programs = [
  ("two defs", "let a = 1 in\nlet b = 2 in\na + b"),
  ("tail op tree", "let a = 1 in\na + 2 * 3 - 4"),
  ("seq semis", "let f = fun x -> x in\nf(1); f(2); f(3)"),
  (
    "case adoption",
    "let f = fun x ->\ncase x\n| 1 => 2\n| _ => 3\nend in\nf(0)",
  ),
  ("blank lines", "let a = 1 in\n\n\nlet b = 2 in\nb"),
  ("single expr", "1 + 2 * 3"),
];

let edge_case = ((name, src), ()) =>
  switch (ParsedCorpus.to_segment(~root=Exp, src)) {
  | None => fail("unparseable edge program: " ++ name)
  | Some(seg) =>
    check_parity(name, seg);
    switch (type_after_item(0, "let z = 0", seg)) {
    | None => fail(name ++ ": could not type")
    | Some(z) =>
      check_parity(
        name ++ " + incomplete let",
        Zipper.unselect_and_zip(~erase_buffer=true, z),
      )
    };
  };

let tests = (
  "CompletionItems",
  List.map(
    ((name, _) as e) => test_case(name, `Quick, edge_case(e)),
    edge_programs,
  )
  @ [
    test_case("mega-1k parity", `Quick, corpus_case("mega-1k.hz")),
    test_case("mega-2k parity", `Quick, corpus_case("mega-2k.hz")),
  ],
);
