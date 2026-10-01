open Alcotest;
open Haz3lcore;
open Language;

/* per-item completion (complete_items) matches whole-segment completion,
   keeps complete items physically, and memoizes */

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
     widening merged into an incomplete predecessor's block, or that an
     open tile above molded into its slot */
  let complete_items =
    Segment.top_items(seg)
    |> List.filter(item =>
         Segment.incomplete_tiles_deep(item) == []
         && Option.is_none(CanonicalCompletion.remolded(~sort=Exp, item))
       );
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

/* type [keys] on a new line after the k-th item, as a user would */
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

/* an unfinished form typed above complete items molds them into its
   open slot (a pattern, a type); whole completion re-molds them */
let open_slot_tails = [
  ("cons tail", "let a = 1 in\nlet b = 2 in\nb + 1 :: [b]"),
  ("var tail", "let a = 1 in\nlet b = 2 in\nzz"),
];
let open_slot_keys = ["let", "let foo", "type t ="];

let open_slot_case = ((tag, src), keys, ()) =>
  switch (ParsedCorpus.to_segment(~root=Exp, src)) {
  | None => fail("unparseable: " ++ src)
  | Some(seg) =>
    switch (type_after_item(0, keys, seg)) {
    | None => fail("could not type " ++ keys)
    | Some(z) =>
      check_parity(
        Printf.sprintf("%S above a %s", keys, tag),
        Zipper.unselect_and_zip(~erase_buffer=true, z),
      )
    }
  };

/* the memo serves both the Exp (decorations) and editor-root (semantics)
   readings: each sort gets its own completion, in either order */
let sort_keyed_case = () =>
  switch (ParsedCorpus.to_segment(~root=Exp, "(a")) {
  | None => fail("unparseable: (a")
  | Some(seg) =>
    let uncached = sort =>
      CanonicalCompletion.complete_item_uncached(~sort, seg).completed_seg;
    let (exp, pat) = (uncached(Exp), uncached(Pat));
    check(
      bool,
      "readings differ by sort",
      false,
      Segment.equiv_mod_grout(exp, pat),
    );
    let same = (name, expected, sort) =>
      check(
        bool,
        name,
        true,
        Segment.equiv_mod_grout(
          expected,
          CanonicalCompletion.complete_items(~sort, seg).completed_seg,
        ),
      );
    same("Exp reading", exp, Exp);
    same("Pat reading after Exp", pat, Pat);
    same("Exp reading after Pat", exp, Exp);
    same("Pat reading again", pat, Pat);
  };

/* the per-item caches: a live set past the bound stays cached, and
   superseded versions don't grow the bound */
let swept_live = () => {
  module Swept = CanonicalCompletion.Swept;
  let t: Swept.t(int, int) = Swept.mk(~bound=8, ());
  let pass = () => {
    Swept.next_pass(t);
    List.fold_left(
      (misses, k) =>
        switch (Swept.find_opt(t, k)) {
        | Some(_) => misses
        | None =>
          Swept.replace(t, k, k);
          misses + 1;
        },
      0,
      List.init(20, k => k),
    );
  };
  check(int, "first pass fills", 20, pass());
  check(int, "second pass hits", 0, pass());
};

let swept_stale = () => {
  module Swept = CanonicalCompletion.Swept;
  let t: Swept.t(int, int) = Swept.mk(~bound=512, ());
  for (i in 1 to 1000) {
    Swept.next_pass(t);
    List.iter(
      k =>
        if (Swept.find_opt(t, k) == None) {
          Swept.replace(t, k, k);
        },
      [0, 1],
    );
    /* an edit: a new version of some item */
    Swept.replace(t, 100 + i, i);
  };
  check(int, "bound kept", 512, t.bound);
  check(bool, "live kept", true, Swept.find_opt(t, 0) == Some(0));
};

let tests = (
  "CompletionItems",
  List.map(
    ((name, _) as e) => test_case(name, `Quick, edge_case(e)),
    edge_programs,
  )
  @ List.concat_map(
      ((tag, _) as tail) =>
        List.map(
          keys =>
            test_case(
              Printf.sprintf("open slot: %S above a %s", keys, tag),
              `Quick,
              open_slot_case(tail, keys),
            ),
          open_slot_keys,
        ),
      open_slot_tails,
    )
  @ [
    test_case("memo keyed by sort", `Quick, sort_keyed_case),
    test_case("a live set past the bound stays cached", `Quick, swept_live),
    test_case("edits don't grow the bound", `Quick, swept_stale),
    test_case("mega-1k parity", `Quick, corpus_case("mega-1k.hz")),
    test_case("mega-2k parity", `Quick, corpus_case("mega-2k.hz")),
  ],
);
