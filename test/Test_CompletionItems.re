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

/* nothing to complete: no incomplete tile, and no open slot before a
   member head (whose member completion ends with a `;`) */
let complete = (~sort: Sort.t, item: Segment.t): bool =>
  Segment.incomplete_tiles_deep(item) == []
  && !CanonicalCompletion.has_open_slot(~sort, item);

let check_parity = (~sort=Sort.Exp, name: string, seg: Segment.t): unit => {
  let whole = CanonicalCompletion.complete_segment_deep(~sort, seg);
  let w0 = CanonicalCompletion.items_widened^;
  let items = CanonicalCompletion.complete_items(~sort, seg);
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
  /* items needing no completion keep their pieces, except the ones a
     widening merged into an incomplete predecessor's block, or that an
     open tile above molded into its slot */
  let complete_items =
    Segment.top_items(seg)
    |> List.filter(item =>
         complete(~sort, item)
         && Option.is_none(CanonicalCompletion.remolded(~sort, item))
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
  let again = CanonicalCompletion.complete_items(~sort, seg);
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

let corpus_case = (file: string, ()) => {
  let seg = CorpusUtil.mega_seg(file);
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

let edge_case = ((name, src), ()) => {
  let seg = CorpusUtil.seg_of(src);
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
  switch (type_after_item(0, keys, CorpusUtil.seg_of(src))) {
  | None => fail("could not type " ++ keys)
  | Some(z) =>
    check_parity(
      Printf.sprintf("%S above a %s", keys, tag),
      Zipper.unselect_and_zip(~erase_buffer=true, z),
    )
  };

/* ---- per member: a module literal item, or a let-chain member,
   completes member by member ---- */

let insertions_sorted = (is: list(CanonicalCompletion.insertion)) =>
  List.sort(compare, is);

/* an incomplete item that may read per member: a module literal, or at a
   Mod level a member that is a let chain */
let per_member = (~sort: Sort.t, item: Segment.t): bool =>
  !complete(~sort, item)
  && (
    ItemSpans.module_item(~root=sort, item) != None
    || sort == Sort.Mod
    && ItemSpans.chain_item(item) != None
  );

/* an item read per member as it reads whole: the same segment up to grout,
   shard records, masks and insertions; its complete parts keep their
   pieces, and its incomplete ones that read per member in turn are checked
   so. How many items were read per member, it and its parts */
let rec check_item = (name: string, ~sort: Sort.t, item: Segment.t): int => {
  let whole = CanonicalCompletion.complete_segment_deep(~sort, item);
  let whole_insertions =
    CanonicalCompletion.derive_insertions(
      ~original=item,
      ~records=whole.shard_records,
      whole.completed_seg,
    );
  let read = CanonicalCompletion.items_by_member^;
  let per = CanonicalCompletion.complete_item_uncached(~sort, item);
  let by_member = CanonicalCompletion.items_by_member^ > read;
  check(
    bool,
    name ++ ": completed alike",
    true,
    Segment.equiv_mod_grout(whole.completed_seg, per.completed_seg),
  );
  check(
    bool,
    name ++ ": same shard records",
    true,
    records_sorted(whole.shard_records) == records_sorted(per.shard_records),
  );
  check(
    bool,
    name ++ ": same masks",
    true,
    Id.Map.equal(
      (==),
      CanonicalCompletion.masks_of_records(whole.shard_records),
      CanonicalCompletion.masks_of_records(per.shard_records),
    ),
  );
  check(
    bool,
    name ++ ": same insertions",
    true,
    insertions_sorted(whole_insertions) == insertions_sorted(per.insertions),
  );
  let parts = (~sort, what, before, after) => {
    let kept = List.concat(after);
    List.fold_left(
      (+),
      0,
      List.mapi(
        (k, part) =>
          if (complete(~sort, part)) {
            check(
              bool,
              Printf.sprintf(
                "%s: complete %s %d keeps its pieces",
                name,
                what,
                k,
              ),
              true,
              List.for_all(p => List.memq(p, kept), part),
            );
            0;
          } else if (per_member(~sort, part)) {
            check_item(
              Printf.sprintf("%s %s %d", name, what, k),
              ~sort,
              part,
            );
          } else {
            0;
          },
        before,
      ),
    );
  };
  let split = (f: Segment.t => option((list(Segment.t), _, _))) =>
    switch (f(item), f(per.completed_seg)) {
    | (Some((before, _, _)), Some((after, _, _))) => Some((before, after))
    | (Some(_), None) => fail(name ++ ": completion lost its parts")
    | (None, _) => None
    };
  if (!by_member) {
    0;
  } else {
    switch (split(ItemSpans.module_item(~root=sort, ~line=None))) {
    | Some((before, after)) => 1 + parts(~sort=Mod, "member", before, after)
    | None =>
      switch (split(ItemSpans.chain_item(~line=None))) {
      | Some((before, after)) =>
        1 + parts(~sort=Exp, "chain item", before, after)
      | None => fail(name ++ ": read per member, but has no members")
      }
    };
  };
};

/* every incomplete module literal or let-chain item of a program, and the
   program itself (per item and per member, as whole): how many items were
   read per member */
let check_members = (name: string, ~sort: Sort.t, seg: Segment.t): int => {
  let n = ref(0);
  List.iteri(
    (k, item) =>
      if (per_member(~sort, item)) {
        n :=
          n^ + check_item(Printf.sprintf("%s item %d", name, k), ~sort, item);
      },
    Segment.top_items(seg),
  );
  check_parity(~sort, name, seg);
  n^;
};

/* member-level shapes, as typed, and how many items read per member (a
   module's, and its members') */
let member_programs = [
  (
    "an unclosed paren",
    Sort.Exp,
    `Typed(
      "module M = {\n  let x = 1;\n  let y = (2 + 3;\n  let z = 4\n} in\nM.x",
    ),
    1,
  ),
  (
    "`let` without `=`",
    Sort.Exp,
    `Typed("module M = {\n  let x = 1;\n  let y\n  let z = 4\n} in\nM.x"),
    1,
  ),
  (
    "a missing `;`",
    Sort.Exp,
    `Typed("module M = {\n  let x = 1\n  let y = 2;\n  let z = 4\n} in\nM.x"),
    1,
  ),
  (
    "in a nested module",
    Sort.Exp,
    `Typed(
      "module A = {\n  let a = 1;\n  module B = {\n    let b = (1 +;\n    let c = 2\n  };\n  let d = 3\n} in\nA.a",
    ),
    2,
  ),
  /* typing makes no let-chain member (a `let` there is a member's), so
     one is parsed whole and then broken */
  (
    "a let chain's item",
    Sort.Mod,
    `Unclosed((
      "module M = {\n  let x = 1;\n  let a = 1 in\n  let b = (2 + 3) in\n  a + b;\n  let z = 4\n};\nM.x",
      ["(", ")"],
    )),
    2,
  ),
  /* its open paren takes the member's `;`: the chain completes whole */
  (
    "a let chain's tail",
    Sort.Mod,
    `Unclosed((
      "module M = {\n  let x = 1;\n  let a = 1 in\n  let b = 2 in\n  (a + b);\n  let z = 4\n};\nM.x",
      ["(", ")"],
    )),
    1,
  ),
  /* an incomplete shell completes whole */
  (
    "an unclosed `}`",
    Sort.Exp,
    `Typed("module M = {\n  let x = 1;\n  let y = 2\nlet z = 4 in\nM.x"),
    0,
  ),
];

/* [seg] with its first [label] tile's closing shard gone, as if deleted:
   its child runs on after its opening shard */
let unclose = (label: list(string), seg: Segment.t): Segment.t => {
  let found = ref(false);
  let rec go = (seg: Segment.t): Segment.t =>
    List.concat_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile({children: [kid], _} as t)
            when ! found^ && Tile.label(t) == label =>
          found := true;
          [
            Piece.Tile({
              ...t,
              shards: [0],
              children: [],
            }),
            ...kid,
          ];
        | Tile(t) => [
            Piece.Tile({
              ...t,
              children: List.map(go, t.children),
            }),
          ]
        | p => [p]
        },
      seg,
    );
  let seg = go(seg);
  found^ ? seg : Alcotest.fail("nothing to unclose");
};

let member_case = ((name, root, src, read), ()) =>
  switch (
    switch (src) {
    | `Typed(src) => CorpusUtil.typed_seg(~root, src)
    | `Unclosed(src, label) =>
      Some(unclose(label, CorpusUtil.seg_of(~root, src)))
    }
  ) {
  | None => fail("untypeable: " ++ name)
  | Some(seg) =>
    check(
      int,
      name ++ ": items read per member",
      read,
      check_members(name, ~sort=root, seg),
    );
    /* its module's members as a Mod root's items */
    switch (ItemSpans.module_item(~root, seg)) {
    | Some((members, _, _)) =>
      ignore(
        check_members(
          name ++ " at a Mod root",
          ~sort=Mod,
          List.concat(members),
        ),
      )
    | None => ()
    };
  };

/* the id of the last piece of member [k] of the [m]th module literal at the
   top level */
let member_end = (~root, seg: Segment.t, m: int, k: int): option(Id.t) => {
  let top = ItemSpans.top(~root, seg);
  let modules =
    List.filter(
      j => ItemSpans.members(top, j) != None,
      List.init(Array.length(top.spans), j => j),
    );
  Option.bind(List.nth_opt(modules, m), j =>
    Option.bind(ItemSpans.members(top, j), (ml: ItemSpans.level) =>
      Option.bind(List.nth_opt(ItemSpans.slices(ml), k), member =>
        Option.map(Piece.id, Util.ListUtil.last_opt(member))
      )
    )
  );
};

/* type a new member into a module, key by key, as a user would */
let typing_case = (~root, file: string, m: int, k: int, keys: string, ()) => {
  let seg = CorpusUtil.mega_seg(~root, file);
  switch (member_end(~root, seg, m, k)) {
  | None => fail(file ++ ": no such member")
  | Some(id) =>
    switch (Move.jump_to_side_of_id(Right, Zipper.unzip(seg), id)) {
    | None => fail(file ++ ": could not place the caret")
    | Some(z0) =>
      let syntax = CachedSyntax.init(~root, z0);
      let statics =
        CachedStatics.init_from_term(
          ~settings,
          ~is_dynamic_term=true,
          MakeTerm.from_zip_for_sem(z0, ~root).term,
        );
      let read = ref(0);
      ignore(
        String.fold_left(
          (z, c) => {
            let z =
              switch (
                Perform.go(
                  ~settings,
                  ~statics,
                  ~syntax,
                  ~root,
                  Action.Insert(String.make(1, c)),
                  {
                    zipper: z,
                    col_target: None,
                  },
                )
              ) {
              | Ok(z) => z
              | Error(_) => z
              };
            read :=
              read^
              + check_members(
                  Printf.sprintf("%s + %S", file, String.make(1, c)),
                  ~sort=root,
                  Zipper.unselect_and_zip(~erase_buffer=true, z),
                );
            z;
          },
          z0,
          "\n" ++ keys,
        ),
      );
      check(bool, file ++ ": items read per member", true, read^ > 0);
    }
  };
};

/* the memo serves both the Exp (decorations) and editor-root (semantics)
   readings: each sort gets its own completion, in either order */
let sort_keyed_case = () => {
  let seg = CorpusUtil.seg_of("(a");
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
  module Swept = Util.Swept;
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
  module Swept = Util.Swept;
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
    CorpusUtil.edge_programs,
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
    test_case(
      "typing a member into a mega-1k module",
      `Quick,
      typing_case(~root=Exp, "mega-1k.hz", 1, 1, "let q = (1 + 2"),
    ),
    test_case(
      "typing a member into a mega-mod-1k module",
      `Quick,
      typing_case(~root=Mod, "mega-mod-1k.hz", 2, 0, "let q = (1 + 2"),
    ),
  ]
  @ List.map(
      ((name, _, _, _) as p) =>
        test_case("per member: " ++ name, `Quick, member_case(p)),
      member_programs,
    ),
);
