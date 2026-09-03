open Alcotest;
open Haz3lcore;

/* ItemPersist: per-item persistence core. The decisive gates:
   EXACT restore (incomplete tiles and grout included — the reason
   this exists: text persistence cannot express them fast-parseably),
   dirty-only writes, GC ordering, and fallback (None) on any
   inconsistency. */

let mem_store = (): (ItemPersist.store, ref(list((string, string)))) => {
  let tbl: Hashtbl.t(string, string) = Hashtbl.create(16);
  let log = ref([]);
  (
    {
      get: k => Hashtbl.find_opt(tbl, k),
      set: (k, v) => {
        log := [("set", k), ...log^];
        Hashtbl.replace(tbl, k, v);
      },
      remove: k => {
        log := [("remove", k), ...log^];
        Hashtbl.remove(tbl, k);
      },
    },
    log,
  );
};

let sets = log =>
  List.filter(((op, _)) => op == "set", log^) |> List.length;

let zip_of_text = (text: string): Zipper.t =>
  switch (MarkerParse.of_text(~root=Sort.Exp, text)) {
  | Some(z) => z
  | None => failwith("parse failed: " ++ text)
  };

let seg_of_text = (text: string): Segment.t =>
  Zipper.unselect_and_zip(~erase_buffer=true, zip_of_text(text));

let seg_equal = (a: Segment.t, b: Segment.t): bool =>
  Sexplib.Sexp.compare(Segment.sexp_of_t(a), Segment.sexp_of_t(b)) == 0;

let doc = "let a : Int = 1 in\nlet b : Int = a + 1 in\nlet c : Int = b * 2 in\na + b + c";

let cases = [
  test_case(
    "round-trip is exact",
    `Quick,
    () => {
      let (store, _) = mem_store();
      let seg = seg_of_text(doc);
      let _ = ItemPersist.save(~store, ~prev=[], seg);
      switch (ItemPersist.load(~store)) {
      | None => fail("load returned None")
      | Some(seg') =>
        check(bool, "segments equal", true, seg_equal(seg, seg'))
      };
    },
  ),
  test_case(
    "incomplete tile and infix hole restore exactly",
    `Quick,
    () => {
      /* the driver for serialized persistence: these are the states the
         text fast parse cannot express (pre-completion-provenance) */
      let (store, _) = mem_store();
      let seg =
        seg_of_text(
          "let x : Int = 1 \xe2\xa7\x96 2 in\nlet y = (3 + in\nx + y",
        );
      let _ = ItemPersist.save(~store, ~prev=[], seg);
      switch (ItemPersist.load(~store)) {
      | None => fail("load returned None")
      | Some(seg') =>
        check(bool, "segments equal", true, seg_equal(seg, seg'))
      };
    },
  ),
  test_case(
    "second save of unchanged doc writes only the roster",
    `Quick,
    () => {
      let (store, log) = mem_store();
      let seg = seg_of_text(doc);
      let saved = ItemPersist.save(~store, ~prev=[], seg);
      let before = sets(log);
      let _ = ItemPersist.save(~store, ~prev=saved, seg);
      check(int, "one write (roster only)", before + 1, sets(log));
    },
  ),
  test_case(
    "editing one item writes one item + roster",
    `Quick,
    () => {
      let (store, log) = mem_store();
      let seg = seg_of_text(doc);
      let saved = ItemPersist.save(~store, ~prev=[], seg);
      /* mutate the SECOND item only (append a space), preserving its
         leading piece and the other items' physical identity, as the
         editing discipline does */
      let items = ItemPersist.items_of(seg);
      let seg' =
        List.concat(
          List.mapi(
            (i, (_, s)) =>
              i == 1
                ? s @ [Piece.Secondary(Secondary.mk_space(Id.mk()))] : s,
            items,
          ),
        );
      let before = sets(log);
      let _ = ItemPersist.save(~store, ~prev=saved, seg');
      check(int, "two writes (item + roster)", before + 2, sets(log));
    },
  ),
  test_case(
    "removed item is GCed after the roster write",
    `Quick,
    () => {
      let (store, log) = mem_store();
      let seg = seg_of_text(doc);
      let saved = ItemPersist.save(~store, ~prev=[], seg);
      let items = ItemPersist.items_of(seg);
      let seg' = List.concat(List.map(snd, List.tl(items)));
      let _ = ItemPersist.save(~store, ~prev=saved, seg');
      let ops = List.rev(log^);
      let removed = List.filter(((op, _)) => op == "remove", ops);
      check(int, "one key removed", 1, List.length(removed));
      /* the remove comes after the last set (roster-last ordering) */
      let rec last_set_before_remove = (ops, seen_remove) =>
        switch (ops) {
        | [] => true
        | [("set", _), ...rest] =>
          !seen_remove && last_set_before_remove(rest, seen_remove)
        | [("remove", _), ...rest] => last_set_before_remove(rest, true)
        | [_, ...rest] => last_set_before_remove(rest, seen_remove)
        };
      check(
        bool,
        "no set after a remove",
        true,
        last_set_before_remove(ops, false),
      );
      switch (ItemPersist.load(~store)) {
      | None => fail("load returned None after GC")
      | Some(seg'') =>
        check(bool, "post-GC load equals", true, seg_equal(seg', seg''))
      };
    },
  ),
  test_case(
    "missing item key falls back (None)",
    `Quick,
    () => {
      let (store, _) = mem_store();
      let seg = seg_of_text(doc);
      let saved = ItemPersist.save(~store, ~prev=[], seg);
      switch (saved) {
      | [(id, _), ..._] =>
        store.remove(ItemPersist.item_key(id));
        check(bool, "load is None", true, ItemPersist.load(~store) == None);
      | [] => fail("no items saved")
      };
    },
  ),
  test_case(
    "corrupt roster falls back (None)",
    `Quick,
    () => {
      let (store, _) = mem_store();
      let seg = seg_of_text(doc);
      let _ = ItemPersist.save(~store, ~prev=[], seg);
      store.set(ItemPersist.roster_key, "not a roster");
      check(bool, "load is None", true, ItemPersist.load(~store) == None);
    },
  ),
  test_case(
    "interrupted save: newer item under old roster still loads",
    `Quick,
    () => {
      /* crash between an item write and the roster write: the old
         roster + a newer same-id same-count item is a benign view */
      let (store, _) = mem_store();
      let seg = seg_of_text(doc);
      let _ = ItemPersist.save(~store, ~prev=[], seg);
      let items = ItemPersist.items_of(seg);
      switch (items) {
      | [(id, s0), ..._] =>
        /* same piece count, different content */
        let replacement = seg_of_text("let a : Int = 2 in");
        if (List.length(replacement) == List.length(s0)) {
          store.set(
            ItemPersist.item_key(id),
            Sexplib.Sexp.to_string(Segment.sexp_of_t(replacement)),
          );
          check(bool, "still loads", true, ItemPersist.load(~store) != None);
        };
      | [] => fail("no items")
      };
    },
  ),
];

let tests = [("ItemPersist", cases)];
