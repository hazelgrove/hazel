/* TypHash: a Merkle hash of a type, ignoring ids, remembered per type
   object. Separate copies of one type hash alike, which is what lets the
   evaluator answer is_consistent once per pair of types instead of once
   per copy (Ascriptions.consistent_by_hash). */
open Alcotest;
open Language;

let t = (term: Typ.term): Typ.t => Typ.fresh(term);
let ty_int = () => t(Atom(Int));
let ty_bool = () => t(Atom(Bool));
let arrow = (a, b) => t(Arrow(a, b));
let labeled = (l, ty) => t(TupLabel(t(Label(l)), ty));

let hash = (ty: Typ.t): (int, int) =>
  switch (TypHash.hash(ty)) {
  | Some(h) => h
  | None => fail("no hash")
  };

let same = (name, a, b) =>
  test_case(
    name,
    `Quick,
    () => {
      check(bool, "separate objects", false, a === b);
      check(pair(int, int), "hash alike", hash(a), hash(b));
    },
  );

let differ = (name, a, b) =>
  test_case(name, `Quick, () =>
    check(bool, "hash apart", false, hash(a) == hash(b))
  );

let tests = (
  "TypHash",
  [
    same(
      "two copies of a type",
      arrow(ty_int(), ty_bool()),
      arrow(ty_int(), ty_bool()),
    ),
    same(
      "a copy with every id replaced",
      t(Prod([labeled("x", ty_int()), labeled("y", ty_bool())])),
      t(Prod([labeled("x", ty_int()), labeled("y", ty_bool())])),
    ),
    same(
      "an unknown's provenance does not count",
      t(Unknown(Internal)),
      t(Unknown(SynSwitch)),
    ),
    differ("Int and Bool", ty_int(), ty_bool()),
    differ(
      "arguments swapped",
      arrow(ty_int(), ty_bool()),
      arrow(ty_bool(), ty_int()),
    ),
    differ(
      "different labels",
      labeled("x", ty_int()),
      labeled("y", ty_int()),
    ),
    differ("a list and its element", t(List(ty_int())), ty_int()),
    differ(
      "one more field",
      t(Prod([ty_int(), ty_bool()])),
      t(Prod([ty_int(), ty_bool(), ty_int()])),
    ),
    test_case(
      "the same object, asked twice",
      `Quick,
      () => {
        let ty = arrow(ty_int(), t(List(ty_bool())));
        check(pair(int, int), "remembered", hash(ty), hash(ty));
      },
    ),
    test_case("a type holding an expression gets no hash", `Quick, () =>
      check(
        bool,
        "None, so callers compute",
        true,
        TypHash.hash(t(ProofOf(Exp.fresh(Atom(Bool(true)))))) == None,
      )
    ),
  ],
);
