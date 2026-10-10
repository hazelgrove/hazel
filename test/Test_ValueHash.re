/* Util.ValueHash: a Merkle hash of an immutable value by its structure,
   ids included, remembered per object, and answers remembered per pair.
   Equality's ~hash_shortcut uses both, to recognize identical subterms
   without walking them and to answer a repeated comparison from memory,
   which made the eval worker's reuse check cheap (IncrEval.reuse_check).
   So the hash must never call two different values the same, and the
   hashed comparison must agree with the plain one, asked once or again. */
open Alcotest;
open Haz3lcore;
open Language;

let elab = text =>
  switch (PersistentZipper.parse_text(~source="t", ~root=Exp, text)) {
  | None => fail("did not parse")
  | Some(z) =>
    let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Exp);
    snd(Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term));
  };

/* A deep copy, as unmarshaling a request makes one: new objects, same
   structure, same ids. */
let copy = (e: Exp.t): Exp.t =>
  Marshal.from_string(Marshal.to_string(e, []), 0);

let program = "let f = fun x -> x + 1 in\nlet g = fun (y : Int) -> f(y) * 2 in\n(g(3), [f(1), f(2)], \"text\")";
let edited = "let f = fun x -> x + 1 in\nlet g = fun (y : Int) -> f(y) * 3 in\n(g(3), [f(1), f(2)], \"text\")";

let tests = (
  "ValueHash",
  [
    test_case(
      "a copy hashes the same",
      `Quick,
      () => {
        let e = elab(program);
        let c = copy(e);
        check(bool, "separate objects", false, e === c);
        check(bool, "same", true, Util.ValueHash.same(e, c));
      },
    ),
    test_case(
      "a different value does not",
      `Quick,
      () => {
        check(
          bool,
          "1 vs 2",
          false,
          Util.ValueHash.same([1, 2, 3], [1, 2, 4]),
        );
        check(bool, "strings", false, Util.ValueHash.same("ab", "ba"));
        check(
          bool,
          "nested",
          false,
          Util.ValueHash.same([[1], [2]], [[2], [1]]),
        );
        /* the same text elaborated twice: fresh ids, so not the same build */
        check(
          bool,
          "fresh ids",
          false,
          Util.ValueHash.same(elab(program), elab(program)),
        );
      },
    ),
    test_case(
      "a function has no hash: never the same",
      `Quick,
      () => {
        let f = (x: int) => x + 1;
        check(
          bool,
          "two closures",
          false,
          Util.ValueHash.same((1, f), (1, f)),
        );
      },
    ),
    test_case(
      "the hashed comparison agrees with fast_equal",
      `Quick,
      () => {
        let e = elab(program);
        let c = copy(e);
        let d = elab(edited);
        check(bool, "copy: plain", true, Exp.fast_equal(e, c));
        check(bool, "copy: hashed", true, Exp.fast_equal_hashed(e, c));
        check(bool, "edited: plain", false, Exp.fast_equal(e, d));
        check(bool, "edited: hashed", false, Exp.fast_equal_hashed(e, d));
        check(
          bool,
          "re-elaborated: hashed agrees with plain",
          Exp.fast_equal(e, elab(program)),
          Exp.fast_equal_hashed(e, elab(program)),
        );
        /* asked again: answered from memory, the same */
        check(bool, "copy, again", true, Exp.fast_equal_hashed(e, c));
        check(bool, "edited, again", false, Exp.fast_equal_hashed(e, d));
      },
    ),
    test_case(
      "pairs: an answer is remembered for exactly that pair",
      `Quick,
      () => {
        let m = Util.ValueHash.Pairs.create();
        let a = [1, 2];
        let b = [1, 2];
        let c = [1, 2];
        check(
          option(bool),
          "unknown",
          None,
          Util.ValueHash.Pairs.find(m, a, b),
        );
        Util.ValueHash.Pairs.add(m, a, b, true);
        check(
          option(bool),
          "a, b",
          Some(true),
          Util.ValueHash.Pairs.find(m, a, b),
        );
        check(
          option(bool),
          "a, c",
          None,
          Util.ValueHash.Pairs.find(m, a, c),
        );
        check(
          option(bool),
          "b, a",
          None,
          Util.ValueHash.Pairs.find(m, b, a),
        );
        Util.ValueHash.Pairs.add(m, a, c, false);
        check(
          option(bool),
          "a, c now",
          Some(false),
          Util.ValueHash.Pairs.find(m, a, c),
        );
        check(
          option(bool),
          "not an object",
          None,
          {
            Util.ValueHash.Pairs.add(m, 3, 3, true);
            Util.ValueHash.Pairs.find(m, 3, 3);
          },
        );
      },
    ),
  ],
);
