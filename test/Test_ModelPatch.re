/* (^ticket(721)) A livelit's commit that changes only its model's literals:
   the model keeps its ids (ModelIds.same_shape_ids), the changed literals are
   found (ModelIds.literal_changes), and the statics' terms are copied only
   along the path to each (ModelPatch.along). The whole patch, on a slide's
   statics, is exercised by the Firefox check on the Witch slide. */
open Alcotest;
open Haz3lcore;
open Language;

module E = IdTagged.FreshGrammar.Exp;

let ids = (e: Exp.t): list(Id.t) =>
  switch (e.term) {
  | Tuple(es) => [Exp.rep_id(e), ...List.map(Exp.rep_id, es)]
  | _ => [Exp.rep_id(e)]
  };

let tests = (
  "ModelPatch",
  [
    test_case(
      "same_shape_ids: the old model's ids, the new model's values",
      `Quick,
      () => {
        let old = E.tuple([E.int(1), E.string("a")]);
        let new_ = E.tuple([E.int(2), E.string("b")]);
        switch (ModelIds.same_shape_ids(old, new_)) {
        | None => fail("the same shape refused")
        | Some(r) =>
          check(bool, "the old ids", true, ids(r) == ids(old));
          switch (ModelIds.literal_changes(old, r)) {
          | Some(changes) =>
            check(int, "both literals changed", 2, List.length(changes));
            check(
              bool,
              "the changed literals are the old ones' ids",
              true,
              List.map(fst, changes) == List.tl(ids(old)),
            );
          | None => fail("changes not found")
          };
        };
      },
    ),
    test_case(
      "same_shape_ids: a different shape keeps the new ids",
      `Quick,
      () => {
        let old = E.tuple([E.int(1), E.int(2)]);
        check(
          bool,
          "a longer tuple",
          true,
          ModelIds.same_shape_ids(
            old,
            E.tuple([E.int(1), E.int(2), E.int(3)]),
          )
          == None,
        );
        check(
          bool,
          "a literal of another class",
          true,
          ModelIds.same_shape_ids(E.int(1), E.string("1")) == None,
        );
      },
    ),
    test_case(
      "literal_changes: none for the same model, and None for other ids",
      `Quick,
      () => {
        let old = E.tuple([E.int(1), E.int(2)]);
        check(
          bool,
          "no change",
          true,
          ModelIds.literal_changes(old, old) == Some([]),
        );
        check(
          bool,
          "fresh ids",
          true,
          ModelIds.literal_changes(old, E.tuple([E.int(1), E.int(3)]))
          == None,
        );
      },
    ),
    test_case(
      "along: only the path to the target is copied",
      `Quick,
      () => {
        let a = E.int(1);
        let inner1 = E.tuple([a, E.int(2)]);
        let inner2 = E.tuple([E.int(3), E.int(4)]);
        let outer = E.tuple([inner1, inner2]);
        let path = id =>
          List.mem(id, [Exp.rep_id(outer), Exp.rep_id(inner1)]);
        switch (
          ModelPatch.along(
            ~path,
            ~target=Exp.rep_id(a),
            ModelPatch.set_literal(E.int(9)),
            outer,
          )
        ) {
        | None => fail("the target not met")
        | Some(r) =>
          switch (r.term) {
          | Tuple([r1, r2]) =>
            check(bool, "off the path: the same object", true, r2 === inner2);
            switch (r1.term) {
            | Tuple([x, _]) =>
              check(
                bool,
                "the target's new value",
                true,
                Exp.fast_equal(x, E.int(9)),
              );
              check(
                bool,
                "the target's id kept",
                true,
                Exp.rep_id(x) == Exp.rep_id(a),
              );
            | _ => fail("the inner tuple lost")
            };
          | _ => fail("the outer tuple lost")
          }
        };
      },
    ),
    test_case(
      "along: a target off the path is not met",
      `Quick,
      () => {
        let a = E.int(1);
        let outer = E.tuple([E.tuple([a, E.int(2)]), E.int(3)]);
        check(
          bool,
          "None",
          true,
          ModelPatch.along(
            ~path=_ => false,
            ~target=Exp.rep_id(a),
            ModelPatch.set_literal(E.int(9)),
            outer,
          )
          == None,
        );
      },
    ),
  ],
);
