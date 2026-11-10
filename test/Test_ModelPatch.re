/* (^ticket(721)) A livelit's commit that changes only its model's literals:
   the model keeps its ids (ModelIds.same_shape_ids), the changed literals are
   found (ModelIds.literal_changes), and the statics' terms are copied only
   along the path to each (ModelPatch.along); and an item patched, not
   analyzed, gives the statics a cold analysis of the edited program gives
   (ModelPatch.item_patch). A livelit's use, the patch inside its expansion,
   is exercised by the Firefox check on the Witch slide. */
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
      "item_patch: a literal changed, the statics a cold analysis makes",
      `Quick,
      () => {
        let statics_of = z =>
          CachedStatics.init_compositional(
            ~settings=CoreSettings.on,
            ~stitch=x => x,
            ~root=Sort.Exp,
            z,
          );
        let seg = CorpusUtil.seg_of("let x = 1 in\nlet y = 10 in\nx + y");
        DefStatics.slots := [];
        let _ = statics_of(Zipper.unzip(seg));
        /* the literal 1 retyped as 5, its tile's id kept */
        let one = ref(None);
        let seg' =
          ZipperBase.MapPiece.of_segment(
            (p: Piece.t) =>
              switch (p) {
              | Tile(t) when Tile.label(t) == ["1"] =>
                one := Some(t.id);
                [
                  Tile({
                    ...t,
                    form: Form.Tok("5"),
                  }),
                ];
              | p => [p]
              },
            seg,
          );
        let z' = Zipper.unzip(seg');
        let changes: ModelPatch.changes = {
          c_projector: None,
          c_literals: [Option.get(one^)],
        };
        let patched =
          ModelPatch.with_item_patch(changes, () => statics_of(z'));
        check(int, "one item patched", 1, DefStatics.items_patched^);
        DefStatics.slots := [];
        let cold = statics_of(z');
        check(
          bool,
          "the same elaboration",
          true,
          Exp.fast_equal(patched.elaborated, cold.elaborated),
        );
        check(
          int,
          "the same keys",
          Id.Map.cardinal(cold.info_map),
          Id.Map.cardinal(patched.info_map),
        );
        Id.Map.iter(
          (k, info) =>
            switch (info, Id.Map.find_opt(k, patched.info_map)) {
            | (Info.InfoExp(c), Some(Info.InfoExp(p))) =>
              check(
                bool,
                "a record's term",
                true,
                Exp.fast_equal(c.user_term, p.user_term),
              );
              check(
                bool,
                "a record's elaborated term",
                true,
                Exp.fast_equal(c.elab_term, p.elab_term)
                && Exp.lexeme_trace(c.elab_term)
                == Exp.lexeme_trace(p.elab_term),
              );
              check(
                bool,
                "a record's type",
                true,
                Typ.fast_equal(c.ty, p.ty),
              );
            | (_, Some(_)) => ()
            | (_, None) => fail("a key missing from the patched statics")
            },
          cold.info_map,
        );
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
