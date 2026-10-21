open Alcotest;
open Haz3lcore;

let theorem_cands = marked =>
  Test_Refactor.drag_cands(marked)
  |> List.filter((c: Refactor.DragCandidate.t) =>
       switch (c.kind) {
       | RewriteTheorem(_) => true
       | _ => false
       }
     );

let rewrite = (~name, ~direction, marked) => {
  let z = Test_Editing.parse_zipper(marked);
  let target = Option.get(Indicated.index(z));
  let info_map = Test_Refactor.info_map_of(z);
  let r =
    RefactorTheorem.rules_at(~info_map, ~target)
    |> List.find((r: RefactorTheorem.rule) => r.name == name);
  Refactor.dedupe_healed := 0;
  let result =
    Test_Editing.perform(
      z,
      [RefactorPath([(RewriteTheorem(r.id, direction, target), target)])],
    );
  Test_Refactor.assert_unique_ids(result);
  check(int, "no duplicate ids healed", 0, Refactor.dedupe_healed^);
  let term = MakeTerm.from_zip_for_sem(result, ~root=Exp).term;
  check(bool, "printed result reparses", true, Refactor.reparses_same(term));
  result;
};

let check_rewrite =
    (~name="t", ~direction=Util.Direction.Right, marked, expected) =>
  check(
    string,
    "source",
    expected,
    rewrite(~name, ~direction, marked) |> Test_Refactor.text_of,
  );

let fusion_prefix = "let inc(n : Int) = n + 1 in let double(n : Int) = n * 2 in let xs = [1, 2, 3] in theorem fusion = forall (items : [Int]) -> forall (f : Int -> Int) -> forall (g : Int -> Int) -> map(map(items, f), g) == map(items, fun x -> g(f(x))) in ";

let tests = [
  (
    "Theorem Refactor",
    [
      test_case("fusion handles include both functions", `Quick, () => {
        ["inc", "double"]
        |> List.iter(name => {
             let body =
               name == "inc"
                 ? "map(map(xs, ¦inc), double)"
                 : "map(map(xs, inc), ¦double)";
             let z = Test_Editing.parse_zipper(fusion_prefix ++ body);
             let target = Indicated.index(z) |> Option.get;
             let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
             let info_map = Test_Refactor.info_map_of(z);
             let measured =
               Measured.of_segment(
                 Zipper.unselect_and_zip(z),
                 ProjectorCore.Shape.Map.empty,
                 Id.Map.empty,
               );
             let cs =
               Refactor.drag_candidates(
                 ~settings=Test_Editing.default_settings,
                 ~info_map,
                 ~term,
                 ~measured,
                 ~target,
                 ~push=false,
                 ~shard=Indicated.shard_index(z),
                 (),
               )
               |> List.filter((c: Refactor.DragCandidate.t) =>
                    switch (c.kind) {
                    | RewriteTheorem(_) => true
                    | _ => false
                    }
                  );
             check(int, name ++ " offers fusion", 1, List.length(cs));
             let c = List.hd(cs);
             check(bool, "nonzero track", true, c.current != c.target);
             check(
               bool,
               "held function id survives",
               true,
               Id.Map.mem(target, c.measured.tiles),
             );
             let committed =
               Refactor.go_path(
                 ~settings=Test_Editing.default_settings,
                 ~info_map,
                 ~term,
                 [(c.kind, c.at)],
                 z,
               )
               |> Option.get;
             Test_Refactor.assert_unique_ids(committed);
             check(
               string,
               "preview equals release",
               Printer.of_segment(~holes="?", ~refractors=[], c.segment),
               Test_Refactor.text_of(committed),
             );
           })
      }),
      test_case(
        "fusion is grabbable by the inner map name",
        `Quick,
        () => {
          let marked = fusion_prefix ++ "map(¦map(xs, inc), double)";
          let z = Test_Editing.parse_zipper(marked);
          let target = Option.get(Indicated.index(z));
          let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
          let info_map = Test_Refactor.info_map_of(z);
          let measured =
            Measured.of_segment(
              Zipper.unselect_and_zip(z),
              ProjectorCore.Shape.Map.empty,
              Id.Map.empty,
            );
          let cs =
            Refactor.drag_candidates(
              ~settings=Test_Editing.default_settings,
              ~info_map,
              ~term,
              ~measured,
              ~target,
              ~push=false,
              ~shard=Indicated.shard_index(z),
              (),
            )
            |> List.filter((c: Refactor.DragCandidate.t) =>
                 switch (c.kind) {
                 | RewriteTheorem(_) => true
                 | _ => false
                 }
               );
          check(int, "one fusion", 1, List.length(cs));
          let c = List.hd(cs);
          let outer =
            RefactorBase.find_path(~hit=RefactorBase.hit_node(target), term)
            |> Option.get
            |> List.find(e =>
                 switch (Language.Exp.term_of(e)) {
                 | Ap(_) => true
                 | _ => false
                 }
               );
          let head =
            switch (Language.Exp.term_of(outer)) {
            | Ap(_, f, _) => f
            | _ => assert(false)
            };
          let landing =
            Measured.find_by_id(Language.Exp.rep_id(head), measured)
            |> Option.get;
          check(
            bool,
            "lands on outer map's live position",
            true,
            c.target == landing.origin,
          );
          check(
            bool,
            "held map id survives",
            true,
            Id.Map.mem(target, c.measured.tiles),
          );
          Refactor.dedupe_healed := 0;
          let committed =
            Refactor.go_path(
              ~settings=Test_Editing.default_settings,
              ~info_map,
              ~term,
              [(c.kind, c.at)],
              z,
            )
            |> Option.get;
          Test_Refactor.assert_unique_ids(committed);
          check(int, "no ids healed", 0, Refactor.dedupe_healed^);
          check(
            string,
            "preview equals release",
            Printer.of_segment(~holes="?", ~refractors=[], c.segment),
            Test_Refactor.text_of(committed),
          );
          let expected =
            Parser.to_segment(
              fusion_prefix ++ "map(xs, fun x -> double(inc(x)))",
              ~root=Exp,
            )
            |> Option.get
            |> MakeTerm.go;
          check(
            bool,
            "fused program",
            true,
            Refactor.eq_defs(
              expected.term,
              MakeTerm.from_zip_for_sem(committed, ~root=Exp).term,
            ),
          );
        },
      ),
      test_case("tokens inside a theorem argument are passengers", `Quick, () =>
        check(
          int,
          "no ancestor commute through a nested argument",
          0,
          List.length(
            theorem_cands(
              "theorem swap = forall (x : Int, y : Int) -> x - y == y - x in (¦1 + 2) - 3",
            ),
          ),
        )
      ),
      test_case(
        "an argument can drive an enclosing swap",
        `Quick,
        () => {
          let cs =
            theorem_cands(
              "theorem swap = forall (x : Int, y : Int) -> x - y == y - x in ¦1 - 3",
            );
          check(bool, "swap offered at the operand", true, cs != []);
          List.iter(
            (c: Refactor.DragCandidate.t) =>
              check(bool, "nonzero track", true, c.current != c.target),
            cs,
          );
        },
      ),
      test_case(
        "IdMatch default keeps its conservative counts",
        `Quick,
        () => {
          let parse = s => Parser.to_segment(s, ~root=Exp) |> Option.get;
          let old_seg = parse("(21, 22)");
          let new_seg = parse("(20, 22)");
          let (result, stats) =
            IdMatch.transplant_with_stats(~old_seg, ~new_seg, ());
          check(int, "original default count", 4, stats.preserved);
          check(
            bool,
            "shape unchanged",
            true,
            IdMatch.same_seg(result, new_seg),
          );
          check(bool, "accepted", false, stats.rejected);
        },
      ),
      test_case("comments survive erased operands", `Quick, () =>
        check_rewrite(
          "theorem t = forall (x : Int) -> x + 0 == x in 3 # keep # ¦+ 0",
          "theorem t = forall (x : Int) -> x + 0 == x in # keep # 3",
        )
      ),
      test_case("theorem comments are not copied into the rewrite", `Quick, () =>
        check_rewrite(
          ~direction=Left,
          "theorem t = forall (x : Int) -> x + # rule # 0 == x in ¦3",
          "theorem t = forall (x : Int) -> x + # rule # 0 == x in 3 + 0",
        )
      ),
      test_case(
        "different numeric mode cannot preview an invalid rewrite", `Quick, () =>
        check(
          int,
          "no candidates",
          0,
          List.length(
            theorem_cands(
              "theorem t = forall (x : Int) -> 0 * x == 0 in use Nat in ¦0",
            ),
          ),
        )
      ),
      test_case(
        "a surviving argument keeps its syntax identity",
        `Quick,
        () => {
          let z =
            Test_Editing.parse_zipper(
              "theorem t = forall (x : Int) -> x + 0 == x in 3 ¦+ 0",
            );
          let target = Option.get(Indicated.index(z));
          let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
          let info_map = Test_Refactor.info_map_of(z);
          let r = RefactorTheorem.rules_at(~info_map, ~target) |> List.hd;
          let value =
            RefactorBase.find_hit(~hit=RefactorBase.hit_node(target), term)
            |> Option.get
            |> Language.Exp.term_of
            |> (
              fun
              | BinOp(_, x, _) => x
              | _ => assert(false)
            );
          let result =
            Test_Editing.perform(
              z,
              [
                RefactorPath([
                  (RewriteTheorem(r.id, Right, target), target),
                ]),
              ],
            );
          check(
            bool,
            "argument id survives",
            true,
            Test_Refactor.collect_tile_ids(result)
            |> List.mem(Language.Exp.rep_id(value)),
          );
        },
      ),
      test_case(
        "every preview alternative passes the release checks",
        `Quick,
        () => {
          let marked = "theorem zero = forall (x : Int) -> 0 * x == 0 in theorem add = forall (x : Int) -> x + 0 == x in ¦0";
          let z = Test_Editing.parse_zipper(marked);
          let target = Option.get(Indicated.index(z));
          let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
          let info_map = Test_Refactor.info_map_of(z);
          let measured =
            Measured.of_segment(
              Zipper.unselect_and_zip(z),
              ProjectorCore.Shape.Map.empty,
              Id.Map.empty,
            );
          Refactor.drag_candidates(
            ~settings=Test_Editing.default_settings,
            ~info_map,
            ~term,
            ~measured,
            ~push=false,
            ~target,
            ~shard=Indicated.shard_index(z),
            (),
          )
          |> List.iter((c: Refactor.DragCandidate.t) =>
               switch (c.kind) {
               | RewriteTheorem(_) =>
                 let result =
                   Refactor.go_path(
                     ~settings=Test_Editing.default_settings,
                     ~info_map,
                     ~term,
                     [(c.kind, c.at)],
                     z,
                   )
                   |> Option.get;
                 Test_Refactor.assert_unique_ids(result);
                 check(
                   string,
                   "preview equals release",
                   Printer.of_segment(~holes="?", ~refractors=[], c.segment),
                   Test_Refactor.text_of(result),
                 );
               | _ => ()
               }
             );
        },
      ),
      test_case("forward equality", `Quick, () =>
        check_rewrite(
          "theorem t = forall (x : Int) -> x + 0 == x in 3 ¦+ 0",
          "theorem t = forall (x : Int) -> x + 0 == x in 3",
        )
      ),
      test_case("reverse rewrite introduces a hole", `Quick, () =>
        check_rewrite(
          ~direction=Left,
          "theorem t = forall (x : Int) -> 0 * x == 0 in ¦0",
          "theorem t = forall (x : Int) -> 0 * x == 0 in 0 * ?",
        )
      ),
      test_case(
        "unmatched variable does not capture a same-named value", `Quick, () =>
        check_rewrite(
          ~direction=Left,
          "theorem t = forall (x : Int) -> 0 * x == 0 in fun (x : Int) -> ¦0",
          "theorem t = forall (x : Int) -> 0 * x == 0 in fun (x : Int) -> 0 * ?",
        )
      ),
      test_case(
        "repeated undetermined occurrences have distinct hole ids", `Quick, () =>
        check_rewrite(
          ~direction=Left,
          "theorem t = forall (x : Int) -> 0 * (x + x) == 0 in ¦0",
          "theorem t = forall (x : Int) -> 0 * (x + x) == 0 in 0 * (? + ?)",
        )
      ),
      test_case("substitution is simultaneous", `Quick, () =>
        check_rewrite(
          "theorem t = forall (x : Int, y : Int) -> x - y == y - x in fun (x : Int, y : Int) -> y ¦- x",
          "theorem t = forall (x : Int, y : Int) -> x - y == y - x in fun (x : Int, y : Int) -> x - y",
        )
      ),
      test_case("substitution parenthesizes a non-atomic argument", `Quick, () =>
        check_rewrite(
          "theorem t = forall (x : Int) -> x + x == 2 * x in (1 + 2) ¦+ (1 + 2)",
          "theorem t = forall (x : Int) -> x + x == 2 * x in 2 * (1 + 2)",
        )
      ),
      test_case(
        "replacement parenthesized in its surrounding program", `Quick, () =>
        check_rewrite(
          ~direction=Left,
          "theorem t = forall (x : Int) -> x + 0 == x in 2 * ¦3",
          "theorem t = forall (x : Int) -> x + 0 == x in 2 * (3 + 0)",
        )
      ),
      test_case("replacement freshens a capturing lambda binder", `Quick, () =>
        check_rewrite(
          ~direction=Left,
          "theorem t = forall (x : Int) -> (fun (y : Int) -> x)(0) == x in fun (y : Int) -> ¦y",
          "theorem t = forall (x : Int) -> (fun (y : Int) -> x)(0) == x in fun (y : Int) -> (fun (y1:Int) -> y)(0)",
        )
      ),
      test_case("only the grabbed occurrence changes", `Quick, () =>
        check_rewrite(
          "theorem t = forall (x : Int) -> x + 0 == x in (3 + 0, 4 ¦+ 0)",
          "theorem t = forall (x : Int) -> x + 0 == x in (3 + 0, 4)",
        )
      ),
      test_case("out-of-scope theorem absent", `Quick, () =>
        check(
          int,
          "no candidates",
          0,
          List.length(
            theorem_cands(
              "(¦0, theorem t = forall (x : Int) -> 0 * x == 0 in 0)",
            ),
          ),
        )
      ),
      test_case("theorem unavailable in its own statement", `Quick, () =>
        check(
          int,
          "no candidates",
          0,
          List.length(
            theorem_cands(
              "theorem t = forall (x : Int) -> ¦0 * x == 0 in 0",
            ),
          ),
        )
      ),
      test_case("value binding shadows a theorem", `Quick, () =>
        check(
          int,
          "no candidates",
          0,
          List.length(
            theorem_cands(
              "theorem t = forall (x : Int) -> 0 * x == 0 in let t = 5 in ¦0",
            ),
          ),
        )
      ),
      test_case("shadowed free name disables rule", `Quick, () =>
        check(
          int,
          "no candidates",
          0,
          List.length(
            theorem_cands(
              "let a = 1 in theorem t = a + 0 == a in let a = 2 in a ¦+ 0",
            ),
          ),
        )
      ),
      test_case("unrelated shadowing keeps rule usable", `Quick, () =>
        check_rewrite(
          "let a = 1 in theorem t = a + 0 == a in let b = 2 in a ¦+ 0",
          "let a = 1 in theorem t = a + 0 == a in let b = 2 in a",
        )
      ),
      test_case("metavariable cannot escape a matched lambda", `Quick, () =>
        check(
          int,
          "no candidates",
          0,
          List.length(
            theorem_cands(
              "theorem t = forall (z : Int) -> (fun (x : Int) -> z) == (fun (y : Int) -> z) in fun (x : Int) -> (¦fun (x : Int) -> x)",
            ),
          ),
        )
      ),
      test_case("quantifier type checked", `Quick, () =>
        check(
          int,
          "no candidates",
          0,
          List.length(
            theorem_cands(
              "theorem t = forall (x : Int) -> x + 0 == x in ¦true",
            ),
          ),
        )
      ),
      test_case("non-equation not a source rewrite", `Quick, () =>
        check(
          int,
          "no candidates",
          0,
          List.length(
            theorem_cands("theorem t = forall (x : Int) -> x > 0 in ¦0"),
          ),
        )
      ),
      test_case("ill-typed statement not a source rewrite", `Quick, () =>
        check(
          int,
          "no candidates",
          0,
          List.length(theorem_cands("theorem t = 0 == true in ¦0")),
        )
      ),
      test_case("identity rewrite omitted", `Quick, () =>
        check(
          int,
          "no candidates",
          0,
          List.length(
            theorem_cands("theorem t = forall (x : Int) -> x == x in ¦3"),
          ),
        )
      ),
      test_case(
        "in-place alternatives have distinct tracks and labels",
        `Quick,
        () => {
          let cs =
            theorem_cands(
              "theorem zero = forall (x : Int) -> 0 * x == 0 in theorem add = forall (x : Int) -> x + 0 == x in ¦0",
            );
          check(int, "two choices", 2, List.length(cs));
          let targets =
            cs |> List.map((c: Refactor.DragCandidate.t) => c.target);
          check(
            int,
            "distinct endpoints",
            2,
            List.length(List.sort_uniq(compare, targets)),
          );
          cs
          |> List.iter((c: Refactor.DragCandidate.t) => {
               check(bool, "nonzero track", true, c.current != c.target);
               check(
                 bool,
                 "label includes direction and result",
                 true,
                 String.length(c.label) > 8,
               );
             });
        },
      ),
      test_case(
        "drag preview agrees with committed source",
        `Quick,
        () => {
          let marked = "theorem t = forall (x : Int) -> x + 0 == x in 3 ¦+ 0";
          let c = theorem_cands(marked) |> List.hd;
          let committed = rewrite(~name="t", ~direction=Right, marked);
          check(
            string,
            "preview equals commit",
            Test_Refactor.text_of(committed),
            Printer.of_segment(~holes="?", ~refractors=[], c.segment),
          );
        },
      ),
      test_case(
        "theorem rewrites never grow ladders",
        `Quick,
        () => {
          let marked = "theorem t = forall (x : Int) -> x + 0 == x in ¦3";
          let z = Test_Editing.parse_zipper(marked);
          let target = Option.get(Indicated.index(z));
          let c = theorem_cands(marked) |> List.hd;
          check(
            bool,
            "no repeated application",
            true,
            Refactor.next_rung(
              ~settings=Test_Editing.default_settings,
              ~info_map=Test_Refactor.info_map_of(z),
              ~root=MakeTerm.from_zip_for_sem(z, ~root=Exp).term,
              ~push=false,
              ~family=c.kind,
              ~held=(target, None),
              c,
            )
            == None,
          );
        },
      ),
    ],
  ),
];
