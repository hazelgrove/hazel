/* IncrEval.copy_descendant_entries stops at function bodies, where no
   entry is ever recorded: on a real cache it copies exactly what the full
   walk (kept here as the reference) copies. */
open Alcotest;
open Language;

let program =
  String.concat(
    "",
    List.init(30, i =>
      Printf.sprintf(
        "let f%d = fun x -> let y = x + %d in (y, [y, y * 2]) in\nlet r%d = (a = f%d(%d), b = [1, 2, %d], c = (if %d > 3 then (4, 5) else (6, 7))) in\nlet s%d = let t = r%d in (t, %d) in\n",
        i,
        i,
        i,
        i,
        i,
        i,
        i,
        i,
        i,
        i,
      )
    ),
  )
  ++ "(r1, s2)";

let tests = (
  "IncrEval.copy_descendant_entries",
  [
    test_case(
      "copies what the full walk copies",
      `Quick,
      () => {
        let term =
          switch (
            Haz3lcore.PersistentZipper.parse_text(
              ~source="t",
              ~root=Exp,
              program,
            )
          ) {
          | None => fail("did not parse")
          | Some(z) => Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp).term
          };
        let (info_map, elab) =
          Statics.mk(
            CoreSettings.on,
            Builtins.ctx_init(Some(Operators.default_mode)),
            term,
          );
        let eval_info =
          EvalInfo.of_info_map(
            ~probe_all=false,
            ~targets=Sample.no_targets,
            info_map,
          );
        let (_, state) =
          Evaluator.evaluate(~eval_info, ~env=Builtins.env_init, elab);
        let prev = state.incr_eval;
        let reference = (~root_id, ~root: Exp.t) => {
          let acc = ref([]);
          let _ =
            TermBase.Exp.map_term(
              ~f_exp=
                (continue, e: Exp.t) => {
                  let id = Exp.rep_id(e);
                  if (!Id.equal(id, root_id) && Id.Map.mem(id, prev.entries)) {
                    acc := [id, ...acc^];
                  };
                  continue(e);
                },
              root,
            );
          List.sort_uniq(Id.compare, acc^);
        };
        let roots = Id.Map.bindings(prev.entries);
        let with_descendants = ref(0);
        let mismatches = ref(0);
        List.iter(
          ((root_id, entry: IncrEval.entry(EvaluatorState.t))) => {
            let want = reference(~root_id, ~root=entry.prev_elab);
            let got =
              IncrEval.copy_descendant_entries(
                ~root_id,
                ~root=entry.prev_elab,
                ~prev,
                IncrEval.empty,
              ).
                entries
              |> Id.Map.bindings
              |> List.map(fst)
              |> List.sort_uniq(Id.compare);
            if (want != []) {
              incr(with_descendants);
            };
            if (!List.equal(Id.equal, want, got)) {
              incr(mismatches);
            };
          },
          roots,
        );
        check(
          bool,
          "some entries have descendants",
          true,
          with_descendants^ > 10,
        );
        check(int, "same entries as the full walk", 0, mismatches^);
      },
    ),
  ],
);
