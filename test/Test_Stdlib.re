open Alcotest;
open Haz3lcore;
open Language;

/* The shipped standard library (plans/stdlib.md), installed at startup
   (haz3ltest.re). */

let ctx = () => Builtins.ctx_init(Some(Operators.default_mode));

/* [src]'s value, checked against the library first */
let eval = (src: string): Exp.t => {
  let (map, elab) =
    Statics.mk(
      CoreSettings.on,
      ctx(),
      Test_Evaluator_Prelude.parse_exp(src),
    );
  check(
    int,
    "static errors in: " ++ src,
    0,
    List.length(Statics.Map.error_ids(map)),
  );
  Test_Evaluator_Prelude.evaluate(elab);
};

let same = (msg, src, expected) =>
  check(Test_Evaluator_Prelude.dhexp_typ, msg, eval(expected), eval(src));

let tests = (
  "Stdlib",
  [
    test_case(
      "modules are source, primitives come through them",
      `Quick,
      () => {
        same("List", "List.length(List.map([1, 2, 3], fun x -> x + 1))", "3");
        same("String, Int", {|String.length("abc") + Int.mod(7, 3)|}, "4");
        same("Option", "Option.map(Some(2), fun x -> x * 10)", "Some(20)");
      },
    ),
    /* #2685: the OCaml-built list functions resolved each other at the
       call site */
    test_case("a program's own drop doesn't change List.slice", `Quick, () =>
      same(
        "slice",
        "let drop = fun (xs, n) -> xs in List.slice(1, 3, [10, 20, 30, 40])",
        "[20, 30]",
      )
    ),
    /* a module as a value (a sample of `List`, typed on the way to
       `List.map`) once shipped as the library's whole List, every message */
    test_case(
      "a library module shows and ships as its name",
      `Quick,
      () => {
        let env = Builtins.env_init();
        let list = Option.get(Environment.lookup(env, "List"));
        check(
          bool,
          "named",
          true,
          Library.name_modules(list).term == Var("List"),
        );
        check(
          bool,
          "elided in a sample's environment",
          true,
          Sample.Env.elide(env, list) == Opaque,
        );
      },
    ),
    test_case(
      "a program evaluates only the definitions it mentions",
      `Quick,
      () => {
        /* afresh: nothing evaluated yet */
        StdlibBaked.install();
        let (_, elab) =
          Statics.mk(
            CoreSettings.on,
            ctx(),
            Test_Evaluator_Prelude.parse_exp("List.length([1, 2])"),
          );
        let env = Builtins.env_for(elab);
        let evaluated = x => List.mem_assoc(x, Builtins.library_modules^);
        check(bool, "List", true, evaluated("List"));
        check(bool, "not Color", false, evaluated("Color"));
        check(
          bool,
          "Color not bound",
          true,
          Environment.lookup(env, "Color") == None,
        );
      },
    ),
    /* the shipped library must be clean; an edit applies with its errors,
       as any program with errors runs */
    test_case(
      "a library with static errors: strict refuses, edits apply",
      `Quick,
      () => {
        let src = "module A = { let x = 1 + true };\nlet y = 2";
        check(
          bool,
          "strict: an error",
          true,
          Result.is_error(LibraryBake.of_text(~strict=true, src)),
        );
        switch (LibraryBake.of_text(~strict=false, src)) {
        | Error(msg) => fail("an edit with errors is refused: " ++ msg)
        | Ok((baked, errors, _)) =>
          check(int, "its errors, counted", 1, errors);
          Library.install_baked(baked);
          /* evaluates past the error, as a program's own would */
          same("y", "y + 1", "3");
          /* the shipped library again, for later tests */
          StdlibBaked.install();
        };
      },
    ),
    /* §9b: library code that keeps its editor's ids is probed like the
       program's own; the samples come from the program's calls */
    test_case(
      "a probe in the library samples the program's calls",
      `Quick,
      () => {
        let src = "module L = {\n  let length = fun xs -> case xs | [] => 0 | _ :: xs => 1 + length(xs) end\n};\nlet y = 1";
        let items =
          switch (
            MakeTerm.Incr.term_of_mod(CorpusUtil.seg_of(~root=Mod, src)).term
          ) {
          | Module(items) => items
          | _ => fail("not module items")
          };
        /* the recursive call, `length(xs)`, by its editor id */
        let call = ref(None);
        let _ =
          Exp.map_term(
            ~f_exp=
              (cont, e: Exp.t) => {
                switch (e.term) {
                | Ap(_, {term: Var("length"), _}, _) =>
                  call := Some(Exp.rep_id(e))
                | _ => ()
                };
                cont(e);
              },
            Exp.fresh(Module(items)),
          );
        let call = Option.get(call^);
        switch (
          LibraryBake.of_items(~strict=true, ~compact=false, ~hash="t", items)
        ) {
        | Error(msg) => fail(msg)
        | Ok({baked, _}) =>
          Library.install_baked(baked);
          let (_, elab) =
            Statics.mk(
              CoreSettings.on,
              ctx(),
              Test_Evaluator_Prelude.parse_exp("L.length([1, 2, 3])"),
            );
          let (_, state) =
            Evaluator.evaluate(
              ~eval_info=
                EvalInfo.of_targets(
                  Id.Map.singleton(call, Sample.empty_capture_spec),
                ),
              ~env=Builtins.env_for(elab),
              elab,
            );
          let samples =
            Option.value(~default=[], Id.Map.find_opt(call, state.probes));
          check(int, "one per recursive call", 3, List.length(samples));
          StdlibBaked.install();
        };
      },
    ),
    test_case(
      "a new starting context re-analyzes every item",
      `Quick,
      () => {
        let term =
          MakeTerm.Incr.term_of(
            CorpusUtil.seg_of("let a = 1 in\nlet b = a + 1 in\nb"),
          );
        let settings = CoreSettings.on;
        let first = DefStatics.calc(~settings, ~ctx_in=ctx(), term);
        let _ = DefStatics.calc(~settings, ~ctx_in=ctx(), ~prev=first, term);
        check(int, "same context: nothing", 0, DefStatics.last_analyzed^);
        let _ =
          DefStatics.calc(
            ~settings,
            ~ctx_in=Builtins.ctx_base(Some(Operators.default_mode)),
            ~prev=first,
            term,
          );
        check(bool, "new context: all", true, DefStatics.last_analyzed^ > 0);
      },
    ),
  ],
);
