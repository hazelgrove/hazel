/* Incremental evaluation of the Livelit Playground slides against fresh
   evaluation, after the edit a slider makes: one int literal, a livelit's
   model, changed in place with every id kept (as the projector's commit
   keeps them). The slides found an incremental run that came back stuck
   where a fresh one completes. */
open Alcotest;
open Language;

let playground = "hazel-programs/docs/livelits/playground/";

let elab_of = (src: string) => {
  let seg = Test_BenchStatics.parse_seg_of(src);
  let term = Haz3lcore.MakeTerm.go(seg).term;
  let (map, elab) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      term,
    );
  (term, map, elab);
};

/* every Atom(Int n) with n = [value], by its rep id */
let int_lits = (value: int, exp: Exp.t): list(Id.t) => {
  let acc = ref([]);
  let f_exp = (continue, e: Exp.t): Exp.t => {
    switch (e.term) {
    | Atom(Int(n)) when Bigint.equal(n, Bigint.of_int(value)) =>
      acc := [Exp.rep_id(e), ...acc^]
    | _ => ()
    };
    continue(e);
  };
  let _ = TermBase.Exp.map_term(~f_exp, exp);
  List.rev(acc^);
};

let replace_int = (~target: Id.t, ~to_: int, exp: Exp.t): Exp.t =>
  TermBase.Exp.map_term(
    ~f_exp=
      (continue, e: Exp.t) =>
        if (Id.equal(Exp.rep_id(e), target)) {
          switch (e.term) {
          | Atom(Int(_)) => {
              ...e,
              term: Atom(Int(Bigint.of_int(to_))),
            }
          | _ => continue(e)
          };
        } else {
          continue(e);
        },
    exp,
  );

let eval = (~prev=IncrEval.empty, map, elab) => {
  let ei =
    EvalInfo.of_info_map(~probe_all=false, ~targets=Sample.no_targets, map);
  Evaluator.evaluate(~prev, ~eval_info=ei, ~env=Builtins.env_init, elab);
};

let summary = (e: Exp.t): string => {
  let (pruned, _) = TermPrune.prune(~budget=60, e);
  let s = Exp.show(pruned);
  String.length(s) > 1500 ? String.sub(s, 0, 1500) ++ "..." : s;
};

/* [slide]'s int literal [from] (the [nth] one) set to [to_]: the
   incremental result is the fresh one */
let case = (~slide, ~from, ~to_, ~nth=0, ()) => {
  let path = playground ++ slide ++ ".hz";
  let path = Sys.file_exists(path) ? path : "../" ++ path;
  let src =
    switch (CorpusUtil.read_file(path)) {
    | Some(src) => src
    | None => failwith("no slide at " ++ path)
    };
  let (term, map, elab) = elab_of(src);
  let (_, state) = eval(map, elab);
  switch (List.nth_opt(int_lits(from, term), nth)) {
  | None => fail(Printf.sprintf("%s: no int literal %d", slide, from))
  | Some(target) =>
    let edited = replace_int(~target, ~to_, term);
    let (map2, elab2) =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        edited,
      );
    let (fresh, _) = eval(map2, elab2);
    let (incr, _) = eval(~prev=state.incr_eval, map2, elab2);
    let same = Equality.semantic.exp(fresh, incr);
    if (!same) {
      Printf.printf(
        "%s %d -> %d\n  fresh: %s\n  incr:  %s\n",
        slide,
        from,
        to_,
        summary(fresh),
        summary(incr),
      );
    };
    check(bool, slide ++ ": incremental agrees with fresh", true, same);
  };
};

let tests = (
  "IncrSlides",
  [
    /* each slide's first ^choice is an inline use with a type
       argument, Drawing.^choice@<T>, whose type application's result
       is an open function: reused as a value, it stuck the run */
    test_case(
      "Arms: arm length",
      `Quick,
      case(~slide="arms", ~from=95, ~to_=150),
    ),
    test_case(
      "Hands: hand size",
      `Quick,
      case(~slide="hands", ~from=14, ~to_=30),
    ),
    test_case(
      "Shoes: shoe size",
      `Quick,
      case(~slide="shoes", ~from=100, ~to_=200),
    ),
    test_case(
      "Eyes: eye size",
      `Quick,
      case(~slide="eyes", ~from=50, ~to_=100),
    ),
  ],
);
