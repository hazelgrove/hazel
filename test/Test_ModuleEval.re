open Alcotest;
open Haz3lcore;
open Language;

/* After an edit, evaluating with the previous run's incremental cache
   must agree with a cold evaluation (compositional statics). */

let settings = CoreSettings.on;

let parse_exp = (src: string): Exp.t =>
  switch (FastParse.of_text(~root=Exp, src)) {
  | Some(seg) => MakeTerm.Incr.term_of(seg)
  | None => fail("exp parse failed")
  };

/* an id-preserving literal edit, as the editor makes one */
let replace_int = (~from: int, ~to_: int, exp: Exp.t): Exp.t => {
  let f_exp = (continue, e: Exp.t): Exp.t =>
    switch (e.term) {
    | Atom(Int(n)) when Bigint.to_string(n) == string_of_int(from) => {
        annotation: e.annotation,
        term: Atom(Int(Bigint.of_int(to_))),
      }
    | _ => continue(e)
    };
  TermBase.Exp.map_term(~f_exp, exp);
};

let eval = (~prev=IncrEval.empty, ds: DefStatics.t) => {
  let elab =
    switch (DefStatics.whole_elab(ds)) {
    | Some(e) => e
    | None => fail("whole_elab")
    };
  let eval_info =
    EvalInfo.of_info_map(~probe_all=false, ~targets=Id.Map.empty, ds.merged);
  let (result, state) =
    Evaluator.evaluate(~prev, ~eval_info, ~env=Builtins.env_init, elab);
  (Exp.show(result), state.incr_eval);
};

let incremental_matches_cold = (t0: Exp.t, t1: Exp.t) => {
  let ds0 = DefStatics.calc(~settings, t0);
  let (v0, cache0) = eval(ds0);
  let ds1 = DefStatics.calc(~settings, ~prev=ds0, t1);
  let (incr, _) = eval(~prev=cache0, ds1);
  let (cold, _) = eval(ds1);
  check(bool, "the edit changes the result", true, v0 != cold);
  check(string, "incremental = cold", cold, incr);
};

let literal_edit = (src, ~from, ~to_, ()) => {
  let t0 = parse_exp(src);
  incremental_matches_cold(t0, replace_int(~from, ~to_, t0));
};

let corpus_member_edit = () =>
  switch (CorpusUtil.corpus_seg("mega-1k.hz")) {
  | None => fail("corpus unreadable")
  | Some(seg0) =>
    let (seg1, found) =
      CorpusUtil.edit_token(~needle="180", ~repl="181", seg0);
    check(bool, "literal found", true, found);
    incremental_matches_cold(
      MakeTerm.Incr.term_of(seg0),
      MakeTerm.Incr.term_of(seg1),
    );
  };

let tests = (
  "ModuleEval",
  [
    test_case(
      "top-level edit",
      `Quick,
      literal_edit("let a = 1 in\nlet b = a + 10 in\nb", ~from=1, ~to_=5),
    ),
    test_case(
      "module member edit",
      `Quick,
      literal_edit(
        "module M = {\nlet a = 1;\nlet b = a + 10\n} in\nM.b",
        ~from=1,
        ~to_=5,
      ),
    ),
    test_case(
      "let-bound module literal edit",
      `Quick,
      literal_edit(
        "let m = {\nlet a = 1;\nlet b = a + 10\n} in\nm.b",
        ~from=1,
        ~to_=5,
      ),
    ),
    test_case(
      "upstream edit used by a member",
      `Quick,
      literal_edit(
        "let k = 1 in\nmodule M = {\nlet b = k + 10\n} in\nM.b",
        ~from=1,
        ~to_=5,
      ),
    ),
    test_case("mega-1k member edit", `Quick, corpus_member_edit),
  ],
);
