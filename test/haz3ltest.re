open Junit_alcotest;

Printexc.register_printer(exn => {
  switch (exn) {
  | Language.EvaluatorError.Exception(msg) =>
    Some(Language.EvaluatorError.show(msg))
  | _ => None
  }
});

let (suite, _) =
  run_and_report(
    ~and_exit=false,
    ~argv=Sys.argv,
    "HazelTests",
    [
      Test_ListUtil.tests,
      Test_OptUtil.tests,
      Test_Grammar.tests,
      Test_ExpToSegment.tests,
      Test_LabeledTuple.tests,
      Test_MakeTerm.tests,
      Test_Menhir.tests,
      Test_StringUtil.tests,
      Test_PatternMatch.tests,
      Test_Id_Transform.tests,
    ]
    @ Test_Typ.tests
    @ Test_Info.tests
    @ Test_Statics.tests
    @ Test_Elaboration.tests
    @ Test_Evaluator.tests
    @ Test_Editing.tests
    @ Test_AutoProbe.tests
    @ Test_Indentation.tests
    @ [Test_Coverage.tests, Test_Unboxing.tests]
    @ Test_Introduce.tests
    @ Test_ReparseDocSlides.tests
    @ Test_MatchExp.tests,
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
Bisect.Runtime.write_coverage_data();
