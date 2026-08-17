/* Runner for the test suites that still depend on the web library
   (js_of_ocaml-only); run under node via run_node.sh. Everything else
   runs natively via haz3ltest.re. */
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
    "HazelWebTests",
    Test_AgentTools.tests
    @ Test_AgentMultiTool.tests
    @ Test_AgentControlFlow.tests
    @ [Test_AgentUX.tests]
    @ Test_WorkerServer.tests
    @ Test_PromptFactory.tests
    @ Test_ReparseDocSlides.tests
    @ Test_TextRoundtrip.tests
    @ [
      Test_LazyHydration.tests,
      Test_GradingReport.tests,
      Test_Derivation.tests,
      Test_StepperBase.tests,
      Test_ExplainThis.tests,
    ],
  );
Junit.to_file(Junit.make([suite]), "junit_tests_web.xml");
Bisect.Runtime.write_coverage_data();
