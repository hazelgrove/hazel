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
      Test_LazyHydration.tests,
      Test_Undo.tests,
      Test_FastParseCorpus.tests,
      Test_FastParse.tests,
      Test_MenhirFuzz.tests,
      Test_MenhirCorpus.tests,
      Test_ListUtil.tests,
      Test_OptUtil.tests,
      Test_Atom.tests,
      Test_Operators.tests,
      Test_BuiltinsADT.tests,
      Test_Builtins_String.tests,
      Test_CsvUtil.tests,
      Test_Grammar.tests,
      Test_Abbreviate.tests,
      Test_LabeledTuple.tests,
      Test_MakeTerm.tests,
      Test_Menhir.tests,
      Test_StringUtil.tests,
      Test_HazelJson_JsonADT.tests,
      Test_PatternMatch.tests,
      Test_Equality.tests,
      Test_Substitution.tests,
    ]
    @ Test_Unicode.tests
    @ Test_WorkerServer.tests
    @ Test_AgentTools.tests
    @ Test_AgentMultiTool.tests
    @ Test_AgentControlFlow.tests
    @ [Test_AgentUX.tests]
    @ Test_ExpToSegment.all
    @ Test_Typ.tests
    @ Test_Statics.tests
    @ Test_Elaboration.tests
    @ Test_Evaluator.tests
    @ Test_Editing.tests
    @ Test_Reassociate.tests
    @ Test_MultiProbe.tests
    @ [Test_SampleSelection.tests]
    @ Test_Indentation.tests
    @ [Test_Coverage.tests, Test_Unboxing.tests]
    @ Test_ProblemCollection.tests
    @ [Test_TermData.tests]
    @ Test_Introduce.tests
    @ Test_ReparseDocSlides.tests
    @ Test_TextRoundtrip.tests
    @ Test_MatchExp.tests
    @ Test_RefractorSerialization.tests
    @ [
      Test_TableCore.tests,
      Test_TableTransforms.tests,
      Test_RichProbeRegistry.tests,
    ]
    @ Test_PrettyPrint.tests
    @ Test_TyDi.tests
    @ [Test_UnusedWarnings.tests]
    @ Test_Indication.tests
    @ [Test_VarHighlight.tests]
    @ [Test_GradingReport.tests]
    @ [Test_Derivation.tests]
    @ Test_DerivationCase.tests
    @ Test_PromptFactory.tests
    @ Test_ShortcutConfiguration.tests
    @ Test_ColorConfiguration.tests
    @ Test_ConfigurationMode.tests
    @ Test_ShortcutAction.tests
    @ Test_Color.tests
    @ [Test_ExplainThis.tests],
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
Bisect.Runtime.write_coverage_data();
