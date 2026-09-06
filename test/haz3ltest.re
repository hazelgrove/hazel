open Junit_alcotest;

Printexc.register_printer(exn => {
  switch (exn) {
  | Language.EvaluatorError.Exception(msg) =>
    Some(Language.EvaluatorError.show(msg))
  | _ => None
  }
});

/* every editing action in the suite asserts sparse-normalize parity
   (Zipper.remold_regrout runs BOTH pipelines and compares) */
Haz3lcore.Zipper.normalize_parity := true;

/* run_and_report always runs Alcotest with and_exit=false so it can produce a
   report, and hands the exit back as a function. ~and_exit=true makes that
   function exit with the test status rather than raise Test_error. */
let (suite, exit_with_test_status) =
  run_and_report(
    ~and_exit=true,
    ~argv=Sys.argv,
    "HazelTests",
    [
      Test_AgentPersist.tests,
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
      Test_PatRootEditor.tests,
      Test_StackFocus.tests,
      Test_Restructure.tests,
      Test_BenchStatics.tests,
      Test_MegaCorpus.tests,
      Test_MeasuredChunks.tests,
      Test_MakeTermIncr.tests,
      Test_PieceIdentity.tests,
      Test_ClickTeleport.tests,
      Test_AliasProbe.tests,
      Test_LabelBench.tests,
      Test_FlatBench.tests,
      Test_ModRoot.tests,
      Test_TypeDeps.tests,
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
    @ Test_ItemPersist.tests
    @ Test_OutlinePaths.tests
    @ Test_RunPin.tests
    @ Test_Reassociate.tests
    @ Test_MultiProbe.tests
    @ [Test_SampleSelection.tests]
    @ [("Canvas anatomy", Test_CanvasAnatomy.tests)]
    @ [("Canvas values", Test_CanvasValue.tests)]
    @ [("Graph layout stability", Test_GraphLayoutStability.tests)]
    @ [("Canvas graph fold", Test_CanvasGraphFold.tests)]
    @ [("Node map (dungeon program)", Test_MergeProbe.tests)]
    @ [("Canvas score", Test_CanvasScore.tests)]
    @ [("Sample focus liveness", Test_SampleFocusLiveness.tests)]
    @ Test_Indentation.tests
    @ Test_CanonicalCompletion.tests
    @ Test_CompletionScoreboard.tests
    @ Test_CompletionVisualization.tests
    @ [Test_Coverage.tests, Test_Unboxing.tests]
    @ Test_ProblemCollection.tests
    @ [Test_TermData.tests]
    @ Test_Introduce.tests
    @ Test_ReparseDocSlides.tests
    @ Test_StreamInterests.tests
    @ Test_TextRoundtrip.tests
    @ Test_RoundtripFuzz.tests
    @ Test_LocalReformat.tests
    @ Test_Refactor.tests
    @ Test_MatchExp.tests
    @ Test_RefractorSerialization.tests
    @ [
      Test_MVU.tests,
      Test_TableCore.tests,
      Test_TableTransforms.tests,
      Test_RichProbeRegistry.tests,
    ]
    @ Test_UserLivelits.tests
    @ Test_PrettyPrint.tests
    @ Test_TyDi.tests
    @ [Test_UnusedWarnings.tests]
    @ Test_Indication.tests
    @ Test_Autoprobe.tests
    @ [
      Test_VarHighlight.tests,
      Test_Evaluator_ProbeNav.tests,
      Test_StepProvenance.tests,
      Test_ObsTraceShadow.tests,
      Test_ObsBench.tests,
    ]
    @ [Test_GradingReport.tests]
    @ [Test_Derivation.tests]
    @ Test_DerivationCase.tests
    @ [Test_ShardCrashRepro.tests]
    @ Test_PromptFactory.tests
    @ [Test_ExplainThis.tests],
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
Bisect.Runtime.write_coverage_data();

/* Must be last, and is the only thing that turns a failing test into a non-zero
   exit status. */
exit_with_test_status();
