open Junit_alcotest;

Printexc.register_printer(exn => {
  switch (exn) {
  | Language.EvaluatorError.Exception(msg) =>
    Some(Language.EvaluatorError.show(msg))
  | DefStaticsCheck.Divergence(ds) =>
    Some("DefStatics diverges: " ++ String.concat("; ", ds))
  | _ => None
  }
});

/* every edit checks sparse normalization against the global pass */
NormalizeCheck.install();

/* and statics against a monolithic analysis, except when benchmarking */
if (!CorpusUtil.bench_enabled) {
  DefStaticsCheck.install();
};

/* run_and_report always runs Alcotest with and_exit=false so it can produce a
   report, and hands the exit back as a function. ~and_exit=true makes that
   function exit with the test status rather than raise Test_error. */
let (suite, exit_with_test_status) =
  run_and_report(
    ~and_exit=true,
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
      Test_FormId.tests,
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
      Test_EditLocality.tests,
      Test_DefStaticsParity.tests,
      Test_ClickTeleport.tests,
      Test_AliasProbe.tests,
      Test_LabelBench.tests,
      Test_ModRoot.tests,
      Test_ModuleEval.tests,
      Test_ProbePersist.tests,
      Test_DividedLaws.tests,
      Test_TermPrune.tests,
      Test_ResultLine.tests,
      Test_SlideView.tests,
      Test_OutlineRename.tests,
      Test_ClosedJump.tests,
      Test_TypeDeps.tests,
      Test_StaticsMemo.tests,
      Test_CaretReveal.tests,
      Test_Menhir.concave_marker_group,
      Test_StringUtil.tests,
      Test_TaskReferenceSplit.tests,
      Test_TutorialReferencePanel.tests,
      Test_HazelJson_JsonADT.tests,
      Test_PatternMatch.tests,
      Test_Equality.tests,
      Test_Substitution.tests,
    ]
    @ Test_Unicode.tests
    @ Test_WorkerServer.tests
    @ [Test_AgentPersist.tests, Test_AgentHardening.tests]
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
    @ Test_TypToSegment.tests
    @ Test_SerBench.tests
    @ Test_ItemPersist.tests
    @ Test_OutlinePaths.tests
    @ Test_RunPin.tests
    @ Test_Reassociate.tests
    @ Test_MultiProbe.tests
    @ [Test_SampleSelection.tests]
    @ Test_Indentation.tests
    @ Test_DynamicTypInfer.tests
    @ Test_CanonicalCompletion.tests
    @ Test_CompletionScoreboard.tests
    @ Test_CompletionVisualization.tests
    @ Test_QuiverDisplay.tests
    @ Test_TabDispatch.tests
    @ Test_ImpliedHole.tests
    @ Test_CaretPreserving.tests
    @ [Test_Coverage.tests, Test_Unboxing.tests]
    @ Test_ProblemCollection.tests
    @ [Test_TermData.tests]
    @ [Test_CtorShadowing.tests]
    @ Test_Introduce.tests
    @ Test_ReparseDocSlides.tests
    @ Test_StreamInterests.tests
    @ Test_ResidentProgram.tests
    @ Test_W2Protocol.tests
    @ Test_DeriveDeterminism.tests
    @ Test_PropagateClamp.tests
    @ Test_TextRoundtrip.tests
    @ Test_RoundtripFuzz.tests
    @ Test_LocalReformat.tests
    @ Test_MatchExp.tests
    @ Test_RefractorSerialization.tests
    @ [
      Test_TableCore.tests,
      Test_TableTransforms.tests,
      Test_RichProbeRegistry.tests,
    ]
    @ Test_PrettyPrint.tests
    @ Test_TyDi.tests
    @ [Test_Move.tests]
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
    @ Test_SlidePath.tests
    @ Test_Tutorial.tests
    @ Test_TutorialText.tests
    @ [Test_TutorialProbeSettings.tests]
    @ [Test_Derivation.tests]
    @ Test_DerivationCase.tests
    @ [Test_ShardCrashRepro.tests]
    @ Test_PromptFactory.tests
    @ Test_ShortcutConfiguration.tests
    @ Test_ColorConfiguration.tests
    @ Test_ConfigurationMode.tests
    @ Test_ShortcutAction.tests
    @ Test_Color.tests
    @ [Test_ExplainThis.tests]
    @ [Test_CompletionItems.tests]
    /* last: the keystroke benchmark leaves less stack for later tests */
    @ (CorpusUtil.bench_enabled ? [Test_MegaBench.tests] : []),
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
Bisect.Runtime.write_coverage_data();

/* Must be last, and is the only thing that turns a failing test into a non-zero
   exit status. */
exit_with_test_status();
