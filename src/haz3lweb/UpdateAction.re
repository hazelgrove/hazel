// open Sexplib.Std;
// open Util;
// open Haz3lcore;
// [@deriving (show({with_path: false}), sexp, yojson)]
// type agent =
//   | TyDi;
// [@deriving (show({with_path: false}), sexp, yojson)]
// type agent_action =
//   | Prompt(agent)
//   | AcceptSuggestion;
// [@deriving (show({with_path: false}), sexp, yojson)]
// type benchmark_action =
//   | Start
//   | Finish;
// [@deriving (show({with_path: false}), sexp, yojson)]
// type t =
//   /* meta */
//   | Globals(Globals.Update.t)
//   | Reset
//   | UpdateExplainThisModel(ExplainThisUpdate.update)
//   | ExportPersistentData
//   | DebugConsole(string)
//   // | MakeActive(Editors.Selection.t)
//   /* editors */
//   // | Editors(Editors.Update.t)
//   | InitImportAll([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
//   | FinishImportAll(option(string))
//   // editors: scratchmode only
//   | InitImportScratchpad([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
//   | FinishImportScratchpad(option(string))
//   /* editor */
//   | TAB
//   | Save
//   | ReparseCurrentEditor
//   | Cut
//   | Copy
//   | Undo
//   | Redo
//   | MoveToNextHole(Direction.t)
//   | Benchmark(benchmark_action)
//   | Assistant(agent_action)
//   | UpdateEvals(WorkerServer.Response.t);
// // | UpdateResult(ModelResults.t);
// module Failure = {
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t =
//     | CantReset
//     | CantSuggest
//     | FailedToLoad
//     | FailedToSwitch
//     | FailedToPerform(Action.Failure.t)
//     | Exception(string);
// };
// module Result = {
//   include Result;
//   type t('success) = Result.t('success, Failure.t);
// };
// let is_edit: t => bool =
//   fun
//   | Editors(
//       Perform(a) | Scratch(MainEditor(a)) | Documentation(MainEditor(a)) |
//       Exercises(_, MainEditor(a)),
//     ) =>
//     Action.is_edit(a)
//   | Globals(Set(s_action)) =>
//     switch (s_action) {
//     | Mode(_) => true
//     | Captions
//     | SecondaryIcons
//     | Statics
//     | Assist
//     | Elaborate
//     | ExplainThis(_)
//     | Dynamics
//     | Benchmark
//     | ContextInspector
//     | InstructorMode
//     | Evaluation(_) => false
//     }
//   | Globals(
//       SetMousedown(_) | SetShowBackpackTargets(_) | SetFontMetrics(_) |
//       JumpToTile(_),
//     ) =>
//     false
//   | Cut
//   | Undo
//   | Redo
//   | Editors(
//       SwitchScratchSlide(_) | SwitchDocumentationSlide(_) |
//       Scratch(ResultAction(_)) |
//       Documentation(ResultAction(_)) |
//       Exercises(_, ResultAction(_)),
//     )
//   | ReparseCurrentEditor
//   | FinishImportAll(_)
//   | FinishImportScratchpad(_)
//   | Editors(ResetCurrentEditor)
//   | Assistant(AcceptSuggestion)
//   | Reset => true
//   | UpdateResult(_)
//   | UpdateEvals(_)
//   | MakeActive(_)
//   | ExportPersistentData
//   | Save
//   | Copy
//   | UpdateExplainThisModel(_)
//   | DebugConsole(_)
//   | InitImportAll(_)
//   | InitImportScratchpad(_)
//   | MoveToNextHole(_)
//   | Benchmark(_)
//   | TAB
//   | Assistant(Prompt(_)) => false;
// let reevaluate_post_update: t => bool =
//   fun
//   | Editors(
//       Perform(a) | Scratch(MainEditor(a)) | Documentation(MainEditor(a)) |
//       Exercises(_, MainEditor(a)),
//     ) =>
//     Action.is_edit(a)
//   | Globals(Set(s_action)) =>
//     switch (s_action) {
//     | Assist
//     | Captions
//     | SecondaryIcons
//     | Statics
//     | ContextInspector
//     | Benchmark
//     | ExplainThis(_)
//     | Evaluation(
//         ShowCaseClauses | ShowFnBodies | ShowCasts | ShowRecord | ShowFixpoints |
//         ShowLookups |
//         ShowFilters |
//         ShowSettings |
//         ShowHiddenSteps,
//       ) =>
//       false
//     | Elaborate
//     | Dynamics
//     | InstructorMode
//     | Mode(_) => true
//     }
//   | Globals(
//       SetMousedown(_) | SetShowBackpackTargets(_) | SetFontMetrics(_) |
//       JumpToTile(_),
//     ) =>
//     false
//   | Assistant(AcceptSuggestion) => true
//   | Assistant(Prompt(_)) => false
//   | MoveToNextHole(_)
//   | Save
//   | Copy
//   | InitImportAll(_)
//   | InitImportScratchpad(_)
//   | UpdateExplainThisModel(_)
//   | ExportPersistentData
//   | UpdateResult(_)
//   | UpdateEvals(_)
//   | MakeActive(_)
//   | DebugConsole(_)
//   | TAB
//   | Benchmark(_) => false
//   | Editors(_)
//   | ReparseCurrentEditor
//   | FinishImportAll(_)
//   | FinishImportScratchpad(_)
//   | Reset
//   | Cut
//   | Undo
//   | Redo => true;
// let should_scroll_to_caret =
//   fun
//   | Globals(Set(s_action)) =>
//     switch (s_action) {
//     | Mode(_) => true
//     | Captions
//     | SecondaryIcons
//     | Statics
//     | Assist
//     | Elaborate
//     | ExplainThis(_)
//     | Dynamics
//     | Benchmark
//     | ContextInspector
//     | InstructorMode
//     | Evaluation(_) => false
//     }
//   | Globals(SetFontMetrics(_)) => true
//   | Globals(SetMousedown(_) | SetShowBackpackTargets(_) | JumpToTile(_)) =>
//     false
//   | Assistant(Prompt(_))
//   | UpdateResult(_)
//   | UpdateEvals(_)
//   | Editors(
//       Scratch(ResultAction(_)) | Documentation(ResultAction(_)) |
//       Exercises(_, ResultAction(_)),
//     ) =>
//     false
//   | Assistant(AcceptSuggestion) => true
//   | FinishImportScratchpad(_)
//   | FinishImportAll(_)
//   | Editors(ResetCurrentEditor)
//   | MakeActive(_)
//   | Editors(SwitchScratchSlide(_))
//   | Editors(SwitchDocumentationSlide(_))
//   | ReparseCurrentEditor
//   | Reset
//   | Copy
//   | Cut
//   | Undo
//   | Redo
//   | MoveToNextHole(_)
//   | TAB => true
//   | Editors(
//       Perform(a) | Scratch(MainEditor(a)) | Documentation(MainEditor(a)) |
//       Exercises(_, MainEditor(a)),
//     ) =>
//     switch (a) {
//     | Move(_)
//     | MoveToNextHole(_)
//     | Jump(_)
//     | Select(Resize(_) | Term(_) | Smart | Tile(_))
//     | Destruct(_)
//     | Insert(_)
//     | Pick_up
//     | Put_down
//     | RotateBackpack
//     | MoveToBackpackTarget(_)
//     | Paste(_) => true
//     | Unselect(_)
//     | Select(All)
//     | Suggest(_)
//     | ResetSuggestion => false
//     }
//   | Save
//   | InitImportAll(_)
//   | InitImportScratchpad(_)
//   | UpdateExplainThisModel(_)
//   | ExportPersistentData
//   | DebugConsole(_)
//   | Benchmark(_) => false;
