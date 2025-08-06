[@deriving (show({with_path: false}), sexp, yojson)]
type benchmark_action =
  | Start
  | Finish;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Globals(Globals.Update.t)
  | Editors(Editors.Update.t)
  | ExplainThis(ExplainThisUpdate.update)
  | Assistant(AssistantUpdate.t)
  | MakeActive(PageModel.selection)
  | Benchmark(benchmark_action)
  | Start
  | Save;

let equal = (===);
