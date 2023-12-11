open Haz3lcore;
open Sexplib.Std;
open QueryCommand;

module ExecutionPlan = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(QueryCommand.t);
};

let query_parser = (command: string, editor: Editor.t): ExecutionPlan.t =>
  switch (command, editor) {
  | _ => [Query(DoNothing)]
  };


module QueryResult = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    result: string,
    textobject: option(Term.t),
  };
};

let execute = (plan: ExecutionPlan.t, editor: Editor.t): QueryResult.t => {
  switch(plan, editor) {
  | _ => {result: "nothing", textobject: None}
  }
};

let execute_command = (command: QueryCommand.t, editor: Editor.t): QueryResult.t => {
  switch(command, editor) {
    | _ => {result: "nothing", textobject: None}
  }
}