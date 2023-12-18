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
  type textobject =
    | Term(Term.t)
    | Info(Info.t)
    | Type(Typ.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    result: string,
    t_object: option(textobject),
  };

  let empty = {result: "nothing", t_object: None};
  let error = (msg: string) => {result: msg, t_object: None};

  let mk = (result, t_object): t => {
    {result, t_object};
  };
};

let execute = (plan: ExecutionPlan.t, editor: Editor.t): QueryResult.t => {
  switch (plan, editor) {
  | _ => {result: "nothing", t_object: None}
  };
};

let execute_command = // TODO: change the input to zipper and info_map
    (
      ~settings: Settings.t,
      ~ctx_init,
      command: QueryCommand.t,
      result: QueryResult.t,
      editor: Editor.t,
    )
    : QueryResult.t => {
  switch (command, result) {
  | (Select(Term), _) =>
    let zipper = editor.state.zipper;
    let (term, _) = MakeTerm.from_zip_for_sem(zipper);
    let info_map =
      Interface.Statics.mk_map_ctx(settings.core, ctx_init, term);
    switch (zipper.backpack, Indicated.index(zipper)) {
    | _ when !settings.core.statics => QueryResult.empty
    | _ when Id.Map.is_empty(info_map) =>
      QueryResult.error("No Static information available")
    | (_, None) => QueryResult.error("No cursor in program")
    | (_, Some(id)) =>
      switch (Id.Map.find_opt(id, info_map)) {
      | None => QueryResult.error("Whitespace or Comment")
      | Some(ci) =>
        QueryResult.mk("info", Some(QueryResult.Info(ci))) //TODO: return the result instead of "info"
      }
    };
  | (Query(Type), _) =>
    let oci =
      switch (result.t_object) {
      | Some(QueryResult.Info(ci)) => Some(ci)
      | _ => None
      };
    switch (oci) {
    | Some(InfoExp(e)) =>
      QueryResult.mk("type", Some(QueryResult.Type(e.ty))) //TODO: return the result instead of "type"
    | Some(InfoPat(p)) =>
      QueryResult.mk("type", Some(QueryResult.Type(p.ty))) //TODO: return the result instead of "type"
    | Some(InfoTyp(t)) =>
      QueryResult.mk("type", Some(QueryResult.Type(t.ty))) //TODO: return the result instead of "type"
    | _ => QueryResult.error("No type information available")
    };
  | _ => QueryResult.empty
  };
};
