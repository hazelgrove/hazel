open Haz3lcore;
open Sexplib.Std;
open QueryAst;

let query_parser = (query: string) => {
  let lexbuf = Lexing.from_string(query);
  let result =
    switch (Parser.main(Lexer.token, lexbuf)) {
    | query_ast => Some(query_ast)
    | exception _ => None
    };
  JsUtil.log(result);
  result;
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

let execute_query = // TODO: change the input to zipper and info_map
    (
      ~settings: Settings.t,
      ~ctx_init,
      ~editor: Editor.t,
      command: QueryAst.command,
    )
    : QueryResult.t => {
  switch (command) {
  | Query((Inner(Term), (None, Type))) =>
    let zipper = editor.state.zipper;
    let (term, _) = MakeTerm.from_zip_for_sem(zipper);
    let info_map =
      Interface.Statics.mk_map_ctx(settings.core, ctx_init, term);
    let result =
      switch (zipper.backpack, Indicated.index(zipper)) {
      | _ when !settings.core.statics => QueryResult.empty
      | _ when Id.Map.is_empty(info_map) =>
        QueryResult.error("No Static information available")
      | (_, None) => QueryResult.error("No cursor in program")
      | (_, Some(id)) =>
        switch (Id.Map.find_opt(id, info_map)) {
        | None => QueryResult.error("Whitespace or Comment")
        | Some(ci) => QueryResult.mk("info", Some(QueryResult.Info(ci))) //TODO: return the result instead of "info"
        }
      };
    let oci =
      switch (result.t_object) {
      | Some(QueryResult.Info(ci)) => Some(ci)
      | _ => None
      };
    switch (oci) {
    | Some(InfoExp(e)) =>
      QueryResult.mk(e.ty |> Typ.show, Some(QueryResult.Type(e.ty)))
    | Some(InfoPat(p)) =>
      QueryResult.mk(p.ty |> Typ.show, Some(QueryResult.Type(p.ty)))
    | Some(InfoTyp(t)) =>
      QueryResult.mk(t.ty |> Typ.show, Some(QueryResult.Type(t.ty)))
    | _ => QueryResult.error("No type information available")
    };
  | _ => QueryResult.empty
  };
};
