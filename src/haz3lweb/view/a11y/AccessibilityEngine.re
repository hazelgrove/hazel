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
    msg: string,
    t_object: option(textobject),
  };

  let empty = {msg: "nothing", t_object: None};
  let error = (msg: string) => {msg, t_object: None};

  let mk = (msg, t_object): t => {
    {msg, t_object};
  };

  let change_msg = (msg: string, result: t): t => {
    {msg, t_object: result.t_object};
  };

  let get_id = (result: t): option(Id.t) => {
    switch (result.t_object) {
    | Some(Info(ci)) => Some(Info.id_of(ci))
    | _ => None
    };
  };
};

let rec evaluate_text_object =
        (
          ~settings: Settings.t,
          ~ctx_init,
          ~editor: Editor.t,
          text_obj: text_object,
        ) => {
  switch (text_obj) {
  | Inner(Term) =>
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
        | Some(ci) => QueryResult.mk("info", Some(QueryResult.Info(ci)))
        }
      };
    result;
  | Inner(Parenthesis) => QueryResult.empty
  | Queried(query) => evaluate_query(~settings, ~ctx_init, ~editor, query)
  };
}

and evaluate_query =
    (
      ~settings: Settings.t,
      ~ctx_init,
      ~editor: Editor.t,
      query: QueryAst.query,
    )
    : QueryResult.t => {
  let (text_obj_command, q_op) = query;
  let text_obj_result =
    evaluate_text_object(~settings, ~ctx_init, ~editor, text_obj_command);
  switch (text_obj_result.t_object, q_op) {
  | (Some(Info(ci)), (None, Type)) =>
    switch (ci) {
    | InfoExp(e) =>
      QueryResult.mk(e.ty |> Typ.show, Some(QueryResult.Type(e.ty)))
    | InfoPat(p) =>
      QueryResult.mk(p.ty |> Typ.show, Some(QueryResult.Type(p.ty)))
    | InfoTyp(t) =>
      QueryResult.mk(t.ty |> Typ.show, Some(QueryResult.Type(t.ty)))
    | _ => QueryResult.error("No type information available")
    }
  | (_, (None, Type)) => QueryResult.error("No type information available")
  | _ => QueryResult.error("No information available")
  };
};

let evaluate_command =
    (
      ~settings: Settings.t,
      ~ctx_init,
      ~editor: Editor.t,
      command: QueryAst.command,
    )
    : QueryResult.t => {
  switch (command) {
  | Query(query) => evaluate_query(~settings, ~ctx_init, ~editor, query)
  | Partial(text_object) =>
    evaluate_text_object(~settings, ~ctx_init, ~editor, text_object)
    |> QueryResult.change_msg("The selected text is highlighted")
  | Action(_) => QueryResult.empty
  };
};
