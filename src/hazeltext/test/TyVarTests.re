module Parsing = Hazeltext.Parsing;
module Print = Hazeltext.Print;

module ZExpTest = {
  type error =
    | SyntaxError(string)
    | ParseFailed(string)
    | BadCursorPath
    | CantFollowCursor
    | ActionFailed(int, Action.t, ZExp.t)
    | CursorEscaped(int, Action.t, ZExp.t);

  let ( let* ) = Result.bind;
  let (let+) = Result.map;

  let (<||) = (opt: Option.t('a), err: error): Result.t('a, error) =>
    Option.to_result(~none=err, opt);
  let (||>) =
      (res: Result.t('ok, 'err), err: 'err => error): Result.t('ok, error) =>
    Result.map_error(err, res);

  let parse = (text): Result.t(UHExp.block, error) =>
    try(
      {
        let* e =
          Parsing.ast_of_lexbuf(Lexing.from_string(text))
          ||> (msg => SyntaxError(msg));
        let (e, _, _) =
          Statics_Exp.fix_and_renumber_holes(Contexts.empty, e);
        Ok(e);
      }
    ) {
    | Failure(msg) => Error(ParseFailed(msg))
    };

  let run =
      (input: string, steps: CursorPath.steps, actions: list(Action.t))
      : Result.t((UHExp.t, ZExp.t), error) => {
    let* e = parse(input);
    let* path = CursorPath_Exp.of_steps(steps, e) <|| BadCursorPath;
    let* ze = CursorPath_Exp.follow(path, e) <|| CantFollowCursor;
    let edit_state = Statics_Exp.fix_and_renumber_holes_z(Contexts.empty, ze);
    let rec loop = (i, actions, ze) =>
      switch (actions) {
      | [action, ...actions'] =>
        switch (Action_Exp.syn_perform(Contexts.empty, action, edit_state)) {
        | Failed => Error(ActionFailed(i, action, ze))
        | CursorEscaped(_) => Error(CursorEscaped(i, action, ze))
        | Succeeded((ze, _, _)) => loop(i + 1, actions', ze)
        }
      | [] => Ok((e, ze))
      };
    loop(1, actions, ze);
  };

  let test =
      (input: string, steps: CursorPath.steps, actions: list(Action.t)): bool =>
    switch (run(input, steps, actions)) {
    | Error(err) =>
      switch (err) {
      | SyntaxError(msg) =>
        Printf.printf("SYNTAX %s", msg);
        false;
      | ParseFailed(msg) =>
        Printf.printf("Parse Failed: %s", msg);
        false;
      | BadCursorPath =>
        Printf.printf(
          "Bad Cursor Path: %s",
          Sexplib.Sexp.to_string_hum(CursorPath.sexp_of_steps(steps)),
        );
        false;
      | CantFollowCursor =>
        print_endline("Can't Follow Cursor Path: ");
        print_endline(
          Sexplib.Sexp.to_string_hum(CursorPath.sexp_of_steps(steps)),
        );
        false;
      | ActionFailed(i, action, ze) =>
        Printf.printf(
          "Action %i Failed: %s\nOn ZExp:\n%s",
          i,
          Sexplib.Sexp.to_string_hum(Action.sexp_of_t(action)),
          Sexplib.Sexp.to_string_hum(ZExp.sexp_of_t(ze)),
        );
        false;
      | CursorEscaped(i, action, ze) =>
        Printf.printf(
          "Action %i Cursor Escaped: %s\nOn ZExp:\n%s",
          i,
          Sexplib.Sexp.to_string_hum(Action.sexp_of_t(action)),
          Sexplib.Sexp.to_string_hum(ZExp.sexp_of_t(ze)),
        );
        false;
      }
    // | Ok((e, ze)) =>
    //   Printf.printf(
    //     "\nSuccess!!\n\n\"%s\" parses to:\n%s\n\nFinal Result:\n%s",
    //     String.escaped(input),
    //     Sexplib.Sexp.to_string_hum(UHExp.sexp_of_t(e)),
    //     Sexplib.Sexp.to_string_hum(ZExp.sexp_of_t(ze)),
    //   );
    //   true
    | Ok(_) => true
    };
};

let%test _ = {
  // the program to parse
  let input = "\\x:?.{x}";
  // a series of steps to the desired cursor position (here, the ?)
  let steps = [0, 0, 0, 0, 1, 0];
  /* actions to perform at the cursor */
  let actions: list(Action.t) = [
    Construct(SChar("I")),
    Construct(SChar("n")),
    Construct(SChar("t")),
  ];
  ZExpTest.test(input, steps, actions);
};
