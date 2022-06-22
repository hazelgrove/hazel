module Parsing = Hazeltext.Parsing;
module Print = Hazeltext.Print;

module UHDoc_Exp = UHDoc_Exp.Make(Memo.DummyMemo);

let verbose = true;

module Success = {
  type t = {
    parsed: UHExp.t,
    zipped: ZExp.t,
    edited: ZExp.t,
    printed: string,
  };
};

module Failure = {
  module Variants = {
    type err =
      | SyntaxError(string)
      | ParseFailed(string)
      | CantFollowPath
      | ActionFailed(int, Action.t, ZExp.t)
      | CursorEscaped(int, Action.t, ZExp.t)
      | NoLayout(UHDoc.t);
    let syntaxerror = msg => SyntaxError(msg);
  };

  type t = Variants.err;

  module Syntax = {
    let ( let* ) = Result.bind;
    let (let+) = (r, f) => Result.map(f, r);

    let (<||) = (opt: Option.t('a), err: t): Result.t('a, t) =>
      Option.to_result(~none=err, opt);
    let (||>) = (res: Result.t('ok, 'err), err: 'err => t): Result.t('ok, t) =>
      Result.map_error(err, res);
  };
};

open Failure.Variants;
open Failure.Syntax;

let parse = (text): Result.t(UHExp.block, Failure.t) =>
  try(
    {
      let+ e = Parsing.ast_of_string(text) ||> syntaxerror;
      let (e, _, _) = Statics_Exp.fix_and_renumber_holes(Context.initial, e);
      e;
    }
  ) {
  | Failure(msg) => Error(ParseFailed(msg))
  };

let print = (e: UHExp.t): Result.t(string, Failure.t) => {
  let doc = Lazy.force(UHDoc_Exp.mk, ~memoize=true, ~enforce_inline=false, e);
  let+ layout =
    Pretty.LayoutOfDoc.layout_of_doc(~width=100, ~pos=0, doc)
    <|| NoLayout(doc);
  Print.string_of_layout(layout);
};

let print_z = (ze: ZExp.t): Result.t(string, Failure.t) =>
  print(ZExp.erase(ze));

let read =
    (input: string, path: CursorPath.t)
    : Result.t((UHExp.t, Statics.edit_state), Failure.t) => {
  let* e = parse(input);
  let* ze = CursorPath_Exp.follow(path, e) <|| CantFollowPath;
  Ok((e, Statics_Exp.fix_and_renumber_holes_z(Context.initial, ze)));
};

let rec eval =
        (
          ~i: int=1,
          actions: list(Action.t),
          (ze, _, _) as edit_state: Statics.edit_state,
        ) =>
  switch (actions) {
  | [action, ...actions'] =>
    switch (Action_Exp.syn_perform(Context.initial, action, edit_state)) {
    | Failed => Error(ActionFailed(i, action, ze))
    | CursorEscaped(_) => Error(CursorEscaped(i, action, ze))
    | Succeeded(edit_state) => eval(~i=i + 1, actions', edit_state)
    }
  | [] => Ok(edit_state)
  };

let read_eval_print =
    (input: string, path: CursorPath.t, actions: list(Action.t))
    : Result.t(Success.t, Failure.t) => {
  let* (parsed, (zipped, _, _) as edit_state) = read(input, path);
  let* (edited, _, _) = eval(actions, edit_state);
  let+ printed = print_z(edited);
  Success.{parsed, zipped, edited, printed};
};

let show_sexp = (sexp: Sexplib.Sexp.t): string =>
  Sexplib.Sexp.to_string_hum(sexp);

let report =
    (
      input: string,
      path: CursorPath.t,
      expect: string,
      result: Result.t(Success.t, Failure.t),
    )
    : bool => {
  verbose
    ? switch (result) {
      | Error(failure) =>
        switch (failure) {
        | SyntaxError(msg) => Printf.printf("SYNTAX %s", msg)
        | ParseFailed(msg) => Printf.printf("Parse Failed: %s", msg)
        | CantFollowPath =>
          Format.printf(
            "Can't Follow Cursor Path: %s",
            show_sexp(CursorPath.sexp_of_t(path)),
          )
        | ActionFailed(i, action, ze) =>
          Printf.printf(
            "Action %i Failed: %s\nOn ZExp:\n%s",
            i,
            show_sexp(Action.sexp_of_t(action)),
            show_sexp(ZExp.sexp_of_t(ze)),
          )
        | CursorEscaped(i, action, ze) =>
          Printf.printf(
            "Action %i Cursor Escaped: %s\nOn ZExp:\n%s",
            i,
            show_sexp(Action.sexp_of_t(action)),
            show_sexp(ZExp.sexp_of_t(ze)),
          )
        | NoLayout(doc) =>
          Printf.printf(
            "Print Failed: No Layout:\n%s",
            show_sexp(UHDoc.sexp_of_t(doc)),
          )
        }
      | Ok(result) =>
        Printf.printf(
          "\nOK: %s\n\nParsed:\n%s\n\nZipped:\n%s\n\nEdited:\n%s\n",
          input,
          show_sexp(UHExp.sexp_of_t(result.parsed)),
          show_sexp(ZExp.sexp_of_t(result.zipped)),
          show_sexp(ZExp.sexp_of_t(result.edited)),
        );
        Printf.printf(
          "\nTest %s!\nExpect: \"%s\"\nActual: \"%s\"\n",
          String.equal(result.printed, expect) ? "Passed" : "Failed",
          expect,
          result.printed,
        );
      }
    : ();
  switch (result) {
  | Error(_) => false
  | Ok(success) => String.equal(success.printed, expect)
  };
};

let test =
    (
      input: string,
      path: CursorPath.t,
      actions: list(Action.t),
      expect: string,
    )
    : bool =>
  read_eval_print(input, path, actions) |> report(input, path, expect);
