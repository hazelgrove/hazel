open Haz3lcore;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type completions =
  | Grammar
  | Context
  | Types;

[@deriving (show({with_path: false}), sexp, yojson)]
type check =
  | Syntax
  | Static
  | Dynamic;

[@deriving (show({with_path: false}), sexp, yojson)]
type api_key_path = string;

[@deriving (show({with_path: false}), sexp, yojson)]
type command =
  | Check(check)
  | Completions(completions)
  | RunTest(api_key_path);

let show_command = (c: command): string =>
  switch (c) {
  | Check(Syntax) => "Check(Syntax)"
  | Check(Static) => "Check(Static)"
  | Check(Dynamic) => "Check(Dynamic)"
  | Completions(Grammar) => "Completions(Grammar)"
  | Completions(Context) => "Completions(Context)"
  | Completions(Types) => "Completions(Types)"
  | RunTest(_) => "RunTest"
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type data = {
  prelude: option(string),
  program: string,
  new_token: option(string),
  epilogue: option(string),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type arguments = {
  debug: bool,
  ctx: Ctx.t,
  command,
  data,
};

let default_data: data = {
  prelude: None,
  program: "",
  new_token: None,
  epilogue: None,
};

let default_settings: arguments = {
  debug: false,
  ctx: Builtins.ctx_base,
  command: Completions(Types),
  data: default_data,
};
