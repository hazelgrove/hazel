open Js_of_ocaml;
open Haz3lweb;
open LSActions;

let usage_check = "<syntax|statics|dynamics>";
let usage_completions = "<grammar|context|types>";
let usage_runtest = "[--expected_type] [--error-rounds-max (0 <= N < 10)] [--run_name <run-name>] --source_folder <path-base> --api-key <api-key-path>";
let usage_command =
  "<CHECK " ++ usage_check ++ " | COMPLETIONS " ++ usage_completions ++ ">";
let usage_debug = "[--debug]";
let usage_ctx = "[--empty-init-ctx]";
let usage_common = "[--common <path>]";
let usage_prelude = "[--prelude <path>]";
let usage_main = "[--main <path>]";
let usage_epilogue = "[--epilogue <path>]";
let usage_new_token = "[--new-token <new-token-to-append>]";

let usage_str =
  String.concat(
    " ",
    [
      "lsp",
      usage_command,
      usage_completions,
      usage_check,
      usage_debug,
      usage_ctx,
      usage_prelude,
      usage_common,
      usage_main,
      usage_new_token,
      usage_epilogue,
      "<program>",
    ],
  );

let get_runtest = command =>
  switch (command) {
  | RunTest(rt) => rt
  | _ => LSTest.default
  };

let get_get_prompt = command =>
  switch (command) {
  | GetPrompt(fo) => fo
  | _ => FillerOptions.init
  };

let validate_error_rounds = (num_rounds: string): bool =>
  switch (int_of_string_opt(num_rounds)) {
  | None => false
  | Some(n) => n >= 0 && n < 10
  };

let rec parse = (args: arguments, strs): arguments =>
  switch (strs) {
  | ["CHECK", ...rest] => parse_check(rest, args)
  | ["COMPLETIONS", ...rest] => parse_completions(rest, args)
  | ["RUNTEST", ...rest] => parse_runtest(rest, {...args, debug: true})
  | ["GETPROMPT", ...rest] => parse_get_prompt(rest, args)
  | _ => failwith("LSP: Command not recognized: " ++ usage_command)
  }
and parse_base = (strs, args: arguments): arguments =>
  switch (strs) {
  | ["--debug", ...rest] => parse_base(rest, {...args, debug: true})
  | ["--empty-init-ctx", ...rest] =>
    parse_base(rest, {...args, init_ctx: []})
  | ["--common", path, ...rest] =>
    switch (LSFiles.string_of_file(path)) {
    | exception _ =>
      failwith("LSP: EXN: Could not load common from path: " ++ path)
    | common =>
      let data = {...args.data, common: Some(common)};
      parse_base(rest, {...args, data});
    }
  | ["--common", ..._] => failwith("LSP: EXN: Usage: " ++ usage_common)
  | ["--prelude", path, ...rest] =>
    switch (LSFiles.string_of_file(path)) {
    | exception _ =>
      failwith("LSP: EXN: Could not load prelude from path: " ++ path)
    | prelude =>
      let data = {...args.data, prelude: Some(prelude)};
      parse_base(rest, {...args, data});
    }
  | ["--prelude", ..._] => failwith("LSP: EXN: Usage: " ++ usage_prelude)
  | ["--epilogue", path, ...rest] =>
    switch (LSFiles.string_of_file(path)) {
    | exception _ =>
      failwith("LSP: EXN: Could not load epilogue from path: " ++ path)
    | epilogue =>
      let data = {...args.data, epilogue: Some(epilogue)};
      parse_base(rest, {...args, data});
    }
  | ["--epilogue", ..._] => failwith("LSP: EXN: Usage: " ++ usage_epilogue)
  | ["--main", path, ...rest] =>
    switch (LSFiles.string_of_file(path)) {
    | exception _ =>
      failwith("LSP: EXN: Could not load main from path: " ++ path)
    | program =>
      let data = {...args.data, program};
      parse_base(rest, {...args, data});
    }
  | ["--main", ..._] => failwith("LSP: EXN: Usage: " ++ usage_main)
  | ["--new-token", new_token, ...rest] =>
    let data = {...args.data, new_token: Some(new_token)};
    parse_base(rest, {...args, data});
  | ["--new-token", ..._] => failwith("LSP: EXN: Usage: " ++ usage_new_token)
  | [arg, ..._] when String.starts_with(~prefix="--", arg) =>
    failwith("LSP: EXN: Unrecognized argument: " ++ arg)
  | [program] =>
    let data = {...args.data, program};
    {...args, data};
  | [] when args.data.program != "" => args
  | [] => failwith("LSP: EXN: No program specified. Usage: " ++ usage_str)
  | [_, ..._] =>
    failwith("LSP: EXN: Multiple unnamed arguments. Usage: " ++ usage_str)
  }
and parse_check = (strs, args: arguments): arguments =>
  switch (strs) {
  | ["syntax", ...rest] =>
    parse_base(rest, {...args, command: Check(Syntax)})
  | ["statics", ...rest] =>
    parse_base(rest, {...args, command: Check(Static)})
  | ["dynamics", ...rest] =>
    parse_base(rest, {...args, command: Check(Dynamic)})
  | _ => failwith("LSP: EXN: Usage: " ++ usage_check)
  }
and parse_completions = (strs, args: arguments): arguments =>
  switch (strs) {
  | ["grammar", ...rest] =>
    parse_base(rest, {...args, command: Completions(Grammar)})
  | ["context", ...rest] =>
    parse_base(rest, {...args, command: Completions(Context)})
  | ["types", ...rest] =>
    parse_base(rest, {...args, command: Completions(Types)})
  | _ => failwith("LSP: EXN: Usage: " ++ usage_completions)
  }
and parse_get_prompt = (strs, args: arguments): arguments =>
  switch (strs) {
  | ["--expected_type", ...rest] =>
    let rt = get_get_prompt(args.command);
    let options = {...rt, expected_type: true};
    parse_get_prompt(rest, {...args, command: GetPrompt(options)});
  | ["--relevant_ctx", ...rest] =>
    let rt = get_get_prompt(args.command);
    let options = {...rt, relevant_ctx: true};
    parse_get_prompt(rest, {...args, command: GetPrompt(options)});
  | rest => parse_base(rest, args)
  }
and parse_runtest = (strs, args: arguments): arguments =>
  switch (strs) {
  /* must be last runtest-specific arg */
  //| ["--api-key", key_path, ...rest] =>
  | ["--api-key", key_path] =>
    switch (LSFiles.string_of_file(key_path)) {
    | exception _ =>
      failwith("LSP: EXN: Could not load api key from path: " ++ key_path)
    | key =>
      let rt = get_runtest(args.command);
      {...args, command: RunTest({...rt, key})};
    //parse_base(rest, {...args, command: RunTest({...rt, api_key})});
    }
  | ["--source_folder", path_base, ...rest] =>
    let common_path = path_base ++ "..//common.haze";
    let common =
      switch (LSFiles.string_of_file(common_path)) {
      | exception _ =>
        failwith("LSP: EXN: Could not load common from path: " ++ common_path)
      | common => common
      };
    let prelude_path = path_base ++ "//prelude.haze";
    let prelude =
      switch (LSFiles.string_of_file(prelude_path)) {
      | exception _ =>
        failwith(
          "LSP: EXN: Could not load prelude from path: " ++ prelude_path,
        )
      | prelude => prelude
      };
    let epilogue_path = path_base ++ "//epilogue.haze";
    let epilogue =
      switch (LSFiles.string_of_file(epilogue_path)) {
      | exception _ =>
        failwith(
          "LSP: EXN: Could not load epilogue from path: " ++ epilogue_path,
        )
      | epilogue => epilogue
      };
    let sketch_path = path_base ++ "//sketch.haze";
    let sketch =
      switch (LSFiles.string_of_file(sketch_path)) {
      | exception _ =>
        failwith("LSP: EXN: Could not load sketch from path: " ++ sketch_path)
      | sketch => sketch
      };
    let data = {
      common: Some(common),
      prelude: Some(prelude),
      epilogue: Some(epilogue),
      program: sketch,
      new_token: None,
    };
    let rt = get_runtest(args.command);
    parse_runtest(
      rest,
      {...args, data, command: RunTest({...rt, source_path: path_base})},
    );
  | ["--run_name", run_name, ...rest] =>
    let rt = get_runtest(args.command);
    parse_runtest(rest, {...args, command: RunTest({...rt, run_name})});
  | ["--error_rounds_max", num_rounds, ...rest]
      when validate_error_rounds(num_rounds) =>
    let rt = get_runtest(args.command);
    let error_rounds_max = int_of_string(num_rounds);
    let options = {...rt.options, error_rounds_max};
    parse_runtest(rest, {...args, command: RunTest({...rt, options})});
  | ["--expected_type", ...rest] =>
    let rt = get_runtest(args.command);
    let options = {...rt.options, expected_type: true};
    parse_runtest(rest, {...args, command: RunTest({...rt, options})});
  | ["--relevant_ctx", ...rest] =>
    let rt = get_runtest(args.command);
    let options = {...rt.options, relevant_ctx: true};
    parse_runtest(rest, {...args, command: RunTest({...rt, options})});
  | _ => failwith("LSP: EXN: Usage: " ++ usage_runtest)
  };

let get_args = (): list(string) => {
  Js.Unsafe.(
    switch (
      get(js_expr("require('process')"), "argv")
      |> Js.to_array
      |> Array.map(Js.to_string)
      |> Array.to_list
    ) {
    | [_, _, ...args] => args
    | _ => failwith("LSP: EXN: Args malformed")
    }
  );
};

let or_empty = (s: option(string)): string =>
  switch (s) {
  | None => ""
  | Some(x) => x
  };

let main = ({debug, data, command, init_ctx}: LSActions.arguments) => {
  let db = s => debug ? print_endline(s) : ();
  db(Printf.sprintf("LSP: Command: %s", LSActions.show_command(command)));
  switch (command) {
  | Completions(completions) =>
    LSCompletions.go(~db, ~settings={data, completions, init_ctx})
  | Check(check) => LSChecker.go(~db, ~settings={data, check, init_ctx})
  | RunTest({run_name, key, options, source_path}) =>
    LSTest.go(
      ~db,
      ~settings={
        options,
        run_name,
        source_path,
        sketch: data.program,
        common: or_empty(data.common),
        prelude: or_empty(data.prelude),
        epilogue: or_empty(data.epilogue),
        init_ctx,
      },
      ~key,
    )
  | GetPrompt(options) =>
    LSTest.get_prompt_info(
      ~db,
      ~settings={
        options,
        run_name: "",
        source_path: "",
        sketch: data.program,
        common: or_empty(data.common),
        prelude: or_empty(data.prelude),
        epilogue: or_empty(data.epilogue),
        init_ctx,
      },
    )
    |> print_endline
  };
};

get_args() |> parse(LSActions.default_settings) |> main;
