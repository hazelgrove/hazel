/*

  TODO:
  node hazeLS.js CHECK dynamics --prelude testdata/todo1/prelude.haze --main testdata/todo1/solution.haze --epilogue testdata/todo1/epilogue.haze
  node hazeLS.js RUNTEST --expected_type --error_rounds_max 2 --api-key ~/azure-4-api-key.txt --debug --prelude testdata/todo1/prelude.haze --main testdata/todo1/sketch.haze --epilogue testdata/todo1/epilogue.haze

  PLAYLIST, include type info:
  node hazeLS.js RUNTEST --expected_type --api-key ~/azure-4-api-key.txt --prelude testdata/playlist1/prelude.haze --main testdata/playlist1/sketch.haze --epilogue testdata/playlist1/epilogue.haze

  PLAYLIST:
  node hazeLS.js CHECK statics --prelude testdata/playlist1/prelude.haze --main testdata/playlist1/solution.haze --epilogue testdata/playlist1/epilogue.haze
  node hazeLS.js CHECK dynamics --prelude testdata/playlist1/prelude.haze --main testdata/playlist1/solution.haze --epilogue testdata/playlist1/epilogue.haze

 */

/*
  FOLDERNAME

 record:
   LOG_OF_STDOUT (for all below)

   run_command
   prelude_path
   sketch_path
   epilogue_path

   commit
   starttime

   foreach req {
     prompt
     usage
   }

   endtime
   parse_error?
   type_errors

   dynamic_tests

   DERIVED:
   */

open Haz3lcore;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type data = {
  prelude: string,
  epilogue: string,
  sketch_pre: string,
  sketch_suf: string,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type settings = {
  init_ctx: Ctx.t,
  data: LSActions.data,
  options: FillerOptions.t,
};

type io = {
  save: (string, string) => unit,
  add: (string, string) => unit,
  update: (string, option(string) => string) => unit,
};

let default_options: FillerOptions.t = {
  llm: OpenAI.Azure_GPT4_0613,
  instructions: true,
  syntax_notes: true,
  num_examples: 9,
  expected_type: false,
  error_rounds_max: 1,
};

let default: LSActions.runtest = {api_key: "SPORK", options: default_options};

let get_caret_mode_and_ctx = (~db, ~init_ctx, ~prelude, sketch_pre) => {
  let sketch_pre_z = LSFiles.get_zipper(~db, sketch_pre, None);
  let init_ctx = LSCompletions.get_prelude_ctx(~db, ~init_ctx, ~prelude);
  //TODO(andrew): this makes tons of assumptions
  let gen_options =
    LSCompletions.generation_options(
      ~init_ctx,
      ~completions=Types,
      ~db,
      sketch_pre_z,
    );
  let ci =
    switch (gen_options) {
    | NewRightConvex(info_dump)
    | CompletionOrNewRightConcave(info_dump, _) => info_dump.ci
    | _ => failwith("ci_of_hole: impossible")
    };
  switch (ci) {
  | InfoExp({mode, ctx, _})
  | InfoPat({mode, ctx, _}) => (mode, ctx)
  | _ => (Syn, [])
  };
};

let ask_gpt = (~key, ~llm, ~prompt, ~handler): unit => {
  if (llm != OpenAI.Azure_GPT4_0613) {
    failwith("LS: ask_gpt: Unsupported chat model");
  };
  API.node_request(
    ~method=POST,
    ~hostname=
      Printf.sprintf(
        "%s.openai.azure.com",
        "hazel2" // resource
      ),
    ~path=
      Printf.sprintf(
        "/openai/deployments/%s/chat/completions?api-version=%s",
        "hazel-gpt-4", // deployment
        "2023-05-15" // api version
      ),
    ~headers=[("Content-Type", "application/json"), ("api-key", key)],
    ~body=OpenAI.body(~llm, prompt),
    handler,
  );
};

let _ask_gpt = (~key as _, ~llm as _, ~prompt as _, ~handler): unit => {
  print_endline("MOCK API RESPONSE");
  handler(
    Some(
      API.Json.from_string(
        {|
  {"choices":[{"finish_reason":"stop","index":0,"message":{"content":"\nfun model, action ->\n  case action\n    | AddTodo => let (buffer, todos) = model in\n                if buffer <> \"\" then (\"\", (buffer, false) :: todos ) else model\n    | RemoveTodo(index) => let (buffer, todos) = model in\n                          (buffer, List.remove(index, todos))\n    | ToggleTodo(index) => let (buffer, todos) = model in\n                          (buffer, List.mapi(fun i, (text, done) -> if i = index then (text, not done) else (text, done), todos))\n    | UpdateBuffer(new_buffer) => let (_, todos) = model in\n                                 (new_buffer, todos)\n  end","role":"assistant"}}],"created":1710546768,"id":"chatcmpl-93Bdwe2gBn7xdCNfeCAwGdjyS4u9B","model":"gpt-4","object":"chat.completion","system_fingerprint":null,"usage":{"completion_tokens":148,"prompt_tokens":1545,"total_tokens":1693}}
  |},
      ),
    ),
  );
};

let fill_marker = "\\?\\?";

let split_sketch = (sketch: string) => {
  let s = Str.split(Str.regexp(fill_marker), sketch);
  switch (s) {
  | [_] => failwith("LS: RunTest: No hole marker in sketch")
  | [pre, suf] => (pre, suf)
  | _ => failwith("LS: RunTest: Multiple hole markers in sketch")
  };
};

let record_round_info =
    (
      ~db,
      ~io: io,
      max,
      fuel,
      reply: OpenAI.reply,
      error_res: (Filler.error_report, string),
    ) => {
  let round = max - fuel;
  db(Printf.sprintf("LS: RunTest: Round %d of %d", round + 1, max + 1));
  db("LS: RunTest: Reply content:" ++ reply.content);
  io.save(Printf.sprintf("round-reply.%d.haze", round), reply.content);
  io.save(Printf.sprintf("round-reply.%d.errors", round), error_res |> snd);
  let num_static =
    switch (error_res |> fst) {
    | NoErrors => 0
    | ParseError(_) => (-1) // signifies parse error
    | StaticErrors(xs) => List.length(xs)
    };
  // io.add(
  //   Printf.sprintf("round-%d-static-errors", round),
  //   num_static |> string_of_int,
  // );
  // io.add(
  //   Printf.sprintf("round-%d-prompt-tokens", round),
  //   reply.usage.prompt_tokens |> string_of_int,
  // );
  // io.add(
  //   Printf.sprintf("round-%d-completion-tokens", round),
  //   reply.usage.completion_tokens |> string_of_int,
  // );
  // io.add(
  //   Printf.sprintf("round-%d-total-tokens", round),
  //   reply.usage.total_tokens |> string_of_int,
  // );
  let prompt_tokens = reply.usage.prompt_tokens |> string_of_int;
  io.update(
    "round-usage-prompt-tokens",
    fun
    | None => prompt_tokens
    | Some(s) => s ++ "," ++ prompt_tokens,
  );
  let completion_tokens = reply.usage.completion_tokens |> string_of_int;
  io.update(
    "round-usage-completion-tokens",
    fun
    | None => completion_tokens
    | Some(s) => s ++ "," ++ completion_tokens,
  );
  let total_tokens = reply.usage.total_tokens |> string_of_int;
  io.update(
    "round-usage-total-tokens",
    fun
    | None => total_tokens
    | Some(s) => s ++ "," ++ total_tokens,
  );
  let num_static = string_of_int(num_static);
  io.update(
    "round-static-errors",
    fun
    | None => num_static
    | Some(s) => s ++ "," ++ num_static,
  );
};

let rec error_loop =
        (
          ~io,
          ~db,
          ~llm,
          ~key,
          ~caret_ctx,
          ~caret_mode,
          ~handler,
          ~max,
          ~fuel,
          ~prompt,
          ~reply: OpenAI.reply,
        )
        : unit => {
  let go =
    error_loop(~db, ~llm, ~key, ~caret_ctx, ~caret_mode, ~handler, ~max);
  let error_res =
    Filler.error_reply(~init_ctx=caret_ctx, ~mode=caret_mode, reply);
  record_round_info(~db, ~io, max, fuel, reply, error_res);
  switch (error_res) {
  | _ when fuel <= 0 =>
    db("LS: RunTest: Error round limit reached, stopping");
    handler(reply);
  | (NoErrors, _) =>
    db("LS: RunTest: No errors, stopping");
    handler(reply);
  | (_, err_msg) =>
    db("LS: RunTest: Reply errors:" ++ err_msg);
    let prompt =
      OpenAI.add_to_prompt(prompt, ~assistant=reply.content, ~user=err_msg);
    ask_gpt(~llm, ~key, ~prompt, ~handler=response =>
      switch (OpenAI.handle_chat(~db, response)) {
      | Some(reply) => go(~io, ~fuel=fuel - 1, ~prompt, ~reply)
      | None => db("WARN: Error loop: Handle returned none ")
      }
    );
  };
};

let record_final_info =
    (~db, ~io: io, results: list(TestStatus.t), completed_sketch: string)
    : unit => {
  db("LS: RunTest: Completed sketch:");
  db(completed_sketch);
  db("LS: RunTest: Test results:");
  db(String.concat("\n", List.map(TestStatus.to_string, results)));
  let counts = LSChecker.collate_test_counts(results);
  io.add("end-time", LSFiles.getCurrentUnixTimestamp());
  io.save("completed-sketch.haze", completed_sketch);
  io.add("tests-total", string_of_int(counts.total));
  io.add("tests-pass", string_of_int(counts.pass));
  io.add("tests-fail", string_of_int(counts.fail));
  io.add("tests-indet", string_of_int(counts.indet));
};

let final_handler =
    (
      ~db,
      ~io: io,
      ~sketch_pre,
      ~sketch_suf,
      ~prelude,
      ~init_ctx,
      ~epilogue,
      reply: OpenAI.reply,
    ) => {
  let completed_sketch = sketch_pre ++ reply.content ++ sketch_suf;
  let results =
    LSChecker.test_combined(
      ~db=ignore,
      {
        init_ctx,
        check: LSActions.Dynamic,
        data: {
          prelude,
          program: completed_sketch,
          new_token: None,
          epilogue,
        },
      },
    );
  record_final_info(~db, ~io, results, completed_sketch);
};

let first_handler =
    (
      ~db,
      ~io,
      ~key,
      ~caret_ctx,
      ~caret_mode,
      ~init_ctx,
      ~prompt,
      ~prelude,
      ~epilogue,
      ~options: FillerOptions.t,
      sketch_pre,
      sketch_suf,
      req,
    ) =>
  switch (OpenAI.handle_chat(~db, req)) {
  | Some(reply) =>
    let handler =
      final_handler(
        ~db,
        ~io,
        ~sketch_pre,
        ~sketch_suf,
        ~prelude,
        ~epilogue,
        ~init_ctx,
      );
    error_loop(
      ~db,
      ~io,
      ~llm=options.llm,
      ~key,
      ~caret_ctx,
      ~caret_mode,
      ~handler,
      ~max=options.error_rounds_max,
      ~fuel=options.error_rounds_max,
      ~prompt,
      ~reply,
    );
  | None => failwith("APINode: handler returned None")
  };

let run_dir = "testout";
let mk_run_name = (base: string) => {
  run_dir ++ "//" ++ base ++ "-" ++ LSFiles.getCurrentISOTimestamp();
};

let or_empty = (s: option(string)): string =>
  switch (s) {
  | None => ""
  | Some(x) => x
  };

let mk_io = (name: string): io => {
  let run_name = mk_run_name(name);
  let run_filepath = name => run_name ++ "//" ++ name;
  let ref_file = run_filepath("run.data");
  let add = LSFiles.append_key_value_to_file(~path=ref_file);
  let save = (filename, content) =>
    LSFiles.save_text_to_file(~path=run_filepath(filename), ~content);
  LSFiles.mk_dir(run_dir);
  LSFiles.mk_dir(run_name);
  let update = (key, update) =>
    LSFiles.update_key_value_in_file(~path=ref_file, ~key, update);
  {save, add, update};
};

let record_init_info =
    (
      ~io: io,
      options: FillerOptions.t,
      prelude: string,
      epilogue: string,
      sketch: string,
    ) => {
  let opt_pre = prop => "option-" ++ prop;
  io.add(opt_pre("llm"), options.llm |> OpenAI.show_chat_models);
  io.add(opt_pre("expected_type"), options.expected_type |> string_of_bool);
  io.add(
    opt_pre("error_rounds_max"),
    options.error_rounds_max |> string_of_int,
  );
  io.save("prelude.haze", prelude);
  io.save("epilogue.haze", epilogue);
  io.save("sketch.haze", sketch);
  io.add("prelude-hash", LSFiles.hash_of_string(prelude));
  io.add("epilogue-hash", LSFiles.hash_of_string(epilogue));
  io.add("sketch-hash", LSFiles.hash_of_string(sketch));
  io.add("commit", LSFiles.getCurrentGitCommit());
  io.add("start-time", LSFiles.getCurrentUnixTimestamp());
};

let go =
    (
      ~db,
      ~settings as
        {init_ctx, data: {program: sketch, prelude, epilogue, _}, options}: settings,
      ~key,
    ) => {
  let io = mk_io("bar");
  db("LS: RunTest: Setting up output folder");
  record_init_info(
    ~io,
    options,
    or_empty(prelude),
    or_empty(epilogue),
    sketch,
  );
  db("LS: RunTest: Generating prompt");
  let (sketch_pre, sketch_suf) = split_sketch(sketch);
  let (caret_mode, caret_ctx) =
    get_caret_mode_and_ctx(~db, ~init_ctx, ~prelude, sketch_pre);
  let expected_ty = ChatLSP.Type.expected(~ctx=caret_ctx, Some(caret_mode));
  switch (Filler.prompt(options, ~sketch, ~expected_ty)) {
  | None => db("LS: RunTest: Prompt generation failed")
  | Some(prompt) =>
    db("LS: RunTest: Prompt generation succeeded");
    io.save("initial-prompt", OpenAI.show_prompt(prompt));
    ask_gpt(
      ~llm=options.llm,
      ~key,
      ~prompt,
      ~handler=
        first_handler(
          ~db,
          ~io,
          ~prompt,
          ~init_ctx,
          ~key,
          ~caret_mode,
          ~caret_ctx,
          ~options,
          ~prelude,
          ~epilogue,
          sketch_pre,
          sketch_suf,
        ),
    );
  };
};
