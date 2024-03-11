/*

   node hazeLS.js CHECK dynamics --prelude testdata/todo1/prelude-shorter.haze --main testdata/todo1/solution.haze --epilogue testdata/todo1/epilogue.haze

  node hazeLS.js RUNTEST --api-key ~/azure-4-api-key.txt --prelude testdata/todo1/prelude-shorter.haze --main testdata/todo1/sketch.haze --epilogue testdata/todo1/epilogue.haze

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

let default_options: FillerOptions.t = {
  llm: OpenAI.Azure_GPT4,
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

let azure_gpt4_req = (~key, ~llm, ~prompt, ~handler): unit =>
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

let fill_marker = "\\?\\?";

let split_sketch = (sketch: string) => {
  let s = Str.split(Str.regexp(fill_marker), sketch);
  switch (s) {
  | [_] => failwith("LS: RunTest: No hole marker in sketch")
  | [pre, suf] => (pre, suf)
  | _ => failwith("LS: RunTest: Multiple hole markers in sketch")
  };
};

let rec error_loop =
        (
          ~llm,
          ~key,
          ~caret_ctx,
          ~caret_mode,
          ~handler,
          ~fuel,
          prompt,
          reply: OpenAI.reply,
        )
        : unit => {
  let go = error_loop(~llm, ~key, ~caret_ctx, ~caret_mode, ~handler);
  print_endline("LS: RunTest: Err rounds left: " ++ string_of_int(fuel));
  print_endline("LS: RunTest: Reply content:" ++ reply.content);
  switch (Filler.error_reply(~init_ctx=caret_ctx, ~mode=caret_mode, reply)) {
  | _ when fuel <= 0 =>
    print_endline("LS: RunTest: Error round limit reached, stopping");
    handler(reply.content);
  | None =>
    print_endline("LS: RunTest: No errors, stopping");
    handler(reply.content);
  | Some(err_msg) =>
    print_endline("LS: RunTest: Reply errors:" ++ err_msg);
    let prompt' =
      OpenAI.add_to_prompt(prompt, ~assistant=reply.content, ~user=err_msg);
    azure_gpt4_req(~llm, ~key, ~prompt=prompt', ~handler=response =>
      switch (OpenAI.handle_chat(response)) {
      | Some(reply') => go(~fuel=fuel - 1, prompt', reply')
      | None => print_endline("WARN: Error loop: Handle returned none ")
      }
    );
  };
};

let final_handler =
    (~sketch_pre, ~sketch_suf, ~prelude, ~init_ctx, ~epilogue, str) => {
  let completed_sketch = sketch_pre ++ str ++ sketch_suf;
  print_endline("completed sketch:");
  print_endline(completed_sketch);
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
  print_endline("LS: RunTest: Test results:");
  print_endline(String.concat("\n", results));
};

let first_handler =
    (
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
  switch (OpenAI.handle_chat(req)) {
  | Some(reply) =>
    error_loop(
      ~llm=options.llm,
      ~key,
      ~caret_ctx,
      ~caret_mode,
      ~handler=
        final_handler(
          ~sketch_pre,
          ~sketch_suf,
          ~prelude,
          ~epilogue,
          ~init_ctx,
        ),
      ~fuel=options.error_rounds_max,
      prompt,
      reply,
    )
  | None => failwith("APINode: handler returned None")
  };

let go =
    (
      ~db,
      ~settings as
        {init_ctx, data: {program, prelude, epilogue, _}, options}: settings,
      ~key,
    ) => {
  let (sketch_pre, sketch_suf) = split_sketch(program);
  let (caret_mode, caret_ctx) =
    get_caret_mode_and_ctx(~db, ~init_ctx, ~prelude, sketch_pre);
  let expected_ty = ChatLSP.Type.expected(~ctx=caret_ctx, Some(caret_mode));
  switch (Filler.prompt(options, ~sketch=program, ~expected_ty)) {
  | None => print_endline("LS: RunTest: prompt generation failed")
  | Some(prompt) =>
    print_endline("LS: RunTest: PROMPT:\n " ++ OpenAI.show_prompt(prompt));
    print_endline("commit: " ++ LSFiles.getCurrentGitCommit());
    print_endline(
      "time: " ++ string_of_int(LSFiles.getCurrentUnixTimestamp()),
    );
    failwith("so long, suckers!!!") |> ignore;
    azure_gpt4_req(
      ~llm=options.llm,
      ~key,
      ~prompt,
      ~handler=
        first_handler(
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
