open Haz3lcore;
/*

  node hazeLS.js CHECK dynamics --prelude testdata/todo1/prelude-shorter.haze --main testdata/todo1/solution.haze --epilogue testdata/todo1/epilogue.haze

 node hazeLS.js RUNTEST --api-key ~/azure-4-api-key.txt --prelude testdata/todo1/prelude-shorter.haze --main testdata/todo1/sketch.haze --epilogue testdata/todo1/epilogue.haze
  */

let ci_of_hole = (~init_ctx, ~prelude, ~db, sketch_pre): Info.t => {
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
  switch (gen_options) {
  | NewRightConvex(info_dump)
  | CompletionOrNewRightConcave(info_dump, _) => info_dump.ci
  | _ => failwith("ci_of_hole: impossible")
  };
};

let get_expected_ty = (~db, ~init_ctx, ~prelude, sketch_pre) => {
  let ci = ci_of_hole(~init_ctx, ~prelude, ~db, sketch_pre);
  let mode =
    switch (ci) {
    | InfoExp({mode, _}) => Some(mode)
    | InfoPat({mode, _}) => Some(mode)
    | _ => None
    };
  ChatLSP.Type.expected(~ctx=Info.ctx_of(ci), mode);
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

let mk_prompt = (~db, ~init_ctx, ~prelude, ~llm, ~sketch_pre, ~sketch) => {
  let filler_options: FillerOptions.t = {
    llm,
    instructions: true,
    syntax_notes: true,
    num_examples: 9,
    expected_type: true,
    error_round: true,
  };
  let expected_ty = get_expected_ty(~db, ~init_ctx, ~prelude, sketch_pre);
  Filler.prompt(filler_options, ~sketch, ~expected_ty);
};

let first_handler = (sketch_pre, sketch_suf, req) =>
  switch (OpenAI.handle_chat(req)) {
  | Some(reply) =>
    let completed_sketch = sketch_pre ++ reply.content ++ sketch_suf;
    print_endline("completed sketch:");
    print_endline(completed_sketch);
  | None => failwith("APINode: handler returned None")
  };

let go =
    (
      ~db,
      ~settings as
        {ctx: init_ctx, data: {program, prelude, _}, _}: LSCompletions.settings,
      ~key,
    ) => {
  let llm = OpenAI.Azure_GPT4;
  let (sketch_pre, sketch_suf) = split_sketch(program);
  switch (
    mk_prompt(~db, ~init_ctx, ~prelude, ~llm, ~sketch_pre, ~sketch=program)
  ) {
  | None => print_endline("LSTest: prompt generation failed")
  | Some(prompt) =>
    print_endline("LSTest: PROMPT:\n " ++ OpenAI.show_prompt(prompt));
    azure_gpt4_req(
      ~llm,
      ~key,
      ~prompt,
      ~handler=first_handler(sketch_pre, sketch_suf),
    );
  };
};
