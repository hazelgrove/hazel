open Haz3lcore;
//open Util;

/*

 node hazeLS.js CHECK dynamics --prelude testdata/todo1/prelude-shorter.haze --main testdata/todo1/solution.haze --epilogue testdata/todo1/epilogue.haze

 node hazeLS.js RUNTEST
   --api-key ~/azure-4-api-key.txt
   --sag-types
   [--sag-context]
   [--error_rounds]
   --prelude testdata/todo1/prelude-shorter.haze
   --main testdata/todo1/solution.haze
   --epilogue testdata/todo1/epilogue.haze
 */

//TODO(andrew): this makes tons of assumptions
let ci_of_hole = (~settings: LSCompletions.settings, ~db, z: Zipper.t): Info.t => {
  let seg_before = z.relatives.siblings |> fst |> List.rev;
  let seg_after = z.relatives.siblings |> snd;
  if (seg_before == [] && seg_after == []) {
    failwith("ci_of_hole: EXN: Empty segment");
  };
  let gen_options = LSCompletions.generation_options(~settings, ~db, z);
  LSCompletions.print_gen_option(~db, gen_options);
  switch (gen_options) {
  | NewRightConvex(info_dump) => info_dump.ci
  | NewRightConcave(info_dump) => info_dump.ci
  | CompletionOrNewRightConvex(_, _, info_dump_new) => info_dump_new.ci
  | CompletionOrNewRightConcave(info_dump, _) => info_dump.ci
  | OnlyCompletion(info_dump, _) => info_dump.ci
  | OnlyCompletionString(_) => failwith("ci_of_hole: impossible")
  };
};

let get_expected_ty = (~db, z: Zipper.t, ~settings: LSCompletions.settings) => {
  let ci = ci_of_hole(~settings: LSCompletions.settings, ~db, z: Zipper.t);
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

let handle_chat = (res: string): option(OpenAI.reply) => {
  open API;
  open Util.OptUtil.Syntax;
  let json = Json.from_string(res);
  let* choices = Json.dot("choices", json);
  let* usage = Json.dot("usage", json);
  let* content = OpenAI.first_message_content(choices);
  let+ usage = OpenAI.of_usage(usage);
  OpenAI.{content, usage};
};

let mk_req = (~llm, ~key, ~prompt) => {
  azure_gpt4_req(~llm, ~key, ~prompt, ~handler=req =>
    switch (handle_chat(req)) {
    | Some(reply) => print_endline("AZURE_GPT4 Reply: " ++ reply.content)
    | None => print_endline("APINode: handler returned None")
    }
  );
};

let go = (~db, ~settings: LSCompletions.settings, ~key) => {
  let z =
    LSFiles.get_zipper(~db, settings.data.program, settings.data.new_token);
  let filler_options: FillerOptions.t = {
    llm: Azure_GPT4,
    instructions: true,
    syntax_notes: true,
    num_examples: 9,
    expected_type: true,
    error_round: true,
  };
  let prompt =
    Filler.prompt(
      filler_options,
      ~sketch=Printer.to_string_basic(z),
      ~expected_ty=get_expected_ty(~db, ~settings, z),
    );
  switch (prompt) {
  | None => print_endline("LSTest: prompt generation failed")
  | Some(prompt) =>
    print_endline("LSTest: PROMPT:\n " ++ OpenAI.show_prompt(prompt));
    mk_req(~llm=filler_options.llm, ~key, ~prompt);
  };
};
