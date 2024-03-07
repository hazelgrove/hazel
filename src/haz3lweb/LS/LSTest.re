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
    OpenAI.start_chat(~llm=filler_options.llm, ~key, prompt, req =>
      switch (OpenAI.handle_chat(req)) {
      | Some(reply) => print_endline("GPT4 Reply: " ++ reply.content)
      | None => print_endline("LSTest: handler returned None")
      }
    );
  };
};
