/*

  TODO:
  node hazeLS.js CHECK dynamics --prelude testdata/todo1/prelude.haze --main testdata/todo1/solution.haze --epilogue testdata/todo1/epilogue.haze
  node hazeLS.js RUNTEST --run_name yoyoyo --expected_type --error_rounds_max 2 --source_folder testdata/todo1/ --api-key ~/azure-4-api-key.txt

  PLAYLIST, include type info:
  node hazeLS.js RUNTEST --run_name yoyoyo --expected_type --error_rounds_max 2 --source_folder testdata/playlist1/ --api-key ~/azure-4-api-key.txt


  PLAYLIST:
  node hazeLS.js CHECK statics --prelude testdata/playlist1/prelude.haze --main testdata/playlist1/solution.haze --epilogue testdata/playlist1/epilogue.haze
  node hazeLS.js CHECK dynamics --prelude testdata/playlist1/prelude.haze --main testdata/playlist1/solution.haze --epilogue testdata/playlist1/epilogue.haze

 */

open Haz3lcore;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings = {
  init_ctx: Ctx.t,
  sketch: string,
  common: string,
  prelude: string,
  epilogue: string,
  run_name: string,
  source_path: string,
  options: FillerOptions.t,
};

type io = {
  save: (string, string) => unit,
  add: (string, string) => unit,
  get: string => option(string),
  update: (string, option(string) => string) => unit,
};

let default_options: FillerOptions.t = {
  llm: OpenAI.Azure_GPT4_0613,
  instructions: true,
  syntax_notes: true,
  num_examples: 9,
  expected_type: false,
  error_rounds_max: 0,
  relevant_ctx: false,
};

let default: LSActions.runtest = {
  key: "NULL",
  run_name: "NULL",
  source_path: "NULL",
  options: default_options,
};

let get_caret_mode_and_ctx = (~db, ~init_ctx, ~common, ~prelude, sketch_pre) => {
  let sketch_pre_z = LSFiles.get_zipper(~db, sketch_pre, None);
  let init_ctx =
    LSCompletions.get_prelude_ctx(
      ~db,
      ~init_ctx,
      ~prelude=Some(prelude),
      ~common=Some(common),
    );
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

let _ask_gpt = (~key, ~llm, ~prompt, ~handler): unit => {
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

// a b c d e f g h i j k l m n o p q r s t u v w x y z
let ask_gpt = (~key as _, ~llm as _, ~prompt as _, ~handler): unit => {
  print_endline("MOCK API RESPONSE");
  handler(
    Some(
      API.Json.from_string(
        {|
  {"choices":[{"finish_reason":"stop","index":0,"message":{"content":"\nfun (model, action) ->
    case action
    | Play(track_id) =>
        let updated_tracks = List.map
              (fun t -> if t.id == track_id then { t with is_playing = true } else { t with is_playing = false })
              model.tracks in
        { model with tracks = updated_tracks }
    | Pause =>
        let updated_tracks = List.map (fun t -> { t with is_playing = false }) model.tracks in
        { model with tracks = updated_tracks }
    | AddTrack(track) =>
        let updated_tracks = track :: model.tracks in
        { model with tracks = updated_tracks }
    | RemoveTrack(track_id) =>
        let updated_tracks = List.filter (fun t -> t.id != track_id) model.tracks in
        { model with tracks = updated_tracks }
    end","role":"assistant"}}],"created":1710546768,"id":"chatcmpl-93Bdwe2gBn7xdCNfeCAwGdjyS4u9B","model":"gpt-4","object":"chat.completion","system_fingerprint":null,"usage":{"completion_tokens":148,"prompt_tokens":1545,"total_tokens":1693}}
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

let mk_io = (name: string): io => {
  let run_dir = "testout";
  let mk_run_name = (base: string) => {
    run_dir ++ "//" ++ base ++ "-" ++ LSFiles.getCurrentISOTimestamp();
  };
  let run_name = mk_run_name(name);
  let run_filepath = name => run_name ++ "//" ++ name;
  let ref_file = run_filepath("run.data");
  let add = LSFiles.append_key_value_to_file(~path=ref_file);
  let get = LSFiles.get_value_by_key_from_file(~path=ref_file);
  let save = (filename, content) =>
    LSFiles.save_text_to_file(~path=run_filepath(filename), ~content);
  LSFiles.mk_dir(run_dir);
  LSFiles.mk_dir(run_name);
  let update = (key, update) =>
    LSFiles.update_key_value_in_file(~path=ref_file, ~key, update);
  {save, add, get, update};
};

let record_init_info =
    (
      ~io: io,
      options: FillerOptions.t,
      prelude: string,
      epilogue: string,
      sketch: string,
      source_path: string,
    ) => {
  let opt_pre = prop => "option-" ++ prop;
  io.add(opt_pre("llm"), options.llm |> OpenAI.show_chat_models);
  io.add(opt_pre("source_path"), source_path);
  io.add(opt_pre("expected_type"), options.expected_type |> string_of_bool);
  io.add(opt_pre("relevant_ctx"), options.relevant_ctx |> string_of_bool);
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
  let append = g =>
    fun
    | None => g
    | Some(s) => s ++ "," ++ g;
  let num_static =
    switch (error_res |> fst) {
    | ParseError(_) => (-1) // signifies parse error
    | NoErrors => 0
    | StaticErrors(xs) => List.length(xs)
    };
  let prompt_tokens = reply.usage.prompt_tokens |> string_of_int;
  io.update("round-usage-prompt-tokens", append(prompt_tokens));
  let completion_tokens = reply.usage.completion_tokens |> string_of_int;
  io.update("round-usage-completion-tokens", append(completion_tokens));
  let total_tokens = reply.usage.total_tokens |> string_of_int;
  io.update("round-usage-total-tokens", append(total_tokens));
  let num_static = string_of_int(num_static);
  io.update("round-static-errors", append(num_static));
};

let percent_string = (a, b) =>
  Printf.sprintf("%.1f", float_of_string(a) *. 100. /. float_of_string(b));
let minus_string = (a, b) =>
  Printf.sprintf("%.1f", float_of_string(a) -. float_of_string(b));
let sum_comma_separated = str =>
  str
  |> Option.get
  |> Str.split(Str.regexp(","))
  |> List.fold_left(
       (a, b) => string_of_int(int_of_string(a) + int_of_string(b)),
       "0",
     );

let record_derived = (~io: io) => {
  let time_start = io.get("start-time") |> Option.get;
  let time_end = io.get("end-time") |> Option.get;
  let time_elapsed = minus_string(time_end, time_start);
  io.add("derived-time-elapsed", time_elapsed);

  let tests_total = io.get("tests-total") |> Option.get;
  let tests_pass = io.get("tests-pass") |> Option.get;
  let percent_passing = percent_string(tests_pass, tests_total);
  io.add("derived-percent-tests", percent_passing);

  io.get("round-static-errors")
  |> Option.get
  |> Str.split(Str.regexp(","))
  |> List.length
  |> (x => x - 1)  // don't count intial prompt
  |> string_of_int
  |> io.add("derived-err-rounds-used");

  io.get("round-usage-total-tokens")
  |> sum_comma_separated
  |> io.add("derived-total-tokens-used");

  let static_errors =
    io.get("round-static-errors")
    |> Option.get
    |> Str.split(Str.regexp(","));

  let first_round_static_errors = static_errors |> List.hd;
  let final_round_static_errors = static_errors |> List.rev |> List.hd;

  let final_parses = final_round_static_errors == "-1" ? "false" : "true";
  io.add("derived-final-parses", final_parses);
  io.add("derived-final-static-errors", final_round_static_errors);

  let percent_errors_fixed =
    percent_string(
      minus_string(final_round_static_errors, first_round_static_errors),
      first_round_static_errors,
    );
  io.add("derived-err-improve", percent_errors_fixed);

  io.add("derived-completed-run", "true");
};

/* Fix temporary issues with Hazel syntax / parser.
   In particular, exchange || to \/ and \t to two spaces */
let fix_parse_problems = s =>
  Js_of_ocaml.Regexp.global_replace(
    Js_of_ocaml.Regexp.regexp("\\|\\|"),
    Js_of_ocaml.Regexp.global_replace(
      Js_of_ocaml.Regexp.regexp("\\t"),
      s,
      "  ",
    ),
    "\\/",
  );

let fix_or_op = ({content, usage}: OpenAI.reply): OpenAI.reply => {
  {content: fix_parse_problems(content), usage};
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
  /* HACK(andrew): convert or op */
  let reply = fix_or_op(reply);
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

let final_handler =
    (
      ~db,
      ~io: io,
      ~sketch_pre,
      ~sketch_suf,
      ~common,
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
          common: Some(common),
          prelude: Some(prelude),
          program: completed_sketch,
          new_token: None,
          epilogue: Some(epilogue),
        },
      },
    );
  record_final_info(~db, ~io, results, completed_sketch);
  record_derived(~io);
  exit(0);
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
      ~common,
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
        ~common,
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

let go =
    (
      ~db,
      ~settings as
        {
          init_ctx,
          run_name,
          sketch,
          common,
          prelude,
          epilogue,
          options,
          source_path,
        }: settings,
      ~key,
    ) => {
  let io = mk_io(run_name);
  db("LS: RunTest: Setting up output folder");
  record_init_info(~io, options, prelude, epilogue, sketch, source_path);
  db("LS: RunTest: Generating prompt");
  let (sketch_pre, sketch_suf) = split_sketch(sketch);
  let (caret_mode, caret_ctx) =
    get_caret_mode_and_ctx(~db, ~init_ctx, ~common, ~prelude, sketch_pre);
  let expected_ty = ChatLSP.Type.expected(~ctx=caret_ctx, Some(caret_mode));
  let relevant_ctx_str = ChatLSP.RelevantCtx.str(caret_ctx, caret_mode);
  switch (Filler.prompt(options, ~sketch, ~expected_ty, ~relevant_ctx_str)) {
  | None => db("LS: RunTest: Prompt generation failed")
  | Some(prompt) =>
    db("LS: RunTest: Prompt generation succeeded");
    io.save("initial-prompt", OpenAI.show_prompt(prompt));
    print_endline(OpenAI.show_prompt(prompt));
    // print_endline(fix_or("a || b"));
    // failwith("YOLO5000") |> ignore;
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
          ~common,
          ~prelude,
          ~epilogue,
          sketch_pre,
          sketch_suf,
        ),
    );
  };
};

/*
 Expected type context suggestion plan:

 Suggest things from the context that could help produce the expected type
 - If the expected type is a tuple, suggest things that could help produce each part of the tuple
  - If the expected type in an arrow, suggest things that could help produce the output type,
    and also things that take the input type (?)

 1. from expected type, generate a list of target types:
   - if the expected type is a tuple, generate a list of the types of each part of the tuple
   - if the expected type is an arrow, generate a list of the output types (and if those are tuple, recurse)

 check consistency, but privilege concrete types over unknowns

 goal: generate a list of stubbed let defs, obtained by filtered from the typing context,
 applying a metric, and taking the top N


 goal type: expected type
 secondary goal types:

 source type: option(expected type source if arrow)

 */
