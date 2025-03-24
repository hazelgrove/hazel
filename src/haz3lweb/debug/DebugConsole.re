open Haz3lcore;

/* This is a place to add ad-hoc debugging print actions.
   It was originally directly in Keyboard, but that added a handler
   dependency on the model, which is technically against architecture */

let print =
    (~settings: Settings.t, editor: CodeWithStatics.Model.t, key: string)
    : unit => {
  let {editor: {state: {zipper, _}, _}, statics, _}: CodeWithStatics.Model.t = editor;
  let term = statics.term;
  let map = statics.info_map;
  let print = print_endline;
  switch (key) {
  | "F1" => zipper |> Zipper.show |> print
  | "F2" => zipper |> Zipper.unselect_and_zip |> Segment.show |> print
  | "F3" => term |> Exp.show |> print
  | "F4" => map |> Statics.Map.show |> print
  | "F5" =>
    let env_init = Builtins.env_init;
    statics.elaborated
    |> Evaluator.evaluate(~settings=settings.core, ~env=env_init)
    |> ProgramResult.show(ProgramResult.pp_inner)
    |> print;
  | "F6" =>
    let index = Indicated.index(zipper);
    switch (index) {
    | Some(index) =>
      print("id:" ++ Id.to_string(index));
      switch (Id.Map.find_opt(index, map)) {
      | Some(ci) => print(Info.show(ci))
      | None => print("DEBUG: No CI found for index")
      };
    | None => print("DEBUG: No indicated index")
    };
  | "F9" =>
    print_endline("STATIC CONTEXT AT CURSOR:");
    Util.OptUtil.Syntax.(
      switch (
        {
          let* index = Indicated.index(zipper);
          let* ci = Id.Map.find_opt(index, map);
          let sketch_seg =
            Zipper.smart_seg(~dump_backpack=true, ~erase_buffer=true, zipper);
          ChatLSP.Prompt.mk_init(ChatLSP.Options.init, ci, sketch_seg, false);
        }
      ) {
      | None => print_endline("prompt generation failed")
      | Some(prompt) =>
        List.iter(
          (message: OpenRouter.message) => {
            print_endline("---------- STRING ----------");
            print_endline(message.content);
            print_endline("---------- STRING ----------");
          },
          prompt,
        )
      }
    );
  | "F10" =>
    print_endline("WHOLE PROGRAM ERROR REPORT:");
    switch (
      {
        let whole_program_str =
          zipper
          |> Zipper.smart_seg(~dump_backpack=true, ~erase_buffer=true)
          |> ChatLSP.Print.seg;
        ChatLSP.ErrorPrint.mk(
          ~init_ctx=Builtins.ctx_init,
          Zipper.init(),
          whole_program_str,
        );
      }
    ) {
    | None => print_endline("error reply generation failed")
    | Some(prompt) => print_endline(prompt)
    };
  | _ => print("DEBUG: No action for key: " ++ key)
  };
};
