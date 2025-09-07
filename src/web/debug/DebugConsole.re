open Language;
open Haz3lcore;
open Util;
open AssistantTreeHelper;

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
  | "F3" => term |> Language.Exp.show |> print
  | "F4" => map |> Language.Statics.Map.show |> print
  | "F5" when settings.core.dynamics =>
    let env_init = Language.Builtins.env_init;
    statics.elaborated
    |> Language.Evaluator.evaluate(~env=env_init)
    |> fst
    |> Language.DHExp.show
    |> print;
  | "F5" => print("Dynamics disabled, cannot show evaluation.")
  | "F6" =>
    let index = Indicated.index(zipper);
    switch (index) {
    | Some(index) =>
      print("id:" ++ Id.to_string(index));
      switch (Id.Map.find_opt(index, map)) {
      | Some(ci) => print(Language.Info.show(ci))
      | None => print("DEBUG: No CI found for index")
      };
    | None => print("DEBUG: No indicated index")
    };
  | "F8" =>
    let curr_node =
      AssistantTreeHelper.build_curr_node_info(
        editor.editor.state.zipper,
        map,
      );
    let errors = ErrorPrint.subtree(Option.get(curr_node).info, map);
    print_endline("Found the following errors:");
    print_endline(String.concat("\n", errors));
    ();
  | "F9" =>
    print(
      CompositionView.str_refs_in(
        ~exclude_rec_refs=false,
        ~exclude_body_refs=false,
        get_node(build_curr_node_info(zipper, map)),
        map,
      ),
    );
    ();
  | "F10" =>
    let context = (local_information: node): string => {
      let info = local_information.info;
      switch (info) {
      | InfoExp(info) =>
        let ctx = info.ctx;
        let bindings: Binding.s =
          List.filter_map(
            (entry: Ctx.entry) => {
              let b =
                switch (entry) {
                | Ctx.VarEntry(entry) => Ctx.binding_of(ctx, entry.name)
                | Ctx.TVarEntry(entry) => Ctx.binding_of(ctx, entry.name)
                | Ctx.ConstructorEntry(entry) =>
                  Ctx.binding_of(ctx, entry.name)
                | _ => Ctx.binding_of(ctx, "") // invalid
                };
              if (b.id == Id.invalid) {
                None;
              } else {
                Some(b);
              };
            },
            ctx.entries,
          );
        "Typing Context: ["
        ++ String.concat(
             ", ",
             List.mapi(
               (i: int, b: Binding.t) =>
                 b.name ++ " (Index: " ++ string_of_int(i) ++ ")",
               bindings,
             ),
           )
        ++ "]";
      | _ => ""
      };
    };
    print(
      context(
        get_node(
          build_curr_node_info(
            editor.editor.state.zipper,
            editor.statics.info_map,
          ),
        ),
      ),
    );
  | "F11" =>
    //simple curr node id print
    let _ =
      CompositionView.prepare_definition(
        zipper,
        get_node(build_curr_node_info(zipper, map)),
      );
    ();

  | _ => print("DEBUG: No action for key: " ++ key)
  };
};
