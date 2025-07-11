open Util;

let get_documentation_as_text = () => {
  let (_, slides) = ScratchMode.StoreDocumentation.load();
  let documentation =
    slides
    |> List.map(((name, persistent)) => {
         let cell_model =
           CellEditor.Model.unpersist(
             ~settings=Language.CoreSettings.off,
             persistent,
           );
         let text =
           Haz3lcore.Printer.of_zipper(cell_model.editor.editor.state.zipper);
         "<slide_name>"
         ++ name
         ++ "</slide_name>\n"
         ++ "<slide_text>"
         ++ text
         ++ "</slide_text>";
       })
    |> String.concat("\n\n");
  "<hazelDocumentation>" ++ documentation ++ "</hazelDocumentation>";
};

let mk_tutor = () => {
  OpenRouter.mk_system_msg(
    TutorPrompt.self ++ "\n\n" ++ get_documentation_as_text(),
  );
};

let mk_composition = (): OpenRouter.message => {
  OpenRouter.mk_system_msg(
    String.concat(
      " ",
      CompositionPrompt.self @ [get_documentation_as_text()],
    ),
  );
};

let mk_suggestion = (): OpenRouter.message => {
  OpenRouter.mk_system_msg(
    "You are a helpful assistant that suggests how to fill holes in Hazel to the user.",
  );
};
