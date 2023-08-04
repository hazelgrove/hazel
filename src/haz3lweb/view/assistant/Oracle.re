open Haz3lcore;

let sanitize_prompt = (prompt: string): string => {
  //HACK: replacement of ?? below
  let prompt = Str.global_replace(Str.regexp("\\?\\?"), "", prompt);
  let prompt =
    if (Str.string_match(Str.regexp("^\".*\"$"), prompt, 0)) {
      String.sub(prompt, 1, String.length(prompt) - 2);
    } else {
      prompt;
    };
  prompt;
};

let ask = (model: Model.t): option(OpenAI.prompt) => {
  let editor = model.editors |> Editors.get_editor;
  let system_prompt = [
    "Respond as minimally as possible",
    "Do not include a period at the end of your response",
  ];
  let body = Printer.to_string_selection(editor);
  let body = sanitize_prompt(body);
  switch (String.trim(body)) {
  | "" => None
  | _ =>
    let prompt =
      [OpenAI.{role: System, content: String.concat("\n", system_prompt)}]
      @ [{role: User, content: body}];
    Some(prompt);
  };
};

let sanitize_response: string => string =
  Str.global_replace(Str.regexp("\""), "'");

let quote = s => "\"" ++ s ++ "\"";

let react = (response: string): UpdateAction.t => {
  let response = response |> sanitize_response |> quote;
  Paste(response);
};
