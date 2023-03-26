open Haz3lcore;

let sanitize_prompt = (prompt: string): string => {
  //HACK: replacement of ??? below
  let prompt = Str.global_replace(Str.regexp("\\?\\?\\?"), "", prompt);
  let prompt =
    if (Str.string_match(Str.regexp("^\".*\"$"), prompt, 0)) {
      String.sub(prompt, 1, String.length(prompt) - 2);
    } else {
      prompt;
    };
  prompt;
};

let prompt = (model: Model.t): option(string) => {
  let editor = model.editors |> Editors.get_editor;
  let prefixes = [
    "Respond as minimally as possible",
    "Do not include a period at the end of your response",
  ];
  let body = Printer.to_string_selection(editor);
  let body = sanitize_prompt(body);
  switch (String.trim(body)) {
  | "" => None
  | _ => Some(String.concat(". ", prefixes) ++ ". " ++ body ++ ":")
  };
};

let sanitize_response = (response: string): string => {
  Str.global_replace(Str.regexp("\""), "'", response);
};

let quote = s => "\"" ++ s ++ "\"";

let react = (response: string): UpdateAction.t => {
  let response = sanitize_response(response);
  Paste(quote(response));
};
