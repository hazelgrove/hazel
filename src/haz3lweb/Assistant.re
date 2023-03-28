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

let prompt_chat = (model: Model.t): option(string) => {
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

let react_chat = (response: string): UpdateAction.t => {
  let response = sanitize_response(response);
  Paste(quote(response));
};

type samples = list((string, string));

let samples = [
  ("let a:Float = ???(5)", "float_of_int"),
  ("let f = ??? in f(5)", "fun x:Int -> ???"),
  ("case Foo(5) | Foo(x) => ??? | Bar => 6", "x"),
];

let collate_samples: samples => list(string) =
  List.map(((prompt, completion)) => {
    "prompt: " ++ prompt ++ "\ncompletion: " ++ completion ++ ""
  });

let code_instructions = [
  "You are an intelligent code completion agent",
  "You will be given a program sketch and are tasked with coming up with a valid replacement for the hole labelled `???`",
  "Your suggestion doesn't have to be complete; it's okay to leave holes (???) in your completion if there isn't enough information to fill them in",
  /*"prompt: let a:Float = ???(5); completion: float_of_int",
    "prompt: let f = ??? in f(5); completion: fun x:Int -> ???",
    "prompt: case Foo(5) | Foo(x) => ??? | Bar => 6; completion: x",*/
  "Respond only with a replacement the symbol `???`",
  "Do not include the provided program sketch in your response",
  "Include only code in your response",
  "Do not include a period at the end of your response",
];

let prompt_code = (model: Model.t): option(string) => {
  let editor = model.editors |> Editors.get_editor;
  let prefixes =
    code_instructions
    @ ["Sample completions:"]
    @ collate_samples(samples)
    @ ["prompt: "];
  let body = Printer.to_string_editor(editor);
  let body = sanitize_prompt(body);
  switch (String.trim(body)) {
  | "" => None
  | _ =>
    Some(String.concat("\n ", prefixes) ++ "\n " ++ body ++ "completion:\n")
  };
};

let react_code = (response: string): UpdateAction.t => {
  //let response = sanitize_response(response);
  Paste("~" ++ response ++ "~");
};
