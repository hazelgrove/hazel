open Haz3lcore;

//TODO(andrew): calculate this in a more principled way
let get_ci = (model: Model.t): option(Info.t) => {
  let editor = model.editors |> Editors.get_editor;
  let index = Indicated.index(editor.state.zipper);
  let get_term = z =>
    z |> Zipper.unselect_and_zip(~ignore_selection=true) |> MakeTerm.go |> fst;
  let map = editor.state.zipper |> get_term |> Statics.mk_map;
  switch (index) {
  | Some(index) => Haz3lcore.Id.Map.find_opt(index, map)
  | _ => None
  };
};

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

let typing_mode = (model: Model.t): option(Typ.mode) =>
  switch (get_ci(model)) {
  | Some(InfoExp({mode, _})) => Some(mode)
  | Some(InfoPat({mode, _})) => Some(mode)
  | _ => None
  };

let type_expectation = (mode: option(Typ.mode)): string => {
  let prefix = "Hole ?? can be filled by an expression with ";
  switch (mode) {
  | Some(Ana(ty)) => prefix ++ "a type consistent with " ++ Typ.to_string(ty)
  | Some(SynFun) =>
    prefix
    ++ "a type consistent with "
    ++ Typ.to_string(Arrow(Unknown(Internal), Unknown(Internal)))
  | Some(Syn) => prefix ++ "any type"
  | _ => "Not applicable"
  };
};

type samples = list((string, string, string));

let samples = [
  ("let a:Float = ??(5)", type_expectation(Some(SynFun)), "float_of_int"),
  ("let f = ?? in f(5)", type_expectation(Some(Syn)), "fun x:Int -> ??"),
  (
    "let g = fun x:Int, y: Bool -> if y then x else 6 in g(5, ??)",
    type_expectation(Some(Ana(Bool))),
    "true",
  ),
  (
    "case Foo(5) | Foo(x) => ?? | Bar => 6",
    type_expectation(Some(Ana(Int))),
    "x",
  ),
];

let mk_prompt =
    (prompt: string, expected_ty: string, completion: string): string =>
  Printf.sprintf(
    {|sample prompt: %s\nexpected type: %ssample completion: %s\n|},
    prompt,
    expected_ty,
    completion,
  );

let collate_samples: samples => list(string) =
  List.mapi((idx, (prompt, expected_ty, completion)) =>
    Printf.sprintf(
      {|sample_%d:
{ prompt: %s,
  expected type: %s,
  completion: %s,
}|},
      idx,
      prompt,
      expected_ty,
      completion,
    )
  );

let code_instructions = [
  {|You are an ancient and thoughtful spirit of code completion|},
  "When you encounter an incomplete program sketch as a prompt, you come up with a reasonable replacement for the hole labelled ?? in the actual prompt",
  "Your replacement suggestion doesn't have to be complete; it's okay to leave holes (denoted ??) in your completion if there isn't enough information to fill them in",
  "Respond only with a replacement for the symbol ?? in the actual prompt",
  "Respond only with a single replacement expression; you do not need to provide replacements for the samples",
  "Do not include the provided program sketch in your response",
  "Include only code in your response",
  "Use C-style function application syntax, with parenthesis around comma-separated arguments",
  "Do not include a period at the end of your response",
];

/*
 IDEA: take into account clipboard, past code positions, selections

 TODO: make holes rendered as some actual text; otherwise it tries to fill them...

 REMEMBER: HACKS in Code, Measured for reponse-wrapping ~ form.contents
 */

let prompt_code = (model: Model.t): option(string) => {
  let editor = model.editors |> Editors.get_editor;
  let prefix =
    ["Consider these examples:"]
    @ collate_samples(samples)
    @ code_instructions
    @ [model |> typing_mode |> type_expectation];
  let body = Printer.to_string_editor(~holes=Some("HOLE"), editor);
  switch (String.trim(body)) {
  | "" => None
  | _ =>
    let prompt_proper =
      Printf.sprintf(
        {|
Finally, the details of the actual program sketch to be completed:
actual_prompt: %s,
actual_expected_type: %s,
actual_completion:
      |},
        body,
        model |> typing_mode |> type_expectation,
      );
    let prompt = String.concat("\n ", prefix) ++ prompt_proper;
    print_endline("ABOUT TO SUBMIT PROMPT:\n " ++ prompt);
    Some(prompt);
  };
};

let react_code = (response: string): UpdateAction.t => {
  PasteIntoSelection(response);
};
