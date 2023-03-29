open Haz3lcore;

//TODO(andrew): calculate this in a more principled way
let get_ci = (model: Model.t): option(Info.t) => {
  let editor = model.editors |> Editors.get_editor;
  let index = Indicated.index(editor.state.zipper);
  let get_term = z => z |> Zipper.unselect_and_zip |> MakeTerm.go |> fst;
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

type samples = list((string, string));

let samples = [
  ("let a:Float = ??(5)", "float_of_int"),
  ("let f = ?? in f(5)", "fun x:Int -> ??"),
  ("case Foo(5) | Foo(x) => ?? | Bar => 6", "x"),
];

let collate_samples: samples => list(string) =
  List.map(((prompt, completion)) => {
    "sample prompt: " ++ prompt ++ "\nsample completion: " ++ completion ++ ""
  });

let code_instructions = [
  "You are an intelligent code completion agent",
  "You will be given a program sketch and are tasked with coming up with a valid replacement for the hole labelled ?? in the actual prompt",
  "Your suggestion doesn't have to be complete; it's okay to leave holes (??) in your completion if there isn't enough information to fill them in",
  "Respond only with a replacement for the symbol ?? in the actual prompt",
  "Respond only with a single replacement expression; you do not need to provide replacements for the samples",
  "Do not include the provided program sketch in your response",
  "Include only code in your response",
  "Do not include a period at the end of your response",
];

/*
 ideas: take into account clipboard, past code positions, selections

 TODO: make holes rendered as some actual text; otherwise it tries to fill them...
 */

/**
   REMEMBER: HACKS in Code, Measured for reponse-wrapping ~ form.contents

   plan for response wrapper:
   current caret ends up after response
   want to move it before response
   hopefully can select whole term (is it indicated atm? not super robust...)
   and then manually change selection focus i guess.
   at this point we want TAB to accept the completion,
   which means getting rid of the wrapper,
   and any(?) other action to get rid of the completion
   so logic is like... check that indicated term is wrapper
   i guess this should be done at top-level of update? feels hacky...
   if indicate term is wrapper, check if command is whatever TAB does (new Commplete action?)
    if so, remove wrapper, and splice in innards, i guess thru zipper surgery?
    otherwise, just remove wrapper+contents
  */

let prompt_code = (model: Model.t): option(string) => {
  let editor = model.editors |> Editors.get_editor;
  let type_expectation =
    switch (get_ci(model)) {
    | Some(InfoExp({mode: Ana(ty), _})) => "type = " ++ Typ.show(ty)
    | _ => "any type"
    };

  let prefix =
    ["First, Consider these examples of a prompt and its completion:"]
    @ collate_samples(samples)
    @ code_instructions
    @ [
      "According to the type checker, the hole must be filled by an expression of "
      ++ type_expectation,
    ]
    @ ["actual prompt: "];
  let body = Printer.to_string_editor(~holes=Some("HOLE"), editor);
  //let body = sanitize_prompt(body);
  switch (String.trim(body)) {
  | "" => None
  | _ =>
    let prompt =
      String.concat("\n ", prefix)
      ++ "\n "
      ++ body
      ++ "\nactual completion:\n";
    print_endline("ABOUT TO SUBMIT PROMPT:\n " ++ prompt);
    Some(prompt);
  };
};

let react_code = (response: string): UpdateAction.t => {
  //let response = sanitize_response(response);
  PasteIntoSelection(
    response,
    // "~" ++ response ++ "~",
  );
};
