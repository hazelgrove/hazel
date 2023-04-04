open Haz3lcore;
open ChatLSP;

type samples = list((string, string, string));

let samples = [
  ("let a:Float = ??(5)", Type.expected(Some(SynFun)), "float_of_int"),
  ("let f = ?? in f(5)", Type.expected(Some(Syn)), "fun x:Int -> ??"),
  (
    "let g = fun x:Int, y: Bool -> if y then x else 6 in g(5, ??)",
    Type.expected(Some(Ana(Bool))),
    "true",
  ),
  (
    "case Foo(5) | Foo(x) => ?? | Bar => 6",
    Type.expected(Some(Ana(Int))),
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

let prompt = (model: Model.t): option(string) => {
  let editor = model.editors |> Editors.get_editor;
  let prefix =
    ["Consider these examples:"]
    @ collate_samples(samples)
    @ code_instructions
    @ [model |> Type.mode |> Type.expected];
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
        model |> Type.mode |> Type.expected,
      );
    let prompt = String.concat("\n ", prefix) ++ prompt_proper;
    print_endline("ABOUT TO SUBMIT PROMPT:\n " ++ prompt);
    Some(prompt);
  };
};

let react = (response: string): UpdateAction.t => {
  Agent(SetBuffer(response));
};
