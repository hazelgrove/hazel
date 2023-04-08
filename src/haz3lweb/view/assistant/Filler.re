open Haz3lcore;
open ChatLSP;

type samples = list((string, string, string));

let samples = [
  ("let a:Float = ??(5)", Type.expected(Some(SynFun)), "float_of_int"),
  ("let f = ?? in f(5)", Type.expected(Some(Syn)), "fun x:Int -> ??"),
  (
    "let g =\nfun x:Int, y: Bool ->\n if y then x else 6 in g(5, ??)",
    Type.expected(Some(Ana(Bool))),
    "true",
  ),
  (
    "case Foo(5) | Foo(x) => ?? | Bar => 6",
    Type.expected(Some(Ana(Int))),
    "x",
  ),
  (
    "let num_or_zero = fun maybe_num -> case maybe_num | Some(num) => ?? | None => 0",
    Type.expected(Some(Syn)),
    "num",
  ),
  (
    "let merge_sort: [Int]->[Int] =\n??\nin\nmerge_sort([4,1,3,7,2])",
    Type.expected(Some(Ana(Arrow(Int, Int)))),
    "fun list ->\nlet split: [Int]->([Int],[Int]) = fun left, right -> ??\nin\nlet merge: ([Int],[Int])->[Int]= ?\nin\nlet merge_sort_helper: [Int]->[Int]= ??\nin\nmerge_sort_helper(list)",
  ),
  (
    "type MenuItem =\n+ Breakfast(Int, Int)\n+ Lunch(Float)\nin\nlet per_lunch_unit = 0.95 in\nlet price: MenuItem-> Float   = fun m ->\ncase m\n| Breakfast(x, y) => ??\n| Lunch(f) => f *. per_lunch_unit\nend\nin price(Breakfast(1,2))/.3.",
    Type.expected(Some(Ana(Var("MenuItem")))),
    "fun m ->\ncase m\n| Breakfast(x, y) => ??\n| Lunch(f) => f *. per_lunch_unit\nend",
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
  "HAZEL SYNTACTIC DIFFERENCES",
  "1. Function application is ALWAYS written using parentheses and commas: use 'function(arg1, arg2)'. DO NOT just use spaces between the function name and arguments.",
  "2. Function parameters are ALWAYS commas separated: 'fun arg1, arg2 -> <exp>'. DO NOT use spaces to separate function arguments.",
  "2. Pattern matching is ALWAYS written a 'case ... end' expression. Cases MUST END in an 'end' keyword. DO NOT USE any other keyword besides 'case' to do pattern matching.  DO NOT USE a 'with' or 'of' keyword with 'case', just start the list of rules. Pattern matching rules use syntax '| pattern => expression'. Note the '=>' arrow.",
  "3. The ONLY way to define a named function is by using a function expression nested in a let expression like 'let <pat> = fun <pat> -> <exp> in <exp'. There is no support for specifying the function arguments directly as part of the let. DO NOT write function arguments in the let pattern.",
  "4. No 'rec' keyword is necessary for 'let' to define a recursive function. DO NOT use the 'rec' keyword with 'let'.",
  "END HAZEL SYNTACTIC DIFFERENCES",
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
    @ code_instructions;
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
    let prompt =
      String.concat("\n ", prefix)
      ++ prompt_proper
      ++ "\nREMEMBER TO USE 'end' where applicable. REMEMBER NOT TO USE 'rec'. Format the code with proper linebreaks.\n";
    print_endline("ABOUT TO SUBMIT PROMPT:\n " ++ prompt);
    Some(prompt);
  };
};

let react = (response: string): UpdateAction.t => {
  Agent(SetBuffer(response));
};
