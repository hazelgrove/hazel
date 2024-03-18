open Haz3lcore;
open ChatLSP;
open Util.OptUtil.Syntax;
open Sexplib.Std;

[@deriving (show({with_path: false}), yojson, sexp)]
type parse_error = option(string);

[@deriving (show({with_path: false}), yojson, sexp)]
type static_errors = list(string);

[@deriving (show({with_path: false}), yojson, sexp)]
type error_report =
  | ParseError(string)
  | StaticErrors(static_errors)
  | NoErrors;

[@deriving (show({with_path: false}), yojson, sexp)]
type round_report = {
  reply: OpenAI.reply,
  error_report,
};

type samples = list((string, string, string));

//TODO(andrew): include ctx in examples to get more precise expected_ty

let samples = [
  (
    {|
let List.length: [(String, Bool)]-> Int =
  fun xs ->
    ?? end in
|},
    Type.expected(Some(Ana(Int)), ~ctx=[]),
    {|
case xs
| [] => 0
| _::xs => 1 + List.length(xs)|},
  ),
  (
    {|
let List.mapi: ((Int, Bool) -> Bool, [Bool]) -> [Bool]=
  fun f, xs ->
    let go: (Int, [Bool])-> [Bool] = fun idx, xs ->
      ?? end in
    go(0, xs) in
|},
    Type.expected(Some(Ana(List(Bool))), ~ctx=[]),
    {|
case xs
| [] => []
| hd::tl => f(idx, hd)::go(idx + 1, tl)
|},
  ),
  (
    {|
type Container =
  + Pod(Int)
  + CapsuleCluster(Int, Int) in
let total_capacity: Container -> Int =
  ??
in
|},
    Type.expected(Some(Ana(Arrow(Var("Container"), Int))), ~ctx=[]),
    {|
fun c ->
    case c
      | Pod(x) => x
      | CapsuleCluster(x, y) => x * y
    end
|},
  ),
  (
    "let f = ?? in f(5)",
    Type.expected(Some(Syn), ~ctx=[]),
    "fun x:Int -> ??",
  ),
  (
    "let get: Option_int => Int =\n"
    ++ "  case Some(5)\n"
    ++ "  | Some(x) => ??\n"
    ++ "  | None => 0 end",
    Type.expected(Some(Ana(Int)), ~ctx=[]),
    "x",
  ),
  (
    "let num_or_zero = fun maybe_num ->\n case maybe_num\n | Some(num) => ?? \n| None => 0 end",
    Type.expected(Some(Syn), ~ctx=[]),
    "num",
  ),
  (
    "let merge_sort: [Int]->[Int] =\n??\nin\nmerge_sort([4,1,3,7,2])",
    Type.expected(Some(Ana(Arrow(Int, Int))), ~ctx=[]),
    "fun list ->\nlet split: [Int]->([Int],[Int]) = fun left, right -> ?\nin\nlet merge: ([Int],[Int])->[Int]= ?\nin\nlet merge_sort_helper: [Int]->[Int]= ?\nin\nmerge_sort_helper(list)",
  ),
  (
    "type MenuItem =\n+ Breakfast(Int, Int)\n+ Lunch(Float)\nin\nlet per_lunch_unit = 0.95 in\nlet price: MenuItem-> Float   = fun m ->\ncase m\n| Breakfast(x, y) => ??\n| Lunch(f) => f *. per_lunch_unit\nend\nin price(Breakfast(1,2))/.3.",
    Type.expected(Some(Ana(Var("MenuItem"))), ~ctx=[]),
    "fun m ->\ncase m\n| Breakfast(x, y) => ??\n| Lunch(f) => f *. per_lunch_unit\nend",
  ),
  (
    {|
let List.merge: (( , )->Bool,[ ], [ ]) -> [ ] = fun cmp,left, right ->
case left, right
| [], _ => right
| _, [] => left
| h1::t1, h2::t2 =>
if cmp(h1, h2)
then h1 :: List.merge(cmp, t1, right)
else h2 :: List.merge(cmp,left, t2)
end
in

let List.sort: ((?, ?) -> Bool, [?]) -> [?] =
fun cmp, list ->
let merge_sort_helper: [?] -> [?] = fun l ->
case  l
| [] => ?
| [x] => [x]
| _ => ??
end
in merge_sort_helper(list)
in
test 2 == List.nth(List.sort(fun a, b -> a<b, [4,1,3,2]), 1) end
    |},
    Type.expected(Some(Ana(List(Unknown(Internal)))), ~ctx=[]),
    {|
let mid = List.length(l) / 2 in
let left, right = List.take(mid, l), List.drop(mid, l) in
List.merge(cmp, merge_sort_helper(left), merge_sort_helper(right))
|},
  ),
];

let main_prompt = [
  "CODE COMPLETION INSTRUCTIONS:",
  "- Reply with a functional, idiomatic replacement for the program hole marked '??' in the provided program sketch",
  //" - Your replacement suggestion doesn't have to be complete; it's okay to leave holes (marked '?') in your completion if there isn't enough information to fill them in",
  "- Reply only with a single replacement term for the unqiue distinguished hole marked '??'",
  "- Reply only with code",
  "- DO NOT suggest more replacements for other holes in the sketch (marked '?'), or implicit holes",
  "- DO NOT include the program sketch in your reply",
  "- DO NOT include a period at the end of your response and DO NOT use markdown",
];

let hazel_syntax_notes = [
  "HAZEL SYNTAX NOTES:",
  "- Hazel uses C-style function application syntax, with parenthesis around comma-separated arguments",
  "- Function application is ALWAYS written using parentheses and commas: use 'function(arg1, arg2)'. DO NOT just use spaces between the function name and arguments.",
  "- Function parameters are ALWAYS commas separated: 'fun arg1, arg2 -> <exp>'. DO NOT use spaces to separate function arguments.",
  "- There is no dot accessor notation for tuples; DO NOT use tuple.field. use pattern matching for destructuring: let (field, _) = tuple in ...",
  "- The following ARE NOT Hazel keywords. DO NOT use these keywords: switch, with, of, rec. ALWAYS omit these keywords",
  "- Pattern matching is ALWAYS written a 'case ... end' expression. Cases MUST END in an 'end' keyword. DO NOT USE any other keyword besides 'case' to do pattern matching.  DO NOT USE a 'with' or 'of' keyword with 'case', just start the list of rules. Pattern matching rules use syntax '| pattern => expression'. Note the '=>' arrow.",
  "- The ONLY way to define a named function is by using a function expression nested in a let expression like 'let <pat> = fun <pat> -> <exp> in <exp'. There is no support for specifying the function arguments directly as part of the let. DO NOT write function arguments in the let pattern.",
  "- No 'rec' keyword is necessary for 'let' to define a recursive function. DO NOT use the 'rec' keyword with 'let'.",
  "- Format the code with proper linebreaks",
];

/*
 IDEA: take into account clipboard, past code positions, selections

 TODO: make holes rendered as some actual text; otherwise it tries to fill them...

 TODO: remove leading spaces before linebreaks from reply

 REMEMBER: HACKS in Code, Measured for reponse-wrapping ~ form.contents
 */

let _ctx_prompt = (ctx: Ctx.t, expected_ty: Typ.t): string => {
  /* If expected_ty not unknown, filter ctx to only include vars consistent with that type,
       or of arrow type where return type is consistent with that type. convert the var names and types
       to strings and seperte with commas.
     */
  switch (expected_ty) {
  | Unknown(_) => "LSP: No variables in scope are obviously relevant here\n"
  | expected_ty =>
    let nontrivially_consistent =
        (ctx: Ctx.t, ty_expect: Typ.t, ty_given: Typ.t): bool =>
      switch (ty_expect, ty_given) {
      //| (Unknown(_), _)
      | (_, Unknown(_)) => false
      | _ => Typ.is_consistent(ctx, ty_expect, ty_given)
      };
    let ctx' =
      List.filter_map(
        fun
        | Ctx.VarEntry({name, typ: Arrow(_, typ), _})
        | Ctx.ConstructorEntry({name, typ: Arrow(_, typ), _})
            when nontrivially_consistent(ctx, expected_ty, typ) =>
          Some(name ++ ": " ++ Typ.to_string(typ))
        | Ctx.VarEntry({name, typ, _})
        | Ctx.ConstructorEntry({name, typ, _})
            when nontrivially_consistent(ctx, expected_ty, typ) =>
          Some(name ++ ":" ++ Typ.to_string(typ))
        | _ => None,
        ctx,
      );
    "LSP: Consider using the following functions and constructors relevant to the expected type:\n  "
    ++ String.concat(",\n  ", ctx')
    ++ "\n";
  };
};

let mk_user_message = (~expected_ty, ~relevant_ctx, sketch: string): string =>
  "{\n"
  ++ String.concat(
       ",\n",
       List.filter_map(
         Fun.id,
         [
           Some("sketch: " ++ sketch),
           Option.map(Printf.sprintf("expected_ty: %s"), expected_ty),
           Option.map(Printf.sprintf("relevant_ctx:\n %s"), relevant_ctx),
         ],
       ),
     )
  ++ ",\n}";

let get_samples = (num_examples, samples) =>
  switch (Util.ListUtil.split_n_opt(num_examples, samples)) {
  | Some(samples) =>
    samples |> fst |> List.map(((s, t, u)) => (s, Some(t), u))
  | None => []
  };

let prompt =
    (
      {
        instructions,
        syntax_notes,
        num_examples,
        expected_type,
        relevant_ctx,
        _,
      }: FillerOptions.t,
      ~sketch,
      ~expected_ty,
      ~relevant_ctx_str,
    )
    : option(OpenAI.prompt) => {
  let+ () = String.trim(sketch) == "" ? None : Some();
  let system_prompt =
    (instructions ? main_prompt : [])
    @ (syntax_notes ? hazel_syntax_notes : []);
  let samples = get_samples(num_examples, samples);
  let expected_ty = expected_type ? Some(expected_ty) : None;
  let relevant_ctx = relevant_ctx ? Some(relevant_ctx_str) : None;
  OpenAI.[{role: System, content: String.concat("\n", system_prompt)}]
  @ Util.ListUtil.flat_map(
      ((sketch, expected_ty, completion)): list(OpenAI.message) =>
        [
          {
            role: User,
            content:
              mk_user_message(sketch, ~expected_ty, ~relevant_ctx=None),
          },
          {role: Assistant, content: completion},
        ],
      samples,
    )
  @ [
    {
      role: User,
      content: mk_user_message(sketch, ~expected_ty, ~relevant_ctx),
    },
  ];
};

let get_top_level_errs = (init_ctx, mode, top_ci: option(Info.exp)) => {
  let self: Self.t =
    switch (top_ci) {
    | Some({self, _}) =>
      switch (Self.typ_of_exp(init_ctx, self)) {
      | None => Just(Unknown(Internal))
      | Some(ty) => Just(ty)
      }
    | None => Just(Unknown(Internal))
    };
  let status = Info.status_common(init_ctx, mode, self);
  switch (status) {
  | InHole(Inconsistent(Expectation({ana, syn}))) => [
      "The suggested filling has the wrong expected type: expected "
      ++ Typ.to_string(ana)
      ++ ", but got "
      ++ Typ.to_string(syn)
      ++ ".",
    ]
  | _ => []
  };
};

let get_parse_err = (filling): Result.t(Zipper.t, string) =>
  switch (Printer.paste_into_zip(Zipper.init(), filling)) {
  | None => Error("Undocumented parse error, no feedback available")
  | Some(filling_z) =>
    //TODO(andrew): for syntax errors, also collect bad syntax eg % operator
    switch (Printer.of_backpack(filling_z)) {
    | [_, ..._] as orphans =>
      Error(
        "The parser has detected the following unmatched delimiters:. The presence of a '=>' in the list likely indicates that a '->' was mistakingly used in a case expression: "
        ++ String.concat(", ", orphans),
      )
    | [] => Ok(filling_z)
    }
  };

let mk_round_report = (~init_ctx, ~mode, reply: OpenAI.reply): round_report =>
  switch (get_parse_err(reply.content)) {
  | Error(err) => {reply, error_report: ParseError(err)}
  | Ok(filling_z) =>
    let (top_ci, info_map) =
      ChatLSP.get_info_and_top_ci_from_zipper(~ctx=init_ctx, filling_z);
    let static_errs =
      get_top_level_errs(init_ctx, mode, top_ci)
      @ ChatLSP.Errors.collect_static(info_map);
    if (List.length(static_errs) == 0) {
      {reply, error_report: NoErrors};
    } else {
      {reply, error_report: StaticErrors(static_errs)};
    };
  };

let error_reply =
    (~init_ctx: Ctx.t, ~mode: Mode.t, reply: OpenAI.reply)
    : (error_report, string) => {
  //TODO(andrew): this is implictly specialized for exp only
  let wrap = (intro, errs) =>
    [intro]
    @ errs
    @ [
      "Please try to address the error(s) by updating your previous code suggestion",
      "Please respond ONLY with the update suggestion",
    ]
    |> String.concat("\n");
  let error_report = mk_round_report(~init_ctx, ~mode, reply).error_report;
  let str =
    switch (error_report) {
    | NoErrors => ""
    | ParseError(err) => wrap("The following parse error occured:", [err])
    | StaticErrors(errs) =>
      wrap("The following static errors were discovered:", errs)
    };
  (error_report, str);
};

/*

 TODO(andrew): incorporate this into syntax error reporting
  levels of syntax error:

  general checks:
  a. get list of all backpack items (use label + shards to get actual delim)
  b. check if exist unbound kw: if, then, else, let, in, fun, case, test, end
  c. check if exist unbound false kws: match, of, with
  d. check if exist multiholes

  heuristics:
     if rule (|,=>) in backpack, suggest => instead of -> (check for ->?)
     if case, test in backpack, suggest end might be missing/unrecognized
     if let, type in backpack, suggest in might be missing/unrecognized
     if ( or [: suggest ) or ] might be missing

  // check if these exist unbound
  reserved: if, then, else, let, in, fun, case, test, end


  1: backpack empty, but exist multiholes:
  1a check for reserved/ false reserved
  1b suggest function application syntax

   */
/*print_endline("TESTING: ChatLSP.Errors.collect:");
  print_endline(
    ChatLSP.Errors.collect_static(info_map) |> String.concat("\n"),
  );*/
