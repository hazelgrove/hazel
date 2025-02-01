open Util;
open OptUtil.Syntax;
open Haz3lcore;

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

[@deriving (show({with_path: false}), sexp, yojson)]
type filler_options = {
  params: OpenAI.params,
  instructions: bool,
  syntax_notes: bool,
  num_examples: int,
  expected_type: bool,
  error_rounds_max: int,
  relevant_ctx: bool,
  rag: option(string),
};

let filler_options_init: filler_options = {
  params: OpenAI.default_params,
  instructions: true,
  syntax_notes: true,
  num_examples: 9,
  expected_type: true,
  error_rounds_max: 2,
  relevant_ctx: true,
  rag: None,
};

let pretty_print_seg =
    (~holes: option(string)=Some(""), segment: Segment.t): string =>
  Printer.to_rows(
    ~holes,
    ~measured=
      segment
      |> ZipperBase.MapPiece.of_segment(
           ProjectorPerform.Update.remove_any_projector,
         )
      |> Measured.of_segment(_, Id.Map.empty),
    ~caret=None,
    ~indent=" ",
    ~segment,
  )
  |> String.concat("\n");

module ErrorPrint = {
  /*
   ERRORS TODO:
   make multihole an error (say something about ap)
   do a completeness check
    */

  let prn = Printf.sprintf;

  let common_error: Info.error_common => string =
    fun
    // | NoType(MultiError) =>
    //   /* NOTE: possible cause explanation actually helps.
    //      e.g. when generating
    //      "if i == index then (description, not(done)) else (description, done)"
    //      it would tend not to parethesize the argument to not
    //       */
    //   prn(
    //     "Incomplete syntax (possible cause: remember that function application is c-style and requires parentheses around the argument)",
    //   )

    | NoType(BadToken(token)) => prn("\"%s\" isn't a valid token", token)
    | NoType(BadTrivAp(ty)) =>
      prn(
        "Function argument type \"%s\" inconsistent with ()",
        Typ.pretty_print(ty),
      )
    | Inconsistent(WithArrow(ty)) =>
      prn("type %s is not consistent with arrow type", Typ.pretty_print(ty))
    | NoType(FreeConstructor(_name)) => prn("Constructor is not defined")
    | Inconsistent(Internal(tys)) =>
      prn(
        "Expecting branches to have consistent types but got types: %s",
        List.map(Typ.pretty_print, tys) |> String.concat(", "),
      )
    | Inconsistent(Expectation({ana, syn})) =>
      prn(
        "Expecting type %s but got inconsistent type %s",
        Typ.pretty_print(ana),
        Typ.pretty_print(syn),
      );

  let exp_error: Info.error_exp => string =
    fun
    | FreeVariable(name) => "Variable " ++ name ++ " is not bound"
    | InexhaustiveMatch(_) => "TODO: Match is not exhaustive"
    | UnusedDeferral => "TODO: Unused deferral"
    | BadPartialAp(_) => "TODO: Bad partial app"
    | Common(error) => common_error(error);

  let pat_error: Info.error_pat => string =
    fun
    | ExpectedConstructor => "Expected a constructor"
    | Redundant(_) => "TODO: Redundant"
    | Common(error) => common_error(error);

  let typ_error: Info.error_typ => string =
    fun
    | FreeTypeVariable(name) => prn("Type variable %s is not bound", name)
    | BadToken(token) => prn("\"%s\" isn't a valid type token", token)
    | WantConstructorFoundAp => "Expected a constructor, found application"
    | WantConstructorFoundType(ty) =>
      prn("Expected a constructor, found type %s", Typ.pretty_print(ty))
    | WantTypeFoundAp => "Constructor application must be in sum"
    | DuplicateConstructor(name) =>
      prn("Constructor %s already used in this sum", name);

  let tpat_error: Info.error_tpat => string =
    fun
    | NotAVar(_) => "TODO: Not a valid type name"
    | ShadowsType(name, _source) => "TODO: Can't shadow type " ++ name;

  let string_of: Info.error => string =
    fun
    | Exp(error) => exp_error(error)
    | Pat(error) => pat_error(error)
    | Typ(error) => typ_error(error)
    | TPat(error) => tpat_error(error);

  let format_error = (term, error) =>
    prn("Error in term:\n  %s\nNature of error: %s", term, error);

  let term_string_of: Info.t => string =
    fun
    | InfoExp({term, _}) =>
      term
      |> ExpToSegment.exp_to_pretty(
           ~settings=
             ExpToSegment.Settings.of_core(~inline=false, CoreSettings.off),
         )
      |> pretty_print_seg(~holes=None)
    | InfoPat({term, _}) =>
      term
      |> ExpToSegment.pat_to_pretty(
           ~settings=
             ExpToSegment.Settings.of_core(~inline=false, CoreSettings.off),
         )
      |> pretty_print_seg(~holes=None)
    | InfoTyp({term, _}) => Typ.pretty_print(term)
    | InfoTPat({term, _}) =>
      term
      |> ExpToSegment.tpat_to_pretty(
           ~settings=
             ExpToSegment.Settings.of_core(~inline=false, CoreSettings.off),
         )
      |> pretty_print_seg(~holes=None)
    | Secondary(_) => "TODO";

  let collect_static = (info_map: Statics.Map.t): list(string) => {
    let errors =
      Id.Map.fold(
        (_id, info: Info.t, acc) =>
          switch (Info.error_of(info)) {
          | None => acc
          | Some(_) => [info] @ acc
          },
        info_map,
        [],
      );
    let errors = List.sort_uniq(compare, errors);
    List.filter_map(
      info =>
        switch (Info.error_of(info)) {
        | None => None
        | Some(error) =>
          let term = term_string_of(info);
          Some(format_error(term, string_of(error)));
        },
      errors,
    );
  };
};

module RelevantType = {
  let expected_ty_no_lookup = (mode: Mode.t): Typ.t => {
    switch (mode) {
    | Ana(ty) => ty
    | SynFun =>
      Typ.fresh(
        Arrow(Typ.fresh(Unknown(Internal)), Typ.fresh(Unknown(Internal))),
      )
    | Syn
    | SynTypFun => Typ.fresh(Unknown(SynSwitch))
    };
  };

  let expected_ty = (~ctx, mode: Mode.t): Typ.t => {
    switch (mode) {
    | Ana({term: Var(name), _}) when Ctx.lookup_alias(ctx, name) != None =>
      let ty_expanded = Ctx.lookup_alias(ctx, name) |> Option.get;
      ty_expanded;
    | _ => expected_ty_no_lookup(mode)
    };
  };

  let format_def = (alias: string, ty: Typ.t): string => {
    Printf.sprintf("type %s = %s in", alias, Typ.pretty_print(ty));
  };

  let subst_if_rec = ((name: string, ty: Typ.t)) => {
    switch (ty) {
    | {term: Rec(name', ty'), _} => (
        name,
        Typ.subst(Typ.fresh(Var(name)), name', ty'),
      )
    | _ => (name, ty)
    };
  };

  let rec get_vars = (ty: Typ.t): list(string) =>
    switch (ty.term) {
    | Int
    | Float
    | Bool
    | String
    | Unknown(_) => []
    | Var(x) => [x]
    | Arrow(ty1, ty2) => get_vars(ty1) @ get_vars(ty2)
    | Prod(tys) => ListUtil.flat_map(get_vars, tys)
    | Sum(sm) =>
      List.concat_map(
        fun
        | ConstructorMap.BadEntry(_) => []
        | Variant(_, _, None) => []
        | Variant(_, _, Some(typ)) => get_vars(typ),
        sm,
      )
    | Rec({term: Var(x), _}, ty) =>
      /* Remove recursive type references */
      get_vars(ty) |> List.filter((x': string) => x' != x)
    | Rec(_, ty) => get_vars(ty)
    | List(ty) => get_vars(ty)
    | Parens(ty) => get_vars(ty)
    | Forall({term: Var(x), _}, ty) =>
      get_vars(ty) |> List.filter((x': string) => x' != x)
    | Forall(_, ty) => get_vars(ty)
    | Ap(ty1, ty2) => get_vars(ty1) @ get_vars(ty2)
    };

  let rec collect_aliases_deep =
          (ctx: Ctx.t, ty: Typ.t): list((string, Typ.t)) => {
    let ty_vars = get_vars(ty);
    let defs =
      ListUtil.flat_map(
        var =>
          switch (Ctx.lookup_alias(ctx, var)) {
          | Some(ty) => [(var, ty)]
          | None => [(var, Typ.fresh(Unknown(Internal)))]
          },
        ty_vars,
      )
      |> List.sort_uniq(((x, _), (y, _)) => compare(x, y));
    let rec_calls =
      ListUtil.flat_map(
        ((_, ty')) => collect_aliases_deep(ctx, ty'),
        defs,
      );
    rec_calls @ defs;
  };

  let collate_aliases = (ctx, expected_ty'): option(string) => {
    let defs =
      collect_aliases_deep(ctx, expected_ty')
      |> Util.ListUtil.dedup
      |> List.map(subst_if_rec)
      |> List.map(((alias, ty)) => format_def(alias, ty));
    switch (defs) {
    | [] => None
    | _ => Some(defs |> String.concat("\n"))
    };
  };

  let expected = (~ctx, mode: Mode.t): string => {
    /*
     TODO(andrew): maybe include more than just the immediate type.
     like for example, when inside a fn(s), include
     argument types.
     like basically to benefit maximally from included type info,
     want to make sure we're including the full expansion of any type
     we might want to either case on or construct.
     expected type should mostly(?) give us the latter,
     but not always the former
     */
    let prefix = "# The expected type of the hole ?? is: ";
    switch (mode) {
    | Ana(ty) =>
      let defs =
        switch (collate_aliases(ctx, expected_ty_no_lookup(mode))) {
        | Some(defs) =>
          "# The following type definitions are likely relevant: #\n" ++ defs
        | None => "\n"
        };
      prefix
      ++ "a type consistent with "
      ++ Typ.pretty_print(ty)
      ++ " #\n"
      ++ defs;
    | SynFun =>
      prefix
      ++ "a type consistent with "
      ++ Typ.pretty_print(
           Typ.fresh(
             Arrow(
               Typ.fresh(Unknown(Internal)),
               Typ.fresh(Unknown(Internal)),
             ),
           ),
         )
      ++ " #"
    | Syn => prefix ++ "any type #"
    | _ => "Not applicable"
    };
  };
};

module RelevantCtx = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type filtered_entry = {
    name: string,
    typ: Typ.t,
    matched_type: Typ.t,
    depth: int,
  };

  let is_list_unk = (ty: Typ.t) =>
    switch (ty.term) {
    | List({term: Unknown(_), _}) => true
    | _ => false
    };

  let is_base = (ty: Typ.t): bool =>
    switch (ty.term) {
    | Int
    | Float
    | Bool
    | String => true
    | _ => false
    };

  let returns_base = (ty: Typ.t) =>
    switch (ty.term) {
    | Arrow(_, ty) => is_base(ty)
    | _ => false
    };

  /* Calculates the total number of nodes (compound
     and leaf)  in the type tree. */
  let rec num_nodes = (ty: Typ.t): int => {
    switch (ty.term) {
    | Int
    | Float
    | Bool
    | String
    | Unknown(_) => 1
    | Var(_) => 1
    | Arrow(t1, t2) => 1 + num_nodes(t1) + num_nodes(t2)
    | Prod(tys) =>
      1 + List.fold_left((acc, ty) => acc + num_nodes(ty), 0, tys)
    | Sum(sm) =>
      1
      + List.fold_left(
          (acc, variant) =>
            switch (variant) {
            | ConstructorMap.BadEntry(_) => acc
            | Variant(_, _, ty) =>
              acc + Util.OptUtil.get(() => 0, Option.map(num_nodes, ty))
            },
          0,
          sm,
        )
    | Rec(_, ty) => 1 + num_nodes(ty)
    | List(ty) => 1 + num_nodes(ty)
    | Parens(ty) => 1 + num_nodes(ty)
    | Forall(_, ty) => 1 + num_nodes(ty)
    | Ap(ty1, ty2) => 1 + num_nodes(ty1) + num_nodes(ty2)
    };
  };

  let rec count_unknowns = (ty: Typ.t): int =>
    switch (ty.term) {
    | Unknown(_) => 1
    | Int
    | Float
    | Bool
    | String
    | Var(_) => 0
    | Arrow(t1, t2) => count_unknowns(t1) + count_unknowns(t2)
    | Prod(tys) =>
      List.fold_left((acc, ty) => acc + count_unknowns(ty), 0, tys)
    | Sum(sm) =>
      List.fold_left(
        (acc, variant) =>
          switch (variant) {
          | ConstructorMap.BadEntry(_) => acc
          | Variant(_, _, ty) =>
            acc + Util.OptUtil.get(() => 0, Option.map(count_unknowns, ty))
          },
        0,
        sm,
      )
    | Rec(_, ty) => count_unknowns(ty)
    | List(ty) => count_unknowns(ty)
    | Parens(ty) => count_unknowns(ty)
    | Forall(_, ty) => count_unknowns(ty)
    | Ap(ty1, ty2) => count_unknowns(ty1) + count_unknowns(ty2)
    };

  let rec contains_sum_or_var = (ty: Typ.t): bool =>
    switch (ty.term) {
    | Int
    | Float
    | Bool
    | String
    | Unknown(_) => false
    | Var("Option") => false //TODO(andrew): hack for LSP
    | Var(_)
    | Sum(_) => true
    | Arrow(t1, t2) => contains_sum_or_var(t1) || contains_sum_or_var(t2)
    | Prod(tys) => List.exists(contains_sum_or_var, tys)
    | Rec(_, ty) => contains_sum_or_var(ty)
    | List(ty) => contains_sum_or_var(ty)
    | Parens(ty) => contains_sum_or_var(ty)
    | Forall(_, ty) => contains_sum_or_var(ty)
    | Ap(ty1, ty2) => contains_sum_or_var(ty1) || contains_sum_or_var(ty2)
    };

  /* Returns the ratio of type nodes which are the Unknown
     constructor. Must recurse and gather results from composite nodes */
  let unknown_ratio = (ty: Typ.t): float => {
    let total = float_of_int(num_nodes(ty));
    let unknowns = float_of_int(count_unknowns(ty));
    (total -. unknowns) /. total;
  };

  let score_type = (ty: Typ.t) => {
    let unk_ratio = unknown_ratio(ty);
    is_base(ty) ? 0.8 : unk_ratio;
  };

  let take_up_to_n = (n, xs) =>
    switch (Util.ListUtil.split_n_opt(n, xs)) {
    | Some((xs, _)) => xs
    | None => xs
    };

  let format_def = (name: string, ty: Typ.t) =>
    Printf.sprintf("let %s: %s =  in", name, Typ.pretty_print(ty));

  let filter_ctx = (ctx: Ctx.t, ty_expect: Typ.t): list(filtered_entry) =>
    List.filter_map(
      fun
      | Ctx.VarEntry({typ, name, _})
          when Typ.is_consistent(ctx, ty_expect, typ) =>
        Some({name, typ, depth: 0, matched_type: typ})
      | Ctx.VarEntry({typ: {term: Arrow(_, return_ty), _} as typ, name, _})
          when Typ.is_consistent(ctx, ty_expect, return_ty) =>
        Some({name, typ, matched_type: return_ty, depth: 1})
      | Ctx.VarEntry({
          typ: {term: Arrow(_, {term: Arrow(_, return_ty), _}), _} as typ,
          name,
          _,
        })
          when Typ.is_consistent(ctx, ty_expect, return_ty) =>
        Some({name, typ, matched_type: return_ty, depth: 2})
      | _ => None,
      ctx,
    );

  let str = (ctx: Ctx.t, mode: Mode.t): string => {
    let primary_goal: Typ.t =
      RelevantType.expected_ty(~ctx, mode) |> Typ.normalize(ctx);
    let secondary_targets =
      switch (primary_goal.term) {
      | Arrow(_source, target) =>
        let terts =
          switch (target.term) {
          | Prod(ts) => ts
          | _ => []
          };
        [target] @ terts;
      | _ => []
      };
    print_endline("primary_goal: " ++ Typ.pretty_print(primary_goal));
    print_endline(
      "secondary_targets: "
      ++ String.concat(",", List.map(Typ.pretty_print, secondary_targets)),
    );
    let primary_entries = filter_ctx(ctx, primary_goal);
    let secondary_entries =
      List.concat(List.map(filter_ctx(ctx, _), secondary_targets));
    let combined_entries =
      secondary_entries
      @ primary_entries
      |> Util.ListUtil.dedup
      |> List.sort((t1, t2) =>
           compare(score_type(t2.matched_type), score_type(t1.matched_type))
         )
      |> List.filter(entry => contains_sum_or_var(entry.typ));
    let entries =
      combined_entries
      |> take_up_to_n(8)
      |> List.map(({name, typ, _}) => format_def(name, typ))
      |> String.concat("\n");
    "# Consider using these variables relevant to the expected type: #\n"
    ++ entries;
  };
};

let mk_user_message =
    (
      ~expected_ty: option(string),
      ~relevant_ctx: option(string),
      sketch: string,
    )
    : string =>
  //TODO: proper JSON construction
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

module Samples = {
  let samples = [
    (
      {|
let List.length: [(String, Bool)]-> Int =
  fun xs ->
    ?? end in
|},
      RelevantType.expected(Ana(Typ.fresh(Int)), ~ctx=[]),
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
      RelevantType.expected(
        Ana(Typ.fresh(List(Typ.fresh(Bool)))),
        ~ctx=[],
      ),
      {|
case xs
| [] => []
| hd::tl => f(idx, hd)::go(idx + 1, tl)
|},
    ),
    (
      {|
type Container =
  + Pod(Bool)
  + CapsuleCluster(Int, Int) in
let total_capacity: Container -> Int =
  ??
in
|},
      RelevantType.expected(
        Ana(
          Typ.fresh(Arrow(Typ.fresh(Var("Container")), Typ.fresh(Int))),
        ),
        ~ctx=[],
      ),
      {|
fun c ->
    case c
      | Pod(b) => if !b && true then 1 else 0
      | CapsuleCluster(x, y) => x * y
    end
|},
    ),
    (
      "let f = ?? in f(5)",
      RelevantType.expected(Syn, ~ctx=[]),
      "fun x:Int -> ??",
    ),
    (
      {|let triple = (4, 8, true) in
let (_, y, condition) = triple in
let get: Option -> Int =
fun maybe_num ->
  case maybe_num
 | Some(x) => ??
 | None => if !condition then 0 else y + 1 end in|},
      RelevantType.expected(Ana(Typ.fresh(Int)), ~ctx=[]),
      "x",
    ),
    (
      "let num_or_zero = fun maybe_num ->\n case maybe_num\n | Some(num) => ?? \n| None => 0 end in",
      RelevantType.expected(Syn, ~ctx=[]),
      "num",
    ),
    (
      "let merge_sort: [Int]->[Int] =\n??\nin\nmerge_sort([4,1,3,7,2])",
      RelevantType.expected(
        Ana(
          Typ.fresh(
            Arrow(
              Typ.fresh(List(Typ.fresh(Int))),
              Typ.fresh(List(Typ.fresh(Int))),
            ),
          ),
        ),
        ~ctx=[],
      ),
      "fun list ->\nlet split: [Int]->([Int],[Int]) = fun left, right -> ?\nin\nlet merge: ([Int],[Int])->[Int]= ?\nin\nlet merge_sort_helper: [Int]->[Int]= ?\nin\nmerge_sort_helper(list)",
    ),
    (
      "type MenuItem =\n+ Breakfast(Int, Int)\n+ Lunch(Float)\nin\nlet per_lunch_unit = 0.95 in\nlet price: MenuItem-> Float   = fun m ->\ncase m\n| Breakfast(x, y) => ??\n| Lunch(f) => f *. per_lunch_unit\nend\nin price(Breakfast(1,2))/.3.",
      RelevantType.expected(Ana(Typ.fresh(Var("MenuItem"))), ~ctx=[]),
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
      RelevantType.expected(
        Ana(Typ.fresh(List(Typ.fresh(Unknown(Internal))))),
        ~ctx=[],
      ),
      {|
let mid = List.length(l) / 2 in
let left, right = List.take(mid, l), List.drop(mid, l) in
List.merge(cmp, merge_sort_helper(left), merge_sort_helper(right))
|},
    ),
  ];

  let get = (num_examples, samples) =>
    switch (Util.ListUtil.split_n_opt(num_examples, samples)) {
    | Some(samples) =>
      samples |> fst |> List.map(((s, t, u)) => (s, Some(t), u))
    | None => []
    };

  let mk = (num_examples: int): list(OpenAI.message) =>
    Util.ListUtil.flat_map(
      ((sketch, expected_ty, completion)): list(OpenAI.message) =>
        [
          {
            role: User,
            content:
              mk_user_message(sketch, ~expected_ty, ~relevant_ctx=None),
          },
          {role: Assistant, content: completion},
        ],
      get(num_examples, samples),
    );
};

module SystemPrompt = {
  let main_prompt = [
    "CODE COMPLETION INSTRUCTIONS:",
    "- Reply with a functional, idiomatic replacement for the program hole marked '??' in the provided program sketch",
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

  let mk =
      ({instructions, syntax_notes, num_examples, _}: filler_options)
      : list(OpenAI.message) => {
    let system_prompt =
      String.concat(
        "\n",
        (instructions ? main_prompt : [])
        @ (syntax_notes ? hazel_syntax_notes : []),
      );
    OpenAI.[{role: System, content: system_prompt}]
    @ Samples.mk(num_examples);
  };
};

module ErrorRound = {
  let get_top_level_errs = (init_ctx, mode, top_ci: Info.exp) => {
    let self: Self.t =
      switch (top_ci) {
      | {self, _} =>
        switch (Self.typ_of_exp(init_ctx, self)) {
        | None => Just(Typ.fresh(Unknown(Internal)))
        | Some(ty) => Just(ty)
        }
      };
    let status = Info.status_common(init_ctx, mode, self);
    switch (status) {
    | InHole(Inconsistent(Expectation({ana, syn}))) => [
        "The suggested filling has the wrong expected type: expected "
        ++ Typ.pretty_print(ana)
        ++ ", but got "
        ++ Typ.pretty_print(syn)
        ++ ".",
      ]
    | _ => []
    };
  };

  let get_parse_errs = (filling: string): Result.t(Zipper.t, string) =>
    switch (Printer.zipper_of_string(filling)) {
    | None => Error("Undocumented parse error, no feedback available")
    | Some(filling_z) =>
      //TODO(andrew): for syntax errors, also collect bad syntax eg % operator
      switch (
        filling_z.backpack
        |> List.map((s: Selection.t) =>
             Printer.of_segment(~holes=None, s.content)
           )
      ) {
      | [_, ..._] as orphans =>
        Error(
          "The parser has detected the following unmatched delimiters:. The presence of a '=>' in the list likely indicates that a '->' was mistakingly used in a case expression: "
          ++ String.concat(", ", orphans),
        )
      | [] => Ok(filling_z)
      }
    };

  let mk_round_report = (~init_ctx, ~mode, reply: OpenAI.reply): round_report =>
    switch (get_parse_errs(reply.content)) {
    | Error(err) => {reply, error_report: ParseError(err)}
    | Ok(filling_z) =>
      let (top_ci, info_map) =
        Statics.uexp_to_info_map(
          ~ctx=init_ctx,
          ~ancestors=[],
          MakeTerm.from_zip_for_sem(filling_z).term,
          Id.Map.empty,
        );
      let static_errs =
        get_top_level_errs(init_ctx, mode, top_ci)
        @ ErrorPrint.collect_static(info_map);
      if (List.length(static_errs) == 0) {
        {reply, error_report: NoErrors};
      } else {
        {reply, error_report: StaticErrors(static_errs)};
      };
    };

  let mk =
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
};

module InitPrompt = {
  let mk_msg =
      (
        {expected_type, relevant_ctx, _}: filler_options,
        ci: Info.t,
        sketch: Segment.t,
      )
      : option(string) => {
    //TODO: Proper errors
    let* mode =
      switch (ci) {
      | InfoExp({mode, _}) => Some(mode)
      | InfoPat({mode, _}) => Some(mode)
      | _ => None
      };
    let sketch = pretty_print_seg(~holes=Some("?"), sketch);
    let+ () = String.trim(sketch) == "" ? None : Some();
    let ctx_at_caret = Info.ctx_of(ci);
    let expected_ty =
      expected_type
        ? Some(RelevantType.expected(~ctx=ctx_at_caret, mode)) : None;
    let relevant_ctx =
      relevant_ctx ? Some(RelevantCtx.str(ctx_at_caret, mode)) : None;
    mk_user_message(sketch, ~expected_ty, ~relevant_ctx);
  };

  let mk =
      (filler_options: filler_options, ci: Info.t, sketch: Segment.t)
      : option(OpenAI.prompt) => {
    let+ user_message = mk_msg(filler_options, ci, sketch);
    SystemPrompt.mk(filler_options) @ [{role: User, content: user_message}];
  };
};
