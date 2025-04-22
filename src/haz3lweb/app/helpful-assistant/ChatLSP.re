open Util;
open OptUtil.Syntax;
open Haz3lcore;
module StringSet = Set.Make(String);

let prn = Printf.sprintf;

let statics_of_exp_zipper =
    (init_ctx: Ctx.t, z: Zipper.t): (Info.exp, Statics.Map.t) =>
  Statics.uexp_to_info_map(
    ~ctx=init_ctx,
    ~ancestors=[],
    MakeTerm.from_zip_for_sem(z).term,
    Id.Map.empty,
    ~duplicates=[],
    ~expected_labels=None,
    ~label_sort=false,
  );

module Options = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    params: OpenRouter.params,
    instructions: bool,
    syntax_notes: bool,
    num_examples: int,
    expected_type: bool,
    relevant_ctx: bool,
    error_rounds_max: int,
  };

  let init: t = {
    params: OpenRouter.default_params,
    instructions: true,
    syntax_notes: true,
    num_examples: 9,
    expected_type: true,
    relevant_ctx: true,
    error_rounds_max: 2,
  };
};

module Print = {
  let seg = (~holes: option(string)=Some(""), segment: Segment.t): string => {
    let segment =
      ZipperBase.MapPiece.of_segment(
        syntax => [ProjectorPerform.remove_any_projector(syntax)],
        segment,
      );
    Printer.to_rows(
      ~holes,
      ~measured=Measured.of_segment(segment, Id.Map.empty),
      ~caret=None,
      ~indent=" ",
      ~segment,
    )
    |> String.concat("\n");
  };

  let term = (term: Term.Any.t): string => {
    let settings =
      ExpToSegment.Settings.of_core(~inline=false, CoreSettings.off);
    term |> ExpToSegment.any_to_pretty(~settings) |> seg(~holes=None);
  };

  let typ = (ty: Typ.t): string => term(Typ(ty));
};

module ErrorPrint = {
  [@deriving (show({with_path: false}), yojson, sexp)]
  type t =
    | ParseError(string)
    | StaticErrors(list(string))
    | NoErrors;

  /* TODO:
      Better errors for more broken programs: Completeness/formedness checks / multihole errors
      Contextualize errors with line numbers (would need to add them to the sketch), or
     including surrounding syntax, or inlining an error representation into a provided sketch */

  let common_error: Info.error_common => string =
    fun
    // TODO: This error class doesn't seem to exist anymore, not sure what happens to multiholes now
    // | NoType(MultiError) =>
    //   /* NOTE: possible cause explanation actually helps.
    //      e.g. when generating
    //      "if i == index then (description, not(done)) else (description, done)"
    //      it would tend not to parethesize the argument to not
    //       */
    //   prn(
    //     "Incomplete syntax (possible cause: remember that function application is c-style and requires parentheses around the argument)",
    //   )
    | NoType(WantTuple) => "Expected a tuple"
    | NoType(LabelNotFound(_, _)) => "Label not found"
    | NoType(BadLabel(_)) => "Invalid label"
    | NoType(InvalidLabel(_)) => "Invalid label"
    | DuplicateLabel(_, _) => "Duplicate label"
    | TupleLabelError(_) => "Invalid tuple label"
    | NoType(BadToken(token)) => prn("\"%s\" isn't a valid token", token)
    | NoType(BadTrivAp(ty)) =>
      prn(
        "Function argument type \"%s\" inconsistent with ()",
        Print.typ(ty),
      )
    | Inconsistent(WithArrow(ty)) =>
      prn("type %s is not consistent with arrow type", Print.typ(ty))
    | NoType(FreeConstructor(_name)) => prn("Constructor is not defined")
    | NoType(BadOperator(msg)) => prn("Invalid operator: %s", msg)
    | InvalidUseMode({bad_typ, _}) =>
      prn(
        "Cannot use type %s for number operators and literals.",
        Print.typ(bad_typ),
      )
    | Inconsistent(Internal(tys)) =>
      prn(
        "Expecting branches to have consistent types but got types: %s",
        List.map(Print.typ, tys) |> String.concat(", "),
      )
    | Inconsistent(Expectation({ana, syn})) =>
      prn(
        "Expecting type %s but got inconsistent type %s",
        Print.typ(ana),
        Print.typ(syn),
      );

  let exp_error: Info.error_exp => string =
    fun
    | FreeVariable(name) => "Variable " ++ name ++ " is not bound"
    | InexhaustiveMatch(_) => "Match is not exhaustive" //TODO: elaborate
    | UnusedDeferral => "Unused deferral" //TODO: better message
    | BadPartialAp(_) => "Bad partial application" //TODO: elaborate
    | Common(error) => common_error(error);

  let pat_error: Info.error_pat => string =
    fun
    | ExpectedConstructor => "Expected a constructor"
    | Redundant(_) => "Redundant" //TODO: elaborate
    | Common(error) => common_error(error);

  let typ_error: Info.error_typ => string =
    fun
    | FreeTypeVariable(name) => prn("Type variable %s is not bound", name)
    | BadToken(token) => prn("\"%s\" isn't a valid type token", token)
    | WantConstructorFoundAp => "Expected a constructor, found application"
    | WantConstructorFoundType(ty) =>
      prn("Expected a constructor, found type %s", Print.typ(ty))
    | WantTypeFoundAp => "Constructor application must be in sum"
    | DuplicateConstructor(name) =>
      prn("Constructor %s already used in this sum", name)
    | WantTuple => "Expected a tuple"
    | WantLabel => "Expected a label"
    | DuplicateLabels(labels, ty) =>
      prn(
        "Duplicate labels in type %s: %s",
        Print.typ(ty),
        String.concat(", ", labels),
      )
    | Duplicate(name, _) => prn("Type %s is already defined", name);

  let tpat_error: Info.error_tpat => string =
    fun
    | NotAVar(_) => "Not a valid type name" //TODO: elaborate
    | ShadowsType(name, _source) => "Can't shadow type " ++ name; //TODO: elaborate

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
    | InfoExp({term, _}) => Print.term(Exp(term))
    | InfoPat({term, _}) => Print.term(Pat(term))
    | InfoTyp({term, _}) => Print.term(Typ(term))
    | InfoTPat({term, _}) => Print.term(TPat(term))
    | Secondary(_) => failwith("ChatLSP: term_string_of: Secondary");

  let collect_static = (info_map: Statics.Map.t): list(string) => {
    Id.Map.fold(
      (_id, info: Info.t, acc) =>
        switch (Info.error_of(info)) {
        | None => acc
        | Some(_) => [info] @ acc
        },
      info_map,
      [],
    )
    |> List.sort_uniq(compare)
    |> List.filter_map(info =>
         switch (Info.error_of(info)) {
         | None => None
         | Some(error) =>
           let term = term_string_of(info);
           Some(format_error(term, string_of(error)));
         }
       );
  };

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
        "The suggested completion has the wrong expected type: expected "
        ++ Print.typ(ana)
        ++ ", but got "
        ++ Print.typ(syn)
        ++ ".",
      ]
    | _ => []
    };
  };

  let get_parse_errs =
      (sketch_z: Zipper.t, completion: string): Result.t(Zipper.t, string) =>
    switch (
      {
        let* sketch_z = Destruct.go(Left, sketch_z);
        let* sketch_z = Destruct.go(Left, sketch_z);
        Perform.paste(sketch_z, completion);
      }
    ) {
    | None => Error("Undocumented parse error, no feedback available")
    | Some(completion_z) =>
      //TODO: For syntax errors, also collect bad syntax eg % operator
      switch (
        completion_z.backpack
        |> List.map((s: Selection.t) =>
             Printer.of_segment(~holes=None, s.content)
           )
      ) {
      | [_, ..._] as orphans =>
        Error(
          "The parser has detected the following unmatched delimiters:. The presence of a '=>' in the list likely indicates that a '->' was mistakingly used in a case expression: "
          ++ String.concat(", ", orphans),
        )
      | [] => Ok(completion_z)
      }
    };

  let mk_errors = (~init_ctx, sketch_z: Zipper.t, reply: string): t =>
    switch (get_parse_errs(sketch_z, reply)) {
    | Error(err) => ParseError(err)
    | Ok(full_z) =>
      //TODO: This is implictly specialized for expressions only
      let (_, info_map) = statics_of_exp_zipper(init_ctx, sketch_z);
      let static_errs_sketch = collect_static(info_map);
      let (_, info_map) = statics_of_exp_zipper(init_ctx, full_z);
      let static_errs_full = collect_static(info_map);
      if (List.length(static_errs_full) == 0) {
        NoErrors;
      } else {
        let sketch_errs = StringSet.of_list(static_errs_sketch);
        let new_errs =
          List.filter(
            err => !StringSet.mem(err, sketch_errs),
            static_errs_full,
          );
        if (List.length(new_errs) == 0) {
          NoErrors;
        } else {
          StaticErrors(new_errs);
        };
      };
    };

  let mk =
      (~init_ctx: Ctx.t, sketch_z: Zipper.t, reply: string): option(string) => {
    let wrap = (intro, errs) =>
      [intro]
      @ errs
      @ [
        "Please try to address the error(s) by updating your previous code suggestion",
        "Please respond ONLY with the update suggestion",
      ]
      |> String.concat("\n");
    let error_report = mk_errors(~init_ctx, sketch_z, reply);
    switch (error_report) {
    | NoErrors => None
    | ParseError(err) =>
      Some(wrap("The following parse error occured:", [err]))
    | StaticErrors(errs) =>
      Some(wrap("The following static errors were discovered:", errs))
    };
  };
};

module RelevantType = {
  let format_def = (alias: string, ty: Typ.t): string =>
    prn("type %s = %s in", alias, Print.typ(ty));

  let subst_if_rec = ((name: string, ty: Typ.t)): (string, Typ.t) => {
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
    | Atom(_)
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
    | Label(_) => []
    | TupLabel(_, ty) => get_vars(ty)
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

  let collate_aliases = (ctx: Ctx.t, expected_ty': Typ.t): option(string) => {
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

  let expected =
      (~ctx: Ctx.t, ana_ty: Typ.t, completion_token: string): string => {
    /* TODO: Maybe include more than just the immediate type.
     * like for example, when inside a fn(s), include argument types.
     * Like basically to benefit maximally from included type info,
     * want to make sure we're including the full expansion of any type
     * we might want to either case on or construct. Rxpected type should
     * mostly(?) give us the latter, but not always the former. */
    let prefix =
      "# The expected type of the hole " ++ completion_token ++ " is: ";
    let defs =
      switch (collate_aliases(ctx, ana_ty)) {
      | Some(defs) =>
        "# The following type definitions are likely relevant: #\n" ++ defs
      | None => "\n"
      };
    prefix ++ "a type consistent with " ++ Print.typ(ana_ty) ++ " #\n" ++ defs;
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

  /* TODO: For all functions on types, we want to makse sure we're
   * normalizing first where appropriate (replacing type aliases with
   * their definitions). Where it's always appropriate, internalize it
   *  into the relevant function; otherwise, list it as a precondition */

  /* TODO: Some of the functions below were hastily updated to dev.
   * The new cases of Typ (Parens, Forsll, Ap) especially should be
   * double-checked */

  let is_list_unk = (ty: Typ.t): bool =>
    switch (ty.term) {
    | List({term: Unknown(_), _}) => true
    | _ => false
    };

  let is_base = (ty: Typ.t): bool =>
    switch (ty.term) {
    | Atom(_) => true
    | _ => false
    };

  let returns_base = (ty: Typ.t): bool =>
    switch (ty.term) {
    | Arrow(_, ty) => is_base(ty)
    | _ => false
    };

  /* Calculates the total number of nodes (compound
     and leaf)  in the type tree. */
  let rec num_nodes = (ty: Typ.t): int => {
    switch (ty.term) {
    | Atom(_)
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
    | Label(_) => 1
    | TupLabel(_, ty) => 1 + num_nodes(ty)
    };
  };

  let rec count_unknowns = (ty: Typ.t): int =>
    switch (ty.term) {
    | Unknown(_) => 1
    | Atom(_)
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
    | Label(_) => 0
    | TupLabel(_, ty) => count_unknowns(ty)
    };

  let rec contains_sum_or_var = (ty: Typ.t): bool =>
    switch (ty.term) {
    | Atom(_)
    | Unknown(_) => false
    | Var("Option") => false //TODO: hack for LSP
    | Var(_)
    | Sum(_) => true
    | Arrow(t1, t2) => contains_sum_or_var(t1) || contains_sum_or_var(t2)
    | Prod(tys) => List.exists(contains_sum_or_var, tys)
    | Rec(_, ty) => contains_sum_or_var(ty)
    | List(ty) => contains_sum_or_var(ty)
    | Parens(ty) => contains_sum_or_var(ty)
    | Forall(_, ty) => contains_sum_or_var(ty)
    | Ap(ty1, ty2) => contains_sum_or_var(ty1) || contains_sum_or_var(ty2)
    | Label(_) => false
    | TupLabel(_, ty) => contains_sum_or_var(ty)
    };

  /* Returns the ratio of type nodes which are the Unknown
     constructor. Must recurse and gather results from composite nodes */
  let unknown_ratio = (ty: Typ.t): float => {
    let total = float_of_int(num_nodes(ty));
    let unknowns = float_of_int(count_unknowns(ty));
    (total -. unknowns) /. total;
  };

  let score_type = (ty: Typ.t): float => {
    let unk_ratio = unknown_ratio(ty);
    is_base(ty) ? 0.8 : unk_ratio;
  };

  let take_up_to_n = (n: int, xs: list('a)): list('a) =>
    switch (Util.ListUtil.split_n_opt(n, xs)) {
    | Some((xs, _)) => xs
    | None => xs
    };

  let format_def = (name: string, ty: Typ.t) =>
    prn("let %s: %s =  in", name, Print.typ(ty));

  let filter_ctx = (ctx: Ctx.t, ty_expect: Typ.t): list(filtered_entry) =>
    List.filter_map(
      fun
      | Ctx.VarEntry({typ, name, _})
          when Typ.is_consistent(ctx, ty_expect, typ) =>
        Some({
          name,
          typ,
          depth: 0,
          matched_type: typ,
        })
      | Ctx.VarEntry({typ: {term: Arrow(_, return_ty), _} as typ, name, _})
          when Typ.is_consistent(ctx, ty_expect, return_ty) =>
        Some({
          name,
          typ,
          matched_type: return_ty,
          depth: 1,
        })
      | Ctx.VarEntry({
          typ: {term: Arrow(_, {term: Arrow(_, return_ty), _}), _} as typ,
          name,
          _,
        })
          when Typ.is_consistent(ctx, ty_expect, return_ty) =>
        Some({
          name,
          typ,
          matched_type: return_ty,
          depth: 2,
        })
      | _ => None,
      ctx.entries,
    );

  let str = (ctx: Ctx.t, primary_goal: Typ.t): string => {
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

module Samples = {
  type t = list((string, string, string));
  let samples = (completion_token: string, advanced_reasoning: bool): t => [
    (
      {|
let List.length: [(String, Bool)]-> Int =
  fun xs ->
    |}
      ++ completion_token
      ++ {| end in
|},
      RelevantType.expected(
        Typ.fresh(Atom(Int)),
        ~ctx=Ctx.empty,
        completion_token,
      ),
      advanced_reasoning
        ? {|
Discussion:
The function List.length takes a list of (String, Bool) tuples and returns an Int. The natural way to compute the length of a list is through recursion.
The base case for an empty list is 0, and for a non-empty list, we increment the count and recursively call List.length on the tail.
```case xs
| [] => 0
| _::xs => 1 + List.length(xs)
```|}
        : {|
case xs
| [] => 0
| _::xs => 1 + List.length(xs)|},
    ),
    (
      {|
let List.mapi: ((Int, Bool) -> Bool, [Bool]) -> [Bool]=
  fun f, xs ->
    let go: (Int, [Bool])-> [Bool] = fun idx, xs ->
      |}
      ++ completion_token
      ++ {| end in
    go(0, xs) in
|},
      RelevantType.expected(
        Typ.fresh(List(Typ.fresh(Atom(Bool)))),
        ~ctx=Ctx.empty,
        completion_token,
      ),
      advanced_reasoning
        ? {|
Discussion:
The function List.mapi applies a function f to each element of a list while keeping track of the index. The helper function go does this recursively.
The base case returns an empty list. In the recursive case, f(idx, hd) is applied to the head, and go(idx + 1, tl) is called recursively on the tail to process the rest of the list.
```case xs
| [] => []
| hd::tl => f(idx, hd)::go(idx + 1, tl)
```|}
        : {|
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
  |}
      ++ completion_token
      ++ {|
in
|},
      RelevantType.expected(
        Typ.fresh(
          Arrow(Typ.fresh(Var("Container")), Typ.fresh(Atom(Int))),
        ),
        ~ctx=Ctx.empty,
        completion_token,
      ),
      advanced_reasoning
        ? {|
Discussion:
The function total_capacity takes a Container and returns an Int. The Pod variant stores a Bool, which likely indicates whether the pod is active.
The condition if !b && true simplifies to if !b, meaning inactive pods have a capacity of 1, while active ones have 0.
The CapsuleCluster variant contains two integers, which are multiplied together to represent the total capacity.
```fun c ->
    case c
      | Pod(b) => if !b && true then 1 else 0
      | CapsuleCluster(x, y) => x * y
    end
```
|}
        : {|
fun c ->
    case c
      | Pod(b) => if !b && true then 1 else 0
      | CapsuleCluster(x, y) => x * y
    end
|},
    ),
    (
      "let f = " ++ completion_token ++ " in f(5)",
      RelevantType.expected(
        Typ.fresh(Unknown(Internal)),
        ~ctx=Ctx.empty,
        completion_token,
      ),
      advanced_reasoning
        ? {|
Discussion:
The expression let f = ?a in f(5) means f should be a function that can take an integer input. A function of type fun x:Int -> ?a is defined, but its body is missing.
Since no constraints are placed on the output type, the hole could be filled with any valid expression.
```
fun x:Int -> ?a
```
      |}
        : "fun x:Int -> ??",
    ),
    (
      {|let triple = (4, 8, true) in
let (_, y, condition) = triple in
let get: Option -> Int =
fun maybe_num ->
  case maybe_num
 | Some(x) => |}
      ++ completion_token
      ++ {|
 | None => if !condition then 0 else y + 1 end in|},
      RelevantType.expected(
        Typ.fresh(Atom(Int)),
        ~ctx=Ctx.empty,
        completion_token,
      ),
      advanced_reasoning
        ? {|
Discussion:
The function get extracts a value from an Option type. If Some(x), the function should return x, as x is already of type Int.
The None case considers a condition; if !condition is true, it returns 0, otherwise, it returns y + 1.
Since x is an Int, returning it in the Some case maintains type consistency.
```
x
```
      |}
        : "x",
    ),
    (
      "let num_or_zero = fun maybe_num ->\n case maybe_num\n | Some(num) => "
      ++ completion_token
      ++ " \n| None => 0 end in",
      RelevantType.expected(
        Typ.fresh(Unknown(Internal)),
        ~ctx=Ctx.empty,
        completion_token,
      ),
      advanced_reasoning
        ? {|
Discussion:
The function num_or_zero takes an Option(Int) and returns an Int. If the input is Some(num), it should return num, as num is already an integer.
If None, the function defaults to returning 0. This ensures type consistency while preserving the stored number when available.
```
num
```
      |}
        : "num",
    ),
    (
      "let merge_sort: [Int]->[Int] =\n"
      ++ completion_token
      ++ "\nin\nmerge_sort([4,1,3,7,2])",
      RelevantType.expected(
        Typ.fresh(
          Arrow(
            Typ.fresh(List(Typ.fresh(Atom(Int)))),
            Typ.fresh(List(Typ.fresh(Atom(Int)))),
          ),
        ),
        ~ctx=Ctx.empty,
        completion_token,
      ),
      advanced_reasoning
        ? {|
Discussion:
The function merge_sort sorts a list of integers. A common approach to implementing merge sort involves:
1. Splitting the list into two halves (split).
2. Recursively sorting both halves (merge_sort_helper).
3. Merging the sorted halves (merge).
The provided structure follows this approach, so we use helper functions to complete the sorting logic.
```
fun list ->\nlet split: [Int]->([Int],[Int]) = fun left, right -> ?\nin\nlet merge: ([Int],[Int])->[Int]= ?\nin\nlet merge_sort_helper: [Int]->[Int]= ?\nin\nmerge_sort_helper(list)
```
      |}
        : "fun list ->\nlet split: [Int]->([Int],[Int]) = fun left, right -> ?\nin\nlet merge: ([Int],[Int])->[Int]= ?\nin\nlet merge_sort_helper: [Int]->[Int]= ?\nin\nmerge_sort_helper(list)",
    ),
    (
      "type MenuItem =\n+ Breakfast(Int, Int)\n+ Lunch(Float)\nin\nlet per_lunch_unit = 0.95 in\nlet price: MenuItem-> Float   = fun m ->\ncase m\n| Breakfast(x, y) => "
      ++ completion_token
      ++ "\n| Lunch(f) => f *. per_lunch_unit\nend\nin price(Breakfast(1,2))/.3.",
      RelevantType.expected(
        Typ.fresh(Var("MenuItem")),
        ~ctx=Ctx.empty,
        completion_token,
      ),
      advanced_reasoning
        ? {|
Discussion:
The function price computes the cost of a MenuItem. The Lunch variant already has a predefined price calculation. For Breakfast(x, y), an expression must return a Float, but the completion is missing.
The function should ensure a proper numeric computation based on x and y.
```
fun m ->\ncase m\n| Breakfast(x, y) => ?a\n| Lunch(f) => f *. per_lunch_unit\nend
```
      |}
        : "fun m ->\ncase m\n| Breakfast(x, y) => ??\n| Lunch(f) => f *. per_lunch_unit\nend",
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
| _ => |}
      ++ completion_token
      ++ {|
end
in merge_sort_helper(list)
in
test 2 == List.nth(List.sort(fun a, b -> a<b, [4,1,3,2]), 1) end
    |},
      RelevantType.expected(
        Typ.fresh(List(Typ.fresh(Unknown(Internal)))),
        ~ctx=Ctx.empty,
        completion_token,
      ),
      advanced_reasoning
        ? {|
Discussion:
The function List.merge merges two sorted lists using a comparator function cmp. The List.sort function applies merge sort, using merge_sort_helper to recursively divide and sort the list.
The base cases return [] or a single-element list. The recursive case splits the list into two halves and merges sorted sublists.
```let mid = List.length(l) / 2 in
let left, right = List.take(mid, l), List.drop(mid, l) in
List.merge(cmp, merge_sort_helper(left), merge_sort_helper(right))
```|}
        : {|
let mid = List.length(l) / 2 in
let left, right = List.take(mid, l), List.drop(mid, l) in
List.merge(cmp, merge_sort_helper(left), merge_sort_helper(right))
|},
    ),
  ];

  let get =
      (num_examples: int, completion_token: string, advanced_reasoning: bool) =>
    switch (
      Util.ListUtil.split_n_opt(
        num_examples,
        samples(completion_token, advanced_reasoning),
      )
    ) {
    | Some(samples) =>
      samples |> fst |> List.map(((s, t, u)) => (s, Some(t), u))
    | None => []
    };
};

module SystemPrompt = {
  let main_prompt = (completion_token: string) => [
    "CODE COMPLETION INSTRUCTIONS:",
    "- Reply with a functional, idiomatic replacement for the program hole marked '"
    ++ completion_token
    ++ "' in the provided program sketch",
    "- The program hole marked '"
    ++ completion_token
    ++ "' consists of the hole token '??' appended with a series of characters to help you differentiate between other holes in the program",
    // Uncomment either of the following lines to test error rounds
    // "- However, make sure that your initial response CAUSES A TYPE ERROR in the program. Then, fix it in your second response",
    // "- However, it is CRITICALLY important to make sure that your response ALWAYS CAUSES A TYPE ERROR in the program, no matter how many times you are re-prompted",
    "- Reply ONLY with a SINGLE replacement term for the unqiue distinguished hole marked '"
    ++ completion_token
    ++ "'",
    "- Reply ONLY with code",
    "- DO NOT suggest more replacements for other holes in the sketch (marked, '?', '??', or '?a'), or implicit holes",
    "- This is critical, and I am going to reiterate it: DO NOT suggest more than one replacement term. It should ONLY be for the hole marked '"
    ++ completion_token
    ++ "'",
    "- For example, if you are being asked to complete 'let f = ? in "
    ++ completion_token
    ++ "', your response should ONLY be a single replacement term for the hole marked '"
    ++ completion_token
    ++ "', NOT a replacement term for the hole marked '?'",
    "- i.e. You should ONLY respond with a function application, or something else which would be a valid replacement term for the hole marked '"
    ++ completion_token
    ++ "'",
    "- If you wish to include a hole in your response, use '??' only, without the appended characters that were used to identify the specific hole you were given",
    "- IT WOULD BE A HUGE MISTAKE TO RESPOND WITH A FUNCTION BODY FOR THE HOLE MARKED '?'",
    "- DO NOT include the program sketch in your reply",
    "- DO NOT include a period at the end of your response and DO NOT use markdown",
  ];

  let advanced_reasoning_prompt = (completion_token: string) => [
    "CODE COMPLETION INSTRUCTIONS:",
    "- First, provide a brief discussion of your approach and reasoning",
    "- Then, provide your code completion for the hole marked '"
    ++ completion_token
    ++ "' enclosed in triple backticks",
    "- The program hole marked '"
    ++ completion_token
    ++ "' consists of the hole token '?a' appended with a series of characters to help you differentiate between other holes in the program",
    "- Your response MUST include two parts:",
    "  1. A discussion section explaining your approach",
    "  2. Your code completion inside triple backticks",
    "- DO NOT include anything else in your response",
    "- DO NOT provide multiple code suggestions",
    "- DO NOT include any text after the code block",
    "- Here is an example of the format you should follow:",
    "- Discussion:",
    "- The function takes an integer n as input and returns a float.",
    "- The base case returns 1.0 when n is 0, ensuring the function adheres to the expected Float return type.",
    "- For all other cases, the function returns 2.0, maintaining consistency in return type while providing a simple branching structure.",
    "  ```",
    "  fun n -> if n == 0 then 1.0 else 2.0",
    "  ```",
    "- The code completion should be a functional, idiomatic replacement for the program hole marked '"
    ++ completion_token
    ++ "' in the provided program sketch",
    // Uncomment either of the following lines to test error rounds
    // "- However, make sure that your initial response CAUSES A TYPE ERROR in the program. Then, fix it in your second response",
    // "- However, it is CRITICALLY important to make sure that your response ALWAYS CAUSES A TYPE ERROR in the program, no matter how many times you are re-prompted",
    "- Reply ONLY with a SINGLE replacement term for the unique distinguished hole marked '"
    ++ completion_token
    ++ "'",
    "- DO NOT suggest more replacements for other holes in the sketch (marked '?', '??', or '?a'), or implicit holes",
    "- This is critical, and I am going to reiterate it: DO NOT suggest more than one replacement term. It should ONLY be for the hole marked '"
    ++ completion_token
    ++ "'",
    "- For example, if you are being asked to complete 'let f = ? in "
    ++ completion_token
    ++ "', your response should ONLY be a single replacement term for the hole marked '"
    ++ completion_token
    ++ "', NOT a replacement term for the hole marked '?'",
    "- i.e. You should ONLY respond with a function application, or something else which would be a valid replacement term for the hole marked '"
    ++ completion_token
    ++ "'",
    "- If you wish to include a hole in your response, use '?a' only, without the appended characters that were used to identify the specific hole you were given",
    "- IT WOULD BE A HUGE MISTAKE TO RESPOND WITH A FUNCTION BODY FOR THE HOLE MARKED '?'",
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
      (
        {instructions, syntax_notes, _}: Options.t,
        completion_token: string,
        advanced_reasoning: bool,
      )
      : string =>
    String.concat(
      "\n",
      (
        instructions
          ? advanced_reasoning
              ? advanced_reasoning_prompt(completion_token)
              : main_prompt(completion_token)
          : []
      )
      @ (syntax_notes ? hazel_syntax_notes : []),
    );
};

module Prompt = {
  //TODO: Build JSON instead of string
  let mk_user_message =
      (
        ~expected_ty: option(string),
        ~relevant_ctx: option(string),
        sketch: string,
      )
      : string =>
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

  let static_context =
      (
        {expected_type, relevant_ctx, _}: Options.t,
        ci: Info.t,
        sketch: Segment.t,
        completion_token: string,
      )
      : option(string) => {
    //TODO: Proper errors
    //TODO: support non exp/pat positions
    let* ty_ana =
      switch (ci) {
      | InfoExp({ana, _})
      | InfoPat({ana, _}) => Some(ana)
      | _ => None
      };
    let sketch = Print.seg(~holes=Some("?"), sketch);
    let+ () = String.trim(sketch) == "" ? None : Some();
    let ctx_at_caret = Info.ctx_of(ci);
    let expected_ty =
      expected_type
        ? Some(
            RelevantType.expected(
              ~ctx=ctx_at_caret,
              ty_ana,
              completion_token,
            ),
          )
        : None;
    let primary_goal: Typ.t =
      (
        switch (ty_ana) {
        | {term: Var(name), _}
            when Ctx.lookup_alias(ctx_at_caret, name) != None =>
          let ty_expanded =
            Ctx.lookup_alias(ctx_at_caret, name) |> Option.get;
          ty_expanded;
        | _ => ty_ana
        }
      )
      |> Typ.normalize(ctx_at_caret);
    let relevant_ctx =
      relevant_ctx
        ? Some(RelevantCtx.str(ctx_at_caret, primary_goal)) : None;
    mk_user_message(sketch, ~expected_ty, ~relevant_ctx);
  };

  let samples =
      (num_examples: int, completion_token: string, advanced_reasoning: bool)
      : list(OpenRouter.message) =>
    Util.ListUtil.flat_map(
      ((sketch, expected_ty, completion)): list(OpenRouter.message) =>
        [
          {
            role: User,
            content:
              mk_user_message(sketch, ~expected_ty, ~relevant_ctx=None),
          },
          {
            role: Assistant,
            content: completion,
          },
        ],
      Samples.get(num_examples, completion_token, advanced_reasoning),
    );

  let mk_init =
      (
        options: Options.t,
        ci: Info.t,
        sketch: Segment.t,
        completion_token: string,
        advanced_reasoning: bool,
      )
      : option(OpenRouter.prompt) => {
    let+ user_message = static_context(options, ci, sketch, completion_token);
    OpenRouter.[
      {
        role: System,
        content:
          SystemPrompt.mk(options, completion_token, advanced_reasoning),
      },
    ]
    @ samples(options.num_examples, completion_token, advanced_reasoning)
    @ [
      {
        role: User,
        content: user_message,
      },
    ];
  };

  let mk_error =
      (ci: Info.t, sketch_z: Zipper.t, reply: string): option(string) => {
    /* TODO: This should maybe take whole JSON convo
     * so far and return an appended version */
    //TODO: Proper errors
    let init_ctx = Info.ctx_of(ci);
    ErrorPrint.mk(~init_ctx, sketch_z, reply);
  };
};
