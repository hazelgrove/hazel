open Util;
open OptUtil.Syntax;
open Sexplib.Std;

/* TyDi: Type-Directed Next-Token Suggestions

    IDEA: Expanded criteria for when to autoshow: Currently, we show only
    when there is at least one suggestion which prefix-matches but is not
    identical to the current nonzero prefix. We might consider relaxing
    the nonzero prefix part. We probably don't want to autoshow on correct
    tokens, but we could autoshow on errors if there are fixes, or on
    empties if there's only one option.

     IDEA: Add a keybinding to force reveal suggestion if not current shown.
     I've stubbed this out (Cmd+?) but needs an option to show suggestions
     even if on hole (ie prefix for completion is "")

     IDEA: If there are ~ no current suggestions, and the indicated term
     has a type error suggest following infixes which fix that type error,
     e.g. given "let a:Float = fst(1.0|" suggest comma
     e.g. given "let b:Bool = 1|" suggest <, >, <=, >=, ==, !=, etc.

     IDEA: UNBIDIRECTIONAL POSITIONS:
    1. In ap funpos: favor input ty consistent with arg
    2. In case scrut, favor the tys of extant patterns
    3. In list element, favor the tys of extant elements
    3. In pattern annotation type: favor patann expected type

    IDEA: If on infix op, suggest based on either operand type,
    especially the case where it would fix an operand type error

    IDEA: If on 2-multihole, suggest infix ops as above or Ap if applicable

   */

[@deriving (show({with_path: false}), sexp, yojson)]
type strategy_all =
  | FromBackpack;

[@deriving (show({with_path: false}), sexp, yojson)]
type strategy_common =
  | NewForm(Typ.t)
  | FromCtx(Typ.t)
  | FromCtxAp(Typ.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type strategy_exp =
  | Common(strategy_common);

[@deriving (show({with_path: false}), sexp, yojson)]
type strategy_pat =
  | Common(strategy_common)
  | FromCoCtx(Typ.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type strategy_typ =
  | NewForm
  | FromCtx;

[@deriving (show({with_path: false}), sexp, yojson)]
type strategy =
  | Any(strategy_all)
  | Exp(strategy_exp)
  | Pat(strategy_pat)
  | Typ(strategy_typ);

[@deriving (show({with_path: false}), sexp, yojson)]
type suggestion = {
  content: string,
  strategy,
};

let unk: Typ.t = Unknown(Internal);

let expected_ty: Info.t => Typ.t =
  fun
  | InfoExp({mode, _})
  | InfoPat({mode, _}) => Mode.ty_of(mode)
  | _ => unk;

let const_mono_delim_tys: list((Token.t, Typ.t)) = [
  ("true", Bool),
  ("false", Bool),
  //("[]", List(unk)), / *NOTE: would need to refactor buffer for this to show up */
  //("()", Prod([])), /* NOTE: would need to refactor buffer for this to show up */
  ("\"\"", String), /* NOTE: Irrelevent as second quote appears automatically */
  ("_", unk),
];

let leading_delim_tys: list((Token.t, Typ.t)) = [
  ("case ", unk),
  ("fun ", Arrow(unk, unk)),
  ("if ", unk),
  ("let ", unk),
  ("test ", Prod([])),
  ("type ", unk),
];

let infix_delim_tys: list((Token.t, Typ.t)) = [
  (",", Prod([unk, unk])), /* NOTE: Current approach doesn't work for this, but irrelevant as 1-char */
  ("::", List(unk)),
  ("@", List(unk)),
  (";", unk),
  ("&&", Bool),
  ("\\/", Bool),
  ("$==", Bool),
  ("==.", Bool),
  ("==", Bool),
  ("!=", Bool),
  ("!=.", Bool),
  ("<", Bool),
  (">", Bool),
  ("<=", Bool),
  (">=", Bool),
  ("<.", Bool),
  (">.", Bool),
  ("<=.", Bool),
  (">=.", Bool),
  ("+", Int),
  ("-", Int),
  ("*", Int),
  ("/", Int),
  ("**", Int),
  ("+.", Float),
  ("-.", Float),
  ("*.", Float),
  ("/.", Float),
  ("**.", Float),
  ("++", String),
];

let filter_by_type =
    (
      ctx: Ctx.t,
      expected_ty: Typ.t,
      self_tys: list((Token.t, Typ.t)),
      delims: list(string),
    )
    : list((Token.t, Typ.t)) =>
  List.filter_map(
    delim => {
      let* self_ty = List.assoc_opt(delim, self_tys);
      Typ.is_consistent(ctx, expected_ty, self_ty)
        ? Some((delim, self_ty)) : None;
    },
    delims,
  );

let suggest_form = (ty_map, delims_of_sort, ci: Info.t): list(suggestion) => {
  let sort = Info.sort_of(ci);
  let delims = delims_of_sort(sort);
  let filtered =
    filter_by_type(Info.ctx_of(ci), expected_ty(ci), ty_map, delims);
  switch (sort) {
  | Exp =>
    List.map(
      ((content, ty)) => {content, strategy: Exp(Common(NewForm(ty)))},
      filtered,
    )
  | Pat =>
    List.map(
      ((content, ty)) => {content, strategy: Pat(Common(NewForm(ty)))},
      filtered,
    )
  | _ => delims |> List.map(content => {content, strategy: Typ(NewForm)})
  };
};

let suggest_operator: Info.t => list(suggestion) =
  suggest_form(infix_delim_tys, Molds.infix_delims);

let suggest_operand: Info.t => list(suggestion) =
  suggest_form(const_mono_delim_tys, Molds.const_mono_delims);

let suggest_leading: Info.t => list(suggestion) =
  suggest_form(leading_delim_tys, Molds.delayed_leading_delims);

let suggest_backpack = (z: Zipper.t): list(suggestion) => {
  /* Note: Sort check unnecessary as wouldn't be able to put down */
  switch (z.backpack) {
  | [] => []
  | [{content, _}, ..._] =>
    switch (content) {
    | [Tile({label, shards: [idx], _})] when Zipper.can_put_down(z) => [
        {content: List.nth(label, idx), strategy: Any(FromBackpack)},
      ]
    | _ => []
    }
  };
};

let free_variables =
    (expected_ty: Typ.t, ctx: Ctx.t, co_ctx: CoCtx.t): list(suggestion) => {
  List.filter_map(
    ((name, entries)) =>
      switch (Ctx.lookup_var(ctx, name)) {
      | None =>
        let joint_use_typ = CoCtx.join(ctx, entries);
        if (Typ.is_consistent(ctx, expected_ty, joint_use_typ)) {
          Some({content: name, strategy: Pat(FromCoCtx(joint_use_typ))});
        } else {
          None;
        };
      | Some(_) => None
      },
    co_ctx,
  );
};

let bound_variables = (ty_expect: Typ.t, ctx: Ctx.t): list(suggestion) =>
  /* get names of all var entries consistent with ty */
  List.filter_map(
    fun
    | Ctx.VarEntry({typ, name, _})
        when Typ.is_consistent(ctx, ty_expect, typ) =>
      Some({content: name, strategy: Exp(Common(FromCtx(typ)))})
    | _ => None,
    ctx,
  );

let bound_constructors =
    (wrap: strategy_common => strategy, ty: Typ.t, ctx: Ctx.t)
    : list(suggestion) =>
  /* get names of all constructor entries consistent with ty */
  List.filter_map(
    fun
    | Ctx.ConstructorEntry({typ, name, _})
        when Typ.is_consistent(ctx, ty, typ) =>
      Some({content: name, strategy: wrap(FromCtx(typ))})
    | _ => None,
    ctx,
  );

let bound_aps = (ty_expect: Typ.t, ctx: Ctx.t): list(suggestion) =>
  /* get names of all var entries consistent with ty */
  List.filter_map(
    fun
    | Ctx.VarEntry({typ: Arrow(_, ty_out) as ty_arr, name, _})
        when
          Typ.is_consistent(ctx, ty_expect, ty_out)
          && !Typ.is_consistent(ctx, ty_expect, ty_arr) => {
        Some
          ({
            content: name ++ "(",
            strategy: Exp(Common(FromCtxAp(ty_out))),
          }); // TODO(andrew): this is a hack
      }
    | _ => None,
    ctx,
  );

let bound_constructor_aps = (wrap, ty: Typ.t, ctx: Ctx.t): list(suggestion) =>
  /* get names of all constructor entries consistent with ty */
  List.filter_map(
    fun
    | Ctx.ConstructorEntry({typ: Arrow(_, ty_out) as ty_arr, name, _})
        when
          Typ.is_consistent(ctx, ty, ty_out)
          && !Typ.is_consistent(ctx, ty, ty_arr) =>
      Some({content: name ++ "(", strategy: wrap(FromCtxAp(ty_out))}) // TODO(andrew): this is a hack
    | _ => None,
    ctx,
  );

let typ_context_entries = (ctx: Ctx.t): list(suggestion) =>
  /* get names of all type aliases */
  List.filter_map(
    fun
    | Ctx.TVarEntry({kind: Singleton(_), name, _}) =>
      Some({content: name, strategy: Typ(FromCtx)})
    | _ => None,
    ctx,
  );

let suggest_variable = (ci: Info.t): list(suggestion) => {
  let ctx = Info.ctx_of(ci);
  switch (ci) {
  | InfoExp({mode, _}) =>
    bound_variables(Mode.ty_of(mode), ctx)
    @ bound_aps(Mode.ty_of(mode), ctx)
    @ bound_constructors(x => Exp(Common(x)), Mode.ty_of(mode), ctx)
    @ bound_constructor_aps(x => Exp(Common(x)), Mode.ty_of(mode), ctx)
  | InfoPat({mode, co_ctx, _}) =>
    free_variables(Mode.ty_of(mode), ctx, co_ctx)
    @ bound_constructors(x => Pat(Common(x)), Mode.ty_of(mode), ctx)
    @ bound_constructor_aps(x => Pat(Common(x)), Mode.ty_of(mode), ctx)
  | InfoTyp(_) => typ_context_entries(ctx)
  | _ => []
  };
};

/**
   Double-token plan:

   Sometimes the expected type is Ty, but we want to enter something of Ty'
   because we're going to follow it up with an infix op of type (Ty', _) -> Ty.

   For now let's consider special-casing such situations as opposed to deriving
   them from the grammar. In the current grammar there are basically 3 classes:

   1. If bool is expected, could be int, float or string (comparisons)
   2. If list(ty) is expected, could be ty (cons)
   3. If tuple([ty, ...]) is expected, could be ty (comma)

   2 and 3 probably most useful because they are more specific (as long as ty is),
   and more peritnently, there is only one such infix op; in this case we can
   complete them together. 1 is more fraught because we'd need to pick a
   representative op to show, and we probably wouldn't want to complete that
   op, forcing the user to backspace if they meant another, so we'd need
   to implement staged completion.

   So for now let's try:
   2.1. if expected is list(ty!=unk), suggest "<ty>::"
   3.1. if expected is tuple([ty!=unk, _...]), suggest "<ty1>,"

   And also maybe:
   2.2. if expected is list(ty!=unk), suggest "<_=>ty>( )::"
   3.2. if expected is tuple([ty!=unk, n...]), suggest "<ty1>, , ," (length n-1 commas)

   Note that the latter two aren't going to be perfect in the current system,
   as they will leave holes to the left of the caret. This can be fixed,
   but requires fancier buffer insertion logic similar to staged completion.

    */

let suggest_lookahead_variable = (ci: Info.t): list(suggestion) => {
  let fix_ahead = (suffix, {content, strategy}) => {
    content: content ++ suffix,
    strategy,
  };
  let ctx = Info.ctx_of(ci);
  switch (ci) {
  | InfoExp({mode, _}) =>
    let exp_refs = ty =>
      bound_variables(ty, ctx)
      @ bound_constructors(x => Exp(Common(x)), ty, ctx);
    let exp_aps = ty =>
      bound_aps(ty, ctx)
      @ bound_constructor_aps(x => Exp(Common(x)), ty, ctx);
    switch (Mode.ty_of(mode)) {
    | List(ty) =>
      List.map(fix_ahead(" )::"), exp_aps(ty))
      @ List.map(fix_ahead("::"), exp_refs(ty))
    | Prod([ty, _]) =>
      List.map(fix_ahead(" ),"), exp_aps(ty))
      @ List.map(fix_ahead(","), exp_refs(ty))
    | Prod([ty, ...rest]) =>
      let commas =
        List.init(List.length(rest), _ => ",") |> String.concat(" ");
      List.map(fix_ahead(" )" ++ commas), exp_aps(ty))
      @ List.map(fix_ahead("" ++ commas), exp_refs(ty));
    | _ => []
    };
  | InfoPat({mode, co_ctx, _}) =>
    let pat_refs = ty =>
      free_variables(ty, ctx, co_ctx)
      @ bound_constructors(x => Pat(Common(x)), ty, ctx);
    let pat_aps = ty => bound_constructor_aps(x => Pat(Common(x)), ty, ctx);
    switch (Mode.ty_of(mode)) {
    | List(ty) =>
      List.map(fix_ahead(" )::"), pat_aps(ty))
      @ List.map(fix_ahead("::"), pat_refs(ty))
    | Prod([ty, _]) =>
      List.map(fix_ahead(" ),"), pat_aps(ty))
      @ List.map(fix_ahead(","), pat_refs(ty))
    | Prod([ty, ...rest]) =>
      let commas =
        List.init(List.length(rest), _ => ",") |> String.concat(" ");
      List.map(fix_ahead(" )" ++ commas), pat_aps(ty))
      @ List.map(fix_ahead("" ++ commas), pat_refs(ty));
    | _ => []
    };
  | InfoTyp(_) => []
  | _ => []
  };
};

let compare_suggestions = (s1: suggestion, s2: suggestion): int => {
  String.compare(s1.content, s2.content);
};

let suggest = (ci: Info.t, z: Zipper.t): list(suggestion) => {
  suggest_backpack(z)
  /* NOTE: Sorting here ensures that if we have an exact match already,
     we won't suggest extending it, but sorting may not be desirable in
     other ways, for example maybe we want recency bias in ctx?
     Possibly revisit this.

     I'm sorting here as opposed to after combination because I always
     want backpack candidates to show up first  */
  @ (
    suggest_operand(ci)
    @ suggest_leading(ci)
    @ suggest_variable(ci)
    @ suggest_lookahead_variable(ci)
    |> List.sort(compare_suggestions)
  )
  @ (suggest_operator(ci) |> List.sort(compare_suggestions));
};

let left_of_mono = (z: Zipper.t): option(string) =>
  switch (
    z.relatives.siblings |> fst |> List.rev,
    z.relatives.siblings |> snd,
  ) {
  | ([Tile({label: [tok_to_left], _}), ..._], _) => Some(tok_to_left)
  //below is an attempt to support [],(); not sure why it doesnt work (causes exception blah)
  /* this doesnt work because fit_of actually needs more info to determine nib shape
     if this isnt a monotile; we would need to return the label here as well to figure
     out the fit */
  | ([Tile({label: [tok_to_left, ..._], shards: [0], _}), ..._], _) =>
    Some(tok_to_left)
  | _ => None
  };

let mk_amorphous_tile = (~sort: Sort.t, sibs: Siblings.t, t: Token.t): Tile.t => {
  let mold = Siblings.mold_fitting_between(sort, Precedence.max, sibs);
  {id: Id.mk(), label: [t], shards: [0], children: [], mold};
};

let get_amorphous_buffer_text = (z: Zipper.t): option(Token.t) =>
  switch (z.selection.mode, z.selection.content) {
  | (Buffer(Amorphous), [Tile({label: [completion], _})]) =>
    Some(completion)
  | _ => None
  };

let suffix_of = (candidate: Token.t, left: Token.t): option(Token.t) => {
  let candidate_suffix =
    String.sub(
      candidate,
      String.length(left),
      String.length(candidate) - String.length(left),
    );
  candidate_suffix == "" ? None : Some(candidate_suffix);
};

//TODO(andrew): PERF DANGER
let z_to_ci = (~settings: CoreSettings.t, ~ctx: Ctx.t, z: Zipper.t) => {
  let map =
    z
    |> MakeTerm.from_zip_for_sem
    |> fst
    |> Interface.Statics.mk_map_ctx(settings, ctx);
  let* index = Indicated.index(z);
  Id.Map.find_opt(index, map);
};

let set_buffer = (~settings, ~ctx: Ctx.t, z: Zipper.t): option(Zipper.t) => {
  let* tok_to_left = left_of_mono(z);
  let* ci = z_to_ci(~settings, ~ctx, z);
  let suggestions = suggest(ci, z);
  let suggestions =
    suggestions
    |> List.filter(({content, _}: suggestion) =>
         String.starts_with(~prefix=tok_to_left, content)
       );
  let* top_suggestion = suggestions |> Util.ListUtil.hd_opt;
  let* suggestion_suffix = suffix_of(top_suggestion.content, tok_to_left);
  let tile =
    mk_amorphous_tile(
      ~sort=Info.sort_of(ci),
      z.relatives.siblings,
      suggestion_suffix,
    );
  let z = Zipper.set_buffer(z, ~content=[Tile(tile)], ~mode=Amorphous);
  Some(z);
};
