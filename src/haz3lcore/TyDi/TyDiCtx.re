open TyDiSuggestion;
open Language;

/* For suggestions in patterns, suggest variables which
 * occur free in that pattern's scope. */
let free_variables =
    (expected_ty: Typ.t, ctx: Ctx.t, co_ctx: CoCtx.t): list(TyDiSuggestion.t) => {
  List.filter_map(
    ((name, entries)) =>
      switch (Ctx.lookup_var(ctx, name)) {
      | None =>
        let meet_use_typ = CoCtx.meet(ctx, entries);
        if (Typ.is_consistent(ctx, expected_ty, meet_use_typ)) {
          Some({
            content: name,
            strategy: Pat(FromCoCtx(meet_use_typ)),
          });
        } else {
          None;
        };
      | Some(_) => None
      },
    co_ctx,
  );
};

/* For suggestsions in expressions, suggest variables from the ctx */
let bound_variables = (ty_expect: Typ.t, ctx: Ctx.t): list(TyDiSuggestion.t) =>
  List.filter_map(
    fun
    | Ctx.VarEntry({typ, name, _})
        when Typ.is_consistent(ctx, ty_expect, typ) =>
      Some({
        content: name,
        strategy: Exp(Common(FromCtx(typ))),
      })
    | _ => None,
    ctx.entries,
  );

let bound_livelits = (ty_expect: Typ.t, ctx: Ctx.t): list(TyDiSuggestion.t) =>
  List.filter_map(
    fun
    | Ctx.LivelitEntry({expansion_t, name, _})
        when Typ.is_consistent(ctx, ty_expect, expansion_t) =>
      Some({
        content: "^" ++ name,
        strategy: Exp(Common(FromCtx(expansion_t))),
      })
    | _ => None,
    ctx.entries,
  );

let bound_constructors =
    (wrap: strategy_common => strategy, ty: Typ.t, ctx: Ctx.t)
    : list(TyDiSuggestion.t) =>
  /* get names of all constructor entries consistent with ty */
  List.filter_map(
    fun
    | Ctx.ConstructorEntry({typ, name, _})
        when Typ.is_consistent(ctx, ty, typ) =>
      Some({
        content: name,
        strategy: wrap(FromCtx(typ)),
      })
    | _ => None,
    ctx.entries,
  );

/* Suggest applying a function from the ctx which returns an appropriate type */
let bound_aps = (ty_expect: Typ.t, ctx: Ctx.t): list(TyDiSuggestion.t) =>
  List.filter_map(
    fun
    | Ctx.VarEntry({typ: {term: Arrow(_, ty_out), _} as ty_arr, name, _})
        when
          Typ.is_consistent(ctx, ty_expect, ty_out)
          && !Typ.is_consistent(ctx, ty_expect, ty_arr) => {
        Some({
          content: name ++ "(",
          strategy: Exp(Common(FromCtxAp(ty_out))),
        });
      }
    | _ => None,
    ctx.entries,
  );

let bound_constructor_aps =
    (wrap, ty: Typ.t, ctx: Ctx.t): list(TyDiSuggestion.t) =>
  List.filter_map(
    fun
    | Ctx.ConstructorEntry({
        typ: {term: Arrow(_, ty_out), _} as ty_arr,
        name,
        _,
      })
        when
          Typ.is_consistent(ctx, ty, ty_out)
          && !Typ.is_consistent(ctx, ty, ty_arr) =>
      Some({
        content: name ++ "(",
        strategy: wrap(FromCtxAp(ty_out)),
      })
    | _ => None,
    ctx.entries,
  );

/* The named fields of a labeled tuple type or the value members of a
 * signature type, with their types (a signature's own manifest type members
 * are substituted into its member types). */
let named_fields = (ctx: Ctx.t, typ: Typ.t): list((string, Typ.t)) =>
  switch (Typ.normalize(ctx, typ) |> Typ.term_of) {
  | Prod(ts) => List.filter_map(Typ.match_tup_label, ts)
  | Sig(items) =>
    /* Close every member in one pass, not one sig_project_value per
     * member: each of those re-substitutes the type members from scratch,
     * and for Html that is its whole HTML type substituted ~50 times, over
     * a second a keystroke. Same substitution, same defaults, and the last
     * declaration of each value wins, as in sig_project_value. */
    let closed = Typ.sig_members_closed(items);
    let closed_value = label =>
      List.fold_left(
        (found, (m: Sig.member, ty)) =>
          switch (m) {
          | Val(x, _) when x == label => Some(ty)
          | _ => found
          },
        None,
        closed,
      );
    Sig.members(items)
    |> Sig.dedup_last
    |> List.filter_map((m: Sig.member) =>
         switch (m) {
         | Val(label, _) =>
           closed_value(label) |> Option.map(field_ty => (label, field_ty))
         | TypeManifest(_)
         | TypeAbstract(_) => None
         }
       );
  | _ => []
  };

/* named_fields across keystrokes. While the typed token is a prefix of a
 * builtin module, completion needs that module's fields on every keystroke,
 * and for Html they cost ~0.5 s: normalizing its signature and closing its
 * members, the same work each time.
 *
 * named_fields reads its ctx only through Ctx.lookup_tvar, lookup_var and
 * lookup_alias (normalize, weak_head_normalize, path_sig; the ctxs they
 * extend put local entries in front of the one passed in). So its answer is
 * a function of the type and of those lookups' answers, in the ctx passed
 * in, for the names it asked about. A cached answer records those names
 * and what each resolved to, and is reused only if every one resolves to
 * the same entry again. Shadowing any of them -- `module Attr = ...`,
 * `type HTML = ...` -- changes an answer and recomputes. Entries are
 * compared by identity: the builtin ones are one list, shared by every
 * keystroke. */
type resolution = {
  name: string,
  tvar: option(Ctx.kind),
  var: option(Ctx.var_entry),
};

let resolve = (ctx: Ctx.t, name: string): resolution => {
  name,
  tvar: Ctx.lookup_tvar(ctx, name),
  var: Ctx.lookup_var(ctx, name),
};

let same_opt = (a, b) =>
  switch (a, b) {
  | (None, None) => true
  | (Some(x), Some(y)) => x === y
  | _ => false
  };

let still_resolves = (ctx: Ctx.t, r: resolution): bool => {
  let now = resolve(ctx, r.name);
  same_opt(now.tvar, r.tvar) && same_opt(now.var, r.var);
};

type cached_fields = {
  typ: Typ.t,
  deps: list(resolution),
  fields: list((string, Typ.t)),
};

/* Most recent first; a handful of module-typed entries is all a keystroke
 * sees. */
let fields_cache: ref(list(cached_fields)) = ref([]);
let fields_cache_size = 16;

/* For tests: how many calls reused a cached answer. */
let fields_cache_hits = ref(0);

let named_fields_cached = (ctx: Ctx.t, typ: Typ.t): list((string, Typ.t)) =>
  switch (
    List.find_opt(
      c => c.typ === typ && List.for_all(still_resolves(ctx), c.deps),
      fields_cache^,
    )
  ) {
  | Some(c) =>
    incr(fields_cache_hits);
    c.fields;
  | None =>
    let (fields, names) =
      Ctx.with_lookup_trace(() => named_fields(ctx, typ));
    let deps =
      names |> List.sort_uniq(String.compare) |> List.map(resolve(ctx));
    let others = List.filter(c => c.typ !== typ, fields_cache^);
    fields_cache :=
      [
        {
          typ,
          deps,
          fields,
        },
        ...others,
      ]
      |> List.filteri((i, _) => i < fields_cache_size);
    fields;
  };

/* named_fields, remembered for one TyDi.suggest call. Every caller in that
 * call derives its ctx the same way from the same Info.t, so an entry's
 * fields are the same each time they are asked for; without this,
 * bound_qualified and bound_qualified_aps compute them twice per keystroke,
 * and the Bool lookahead more. Keyed on the entry's type by physical
 * identity: the builtin entries are the same value throughout. */
let fields_memo = () => {
  let seen = ref([]);
  (ctx: Ctx.t, typ: Typ.t) =>
    switch (List.assq_opt(typ, seen^)) {
    | Some(fields) => fields
    | None =>
      let fields = named_fields_cached(ctx, typ);
      seen := [(typ, fields), ...seen^];
      fields;
    };
};

/* Suggest qualified member access: for variables with labeled tuple or
 * module types, suggest Name.label for fields consistent with the expected
 * type. E.g., if String has type { let empty : String; let length : String
 * -> Int } and we expect String, suggest "String.empty".
 *
 * TODO: Only goes one level deep. Nested qualified access (A.B.x) would
 * require recursive expansion. See also: List(Prod) types could generate
 * qualified suggestions where field types are wrapped in List(...). */

/* Whether `name.` can start a suggestion the caller will keep. The only
 * caller, TyDi.set_buffer, keeps a suggestion only if it starts with the
 * token left of the caret; a qualified one is `name.label...`, so that
 * needs the token and `name.` to be prefixes one of the other. Checked
 * before named_fields, which normalizes the entry's type -- for a module,
 * its whole signature -- and is most of a keystroke's cost when every
 * builtin module is in scope. No prefix: keep everything. */
let could_qualify = (~prefix: option(string), name: string): bool =>
  switch (prefix) {
  | None => true
  | Some(tok) =>
    let head = name ++ ".";
    String.starts_with(~prefix=tok, head)
    || String.starts_with(~prefix=head, tok);
  };

let bound_qualified =
    (~prefix=?, ~fields=named_fields, ty_expect: Typ.t, ctx: Ctx.t)
    : list(TyDiSuggestion.t) =>
  List.concat_map(
    fun
    | Ctx.VarEntry({typ, name, _}) when could_qualify(~prefix, name) =>
      fields(ctx, typ)
      |> List.filter_map(((label, field_ty)) =>
           Typ.is_consistent(ctx, ty_expect, field_ty)
             ? Some(
                 TyDiSuggestion.{
                   content: name ++ "." ++ label,
                   strategy: Exp(Common(FromCtx(field_ty))),
                 },
               )
             : None
         )
    | _ => [],
    ctx.entries,
  );

/* Like bound_qualified but for arrow-typed fields: suggest Name.label(
 * when the field's return type is consistent with the expected type.
 * E.g., if String has (length=String->Int) and we expect Int,
 * suggest "String.length(". */
let bound_qualified_aps =
    (~prefix=?, ~fields=named_fields, ty_expect: Typ.t, ctx: Ctx.t)
    : list(TyDiSuggestion.t) =>
  List.concat_map(
    fun
    | Ctx.VarEntry({typ, name, _}) when could_qualify(~prefix, name) =>
      fields(ctx, typ)
      |> List.filter_map(((label, field_ty: Typ.t)) =>
           switch (field_ty.term) {
           | Arrow(_, ty_out)
               when
                 Typ.is_consistent(ctx, ty_expect, ty_out)
                 && !Typ.is_consistent(ctx, ty_expect, field_ty) =>
             Some(
               TyDiSuggestion.{
                 content: name ++ "." ++ label ++ "(",
                 strategy: Exp(Common(FromCtxAp(ty_out))),
               },
             )
           | _ => None
           }
         )
    | _ => [],
    ctx.entries,
  );

/* Suggest bound type aliases in type annotations or definitions */
let typ_context_entries = (ctx: Ctx.t): list(TyDiSuggestion.t) =>
  List.filter_map(
    fun
    | Ctx.TVarEntry({kind: Singleton(_), name, _}) =>
      Some({
        content: name,
        strategy: Typ(FromCtx),
      })
    | _ => None,
    ctx.entries,
  );

/* NOTE(perf): suggest_variable and suggest_lookahead_variable each iterate
 * over ctx.entries multiple times (currently ~7 passes in suggest_variable,
 * up to ~33 in lookahead worst case for Bool). At typical context sizes
 * (<500 entries) this is negligible. If it becomes a bottleneck, the main
 * optimization is a single-pass refactor that classifies entries into
 * buckets in one traversal, and/or pre-caching results for the fixed
 * builtin context. */
let suggest_variable =
    (~prefix=?, ~fields=?, ci: Info.t): list(TyDiSuggestion.t) => {
  let ctx = Info.ctx_of(ci);
  let ctx = Ctx.filter_shadowed(ctx); /* Remove shadowing */
  switch (ci) {
  | InfoExp({ana, _}) =>
    bound_variables(ana, ctx)
    @ bound_livelits(ana, ctx)
    @ bound_aps(ana, ctx)
    @ bound_qualified(~prefix?, ~fields?, ana, ctx)
    @ bound_qualified_aps(~prefix?, ~fields?, ana, ctx)
    @ bound_constructors(x => Exp(Common(x)), ana, ctx)
    @ bound_constructor_aps(x => Exp(Common(x)), ana, ctx)
  | InfoPat({ana, co_ctx, _}) =>
    free_variables(ana, ctx, co_ctx)
    @ bound_constructors(x => Pat(Common(x)), ana, ctx)
    @ bound_constructor_aps(x => Pat(Common(x)), ana, ctx)
  | InfoTyp(_) => typ_context_entries(ctx)
  | _ => []
  };
};

/* Suggest lookahead tokens:
 *
 * Sometimes the expected type is Ty, but we want to enter something of Ty'
 * because we're going to follow it up with an infix op of type (Ty', _) -> Ty.
 *
 * For now we special-case such situations instead of deriving them from the
 * grammar. In the current grammar there are basically 3 classes:
 *
 * 1. If bool is expected, could be int, float or string (comparisons)
 * 2. If list(ty) is expected, could be ty (cons)
 * 3. If tuple([ty, ...]) is expected, could be ty (comma)

 * 2 and 3 are the easiest to make ergonomic as there is only one such
 * infix op, so we can just combine the two tokens into a single completion.
 * 1 is slightly more fraught because as we either need to not show the
 * second token, or pick an arbitrary representative op to show, and we
 * probably wouldn't want to complete that op, forcing the user to backspace
 * if they meant another, so we'd need to implement staged completion.
 * For now we just don't show a second token, which can be slightly confusing.
 *
 */

let suggest_lookahead_variable =
    (~prefix=?, ~fields=?, ci: Info.t): list(TyDiSuggestion.t) => {
  let restrategize = (suffix, {content, strategy}) => {
    content: content ++ suffix,
    strategy,
  };
  let ctx = Info.ctx_of(ci);
  let ctx = Ctx.filter_shadowed(ctx); /* Remove shadowing */
  switch (ci) {
  | InfoExp({ana, _}) =>
    let exp_refs = ty =>
      bound_variables(ty, ctx)
      @ bound_qualified(~prefix?, ~fields?, ty, ctx)
      @ bound_constructors(x => Exp(Common(x)), ty, ctx);
    let exp_aps = ty =>
      bound_aps(ty, ctx)
      @ bound_qualified_aps(~prefix?, ~fields?, ty, ctx)
      @ bound_constructor_aps(x => Exp(Common(x)), ty, ctx);
    switch (ana |> Typ.term_of) {
    | List(ty) =>
      List.map(restrategize(" )::"), exp_aps(ty))
      @ List.map(restrategize("::"), exp_refs(ty))
    | Prod([ty, ...tys]) =>
      let commas =
        List.init(List.length(tys), _ => ",") |> String.concat(" ");
      List.map(restrategize(" )" ++ commas), exp_aps(ty))
      @ List.map(restrategize(commas), exp_refs(ty));
    | Atom(Bool) =>
      /* TODO: Find a UI to make these less confusing */
      exp_refs(Atom(Int) |> Typ.fresh)
      @ exp_refs(Atom(SInt) |> Typ.fresh)
      @ exp_refs(Atom(Nat) |> Typ.fresh)
      @ exp_refs(Atom(Float) |> Typ.fresh)
      @ exp_refs(Atom(String) |> Typ.fresh)
      @ exp_aps(Atom(Int) |> Typ.fresh)
      @ exp_aps(Atom(Float) |> Typ.fresh)
      @ exp_aps(Atom(String) |> Typ.fresh)
    | _ => []
    };
  | InfoPat({ana, co_ctx, _}) =>
    let pat_refs = ty =>
      free_variables(ty, ctx, co_ctx)
      @ bound_constructors(x => Pat(Common(x)), ty, ctx);
    let pat_aps = ty => bound_constructor_aps(x => Pat(Common(x)), ty, ctx);
    switch (ana |> Typ.term_of) {
    | List(ty) =>
      List.map(restrategize(" )::"), pat_aps(ty))
      @ List.map(restrategize("::"), pat_refs(ty))
    | Prod([ty, ...tys]) =>
      let commas =
        List.init(List.length(tys), _ => ",") |> String.concat(" ");
      List.map(restrategize(" )" ++ commas), pat_aps(ty))
      @ List.map(restrategize(commas), pat_refs(ty));
    | _ => []
    };
  | InfoTyp(_) => []
  | _ => []
  };
};
