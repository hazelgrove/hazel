/* STATICS.re

      This module determines the statics semantics of a program. This
      includes the type information and the elaboration for expressions
      and patterns.

   */

open Util;
include StaticsBase;
let add_info = Map.add_info;
let add_missing_info = Map.add_missing_info;

let slice_scratch: Slice.scratch(Info.t) = {
  read:
    fun
    | Info.InfoSliceScratch(children) => Some(children)
    | _ => None,
  write: children => Info.InfoSliceScratch(children),
};

let record = (~id, role, component, m) =>
  Slice.record(~scratch=slice_scratch, ~id, role, component, m);
let take_children = (~id, m) => Slice.take(~scratch=slice_scratch, ~id, m);
let edge = (~at, role, slice_of, component, k) =>
  Slice.edge(~scratch=slice_scratch, ~at, role, slice_of, component, k);
let edge_typ = (~at, role, slice_of, component, k) =>
  Slice.edge_typ(~scratch=slice_scratch, ~at, role, slice_of, component, k);

/* Compute a type's kind without descending into descendants. The
   recursive `utyp_to_info_map` traversal puts kind marks on each node
   individually, so this helper is purely for "what kind does the
   surface produce here?" callsites (alias-body kind, callee kind for
   `TypParamAp`, expected-Type checks).

   `Var(name)` falls back to `Unknown` (not `Type`) when the name is
   neither a base type nor in the context, so a free `L` in
   `Cons(a, L(a))` produces only the free-variable mark on `L` and
   doesn't pile a "cannot apply" mark on the surrounding application.

   Top-level (rather than nested inside `utyp_to_info_map`) so
   `TyAlias` can use it to compute the kind threaded through
   `extend_alias` and the cursor-inspector entry on the alias name. */
let rec kind_of_typ = (ctx: Ctx.t, ty: Typ.t): TypKind.t => {
  let type_ = TypKind.Type;
  switch (ty.term) {
  | Unknown(_) => TypKind.Unknown
  | Atom(_)
  | DrvQuoteTy(_)
  | Label(_)
  | ExplicitNonlabel => type_
  | Var(name) =>
    switch (Ctx.lookup_tvar_typ_kind(ctx, name)) {
    | Some(kind) => kind
    | None when Ctx.is_base_typ(name) => TypKind.Type
    | None => TypKind.Unknown
    }
  | Parens(t)
  | Projector(_, t) => kind_of_typ(ctx, t)
  | ProdProjection(_) =>
    /* `M.T`: resolve through the module's labeled-tuple exports so
       a parameterized export reports its full `(Type, …) -> Type`
       kind. Same path `weak_head_normalize` follows. */
    let resolved = Typ.weak_head_normalize(ctx, ty);
    switch (resolved.term) {
    | ProdProjection(_) => type_
    | Unknown(_)
    | Atom(_)
    | DrvQuoteTy(_)
    | Label(_)
    | ExplicitNonlabel
    | Var(_)
    | Parens(_)
    | Projector(_)
    | List(_)
    | Arrow(_)
    | TupLabel(_)
    | ProdExtension(_)
    | Prod(_)
    | Sum(_)
    | Poly(_)
    | ProofOf(_)
    | Sig(_)
    | TypTuple(_)
    | TypFun(_)
    | TypParamAp(_)
    | Rec(_) => kind_of_typ(ctx, resolved)
    };
  | List(_)
  | Arrow(_)
  | TupLabel(_)
  | ProdExtension(_)
  | Prod(_)
  | Sum(_)
  | Poly(_)
  | ProofOf(_)
  | Sig(_) => type_
  | TypTuple(_) =>
    /* No kind on its own — only ever the second arg of `TypParamAp`,
       checked there. `Type` is a neutral fallback so an isolated
       `TypTuple` doesn't poison surrounding kind checks. */
    type_
  | TypFun(param, body) =>
    let n = List.length(TPat.binders_of(param));
    let body_ctx = Ctx.extend_dummy_tvar(ctx, param);
    TypKind.arrows(List.init(n, _ => type_), kind_of_typ(body_ctx, body));
  | TypParamAp(fn, arg) =>
    let fn_kind = kind_of_typ(ctx, fn);
    let arg_kinds =
      switch (arg.term) {
      | TypTuple(ts) => List.map(kind_of_typ(ctx), ts)
      | Unknown(_)
      | Atom(_)
      | DrvQuoteTy(_)
      | Label(_)
      | ExplicitNonlabel
      | Var(_)
      | Parens(_)
      | Projector(_)
      | ProdProjection(_)
      | List(_)
      | Arrow(_)
      | TupLabel(_)
      | ProdExtension(_)
      | Prod(_)
      | Sum(_)
      | Poly(_)
      | ProofOf(_)
      | Sig(_)
      | TypFun(_)
      | TypParamAp(_)
      | Rec(_) => [kind_of_typ(ctx, arg)]
      };
    switch (TypKind.apply_all(fn_kind, arg_kinds)) {
    | Some(result) => result
    | None => type_
    };
  | Rec(param, body) =>
    let body_ctx = Ctx.extend_dummy_tvar(ctx, param);
    kind_of_typ(body_ctx, body);
  };
};

let rec any_to_info_map =
        (~ctx: Ctx.t, ~ancestors, any: Any.t, m: Map.t)
        : (CoCtx.t, Any.t, Map.t) =>
  switch (any) {
  | Exp(e) =>
    let ({co_ctx, _}: Info.exp, elab, m) =
      uexp_to_info_map(~ctx, ~ancestors, e, m);
    (co_ctx, Exp(elab), m);
  | Pat(p) =>
    let (_, elab, m) =
      upat_to_info_map(
        ~is_synswitch=false,
        ~co_ctx=CoCtx.empty,
        ~ancestors,
        ~duplicate_bindings=[],
        ~ctx,
        p,
        m,
      );
    (CoCtx.empty, Pat(elab), m);
  | TPat(tp) =>
    let m = utpat_to_info_map(~ctx, ~ancestors, tp, m) |> snd;
    (CoCtx.empty, TPat(tp), m);
  | Typ(ty) =>
    let m = utyp_to_info_map(~ctx, ~ancestors, ty, m) |> snd;
    (CoCtx.empty, Typ(ty), m);
  | Drv(drv) =>
    let m = drv_to_info_map(drv, m, ~ctx, ~ancestors, ~sort=Jdmt);
    (CoCtx.empty, Drv(drv), m);
  | Rul(r) => rul_to_info_map(~ctx, ~ancestors, r, m)
  | Mod(m_term) => mod_to_info_map(~ctx, ~ancestors, m_term, m)
  | Sig(s_term) => sig_to_info_map(~ctx, ~ancestors, s_term, m)
  | MPat(mp_term) => mpat_to_info_map(~ctx, ~ancestors, mp_term, m)
  | Any () => (CoCtx.empty, Any(), m)
  }
and multi = (~ctx, ~ancestors, m, tms): (list(CoCtx.t), list(Any.t), Map.t) =>
  List.fold_left(
    ((co_ctxs, tms_elab, m), any) => {
      let (co_ctx, any_elab, m) = any_to_info_map(~ctx, ~ancestors, any, m);
      (co_ctxs @ [co_ctx], tms_elab @ [any_elab], m);
    },
    ([], [], m),
    tms,
  )
and drv_to_info_map =
    (drv: Drv.Any.t, m: Map.t, ~ctx, ~ancestors, ~sort: DrvSort.t): Map.t => {
  let rec go = (drv: Drv.Any.t, m, ~sort: DrvSort.t) => {
    let add = info => add_info(Drv.Any.ids(drv), InfoDrv(info));
    let info = DrvInfo.derived(drv, ~ancestors, ~sort);
    let add_quote = (x, m) =>
      switch (Ctx.lookup_var(ctx, x)) {
      | Some({typ: {term: DrvQuoteTy(s), _}, _}) when sort == s =>
        m |> add(info)
      | Some({typ: {term: Unknown(_), _}, _}) => m |> add(info)
      | Some({typ, _}) =>
        m
        |> add({
             ...info,
             status: InHole(VarNoJoin(sort, typ)),
           })
      | None =>
        m
        |> add({
             ...info,
             status: InHole(FreeVar),
           })
      };
    let add = add(info);
    switch (drv) {
    | Exp(exp) =>
      switch (exp.term) {
      | Hole(_) => m |> add
      | Var(_) => m |> add
      | Quote(x) => m |> add_quote(x)
      | Parens(e) => m |> go(Exp(e), ~sort) |> add
      | Val(e) => m |> go_exp(e) |> add
      | Eval(e1, e2) => m |> go_exp(e1) |> go_exp(e2) |> add
      | Entail(ctx, p) => m |> go_ctx(ctx) |> go_prop(p) |> add
      | Consistent(t1, t2) => m |> go_typ(t1) |> go_typ(t2) |> add
      | MatchedArrow(t1, t2)
      | MatchedProd(t1, t2)
      | MatchedSum(t1, t2) => m |> go_typ(t1) |> go_typ(t2) |> add
      | Ctx(es) => List.fold_left((m, e) => m |> go_prop(e), m, es) |> add
      | Cons(e1, e2) => m |> go_prop(e1) |> go_ctx(e2) |> add
      | Concat(e1, e2) => m |> go_ctx(e1) |> go_ctx(e2) |> add
      | And(p1, p2)
      | Or(p1, p2)
      | Impl(p1, p2) => m |> go_prop(p1) |> go_prop(p2) |> add
      | Truth
      | Falsity => m |> add
      | Type(t) => m |> go_typ(t) |> add
      | HasType(e, t)
      | Syn(e, t)
      | Ana(e, t) => m |> go_exp(e) |> go_typ(t) |> add
      | NumLit(_) => m |> add
      | Neg(e) => m |> go_exp(e) |> add
      | BinOp(_, e1, e2) => m |> go_exp(e1) |> go_exp(e2) |> add
      | True
      | False => m |> add
      | If(e1, e2, e3) =>
        m |> go_exp(e1) |> go_exp(e2) |> go_exp(e3) |> add
      | Let(p, e1, e2) => m |> go_pat(p) |> go_exp(e1) |> go_exp(e2) |> add
      | Fix(p, e)
      | Fun(p, e) => m |> go_pat(p) |> go_exp(e) |> add
      | Ap(e1, e2) => m |> go_exp(e1) |> go_exp(e2) |> add
      | Tuple(es) => List.fold_left((m, e) => m |> go_exp(e), m, es) |> add
      | Pair(e1, e2) => m |> go_exp(e1) |> go_exp(e2) |> add
      | Triv => m |> add
      | PrjL(e)
      | PrjR(e) => m |> go_exp(e) |> add
      | InjL(e)
      | InjR(e) => m |> go_exp(e) |> add
      | Roll(e) => m |> go_exp(e) |> add
      | Unroll(e) => m |> go_exp(e) |> add
      | ExpHole => m |> add
      | Case(e, x, e1, y, e2) =>
        m
        |> go_exp(e)
        |> go_pat(x)
        |> go_exp(e1)
        |> go_pat(y)
        |> go_exp(e2)
        |> add
      }
    | Pat(pat) =>
      switch (pat.term) {
      | Hole(_) => m |> add
      | Quote(x) => m |> add_quote(x)
      | Var(_) => m |> add
      | Parens(p) => m |> go_pat(p) |> add
      | Cast(p, t) => m |> go_pat(p) |> go_typ(t) |> add
      | Pair(p1, p2) => m |> go_pat(p1) |> go_pat(p2) |> add
      | InjL(p)
      | InjR(p) => m |> go_pat(p) |> add
      }
    | Typ(ty) =>
      switch (ty.term) {
      | Hole(_) => m |> add
      | Quote(x) => m |> add_quote(x)
      | Var(_) => m |> add
      | Parens(t) => m |> go_typ(t) |> add
      | Num => m |> add
      | Bool => m |> add
      | Arrow(t1, t2) => m |> go_typ(t1) |> go_typ(t2) |> add
      | Prod(t1, t2) => m |> go_typ(t1) |> go_typ(t2) |> add
      | Unit => m |> add
      | Sum(t1, t2) => m |> go_typ(t1) |> go_typ(t2) |> add
      | Rec(p, t) => m |> go_tpat(p) |> go_typ(t) |> add
      | TypHole => m |> add
      }
    | TPat(tp) =>
      switch (tp.term) {
      | Hole(_) => m |> add
      | Quote(x) => m |> add_quote(x)
      | Var(_) => m |> add
      }
    };
  }
  and go_ctx = ctx => go(Exp(ctx), ~sort=Ctx)
  and go_prop = prop => go(Exp(prop), ~sort=Prop)
  and go_exp = exp => go(Exp(exp), ~sort=Exp)
  and go_pat = pat => go(Pat(pat), ~sort=Pat)
  and go_typ = typ => go(Typ(typ), ~sort=Typ)
  and go_tpat = tpat => go(TPat(tpat), ~sort=TPat);
  go(drv, m, ~sort);
}
and uexp_to_info_map =
    (
      ~ctx: Ctx.t,
      ~ana=syn,
      ~is_in_filter=false,
      ~ancestors,
      uexp: Exp.t,
      m: Map.t,
    )
    : (Info.exp, Exp.t, Map.t) => {
  let ids = IdTagged.ids(uexp);
  let (term, rewrap) = Exp.unwrap(uexp);
  let add =
      (
        ~user_term=uexp,
        ~elab_term: Exp.t,
        ~elab_syn_ty: Typ.t,
        ~marks: list(Mark.t)=[],
        ~warnings: list(Warning.list_item)=[],
        ~ctx=ctx,
        ~ana=ana,
        ~ancestors=ancestors,
        ~co_ctx: CoCtx.t,
        ~message: option(Message.t)=?,
        ~label_inference: option(Info.label_inference(Info.exp))=None, // TODO[Matt]: combine with message
        ~inferred_label: option(string)=None,
        ~label_sort=false,
        ~dot_labels: list(string)=[],
        ~uses: list(Slice.use)=[],
        ~slice: option(Slice.t)=None,
        m: Map.t,
      )
      : (Info.exp, Exp.t, Map.t) => {
    let marks =
      switch (expectation_mismatch_mark(ctx, ana, elab_syn_ty)) {
      | None => marks
      | Some(m) when marks == [] => [m] // TODO: we should probably eventually add this on top of existing marks
      | Some(_) => marks
      };
    let message =
      OptUtil.get(
        () =>
          switch (ana) {
          | {term: Unknown(SynSwitch), _} => Message.Exp(Default)
          | _ =>
            Message.Exp(Common(syn_ana_ok_common(ctx, ana, elab_syn_ty)))
          },
        message,
      );
    let cls = Cls.Exp(Exp.cls_of_term(uexp.term));
    let ty = fixed_typ(ctx, ana, elab_syn_ty);
    let (children, m) = take_children(~id=Exp.rep_id(user_term), m);
    let slice =
      Slice.mk(
        ~ctx,
        ~id=Exp.rep_id(user_term),
        ~ids=Slice.exp_ids(user_term),
        ~shape=elab_syn_ty,
        ~components=children,
        ~uses,
        ~override=slice,
        (),
      );
    let info: Info.exp = {
      cls,
      elab_syn_ty,
      marks,
      ty,
      ana,
      message,
      warnings,
      ctx,
      co_ctx,
      ancestors,
      user_term,
      elab_term,
      label_inference,
      inferred_label,
      label_sort,
      dot_labels,
      slice,
    };
    (info, elab_term, add_info(IdTagged.ids(user_term), InfoExp(info), m));
  };
  let ancestors_inclusive = [Exp.rep_id(uexp)] @ ancestors;
  let ancestors = (); // Deliberately shadowed so there's no risk of using it by mistake
  let go =
      (
        ~ctx=ctx,
        ~ana=syn,
        ~is_in_filter=is_in_filter,
        ~ancestors=ancestors_inclusive,
        uexp: Exp.t,
        m: Map.t,
      )
      : (Info.exp, Exp.t, Map.t) => {
    uexp_to_info_map(~ctx, ~ana, ~is_in_filter, ~ancestors, uexp, m);
  };
  let here = Exp.rep_id(uexp);
  let exp_edge = role => edge(~at=here, role, (i: Info.exp) => i.slice);
  let typ_edge = role => edge_typ(~at=here, role, (i: Info.typ) => i.slice);
  // use when the sub-term's type becomes an argument of the type constructor
  // this rule applies: in `1 :: []` the head's `Int` is the argument of `[_]`
  let ( let* ) = (component, k) => exp_edge(Part, component, k);
  // use when the sub-term's type is this rule's whole type: `(e)`, `1; e`
  let (let^) = (component, k) => exp_edge(Through, component, k);
  // use for any sub-term that is only type checked: `f(x)`'s `x`
  let (let&) = (component, k) => exp_edge(Omit, component, k);
  // use when the sub-term is a definition the binders demand from:
  // `case e | ...`'s `e`
  let (let$) = (component, k) => exp_edge(Source, component, k);
  // use when the sub-term is one of several branches supplying the same type:
  // `if c then a else b`
  let (let+) = (component, k) => exp_edge(Alternative, component, k);
  // use when an annotation supplies this rule's whole type: `(e : Int)`'s `Int`
  let (let^^) = (component, k) => typ_edge(Through, component, k);
  // use when a pattern binds names without contributing to this rule's type:
  // `let (x, y) = d in b`'s pattern. Unused: the binder rules re-analyze their
  // pattern after the body, so they record it themselves once both are checked.
  // let pat_edge = role => edge(~at=here, role, (i: Info.pat) => i.slice);
  // let (let!) = (component, k) => pat_edge(Binder, component, k);
  // use when an annotation is one argument of this rule's type constructor.
  // let ( let** ) = (component, k) => typ_edge(Part, component, k);
  // use when an annotation is sliced backwards by this rule's binders.
  // let (let$$) = (component, k) => typ_edge(Source, component, k);
  let map_m_go = (m, anas, es) => {
    let (pairs, m) =
      map_m2(
        (ana, e, m) =>
          go(~ana, e, m) |> (((e, elab, m)) => ((e, elab), m)),
        anas,
        es,
        m,
      );
    (List.split(pairs), m);
  };
  let map_m_go_omit = (m, anas, es) => {
    let (pairs, m) =
      map_m2(
        (ana, e, m) => {
          let& (e, elab, m) = go(~ana, e, m);
          ((e, elab), m);
        },
        anas,
        es,
        m,
      );
    (List.split(pairs), m);
  };
  let go_pat = upat_to_info_map(~ctx, ~ancestors=ancestors_inclusive);
  let go_typ = utyp_to_info_map(~ctx, ~ancestors=ancestors_inclusive);
  /* Analyze an expression in label position. Adds info for the label
     directly (like TupLabel does for its children) and returns
     the label name if valid. Used by CustomStatics for builtin label args. */
  // This lifts an expression into a singleton labeled tuple by rewriting the syntax in the Statics Map.
  let autolabel_singleton_tuple = (uexp: Exp.t, inner_ty, l, m) => {
    let (term, rewrap) = Exp.unwrap(uexp);
    let original_expression = Exp.fresh(term);
    let (inner_info, _, m) =
      uexp_to_info_map(
        ~ctx,
        ~ana=inner_ty,
        ~is_in_filter,
        ~ancestors=ancestors_inclusive,
        original_expression,
        m,
      );
    /* SynSwitch pre-pass on uexp misses expectation errors; inner_ty analysis
       is on a fresh copy — mirror it onto source ids for cursor/tests. */
    let inner_for_source = {
      ...inner_info,
      user_term: uexp,
    };

    let elaborated_exp =
      rewrap(
        Tuple([
          TupLabel(Label(l) |> Exp.fresh, original_expression) |> Exp.fresh,
        ]),
      );

    let (info, _, m) =
      uexp_to_info_map(
        ~ctx,
        ~ana,
        ~ancestors=ancestors_inclusive,
        elaborated_exp,
        m,
      );
    let info = {
      ...info,
      message: inner_info.message,
      label_inference:
        Some(
          SingletonLabelInference({
            label: l,
            pre_labeled_info: inner_for_source,
          }),
        ),
    };

    let m = add_info(IdTagged.ids(elaborated_exp), InfoExp(info), m);
    let m = add_info(IdTagged.ids(uexp), InfoExp(inner_for_source), m);
    (info, elaborated_exp, m);
  };

  // HACK: we use the co-context to check for unused variables in surrounding
  // pattern bindings, but we don't want unused variable warnings to appear
  // when there are holes present in the binding scopes. so if we detect a
  // a hole in this expression, we add a "$hole" entry to the co-context
  // that gets bubbled up to the relevant bindings and is checked for in the
  // warning logic.
  let hole_co_ctx =
    CoCtx.singleton(
      "$hole",
      Exp.rep_id(uexp),
      Unknown(Internal) |> Typ.temp,
    );

  let implicit_poly_args = (binder: TPat.t): (Typ.t, list(Typ.t)) => {
    let binders = TPat.binders_of(binder);
    let args =
      List.init(List.length(binders), _ => Unknown(Internal) |> Typ.fresh);
    let arg =
      switch (args) {
      | [arg] => arg
      | _ => TypTuple(args) |> Typ.fresh
      };
    (arg, args);
  };

  let rec implicit_poly_instantiate =
          (ty: Typ.t, elab: Exp.t): (Typ.t, Exp.t) =>
    switch (MatchedTyp.poly_pair(ctx, ty)) {
    | Some((Some(binder), body)) =>
      let (arg, args) = implicit_poly_args(binder);
      let binders = TPat.binders_of(binder);
      let body = Typ.subst_many(args, binders, body);
      implicit_poly_instantiate(body, TypAp(elab, arg) |> Exp.fresh);
    | Some((None, _))
    | None => (ty, elab)
    };

  let rec expects_poly_callee = (e: Exp.t): bool =>
    switch (e.term) {
    | Var(v) =>
      Ctx.lookup_var(ctx, v)
      |> Option.map((entry: Ctx.var_entry) =>
           MatchedTyp.poly_pair(ctx, entry.typ) != None
         )
      |> Option.value(~default=false)
    | TypAbs(_) => true
    | Parens(e)
    | Projector(_, e) => expects_poly_callee(e)
    | _ => false
    };

  let constructor_has_poly_schema = name =>
    Ctx.lookup_ctr(ctx, name)
    |> Option.map((entry: Ctx.var_entry) =>
         MatchedTyp.poly_pair(ctx, entry.typ) != None
       )
    |> Option.value(~default=false);

  // This is the case where we aren't a singleton labeled tuple
  let default_case = () => {
    switch (term) {
    | Closure(env, e) =>
      // TODO: implement closure type checking properly - see how dynamic type assignment does it
      let (e, e_elab, m) = go(~ana, e, m);
      add(
        ~elab_term=Closure(env, e_elab) |> rewrap,
        ~elab_syn_ty=e.elab_syn_ty,
        ~marks=[],
        ~co_ctx=e.co_ctx,
        m,
      );
    | MultiHole([Exp(e1), Exp(e2)]) =>
      let (e1, e1_elab, m) = go(~ana=syn, e1, m);
      let (e2, e2_elab, m) = go(~ana=syn, e2, m);
      add(
        ~elab_term=Seq(e1_elab, e2_elab) |> rewrap,
        ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[IsMulti],
        ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
        m,
      );
    | MultiHole(tms) =>
      let (co_ctxs, tms_elab, m) =
        multi(~ctx, ~ancestors=ancestors_inclusive, m, tms);
      add(
        ~elab_term=MultiHole(tms_elab) |> rewrap,
        ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[IsMulti],
        ~co_ctx=CoCtx.union(co_ctxs),
        m,
      );
    | Asc(e, t2) =>
      let^^ (t, m) = go_typ(t2, ~expects=TypExpectation.TypeExpected, m);
      /* Desugar any Sig types in the annotation without full normalization */
      let t_ty = Typ.desugar_sig(ctx, t.user_term);
      let& (e, e_elab, m) = go(~ana=t_ty, ~ctx=t.ctx, e, m);
      let typ_refs =
        ModuleHelpers.collect_module_refs_in_typ(ctx, Typ.rep_id(t2), t2);
      add(
        ~elab_term=Asc(e_elab, Typ.normalize(ctx, t2)) |> rewrap,
        ~elab_syn_ty=t_ty,
        ~marks=[],
        ~co_ctx=CoCtx.union([e.co_ctx, typ_refs]),
        m,
      );
    | Invalid(token) =>
      add(
        ~elab_term=Invalid(token) |> rewrap,
        ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[BadToken(token)],
        ~co_ctx=hole_co_ctx,
        m,
      )
    | EmptyHole =>
      add(
        ~elab_term=EmptyHole |> rewrap,
        ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[],
        ~co_ctx=hole_co_ctx,
        m,
      )
    | Deferral(position) =>
      let (marks: list(Mark.t), message: option(Message.t)) =
        switch (position) {
        | InAp => ([], Some(Exp(AnaDeferralConsistent(ana))))
        | OutsideAp => ([IsDeferral(position)], None)
        };
      add(
        ~elab_term=Deferral(position) |> rewrap,
        ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks,
        ~message?,
        ~co_ctx=CoCtx.empty,
        m,
      );
    | Undefined =>
      add(
        ~elab_term=Undefined |> rewrap,
        ~elab_syn_ty=Unknown(Hole(EmptyHole)) |> Typ.temp,
        ~marks=[],
        ~co_ctx=CoCtx.empty,
        m,
      )
    | DrvQuote(term, sort) =>
      let m =
        drv_to_info_map(term, m, ~ctx, ~ancestors=ancestors_inclusive, ~sort);
      add(
        ~elab_term=DrvQuote(term, sort) |> rewrap,
        ~elab_syn_ty=DrvQuoteTy(sort) |> Typ.temp,
        ~marks=[],
        ~co_ctx=CoCtx.empty,
        m,
      );
    | Atom(c) =>
      // Replace literal if necessary due to `use` or ana
      let c =
        Operators.replace_literal(c, Typ.is_ana_atom(ana), ctx.use_mode);
      switch (c) {
      | L(c) =>
        let ty = Atom(Atom.cls_of_t(c)) |> Typ.temp;
        add(
          ~elab_term=Atom(c) |> rewrap,
          ~elab_syn_ty=ty,
          ~marks=[],
          ~co_ctx=CoCtx.empty,
          m,
        );
      | R(BadInt(str)) =>
        add(
          ~elab_term=Invalid(str) |> rewrap,
          ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
          ~marks=[BadToken(str)],
          ~co_ctx=CoCtx.empty,
          m,
        )
      };

    | LivelitName(name) =>
      let (syn_lit, marks_lit) =
        switch (Ctx.lookup_livelit(ctx, name)) {
        | None => (SynTy.unknown_internal(), [Mark.Free(name)])
        | Some(livelit) => (livelit.expansion_t, [])
        };
      add(
        ~elab_term=LivelitName(name) |> rewrap,
        ~elab_syn_ty=syn_lit,
        ~marks=marks_lit,
        ~co_ctx=CoCtx.singleton(name, Exp.rep_id(uexp), ana),
        m,
      );
    | ListLit(es) =>
      let ids = List.map(Exp.rep_id, es);
      let inner_ana_ty = MatchedTyp.tolerant1(MatchedTyp.list, ctx, ana);
      let anas = List.init(List.length(es), _ => inner_ana_ty);
      let ((es, es_elabs), m) = map_m_go(m, anas, es);
      let m =
        List.fold_left(
          (m, e: Info.exp) => record(~id=here, Part, e.slice, m),
          m,
          es,
        );
      /* Use elements' synthesized types consistently for both the meet and
         the per-element ascription decision. Using `e.ty` (ana-coerced)
         would disagree with the syn-based meet and cause spurious Asc
         wrappings on elements that already syn to the meet type. */
      let syn_tys = List.map((e: Info.exp) => e.elab_syn_ty, es);
      let meet_ty =
        Typ.meet_all(~empty=Unknown(Internal) |> Typ.temp, ctx, syn_tys);
      let ds =
        List.map2(
          (d, t) => fresh_ascription(ctx, d, t, meet_ty),
          es_elabs,
          syn_tys,
        );
      switch (meet_ty) {
      | None =>
        let syn_no_meet = SynTy.meet_of(List, Unknown(Internal) |> Typ.temp);
        add(
          ~elab_term=ListLit(ds) |> rewrap,
          ~elab_syn_ty=syn_no_meet,
          ~marks=
            should_emit_nomeet_mark(ctx, ana, syn_no_meet)
              ? [NoMeet(List, Typ.add_source(ids, syn_tys))] : [],
          ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
          m,
        );
      | Some(ty) =>
        add(
          ~elab_term=ListLit(ds) |> rewrap,
          ~elab_syn_ty=List(ty) |> Typ.temp,
          ~marks=[],
          ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
          m,
        )
      };
    | Cons(hd, tl) =>
      let head_ana_ty = MatchedTyp.tolerant1(MatchedTyp.list, ctx, ana);
      let* (hd, hd_elab, m) = go(~ana=head_ana_ty, hd, m);
      let tail_ana_ty = Typ.match_synswitch(ana, List(hd.ty) |> Typ.temp);
      let& (tl, tl_elab, m) = go(~ana=tail_ana_ty, tl, m);
      /* `hd` was analyzed against `head_ana_ty` (the element-level ana),
         so `hd.ty` already incorporates ana info at the element level.
         Using it directly as the element type means fresh re-synthesis of
         the elab_term (which will ana-wrap hd via fresh_ascription below)
         agrees with the recorded type. */
      let inner_elab_syn_ty = hd.ty |> Typ.normalize(ctx) |> Typ.all_ids_temp;
      let elab_term =
        Cons(
          hd_elab
          |> fresh_ascription(
               ctx,
               _,
               hd.elab_syn_ty,
               Some(inner_elab_syn_ty),
             ),
          tl_elab
          |> fresh_ascription(
               ctx,
               _,
               tl.elab_syn_ty,
               Some(List(inner_elab_syn_ty) |> Typ.temp),
             ),
        )
        |> rewrap;
      add(
        ~elab_term,
        ~elab_syn_ty=List(inner_elab_syn_ty) |> Typ.temp,
        ~marks=[],
        ~co_ctx=CoCtx.union([hd.co_ctx, tl.co_ctx]),
        m,
      );
    | ListConcat(e1, e2) =>
      let inner_ana_ty =
        List(MatchedTyp.tolerant1(MatchedTyp.list, ctx, ana)) |> Typ.temp;
      let ids = List.map(Exp.rep_id, [e1, e2]);
      let^ (e1, e1_elab, m) = go(~ana=inner_ana_ty, e1, m);
      let& (e2, e2_elab, m) = go(~ana=inner_ana_ty, e2, m);
      /* Project each argument's synthesized type to its list element type.
         `list_tolerant` returns `?` when the arg's syn isn't a list, which
         is the correct behaviour for e.g. `A @ A` (where each `A` syns to
         a non-list constructor type but the result should still be `[?]`). */
      let elem_ty1 =
        MatchedTyp.tolerant1(MatchedTyp.list, ctx, e1.elab_syn_ty);
      let elem_ty2 =
        MatchedTyp.tolerant1(MatchedTyp.list, ctx, e2.elab_syn_ty);
      switch (
        Typ.meet_all(
          ~empty=Unknown(Internal) |> Typ.temp,
          ctx,
          [elem_ty1, elem_ty2],
        )
      ) {
      | None =>
        let syn_no_meet = SynTy.meet_of(List, Unknown(Internal) |> Typ.temp);
        add(
          ~elab_term=ListConcat(e1_elab, e2_elab) |> rewrap,
          ~elab_syn_ty=syn_no_meet,
          ~marks=
            should_emit_nomeet_mark(ctx, ana, syn_no_meet)
              ? [
                NoMeet(
                  List,
                  Typ.add_source(ids, [e1.elab_syn_ty, e2.elab_syn_ty]),
                ),
              ]
              : [],
          ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
          m,
        );
      | Some(elem_ty) =>
        add(
          ~elab_term=ListConcat(e1_elab, e2_elab) |> rewrap,
          ~elab_syn_ty=List(elem_ty) |> Typ.temp,
          ~marks=[],
          ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
          m,
        )
      };
    | Var(("$e" | "$v") as name) when is_in_filter =>
      /* Inside a filter, the meta-variables `$e` and `$v` stand for any
         expression/value, so we synthesize to `?` without consulting the ctx. */
      add(
        ~elab_term=Var(name) |> rewrap,
        ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[],
        ~co_ctx=CoCtx.empty,
        m,
      )
    | Var(name) =>
      let co_ctx = CoCtx.singleton(name, Exp.rep_id(uexp), ana);

      let (syn_v, marks_v) =
        switch (Ctx.lookup_var(ctx, name)) {
        | None => (SynTy.unknown_internal(), [Mark.Free(name)])
        | Some(var) => (var.typ, [])
        };
      add(
        ~elab_term=Var(name) |> rewrap,
        ~elab_syn_ty=syn_v,
        ~marks=marks_v,
        ~co_ctx,
        ~uses=[Slice.use(~sort=Value, ~name, ~id=Exp.rep_id(uexp), ())],
        m,
      );
    | DynamicErrorHole(e, err) =>
      let^ (e, e_elab, m) = go(~ana, e, m);
      add(
        ~elab_term=DynamicErrorHole(e_elab, err) |> rewrap,
        ~elab_syn_ty=e.elab_syn_ty,
        ~marks=e.marks,
        ~co_ctx=e.co_ctx,
        m,
      );
    | Parens(e) =>
      let^ (e, e_elab, m) = go(~ana, e, m);
      add(
        ~elab_term=Parens(e_elab) |> rewrap,
        ~elab_syn_ty=e.elab_syn_ty,
        ~marks=e.marks,
        ~co_ctx=e.co_ctx,
        m,
      );
    | Projector(data, e) =>
      let^ (e, e_elab, m) = go(~ana, e, m);
      add(
        ~elab_term=Projector(data, e_elab) |> rewrap,
        ~elab_syn_ty=e.elab_syn_ty,
        ~marks=e.marks,
        ~co_ctx=e.co_ctx,
        m,
      );
    | UnOp(op, e) =>
      let op = Operators.replace_un_op(op, ctx.use_mode); // Replace op if necessary due to `use`
      let op_semantics = Operators.semantics_of_un_op(op);
      switch (op_semantics) {
      | Undefined(msg) =>
        let& (e, e_elab, m) = go(~ana=syn, e, m);
        add(
          ~elab_term=UnOp(op, e_elab) |> rewrap,
          ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
          ~marks=[BadOperator(msg)],
          ~co_ctx=e.co_ctx,
          m,
        );
      | Defined(ty_in, ty_out, _) =>
        let ty_in = Atom(Atom.cls_of_kind(ty_in)) |> Typ.temp;
        let ty_out = Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp;
        let& (e, e_elab, m) = go(~ana=ty_in, e, m);
        add(
          ~elab_term=UnOp(op, e_elab) |> rewrap,
          ~elab_syn_ty=ty_out,
          ~marks=[],
          ~co_ctx=e.co_ctx,
          m,
        );
      };
    | BinOp(op, e1, e2) =>
      let op = Operators.replace_bin_op(op, ctx.use_mode); // Replace op if necessary due to `use`
      let op_semantics = Operators.semantics_of_bin_op(op);
      switch (op_semantics) {
      | Undefined(msg) =>
        let& (e1, e1_elab, m) = go(~ana=syn, e1, m);
        let& (e2, e2_elab, m) = go(~ana=syn, e2, m);
        add(
          ~elab_term=BinOp(op, e1_elab, e2_elab) |> rewrap,
          ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
          ~marks=[BadOperator(msg)],
          ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
          m,
        );
      | DefinedPoly(_) =>
        let ids = List.map(Exp.rep_id, [e1, e2]);
        let ((es, es_elabs), m) =
          map_m_go_omit(
            m,
            [Unknown(Internal) |> Typ.temp, Unknown(Internal) |> Typ.temp],
            [e1, e2],
          );
        let tys = List.map(Info.exp_ty, es);
        let elab_poly =
          BinOp(op, List.nth(es_elabs, 0), List.nth(es_elabs, 1)) |> rewrap;
        let co_poly = CoCtx.union(List.map(Info.exp_co_ctx, es));
        switch (Typ.meet_all(~empty=Unknown(Internal) |> Typ.temp, ctx, tys)) {
        | None =>
          add(
            ~elab_term=elab_poly,
            ~elab_syn_ty=Atom(Bool) |> Typ.fresh,
            ~marks=[NoMeet(PolyEq, Typ.add_source(ids, tys))],
            ~co_ctx=co_poly,
            m,
          )
        | Some(ty) when Typ.normalize(ctx, ty) |> Typ.has_fun =>
          add(
            ~elab_term=elab_poly,
            ~elab_syn_ty=Atom(Bool) |> Typ.fresh,
            ~marks=[CompareFun(ty)],
            ~co_ctx=co_poly,
            m,
          )
        | Some(_) =>
          add(
            ~elab_term=elab_poly,
            ~elab_syn_ty=Atom(Bool) |> Typ.fresh,
            ~marks=[],
            ~co_ctx=co_poly,
            m,
          )
        };
      | Defined(ty1, ty2, ty_out, _) =>
        let ty1 = Atom(Atom.cls_of_kind(ty1)) |> Typ.temp;
        let ty2 = Atom(Atom.cls_of_kind(ty2)) |> Typ.temp;
        let ty_out = Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp;
        let& (e1, e1_elab, m) = go(~ana=ty1, e1, m);
        let& (e2, e2_elab, m) = go(~ana=ty2, e2, m);
        add(
          ~elab_term=BinOp(op, e1_elab, e2_elab) |> rewrap,
          ~elab_syn_ty=ty_out,
          ~marks=[],
          ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
          m,
        );
      };
    | TupleExtension(e1, e2) =>
      let (t1, e1_elab, m) = go(e1, m);
      let m =
        switch (Typ.normalize(ctx, t1.ty).term) {
        | Prod(_)
        | Unknown(_) => m
        | _ => append_mark_exp(m, e1, [TupleExtensionRequiresTuples])
        };
      let (t2, e2_elab, m) = go(e2, m);
      let m =
        switch (Typ.normalize(ctx, t2.ty).term) {
        | Prod(_)
        | Unknown(_) => m
        | _ => append_mark_exp(m, e2, [TupleExtensionRequiresTuples])
        };

      let co_ctx = CoCtx.union([t1.co_ctx, t2.co_ctx]);
      let elab_term = TupleExtension(e1_elab, e2_elab) |> rewrap;

      switch (
        Typ.normalize(ctx, t1.ty).term,
        Typ.normalize(ctx, t2.ty).term,
      ) {
      | (Prod(ts1), Prod(ts2)) =>
        let extract_entry: Typ.t => (option(string), Typ.t) = (
          t =>
            switch (Typ.match_tup_label(t)) {
            | Some((name, t)) => (Some(name), t)
            | None => (None, t)
            }
        );
        let e1_entries = List.map(extract_entry, ts1);
        let e2_entries = List.map(extract_entry, ts2);

        let ty: Grammar.typ_t(IdTagged.IdTag.t) =
          IdTagged.FreshGrammar.Typ.(
            prod(
              List.map(
                ((lab, d)) =>
                  switch (lab) {
                  | Some(l) => tup_label(label(l), d)
                  | None => d
                  },
                LabeledTuple.extension(e1_entries, e2_entries),
              ),
            )
          );

        add(~elab_term, ~elab_syn_ty=ty, ~marks=[], ~co_ctx, m);
      | _ =>
        add(
          ~elab_term,
          ~elab_syn_ty=IdTagged.FreshGrammar.Typ.unknown(Internal),
          ~marks=[],
          ~co_ctx,
          m,
        )
      };

    | Tuple(es) =>
      let expected_labels =
        LabeledTupleStaticsHelpers.expected_labels_of_ana(ctx, ana);

      let original_labels =
        List.map(e => Exp.match_tup_label(e) |> Option.map(fst), es);

      let (inferred_es, ana_tys) =
        MatchedTyp.prod_rearrange(
          ctx,
          List.map(e => (None: option(string), e), es),
          ((inferred, e)) => {
            Exp.match_tup_label(e)
            |> Option.map(((label, _))
                 // Keep the original syntax node so label subtrees are analyzed.
                 => (label, (inferred, e)))
          },
          ana,
          (name, (_, e)) =>
            (
              Some(name),
              TupLabel(Label(name) |> Exp.fresh, e) |> Exp.fresh,
            ),
        );
      let es = List.map(snd, inferred_es);
      let inferred = List.map(fst, inferred_es);

      let new_labels =
        List.map(e => Exp.match_tup_label(e) |> Option.map(fst), es);

      let unique_duplicate_labels =
        LabeledTuple.get_duplicate_labels(Exp.match_tup_label, es);
      let duplicate_labels =
        LabeledTupleStaticsHelpers.expand_duplicate_labels(
          ~match_tup_label=Exp.match_tup_label,
          ~unique_duplicates=unique_duplicate_labels,
          es,
        );
      let invalid_labels =
        LabeledTupleStaticsHelpers.compute_invalid_labels(
          ~match_tup_label=Exp.match_tup_label,
          ~expected_labels,
          es,
        );

      let (es', es_elab, m) =
        List.fold_left2(
          ((es, es_elab, m), ana, (inferred_label, e: Exp.t)) =>
            switch (e.term) {
            | TupLabel({term: ExplicitNonlabel, _}, _) =>
              let (e_info, elab, m) = go(~ana, e, m);
              let (e_info, m) =
                LabeledTupleStaticsHelpers.apply_inferred_label_exp(
                  ~inferred_label,
                  e_info,
                  m,
                );
              (es @ [e_info], es_elab @ [elab], m);
            | TupLabel(label, value) =>
              let (labmode, val_mode) =
                LabeledTupleStaticsHelpers.decompose_label_mode(ctx, ana);
              let (value_info, value_elab, m) = go(~ana=val_mode, value, m);
              let (lab_name, label_invalid, m) =
                switch (label.term) {
                | Label(name) =>
                  let (label_syn, label_marks, label_invalid) =
                    LabeledTupleStaticsHelpers.validate_label_name(
                      ~name,
                      ~expected_labels,
                      ~duplicate_labels,
                    );
                  let (label_info, _, m) =
                    add(
                      ~user_term=label,
                      ~ancestors=ancestors_inclusive,
                      ~elab_term=label,
                      ~ctx,
                      ~ana=labmode,
                      ~elab_syn_ty=label_syn,
                      ~marks=label_marks,
                      ~co_ctx=CoCtx.empty,
                      ~label_inference=None,
                      ~inferred_label=None,
                      ~dot_labels=[],
                      ~label_sort=true,
                      ~warnings=[],
                      m,
                    );
                  (
                    Some(name),
                    label_invalid,
                    record(~id=Exp.rep_id(e), Part, label_info.slice, m),
                  );
                | EmptyHole =>
                  let (label_info, _, m) =
                    add(
                      ~user_term=label,
                      ~ancestors=ancestors_inclusive,
                      ~elab_term=label,
                      ~ctx,
                      ~ana=labmode,
                      ~elab_syn_ty=Unknown(SynSwitch) |> Typ.temp,
                      ~marks=[],
                      ~co_ctx=CoCtx.empty,
                      ~label_inference=None,
                      ~inferred_label=None,
                      ~dot_labels=[],
                      ~label_sort=true,
                      ~warnings=[],
                      m,
                    );
                  (
                    None,
                    false,
                    record(~id=Exp.rep_id(e), Part, label_info.slice, m),
                  );
                | _ =>
                  let (_, _, m) = go(~ana=labmode, label, m);
                  (
                    None,
                    false,
                    m
                    |> append_mark_exp(_, label, [BadLabel(Exp(label))])
                    |> set_label_sort_exp(_, label, true),
                  );
                };
              let (syn_tl, cms_tl) =
                LabeledTupleStaticsHelpers.tup_label_self_type(
                  ~lab_name,
                  ~label_invalid,
                  ~duplicate_labels,
                  ~value_ty=value_info.elab_syn_ty,
                  ~label_is_empty_hole=label.term == EmptyHole,
                  ~malformed_source=Exp(label),
                );
              let m = record(~id=Exp.rep_id(e), Part, value_info.slice, m);
              let (e_info, elab, m) =
                add(
                  ~user_term=e,
                  ~elab_term=TupLabel(label, value_elab) |> rewrap,
                  ~ctx,
                  ~ana,
                  ~ancestors=ancestors_inclusive,
                  ~elab_syn_ty=syn_tl,
                  ~marks=cms_tl,
                  ~co_ctx=value_info.co_ctx,
                  ~label_inference=None,
                  ~inferred_label,
                  ~dot_labels=[],
                  ~label_sort=false,
                  ~warnings=[],
                  m,
                );
              (es @ [e_info], es_elab @ [elab], m);
            | _ =>
              let (e_info, elab, m) = go(~ana, e, m);
              let (e_info, m) =
                LabeledTupleStaticsHelpers.apply_inferred_label_exp(
                  ~inferred_label,
                  e_info,
                  m,
                );
              (es @ [e_info], es_elab @ [elab], m);
            },
          ([], [], m),
          ana_tys,
          List.combine(inferred, es),
        );

      let m =
        List.fold_left(
          (m, e: Info.exp) => record(~id=here, Part, e.slice, m),
          m,
          es',
        );

      let ty_list = List.map((e: Info.exp) => e.elab_syn_ty, es');

      let malformed_labels =
        LabeledTupleStaticsHelpers.collect_malformed_labels(
          ~has_tup_label=
            (e: Info.exp) =>
              switch (e.user_term.term) {
              | TupLabel(_, _) => true
              | _ => false
              },
          ~get_marks=(e: Info.exp) => e.marks,
          es',
        );
      let (syn_tuple, cms_tuple) =
        LabeledTupleStaticsHelpers.finalize_tuple_type(
          ~duplicate_labels,
          ~invalid_labels,
          ~malformed_labels,
          ty_list,
        );
      let tuple_elab =
        switch (Typ.weak_head_normalize(ctx, ana).term) {
        | Prod(ts) =>
          Tuple(
            LabeledTuple.rearrange(
              Typ.match_tup_label,
              Exp.match_tup_label,
              ts,
              es_elab,
              (label, body) =>
              TupLabel(Label(label) |> Exp.fresh, body) |> Exp.fresh
            ),
          )
          |> rewrap
        | _ => Tuple(es_elab) |> rewrap
        };
      add(
        ~elab_term=tuple_elab,
        ~elab_syn_ty=syn_tuple,
        ~marks=cms_tuple,
        ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es')),
        ~label_inference=
          Some(
            LabeledTupleHelpers.derive_label_inference_info(
              original_labels,
              new_labels,
            ),
          ),
        m,
      );
    | TupLabel({term: ExplicitNonlabel, _} as label, e) =>
      let (e, elab_inner, m) = go(~ana, e, m);
      /* Add info for the ExplicitNonlabel directly */
      let (_, elab_label, m) =
        add(
          ~user_term=label,
          ~elab_term=label,
          ~ancestors=ancestors_inclusive,
          ~ctx,
          ~ana=syn,
          ~elab_syn_ty=ExplicitNonlabel |> Typ.temp,
          ~marks=[],
          ~co_ctx=CoCtx.empty,
          ~label_inference=None,
          ~inferred_label=None,
          ~dot_labels=[],
          ~label_sort=true,
          ~warnings=[],
          m,
        );
      add(
        ~elab_term=TupLabel(elab_label, elab_inner) |> rewrap,
        ~elab_syn_ty=
          TupLabel(ExplicitNonlabel |> Typ.temp, e.elab_syn_ty) |> Typ.temp,
        ~marks=[],
        ~co_ctx=e.co_ctx,
        m,
      );
    | TupLabel(label, e) =>
      let (labmode, val_mode) =
        LabeledTupleStaticsHelpers.decompose_label_mode(ctx, ana);
      let (e, elab_child, m) = go(~ana=val_mode, e, m);
      let (lab_name, m) =
        switch (label.term) {
        | Label(name) =>
          let (_, _, m) =
            add(
              ~user_term=label,
              ~elab_term=label,
              ~ancestors=ancestors_inclusive,
              ~ctx,
              ~ana=labmode,
              ~elab_syn_ty=Label(name) |> Typ.temp,
              ~marks=[],
              ~co_ctx=CoCtx.empty,
              ~label_inference=None,
              ~inferred_label=None,
              ~dot_labels=[],
              ~label_sort=true,
              ~warnings=[],
              m,
            );
          (Some(name), m);
        | EmptyHole =>
          let (_, _, m) =
            add(
              ~user_term=label,
              ~elab_term=label,
              ~ancestors=ancestors_inclusive,
              ~ctx,
              ~ana=labmode,
              ~elab_syn_ty=Unknown(SynSwitch) |> Typ.temp,
              ~marks=[],
              ~co_ctx=CoCtx.empty,
              ~label_inference=None,
              ~inferred_label=None,
              ~dot_labels=[],
              ~label_sort=true,
              ~warnings=[],
              m,
            );
          (None, m);
        | _ =>
          let (_, _, m) = go(~ana=labmode, label, m);
          (
            None,
            m
            |> set_label_sort_exp(_, label, true)
            |> append_mark_exp(_, label, [BadLabel(Exp(label))]),
          );
        };
      let (syn_tl, cms_tl) =
        LabeledTupleStaticsHelpers.standalone_tup_label_self_type(
          ~lab_name,
          ~value_ty=e.elab_syn_ty,
          ~label_is_empty_hole=label.term == EmptyHole,
          ~malformed_source=Exp(label),
        );
      add(
        ~elab_term=TupLabel(label, elab_child) |> rewrap,
        ~elab_syn_ty=syn_tl,
        ~marks=cms_tl,
        ~co_ctx=e.co_ctx,
        m,
      );
    | ExplicitNonlabel =>
      add(
        ~elab_term=ExplicitNonlabel |> rewrap,
        ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[ExplicitNonlabel],
        ~co_ctx=CoCtx.empty,
        m,
      )
    | Label(name) =>
      add(
        ~elab_term=Label(name) |> rewrap,
        ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[UnexpectedLabelSort(name)],
        ~co_ctx=CoCtx.empty,
        m,
      )
    | BuiltinFun(string) =>
      let (syn_b, marks_b) =
        switch (Ctx.lookup_var(Builtins.ctx_init(None), string)) {
        | None => (SynTy.unknown_internal(), [Mark.Free(string)])
        | Some(var) => (var.typ, [])
        };
      add(
        ~elab_term=BuiltinFun(string) |> rewrap,
        ~elab_syn_ty=syn_b,
        ~marks=marks_b,
        ~co_ctx=CoCtx.empty,
        m,
      );

    | Dot(e1, e2) =>
      let (info_e1, e1_elab, m) = go(~ana=syn, e1, m);
      let available_labels = {
        let ty = Typ.normalize(ctx, info_e1.ty);
        switch (ty.term) {
        | Prod(ts) =>
          List.filter_map(Typ.match_tup_label, ts) |> List.map(fst)
        | List({term: Prod(ts), _}) =>
          List.filter_map(Typ.match_tup_label, ts) |> List.map(fst)
        | _ => []
        };
      };

      /* Analyze label child, then patch with label_sort, dot_labels,
         and correct self (Label produces UnexpectedLabelSort by default,
         but in dot position it should be Just(Label(name))) */

      let (info_e2, elab_e2, m) =
        switch (e2.term) {
        | Label(name) =>
          add(
            ~user_term=e2,
            ~elab_term=e2,
            ~ancestors=ancestors_inclusive,
            ~ctx,
            ~ana=syn,
            ~elab_syn_ty=Label(name) |> Typ.temp,
            ~marks=[],
            ~co_ctx=CoCtx.empty,
            ~label_inference=None,
            ~inferred_label=None,
            ~dot_labels=available_labels,
            ~label_sort=true,
            ~warnings=[],
            m,
          )
        | _ =>
          /* Malformed label — analyze via go to cover sub-expression IDs */
          let (info_e2, elab_e2, m) = go(~ana=syn, e2, m);
          (
            info_e2,
            elab_e2,
            m
            |> set_label_sort_exp(_, e2, true)
            |> set_dot_labels_exp(_, e2, available_labels),
          );
        };

      let dot_elab = Dot(e1_elab, elab_e2) |> rewrap;
      let dot_co_ctx = CoCtx.union([info_e1.co_ctx, info_e2.co_ctx]);

      let (ty, m) = {
        switch (info_e1.ty.term, info_e2.ty.term) {
        | (Unknown(_), Label(name)) =>
          // This is so that the statics will result in Unknown(Internal)
          let ty =
            Prod([
              TupLabel(
                Label(name) |> Typ.temp,
                Unknown(Internal) |> Typ.temp,
              )
              |> Typ.temp,
            ])
            |> Typ.temp;
          let (_, _, m) = go(~ana=ty, e1, m);
          (ty, m);
        | _ => (Typ.normalize(ctx, info_e1.ty), m)
        };
      };
      switch (ty.term) {
      | Prod(ts) =>
        let labels =
          List.filter_map(Typ.match_tup_label, ts) |> List.map(fst);

        switch (e2.term) {
        | Label(name) =>
          let element: option(Typ.t) =
            LabeledTuple.find_label(Typ.match_tup_label, ts, name);
          let field_index =
            List.find_index(
              (t: Typ.t) =>
                switch (Typ.match_tup_label(t)) {
                | Some((label, _)) => label == name
                | None => false
                },
              ts,
            );
          switch (element) {
          | Some({term: TupLabel(_, typ), _})
          | Some(typ) =>
            let slice =
              Option.map(
                index =>
                  Slice.component(
                    ~ctx,
                    ~matcher=MatchedTyp.label,
                    ~index=1,
                    Slice.component(
                      ~ctx,
                      ~matcher=MatchedTyp.prod(List.length(ts)),
                      ~index,
                      info_e1.slice,
                    ),
                  ),
                field_index,
              );
            add(
              ~elab_term=dot_elab,
              ~elab_syn_ty=typ,
              ~marks=[],
              ~dot_labels=available_labels,
              ~co_ctx=dot_co_ctx,
              ~slice,
              m,
            );
          | None =>
            add(
              ~elab_term=dot_elab,
              ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
              ~marks=[LabelNotFound(name, labels)],
              ~dot_labels=available_labels,
              ~co_ctx=dot_co_ctx,
              m,
            )
          };
        | EmptyHole =>
          add(
            ~elab_term=dot_elab,
            ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
            ~marks=[],
            ~dot_labels=available_labels,
            ~co_ctx=dot_co_ctx,
            m,
          )
        | _ =>
          add(
            ~elab_term=dot_elab,
            ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
            ~marks=[BadLabel(Exp(e2))],
            ~dot_labels=available_labels,
            ~co_ctx=dot_co_ctx,
            m,
          )
        };
      | List({term: Prod(ts), _}) =>
        let labels =
          List.filter_map(Typ.match_tup_label, ts) |> List.map(fst);

        switch (e2.term) {
        | Label(name) =>
          let element: option(Typ.t) =
            LabeledTuple.find_label(Typ.match_tup_label, ts, name);
          switch (element) {
          | Some({term: TupLabel(_, typ), _})
          | Some(typ) =>
            add(
              ~elab_term=dot_elab,
              ~elab_syn_ty=List(typ) |> Typ.fresh,
              ~marks=[],
              ~dot_labels=available_labels,
              ~co_ctx=dot_co_ctx,
              m,
            )
          | None =>
            add(
              ~elab_term=dot_elab,
              ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
              ~marks=[LabelNotFound(name, labels)],
              ~dot_labels=available_labels,
              ~co_ctx=dot_co_ctx,
              m,
            )
          };
        | EmptyHole =>
          add(
            ~elab_term=dot_elab,
            ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
            ~marks=[],
            ~dot_labels=available_labels,
            ~co_ctx=dot_co_ctx,
            m,
          )
        | _ =>
          add(
            ~elab_term=dot_elab,
            ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
            ~marks=[BadLabel(Exp(e2))],
            ~dot_labels=available_labels,
            ~co_ctx=dot_co_ctx,
            m,
          )
        };
      | List({term: Unknown(_), _}) =>
        add(
          ~elab_term=dot_elab,
          ~elab_syn_ty=List(Unknown(Internal) |> Typ.temp) |> Typ.temp,
          ~marks=[],
          ~dot_labels=available_labels,
          ~co_ctx=dot_co_ctx,
          m,
        )
      | _ =>
        add(
          ~elab_term=dot_elab,
          ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
          ~marks=[DotOperatorRequiresTuple],
          ~dot_labels=available_labels,
          ~co_ctx=dot_co_ctx,
          m,
        )
      };
    | Test(e) =>
      let& (e, e_elab, m) = go(~ana=Atom(Bool) |> Typ.temp, e, m);
      add(
        ~elab_term=Test(e_elab) |> rewrap,
        ~elab_syn_ty=Prod([]) |> Typ.temp,
        ~marks=[],
        ~co_ctx=e.co_ctx,
        m,
      );
    | HintedTest(e, hint) =>
      let (e, e_elab, m) = go(~ana=Atom(Bool) |> Typ.temp, e, m);
      let (hint, hint_elab, m) = go(~ana=Atom(String) |> Typ.temp, hint, m);
      add(
        ~elab_term=HintedTest(e_elab, hint_elab) |> rewrap,
        ~elab_syn_ty=Prod([]) |> Typ.temp,
        ~marks=[],
        ~co_ctx=CoCtx.union([e.co_ctx, hint.co_ctx]),
        m,
      );
    | Filter(Filter({pat: cond, act}), body) =>
      let& (cond, cond_elab, m) = go(~ana=syn, cond, m, ~is_in_filter=true);
      let^ (body, body_elab, m) = go(~ana, body, m);
      add(
        ~elab_term=
          Filter(
            Filter({
              act,
              pat: cond_elab,
            }),
            body_elab,
          )
          |> rewrap,
        ~elab_syn_ty=body.elab_syn_ty,
        ~marks=[],
        ~co_ctx=CoCtx.union([cond.co_ctx, body.co_ctx]),
        m,
      );
    | Filter(Residue(i, act), body) =>
      let^ (body, body_elab, m) = go(~ana, body, m);
      add(
        ~elab_term=Filter(Residue(i, act), body_elab) |> rewrap,
        ~elab_syn_ty=body.elab_syn_ty,
        ~marks=[],
        ~co_ctx=CoCtx.union([body.co_ctx]),
        m,
      );
    | Seq(e1, e2) =>
      let& (e1, e1_elab, m) = go(~ana=syn, e1, m);
      let^ (e2, e2_elab, m) = go(~ana, e2, m);
      add(
        ~elab_term=Seq(e1_elab, e2_elab) |> rewrap,
        ~elab_syn_ty=e2.elab_syn_ty,
        ~marks=[],
        ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
        m,
      );
    | Constructor(ctr, ty) =>
      let (syn_res, marks_res) =
        ConstructorStaticsHelpers.syn_marks_ctr(ctx, ctr, ana, ty);
      switch (marks_res) {
      | [FreeConstructor(name)] =>
        /* If not a known constructor, try looking up as a variable.
           This supports capitalized module names like M.x where M is
           parsed as Constructor but is actually a variable binding. */
        switch (Ctx.lookup_var(ctx, name)) {
        | Some({typ, _}) =>
          let co_ctx = CoCtx.singleton(name, Exp.rep_id(uexp), ana);
          let elab_term = Var(name) |> rewrap;
          let (info, _, m) =
            add(~elab_term, ~elab_syn_ty=typ, ~marks=[], ~co_ctx, m);
          let m =
            add_info(
              ids,
              Info.InfoExp({
                ...info,
                cls: Exp(Var),
              }),
              m,
            );
          (info, elab_term, m);
        | None =>
          let elab_term = Constructor(ctr, Some(None)) |> rewrap;
          add(
            ~elab_term,
            ~elab_syn_ty=syn_res,
            ~marks=marks_res,
            ~co_ctx=CoCtx.empty,
            ~uses=[
              Slice.use(
                ~sort=Constructor,
                ~name=ctr,
                ~id=Exp.rep_id(uexp),
                (),
              ),
            ],
            m,
          );
        }
      | _ =>
        /* `ctor_ty` is the user-visible specialization, e.g.
           preserving `TypParamAp(Var("List"), Int)` form for
           parameterized aliases. Flows out as `elab_syn_ty` so the
           surrounding `Ap` can propagate the same parameterized
           result into nested constructor analyses, letting implicit
           instantiation be inserted at every level. */
        let ctor_ty = fixed_typ(ctx, ana, syn_res);
        let ctor_ty_for_ann = Typ.normalize(ctx, ctor_ty);
        /* The annotation the elaborated `Constructor` carries:
           - Polymorphic ctx-schema constructors carry the schema
             *normalized* — a `Poly` whose body exposes the
             underlying `Rec`/`Sum` for every parameterized alias
             reference. Lets re-statics meet constructor
             annotations and ascriptions structurally even after
             `get_sum_constructors`/`unroll` produces
             `TypParamAp(Rec(…), Int)` residues from both sides.
           - Monomorphic constructors use `ctor_ty_for_ann`, which
             exposes their underlying `Sum` (and any hidden arrows
             inside variants) for `DHExp.ty_comparable`.
           - Prior explicit ascriptions (`ty = Some(Some(_))`) are
             preserved as-is. */
        let poly_schema =
          switch (Ctx.lookup_ctr(ctx, ctr)) {
          | Some({typ: {term: Poly(_), _} as schema, _}) =>
            Some(Typ.normalize(ctx, schema))
          | _ => None
          };
        let type_args =
          ty == None
            ? ConstructorStaticsHelpers.instantiation_args_for(ctx, ctr, ana)
            : [];
        let elab_term =
          switch (type_args) {
          | [] =>
            let annotation =
              switch (ty, poly_schema) {
              | (Some(Some(_)), _) => ctor_ty_for_ann
              | (_, Some(schema)) => schema
              | (_, None) => ctor_ty_for_ann
              };
            Constructor(ctr, Some(Some(annotation))) |> rewrap;
          | _ =>
            let schema =
              switch (poly_schema) {
              | Some(schema) => schema
              | None => ctor_ty_for_ann
              };
            ConstructorStaticsHelpers.wrap_typ_param_aps(
              Constructor(ctr, Some(Some(schema))) |> Exp.fresh,
              type_args,
            );
          };
        /* When the elaboration wraps the constructor in
           `TypAp(_, type_args)`, the resulting expression's syn
           type is the schema's body with the args substituted —
           not the schema itself. */
        let elab_syn_ty =
          switch (type_args, poly_schema) {
          | ([_, ..._], Some({term: Poly(binder, body), _})) =>
            let binders = TPat.binders_of(binder);
            if (List.length(binders) == List.length(type_args)) {
              Typ.subst_many(type_args, binders, body);
            } else {
              ctor_ty;
            };
          | _ => ctor_ty
          };
        /* Manually emit ExpectationMismatch based on the clean syn_res
           (not ctor_ty), since ctor_ty has already been reconciled with ana
           and would otherwise silently meet. */
        let marks_res =
          switch (expectation_mismatch_mark(ctx, ana, syn_res)) {
          | None => marks_res
          | Some(m) when marks_res == [] => [m]
          | Some(_) => marks_res
          };
        let uses =
          [
            Slice.use(~sort=Constructor, ~name=ctr, ~id=Exp.rep_id(uexp), ()),
          ]
          @ (
            switch (ConstructorStaticsHelpers.alias_of_ctr(ctx, ctr)) {
            | Some(entry) => [
                Slice.use(
                  ~sort=Alias,
                  ~name=entry.name,
                  ~id=entry.id,
                  ~demanded=
                    ConstructorStaticsHelpers.alias_demand(ctx, ctr, entry),
                  (),
                ),
              ]
            | None => []
            }
          );
        add(
          ~elab_term,
          ~elab_syn_ty,
          ~marks=marks_res,
          ~co_ctx=CoCtx.empty,
          ~uses,
          m,
        );
      };
    | Ap(dir, fn, arg) =>
      switch (fn.term) {
      | LivelitName(s) =>
        // refer to livelit context to find types
        switch (Ctx.lookup_livelit(ctx, s)) {
        | Some({expansion_t, model_t, expand, _}) =>
          let (fn, fn_elab, m) = go(~ana=expansion_t, fn, m);
          let (arg, arg_elab, m) = go(~ana=model_t, arg, m);

          // try to expand
          switch (expand(arg.user_term)) {
          | Some(expanded) =>
            let (info, elab, m) =
              add(
                ~elab_term=expanded,
                ~elab_syn_ty=expansion_t,
                ~marks=[],
                ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]),
                m,
              );
            (
              info,
              elab,
              IdTagged.ids(expanded)
              |> add_missing_info(_, Info.InfoExp(info), m),
            );
          | None =>
            // if we can't expand, flag as improper model
            add(
              ~elab_term=Ap(dir, fn_elab, arg_elab) |> rewrap,
              ~elab_syn_ty=expansion_t,
              ~marks=[BadLivelitModel(expansion_t)],
              ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]),
              m,
            )
          };

        | None =>
          let (fn, fn_elab, m) =
            go(~ana=Unknown(Internal) |> Typ.temp, fn, m);
          let (arg, arg_elab, m) =
            go(~ana=Unknown(Internal) |> Typ.temp, arg, m);
          add(
            ~elab_term=Ap(dir, fn_elab, arg_elab) |> rewrap,
            ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
            ~marks=[],
            ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]),
            m,
          );
        }
      | _ =>
        /* If this is a builtin with custom statics */
        let custom_statics =
          switch (fn.term) {
          | Var(v) =>
            Ctx.lookup_var(ctx, v)
            |> Option.bind(_, (e: Ctx.var_entry) => e.custom_statics)
          | _ => None
          };

        /* This logic lets us treat constructors differently to functions in
           terms of error localization */
        let fn_ana =
          switch (Exp.ctr_name(fn)) {
          | Some(name) =>
            switch (fn.term) {
            | Constructor(_, Some(Some(ty)))
                when MatchedTyp.poly_pair(ctx, ty) == None => ty
            | _ =>
              switch (ConstructorStaticsHelpers.ctr_ana_typ(ctx, ana, name)) {
              | Some(ty_ana) =>
                switch (MatchedTyp.strict2(MatchedTyp.arrow, ctx, ty_ana)) {
                | Some((ty1, ty2)) => Arrow(ty1, ty2) |> Typ.temp
                | None =>
                  MatchedTyp.poly_pair(ctx, ty_ana) != None
                    ? Poly(EmptyHole |> TPat.fresh, syn) |> Typ.temp
                    : Arrow(syn, syn) |> Typ.temp
                }
              | None =>
                constructor_has_poly_schema(name)
                  ? Poly(EmptyHole |> TPat.fresh, syn) |> Typ.temp
                  : Arrow(syn, syn) |> Typ.temp
              }
            }
          | None =>
            expects_poly_callee(fn)
              ? Poly(EmptyHole |> TPat.fresh, syn) |> Typ.temp
              : Arrow(syn, syn) |> Typ.temp
          };
        let (fn, fn_elab, m) = go(~ana=fn_ana, fn, m);
        switch (custom_statics) {
        | Some(kind) =>
          CustomStatics.custom_statics_ap(
            ~ctx,
            ~ancestors=ancestors_inclusive,
            ~fn_info=fn,
            kind,
            (module
             {
               let uexp_to_info_map =
                   (~ctx, ~ana=?, ~is_in_filter=?, ~ancestors=?, exp, m) =>
                 go(~ctx, ~ana?, ~is_in_filter?, ~ancestors?, exp, m);
               let add = add;
             }),
            m,
            arg,
          )
        | None =>
          let (fn_ty, fn_elab) =
            implicit_poly_instantiate(fn.elab_syn_ty, fn_elab);
          let (ty_in, ty_out) =
            MatchedTyp.tolerant2(MatchedTyp.arrow, ctx, fn_ty);
          let& (arg, arg_elab, m) = go(~ana=ty_in, arg, m);
          let elab_term = Ap(dir, fn_elab, arg_elab) |> rewrap;
          let co_ap = CoCtx.union([fn.co_ctx, arg.co_ctx]);
          let slice =
            Some(
              Slice.component(
                ~ctx,
                ~matcher=MatchedTyp.arrow,
                ~index=1,
                fn.slice,
              ),
            );
          Id.is_nullary_ap_flag(IdTagged.ids(arg.user_term))
          && !Typ.is_consistent(ctx, ty_in, Prod([]) |> Typ.temp)
            ? add(
                ~elab_term,
                ~elab_syn_ty=ty_out,
                ~marks=[BadTrivAp(ty_in)],
                ~co_ctx=co_ap,
                ~slice,
                m,
              )
            : add(
                ~elab_term,
                ~elab_syn_ty=ty_out,
                ~marks=[],
                ~co_ctx=co_ap,
                ~slice,
                m,
              );
        };
      }
    | TypAp(fn, utyp) =>
      let typfn_ana = Poly(EmptyHole |> TPat.fresh, syn) |> Typ.temp;
      let (fn, fn_elab, m) = go(~ana=typfn_ana, fn, m);
      let (_, m) =
        utyp_to_info_map(~ctx, ~ancestors=ancestors_inclusive, utyp, m);
      let elab_term = TypAp(fn_elab, Typ.normalize(ctx, utyp)) |> rewrap;
      let (option_name, ty_body) = MatchedTyp.poly_pair_tolerant(ctx, fn.ty);
      /* Check the type-arg arity against the `Poly` binders. A
         multi-binder `Poly(TPat.Tuple([a, b, …]), body)` requires
         all args in one source-level application (surface
         `e@<T1, T2>` parses as `TypAp(e, TypTuple([T1, T2]))`). On
         arity mismatch, mark this node and use `Unknown(Internal)`
         as the result so the surrounding expression doesn't see a
         body with free type variables left over from a partial
         substitution. */
      let (elab_syn_ty, marks) =
        switch (option_name) {
        | None => (ty_body, [])
        | Some(name) =>
          let binders = TPat.binders_of(name);
          let n_expected = List.length(binders);
          let arg_list =
            switch (Typ.term_of(utyp), n_expected) {
            | (TypTuple(ts), n) when n > 1 => ts
            | _ => [utyp]
            };
          let n_actual = List.length(arg_list);
          if (n_expected == n_actual) {
            (Typ.subst_many(arg_list, binders, ty_body), []);
          } else {
            (
              Unknown(Internal) |> Typ.temp,
              [
                Mark.TypAbsApplyArityMismatch({
                  expected: n_expected,
                  actual: n_actual,
                }),
              ],
            );
          };
        };
      add(~elab_term, ~elab_syn_ty, ~marks, ~co_ctx=fn.co_ctx, m);
    | DeferredAp(fn, args) =>
      /* If this is a builtin with custom statics */
      let custom_statics =
        switch (fn.term) {
        | Var(v) =>
          Ctx.lookup_var(ctx, v)
          |> Option.bind(_, (e: Ctx.var_entry) => e.custom_statics)
        | _ => None
        };

      /* This logic lets us treat constructors differently to functions in
         terms of error localization */
      let fn_ana =
        switch (Exp.ctr_name(fn)) {
        | Some(name) =>
          switch (fn.term) {
          | Constructor(_, Some(Some(ty)))
              when MatchedTyp.poly_pair(ctx, ty) == None => ty
          | _ =>
            switch (ConstructorStaticsHelpers.ctr_ana_typ(ctx, ana, name)) {
            | Some(ty_ana) =>
              switch (MatchedTyp.strict2(MatchedTyp.arrow, ctx, ty_ana)) {
              | Some((ty1, ty2)) => Arrow(ty1, ty2) |> Typ.temp
              | None =>
                MatchedTyp.poly_pair(ctx, ty_ana) != None
                  ? Poly(EmptyHole |> TPat.fresh, syn) |> Typ.temp
                  : Arrow(syn, syn) |> Typ.temp
              }
            | None =>
              constructor_has_poly_schema(name)
                ? Poly(EmptyHole |> TPat.fresh, syn) |> Typ.temp
                : Arrow(syn, syn) |> Typ.temp
            }
          }
        | None =>
          expects_poly_callee(fn)
            ? Poly(EmptyHole |> TPat.fresh, syn) |> Typ.temp
            : Arrow(syn, syn) |> Typ.temp
        };
      let (fn, fn_elab, m) = go(~ana=fn_ana, fn, m);

      switch (custom_statics) {
      | Some(kind) =>
        CustomStatics.custom_statics_deferred_ap(
          ~elab_term=DeferredAp(fn_elab, args) |> rewrap,
          ~ctx,
          ~ancestors,
          ~fn_info=fn,
          kind,
          (module
           {
             let uexp_to_info_map =
                 (~ctx, ~ana=?, ~is_in_filter=?, ~ancestors=?, exp, m) =>
               go(~ctx, ~ana?, ~is_in_filter?, ~ancestors?, exp, m);
             let add = add;
           }),
          m,
          args,
        )
      | None =>
        let (ty_in, ty_out) =
          MatchedTyp.tolerant2(MatchedTyp.arrow, ctx, fn.ty);
        let num_args = List.length(args);
        switch (MatchedTyp.args(ctx, ty_in, num_args)) {
        | L(ty_ins) =>
          let ((args_infos, args_elabs), m) = map_m_go(m, ty_ins, args);
          let arg_co_ctx =
            CoCtx.union(List.map(Info.exp_co_ctx, args_infos));
          let ty_in' =
            List.combine(ty_ins, args)
            |> List.filter(((_, e)) => Exp.is_deferral(e))
            |> List.map(fst)
            |> (
              fun
              | [x] => x
              | xs => Prod(xs) |> Typ.temp
            );
          add(
            ~elab_term=DeferredAp(fn_elab, args_elabs) |> rewrap,
            ~elab_syn_ty=Arrow(ty_in', ty_out) |> Typ.temp,
            ~marks=[],
            ~co_ctx=CoCtx.union([fn.co_ctx, arg_co_ctx]),
            m,
          );
        | R(expected) =>
          let ty_ins =
            List.init(num_args, _ => Unknown(Internal) |> Typ.temp);
          let ((args, args_elabs), m) = map_m_go(m, ty_ins, args);
          let arg_co_ctx = CoCtx.union(List.map(Info.exp_co_ctx, args));
          add(
            ~elab_term=DeferredAp(fn_elab, args_elabs) |> rewrap,
            ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
            ~marks=[
              IsBadPartialAp(
                ArityMismatch({
                  expected,
                  actual: num_args,
                }),
              ),
            ],
            ~co_ctx=CoCtx.union([fn.co_ctx, arg_co_ctx]),
            m,
          );
        };
      };
    | Fun(p, e, typ, n) =>
      let pat_typ_refs = ModuleHelpers.collect_pat_type_refs(ctx, p);
      let (mode_pat, mode_body) =
        MatchedTyp.tolerant2(MatchedTyp.arrow, ctx, ana);
      let mode_pat = Option.value(~default=mode_pat, typ);
      let (p', _, _) =
        go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~ana=mode_pat, p, m);
      let (e, e_elab, m) = go(~ctx=p'.ctx, ~ana=mode_body, e, m);
      /* Second pass: re-analyze the pattern to attach the body's co_ctx.
         Use `p'.ty` (the ana-meet'd type) rather than `p'.elab_syn_ty`.
         For bare `Var`/`EmptyHole` patterns `elab_syn_ty` is `?`, which
         would erase the ana info on the pattern (breaking e.g. the
         Introduce feature and any display that relies on the pattern's
         recorded `ana`). `p'.ty` preserves the ana. */
      let (p, p_elab, m) =
        go_pat(~is_synswitch=false, ~co_ctx=e.co_ctx, ~ana=p'.ty, p, m);
      let m =
        m
        |> record(~id=here, Binder, p.slice)
        |> record(~id=here, Part, e.slice);
      let syn_ty_fun = Arrow(p.ty, e.elab_syn_ty) |> Typ.temp;
      let Coverage.CheckMatrix.{exhaustiveness, _} =
        Coverage.check([Info.pat_constraint(p)], Typ.normalize(ctx, p.ty));
      let marks_fun =
        switch (exhaustiveness) {
        | Exhaustive => []
        | Inexhaustive(unseen_pattern) => [
            Mark.InexhaustiveMatch(syn_ty_fun, [], unseen_pattern),
          ]
        };
      let elab_term = Fun(p_elab, e_elab, Some(p.ty), n) |> rewrap;
      add(
        ~elab_term,
        ~elab_syn_ty=syn_ty_fun,
        ~marks=marks_fun,
        ~co_ctx=CoCtx.union([CoCtx.mk(ctx, p.ctx, e.co_ctx), pat_typ_refs]),
        m,
      );
    | Forall(p, e) =>
      let (p, p_elab, m) =
        go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, p, m);
      let (e, e_elab, m) =
        go(~ctx=p.ctx, ~ana=Atom(Bool) |> Typ.temp, e, m);
      add(
        ~elab_term=Forall(p_elab, e_elab) |> rewrap,
        ~elab_syn_ty=Atom(Bool) |> Typ.temp,
        ~marks=[],
        ~co_ctx=CoCtx.mk(ctx, p.ctx, e.co_ctx),
        m,
      );
    | TypAbs(utpat, body, tfname) =>
      /* `utpat` may be a single binder or a `TPat.Tuple([…])`
         representing a multi-binder `typfun a, b -> e`. Extend the
         context with each binder, and rename the expected Poly's
         binder list element-wise so the body's expected type uses the
         user-written names. */
      let (name_expected_opt, item) =
        MatchedTyp.poly_pair_tolerant(ctx, ana);
      let user_binders = TPat.binders_of(utpat);
      let user_names_safe =
        user_binders
        |> List.filter(b =>
             switch (TPat.tyvar_of_utpat(b)) {
             | Some(name) => !Ctx.is_base_typ(name)
             | None => false
             }
           );
      let mode_body =
        switch (name_expected_opt) {
        | Some(expected_tpat) =>
          let expected_binders = TPat.binders_of(expected_tpat);
          if (List.length(expected_binders) != List.length(user_binders)) {
            item;
          } else {
            List.fold_left2(
              (body, exp_b, user_b) =>
                switch (TPat.tyvar_of_utpat(user_b)) {
                | Some(name) when !Ctx.is_base_typ(name) =>
                  Typ.subst(Var(name) |> Typ.temp, exp_b, body)
                | _ => body
                },
              item,
              expected_binders,
              user_binders,
            );
          };
        | None => item
        };
      let ctx_body =
        List.fold_left(
          (ctx, b: TPat.t) =>
            switch (TPat.tyvar_of_utpat(b)) {
            | Some(name) =>
              Ctx.extend_tvar(
                ctx,
                {
                  name,
                  id: TPat.rep_id(b),
                  kind: Abstract,
                  typ_kind: TypKind.Type,
                },
              )
            | None => ctx
            },
          ctx,
          user_names_safe,
        );
      let m =
        utpat_to_info_map(~ctx, ~ancestors=ancestors_inclusive, utpat, m)
        |> snd;
      let (body, body_elab, m) = go(~ctx=ctx_body, ~ana=mode_body, body, m);
      add(
        ~elab_term=TypAbs(utpat, body_elab, tfname) |> rewrap,
        ~elab_syn_ty=Poly(utpat, body.elab_syn_ty) |> Typ.temp,
        ~marks=[],
        ~co_ctx=body.co_ctx,
        m,
      );
    | Let(p, def, body) when Option.is_some(FunctionSugar.detect(p)) =>
      /* Syntactic sugar: `let f(x: Int, y): Ret = def` desugars to
         `let f = fun (x: Int, y) -> (def : Ret)`. Build the rewrite and
         delegate to the regular Let machinery by recursing; patch up
         the info map for pattern ids that vanish in the rewrite
         (the Ap wrapper and optional outer Asc). Same structural
         pattern as `ModuleExp` expansion above and `Typ.desugar_sig`. */
      let (f_name, args, ret_ty) = Option.get(FunctionSugar.detect(p));
      let rewritten =
        FunctionSugar.rewrite(
          ~orig_let=uexp,
          ~f_name,
          ~args,
          ~ret_ty,
          ~def,
          ~body,
        );
      let (rewritten_info, rewritten_elab, m) = go(~ana, rewritten, m);
      let m = FunctionSugar.add_binder_infos(m, ~user_pat=p, ~f_name);
      add(
        ~elab_term=rewritten_elab,
        ~elab_syn_ty=rewritten_info.elab_syn_ty,
        ~marks=rewritten_info.marks,
        ~co_ctx=rewritten_info.co_ctx,
        m,
      );
    | Let(p, def, body) =>
      let is_recursive = (ctx, p, def, syn: Typ.t) => {
        switch (Pat.get_num_of_vars(p), Exp.get_num_of_functions(def)) {
        | (Some(num_vars), Some(num_fns))
            when num_vars != 0 && num_vars == num_fns =>
          let norm = Typ.normalize(ctx, syn);
          switch (norm |> Typ.term_of) {
          | Prod(syns) when List.length(syns) == num_vars =>
            syns |> List.for_all(Typ.is_arrow_like)
          | _ when Typ.is_arrow_like(norm) => num_vars == 1
          | _ => false
          };
        | _ => false
        };
      };
      /* Save module items and RHS variable name before def is shadowed */
      let module_items =
        switch (def.term) {
        | Module(items) => Some(items)
        | _ => None
        };
      let def_rhs_var =
        switch (def.term) {
        | Var(v) => Some(v)
        | Constructor(v, _) when Ctx.lookup_var(ctx, v) != None => Some(v)
        | _ => None
        };
      let (p_syn, _, _) =
        go_pat(~is_synswitch=true, ~co_ctx=CoCtx.empty, ~ana=syn, p, m);
      let (def_term, def_rewrap) = Exp.unwrap(def);
      let def =
        switch (def_term, Typ.term_of(Typ.normalize(ctx, p_syn.ty))) {
        | (Tuple(ds), Prod(tys)) =>
          Tuple(
            LabeledTuple.rearrange(
              Typ.match_tup_label, DHExp.match_tup_label, tys, ds, (t, b) =>
              TupLabel(Label(t) |> Exp.fresh, b) |> Exp.fresh
            ),
          )
          |> def_rewrap
        | (_, _) => def
        };
      let (def_rec_probe, _, _) = go(~ctx=p_syn.ctx, ~ana=p_syn.ty, def, m);
      let rec_check_ty =
        switch (Typ.term_of(Typ.normalize(ctx, p_syn.ty))) {
        | Unknown(SynSwitch) => def_rec_probe.ty
        | _ => p_syn.ty
        };
      let is_rec = is_recursive(ctx, p, def, rec_check_ty);
      let (def, def_elab, p_ana_ctx, m, ty_p_ana) =
        if (!is_rec) {
          let (def, def_elab, m) = go(~ana=p_syn.ty, def, m);
          let ty_p_ana = def.ty;
          let (p_ana', _, _) =
            go_pat(
              ~is_synswitch=false,
              ~co_ctx=CoCtx.empty,
              ~ana=ty_p_ana,
              p,
              m,
            );
          (def, def_elab, p_ana'.ctx, m, ty_p_ana);
        } else {
          let (def_base, _, _) = go(~ctx=p_syn.ctx, ~ana=p_syn.ty, def, m);
          let ty_p_ana = def_base.ty;
          /* Analyze pattern to incorporate def type into ctx */
          let (p_ana', _, _) =
            go_pat(
              ~is_synswitch=false,
              ~co_ctx=CoCtx.empty,
              ~ana=ty_p_ana,
              p,
              m,
            );
          let def_ctx = p_ana'.ctx;
          let (def_base2, _, _) = go(~ctx=def_ctx, ~ana=p_syn.ty, def, m);
          let ana_ty_fn = ((ty_fn1, ty_fn2), ty_p) => {
            Typ.term_of(ty_p) == Unknown(SynSwitch)
            && !Typ.equal(ty_fn1, ty_fn2)
              ? ty_fn1 : ty_p;
          };
          let ana =
            switch (
              (def_base.ty |> Typ.term_of, def_base2.ty |> Typ.term_of),
              p_syn.ty |> Typ.term_of,
            ) {
            | ((Prod(ty_fns1), Prod(ty_fns2)), Prod(ty_ps)) =>
              let tys =
                List.map2(ana_ty_fn, List.combine(ty_fns1, ty_fns2), ty_ps);
              Prod(tys) |> Typ.temp;
            | ((_, _), _) =>
              ana_ty_fn((def_base.ty, def_base2.ty), p_syn.ty)
            };
          let (def, def_elab, m) = go(~ctx=def_ctx, ~ana, def, m);
          (def, def_elab, def_ctx, m, ty_p_ana);
        };
      /* Inject module type exports into body context */
      let p_ana_ctx =
        switch (module_items) {
        | Some(items) =>
          switch (ExpandModule.single_bound_var(p)) {
          | Some(name) =>
            let exports = ExpandModule.collect_type_exports(ctx, items);
            switch (exports) {
            | [] => p_ana_ctx
            | _ =>
              let exports_ty = ExpandModule.build_type_exports_type(exports);
              Ctx.extend_alias(p_ana_ctx, name, Pat.rep_id(p), exports_ty);
            };
          | None => p_ana_ctx
          }
        | None =>
          /* Phase 1b: variable aliasing — propagate TVarEntry from RHS */
          switch (ExpandModule.single_bound_var(p), def_rhs_var) {
          | (Some(name), Some(rhs)) =>
            switch (Ctx.lookup_tvar(ctx, rhs)) {
            | Some(Singleton(exports_ty)) =>
              Ctx.extend_alias(p_ana_ctx, name, Pat.rep_id(p), exports_ty)
            | _ => p_ana_ctx
            }
          | _ => p_ana_ctx
          }
        };
      let (body, body_elab, m) = go(~ctx=p_ana_ctx, ~ana, body, m);
      /* add co_ctx to pattern */
      let (p_ana, p_elab, m) =
        go_pat(~is_synswitch=false, ~co_ctx=body.co_ctx, ~ana=ty_p_ana, p, m);
      let m =
        m
        |> record(~id=here, Binder, p_ana.slice)
        |> record(~id=here, Source, def.slice)
        |> record(~id=here, Through, body.slice);
      let syn_ty_let = body.elab_syn_ty;
      let Coverage.CheckMatrix.{exhaustiveness, _} =
        Coverage.check(
          [Info.pat_constraint(p_ana)],
          Typ.normalize(ctx, p_ana.ty),
        );
      let marks_let =
        switch (exhaustiveness) {
        | Exhaustive => []
        | Inexhaustive(unseen_pattern) => [
            Mark.InexhaustiveMatch(syn_ty_let, [], unseen_pattern),
          ]
        };
      let pat_typ_refs = ModuleHelpers.collect_pat_type_refs(ctx, p);
      let requires_fixf =
        is_rec
        && CoCtx.has_any(
             CoCtx.union([
               def.co_ctx,
               CoCtx.mk(ctx, p_ana_ctx, body.co_ctx),
               pat_typ_refs,
             ]),
             Pat.bound_vars(p),
           );
      let elab_term =
        if (!requires_fixf) {
          let def_elab =
            LabeledTupleHelpers.align_exp_if_needed(ctx, p_syn.ty, def_elab)
            |> Exp.add_name(Pat.get_var(p));
          Let(p_elab, def_elab, body_elab) |> rewrap;
        } else {
          let def_elab =
            LabeledTupleHelpers.align_exp_if_needed(ctx, p_syn.ty, def_elab)
            |> Exp.add_name(Option.map(s => s ++ "+", Pat.get_var(p)));
          let fixf =
            (FixF(p_elab, def_elab, None): Exp.term)
            |> IdTagged.fresh_deterministic(Exp.rep_id(uexp));
          Let(p_elab, fixf, body_elab) |> rewrap;
        };
      add(
        ~elab_term,
        ~elab_syn_ty=syn_ty_let,
        ~marks=marks_let,
        ~co_ctx=
          CoCtx.union([
            def.co_ctx,
            CoCtx.mk(ctx, p_ana.ctx, body.co_ctx),
            pat_typ_refs,
          ]),
        m,
      );
    | Theorem({term: Var(_), _} as p, e1, e2) =>
      let pat_typ_refs = ModuleHelpers.collect_pat_type_refs(ctx, p);
      let (e1', e1_elab, m) = go(~ctx, ~ana=Atom(Bool) |> Typ.temp, e1, m);
      let (p', _, _) =
        go_pat(
          ~is_synswitch=false,
          ~co_ctx=CoCtx.empty,
          ~ana=Typ.fresh(ProofOf(e1)),
          p,
          m,
        );
      let (e2, e2_elab, m) = go(~ctx=p'.ctx, ~ana, e2, m);
      /* add co_ctx to pattern */
      let (p, p_elab, m) =
        go_pat(~is_synswitch=false, ~co_ctx=e2.co_ctx, ~ana=syn, p, m);
      add(
        ~elab_term=Theorem(p_elab, e1_elab, e2_elab) |> rewrap,
        ~elab_syn_ty=e2.elab_syn_ty,
        ~marks=[],
        ~co_ctx=
          CoCtx.union([
            p'.co_ctx,
            e1'.co_ctx,
            CoCtx.mk(ctx, p.ctx, e2.co_ctx),
            pat_typ_refs,
          ]),
        m,
      );
    | Theorem(p, e1, e2) =>
      let pat_typ_refs = ModuleHelpers.collect_pat_type_refs(ctx, p);
      let (_, e1_elab, m) = go(~ctx, ~ana=Atom(Bool) |> Typ.temp, e1, m);
      let (p', _, _) =
        go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~ana=syn, p, m);
      let (e2, e2_elab, m) = go(~ctx=p'.ctx, ~ana, e2, m);
      /* add co_ctx to pattern */
      let (p, p_elab, m) =
        go_pat(~is_synswitch=false, ~co_ctx=e2.co_ctx, ~ana=syn, p, m);
      add(
        ~elab_term=Theorem(p_elab, e1_elab, e2_elab) |> rewrap,
        ~elab_syn_ty=e2.elab_syn_ty,
        ~marks=[BadTheorem(e2.ty)],
        ~co_ctx=
          CoCtx.union([
            p'.co_ctx,
            CoCtx.mk(ctx, p.ctx, e2.co_ctx),
            pat_typ_refs,
          ]),
        m,
      );
    | ProofObject(e) =>
      let (_, e_elab, m) = go(~ctx, ~ana=Atom(Bool) |> Typ.temp, e, m);
      add(
        ~elab_term=ProofObject(e_elab) |> rewrap,
        ~elab_syn_ty=Typ.temp(ProofOf(e)),
        ~marks=[],
        ~co_ctx=CoCtx.empty,
        m,
      ); // TODO[Matt]: do types need coctxs now?
    | FixF(p, e, env) =>
      let (p', _, _) =
        go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~ana, p, m);
      let (e', e_elab, m) = go(~ctx=p'.ctx, ~ana=p'.ty, e, m);
      let (p'', p_elab, m) =
        go_pat(~is_synswitch=false, ~co_ctx=e'.co_ctx, ~ana, p, m);
      let pat_typ_refs = ModuleHelpers.collect_pat_type_refs(ctx, p);
      let elab_term =
        FixF(p_elab, Asc(e_elab, p'.ty) |> Exp.fresh, env) |> rewrap;
      add(
        ~elab_term,
        ~elab_syn_ty=p'.elab_syn_ty,
        ~marks=[],
        ~co_ctx=
          CoCtx.union([CoCtx.mk(ctx, p''.ctx, e'.co_ctx), pat_typ_refs]),
        m,
      );
    | If(e0, e1, e2) =>
      let branch_ids = List.map(Exp.rep_id, [e1, e2]);
      let& (cond, cond_elab, m) = go(~ana=Atom(Bool) |> Typ.temp, e0, m);
      let+ (cons, cons_elab, m) = go(~ana, e1, m);
      let+ (alt, alt_elab, m) = go(~ana, e2, m);
      let (syn_if, cms_if) =
        ConstructorStaticsHelpers.syn_marks_match(
          ctx,
          [cons.elab_syn_ty, alt.elab_syn_ty],
          branch_ids,
        );
      let result_ty =
        fixed_typ(ctx, ana, syn_if) |> Typ.normalize(ctx) |> Typ.all_ids_temp;
      let elab =
        If(
          cond_elab,
          fresh_ascription(ctx, cons_elab, cons.ty, Some(result_ty)),
          fresh_ascription(ctx, alt_elab, alt.ty, Some(result_ty)),
        )
        |> rewrap;
      /* Compute the `elab_syn_ty` that a fresh re-synthesis of the
         elaborated If would produce. Each branch contributes
         `result_ty` iff `fresh_ascription` actually wrapped it (i.e.
         the branch got an outer Asc), otherwise it contributes its
         original raw `elab_syn_ty`. This keeps the recorded type in
         sync with what fresh re-synth yields without altering the
         wrap decision. */
      let branch_fresh_syn = (branch_info: Info.exp) => {
        let wrapped =
          switch (result_ty.term) {
          | Unknown(Internal) => false
          | _ =>
            !
              Typ.fast_equal(
                Typ.normalize(ctx, result_ty),
                Typ.normalize(ctx, branch_info.ty),
              )
          };
        wrapped ? result_ty : branch_info.elab_syn_ty;
      };
      let (elab_syn_ty, _) =
        ConstructorStaticsHelpers.syn_marks_match(
          ctx,
          [branch_fresh_syn(cons), branch_fresh_syn(alt)],
          branch_ids,
        );
      add(
        ~elab_term=elab,
        ~elab_syn_ty,
        ~marks=cms_if,
        ~co_ctx=CoCtx.union([cond.co_ctx, cons.co_ctx, alt.co_ctx]),
        m,
      );
    | Match(scrut, rules) =>
      let$ (scrut, scrut_elab, m) = go(~ana=syn, scrut, m);
      let (ps, es) = List.split(rules);
      let branch_ids = List.map(Exp.rep_id, es);
      let (ps', _) =
        map_m(
          (p, m) => {
            let (info, _, m) =
              go_pat(
                ~is_synswitch=false,
                ~co_ctx=CoCtx.empty,
                ~ana=scrut.ty,
                p,
                m,
              );
            (info, m);
          },
          ps,
          m,
        );

      let p_ctxs = List.map(Info.pat_ctx, ps');
      let (es, es_elabs, m) =
        List.fold_left2(
          ((es, elabs, m), e, ctx) =>
            go(~ctx, ~ana, e, m)
            |> (((e, elab, m)) => (es @ [e], elabs @ [elab], m)),
          ([], [], m),
          es,
          p_ctxs,
        );

      let e_syn_tys = List.map((e: Info.exp) => e.elab_syn_ty, es);
      let e_co_ctxs = List.map(Info.exp_co_ctx, es);
      let (syn_ty_match, marks_match) =
        ConstructorStaticsHelpers.syn_marks_match(ctx, e_syn_tys, branch_ids);
      let (constraints, ps_elabs, m) =
        List.fold_left(
          (
            (
              constraints: list(Coverage.Constraint.t),
              ps_elabs: list(Pat.t),
              m: Map.t,
            ),
            ((p, co_ctx), e: Info.exp),
          ) => {
            let (info, p_elab, m) =
              go_pat(~is_synswitch=false, ~co_ctx, ~ana=scrut.ty, p, m);
            let m =
              m
              |> record(~id=here, Binder, info.slice)
              |> record(~id=here, Alternative, e.slice);
            let p_constraint = Info.pat_constraint(info);
            ([p_constraint, ...constraints], ps_elabs @ [p_elab], m);
          },
          ([], [], m),
          List.combine(List.combine(ps, e_co_ctxs), es),
        );

      let constraints = List.rev(constraints);

      let normalized_scrut_ty = Typ.normalize(ctx, scrut.ty);
      let Coverage.CheckMatrix.{exhaustiveness, redundant_rows} =
        Coverage.check(constraints, normalized_scrut_ty);

      let marks_match' =
        switch (exhaustiveness) {
        | Exhaustive => marks_match
        | Inexhaustive(unseen_pattern) => [
            Mark.InexhaustiveMatch(syn_ty_match, marks_match, unseen_pattern),
          ]
        };
      let add_pattern_redundancy =
          (ps: list(Pat.t), redundant_rows: list(int), m: Map.t): Map.t =>
        List.fold_left(
          (m, row) => {
            let p = List.nth(ps, row);
            switch (Id.Map.find(IdTagged.rep_id(p), m)) {
            | Info.InfoPat(info) =>
              let info =
                prepend_pat_mark(info, Mark.Redundant, ~warnings=[], ());
              add_info(IdTagged.ids(p), InfoPat(info), m);
            | _ => failwith("Invalid sort for pattern.")
            };
          },
          m,
          redundant_rows,
        );
      let m = add_pattern_redundancy(ps, redundant_rows, m);
      let co_ctx =
        CoCtx.union([
          scrut.co_ctx,
          ...List.map2(CoCtx.mk(ctx), p_ctxs, e_co_ctxs),
        ]);
      /* Build elaboration with ascriptions on branch bodies */
      let result_ty =
        fixed_typ(ctx, ana, syn_ty_match)
        |> Typ.normalize(ctx)
        |> Typ.all_ids_temp;
      let e_tys = List.map(Info.exp_ty, es);
      let es_elabs =
        List.map2(
          (e_elab, ty) =>
            fresh_ascription(ctx, e_elab, ty, Some(result_ty)),
          es_elabs,
          e_tys,
        );
      let elab_term =
        Match(scrut_elab, List.combine(ps_elabs, es_elabs)) |> rewrap;
      /* Compute the `elab_syn_ty` that a fresh re-synthesis of the
         elaborated Match would produce. See analogous comment on If. */
      let branch_fresh_syn = (e: Info.exp) => {
        let wrapped =
          switch (result_ty.term) {
          | Unknown(Internal) => false
          | _ =>
            !
              Typ.fast_equal(
                Typ.normalize(ctx, result_ty),
                Typ.normalize(ctx, e.ty),
              )
          };
        wrapped ? result_ty : e.elab_syn_ty;
      };
      let (elab_syn_ty, _) =
        ConstructorStaticsHelpers.syn_marks_match(
          ctx,
          List.map(branch_fresh_syn, es),
          branch_ids,
        );
      add(~elab_term, ~elab_syn_ty, ~marks=marks_match', ~co_ctx, m);
    | TyAlias(typat, utyp, body) =>
      /* Desugar Sig types so type aliases like `type T = {let x : Int}`
         store `Prod([TupLabel(...)])` rather than `Sig([...])` in the
         context (so meet/join can unify them with module expression
         types). */
      let utyp_desugared = Typ.desugar_sig(ctx, utyp);
      /* `type T = typfun a, b -> body` is the prefix-binder spelling
         of `type T(a, b) = body` — a single multi-binder `TypFun`.
         Peel it so the alias takes the `Param`-branch path
         (params extension, polymorphic constructor schemas,
         tuple-arrow kind `(Type, …) -> Type`).

         A *curried* form `type T = typfun a -> typfun b -> body`
         is left intact: each unary `TypFun` stays as its own
         binder so the alias has the curried kind
         `Type -> Type -> kind(body)` and accepts curried
         applications `T(a)(b)`. `peel_typlams` refuses to peel a
         TypFun whose body is itself a TypFun. */
      let rec strip_parens = (t: Typ.t): Typ.t =>
        switch (t.term) {
        | Parens(inner) => strip_parens(inner)
        | Unknown(_)
        | Atom(_)
        | DrvQuoteTy(_)
        | Label(_)
        | ExplicitNonlabel
        | Var(_)
        | Projector(_)
        | ProdProjection(_)
        | List(_)
        | Arrow(_)
        | TupLabel(_)
        | ProdExtension(_)
        | Prod(_)
        | Sum(_)
        | Poly(_)
        | ProofOf(_)
        | Sig(_)
        | TypTuple(_)
        | TypFun(_)
        | TypParamAp(_)
        | Rec(_) => t
        };
      let peel_typlams = (typ: Typ.t): (list(TPat.t), Typ.t) => {
        let stripped = strip_parens(typ);
        switch (stripped.term) {
        | TypFun(p, inner) =>
          let stripped_inner = strip_parens(inner);
          switch (stripped_inner.term) {
          | TypFun(_) => ([], typ)
          | Unknown(_)
          | Atom(_)
          | DrvQuoteTy(_)
          | Label(_)
          | ExplicitNonlabel
          | Var(_)
          | Parens(_)
          | Projector(_)
          | ProdProjection(_)
          | List(_)
          | Arrow(_)
          | TupLabel(_)
          | ProdExtension(_)
          | Prod(_)
          | Sum(_)
          | Poly(_)
          | ProofOf(_)
          | Sig(_)
          | TypTuple(_)
          | TypParamAp(_)
          | Rec(_) => (TPat.binders_of(p), stripped_inner)
          };
        | Unknown(_)
        | Atom(_)
        | DrvQuoteTy(_)
        | Label(_)
        | ExplicitNonlabel
        | Var(_)
        | Parens(_)
        | Projector(_)
        | ProdProjection(_)
        | List(_)
        | Arrow(_)
        | TupLabel(_)
        | ProdExtension(_)
        | Prod(_)
        | Sum(_)
        | Poly(_)
        | ProofOf(_)
        | Sig(_)
        | TypTuple(_)
        | TypParamAp(_)
        | Rec(_) => ([], typ)
        };
      };
      /* If the typat is a bare `Var(name)` and the body has a
         peelable single multi-binder `TypFun`, rebundle as
         `Param(head, params)` so the rest of the branch treats this
         identically to the explicit `type Name(params) = body`
         spelling. The new `Param` reuses the original `Var` tile id
         for its head so jump-to-definition lands on the alias
         name. */
      let (typat, utyp, utyp_desugared) =
        switch (typat.term, peel_typlams(utyp_desugared)) {
        | (Var(name), ([_, ..._] as params, inner_body)) =>
          let head: TPat.t = {
            term: Var(name),
            annotation: typat.annotation,
          };
          let new_typat: TPat.t = {
            term: Param(head, params),
            annotation: typat.annotation,
          };
          let (_, inner_orig) = peel_typlams(utyp);
          (new_typat, inner_orig, inner_body);
        /* No rebundle: either typat isn't a `Var` (already a
           `Param`/`Tuple`/etc.), or `peel_typlams` returned no
           binders (body isn't a peelable `TypFun`). */
        | (Var(_), ([], _))
        | (
            Param(_, _) | Tuple(_) | Parens(_) | Invalid(_) | EmptyHole |
            MultiHole(_),
            _,
          ) => (
            typat,
            utyp,
            utyp_desugared,
          )
        };
      /* Single source of truth for the alias's kind: stored on the
         `TVarEntry`, surfaced at the alias-name tpat through
         `~alias_kind`, and consumed by every downstream
         `kind_of_typ` lookup of a `Var(name)` reference to this
         alias. Mirrors the kind `kind_of_typ` derives from the
         alias body — `(Type, …) -> kind(body)` for the rebundled
         multi-binder case (with the params bound abstractly in the
         body's context), or just `kind_of_typ(body)` otherwise (a
         curried `typfun a -> typfun b -> Sum(...)` body yields
         `Type -> Type -> Type`). */
      let alias_body_kind =
        switch (typat.term) {
        | Param(_, params) =>
          let n = List.length(params);
          let body_ctx =
            List.fold_left(
              (ctx, p) =>
                switch (TPat.tyvar_of_utpat(p)) {
                | Some(pname) =>
                  Ctx.extend_tvar(
                    ctx,
                    {
                      name: pname,
                      id: TPat.rep_id(p),
                      kind: Abstract,
                      typ_kind: TypKind.Type,
                    },
                  )
                | None => ctx
                },
              ctx,
              params,
            );
          TypKind.arrows(
            List.init(n, _ => TypKind.Type),
            kind_of_typ(body_ctx, utyp_desugared),
          );
        | Var(_)
        | Tuple(_)
        | Parens(_)
        | Invalid(_)
        | EmptyHole
        | MultiHole(_) => kind_of_typ(ctx, utyp_desugared)
        };
      /* Visit the (possibly rebundled) typat *after* peeling so the
         alias-head info entry sees the `Param`/`Var` shape
         post-rebundle and the computed kind. */
      let m =
        utpat_to_info_map(
          ~at_alias_head=true,
          ~alias_kind=alias_body_kind,
          ~ctx,
          ~ancestors=ancestors_inclusive,
          typat,
          m,
        )
        |> snd;
      switch (typat.term) {
      | Param(head, params)
          when
            switch (TPat.head_name_of(head)) {
            | Some(name) => !Ctx.is_base_typ(name)
            | None => false
            } =>
        let name = TPat.head_name_of(head) |> Option.get;
        /* Use the head tile's id (not the Param's rep_id, which
           points at the postfix application tile) as the binding
           site so jump-to-definition lands on the alias name. */
        let binding_id = TPat.rep_id(head);
        let extend_param_ctx = ctx =>
          List.fold_left(
            (ctx, param) =>
              switch (TPat.tyvar_of_utpat(param)) {
              | Some(param_name) =>
                Ctx.extend_tvar(
                  ctx,
                  {
                    name: param_name,
                    id: TPat.rep_id(param),
                    kind: Abstract,
                    typ_kind: TypKind.Type,
                  },
                )
              | None => ctx
              },
            ctx,
            params,
          );
        /* Single uncurried `TypFun(TPat.Tuple([a, b, …]), body)`
           for the multi-binder case (no curried `TypFun(a, TypFun(b,
           body))` chain). The `TypParamAp` reduction zips a
           `TypTuple` argument against the tuple binder element-wise
           in one substitution step. */
        let ty_lam: Typ.t =
          switch (params) {
          | [] => utyp_desugared
          | [single] => TypFun(single, utyp_desugared) |> Typ.temp
          | _ =>
            let tuple_binder: TPat.t =
              (Tuple(params): TPat.term) |> IdTagged.fresh;
            TypFun(tuple_binder, utyp_desugared) |> Typ.temp;
          };
        let (ty_def, ctx_def, ctx_body) =
          if (List.mem(name, Typ.free_vars(utyp_desugared))) {
            let ty_rec = Rec(Var(name) |> TPat.fresh, ty_lam) |> Typ.temp;
            let ctx_def =
              Ctx.extend_alias(
                ctx,
                name,
                binding_id,
                ~typ_kind=alias_body_kind,
                ty_rec,
              );
            (ty_rec, ctx_def, ctx_def);
          } else {
            let ctx_body =
              Ctx.extend_alias(
                ctx,
                name,
                binding_id,
                ~typ_kind=alias_body_kind,
                ty_lam,
              );
            (ty_lam, ctx, ctx_body);
          };
        let ctx_for_def = extend_param_ctx(ctx_def);
        let ctx_body =
          switch (Typ.get_sum_constructors(ctx_for_def, utyp_desugared)) {
          | Some(sm) => Ctx.add_ctrs_with_params(ctx_body, name, params, sm)
          | None => ctx_body
          };
        let (
          {co_ctx, elab_syn_ty: ty_body, slice: body_slice, _}: Info.exp,
          body_elab,
          m,
        ) =
          go(~ctx=ctx_body, ~ana, body, m);
        let ty_escape = Typ.subst(ty_def, Var(name) |> TPat.temp, ty_body);
        let (def_info, m) =
          utyp_to_info_map(
            ~ctx=ctx_for_def,
            ~ancestors=ancestors_inclusive,
            ~expects=AnyKindExpected,
            utyp,
            m,
          );
        let m =
          m
          |> record(
               ~id=here,
               Binder,
               Slice.binding(
                 ~sort=Alias,
                 ~name,
                 ~id=TPat.rep_id(typat),
                 ~ids=Id.Set.of_list(IdTagged.ids(typat)),
               ),
             )
          |> record(~id=here, Source, def_info.slice)
          |> record(~id=here, Through, body_slice);
        let typ_refs =
          ModuleHelpers.collect_module_refs_in_typ(
            ctx,
            Typ.rep_id(utyp),
            utyp,
          );
        add(
          ~elab_term=body_elab,
          ~elab_syn_ty=ty_escape,
          ~marks=[],
          ~co_ctx=CoCtx.union([co_ctx, typ_refs]),
          m,
        );
      | Var(name) when !Ctx.is_base_typ(name) =>
        /* Tentatively add the alias as an abstract type when its
           body mentions itself, so `free_vars` can detect the
           recursive case before we commit to a `Rec(...)` wrapper.
           `~typ_kind=alias_body_kind` keeps the alias's kind in
           context aligned with what the cursor inspector showed at
           the tpat node. */
        let (ty_def, ctx_def, ctx_body) =
          if (List.mem(name, Typ.free_vars(utyp_desugared))) {
            let ty_rec =
              Rec(Var(name) |> TPat.fresh, utyp_desugared) |> Typ.temp;
            let ctx_def =
              Ctx.extend_alias(
                ctx,
                name,
                TPat.rep_id(typat),
                ~typ_kind=alias_body_kind,
                ty_rec,
              );
            (ty_rec, ctx_def, ctx_def);
          } else {
            (
              utyp_desugared,
              ctx,
              Ctx.extend_alias(
                ctx,
                name,
                TPat.rep_id(typat),
                ~typ_kind=alias_body_kind,
                utyp_desugared,
              ),
            );
          };
        let ctx_body =
          /* Only register new constructors when the alias RHS literally
             defines a sum (e.g. `type T = + A + B`). For an alias whose
             RHS is a reference to an existing parameterized constructor
             (e.g. `type IntOption = Option(Int)`), reusing `add_ctrs`
             would shadow the polymorphic schemas of Some/None with
             monomorphic ones, breaking implicit instantiation. */
          switch (utyp_desugared.term) {
          | Sum(_) =>
            switch (Typ.get_sum_constructors(ctx, ty_def)) {
            | Some(sm) => Ctx.add_ctrs(ctx_body, name, sm)
            | None => ctx_body
            }
          | _ => ctx_body
          };
        let (
          {co_ctx, elab_syn_ty: ty_body, slice: body_slice, _}: Info.exp,
          body_elab,
          m,
        ) =
          go(~ctx=ctx_body, ~ana, body, m);
        let ty_escape = Typ.subst(ty_def, typat, ty_body);
        let (def_info, m) =
          utyp_to_info_map(
            ~ctx=ctx_def,
            ~ancestors=ancestors_inclusive,
            ~expects=AnyKindExpected,
            utyp,
            m,
          );
        let m =
          m
          |> record(
               ~id=here,
               Binder,
               Slice.binding(
                 ~sort=Alias,
                 ~name,
                 ~id=TPat.rep_id(typat),
                 ~ids=Id.Set.of_list(IdTagged.ids(typat)),
               ),
             )
          |> record(~id=here, Source, def_info.slice)
          |> record(~id=here, Through, body_slice);
        let typ_refs =
          ModuleHelpers.collect_module_refs_in_typ(
            ctx,
            Typ.rep_id(utyp),
            utyp,
          );
        add(
          ~elab_term=body_elab,
          ~elab_syn_ty=ty_escape,
          ~marks=[],
          ~co_ctx=CoCtx.union([co_ctx, typ_refs]),
          m,
        );
      | Var(_)
      | Param(_)
      | Tuple(_)
      | Parens(_)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_) =>
        let ({co_ctx, elab_syn_ty: ty_body, _}: Info.exp, body_elab, m) =
          go(~ctx, ~ana, body, m);
        let m =
          utyp_to_info_map(
            ~ctx,
            ~ancestors=ancestors_inclusive,
            ~expects=AnyKindExpected,
            utyp,
            m,
          )
          |> snd;
        let typ_refs =
          ModuleHelpers.collect_module_refs_in_typ(
            ctx,
            Typ.rep_id(utyp),
            utyp,
          );
        add(
          ~elab_term=body_elab,
          ~elab_syn_ty=ty_body,
          ~marks=[],
          ~co_ctx=CoCtx.union([co_ctx, typ_refs]),
          m,
        );
      };
    | Use(typ, body) =>
      let (typ, m) =
        utyp_to_info_map(~ctx, ~ancestors=ancestors_inclusive, typ, m);
      let use_mode: option(Operators.mode) =
        switch (typ.user_term |> Typ.weak_head_normalize(ctx) |> Typ.term_of) {
        | Atom(Nat) => Some(Nat)
        | Atom(Int) => Some(Int)
        | Atom(Float) => Some(Float)
        | Atom(SInt) => Some(SInt)
        | _ => None
        };
      let ctx' =
        switch (use_mode) {
        | Some(mode) => Ctx.set_use_mode(ctx, Some(mode))
        | None => ctx
        };
      let (body, body_elab, m) = go(~ctx=ctx', ~ana, body, m);
      switch (use_mode) {
      | Some(_) =>
        add(
          ~elab_term=body_elab,
          ~elab_syn_ty=body.elab_syn_ty,
          ~marks=[],
          ~co_ctx=body.co_ctx,
          m,
        )
      | None
          when Typ.fast_equal(Unknown(Internal) |> Typ.temp, typ.user_term) =>
        add(
          ~elab_term=body_elab,
          ~elab_syn_ty=body.elab_syn_ty,
          ~marks=[],
          ~co_ctx=body.co_ctx,
          m,
        )
      | None =>
        add(
          ~elab_term=body_elab,
          ~elab_syn_ty=body.elab_syn_ty,
          ~marks=[
            InvalidUseMode({
              bad_typ: typ.user_term,
              inner_typ: body.ty,
            }),
          ],
          ~co_ctx=body.co_ctx,
          m,
        )
      };
    | Module(items) =>
      /* Expand module to nested let/type + labeled tuple, then type-check expansion.
         The expansion preserves Mod item IDs on wrapper Let/TyAlias expressions.
         Pass ~ana to expand so it can add sig type annotations to patterns.
         Process expansion in syn mode: definition errors are caught via pattern
         annotations, and the Module's own add() checks the overall type against
         ana. Using ~ana here would double-count type inconsistencies (once on
         the expansion's inner tuple, once on the Module expression). */
      let expanded = ExpandModule.expand(~ana, items);
      let (expanded_info, expanded_elab, m) = go(expanded, m);
      let m = ModuleHelpers.reclassify_expanded_module_items(items, m);
      /* Build actual Prod type from module's exported bindings, rather than
         using expanded_info.ty which masks width errors via fixed_typ. */
      let actual_ty = ModuleHelpers.module_actual_type(items, m);
      let module_elab =
        ModuleHelpers.module_elab(
          ~module_exp_id=Exp.rep_id(uexp),
          expanded_elab,
        );
      add(
        ~elab_term=module_elab,
        ~elab_syn_ty=actual_ty,
        ~marks=[],
        ~co_ctx=expanded_info.co_ctx,
        m,
      );
    | ModuleExp(mp, def, body) =>
      /* Expand module M = def in body → let M = def in body.
         Process the MPat for cursor info, then expand to Let and type-check. */
      let (_, _, m) =
        any_to_info_map(~ctx, ~ancestors=ancestors_inclusive, MPat(mp), m);
      let pat = ExpandModule.mpat_to_pat(mp);
      let expanded =
        IdTagged.fast_copy(
          Exp.rep_id(uexp),
          Exp.fresh(Let(pat, def, body)),
        );
      let (expanded_info, expanded_elab, m) = go(~ana, expanded, m);
      /* Override cls to show "Module binding" */
      let m =
        switch (Id.Map.find_opt(Exp.rep_id(uexp), m)) {
        | Some(Info.InfoExp(info)) =>
          add_info(
            ids,
            Info.InfoExp({
              ...info,
              cls: Exp(ModuleExp),
            }),
            m,
          )
        | _ => m
        };
      let def_ana =
        switch (pat.term) {
        | Asc(_, typ) => typ
        | _ => syn
        };
      let (_, def_elab_direct, m) = go(~ana=def_ana, def, m);
      let moduleexp_elab =
        ModuleHelpers.moduleexp_elab(~def_elab_direct, expanded_elab);
      add(
        ~elab_term=moduleexp_elab,
        ~elab_syn_ty=expanded_info.elab_syn_ty,
        ~marks=[],
        ~co_ctx=expanded_info.co_ctx,
        m,
      );
    };
  };

  // This is for lifting single values into a singleton labeled tuple when the label is not present

  switch (Typ.weak_head_normalize(ctx, ana).term) {
  | Prod([{term: TupLabel({term: Label(l1), _}, ana_ty), _}]) =>
    // We can flatten this by pulling it up on the case match but since OCaml is strict it'll be evaluated.
    // So for performance reasons we'll just do it here.
    let (e, _, m) = go(~ana=syn, uexp, m);

    switch (Typ.weak_head_normalize(ctx, e.ty).term) {
    | Prod([{term: TupLabel({term: Label(l2), _}, _), _}]) when l1 == l2 =>
      default_case()
    | Unknown(_) => default_case() // TODO I don't know if this is correct
    | _ => autolabel_singleton_tuple(uexp, ana_ty, l1, m)
    };
  | _ => default_case()
  };
}
and upat_to_info_map =
    (
      ~is_synswitch,
      ~ctx,
      // the co-ctx of the pattern's scope
      ~co_ctx,
      ~ancestors: Info.ancestors,
      ~duplicate_bindings: list(string)=[],
      ~ana: Typ.t=Unknown(Internal) |> Typ.temp,
      ~under_ascription: bool=false,
      upat: Pat.t,
      m: Map.t,
    )
    : (Info.pat, Pat.t, Map.t) => {
  let ids = IdTagged.ids(upat);
  let (term, rewrap) = Pat.unwrap(upat);
  let ancestors_inclusive = [Pat.rep_id(upat)] @ ancestors;
  let add =
      (
        ~user_term: Pat.t=upat,
        ~elab_term: Pat.t=user_term,
        ~ctx=ctx,
        ~co_ctx=co_ctx,
        ~ana=ana,
        ~ancestors=ancestors_inclusive,
        ~elab_syn_ty: Typ.t,
        ~marks: list(Mark.t)=[],
        ~warnings: list(Warning.list_item)=[],
        ~constraint_: Coverage.Constraint.t,
        ~label_inference: option(Info.label_inference(Info.pat))=None,
        ~inferred_label: option(LabeledTuple.label)=None,
        ~label_sort=false,
        ~binds: list((Slice.sort, string, Id.t))=[],
        ~uses: list(Slice.use)=[],
        ~slice: option(Slice.t)=None,
        m: Id.Map.t(Info.t),
      )
      : (Info.pat, Pat.t, Map.t) => {
    let marks =
      if (marks != []) {
        marks;
      } else {
        switch (expectation_mismatch_mark(ctx, ana, elab_syn_ty)) {
        | None => marks
        | Some(m) => marks @ [m]
        };
      };
    let message =
      marks != []
        ? Message.Pat(Message.Default)
        : Message.Pat(
            switch (ana) {
            | {term: Unknown(SynSwitch), _} => Message.Default
            | _ => Message.Common(syn_ana_ok_common(ctx, ana, elab_syn_ty))
            },
          );
    let cls = Cls.Pat(Pat.cls_of_term(user_term.term));
    let ty = fixed_typ(ctx, ana, elab_syn_ty);
    let warning_acc =
      warnings
      @ (
        switch (user_term.term) {
        | Var(name) => Warning.to_list(Warning.var_is_unused(co_ctx, name))
        | _ => []
        }
      );
    let constraint_': Coverage.Constraint.t =
      switch (constraint_, marks != []) {
      | (Coverage.Constraint.Hole(_), _) => constraint_
      | (_, true) => Hole(Some(constraint_))
      | (_, false) => constraint_
      };
    let (children, m) = take_children(~id=Pat.rep_id(user_term), m);
    let slice =
      Slice.mk(
        ~ctx,
        ~id=Pat.rep_id(user_term),
        ~ids=Slice.pat_ids(user_term),
        ~shape=ty,
        ~components=children,
        ~uses,
        ~binds,
        ~binder=true,
        ~override=slice,
        (),
      );
    let info: Info.pat = {
      cls,
      elab_syn_ty,
      marks,
      ana,
      ty,
      message,
      warnings: warning_acc,
      ctx,
      co_ctx,
      ancestors,
      user_term,
      elab_term,
      constraint_: constraint_',
      label_inference,
      inferred_label,
      label_sort,
      slice,
    };
    (info, elab_term, add_info(IdTagged.ids(user_term), InfoPat(info), m));
  };
  let ancestors = (); // Deliberately shadowed so there's no risk of using it by mistake
  let _ = ancestors;
  let go =
      (
        ~is_synswitch=is_synswitch,
        ~ctx=ctx,
        ~co_ctx=co_ctx,
        ~duplicate_bindings=[],
        ~ana=ana,
        ~under_ascription=false,
        upat: Pat.t,
        m: Map.t,
      ) => {
    upat_to_info_map(
      ~is_synswitch,
      ~ctx,
      ~co_ctx,
      ~ancestors=ancestors_inclusive,
      ~duplicate_bindings,
      ~ana,
      ~under_ascription,
      upat,
      m: Map.t,
    );
  };
  let here = Pat.rep_id(upat);
  let pat_edge = role => edge(~at=here, role, (i: Info.pat) => i.slice);
  let typ_edge = role => edge_typ(~at=here, role, (i: Info.typ) => i.slice);
  // use when the sub-pattern's type is this pattern's whole type: `(p)`
  let (let^) = (component, k) => pat_edge(Through, component, k);
  // use when an annotation supplies this pattern's whole type: `(p : Int)`'s `Int`
  let (let^^) = (component, k) => typ_edge(Through, component, k);
  // use when the sub-pattern's type becomes an argument of the type constructor
  // this pattern applies: in `hd :: tl` the head's `Int` is the argument of
  // `[_]`. Unused: the pattern rules that do this fold over their sub-patterns,
  // so they record them in the fold.
  // let ( let* ) = (component, k) => pat_edge(Part, component, k);
  // use for any sub-pattern that is only type checked.
  // let (let&) = (component, k) => pat_edge(Omit, component, k);
  let unknown = Unknown(is_synswitch ? SynSwitch : Internal) |> Typ.temp;

  let elaborate_singleton_tuple = (upat: Pat.t, inner_ty, l, m) =>
    LabeledTupleHelpers.autolabel_singleton_pat(
      ~analyze_original=
        (~ana, pat, m) =>
          upat_to_info_map(
            ~ctx,
            ~co_ctx,
            ~is_synswitch,
            ~ancestors=ancestors_inclusive,
            ~ana,
            pat,
            m,
          ),
      ~analyze_elaborated=
        (~ana, pat, m) =>
          upat_to_info_map(
            ~ctx,
            ~co_ctx,
            ~is_synswitch,
            ~ancestors=ancestors_inclusive,
            ~ana,
            pat,
            m,
          ),
      ~store_info=
        (elaborated_pat, info, m) =>
          add_info(IdTagged.ids(elaborated_pat), InfoPat(info), m),
      upat,
      ~inner_ty,
      ~ana,
      ~label=l,
      m,
    );

  let default_case = () =>
    switch (term) {
    | MultiHole(tms) =>
      let (_, _, m) = multi(~ctx, ~ancestors=ancestors_inclusive, m, tms);
      add(
        ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[IsMulti],
        ~ctx,
        ~constraint_=Coverage.Constraint.Hole(None),
        m,
      );
    | Invalid(token) =>
      add(
        ~elab_syn_ty=SynTy.unknown_internal(),
        ~marks=[BadToken(token)],
        ~ctx,
        ~constraint_=Coverage.Constraint.Hole(None),
        m,
      )
    | EmptyHole =>
      add(
        ~elab_syn_ty=unknown,
        ~marks=[],
        ~ctx,
        ~constraint_=Coverage.Constraint.Hole(None),
        m,
      )
    | Atom(c) =>
      let c =
        Operators.replace_literal(c, Typ.is_ana_atom(ana), ctx.use_mode); // Replace literal if necessary due to `use`
      switch (c) {
      | L(Nat(nat)) =>
        add(
          ~elab_term=Atom(Nat(nat)) |> rewrap,
          ~elab_syn_ty=Atom(Nat) |> Typ.temp,
          ~marks=[],
          ~ctx,
          ~constraint_=Coverage.Constraint.BigInt(nat),
          m,
        )
      | L(Int(int)) =>
        add(
          ~elab_term=Atom(Int(int)) |> rewrap,
          ~elab_syn_ty=Atom(Int) |> Typ.temp,
          ~marks=[],
          ~ctx,
          ~constraint_=Coverage.Constraint.BigInt(int),
          m,
        )
      | L(SInt(int)) =>
        add(
          ~elab_term=Atom(SInt(int)) |> rewrap,
          ~elab_syn_ty=Atom(SInt) |> Typ.temp,
          ~marks=[],
          ~ctx,
          ~constraint_=Coverage.Constraint.SInt(int),
          m,
        )
      | L(Float(float)) =>
        add(
          ~elab_term=Atom(Float(float)) |> rewrap,
          ~elab_syn_ty=Atom(Float) |> Typ.temp,
          ~marks=[],
          ~ctx,
          ~constraint_=Coverage.Constraint.Float(float),
          m,
        )
      | L(Bool(bool)) =>
        add(
          ~elab_term=Atom(Bool(bool)) |> rewrap,
          ~elab_syn_ty=Atom(Bool) |> Typ.temp,
          ~marks=[],
          ~ctx,
          ~constraint_=
            bool ? Coverage.Constraint.true_ : Coverage.Constraint.false_,
          m,
        )
      | L(String(string)) =>
        add(
          ~elab_term=Atom(String(string)) |> rewrap,
          ~elab_syn_ty=Atom(String) |> Typ.temp,
          ~marks=[],
          ~ctx,
          ~constraint_=Coverage.Constraint.String(string),
          m,
        )
      | R(BadInt(str)) =>
        add(
          ~elab_term=Invalid(str) |> rewrap,
          ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
          ~marks=[BadToken(str)],
          ~ctx,
          ~constraint_=Coverage.Constraint.Hole(None),
          m,
        )
      };
    | ListLit(ps) =>
      let list_constraint =
          (cons: list(Coverage.Constraint.t)): Coverage.Constraint.t =>
        List.fold_right(
          (hd, tl) => Coverage.Constraint.cons(hd, tl),
          cons,
          Coverage.Constraint.nil,
        );
      let ids = List.map(Pat.rep_id, ps);
      let mode = MatchedTyp.tolerant1(MatchedTyp.list, ctx, ana);
      let modes = List.init(List.length(ps), _ => mode);
      /* First pass: analyze each element with the initial mode, so sibling
         elements can contribute to the refined element type via meet. We
         discard the intermediate info map and only use the synthesized types
         to compute the refined mode. */
      let (_, tys_first, _, _, _, _) =
        fold_patterns_with_modes(
          ~analyze=
            (~ctx, ~ana, ~duplicate_bindings, p, m) =>
              go(~ctx, ~ana, ~duplicate_bindings, p, m),
          ~ctx,
          ps,
          modes,
          m,
        );
      /* Second pass: re-analyze each element against the refined element
         type so that pattern-variable bindings in sibling positions (e.g.
         `x` in `[false, x]`) pick up the refined type in their context.
         See also: the `Let` case above, which performs a similar re-analysis
         after the def's type is known. */
      let refined_mode =
        switch (Typ.meet_all(~empty=unknown, ctx, tys_first)) {
        | Some(ty) => ty
        | None => mode
        };
      let refined_modes = List.init(List.length(ps), _ => refined_mode);
      let (ctx, tys, cons, m, infos, ps_elabs) =
        fold_patterns_with_modes(
          ~analyze=
            (~ctx, ~ana, ~duplicate_bindings, p, m) =>
              go(~ctx, ~ana, ~duplicate_bindings, p, m),
          ~ctx,
          ps,
          refined_modes,
          m,
        );
      let syn_tys = List.map((info: Info.pat) => info.elab_syn_ty, infos);
      switch (Typ.meet_all(~empty=unknown, ctx, syn_tys)) {
      | None =>
        let syn_no_meet = SynTy.meet_of(List, Unknown(Internal) |> Typ.temp);
        add(
          ~elab_term=ListLit(ps_elabs) |> rewrap,
          ~elab_syn_ty=syn_no_meet,
          ~marks=
            should_emit_nomeet_mark(ctx, ana, syn_no_meet)
              ? [NoMeet(List, Typ.add_source(ids, tys))] : [],
          ~ctx,
          ~constraint_=list_constraint(cons),
          m,
        );
      | Some(ty) =>
        add(
          ~elab_term=ListLit(ps_elabs) |> rewrap,
          ~elab_syn_ty=List(ty) |> Typ.temp,
          ~marks=[],
          ~ctx,
          ~constraint_=list_constraint(cons),
          m,
        )
      };
    | Cons(hd, tl) =>
      let inner_ty = MatchedTyp.tolerant1(MatchedTyp.list, ctx, ana);
      /* First pass: determine the head's synthesized type so we can refine
         the element type used to analyze both the head and tail in pass two. */
      let (hd_first, _, _) = go(~ctx, ~ana=inner_ty, hd, m);
      let refined_inner =
        switch (Typ.meet(ctx, inner_ty, hd_first.ty)) {
        | Some(ty) => ty
        | None => inner_ty
        };
      /* Second pass: re-analyze with the refined element type so that
         pattern-variable bindings (e.g. `x` in `0 :: x` giving `x : [Int]`)
         pick up the refined type in their context. Mirrors the re-analysis
         performed for `Let` patterns once the def's type is known. */
      let (hd, hd_elab, m) = go(~ctx, ~ana=refined_inner, hd, m);
      let (tl, tl_elab, m) =
        go(~ctx=hd.ctx, ~ana=List(refined_inner) |> Typ.fresh, tl, m);
      add(
        ~elab_term=Cons(hd_elab, tl_elab) |> rewrap,
        ~elab_syn_ty=List(hd.elab_syn_ty) |> Typ.temp,
        ~marks=[],
        ~ctx=tl.ctx,
        ~constraint_=Coverage.Constraint.cons(hd.constraint_, tl.constraint_),
        m,
      );
    | Wild =>
      add(
        ~elab_syn_ty=unknown,
        ~marks=[],
        ~ctx,
        ~constraint_=Coverage.Constraint.Truth,
        m,
      )
    | Var(name) =>
      /* NOTE: The self type assigned to pattern variables (Unknown)
         may be SynSwitch, but SynSwitch is never added to the context;
         Unknown(Internal) is used in this case */
      let ctx_typ = fixed_typ(ctx, ana, Unknown(Internal) |> Typ.temp);
      let entry =
        Ctx.VarEntry({
          name,
          id: Pat.rep_id(upat),
          typ: ctx_typ,
          custom_statics: None,
        });

      List.exists(l => name == l, duplicate_bindings)
        ? {
          add(
            ~elab_syn_ty=unknown,
            ~marks=[Mark.DuplicateVar(name, unknown)],
            ~ctx=Ctx.extend(ctx, entry),
            ~constraint_=Coverage.Constraint.Truth,
            ~binds=[(Slice.Value, name, Pat.rep_id(upat))],
            m,
          );
        }
        : add(
            ~elab_syn_ty=unknown,
            ~marks=[],
            ~ctx=Ctx.extend(ctx, entry),
            ~constraint_=Coverage.Constraint.Truth,
            ~binds=[(Slice.Value, name, Pat.rep_id(upat))],
            m,
          );

    | TupLabel({term: ExplicitNonlabel, _} as label, p) =>
      let (p, p_elab, m) = go(~ana, ~ctx, p, m);
      /* Add info for the ExplicitNonlabel directly */
      let (_, _, m) =
        add(
          ~user_term=label,
          ~elab_term=label,
          ~ctx,
          ~co_ctx,
          ~ana=syn,
          ~ancestors=ancestors_inclusive,
          ~elab_syn_ty=ExplicitNonlabel |> Typ.temp,
          ~marks=[],
          ~constraint_=Coverage.Constraint.Truth,
          ~label_inference=None,
          ~inferred_label=None,
          ~label_sort=true,
          ~warnings=[],
          m,
        );
      (p, p_elab, add_info(ids, InfoPat(p), m));
    | ExplicitNonlabel =>
      add(
        ~elab_syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[ExplicitNonlabel],
        ~ctx,
        ~constraint_=Coverage.Constraint.Truth,
        m,
      )
    | TupLabel(label, p) =>
      let (labmode, val_mode) =
        LabeledTupleStaticsHelpers.decompose_label_mode(ctx, ana);
      let (p, _, m) = go(~ctx, ~ana=val_mode, ~duplicate_bindings, p, m);
      let (lab_name, m) =
        switch (label.term) {
        | Label(name) =>
          let (_, _, m) =
            add(
              ~user_term=label,
              ~elab_term=label,
              ~ctx,
              ~co_ctx,
              ~ana=labmode,
              ~ancestors=ancestors_inclusive,
              ~elab_syn_ty=Label(name) |> Typ.temp,
              ~marks=[],
              ~constraint_=Coverage.Constraint.Truth,
              ~label_inference=None,
              ~inferred_label=None,
              ~label_sort=true,
              ~warnings=[],
              m,
            );
          (Some(name), m);
        | EmptyHole =>
          let (_, _, m) =
            add(
              ~user_term=label,
              ~elab_term=label,
              ~ctx,
              ~co_ctx,
              ~ana=labmode,
              ~ancestors=ancestors_inclusive,
              ~elab_syn_ty=Unknown(SynSwitch) |> Typ.temp,
              ~marks=[],
              ~constraint_=Coverage.Constraint.Truth,
              ~label_inference=None,
              ~inferred_label=None,
              ~label_sort=true,
              ~warnings=[],
              m,
            );
          (None, m);
        | _ =>
          let (p_info, p_elab, m) = go(~ctx, ~ana=labmode, label, m);
          let (_, _, m) =
            add(
              ~user_term=p_info.user_term,
              ~elab_term=p_elab,
              ~ctx=p_info.ctx,
              ~co_ctx=p_info.co_ctx,
              ~ana=p_info.ana,
              ~ancestors=p_info.ancestors,
              ~elab_syn_ty=p_info.elab_syn_ty,
              ~marks=p_info.marks @ [BadLabel(Pat(label))],
              ~constraint_=p_info.constraint_,
              ~label_inference=p_info.label_inference,
              ~inferred_label=p_info.inferred_label,
              ~label_sort=true,
              ~warnings=p_info.warnings,
              m,
            );
          (None, m);
        };
      let (syn_tl, cms_tl) =
        LabeledTupleStaticsHelpers.standalone_tup_label_self_type(
          ~lab_name,
          ~value_ty=p.elab_syn_ty,
          ~label_is_empty_hole=label.term == EmptyHole,
          ~malformed_source=Pat(label),
        );
      add(
        ~elab_syn_ty=syn_tl,
        ~marks=cms_tl,
        ~ctx=p.ctx,
        ~constraint_=Coverage.Constraint.Tuple([p.constraint_]),
        m,
      );
    | Tuple(ps) =>
      let expected_labels =
        LabeledTupleStaticsHelpers.expected_labels_of_ana(ctx, ana);

      let original_labels =
        List.map(p => Pat.match_tup_label(p) |> Option.map(fst), ps);

      let (inferred_ps, modes) =
        MatchedTyp.prod_rearrange(
          ctx,
          List.map(p => (None: option(string), p), ps),
          ((inferred, p)) => {
            Pat.match_tup_label(p)
            |> Option.map(((label, value)) => (label, (inferred, value)))
          },
          ana,
          (name, (_, p)) =>
            (
              Some(name),
              TupLabel(Label(name) |> Pat.fresh, p) |> Pat.fresh,
            ),
        );
      let ps = List.map(snd, inferred_ps);
      let inferred = List.map(fst, inferred_ps);

      let new_labels =
        List.map(p => Pat.match_tup_label(p) |> Option.map(fst), ps);
      let new_duplicate_bindings =
        Pat.get_duplicate_bindings(Pat.fresh(term));
      let new_duplicate_labels =
        LabeledTuple.get_duplicate_labels(Pat.match_tup_label, ps);
      let duplicate_labels =
        LabeledTupleStaticsHelpers.expand_duplicate_labels(
          ~match_tup_label=Pat.match_tup_label,
          ~unique_duplicates=new_duplicate_labels,
          ps,
        );
      let invalid_labels =
        LabeledTupleStaticsHelpers.compute_invalid_labels(
          ~match_tup_label=Pat.match_tup_label,
          ~expected_labels,
          ps,
        );

      let (ctx, tys, cons, m, info_pats, ps_elabs) =
        List.fold_left2(
          (
            (ctx, tys, cons, m, info_all, elabs),
            (inferred_label, e: Pat.t),
            ana,
          ) =>
            switch (e.term) {
            | TupLabel({term: ExplicitNonlabel, _}, _) =>
              let (info, elab, m) =
                go(
                  ~ctx,
                  ~ana,
                  ~duplicate_bindings=
                    duplicate_bindings @ new_duplicate_bindings,
                  e,
                  m,
                );
              let (info, m) =
                LabeledTupleStaticsHelpers.apply_inferred_label_pat(
                  ~inferred_label,
                  info,
                  m,
                );
              (
                info.ctx,
                tys @ [info.elab_syn_ty],
                cons @ [info.constraint_],
                m,
                info_all @ [info],
                elabs @ [elab],
              );
            | TupLabel(label, value) =>
              let (labmode, val_mode) =
                LabeledTupleStaticsHelpers.decompose_label_mode(ctx, ana);
              let (value_info, value_elab, m) =
                go(
                  ~ctx,
                  ~ana=val_mode,
                  ~duplicate_bindings=
                    duplicate_bindings @ new_duplicate_bindings,
                  value,
                  m,
                );
              let (lab_name, label_invalid, m) =
                switch (label.term) {
                | Label(name) =>
                  let (label_syn, label_marks, label_invalid) =
                    LabeledTupleStaticsHelpers.validate_label_name(
                      ~name,
                      ~expected_labels,
                      ~duplicate_labels=new_duplicate_labels,
                    );
                  let (_, _, m) =
                    add(
                      ~user_term=label,
                      ~elab_term=label,
                      ~ctx,
                      ~co_ctx,
                      ~ana=labmode,
                      ~ancestors=ancestors_inclusive,
                      ~elab_syn_ty=label_syn,
                      ~marks=label_marks,
                      ~constraint_=Coverage.Constraint.Truth,
                      ~label_inference=None,
                      ~inferred_label=None,
                      ~label_sort=true,
                      ~warnings=[],
                      m,
                    );
                  (Some(name), label_invalid, m);
                | EmptyHole =>
                  let (_, _, m) =
                    add(
                      ~user_term=label,
                      ~elab_term=label,
                      ~ctx,
                      ~co_ctx,
                      ~ana=labmode,
                      ~ancestors=ancestors_inclusive,
                      ~elab_syn_ty=Unknown(SynSwitch) |> Typ.temp,
                      ~marks=[],
                      ~constraint_=Coverage.Constraint.Truth,
                      ~label_inference=None,
                      ~inferred_label=None,
                      ~label_sort=true,
                      ~warnings=[],
                      m,
                    );
                  (None, false, m);
                | _ =>
                  let (p_info, p_elab, m) = go(~ctx, ~ana=labmode, label, m);
                  let (p_info, _, m) =
                    add(
                      ~user_term=p_info.user_term,
                      ~elab_term=p_elab,
                      ~ctx=p_info.ctx,
                      ~co_ctx=p_info.co_ctx,
                      ~ana=p_info.ana,
                      ~ancestors=p_info.ancestors,
                      ~elab_syn_ty=p_info.elab_syn_ty,
                      ~marks=p_info.marks @ [BadLabel(Pat(label))],
                      ~constraint_=p_info.constraint_,
                      ~label_inference=p_info.label_inference,
                      ~inferred_label=p_info.inferred_label,
                      ~label_sort=true,
                      ~warnings=p_info.warnings,
                      m,
                    );
                  (
                    None,
                    false,
                    add_info(
                      IdTagged.ids(p_info.user_term),
                      InfoPat(p_info),
                      m,
                    ),
                  );
                };
              let (syn_tl, cms_tl) =
                LabeledTupleStaticsHelpers.tup_label_self_type(
                  ~lab_name,
                  ~label_invalid,
                  ~duplicate_labels=new_duplicate_labels,
                  ~value_ty=value_info.elab_syn_ty,
                  ~label_is_empty_hole=label.term == EmptyHole,
                  ~malformed_source=Pat(label),
                );
              let constraint_ =
                Coverage.Constraint.Tuple([value_info.constraint_]);
              let (_, e_rewrap) = Pat.unwrap(e);
              let elab_tl = TupLabel(label, value_elab) |> e_rewrap;
              let (info, _, m) =
                add(
                  ~user_term=e,
                  ~elab_term=elab_tl,
                  ~ctx=value_info.ctx,
                  ~co_ctx,
                  ~ana,
                  ~ancestors=ancestors_inclusive,
                  ~elab_syn_ty=syn_tl,
                  ~marks=cms_tl,
                  ~constraint_,
                  ~label_inference=None,
                  ~inferred_label,
                  ~label_sort=false,
                  ~warnings=[],
                  m,
                );
              (
                info.ctx,
                tys @ [info.elab_syn_ty],
                cons @ [info.constraint_],
                m,
                info_all @ [info],
                elabs @ [elab_tl],
              );
            | _ =>
              let (info, elab, m) =
                go(
                  ~ctx,
                  ~ana,
                  ~duplicate_bindings=
                    duplicate_bindings @ new_duplicate_bindings,
                  e,
                  m,
                );
              let (info, m) =
                LabeledTupleStaticsHelpers.apply_inferred_label_pat(
                  ~inferred_label,
                  info,
                  m,
                );
              (
                info.ctx,
                tys @ [info.elab_syn_ty],
                cons @ [info.constraint_],
                m,
                info_all @ [info],
                elabs @ [elab],
              );
            },
          (ctx, [], [], m, [], []),
          List.combine(inferred, ps),
          modes,
        );
      let constraint_ = Coverage.Constraint.Tuple(cons);
      let m =
        List.fold_left(
          (m, p: Info.pat) => record(~id=here, Part, p.slice, m),
          m,
          info_pats,
        );

      let malformed_labels =
        LabeledTupleStaticsHelpers.collect_malformed_labels(
          ~has_tup_label=
            (e: Info.pat) =>
              switch (e.user_term.term) {
              | TupLabel(_, _) => true
              | _ => false
              },
          ~get_marks=(e: Info.pat) => e.marks,
          info_pats,
        );
      let (syn_tp, cms_tp) =
        LabeledTupleStaticsHelpers.finalize_tuple_type(
          ~duplicate_labels,
          ~invalid_labels,
          ~malformed_labels,
          tys,
        );
      add(
        ~elab_syn_ty=syn_tp,
        ~marks=cms_tp,
        ~ctx,
        ~constraint_,
        ~label_inference=
          Some(
            LabeledTupleHelpers.derive_label_inference_info(
              original_labels,
              new_labels,
            ),
          ),
        ~elab_term=Tuple(ps_elabs) |> rewrap,
        m,
      );
    | Label(name) =>
      add(
        ~elab_syn_ty=Label(name) |> Typ.temp,
        ~marks=[],
        ~ctx,
        ~constraint_=Coverage.Constraint.Truth,
        m,
      )
    | Parens(p) =>
      let^ (p, p_elab, m) = go(~ctx, ~ana, p, ~duplicate_bindings, m);
      add(
        ~elab_term=Parens(p_elab) |> rewrap,
        ~elab_syn_ty=p.elab_syn_ty,
        ~marks=p.marks,
        ~ctx=p.ctx,
        ~constraint_=p.constraint_,
        m,
      );
    | Projector(data, p) =>
      let (p, p_elab, m) = go(~ctx, ~ana, p, ~duplicate_bindings, m);
      add(
        ~elab_term=Projector(data, p_elab) |> rewrap,
        ~elab_syn_ty=p.elab_syn_ty,
        ~marks=p.marks,
        ~ctx=p.ctx,
        ~constraint_=p.constraint_,
        m,
      );
    | Constructor(ctr, ty) =>
      let (syn_ctr, cms_ctr) =
        ConstructorStaticsHelpers.syn_marks_ctr(ctx, ctr, ana, ty);
      let elab_ty =
        switch (
          ConstructorStaticsHelpers.ctr_ana_typ(ctx, ana, ctr),
          Ctx.lookup_ctr(ctx, ctr),
        ) {
        | (Some(ana_ty), _) => Some(Typ.normalize(ctx, ana_ty))
        | (_, Some({typ: elab_syn_ty, _})) =>
          Some(Typ.normalize(ctx, elab_syn_ty))
        | _ => None
        };
      add(
        ~elab_term=Constructor(ctr, Some(elab_ty)) |> rewrap,
        ~elab_syn_ty=syn_ctr,
        ~marks=cms_ctr,
        ~ctx,
        ~constraint_=Coverage.Constraint.Ap(ctr, None),
        m,
      );
    | Ap(fn, arg) =>
      let ctr = Pat.ctr_name(fn);
      let fn_ana = Arrow(syn, ana) |> Typ.temp;
      let (fn', fn_elab, m) = go(~ctx, ~ana=fn_ana, fn, m);
      let m = {
        switch (ctr) {
        | Some(_) => m
        | _ =>
          let info = prepend_pat_mark(fn', Mark.ExpectedConstructor, ());
          add_info(IdTagged.ids(fn), InfoPat(info), m);
        };
      };
      let (ty_in, ty_out) =
        MatchedTyp.tolerant2(MatchedTyp.arrow, ctx, fn'.elab_syn_ty);
      let (arg, arg_elab, m) = go(~ctx, ~ana=ty_in, arg, m);
      let constraint_ =
        switch (ctr) {
        | Some(ctr) => Coverage.Constraint.Ap(ctr, Some(arg.constraint_))
        | None => Coverage.Constraint.Hole(None)
        };
      add(
        ~elab_term=Ap(fn_elab, arg_elab) |> rewrap,
        ~elab_syn_ty=ty_out,
        ~marks=[],
        ~ctx=arg.ctx,
        ~constraint_,
        m,
      );
    | Asc(p, ann) =>
      let^^ (ann, m) =
        utyp_to_info_map(~ctx, ~ancestors=ancestors_inclusive, ann, m);
      /* Desugar any Sig types in the annotation without full normalization */
      let ann_ty = Typ.desugar_sig(ctx, ann.user_term);
      let^ (p, p_elab, m) =
        go(~ctx, ~under_ascription=true, ~ana=ann_ty, p, m);
      add(
        ~elab_term=Asc(p_elab, Typ.normalize(ctx, ann.user_term)) |> rewrap,
        ~elab_syn_ty=ann_ty,
        ~marks=[],
        ~ctx=p.ctx,
        ~constraint_=p.constraint_,
        m,
      );
    };

  // This is to allow lifting single values into a singleton labeled tuple when the label is not present
  if (under_ascription) {
    default_case();
  } else {
    switch (Typ.weak_head_normalize(ctx, ana).term) {
    | Prod([{term: TupLabel({term: Label(l1), _}, ana_ty), _}]) =>
      // We can flatten this by pulling it up on the case match but since OCaml is strict it'll be evaluated.
      // So for performance reasons we'll just do it here.
      let (e, _, m) = go(~ana=syn, ~ctx, upat, m);

      switch (Typ.weak_head_normalize(ctx, e.ty).term) {
      | Prod([{term: TupLabel({term: Label(l2), _}, _), _}]) when l1 == l2 =>
        default_case()
      | Unknown(_) =>
        /* Unknown type could be a singleton labeled tuple. Only elaborate
           (destructure) if the pattern is a Var whose name matches the label.
           Otherwise, the pattern should have the full tuple type. */
        switch (upat.term) {
        | Var(name) when name == l1 =>
          /* Pattern name matches label - this is destructuring */
          elaborate_singleton_tuple(upat, ana_ty, l1, m)
        | _ =>
          /* Pattern name doesn't match label - use full tuple type */
          default_case()
        }
      | _ => elaborate_singleton_tuple(upat, ana_ty, l1, m)
      };
    | _ => default_case()
    };
  };
}
and utyp_to_info_map =
    (
      ~ctx,
      ~expects=TypExpectation.TypeExpected,
      ~ancestors,
      utyp: Typ.t,
      m: Map.t,
    )
    : (Info.typ, Map.t) => {
  open TypExpectation;
  /* When called with `AnyKindExpected`, skip the strict-Type check —
     the surrounding context (an alias body) is happy with any kind. */
  let kind_marks_for_expected_type =
      (~expects: TypExpectation.t, utyp: Typ.t): list(Mark.t) => {
    switch (expects) {
    | AnyKindExpected => []
    | TypeExpected
    | LabelExpected(_)
    | LabelProjectionExpected(_)
    | ProductExpected
    | ConstructorExpected(_)
    | VariantExpected(_) =>
      let actual = kind_of_typ(ctx, utyp);
      TypKind.consistent(actual, TypKind.Type)
        ? []
        : [
          Mark.TypKindMismatch({
            expected: TypKind.Type,
            actual,
          }),
        ];
    };
  };
  let ids = IdTagged.ids(utyp);
  let term = IdTagged.term_of(utyp);
  let rec status_for_node =
          (~expects=expects, utyp: Typ.t)
          : (list(Mark.t), option(Message.ok_typ)) => {
    let ok = (o: Message.ok_typ): (list(Mark.t), option(Message.ok_typ)) => {
      ([], Some(o));
    };
    let err = (m: Mark.t): (list(Mark.t), option(Message.ok_typ)) => {
      ([m], None);
    };
    switch (expects, utyp.term) {
    | (_, Unknown(Hole(Invalid(token)))) => err(BadToken(token))
    | (LabelExpected(_), Unknown(Hole(EmptyHole))) =>
      ok(Message.EmptyLabel)
    | (LabelProjectionExpected(_), Unknown(Hole(EmptyHole))) =>
      ok(Message.EmptyLabel)
    | (
        TypeExpected | AnyKindExpected | ProductExpected,
        ProdProjection(pty, l),
      ) =>
      switch (Typ.weak_head_normalize(ctx, pty), l.term) {
      | ({term: Prod(tys), _}, Label(l)) =>
        switch (Typ.project_type(tys, l)) {
        | Some(ty') =>
          ok(
            Message.WHNormalizedTo({
              unnormalized: utyp,
              whnormalized: ty',
            }),
          )
        | None =>
          ok(
            Message.TypeUnderdetermined(
              Message.ProdProjectionMissingLabel(
                l,
                List.filter_map(
                  t => Typ.match_tup_label(t) |> Option.map(fst),
                  tys,
                ),
              ),
            ),
          )
        }
      | (t1, _) =>
        ok(
          Message.TypeUnderdetermined(
            Message.ProdProjectionBadArgs({
              product:
                switch (t1.term) {
                | Prod(_) => None
                | _ => Some(Typ.weak_head_normalize(ctx, utyp))
                },
              label:
                switch (l.term) {
                | Label(_) => None
                | _ => Some(l)
                },
            }),
          ),
        )
      }
    | (
        TypeExpected | AnyKindExpected | ProductExpected,
        ProdExtension(t1, t2),
      ) =>
      switch (
        Typ.weak_head_normalize(ctx, t1).term,
        Typ.weak_head_normalize(ctx, t2).term,
      ) {
      | (Prod(t1s), Prod(t2s)) =>
        ok(
          Message.WHNormalizedTo({
            unnormalized: utyp,
            whnormalized: Typ.product_extension(t1s, t2s) |> Typ.fresh,
          }),
        )
      | (Prod(_), _) =>
        ok(
          Message.TypeUnderdetermined(
            Message.ProdExtensionUnderdetermined([t2]),
          ),
        )
      | (_, Prod(_)) =>
        ok(
          Message.TypeUnderdetermined(
            Message.ProdExtensionUnderdetermined([t1]),
          ),
        )
      | _ =>
        ok(
          Message.TypeUnderdetermined(
            Message.ProdExtensionUnderdetermined([t1, t2]),
          ),
        )
      }
    | (ProductExpected, _) =>
      switch (Typ.weak_head_normalize(ctx, utyp)) {
      | {term: Prod(_), _} as ty_prod => ok(Message.Type(ty_prod))
      | ty_n => err(TypWantProduct(ty_n))
      }
    | (_, Unknown(Hole(EmptyHole))) => ok(Message.Type(utyp))
    | (_, Unknown(Hole(MultiHole(_tms)))) => err(TypParseFailure)
    | (VariantExpected(Unique, sum_ty), Var(name))
    | (ConstructorExpected(Unique, sum_ty), Var(name)) =>
      ok(Message.Variant(name, sum_ty))
    | (VariantExpected(Duplicate, _), Var(name))
    | (ConstructorExpected(Duplicate, _), Var(name)) =>
      err(TypDuplicateConstructor(name))
    | (TypeExpected | AnyKindExpected, Var(name)) =>
      let kind_marks = kind_marks_for_expected_type(~expects, utyp);
      switch (Ctx.is_alias(ctx, name)) {
      | false =>
        switch (Ctx.is_abstract(ctx, name)) {
        | false => err(TypFreeTypeVariable(name))
        | true =>
          switch (kind_marks) {
          | [] => ok(Message.Type(Var(name) |> Typ.temp))
          | [mark, ..._] => err(mark)
          }
        }
      | true =>
        switch (kind_marks) {
        | [] =>
          ok(Message.TypeAlias(name, Typ.weak_head_normalize(ctx, utyp)))
        | [mark, ..._] => err(mark)
        }
      };
    | (TypeExpected | AnyKindExpected, Label(_))
    | (LabelExpected(Unique, _), Label(_)) => ok(Message.Type(utyp))
    | (LabelExpected(Duplicate, dupes), Label(name)) =>
      List.exists(l => name == l, dupes)
        ? err(DuplicateLabel(name, utyp)) : err(TypWantLabel)
    | (LabelProjectionExpected(Some(labels)), Label(name)) =>
      List.mem(name, labels)
        ? ok(Message.Type(utyp)) : err(InvalidLabel(name, labels))
    | (LabelProjectionExpected(None), Label(_)) =>
      ok(Message.Type(Unknown(Internal) |> Typ.temp))
    | (ConstructorExpected(_), Label(_))
    | (VariantExpected(_), Label(_)) =>
      err(TypWantConstructorFoundType(utyp))
    | (LabelExpected(_), _)
    | (LabelProjectionExpected(_), _) => err(TypWantLabel)
    | (ConstructorExpected(_), _)
    | (VariantExpected(_), _) => err(TypWantConstructorFoundType(utyp))
    | (_, Parens(t)) => status_for_node(~expects, t)
    | (TypeExpected | AnyKindExpected, TypTuple(_)) =>
      /* `TypTuple` is the multi-arg bundle inside a `TypParamAp` —
         not a stand-alone type. Elements are checked at the
         enclosing `TypParamAp` site. */
      ok(Message.Default)
    | (TypeExpected | AnyKindExpected, TypParamAp(fn, arg)) =>
      /* `T(a, …)` parses as `TypParamAp(T, TypTuple([a, …]))`;
         single-arg `T(a)` arrives as `TypParamAp(T, a)`. We treat
         the latter as a length-1 list against the callee's kind.

         The "result has kind `Type`" check is gated on the
         expectation: in `AnyKindExpected` (alias-body) position the
         result may legitimately have a higher kind (e.g. partial
         application `T(String) :: Type -> Type`), and the alias
         just inherits it. In `TypeExpected` position the result
         must be `Type`. */
      let fn_kind = kind_of_typ(ctx, fn);
      let arg_kinds =
        switch (arg.term) {
        | TypTuple(ts) => List.map(kind_of_typ(ctx), ts)
        | Unknown(_)
        | Atom(_)
        | DrvQuoteTy(_)
        | Label(_)
        | ExplicitNonlabel
        | Var(_)
        | Parens(_)
        | Projector(_)
        | ProdProjection(_)
        | List(_)
        | Arrow(_)
        | TupLabel(_)
        | ProdExtension(_)
        | Prod(_)
        | Sum(_)
        | Poly(_)
        | ProofOf(_)
        | Sig(_)
        | TypFun(_)
        | TypParamAp(_)
        | Rec(_) => [kind_of_typ(ctx, arg)]
        };
      switch (fn_kind) {
      | TypKind.Unknown =>
        /* Callee kind unknown (e.g. unbound type var). Don't pile
           a "cannot apply" mark on top of the free-variable mark
           already reported at the callee. */
        ok(Message.Type(utyp))
      | TypKind.Type => err(Mark.TypParamApplyNonArrowKind(fn_kind))
      | TypKind.Arrow(expected, result) =>
        let n_expected = List.length(expected);
        let n_actual = List.length(arg_kinds);
        if (n_expected != n_actual) {
          err(
            Mark.TypParamApplyArityMismatch({
              callee: fn,
              callee_kind: fn_kind,
              expected: n_expected,
              actual: n_actual,
            }),
          );
        } else if (!List.for_all2(TypKind.consistent, expected, arg_kinds)) {
          err(
            Mark.TypKindMismatch({
              expected:
                switch (expected) {
                | [k] => k
                | _ => TypKind.Arrow(expected, TypKind.Type)
                },
              actual:
                switch (arg_kinds) {
                | [k] => k
                | _ => TypKind.Arrow(arg_kinds, TypKind.Type)
                },
            }),
          );
        } else if (expects == TypeExpected
                   && !TypKind.consistent(result, TypKind.Type)) {
          err(
            Mark.TypKindMismatch({
              expected: TypKind.Type,
              actual: result,
            }),
          );
        } else {
          ok(Message.Type(utyp));
        };
      };
    | (TypeExpected | AnyKindExpected, TypFun(_, _)) =>
      /* `TypFun` has an `Arrow` kind. Accept it as kind-OK at this
         node; descendants are visited via the `TypFun` arm of the
         outer `switch` with `AnyKindExpected` so a curried tail
         doesn't repeat this check. */
      ok(Message.Type(utyp))
    | (TypeExpected | AnyKindExpected, _) =>
      switch (kind_marks_for_expected_type(~expects, utyp)) {
      | [] => ok(Message.Type(utyp))
      | [mark, ..._] => err(mark)
      }
    };
  };
  let add = (~expects=expects, ~utyp=utyp, ~uses=[], m) => {
    let st = status_for_node(~expects, utyp);
    let cls: Cls.t =
      switch (expects, Typ.cls_of_term(utyp.term)) {
      | (
          TypExpectation.VariantExpected(_) |
          TypExpectation.ConstructorExpected(_),
          Var,
        ) =>
        Cls.Typ(Constructor)
      | (_, cls) => Cls.Typ(cls)
      };
    let (children, m) = take_children(~id=Typ.rep_id(utyp), m);
    let slice =
      Slice.mk(
        ~ctx,
        ~id=Typ.rep_id(utyp),
        ~ids=Slice.typ_ids(utyp),
        ~shape=utyp,
        ~components=children,
        ~uses,
        (),
      );
    let info: Info.typ = {
      cls,
      ctx,
      ancestors,
      marks: fst(st),
      message: Option.map(x => Message.TypOk(x), snd(st)),
      expects,
      warnings: [],
      user_term: utyp,
      slice,
    };
    (info, add_info(ids, InfoTyp(info), m));
  };
  let ancestors_inclusive = [Typ.rep_id(utyp)] @ ancestors;
  let ancestors = (); // Deliberately shadowed so there's no risk of using it by mistake
  let _ = ancestors;
  let go =
      (~ctx=ctx, ~expects=TypExpectation.TypeExpected, t: Typ.t, m: Map.t) => {
    let (info, m) =
      utyp_to_info_map(~ctx, ~ancestors=ancestors_inclusive, ~expects, t, m);
    (info, record(~id=Typ.rep_id(utyp), Part, info.slice, m));
  };
  switch (term) {
  | Unknown(Hole(MultiHole(tms))) =>
    let (_, _, m) = multi(~ctx, ~ancestors=ancestors_inclusive, m, tms);
    add(m);
  | Unknown(_)
  | DrvQuoteTy(_) => add(m)
  | Atom(_) => add(m)
  | Var(_) =>
    /* Names are resolved in this function's status rules */
    add(m)
  | List(t)
  | Parens(t)
  | Projector(_, t) => add(go(t, m) |> snd)
  | Arrow(t1, t2) =>
    let m = go(t1, m) |> snd;
    let m = go(t2, m) |> snd;
    add(m);
  | TypParamAp(t1, t2) =>
    /* The callee `t1` isn't in `TypeExpected` position — its kind
       is constrained by the enclosing `TypParamAp`, not required
       to be `Type`. We recurse with the ordinary `go` so descendants
       still get info entries, then overwrite `t1`'s own info entry
       with a callee-context one (drops the spurious kind-arrow
       marks and shows a "has kind …" summary). */
    let fn_kind = kind_of_typ(ctx, t1);
    let m = go(t1, m) |> snd;
    let prior_marks =
      switch (Map.lookup(Typ.rep_id(t1), m)) {
      | Some(InfoTyp(info)) => info.marks
      | _ => []
      };
    let kept_marks =
      List.filter(
        fun
        | Mark.TypKindMismatch(_)
        | Mark.TypParamApplyNonArrowKind(_)
        | Mark.TypParamApplyArityMismatch(_) => false
        | _ => true,
        prior_marks,
      );
    let fn_message =
      kept_marks == [] ? Some(Message.TypOk(Message.Kind(fn_kind))) : None;
    let prior_slice =
      switch (Map.lookup(Typ.rep_id(t1), m)) {
      | Some(InfoTyp(info)) => info.slice
      | _ => Slice.opaque
      };
    let fn_info: Info.typ = {
      cls: Cls.Typ(Typ.cls_of_term(t1.term)),
      ctx,
      ancestors: ancestors_inclusive,
      marks: kept_marks,
      message: fn_message,
      expects,
      warnings: [],
      user_term: t1,
      slice: prior_slice,
    };
    let m = add_info(IdTagged.ids(t1), InfoTyp(fn_info), m);
    let m = go(t2, m) |> snd;
    add(m);
  | TypTuple(ts) =>
    /* `TypTuple` is the multi-arg bundle in a `TypParamAp`; the
       node itself is checked at its parent's site. Recurse so each
       element gets its own kind info. */
    let m = map_m(go, ts, m) |> snd;
    add(m);
  | Prod(ts) =>
    let duplicate_labels =
      LabeledTuple.get_duplicate_labels(Typ.match_tup_label, ts);
    let m =
      List.is_empty(duplicate_labels)
        ? map_m(go, ts, m) |> snd
        : map_m(
            (t: Typ.t) =>
              go(
                ~expects=
                  switch (t.term) {
                  | Label(_)
                  | TupLabel(_, _) =>
                    LabelExpected(Duplicate, duplicate_labels)
                  | _ => TypeExpected
                  },
                t,
              ),
            ts,
            m,
          )
          |> snd;
    add(m);
  | ProdProjection(t, label) =>
    let labels =
      switch (Typ.normalize(ctx, t).term) {
      | Prod(ts) =>
        Some(
          List.filter_map(
            t => Typ.match_tup_label(t) |> Option.map(fst),
            ts,
          ),
        )
      | _ => None
      };
    let m = go(~expects=LabelProjectionExpected(labels), label, m) |> snd;
    let m = go(~expects=ProductExpected, t, m) |> snd;
    add(~expects=TypeExpected, m);
  | ProdExtension(t1, t2) =>
    let m = go(~expects=ProductExpected, t1, m) |> snd;
    let m = go(~expects=ProductExpected, t2, m) |> snd;
    add(m);
  | ExplicitNonlabel =>
    let ancestors = List.tl(ancestors_inclusive); // Recover original ancestors

    let info: Info.typ = {
      cls: Typ(ExplicitNonlabel),
      ctx,
      ancestors,
      marks: [Mark.BadToken("_")],
      message: None,
      expects,
      user_term: utyp,
      warnings: [],
      slice: Slice.opaque,
    };
    (info, add_info(ids, InfoTyp(info), m));
  | TupLabel({term: ExplicitNonlabel, _} as label, t) =>
    let (_, m) = go(t, m);

    let label_info: Info.typ = {
      cls: Typ(ExplicitNonlabel),
      ctx,
      ancestors: ancestors_inclusive,
      marks: [],
      message: Some(Message.TypOk(Message.EmptyLabel)),
      expects,
      user_term: utyp,
      warnings: [],
      slice: Slice.opaque,
    };

    let m = add_info(label.annotation.ids, InfoTyp(label_info), m);
    add(~expects=TypeExpected, ~utyp=t, m);
  | TupLabel(label, t) =>
    let expects_label =
      switch (expects) {
      | LabelExpected(_) => expects
      | _ => LabelExpected(Unique, [])
      };
    let m = go(~expects=expects_label, label, m) |> snd;
    let m = go(t, m) |> snd;
    add(~expects=TypeExpected, m);
  | Label(_) => add(m)
  | Sum(variants) =>
    let (m, _) =
      List.fold_left(
        variant_to_info_map(
          ~ctx,
          ~ancestors=ancestors_inclusive,
          ~ty_sum=utyp,
        ),
        (m, []),
        variants,
      );
    add(m);
  | Poly(utpat, tbody) =>
    /* `utpat` may be a single binder (`Var`) or `TPat.Tuple([…])`
       representing `poly a, b, … -> body`. Extend the body context
       with each named binder. Non-name binders (holes / invalid) are
       still recorded via `utpat_to_info_map` but do not contribute to
       the body's type environment. */
    let body_ctx =
      List.fold_left(
        (ctx, b: TPat.t) =>
          switch (TPat.tyvar_of_utpat(b)) {
          | Some(name) =>
            Ctx.extend_tvar(
              ctx,
              {
                name,
                id: TPat.rep_id(b),
                kind: Abstract,
                typ_kind: TypKind.Type,
              },
            )
          | None => ctx
          },
        ctx,
        TPat.binders_of(utpat),
      );
    let m =
      utyp_to_info_map(
        tbody,
        ~ctx=body_ctx,
        ~ancestors=ancestors_inclusive,
        ~expects=TypeExpected,
        m,
      )
      |> snd;
    let m =
      utpat_to_info_map(~ctx, ~ancestors=ancestors_inclusive, utpat, m) |> snd;
    add(m);
  | TypFun(utpat, tbody) =>
    let body_ctx =
      List.fold_left(
        (ctx, b: TPat.t) =>
          switch (TPat.tyvar_of_utpat(b)) {
          | Some(name) =>
            Ctx.extend_tvar(
              ctx,
              {
                name,
                id: TPat.rep_id(b),
                kind: Abstract,
                typ_kind: TypKind.Type,
              },
            )
          | None => ctx
          },
        ctx,
        TPat.binders_of(utpat),
      );
    let m =
      utyp_to_info_map(
        tbody,
        ~ctx=body_ctx,
        ~ancestors=ancestors_inclusive,
        /* The body may itself be a `TypFun` (curried alias) or a
           partial `TypParamAp` whose result is an arrow kind; the
           outer `TypFun` already accounts for that extra arrow in
           its own kind, so don't re-flag it here. */
        ~expects=AnyKindExpected,
        m,
      )
      |> snd;
    let m =
      utpat_to_info_map(~ctx, ~ancestors=ancestors_inclusive, utpat, m) |> snd;
    add(m);
  | ProofOf(e) =>
    let (_, _, m) =
      uexp_to_info_map(
        ~ctx,
        ~ancestors=ancestors_inclusive,
        ~ana=Atom(Bool) |> Typ.temp,
        e,
        m,
      );
    add(m);
  | Rec({term: Var(name), _} as utpat, tbody) =>
    let body_ctx =
      Ctx.extend_tvar(
        ctx,
        {
          name,
          id: TPat.rep_id(utpat),
          kind: Singleton(utyp),
          typ_kind: TypKind.Type,
        },
      );
    let m =
      utyp_to_info_map(
        tbody,
        ~ctx=body_ctx,
        ~ancestors=ancestors_inclusive,
        ~expects=TypeExpected,
        m,
      )
      |> snd;
    let m =
      utpat_to_info_map(~ctx, ~ancestors=ancestors_inclusive, utpat, m) |> snd;
    add(m); // TODO: check with andrew
  | Rec(utpat, tbody) =>
    let m =
      utyp_to_info_map(
        tbody,
        ~ctx,
        ~ancestors=ancestors_inclusive,
        ~expects=TypeExpected,
        m,
      )
      |> snd;
    let m =
      utpat_to_info_map(~ctx, ~ancestors=ancestors_inclusive, utpat, m) |> snd;
    add(m); // TODO: check with andrew
  | Sig(items) =>
    let m =
      List.fold_left(
        (m, item: Sig.t) => {
          let (_, _, m) =
            any_to_info_map(
              ~ctx,
              ~ancestors=ancestors_inclusive,
              Sig(item),
              m,
            );
          m;
        },
        m,
        items,
      );
    add(m);
  };
}
and utpat_to_info_map =
    (
      ~at_alias_head=false,
      ~alias_kind: option(TypKind.t)=?,
      ~ctx,
      ~ancestors,
      utpat: TPat.t,
      m: Map.t,
    )
    : (Info.tpat, Map.t) => {
  /* `at_alias_head` is `true` only for the outermost tpat of a
     `type … = …` declaration. The `T(a, b)` parameter-list form
     (`TPat.Param`) is allowed there and rejected elsewhere.
     `~alias_kind`, when provided, is the kind the surrounding
     `TyAlias` computed for the alias's body via `kind_of_typ`;
     it's the single source of truth for the alias's kind, surfaced
     here in the cursor-inspector message at the alias name. */
  let ids = IdTagged.ids(utpat);
  let term = IdTagged.term_of(utpat);
  let rec status_for_node =
          (utpat: TPat.t): (list(Mark.t), option(Message.ok_tpat)) =>
    switch (utpat.term) {
    /* `Parens` is transparent — the cursor inspector entry for a
       parens tile shows the inner node's info (class and message),
       matching the exp/pat/typ convention. */
    | Parens(inner) => status_for_node(inner)
    | EmptyHole => ([], Some(Message.Empty))
    | Var(name) when Ctx.is_base_typ(name) => (
        [TPatShadowsType(name, BaseTyp)],
        None,
      )
    | Var(name) =>
      /* At the alias's head, the alias being defined isn't yet in
         ctx, so prefer the caller-supplied `alias_kind` over the
         lookup. */
      switch (alias_kind, Ctx.lookup_tvar_typ_kind(ctx, name)) {
      | (Some(kind), _) when at_alias_head => (
          [],
          Some(
            Message.TypeAlias({
              name,
              kind,
            }),
          ),
        )
      | (_, Some(kind)) when Ctx.is_abstract(ctx, name) => (
          [],
          Some(
            Message.TypeParameter({
              name,
              kind,
            }),
          ),
        )
      | (_, Some(kind)) => (
          [],
          Some(
            Message.TypeAlias({
              name,
              kind,
            }),
          ),
        )
      | (_, None) => (
          [],
          Some(
            Message.TypeAlias({
              name,
              kind: TypKind.Type,
            }),
          ),
        )
      }
    | Param(head, _params) when !at_alias_head =>
      let name = TPat.head_name_of(head) |> Option.value(~default="?");
      ([Mark.TPatParamNotAtAliasHead(name)], None);
    | Param(head, params) =>
      switch (TPat.head_name_of(head)) {
      | Some(name) when Ctx.is_base_typ(name) => (
          [TPatShadowsType(name, BaseTyp)],
          None,
        )
      | Some(name) =>
        /* Prefer the caller-supplied kind over `of_param_count`,
           which counts only surface params and would miss any
           `TypFun` tail in the body. */
        let kind =
          switch (alias_kind) {
          | Some(k) => k
          | None => TypKind.of_param_count(List.length(params))
          };
        (
          [],
          Some(
            Message.TypeAlias({
              name,
              kind,
            }),
          ),
        );
      | None => ([TPatNotAVar(Other)], None)
      }
    | Tuple(_) => ([], Some(Message.Default))
    | Invalid(_) => ([TPatNotAVar(NotCapitalized)], None)
    | MultiHole(_) => ([TPatNotAVar(Other)], None)
    };
  let add = m => {
    let st = status_for_node(utpat);
    let info: Info.tpat = {
      cls: Cls.TPat(TPat.cls_of_term(utpat.term)),
      ancestors,
      marks: fst(st),
      message: Option.map(x => Message.TPatOk(x), snd(st)),
      warnings: [],
      ctx,
      user_term: utpat,
    };
    (info, add_info(ids, InfoTPat(info), m));
  };
  let ancestors_inclusive = [TPat.rep_id(utpat)] @ ancestors;
  let ancestors = (); // Deliberately shadowed so there's no risk of using it by mistake
  let _ = ancestors;
  switch (term) {
  | MultiHole(tms) =>
    let (_, _, m) = multi(~ctx, ~ancestors=ancestors_inclusive, m, tms);
    add(m);
  | Invalid(_)
  | EmptyHole
  | Var(_) => add(m)
  | Param(head, params) =>
    /* The head is a sibling AST node with its own tile id. Recurse
       so it gets an info entry at its own id; in valid alias-head
       position, override it with one carrying the alias's kind.
       Sub-tpats are always visited with `at_alias_head=false`. */
    let m =
      utpat_to_info_map(~ctx, ~ancestors=ancestors_inclusive, head, m) |> snd;
    let m =
      if (at_alias_head) {
        let head_kind = TypKind.of_param_count(List.length(params));
        let head_message =
          switch (TPat.head_name_of(head)) {
          | Some(name) when Ctx.is_base_typ(name) => None
          | Some(name) =>
            Some(
              Message.TPatOk(
                Message.TypeAlias({
                  name,
                  kind: head_kind,
                }),
              ),
            )
          | None => None
          };
        let head_info: Info.tpat = {
          cls: Cls.TPat(TPat.cls_of_term(head.term)),
          ancestors: ancestors_inclusive,
          marks:
            switch (TPat.head_name_of(head)) {
            | Some(name) when Ctx.is_base_typ(name) => [
                TPatShadowsType(name, BaseTyp),
              ]
            | Some(_)
            | None => []
            },
          message: head_message,
          warnings: [],
          ctx,
          user_term: head,
        };
        add_info(IdTagged.ids(head), InfoTPat(head_info), m);
      } else {
        m;
      };
    let param_ctx =
      List.fold_left(
        (ctx, param) =>
          switch (TPat.tyvar_of_utpat(param)) {
          | Some(name) =>
            Ctx.extend_tvar(
              ctx,
              {
                name,
                id: TPat.rep_id(param),
                kind: Abstract,
                typ_kind: TypKind.Type,
              },
            )
          | None => ctx
          },
        ctx,
        params,
      );
    let m =
      List.fold_left(
        (m, param) =>
          utpat_to_info_map(
            ~ctx=param_ctx,
            ~ancestors=ancestors_inclusive,
            param,
            m,
          )
          |> snd,
        m,
        params,
      );
    add(m);
  | Tuple(tps) =>
    /* `Tuple` is a binder list for `Poly`/`TypAbs`/`TypFun`/`Rec`.
       Each element is a single binder; recurse with each extending
       a local tvar context so siblings see each other as abstract
       type parameters. */
    let elt_ctx =
      List.fold_left(
        (ctx, tp) =>
          switch (TPat.tyvar_of_utpat(tp)) {
          | Some(name) =>
            Ctx.extend_tvar(
              ctx,
              {
                name,
                id: TPat.rep_id(tp),
                kind: Abstract,
                typ_kind: TypKind.Type,
              },
            )
          | None => ctx
          },
        ctx,
        tps,
      );
    let m =
      List.fold_left(
        (m, tp) =>
          utpat_to_info_map(
            ~ctx=elt_ctx,
            ~ancestors=ancestors_inclusive,
            tp,
            m,
          )
          |> snd,
        m,
        tps,
      );
    add(m);
  | Parens(inner) =>
    /* `Parens` is transparent — propagate `at_alias_head` so e.g.
       `type (T(a)) = …` recognizes the inner `T(a)` as an alias
       head. */
    let m =
      utpat_to_info_map(
        ~at_alias_head,
        ~ctx,
        ~ancestors=ancestors_inclusive,
        inner,
        m,
      )
      |> snd;
    add(m);
  };
}
and variant_to_info_map =
    (
      ~ctx,
      ~ancestors,
      ~ty_sum,
      (m, ctrs),
      uty: ConstructorMap.variant(Typ.t),
    ) => {
  open TypExpectation;
  let go = expects => utyp_to_info_map(~ctx, ~ancestors, ~expects);
  switch (uty) {
  | BadEntry(uty) =>
    let m = go(VariantExpected(Unique, ty_sum), uty, m) |> snd;
    (m, ctrs);
  | Variant(ctr, ann, param) =>
    let m =
      go(
        ConstructorExpected(
          List.mem(ctr, ctrs) ? Duplicate : Unique,
          ty_sum,
        ),
        {
          term: Var(ctr),
          annotation: IdTagged.IdTag.mk_internal(ann.ids),
        },
        m,
      )
      |> snd;
    let m =
      switch (param) {
      | Some(param_ty) => go(TypeExpected, param_ty, m) |> snd
      | None => m
      };
    (m, [ctr, ...ctrs]);
  };
}
and rul_to_info_map =
    (~ctx, ~ancestors, r: Rul.t, m: Map.t): (CoCtx.t, Any.t, Map.t) =>
  /* NOTE: This function is only used for rules that are not properly positioned in cases.
     Properly positioned rules would already have been removed in maketerm and became part
     of case expressions, so we don't need to worry about them here. */
  switch (r.term) {
  | Rules(scrut, rules) =>
    /* Treat rules not properly positioned in cases as multiholes.
     * Properly positioned rules would already have been removed
     * in maketerm and became part of case expressions */
    let tms =
      rules
      |> List.map(((p, e)) => [Grammar.Pat(p), Grammar.Exp(e)])
      |> List.concat;
    any_to_info_map(
      ~ctx,
      ~ancestors,
      Exp({
        term: MultiHole([Exp(scrut), ...tms]),
        annotation: r.annotation,
      }),
      m,
    );
  | MultiHole(tms) =>
    let (co_ctxs, _, m) = multi(~ctx, ~ancestors, m, tms);
    (CoCtx.union(co_ctxs), Rul(r), m);
  | Invalid(_) => (CoCtx.empty, Rul(r), m)
  }
and mod_to_info_map =
    (~ctx, ~ancestors, m_term: Mod.t, m: Map.t): (CoCtx.t, Any.t, Map.t) => {
  /* NOTE: This function is only used for module parts that are not properly positioned in modules.
     Properly positioned module parts are handled in the module cases of exp_to_info_map. */
  let ids = IdTagged.ids(m_term);
  let cls = Cls.Mod(Mod.cls_of_term(m_term.term));
  let add_mod_info = m =>
    add_info(
      ids,
      InfoMod({
        id: IdTagged.rep_id(m_term),
        user_term: m_term,
        cls,
        sort: Mod,
        ctx,
        ancestors,
      }),
      m,
    );
  switch (m_term.term) {
  | Invalid(_)
  | EmptyHole => (CoCtx.empty, Mod(m_term), add_mod_info(m))
  | MultiHole(tms) =>
    let (co_ctxs, _, m) = multi(~ctx, ~ancestors, m, tms);
    (CoCtx.union(co_ctxs), Mod(m_term), add_mod_info(m));
  | ModLet(p, e) =>
    let (co_ctx_e, _, m) = any_to_info_map(~ctx, ~ancestors, Exp(e), m);
    let (_, _, m) = any_to_info_map(~ctx, ~ancestors, Pat(p), m);
    (co_ctx_e, Mod(m_term), add_mod_info(m));
  | ModType(tp, t) =>
    let (_, _, m) = any_to_info_map(~ctx, ~ancestors, TPat(tp), m);
    let (_, _, m) = any_to_info_map(~ctx, ~ancestors, Typ(t), m);
    (CoCtx.empty, Mod(m_term), add_mod_info(m));
  | ModExp(e) =>
    let (co_ctx, _, m) = any_to_info_map(~ctx, ~ancestors, Exp(e), m);
    (co_ctx, Mod(m_term), add_mod_info(m));
  | ModuleMod(mp, e) =>
    let (_, _, m) = any_to_info_map(~ctx, ~ancestors, MPat(mp), m);
    let (co_ctx, _, m) = any_to_info_map(~ctx, ~ancestors, Exp(e), m);
    (co_ctx, Mod(m_term), add_mod_info(m));
  };
}
and sig_to_info_map =
    (~ctx, ~ancestors, s_term: Sig.t, m: Map.t): (CoCtx.t, Any.t, Map.t) => {
  /* NOTE: This function is only used for signature items that are not properly positioned in signatures.
     Properly positioned signature items are handled in the signature cases of typ_to_info_map. */
  let ids = IdTagged.ids(s_term);
  let cls = Cls.Sig(Sig.cls_of_term(s_term.term));
  let add_sig_info = m =>
    add_info(
      ids,
      InfoSig({
        id: IdTagged.rep_id(s_term),
        user_term: s_term,
        cls,
        sort: Sig,
        ctx,
        ancestors,
      }),
      m,
    );
  switch (s_term.term) {
  | Invalid(_)
  | EmptyHole => (CoCtx.empty, Sig(s_term), add_sig_info(m))
  | MultiHole(tms) =>
    let (co_ctxs, _, m) = multi(~ctx, ~ancestors, m, tms);
    (CoCtx.union(co_ctxs), Sig(s_term), add_sig_info(m));
  | SigLet(p) =>
    let hole_co_ctx =
      CoCtx.singleton(
        "$hole",
        IdTagged.rep_id(s_term),
        Unknown(Internal) |> Typ.temp,
      );
    let (_, _, m) =
      upat_to_info_map(
        ~is_synswitch=false,
        ~co_ctx=hole_co_ctx,
        ~ancestors,
        ~ctx,
        p,
        m,
      );
    (CoCtx.empty, Sig(s_term), add_sig_info(m));
  | SigType(tp, t) =>
    let (_, _, m) = any_to_info_map(~ctx, ~ancestors, TPat(tp), m);
    let (_, _, m) = any_to_info_map(~ctx, ~ancestors, Typ(t), m);
    (CoCtx.empty, Sig(s_term), add_sig_info(m));
  };
}
and mpat_to_info_map =
    (~ctx, ~ancestors, mp_term: MPat.t, m: Map.t): (CoCtx.t, Any.t, Map.t) => {
  let ids = IdTagged.ids(mp_term);
  let cls = Cls.MPat(MPat.cls_of_term(mp_term.term));
  let add_mpat_info = m =>
    add_info(
      ids,
      InfoMPat({
        id: IdTagged.rep_id(mp_term),
        user_term: mp_term,
        cls,
        sort: MPat,
        ctx,
        ancestors,
      }),
      m,
    );
  switch (mp_term.term) {
  | Invalid(_)
  | EmptyHole
  | Var(_) => (CoCtx.empty, MPat(mp_term), add_mpat_info(m))
  | MultiHole(tms) =>
    let (co_ctxs, _, m) = multi(~ctx, ~ancestors, m, tms);
    (CoCtx.union(co_ctxs), MPat(mp_term), add_mpat_info(m));
  | Asc(inner, typ) =>
    let (_, _, m) = any_to_info_map(~ctx, ~ancestors, MPat(inner), m);
    let (_, _, m) = any_to_info_map(~ctx, ~ancestors, Typ(typ), m);
    (CoCtx.empty, MPat(mp_term), add_mpat_info(m));
  };
};

let mk =
  Core.Memo.general(
    ~cache_size_bound=1000,
    (ana, ctx, e) => {
      let (_, elab, m) =
        uexp_to_info_map(~ana, ~ctx, ~ancestors=[], e, Id.Map.empty);
      /* Some syntax nodes carry multiple equivalent ids (e.g. shard ids).
         Ensure they all resolve to the same info entry for cursor features. */
      let m_ref = ref(m);
      let _ =
        Grammar.map_exp_annotation(
          ({ids, _}: IdTagged.IdTag.t) => {
            let info_opt =
              List.find_map(id => Id.Map.find_opt(id, m_ref^), ids);
            switch (info_opt) {
            | Some(info) => m_ref := add_missing_info(ids, info, m_ref^)
            | None => ()
            };
            ();
          },
          e,
        );
      (m_ref^, elab);
    },
  );

let mk = (~ana=Typ.temp(Unknown(SynSwitch)), core: CoreSettings.t, ctx, exp) =>
  core.statics ? mk(ana, ctx, exp) : (Id.Map.empty, Exp.fresh(Tuple([])));

let mk_typ = (core: CoreSettings.t, ctx, typ: Typ.t): Map.t =>
  if (core.statics) {
    let (_, m) = utyp_to_info_map(~ctx, ~ancestors=[], typ, Id.Map.empty);
    let m_ref = ref(m);
    let _ =
      Grammar.map_typ_annotation(
        ({ids, _}: IdTagged.IdTag.t) => {
          let info_opt =
            List.find_map(id => Id.Map.find_opt(id, m_ref^), ids);
          switch (info_opt) {
          | Some(info) => m_ref := add_missing_info(ids, info, m_ref^)
          | None => ()
          };
          ();
        },
        typ,
      );
    m_ref^;
  } else {
    Id.Map.empty;
  };

module Slice = Slice;

let slice =
    (
      ~ctx=Ctx.empty_pre_elaboration,
      ~ana=Typ.temp(Unknown(SynSwitch)),
      ~focus=None,
      ~direction=`Syn,
      exp,
      query,
    )
    : Slice.result => {
  let _ = direction;
  let (root, _, m) =
    uexp_to_info_map(~ana, ~ctx, ~ancestors=[], exp, Id.Map.empty);
  let focused = id =>
    Map.lookup(id, m)
    |> Option.map((info: Info.t) =>
         Slice.{
           is_exp: Info.sort_of(info) == Exp,
           ancestors: Info.ancestors_of(info),
           ctx: Info.ctx_of(info),
           syn:
             switch (info) {
             | InfoExp({elab_syn_ty, _}) => elab_syn_ty
             | _ => Typ.gap
             },
         }
       );
  Slice.slice(
    ~focus,
    ~root_id=Exp.rep_id(exp),
    ~root=root.slice,
    ~focused,
    query,
  );
};
