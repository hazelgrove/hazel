/*
 A nice property would be that elaboration is idempotent...
 */

open Util;

exception MissingTypeInfo;

module ElaborationResult = {
  [@deriving sexp]
  type t =
    | Elaborates(DHExp.t, Typ.t)
    | DoesNotElaborate;
};

let fresh_ascription = (d: Exp.t, t: Typ.t, t': option(Typ.t)) => {
  IdTagged.FreshGrammar.Exp.(
    switch (t') {
    | Some({term: Unknown(Internal), _}) => d
    | Some(ty) when !Typ.fast_equal(ty, t) => asc(d, ty)
    | _ => d
    }
  );
};
let elaborated_type =
    (m: Statics.Map.t, uexp: Exp.t): (Typ.t, Typ.t, Ctx.t, CoCtx.t, Exp.t) => {
  let (ana_ty, self_ty, ctx, co_ctx, term) =
    switch (Id.Map.find_opt(Exp.rep_id(uexp), m)) {
    | Some(Info.InfoExp({ana, ty, ctx, co_ctx, term: new_term, _})) => (
        ana,
        ty,
        ctx,
        co_ctx,
        new_term,
      )
    | _ => raise(MissingTypeInfo)
    };
  let elab_ty = Typ.match_synswitch(ana_ty, self_ty);
  (
    elab_ty |> Typ.normalize(ctx) |> Typ.all_ids_temp,
    ana_ty,
    ctx,
    co_ctx,
    term,
  );
};

let elaborated_pat_type =
    (m: Statics.Map.t, upat: Pat.t): (Typ.t, Typ.t, Ctx.t, Pat.t) => {
  let (ana_ty, self_ty, ctx, prev_synswitch, term, label_inference) =
    switch (Id.Map.find_opt(Pat.rep_id(upat), m)) {
    | Some(
        Info.InfoPat({
          ana,
          ty,
          ctx,
          prev_synswitch,
          term: new_term,
          label_inference,
          _,
        }),
      ) => (
        ana,
        ty,
        ctx,
        prev_synswitch,
        new_term,
        label_inference,
      )
    | _ => raise(MissingTypeInfo)
    };
  let elab_ty =
    switch (prev_synswitch) {
    | None => Typ.match_synswitch(self_ty, ana_ty)
    | Some(syn_ty) =>
      // Autolabelling for singleton labeled tuples
      switch (label_inference) {
      | Some(SingletonLabelInference({label: l, _})) =>
        Typ.match_synswitch(
          Prod([TupLabel(Label(l) |> Typ.temp, syn_ty) |> Typ.temp])
          |> Typ.temp,
          ana_ty,
        )
      | _ => Typ.match_synswitch(syn_ty, ana_ty)
      }
    };
  (elab_ty |> Typ.normalize(ctx) |> Typ.all_ids_temp, ana_ty, ctx, term);
};

let rec elaborate_pattern =
        (m: Statics.Map.t, upat: Pat.t, in_container: bool): (Pat.t, Typ.t) => {
  // Pulling upat back out of the statics map for statics level singleton tuple autolabeling
  let (elaborated_type, ana, ctx, upat) = elaborated_pat_type(m, upat);
  let elaborate_pattern = (~in_container=false, m, upat) =>
    elaborate_pattern(m, upat, in_container);
  let (term, rewrap) = Pat.unwrap(upat);
  let dpat =
    switch (term) {
    | Atom(c) =>
      let c =
        Operators.replace_literal(c, Typ.is_ana_atom(ana), ctx.use_mode);
      switch (c) {
      | L(c) => Atom(c) |> rewrap
      | R(BadInt(s)) => Invalid(s) |> rewrap
      };
    | ListLit(ps) =>
      let (ps, _) = List.map(elaborate_pattern(m), ps) |> List.split;
      ListLit(ps) |> rewrap;
    | Cons(p1, p2) =>
      let (p1', _) = elaborate_pattern(m, p1);
      let (p2', _) = elaborate_pattern(m, p2);
      Cons(p1', p2') |> rewrap;
    | TupLabel({term: ExplicitNonlabel, _}, p) => p
    | TupLabel(lab, p) =>
      let (plab, _) = elaborate_pattern(m, lab);
      let (p', _) = elaborate_pattern(m, p);
      if (in_container) {
        TupLabel(plab, p') |> rewrap;
      } else {
        Tuple([TupLabel(plab, p') |> rewrap]) |> DHPat.fresh;
      };
    | Tuple(ps) =>
      let (ps', _) =
        List.map(elaborate_pattern(m, ~in_container=true), ps) |> List.split;
      let expected_labels: list(option(string)) =
        Typ.get_labels(ctx, elaborated_type);

      let ps' =
        LabeledTuple.rearrange(
          s => Option.map(x => (x, Some(x)), s),
          Pat.match_tup_label,
          expected_labels,
          ps',
          (name, e) => {TupLabel(Label(name) |> Pat.fresh, e) |> Pat.fresh},
        );

      Tuple(ps') |> rewrap;
    | ExplicitNonlabel =>
      raise(Failure("Explicit nonlabel pattern outside of tuplabel"))
    | Label(_) => upat
    | Ap(p1, p2) =>
      let (p1', _) = elaborate_pattern(m, p1);
      let (p2', _) = elaborate_pattern(m, p2);
      Ap(p1', p2') |> rewrap;
    | MultiHole([Pat(p1), ..._]) =>
      /* Elaborate up to the multi-hole */
      let (p1', _) = elaborate_pattern(m, p1);
      p1';
    | MultiHole(_)
    | Invalid(_)
    | EmptyHole
    | Wild => upat
    | Var(_) => upat
    // Type annotations should already appeard
    | Parens(p) =>
      let (p', _) = elaborate_pattern(m, p);
      Parens(p') |> rewrap;
    | Asc(p, t) =>
      let (p', _) = elaborate_pattern(m, p);
      Asc(p', Typ.normalize(ctx, t)) |> rewrap;
    | Constructor(c, _) =>
      let ana_ty =
        switch (Id.Map.find_opt(Pat.rep_id(upat), m)) {
        | Some(Info.InfoPat({ana, _})) => ana
        | _ => raise(MissingTypeInfo)
        };
      let t =
        switch (Self.ctr_ana_typ(ctx, ana_ty, c), Ctx.lookup_ctr(ctx, c)) {
        | (Some(ana_ty), _) => Some(Typ.normalize(ctx, ana_ty))
        | (_, Some({typ: syn_ty, _})) => Some(Typ.normalize(ctx, syn_ty))
        | _ => None
        };
      Constructor(c, Some(t)) |> rewrap;
    };
  (dpat, elaborated_type);
};

let rec elaborate = (m: Statics.Map.t, uexp: Exp.t): (DHExp.t, Typ.t) => {
  // In the case of singleton labeled tuples we update the syntax in Statics.
  // We store this syntax with the same ID as the original expression and store it on the Info.exp in the Statics.map
  // We are then pulling this out and using it in place of the actual expression.

  let (elaborated_type, ana, ctx, co_ctx, statics_pseudo_elaborated) =
    elaborated_type(m, uexp);
  let (_, rewrap) = Exp.unwrap(uexp);
  let uexp = rewrap(statics_pseudo_elaborated.term);

  let (term, rewrap) = Exp.unwrap(uexp);
  let dhexp =
    switch (term) {
    | Invalid(_)
    | Undefined
    | EmptyHole => uexp
    | MultiHole([Exp(e1), Exp(e2)]) =>
      /* Treat two-expression multiholes as seqs */
      Seq(fst(elaborate(m, e1)), fst(elaborate(m, e2))) |> rewrap
    | MultiHole(stuff) =>
      Any.map_term(
        ~f_exp=(_, exp) => {elaborate(m, exp) |> fst},
        ~f_pat=(_, pat) => {elaborate_pattern(m, pat, false) |> fst},
        _,
      )
      |> List.map(_, stuff)
      |> (stuff => MultiHole(stuff) |> rewrap)
    | DynamicErrorHole(e, err) =>
      let (e', _) = elaborate(m, e);
      DynamicErrorHole(e', err) |> rewrap;
    | Asc(e, t) =>
      Asc(elaborate(m, e) |> fst, Typ.normalize(ctx, t)) |> rewrap
    | Parens(e) =>
      let (e', _) = elaborate(m, e);
      Parens(e') |> rewrap;
    | Deferral(_) => uexp
    | Atom(c) =>
      let c =
        Operators.replace_literal(c, Typ.is_ana_atom(ana), ctx.use_mode);
      switch (c) {
      | L(c) => Atom(c) |> rewrap
      | R(BadInt(s)) => Invalid(s) |> rewrap
      };
    | ListLit(es) =>
      let (ds, tys) = List.map(elaborate(m), es) |> List.split;
      let meet_ty =
        Typ.meet_all(~empty=Unknown(Internal) |> Typ.temp, ctx, tys);

      let ds' =
        List.map2((d, t) => fresh_ascription(d, t, meet_ty), ds, tys);
      ListLit(ds') |> rewrap;
    | LivelitName(_) => uexp
    | Constructor(c, _) =>
      let (self, ty) =
        switch (Id.Map.find_opt(Exp.rep_id(uexp), m)) {
        | Some(Info.InfoExp({self, ty, _})) => (self, ty)
        | _ => raise(MissingTypeInfo)
        };
      let t =
        switch (self) {
        | Common(FreeConstructor(_)) => Some(None)
        | _ => Some(Some(Typ.normalize(ctx, ty)))
        };
      Constructor(c, t) |> rewrap;
    | Fun(p, e, _, n) =>
      let (p', typ) = elaborate_pattern(m, p, false);
      let (e', _) = elaborate(m, e);
      Fun(p', e', Some(typ), n) |> rewrap;
    | Forall(p, e) =>
      let (p', _) = elaborate_pattern(m, p, false);
      let (e', _) = elaborate(m, e);
      Forall(p', e') |> rewrap;
    | TypFun(tpat, e, name) =>
      let (e', _) = elaborate(m, e);
      TypFun(tpat, e', name) |> rewrap;
    | Tuple(es) =>
      let (ds, _) = List.map(elaborate(m), es) |> List.split;

      let expected_labels: list(option(string)) =
        Typ.get_labels(ctx, elaborated_type);
      let ds =
        LabeledTuple.rearrange(
          s => Option.map(x => (x, Some(x)), s),
          Exp.match_tup_label,
          expected_labels,
          ds,
          (name, e) => {
            TupLabel(Label(name) |> DHExp.fresh, e) |> DHExp.fresh
          },
        );

      Tuple(ds) |> rewrap;
    | TupLabel(label, e) =>
      switch (label.term) {
      | ExplicitNonlabel => elaborate(m, e) |> fst
      | _ =>
        let (label', _) = elaborate(m, label);
        let (e', _) = elaborate(m, e);
        TupLabel(label', e') |> rewrap;
      }
    | ExplicitNonlabel
    | Label(_) => uexp
    | Dot(e1, e2) =>
      let (e1, _) = elaborate(m, e1);
      Dot(e1, e2) |> rewrap;
    | Var(_) => uexp
    | Let(p, def, body) =>
      let add_name: (option(string), DHExp.t) => DHExp.t = (
        (name, exp) => {
          let (term, rewrap) = DHExp.unwrap(exp);
          switch (term) {
          | Fun(p, e, t, _) => Fun(p, e, t, name) |> rewrap
          | TypFun(tpat, e, _) => TypFun(tpat, e, name) |> rewrap
          | _ => exp
          };
        }
      );
      let (p, ty1) = elaborate_pattern(m, p, false);
      // attach labels if needed for labeled tuples
      let (def_term, def_rewrap) = DHExp.unwrap(def);
      let def =
        switch (def_term, Typ.term_of(Typ.normalize(ctx, ty1))) {
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
      let is_recursive =
        Statics.is_recursive(ctx, p, def, ty1)
        && Pat.get_bindings(p)
        |> Option.get
        |> List.exists(f => VarMap.lookup(co_ctx, f) != None);
      if (!is_recursive) {
        let (def, _) = elaborate(m, def);
        let def = add_name(Pat.get_var(p), def);
        let (body, _) = elaborate(m, body);
        Let(p, def, body) |> rewrap;
      } else {
        // TODO: Add names to mutually recursive functions
        let (def, _) = elaborate(m, def);
        let def = add_name(Option.map(s => s ++ "+", Pat.get_var(p)), def);
        let (body, _) = elaborate(m, body);
        let fixf =
          (FixF(p, def, None): Exp.term)
          |> IdTagged.fresh_deterministic(DHExp.rep_id(uexp));
        Let(p, fixf, body) |> rewrap;
      };
    | Theorem(p, e1, e2) =>
      let (p', _) = elaborate_pattern(m, p, false);
      let (e1', _) = elaborate(m, e1);
      let (e2', _) = elaborate(m, e2);
      Theorem(p', e1', e2') |> rewrap;
    | ProofObject(e) =>
      let (e', _) = elaborate(m, e);
      ProofObject(e') |> rewrap;
    | FixF(p, e, env) =>
      let (p', pty) = elaborate_pattern(m, p, false);
      let (e', _) = elaborate(m, e);
      FixF(p', Asc(e', pty) |> Exp.fresh, env) |> rewrap; // TODO Consider if there's a better strategy than always ascribing the type
    // These forms are removed in elaboration
    | Use(_, e)
    | TyAlias(_, _, e) =>
      let (e', _) = elaborate(m, e);
      e';
    | Ap(dir, f, a) =>
      switch (f.term) {
      | LivelitName(s) =>
        switch (Ctx.lookup_livelit(ctx, s)) {
        | Some(ll) =>
          switch (ll.expand(a)) {
          | Some(ll_expand) => ll_expand
          | None => uexp
          }
        | None => uexp
        }
      | _ =>
        let (f', _) = elaborate(m, f);
        let (a', _) = elaborate(m, a);
        Ap(dir, f', a') |> rewrap;
      }
    | DeferredAp(f, args) =>
      let (f', _) = elaborate(m, f);
      let (args', _) = List.map(elaborate(m), args) |> List.split;
      DeferredAp(f', args') |> rewrap;
    | TypAp(e, ut) =>
      let (e', _) = elaborate(m, e);
      let ut' = Typ.normalize(ctx, ut);
      TypAp(e', ut') |> rewrap;
    | If(c, t, f) =>
      let (c', _) = elaborate(m, c);
      let (t', t_ty) = elaborate(m, t);
      let (f', f_ty) = elaborate(m, f);
      If(
        c',
        fresh_ascription(t', t_ty, Some(elaborated_type)),
        fresh_ascription(f', f_ty, Some(elaborated_type)),
      )
      |> rewrap;
    | Seq(e1, e2) =>
      let (e1', _) = elaborate(m, e1);
      let (e2', _) = elaborate(m, e2);
      Seq(e1', e2') |> rewrap;
    | Test(e) =>
      let (e', _) = elaborate(m, e);
      Test(e') |> rewrap;
    | HintedTest(e1, e2) =>
      let (e1', _) = elaborate(m, e1);
      let (e2', _) = elaborate(m, e2);
      HintedTest(e1', e2') |> rewrap;
    | Filter(kind, e) =>
      let (e', _) = elaborate(m, e);
      let kind' =
        switch (kind) {
        | Residue(_) => kind
        | Filter({act, pat}) =>
          Filter({
            act,
            pat: elaborate(m, pat) |> fst,
          })
        };
      Filter(kind', e') |> rewrap;
    | Closure(env, e) =>
      // Should we be elaborating the contents of the environment?
      let (e', _) = elaborate(m, e);
      Closure(env, e') |> rewrap;
    | Cons(e1, e2) =>
      let (e1', _) = elaborate(m, e1);
      let (e2', _) = elaborate(m, e2);
      Cons(e1', e2')
      |> rewrap
      |> IdTagged.FreshGrammar.Exp.asc(_, elaborated_type);
    | ListConcat(e1, e2) =>
      let (e1', _) = elaborate(m, e1);
      let (e2', _) = elaborate(m, e2);
      ListConcat(e1', e2') |> rewrap;
    | UnOp(Meta(Unquote), e) =>
      switch (e.term) {
      // TODO: confirm whether these types are correct
      | Var("e") =>
        Constructor("$e", Some(Some(Unknown(Internal) |> Typ.fresh)))
        |> rewrap
      | Var("v") =>
        Constructor("$v", Some(Some(Unknown(Internal) |> Typ.fresh)))
        |> rewrap
      | _ => EmptyHole |> rewrap
      }
    | UnOp(op, e) =>
      let op = Operators.replace_un_op(op, ctx.use_mode);
      let (e', _) = elaborate(m, e);
      UnOp(op, e') |> rewrap;
    | BinOp(op, e1, e2) =>
      let op = Operators.replace_bin_op(op, ctx.use_mode);
      let (e1', _) = elaborate(m, e1);
      let (e2', _) = elaborate(m, e2);
      BinOp(op, e1', e2') |> rewrap;
    | TupleExtension(e1, e2) =>
      let (e1', _) = elaborate(m, e1);
      let (e2', _) = elaborate(m, e2);
      TupleExtension(e1', e2') |> rewrap;
    | BuiltinFun(_) => uexp
    | Match(e, cases) =>
      let (e', _) = elaborate(m, e);
      let (ps, es) = List.split(cases);
      let (ps', _) =
        List.map(p => elaborate_pattern(m, p, false), ps) |> List.split;
      let es' =
        List.map(
          e => {
            let (e', ty) = elaborate(m, e);
            fresh_ascription(e', ty, Some(elaborated_type));
          },
          es,
        );
      Match(e', List.combine(ps', es')) |> rewrap;
    | Module(items) =>
      /* Elaborate module by expanding to nested let/type + labeled tuple.
         We elaborate each item's inner expressions and construct the expanded
         form directly, preserving Mod item IDs on the wrapper expressions
         for cursor inspector support. */
      let elaborate_mod_item = (item: Mod.t, body: Exp.t): Exp.t => {
        let item_id = Mod.rep_id(item);
        switch (item.term) {
        | ModLet(pat, def) =>
          let (pat', _) = elaborate_pattern(m, pat, false);
          let (def', _) = elaborate(m, def);
          /* Preserve ModLet's ID on wrapper Let for cursor inspector */
          IdTagged.fast_copy(item_id, Exp.fresh(Let(pat', def', body)));
        | ModType(tpat, typ) =>
          /* Type aliases don't need elaboration of their type */
          IdTagged.fast_copy(item_id, Exp.fresh(TyAlias(tpat, Typ.normalize(ctx, typ), body)));
        | ModExp(e) =>
          /* Bare expression: fresh ID since ModExp is synthetic.
             The inner expression e keeps its original IDs. */
          let (e', _) = elaborate(m, e);
          let wild_pat = Pat.fresh(Wild);
          Exp.fresh(Let(wild_pat, e', body));
        | Invalid(_)
        | EmptyHole
        | MultiHole(_) =>
          /* Error cases - skip the item */
          body
        };
      };

      /* Build the labeled tuple body (fresh ID, not Module's ID) */
      let non_shadowed = ExpandModule.compute_non_shadowed_bindings(items);
      let tuple_body = ExpandModule.build_labeled_tuple(non_shadowed);

      /* Wrap with elaborated items from bottom to top */
      List.fold_right(elaborate_mod_item, items, tuple_body);
    };
  (dhexp, elaborated_type);
};

//let dhexp_of_uexp = Core.Memo.general(~cache_size_bound=1000, dhexp_of_uexp);

/* This function gives a new id to all the types
   in the expression. It does this to get rid of
   all the invalid ids we added to prevent generating
   too many new ids */
let fix_typ_ids =
  Exp.map_term(~f_typ=(cont, e) => e |> IdTagged.new_ids |> cont);

let uexp_elab = (m: Statics.Map.t, uexp: Exp.t): ElaborationResult.t => {
  switch (elaborate(m, uexp)) {
  | exception MissingTypeInfo => DoesNotElaborate
  | (d, ty) => Elaborates(d |> fix_typ_ids, ty)
  };
};
