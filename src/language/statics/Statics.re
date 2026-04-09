/* STATICS.re

   This module determines the statics semantics of a program.
   It makes use of the following modules:

   INFO.re: Defines the Info.t type which is used to represent the
   static STATUS of a term. This STATUS can be either OK or ERROR,
   and is determined by reconcilling two sources of typing information,
   the ANA and the SELF.

   (ana:Typ.t): Defines the Mode.t type which is used to represent the
   typing expectations imposed by a term's ancestors.

   SELF.re: Define the Mark.t type which is used to represent the
   type information derivable from the term itself.

   The point of STATICS.re itself is to derive a map between each
   term's unique id and that term's static INFO. The below functions
   are intended mostly as infrastructure: The point is to define a
   traversal through the syntax tree which, for each term, passes
   down the MODE, passes up the SELF, calculates the INFO, and adds
   it to the map.

   The architectural intention here is that most type-manipulation
   logic is defined in INFO, MODE, and SELF, and the STATICS module
   itself is dedicated to the piping necessary to (A) introduce and
   (B) propagate the necessary information through the syntax tree.

    */

include StaticsBase;
let add_info = Map.add_info;
let add_missing_info = Map.add_missing_info;

type typ_status = (list(Mark.t), option(Message.ok_typ));
type tpat_status = (list(Mark.t), option(Message.ok_tpat));

let show_typ_status = ((marks, ok): typ_status): string =>
  "{" ++ [%derive.show: list(Mark.t)](marks)
  ++ ", "
  ++ [%derive.show: option(Message.ok_typ)](ok)
  ++ "}";

let equal_typ_status = ((m1, o1): typ_status, (m2, o2): typ_status): bool =>
  m1 == m2 && o1 == o2;

let derive_tpat_status = (_ctx: Ctx.t, utpat: TPat.t): tpat_status =>
  switch (utpat.term) {
  | EmptyHole => ([], Some(Message.Empty))
  | Var(name) when Ctx.is_base_typ(name) =>
    ([TPatShadowsType(name, BaseTyp)], None)
  | Var(name) => ([], Some(Message.Var(name)))
  | Invalid(_) => ([TPatNotAVar(NotCapitalized)], None)
  | MultiHole(_) => ([TPatNotAVar(Other)], None)
  };

/* Label/tuple helper cluster */
let derive_label_inference_info = (original_labels, new_labels) => {
  let introduced_labels =
    List.filter(
      l => !List.mem(l, List.filter_map(Fun.id, original_labels)),
      List.filter_map(Fun.id, new_labels),
    );
  let reordered =
    !
      List.equal(
        (a, b) => {
          switch (a, b) {
          | (Some(a), Some(b)) => a == b
          | (Some(a), None) => List.mem(a, introduced_labels)
          | (None, Some(_)) => false
          | (None, None) => true
          }
        },
        new_labels,
        original_labels,
      );
  Info.MultiLabelInference({
    reordered,
    introduced_labels,
  });
};

/* Meet/NoMeet helper cluster */
let should_emit_nomeet_mark = (ctx: Ctx.t, ana: Typ.t, syn_ty: Typ.t): bool =>
  switch (Typ.meet(ctx, ana_skip_explicit_nonlabel(ana), syn_ty)) {
  | Some(_) => false
  | None => true
  };

let add_source =
  List.map2((id, ty) =>
    Typ.{
      id,
      ty,
    }
  );

/* Constructor synthesis helper cluster */
let free_constructor_syn_ty = (name: Constructor.t): Typ.t =>
  Sum([
    ConstructorMap.Variant(
      name,
      ConstructorMap.mk_variant_ann(~ids=[Id.invalid], ()),
      None,
    ),
    ConstructorMap.BadEntry(SynTy.unknown_internal()),
  ])
  |> Typ.temp;

let syn_marks_match =
    (ctx: Ctx.t, tys: list(Typ.t), ids: list(Id.t)): (Typ.t, list(Mark.t)) =>
  switch (Typ.meet_all(~empty=Unknown(Internal) |> Typ.fresh, ctx, tys)) {
  | None => (
      SynTy.meet_of(Id, SynTy.unknown_internal()),
      [Mark.NoMeet(Id, add_source(ids, tys))],
    )
  | Some(ty) => (ty, [])
  };

let ctr_ana_typ =
    (ctx: Ctx.t, ty_ana: Typ.t, ctr: Constructor.t): option(Typ.t) => {
  Util.OptUtil.Syntax.(
    switch (ty_ana) {
    | {term: Arrow(_, ty_out), _} =>
      let* ctrs = Typ.get_sum_constructors(ctx, ty_out);
      let* ty_entry = ConstructorMap.get_entry(ctr, ctrs);
      switch (ty_entry) {
      | None => None
      | Some(ty_in) => Some(Arrow(ty_in, ty_out) |> Typ.temp)
      };
    | _ =>
      let* ctrs = Typ.get_sum_constructors(ctx, ty_ana);
      let+ ty_entry = ConstructorMap.get_entry(ctr, ctrs);
      switch (ty_entry) {
      | None => ty_ana
      | Some(ty_in) => Arrow(ty_in, ty_ana) |> Typ.temp
      };
    }
  );
};

let syn_marks_ctr =
    (ctx: Ctx.t, name: Constructor.t, ana: Typ.t, ty: option(option(Typ.t)))
    : (Typ.t, list(Mark.t)) =>
  switch (ty) {
  | Some(Some(ty)) => (ty, [])
  | Some(None) => (free_constructor_syn_ty(name), [Mark.FreeConstructor(name)])
  | None =>
    switch (ctr_ana_typ(ctx, ana, name)) {
    | Some(ty) => (ty, [])
    | None =>
      switch (Ctx.lookup_ctr(ctx, name)) {
      | Some({typ, _}) => (typ, [])
      | None => (free_constructor_syn_ty(name), [Mark.FreeConstructor(name)])
      }
    }
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
  let mk_exp_info =
      (
        ~uexp: Exp.t=uexp,
        ~ctx=ctx,
        ~ana=ana,
        ~ancestors=ancestors,
        ~syn_ty: Typ.t,
        ~marks: list(Mark.t)=[],
        ~warnings: list(Warning.list_item)=[],
        ~co_ctx: CoCtx.t,
        ~label_inference: option(Info.label_inference(Info.exp))=None,
        ~inferred_label: option(LabeledTuple.label)=None,
        ~dot_labels: list(string)=[],
        ~label_sort=false,
        (),
      ): Info.exp => {
    let marks =
      switch (uexp.term) {
      | Deferral(InAp) => marks
      | _ when marks != [] => marks
      | _ =>
        switch (expectation_mismatch_mark(ctx, ana, syn_ty)) {
        | None => marks
        | Some(m) => marks @ [m]
        }
      };
    let message =
      marks != []
        ? Message.Exp(Message.Default)
        : Message.Exp(
            switch (uexp.term) {
            | Deferral(InAp) => Message.AnaDeferralConsistent(ana)
            | _ =>
              switch (ana) {
              | {term: Unknown(SynSwitch), _} => Message.Default
              | _ => Message.Common(syn_ana_ok_common(ctx, ana, syn_ty))
              }
            },
          );
    let cls = Cls.Exp(Exp.cls_of_term(uexp.term));
    let ty = fixed_typ(ctx, ana, syn_ty);
    {
      cls,
      syn_ty,
      marks,
      ty,
      ana,
      message,
      warnings,
      ctx,
      co_ctx,
      ancestors,
      user_term: uexp,
      label_inference,
      inferred_label,
      label_sort,
      dot_labels,
    };
  };
  let add =
      (
        ~elab: Exp.t=uexp,
        ~label_inference: option(Info.label_inference(Info.exp))=None,
        ~syn_ty: Typ.t,
        ~marks: list(Mark.t)=[],
        ~warnings: list(Warning.list_item)=[],
        ~co_ctx: CoCtx.t,
        m: Map.t,
      )
      : (Info.exp, Exp.t, Map.t) => {
    let info: Info.exp =
      mk_exp_info(
        ~syn_ty,
        ~marks,
        ~warnings,
        ~co_ctx,
        ~label_inference,
        ~inferred_label=None,
        ~dot_labels=[],
        ~label_sort=false,
        (),
      );
    (info, elab, add_info(ids, InfoExp(info), m));
  };
  /* Passed into CustomStatics functor — must not be shadowed by inner `let add`. */
  let add_for_custom = add;
  let ancestors = [Exp.rep_id(uexp)] @ ancestors;
  let go =
      (
        ~ctx=ctx,
        ~ana=syn,
        ~is_in_filter=is_in_filter,
        ~ancestors=ancestors,
        uexp: Exp.t,
        m: Map.t,
      )
      : (Info.exp, Exp.t, Map.t) => {
    uexp_to_info_map(~ctx, ~ana, ~is_in_filter, ~ancestors, uexp, m);
  };
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
  let go_pat = upat_to_info_map(~ctx, ~ancestors);
  let go_typ = utyp_to_info_map(~ctx, ~ancestors);
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
        ~ancestors,
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
      uexp_to_info_map(~ctx, ~ana, ~ancestors, elaborated_exp, m);
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

  // This is the case where we aren't a singleton labeled tuple
  let default_case = () => {
    switch (term) {
    | Closure(env, e) =>
      // TODO: implement closure type checking properly - see how dynamic type assignment does it
      let (e, e_elab, m) = go(~ana, e, m);
      add(
        ~elab=Closure(env, e_elab) |> rewrap,
        ~syn_ty=e.ty,
        ~marks=[],
        ~co_ctx=e.co_ctx,
        m,
      );
    | MultiHole([Exp(e1), Exp(e2)]) =>
      let (e1, e1_elab, m) = go(~ana=syn, e1, m);
      let (e2, e2_elab, m) = go(~ana=syn, e2, m);
      add(
        ~elab=Seq(e1_elab, e2_elab) |> rewrap,
        ~syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[IsMulti],
        ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
        m,
      );
    | MultiHole(tms) =>
      let (co_ctxs, tms_elab, m) = multi(~ctx, ~ancestors, m, tms);
      add(
        ~elab=MultiHole(tms_elab) |> rewrap,
        ~syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[IsMulti],
        ~co_ctx=CoCtx.union(co_ctxs),
        m,
      );
    | Asc(e, t2) =>
      let (t, m) = go_typ(t2, ~expects=TypExpectation.TypeExpected, m);
      /* Desugar any Sig types in the annotation without full normalization */
      let t_ty = Typ.desugar_sig(ctx, t.user_term);
      let (e, e_elab, m) = go(~ana=t_ty, ~ctx=t.ctx, e, m);
      let typ_refs =
        ModuleHelpers.collect_module_refs_in_typ(ctx, Typ.rep_id(t2), t2);
      add(
        ~elab=Asc(e_elab, Typ.normalize(ctx, t2)) |> rewrap,
        ~syn_ty=t_ty,
        ~marks=[],
        ~co_ctx=CoCtx.union([e.co_ctx, typ_refs]),
        m,
      );
    | Invalid(token) =>
      add(
        ~syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[BadToken(token)],
        ~co_ctx=hole_co_ctx,
        m,
      )
    | EmptyHole =>
      add(
        ~syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[],
        ~co_ctx=hole_co_ctx,
        m,
      )
    | Deferral(position) =>
      add(
        ~syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=
          switch (position) {
          | InAp => []
          | OutsideAp => [IsDeferral(position)]
          },
        ~co_ctx=CoCtx.empty,
        m,
      )
    | Undefined =>
      add(
        ~syn_ty=Unknown(Hole(EmptyHole)) |> Typ.temp,
        ~marks=[],
        ~co_ctx=CoCtx.empty,
        m,
      )
    | Atom(c) =>
      let c =
        Operators.replace_literal(c, Typ.is_ana_atom(ana), ctx.use_mode); // Replace literal if necessary due to `use`
      switch (c) {
      | L(c) =>
        let ty = Atom(Atom.cls_of_t(c)) |> Typ.temp;
        add(
          ~elab=Atom(c) |> rewrap,
          ~syn_ty=ty,
          ~marks=[],
          ~co_ctx=CoCtx.empty,
          m,
        );
      | R(BadInt(str)) =>
        add(
          ~elab=Invalid(str) |> rewrap,
          ~syn_ty=Unknown(Internal) |> Typ.temp,
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
        ~syn_ty=syn_lit,
        ~marks=marks_lit,
        ~co_ctx=CoCtx.singleton(name, Exp.rep_id(uexp), ana),
        m,
      );
    | ListLit(es) =>
      let ids = List.map(Exp.rep_id, es);
      let inner_ana_ty = MatchedTyp.list_tolerant(ctx, ana);
      let anas = List.init(List.length(es), _ => inner_ana_ty);
      let ((es, es_elabs), m) = map_m_go(m, anas, es);
      let tys = List.map(Info.exp_ty, es);
      let meet_ty =
        Typ.meet_all(~empty=Unknown(Internal) |> Typ.temp, ctx, tys);
      let ds =
        List.map2(
          (d, t) => fresh_ascription(ctx, d, t, meet_ty),
          es_elabs,
          tys,
        );
      switch (meet_ty) {
      | None =>
        let syn_no_meet = SynTy.meet_of(List, Unknown(Internal) |> Typ.temp);
        add(
          ~elab=ListLit(ds) |> rewrap,
          ~syn_ty=syn_no_meet,
          ~marks=
            should_emit_nomeet_mark(ctx, ana, syn_no_meet)
              ? [NoMeet(List, add_source(ids, tys))]
              : [],
          ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
          m,
        )
      | Some(ty) =>
        add(
          ~elab=ListLit(ds) |> rewrap,
          ~syn_ty=List(ty) |> Typ.temp,
          ~marks=[],
          ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
          m,
        )
      };
    | Cons(hd, tl) =>
      let inner_ana_ty = MatchedTyp.list_tolerant(ctx, ana);
      let (hd, hd_elab, m) = go(~ana=inner_ana_ty, hd, m);
      let (tl, tl_elab, m) =
        go(
          ~ana=
            List(Typ.is_syn(inner_ana_ty) ? hd.ty : inner_ana_ty) |> Typ.temp,
          tl,
          m,
        );
      let self_ty = List(hd.ty) |> Typ.temp;
      let elab_ty =
        Typ.match_synswitch(ana, self_ty)
        |> Typ.normalize(ctx)
        |> Typ.all_ids_temp;
      let elab =
        Cons(hd_elab, tl_elab)
        |> rewrap
        |> IdTagged.FreshGrammar.Exp.asc(_, elab_ty);
      add(
        ~elab,
        ~syn_ty=self_ty,
        ~marks=[],
        ~co_ctx=CoCtx.union([hd.co_ctx, tl.co_ctx]),
        m,
      );
    | ListConcat(e1, e2) =>
      let inner_ana_ty =
        List(MatchedTyp.list_tolerant(ctx, ana)) |> Typ.temp;
      let ids = List.map(Exp.rep_id, [e1, e2]);
      let (e1, e1_elab, m) = go(~ana=inner_ana_ty, e1, m);
      let (e2, e2_elab, m) = go(~ana=inner_ana_ty, e2, m);
      switch (
        Typ.meet_all(
          ~empty=Unknown(Internal) |> Typ.temp,
          ctx,
          [e1.ty, e2.ty],
        )
      ) {
      | None =>
        let syn_no_meet = SynTy.meet_of(List, Unknown(Internal) |> Typ.temp);
        add(
          ~elab=ListConcat(e1_elab, e2_elab) |> rewrap,
          ~syn_ty=syn_no_meet,
          ~marks=
            should_emit_nomeet_mark(ctx, ana, syn_no_meet)
              ? [NoMeet(List, add_source(ids, [e1.ty, e2.ty]))]
              : [],
          ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
          m,
        )
      | Some(ty) =>
        add(
          ~elab=ListConcat(e1_elab, e2_elab) |> rewrap,
          ~syn_ty=ty,
          ~marks=[],
          ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
          m,
        )
      };
    | Var(name) =>
      let co_ctx = CoCtx.singleton(name, Exp.rep_id(uexp), ana);

      let (syn_v, marks_v) =
        switch (Ctx.lookup_var(ctx, name)) {
        | None => (SynTy.unknown_internal(), [Mark.Free(name)])
        | Some(var) => (var.typ, [])
        };
      add(~syn_ty=syn_v, ~marks=marks_v, ~co_ctx, m);
    | DynamicErrorHole(e, err) =>
      let (e, e_elab, m) = go(~ana, e, m);
      add(
        ~elab=DynamicErrorHole(e_elab, err) |> rewrap,
        ~syn_ty=e.syn_ty,
        ~marks=e.marks,
        ~co_ctx=e.co_ctx,
        m,
      );
    | Parens(e) =>
      let (e, e_elab, m) = go(~ana, e, m);
      add(
        ~elab=Parens(e_elab) |> rewrap,
        ~syn_ty=e.syn_ty,
        ~marks=e.marks,
        ~co_ctx=e.co_ctx,
        m,
      );
    | Projector(data, e) =>
      let (e, e_elab, m) = go(~ana, e, m);
      add(
        ~elab=Projector(data, e_elab) |> rewrap,
        ~syn_ty=e.syn_ty,
        ~marks=e.marks,
        ~co_ctx=e.co_ctx,
        m,
      );
    | UnOp(Meta(Unquote), e) =>
      let unquote_elab =
        switch (e.term) {
        | Var("e") =>
          Constructor("$e", Some(Some(Unknown(Internal) |> Typ.fresh)))
          |> rewrap
        | Var("v") =>
          Constructor("$v", Some(Some(Unknown(Internal) |> Typ.fresh)))
          |> rewrap
        | _ => EmptyHole |> rewrap
        };
      let (e, _, m) =
        if (is_in_filter) {
          let e: Exp.t = {
            annotation: IdTagged.IdTag.mk_internal(IdTagged.ids(e)),
            term:
              switch (e.term) {
              | Var("e") =>
                Constructor(
                  "$e",
                  Some(Some(Unknown(Internal) |> Typ.fresh)),
                )
              | Var("v") =>
                Constructor(
                  "$v",
                  Some(Some(Unknown(Internal) |> Typ.fresh)),
                )
              | _ => e.term
              },
          };
          go(~ana=Var("$Meta") |> Typ.temp, e, m);
        } else {
          go(~ana=syn, e, m);
        };
      let (info, elab, m) =
        if (is_in_filter) {
          add(
            ~elab=unquote_elab,
            ~syn_ty=Unknown(Internal) |> Typ.temp,
            ~marks=[],
            ~co_ctx=e.co_ctx,
            m,
          );
        } else {
          add(
            ~elab=unquote_elab,
            ~syn_ty=Unknown(Internal) |> Typ.temp,
            ~marks=[BadOperator("Unquote not in filter")],
            ~co_ctx=e.co_ctx,
            m,
          );
        };
      let m =
        switch (unquote_elab.term) {
        | Constructor(_, Some(Some(typ))) =>
          go_typ(typ, ~expects=TypExpectation.TypeExpected, m) |> snd
        | _ => m
        };
      (
        info,
        elab,
        IdTagged.ids(unquote_elab)
        |> add_missing_info(_, Info.InfoExp(info), m),
      );
    | UnOp(op, e) =>
      let op = Operators.replace_un_op(op, ctx.use_mode); // Replace op if necessary due to `use`
      let op_semantics = Operators.semantics_of_un_op(op);
      switch (op_semantics) {
      | Undefined(msg) =>
        let (e, e_elab, m) = go(~ana=syn, e, m);
        add(
          ~elab=UnOp(op, e_elab) |> rewrap,
          ~syn_ty=Unknown(Internal) |> Typ.temp,
          ~marks=[BadOperator(msg)],
          ~co_ctx=e.co_ctx,
          m,
        );
      | Defined(ty_in, ty_out, _) =>
        let ty_in = Atom(Atom.cls_of_kind(ty_in)) |> Typ.temp;
        let ty_out = Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp;
        let (e, e_elab, m) = go(~ana=ty_in, e, m);
        add(
          ~elab=UnOp(op, e_elab) |> rewrap,
          ~syn_ty=ty_out,
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
        let (e1, e1_elab, m) = go(~ana=syn, e1, m);
        let (e2, e2_elab, m) = go(~ana=syn, e2, m);
        add(
          ~elab=BinOp(op, e1_elab, e2_elab) |> rewrap,
          ~syn_ty=Unknown(Internal) |> Typ.temp,
          ~marks=[BadOperator(msg)],
          ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
          m,
        );
      | DefinedPoly(_) =>
        let ids = List.map(Exp.rep_id, [e1, e2]);
        let ((es, es_elabs), m) =
          map_m_go(
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
            ~elab=elab_poly,
            ~syn_ty=SynTy.meet_of(PolyEq, Unknown(Internal) |> Typ.temp),
            ~marks=[NoMeet(PolyEq, add_source(ids, tys))],
            ~co_ctx=co_poly,
            m,
          )
        | Some(ty) when Typ.normalize(ctx, ty) |> Typ.has_fun =>
          add(
            ~elab=elab_poly,
            ~syn_ty=Atom(Bool) |> Typ.fresh,
            ~marks=[CompareFun(ty)],
            ~co_ctx=co_poly,
            m,
          )
        | Some(_) =>
          add(
            ~elab=elab_poly,
            ~syn_ty=Atom(Bool) |> Typ.fresh,
            ~marks=[],
            ~co_ctx=co_poly,
            m,
          )
        };
      | Defined(ty1, ty2, ty_out, _) =>
        let ty1 = Atom(Atom.cls_of_kind(ty1)) |> Typ.temp;
        let ty2 = Atom(Atom.cls_of_kind(ty2)) |> Typ.temp;
        let ty_out = Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp;
        let (e1, e1_elab, m) = go(~ana=ty1, e1, m);
        let (e2, e2_elab, m) = go(~ana=ty2, e2, m);
        add(
          ~elab=BinOp(op, e1_elab, e2_elab) |> rewrap,
          ~syn_ty=ty_out,
          ~marks=[],
          ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
          m,
        );
      };
    | TupleExtension(e1, e2) =>
      let (t1, e1_elab, m) = {
        let (t1, e1_elab, m) = go(e1, m);
        switch (Typ.normalize(ctx, t1.ty).term) {
        | Prod(_)
        | Unknown(_) => (t1, e1_elab, m)
        | _ =>
          let t1 =
            mk_exp_info(
              ~uexp=t1.user_term,
              ~ctx=t1.ctx,
              ~ana=t1.ana,
              ~ancestors=t1.ancestors,
              ~syn_ty=SynTy.unknown_internal(),
              ~marks=[TupleExtensionRequiresTuples],
              ~co_ctx=t1.co_ctx,
              ~label_inference=t1.label_inference,
              ~inferred_label=t1.inferred_label,
              ~dot_labels=t1.dot_labels,
              ~label_sort=t1.label_sort,
              ~warnings=t1.warnings,
              (),
            );
          let m = add_info(IdTagged.ids(t1.user_term), InfoExp(t1), m);
          (t1, e1_elab, m);
        };
      };
      let (t2, e2_elab, m) = {
        let (t2, e2_elab, m) = go(e2, m);
        switch (Typ.normalize(ctx, t2.ty).term) {
        | Prod(_)
        | Unknown(_) => (t2, e2_elab, m)
        | _ =>
          let t2 =
            mk_exp_info(
              ~uexp=t2.user_term,
              ~ctx=t2.ctx,
              ~ana=t2.ana,
              ~ancestors=t2.ancestors,
              ~syn_ty=SynTy.unknown_internal(),
              ~marks=[TupleExtensionRequiresTuples],
              ~co_ctx=t2.co_ctx,
              ~label_inference=t2.label_inference,
              ~inferred_label=t2.inferred_label,
              ~dot_labels=t2.dot_labels,
              ~label_sort=t2.label_sort,
              ~warnings=t2.warnings,
              (),
            );
          let m = add_info(IdTagged.ids(t2.user_term), InfoExp(t2), m);
          (t2, e2_elab, m);
        };
      };
      let co_ctx = CoCtx.union([t1.co_ctx, t2.co_ctx]);
      let elab = TupleExtension(e1_elab, e2_elab) |> rewrap;

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

        add(~elab, ~syn_ty=ty, ~marks=[], ~co_ctx, m);
      | (Unknown(_), _)
      | (_, Unknown(_)) =>
        add(
          ~elab,
          ~syn_ty=IdTagged.FreshGrammar.Typ.unknown(Internal),
          ~marks=[],
          ~co_ctx,
          m,
        )
      | _ =>
        add(
          ~elab,
          ~syn_ty=IdTagged.FreshGrammar.Typ.unknown(Internal),
          ~marks=[],
          ~co_ctx,
          m,
        )
      };

    | Tuple(es) =>
      let expected_labels =
        switch (Typ.weak_head_normalize(ctx, ana).term) {
        | Prod(ts) =>
          Some(
            List.filter_map(
              t => Typ.match_tup_label(t) |> Option.map(fst),
              ts,
            ),
          )
        | _ => None
        };

      let original_labels =
        List.map(e => Exp.match_tup_label(e) |> Option.map(fst), es);

      let (inferred_es, ana_tys) =
        MatchedTyp.prod(
          ctx,
          List.map(e => (None: option(string), e), es),
          ((inferred, e)) => {
            Exp.match_tup_label(e)
            |> Option.map(((label, element)) =>
                 (label, (inferred, element))
               )
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
      /* Build list with one entry per duplicate occurrence (matching old behavior) */
      let duplicate_labels =
        List.filter_map(
          (e: Exp.t) =>
            switch (Exp.match_tup_label(e)) {
            | Some((name, _)) when List.mem(name, unique_duplicate_labels) =>
              Some(name)
            | _ => None
            },
          es,
        );

      let invalid_labels =
        switch (expected_labels) {
        | None => []
        | Some(expected) =>
          List.filter_map(
            (e: Exp.t) =>
              switch (Exp.match_tup_label(e)) {
              | Some((name, _)) when !List.mem(name, expected) => Some(name)
              | _ => None
              },
            es,
          )
        };

      let (es', es_elab, m) =
        List.fold_left2(
          ((es, es_elab, m), ana, (inferred_label, e: Exp.t)) =>
            switch (e.term) {
            | TupLabel({term: ExplicitNonlabel, _}, _) =>
              let (e_info, elab, m) = go(~ana, e, m);
              let e_info =
                switch (inferred_label) {
                | Some(_) => {...e_info, inferred_label}
                | None => e_info
                };
              let m =
                switch (inferred_label) {
                | Some(_) =>
                  add_info(
                    IdTagged.ids(e_info.user_term),
                    InfoExp(e_info),
                    m,
                  )
                | None => m
                };
              (es @ [e_info], es_elab @ [elab], m);
            | TupLabel(label, value) =>
              let (labmode, val_mode) =
                switch (MatchedTyp.label(ctx, ana)) {
                | Some((labmode, val_mode)) => (labmode, val_mode)
                | _ => (
                    Unknown(SynSwitch) |> Typ.temp,
                    Unknown(Internal) |> Typ.temp,
                  )
                };
              let (value_info, _, m) = go(~ana=val_mode, value, m);
              let (lab_name, label_invalid, m) =
                switch (label.term) {
                | Label(name) =>
                  let (label_syn, label_marks, label_invalid) =
                    switch (expected_labels) {
                    | Some(expected) when !List.mem(name, expected) => (
                        SynTy.unknown_internal(),
                        [Mark.InvalidLabel(name, expected)],
                        true,
                      )
                    | _ =>
                      List.mem(name, duplicate_labels)
                        ? (
                            Label(name) |> Typ.temp,
                            [Mark.DuplicateLabel(name, Label(name) |> Typ.temp)],
                            false,
                          )
                        : (Label(name) |> Typ.temp, [], false)
                    };
                  let label_info =
                    mk_exp_info(
                      ~uexp=label,
                      ~ctx,
                      ~ana=labmode,
                      ~ancestors,
                      ~syn_ty=label_syn,
                      ~marks=label_marks,
                      ~co_ctx=CoCtx.empty,
                      ~label_inference=None,
                      ~inferred_label=None,
                      ~dot_labels=[],
                      ~label_sort=true,
                      ~warnings=[],
                      (),
                    );
                  (
                    Some(name),
                    label_invalid,
                    add_info(IdTagged.ids(label), InfoExp(label_info), m),
                  );
                | EmptyHole =>
                  let label_info =
                    mk_exp_info(
                      ~uexp=label,
                      ~ctx,
                      ~ana=labmode,
                      ~ancestors,
                      ~syn_ty=Unknown(SynSwitch) |> Typ.temp,
                      ~marks=[],
                      ~co_ctx=CoCtx.empty,
                      ~label_inference=None,
                      ~inferred_label=None,
                      ~dot_labels=[],
                      ~label_sort=true,
                      ~warnings=[],
                      (),
                    );
                  (None, false, add_info(IdTagged.ids(label), InfoExp(label_info), m));
                | _ =>
                  /* Malformed label — analyze via go to cover sub-expression IDs */
                  let (i, _, m) = go(~ana=labmode, label, m);
                  let i =
                    mk_exp_info(
                      ~uexp=i.user_term,
                      ~ctx=i.ctx,
                      ~ana=i.ana,
                      ~ancestors=i.ancestors,
                      ~syn_ty=i.syn_ty,
                      ~marks=i.marks @ [BadLabel(Exp(label))],
                      ~co_ctx=i.co_ctx,
                      ~label_inference=i.label_inference,
                      ~inferred_label=i.inferred_label,
                      ~dot_labels=i.dot_labels,
                      ~label_sort=true,
                      ~warnings=i.warnings,
                      (),
                    );
                  (None, false, add_info(IdTagged.ids(i.user_term), InfoExp(i), m));
                };
              let (syn_tl, cms_tl) =
                switch (lab_name) {
                | Some(name) =>
                  let tup_syn =
                    TupLabel(Label(name) |> Typ.temp, value_info.ty) |> Typ.temp;
                  label_invalid
                    ? (
                        tup_syn,
                        [
                          Mark.TupleLabelError({
                            malformed_labels: [],
                            duplicate_labels: [],
                            invalid_labels: [name],
                            typ: tup_syn,
                          }),
                        ],
                      )
                    : List.mem(name, duplicate_labels)
                    ? (
                        tup_syn,
                        [
                          Mark.TupleLabelError({
                            malformed_labels: [],
                            duplicate_labels: [name],
                            invalid_labels: [],
                            typ: tup_syn,
                          }),
                        ],
                      )
                    : (tup_syn, [])
                | None =>
                  switch (label.term) {
                  | EmptyHole => (
                      TupLabel(Unknown(SynSwitch) |> Typ.temp, value_info.ty)
                      |> Typ.temp,
                      [],
                    )
                  | _ => (
                      TupLabel(Unknown(Internal) |> Typ.temp, value_info.ty)
                      |> Typ.temp,
                      [
                        Mark.TupleLabelError({
                          malformed_labels: [Exp(label)],
                          duplicate_labels: [],
                          invalid_labels: [],
                          typ:
                            TupLabel(Unknown(Internal) |> Typ.temp, value_info.ty)
                            |> Typ.temp,
                        }),
                      ],
                    )
                  }
                };
              let e_info =
                mk_exp_info(
                  ~uexp=e,
                  ~ctx,
                  ~ana,
                  ~ancestors,
                  ~syn_ty=syn_tl,
                  ~marks=cms_tl,
                  ~co_ctx=value_info.co_ctx,
                  ~label_inference=None,
                  ~inferred_label,
                  ~dot_labels=[],
                  ~label_sort=false,
                  ~warnings=[],
                  (),
                );
              let m = add_info(IdTagged.ids(e), InfoExp(e_info), m);
              (es @ [e_info], es_elab @ [e], m);
            | _ =>
              let (e_info, elab, m) = go(~ana, e, m);
              let e_info =
                switch (inferred_label) {
                | Some(_) => {...e_info, inferred_label}
                | None => e_info
                };
              let m =
                switch (inferred_label) {
                | Some(_) =>
                  add_info(
                    IdTagged.ids(e_info.user_term),
                    InfoExp(e_info),
                    m,
                  )
                | None => m
                };
              (es @ [e_info], es_elab @ [elab], m);
            },
          ([], [], m),
          ana_tys,
          List.combine(inferred, es),
        );

      let ty_list = List.map(Info.exp_ty, es');

      /* Collect malformed label errors from children.
         Duplicate and invalid labels are already known from above. */
      let malformed_labels =
        List.fold_left2(
          (a, e: Exp.t, e_info: Info.exp) => {
            switch (e.term, MarkSelection.highest_ranked_mark(e_info.marks)) {
            | (
                TupLabel(_, _),
                Some(Mark.TupleLabelError({malformed_labels, _})),
              ) =>
              a @ malformed_labels
            | _ => a
            }
          },
          [],
          es,
          es',
        );

      let ty_list = Typ.remove_duplicate_labels(~duplicate_labels, ty_list);
      /* Strip TupLabel wrapper from invalid labels */
      let ty_list =
        List.map(
          ty =>
            switch (Typ.match_tup_label(ty)) {
            | Some((name, inner)) when List.mem(name, invalid_labels) => inner
            | _ => ty
            },
          ty_list,
        );

      let prod_ty = Prod(ty_list) |> Typ.temp;
      let (syn_tuple, cms_tuple) =
        List.is_empty(malformed_labels)
        && List.is_empty(duplicate_labels)
        && List.is_empty(invalid_labels)
          ? (prod_ty, [])
          : (
            prod_ty,
            [
              Mark.TupleLabelError({
                malformed_labels,
                duplicate_labels,
                invalid_labels,
                typ: prod_ty,
              }),
            ],
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
        ~elab=tuple_elab,
        ~syn_ty=syn_tuple,
        ~marks=cms_tuple,
        ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es')),
        ~label_inference=
          Some(
            derive_label_inference_info(original_labels, new_labels),
          ),
        m,
      );
    | TupLabel({term: ExplicitNonlabel, _} as label, e) =>
      let (e, _, m) = go(~ana, e, m);
      /* Add info for the ExplicitNonlabel directly */
      let m =
        add_info(
          IdTagged.ids(label),
          InfoExp(
            mk_exp_info(
              ~uexp=label,
              ~ctx,
              ~ana=syn,
              ~ancestors,
              ~syn_ty=ExplicitNonlabel |> Typ.temp,
              ~marks=[],
              ~co_ctx=CoCtx.empty,
              ~label_inference=None,
              ~inferred_label=None,
              ~dot_labels=[],
              ~label_sort=true,
              ~warnings=[],
              (),
            ),
          ),
          m,
        );
      add(~syn_ty=e.ty, ~marks=[], ~co_ctx=e.co_ctx, m);
    | TupLabel(label, e) =>
      let (labmode, val_mode) =
        switch (MatchedTyp.label(ctx, ana)) {
        | Some((labmode, val_mode)) => (labmode, val_mode)
        | _ => (
            Unknown(SynSwitch) |> Typ.temp,
            Unknown(Internal) |> Typ.temp,
          )
        };
      /* Analyze value child */
      let (e, _, m) = go(~ana=val_mode, e, m);
      /* Add info for the label child directly — TupLabel owns its label */
      let (lab_name, m) =
        switch (label.term) {
        | Label(name) =>
          let lab_info =
            mk_exp_info(
              ~uexp=label,
              ~ctx,
              ~ana=labmode,
              ~ancestors,
              ~syn_ty=Label(name) |> Typ.temp,
              ~marks=[],
              ~co_ctx=CoCtx.empty,
              ~label_inference=None,
              ~inferred_label=None,
              ~dot_labels=[],
              ~label_sort=true,
              ~warnings=[],
              (),
            );
          (
            Some(name),
            add_info(IdTagged.ids(label), InfoExp(lab_info), m),
          );
        | EmptyHole =>
          let lab_info =
            mk_exp_info(
              ~uexp=label,
              ~ctx,
              ~ana=labmode,
              ~ancestors,
              ~syn_ty=Unknown(SynSwitch) |> Typ.temp,
              ~marks=[],
              ~co_ctx=CoCtx.empty,
              ~label_inference=None,
              ~inferred_label=None,
              ~dot_labels=[],
              ~label_sort=true,
              ~warnings=[],
              (),
            );
          (None, add_info(IdTagged.ids(label), InfoExp(lab_info), m));
        | _ =>
          /* Malformed label — analyze via go to cover sub-expression IDs */
          let (i, _, m) = go(~ana=labmode, label, m);
          let i =
            mk_exp_info(
              ~uexp=i.user_term,
              ~ctx=i.ctx,
              ~ana=i.ana,
              ~ancestors=i.ancestors,
              ~syn_ty=i.syn_ty,
              ~marks=i.marks @ [BadLabel(Exp(label))],
              ~co_ctx=i.co_ctx,
              ~label_inference=i.label_inference,
              ~inferred_label=i.inferred_label,
              ~dot_labels=i.dot_labels,
              ~label_sort=true,
              ~warnings=i.warnings,
              (),
            );
          let m = add_info(IdTagged.ids(i.user_term), InfoExp(i), m);
          (None, m);
        };
      /* Compute TupLabel's own self */
      let (syn_tl, cms_tl) =
        switch (lab_name) {
        | Some(name) => (
            TupLabel(Label(name) |> Typ.temp, e.ty) |> Typ.temp,
            [],
          )
        | None =>
          switch (label.term) {
          | EmptyHole => (
              TupLabel(Unknown(SynSwitch) |> Typ.temp, e.ty) |> Typ.temp,
              [],
            )
          | _ => (
              TupLabel(Unknown(Internal) |> Typ.temp, e.ty) |> Typ.temp,
              [
                Mark.TupleLabelError({
                  malformed_labels: [Exp(label)],
                  duplicate_labels: [],
                  invalid_labels: [],
                  typ:
                    TupLabel(Unknown(Internal) |> Typ.temp, e.ty) |> Typ.temp,
                }),
              ],
            )
          }
        };
      add(~syn_ty=syn_tl, ~marks=cms_tl, ~co_ctx=e.co_ctx, m);
    | ExplicitNonlabel =>
      add(
        ~syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[ExplicitNonlabel],
        ~co_ctx=CoCtx.empty,
        m,
      )
    | Label(name) =>
      add(
        ~syn_ty=Unknown(Internal) |> Typ.temp,
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
      add(~syn_ty=syn_b, ~marks=marks_b, ~co_ctx=CoCtx.empty, m);

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
      let (info_e2, _, m) = go(~ana=Label("") |> Typ.temp, e2, m);
      let (info_e2, m) = {
        let (syn_ls, marks_ls) =
          switch (e2.term) {
          | Label(name) => (Label(name) |> Typ.temp, [])
          | _ => (info_e2.syn_ty, info_e2.marks)
          };
        let patched =
          mk_exp_info(
            ~uexp=info_e2.user_term,
            ~ctx=info_e2.ctx,
            ~ana=info_e2.ana,
            ~ancestors=info_e2.ancestors,
            ~syn_ty=syn_ls,
            ~marks=marks_ls,
            ~co_ctx=info_e2.co_ctx,
            ~label_inference=info_e2.label_inference,
            ~inferred_label=info_e2.inferred_label,
            ~label_sort=true,
            ~dot_labels=available_labels,
            ~warnings=info_e2.warnings,
            (),
          );
        (
          patched,
          add_info(IdTagged.ids(info_e2.user_term), InfoExp(patched), m),
        );
      };
      let dot_elab = Dot(e1_elab, e2) |> rewrap;
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
          switch (element) {
          | Some({term: TupLabel(_, typ), _})
          | Some(typ) =>
            add(~elab=dot_elab, ~syn_ty=typ, ~marks=[], ~co_ctx=dot_co_ctx, m)
          | None =>
            add(
              ~elab=dot_elab,
              ~syn_ty=Unknown(Internal) |> Typ.temp,
              ~marks=[LabelNotFound(name, labels)],
              ~co_ctx=dot_co_ctx,
              m,
            )
          };
        | EmptyHole =>
          add(
            ~elab=dot_elab,
            ~syn_ty=Unknown(Internal) |> Typ.temp,
            ~marks=[],
            ~co_ctx=dot_co_ctx,
            m,
          )
        | _ =>
          add(
            ~elab=dot_elab,
            ~syn_ty=Unknown(Internal) |> Typ.temp,
            ~marks=[BadLabel(Exp(e2))],
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
              ~elab=dot_elab,
              ~syn_ty=List(typ) |> Typ.fresh,
              ~marks=[],
              ~co_ctx=dot_co_ctx,
              m,
            )
          | None =>
            add(
              ~elab=dot_elab,
              ~syn_ty=Unknown(Internal) |> Typ.temp,
              ~marks=[LabelNotFound(name, labels)],
              ~co_ctx=dot_co_ctx,
              m,
            )
          };
        | EmptyHole =>
          add(
            ~elab=dot_elab,
            ~syn_ty=Unknown(Internal) |> Typ.temp,
            ~marks=[],
            ~co_ctx=dot_co_ctx,
            m,
          )
        | _ =>
          add(
            ~elab=dot_elab,
            ~syn_ty=Unknown(Internal) |> Typ.temp,
            ~marks=[BadLabel(Exp(e2))],
            ~co_ctx=dot_co_ctx,
            m,
          )
        };
      | List({term: Unknown(_), _}) =>
        add(
          ~elab=dot_elab,
          ~syn_ty=List(Unknown(Internal) |> Typ.temp) |> Typ.temp,
          ~marks=[],
          ~co_ctx=dot_co_ctx,
          m,
        )
      | _ =>
        add(
          ~elab=dot_elab,
          ~syn_ty=Unknown(Internal) |> Typ.temp,
          ~marks=[DotOperatorRequiresTuple],
          ~co_ctx=dot_co_ctx,
          m,
        )
      };
    | Test(e) =>
      let (e, e_elab, m) = go(~ana=Atom(Bool) |> Typ.temp, e, m);
      add(
        ~elab=Test(e_elab) |> rewrap,
        ~syn_ty=Prod([]) |> Typ.temp,
        ~marks=[],
        ~co_ctx=e.co_ctx,
        m,
      );
    | HintedTest(e, hint) =>
      let (e, e_elab, m) = go(~ana=Atom(Bool) |> Typ.temp, e, m);
      let (hint, hint_elab, m) = go(~ana=Atom(String) |> Typ.temp, hint, m);
      add(
        ~elab=HintedTest(e_elab, hint_elab) |> rewrap,
        ~syn_ty=Prod([]) |> Typ.temp,
        ~marks=[],
        ~co_ctx=CoCtx.union([e.co_ctx, hint.co_ctx]),
        m,
      );
    | Filter(Filter({pat: cond, act}), body) =>
      let (cond, cond_elab, m) = go(~ana=syn, cond, m, ~is_in_filter=true);
      let (body, body_elab, m) = go(~ana, body, m);
      add(
        ~elab=
          Filter(
            Filter({
              act,
              pat: cond_elab,
            }),
            body_elab,
          )
          |> rewrap,
        ~syn_ty=body.ty,
        ~marks=[],
        ~co_ctx=CoCtx.union([cond.co_ctx, body.co_ctx]),
        m,
      );
    | Filter(Residue(i, act), body) =>
      let (body, body_elab, m) = go(~ana, body, m);
      add(
        ~elab=Filter(Residue(i, act), body_elab) |> rewrap,
        ~syn_ty=body.ty,
        ~marks=[],
        ~co_ctx=CoCtx.union([body.co_ctx]),
        m,
      );
    | Seq(e1, e2) =>
      let (e1, e1_elab, m) = go(~ana=syn, e1, m);
      let (e2, e2_elab, m) = go(~ana, e2, m);
      add(
        ~elab=Seq(e1_elab, e2_elab) |> rewrap,
        ~syn_ty=e2.ty,
        ~marks=[],
        ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
        m,
      );
    | Constructor(ctr, ty) =>
      let (syn_res, marks_res) = syn_marks_ctr(ctx, ctr, ana, ty);
      switch (marks_res) {
      | [FreeConstructor(name)] =>
        /* If not a known constructor, try looking up as a variable.
           This supports capitalized module names like M.x where M is
           parsed as Constructor but is actually a variable binding. */
        switch (Ctx.lookup_var(ctx, name)) {
        | Some({typ, _}) =>
          let co_ctx = CoCtx.singleton(name, Exp.rep_id(uexp), ana);
          let elab = Var(name) |> rewrap;
          let (info, _, m) = add(~elab, ~syn_ty=typ, ~marks=[], ~co_ctx, m);
          let m =
            add_info(
              ids,
              Info.InfoExp({
                ...info,
                cls: Exp(Var),
              }),
              m,
            );
          (info, elab, m);
        | None =>
          let elab = Constructor(ctr, Some(None)) |> rewrap;
          add(
            ~elab,
            ~syn_ty=syn_res,
            ~marks=marks_res,
            ~co_ctx=CoCtx.empty,
            m,
          );
        }
      | _ =>
        let elab =
          Constructor(
            ctr,
            Some(Some(fixed_typ(ctx, ana, syn_res) |> Typ.normalize(ctx))),
          )
          |> rewrap;
        add(~elab, ~syn_ty=syn_res, ~marks=marks_res, ~co_ctx=CoCtx.empty, m);
      };
    | Ap(dir, fn, arg) =>
      switch (fn.term) {
      | LivelitName(s) =>
        // refer to livelit context to find types
        switch (Ctx.lookup_livelit(ctx, s)) {
        | Some({expansion_t, model_t, expand, _}) =>
          let (fn, _, m) = go(~ana=expansion_t, fn, m);
          let (arg, _, m) = go(~ana=model_t, arg, m);

          // try to expand
          switch (expand(arg.user_term)) {
          | Some(expanded) =>
            let (info, elab, m) =
              add(
                ~elab=expanded,
                ~syn_ty=expansion_t,
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
              ~syn_ty=expansion_t,
              ~marks=[BadLivelitModel(expansion_t)],
              ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]),
              m,
            )
          };

        | None =>
          let (fn, _, m) = go(~ana=Unknown(Internal) |> Typ.temp, fn, m);
          let (arg, _, m) = go(~ana=Unknown(Internal) |> Typ.temp, arg, m);
          add(
            ~syn_ty=Unknown(Internal) |> Typ.temp,
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
            switch (ctr_ana_typ(ctx, ana, name)) {
            | Some(ty_ana) =>
              switch (MatchedTyp.arrow(ctx, ty_ana)) {
              | Some((ty1, ty2)) => Arrow(ty1, ty2) |> Typ.temp
              | None => Arrow(syn, syn) |> Typ.temp
              }
            | None => Arrow(syn, syn) |> Typ.temp
            }
          | None => Arrow(syn, syn) |> Typ.temp
          };
        let (fn, fn_elab, m) = go(~ana=fn_ana, fn, m);
        switch (custom_statics) {
        | Some(kind) =>
          CustomStatics.custom_statics_ap(
            ~ctx,
            ~ancestors,
            ~fn_info=fn,
            kind,
            (module
             {
               let uexp_to_info_map =
                   (~ctx, ~ana=?, ~is_in_filter=?, ~ancestors=?, exp, m) =>
                 go(~ctx, ~ana?, ~is_in_filter?, ~ancestors?, exp, m);
               let add = add_for_custom;
             }),
            m,
            arg,
          )
        | None =>
          let (ty_in, ty_out) = MatchedTyp.arrow_tolerant(ctx, fn.ty);
          let (arg, arg_elab, m) = go(~ana=ty_in, arg, m);
          let elab = Ap(dir, fn_elab, arg_elab) |> rewrap;
          let co_ap = CoCtx.union([fn.co_ctx, arg.co_ctx]);
          Id.is_nullary_ap_flag(IdTagged.ids(arg.user_term))
          && !Typ.is_consistent(ctx, ty_in, Prod([]) |> Typ.temp)
            ? add(
                ~elab,
                ~syn_ty=ty_out,
                ~marks=[BadTrivAp(ty_in)],
                ~co_ctx=co_ap,
                m,
              )
            : add(~elab, ~syn_ty=ty_out, ~marks=[], ~co_ctx=co_ap, m);
        };
      }
    | TypAp(fn, utyp) =>
      let typfn_ana = Poly(EmptyHole |> TPat.fresh, syn) |> Typ.temp;
      let (fn, fn_elab, m) = go(~ana=typfn_ana, fn, m);
      let (_, m) = utyp_to_info_map(~ctx, ~ancestors, utyp, m);
      let elab = TypAp(fn_elab, Typ.normalize(ctx, utyp)) |> rewrap;
      let (option_name, ty_body) = MatchedTyp.poly_pair_tolerant(ctx, fn.ty);
      switch (option_name) {
      | Some(name) =>
        add(
          ~elab,
          ~syn_ty=Typ.subst(utyp, name, ty_body),
          ~marks=[],
          ~co_ctx=fn.co_ctx,
          m,
        )
      | None => add(~elab, ~syn_ty=ty_body, ~marks=[], ~co_ctx=fn.co_ctx, m) /* invalid name matches with no free type variables. */
      };
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
          switch (ctr_ana_typ(ctx, ana, name)) {
          | Some(ty_ana) =>
            switch (MatchedTyp.arrow(ctx, ty_ana)) {
            | Some((ty1, ty2)) => Arrow(ty1, ty2) |> Typ.temp
            | None => Arrow(syn, syn) |> Typ.temp
            }
          | None => Arrow(syn, syn) |> Typ.temp
          }
        | None => Arrow(syn, syn) |> Typ.temp
        };
      let (fn, fn_elab, m) = go(~ana=fn_ana, fn, m);

      switch (custom_statics) {
      | Some(kind) =>
        CustomStatics.custom_statics_deferred_ap(
          ~ctx,
          ~ancestors,
          ~fn_info=fn,
          kind,
          (module
           {
             let uexp_to_info_map =
                 (~ctx, ~ana=?, ~is_in_filter=?, ~ancestors=?, exp, m) =>
               go(~ctx, ~ana?, ~is_in_filter?, ~ancestors?, exp, m);
             let add = add_for_custom;
           }),
          m,
          args,
        )
      | None =>
        let (ty_in, ty_out) = MatchedTyp.arrow_tolerant(ctx, fn.ty);
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
            ~elab=DeferredAp(fn_elab, args_elabs) |> rewrap,
            ~syn_ty=Arrow(ty_in', ty_out) |> Typ.temp,
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
            ~elab=DeferredAp(fn_elab, args_elabs) |> rewrap,
            ~syn_ty=Unknown(Internal) |> Typ.temp,
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
      let (mode_pat, mode_body) = MatchedTyp.arrow_tolerant(ctx, ana);
      let mode_pat = Option.value(~default=mode_pat, typ);
      let (p', _, _) =
        go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~ana=mode_pat, p, m);
      let (e, e_elab, m) = go(~ctx=p'.ctx, ~ana=mode_body, e, m);
      /* add co_ctx to pattern */
      let (p, p_elab, m) =
        go_pat(~is_synswitch=false, ~co_ctx=e.co_ctx, ~ana=mode_pat, p, m);
      let syn_ty_fun = Arrow(p.ty, e.ty) |> Typ.temp;
      let Coverage.CheckMatrix.{exhaustiveness, _} =
        Coverage.check([Info.pat_constraint(p)], Typ.normalize(ctx, p.ty));
      let marks_fun =
        switch (exhaustiveness) {
        | Exhaustive => []
        | Inexhaustive(unseen_pattern) => [
            Mark.InexhaustiveMatch(syn_ty_fun, [], unseen_pattern),
          ]
        };
      let elab = Fun(p_elab, e_elab, Some(p.ty), n) |> rewrap;
      add(
        ~elab,
        ~syn_ty=syn_ty_fun,
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
        ~elab=Forall(p_elab, e_elab) |> rewrap,
        ~syn_ty=Atom(Bool) |> Typ.temp,
        ~marks=[],
        ~co_ctx=CoCtx.mk(ctx, p.ctx, e.co_ctx),
        m,
      );
    | TypFun(utpat, body, tfname) =>
      let (name_expected_opt, item) =
        MatchedTyp.poly_pair_tolerant(ctx, ana);
      let (mode_body, ctx_body) =
        switch (TPat.tyvar_of_utpat(utpat)) {
        | Some(name) when !Ctx.is_base_typ(name) =>
          let mode_body = {
            switch (name_expected_opt) {
            | Some(name_expected) =>
              Typ.subst(Var(name) |> Typ.temp, name_expected, item)
            | _ => item
            };
          };
          let ctx_body =
            Ctx.extend_tvar(
              ctx,
              {
                name,
                id: TPat.rep_id(utpat),
                kind: Abstract,
              },
            );
          (mode_body, ctx_body);
        | Some(_)
        | None => (item, ctx)
        };
      let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
      let (body, body_elab, m) = go(~ctx=ctx_body, ~ana=mode_body, body, m);
      add(
        ~elab=TypFun(utpat, body_elab, tfname) |> rewrap,
        ~syn_ty=Poly(utpat, body.ty) |> Typ.temp,
        ~marks=[],
        ~co_ctx=body.co_ctx,
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
      let syn_ty_let = body.ty;
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
      let elab =
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
        ~elab,
        ~syn_ty=syn_ty_let,
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
        ~elab=Theorem(p_elab, e1_elab, e2_elab) |> rewrap,
        ~syn_ty=e2.ty,
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
        ~elab=Theorem(p_elab, e1_elab, e2_elab) |> rewrap,
        ~syn_ty=e2.ty,
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
        ~elab=ProofObject(e_elab) |> rewrap,
        ~syn_ty=Typ.temp(ProofOf(e)),
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
      let elab =
        FixF(p_elab, Asc(e_elab, p'.ty) |> Exp.fresh, env) |> rewrap;
      add(
        ~elab,
        ~syn_ty=p'.ty,
        ~marks=[],
        ~co_ctx=
          CoCtx.union([CoCtx.mk(ctx, p''.ctx, e'.co_ctx), pat_typ_refs]),
        m,
      );
    | If(e0, e1, e2) =>
      let branch_ids = List.map(Exp.rep_id, [e1, e2]);
      let (cond, cond_elab, m) = go(~ana=Atom(Bool) |> Typ.temp, e0, m);
      let (cons, cons_elab, m) = go(~ana, e1, m);
      let (alt, alt_elab, m) = go(~ana, e2, m);
      let (syn_if, cms_if) =
        syn_marks_match(ctx, [cons.ty, alt.ty], branch_ids);
      let result_ty =
        fixed_typ(ctx, ana, syn_if)
        |> Typ.normalize(ctx)
        |> Typ.all_ids_temp;
      let elab =
        If(
          cond_elab,
          fresh_ascription(ctx, cons_elab, cons.ty, Some(result_ty)),
          fresh_ascription(ctx, alt_elab, alt.ty, Some(result_ty)),
        )
        |> rewrap;
      add(
        ~elab,
        ~syn_ty=syn_if,
        ~marks=cms_if,
        ~co_ctx=CoCtx.union([cond.co_ctx, cons.co_ctx, alt.co_ctx]),
        m,
      );
    | Match(scrut, rules) =>
      let (scrut, scrut_elab, m) = go(~ana=syn, scrut, m);
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

      let e_tys = List.map(Info.exp_ty, es);
      let e_co_ctxs = List.map(Info.exp_co_ctx, es);
      let (syn_ty_match, marks_match) =
        syn_marks_match(ctx, e_tys, branch_ids);
      let (constraints, ps_elabs, m) =
        List.fold_left(
          (
            (
              constraints: list(Coverage.Constraint.t),
              ps_elabs: list(Pat.t),
              m: Map.t,
            ),
            (p, co_ctx),
          ) => {
            let (info, p_elab, m) =
              go_pat(~is_synswitch=false, ~co_ctx, ~ana=scrut.ty, p, m);

            let p_constraint = Info.pat_constraint(info);
            ([p_constraint, ...constraints], ps_elabs @ [p_elab], m);
          },
          ([], [], m),
          List.combine(ps, e_co_ctxs),
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
      let es_elabs =
        List.map2(
          (e_elab, ty) =>
            fresh_ascription(ctx, e_elab, ty, Some(result_ty)),
          es_elabs,
          e_tys,
        );
      let elab =
        Match(scrut_elab, List.combine(ps_elabs, es_elabs)) |> rewrap;
      add(~elab, ~syn_ty=syn_ty_match, ~marks=marks_match', ~co_ctx, m);
    | TyAlias(typat, utyp, body) =>
      let m = utpat_to_info_map(~ctx, ~ancestors, typat, m) |> snd;
      /* Desugar Sig types so that type aliases like `type T = {let x : Int}`
         store Prod([TupLabel(...)]) rather than Sig([...]) in the context.
         This ensures meet/join can unify them with module expression types. */
      let utyp_desugared = Typ.desugar_sig(ctx, utyp);
      switch (typat.term) {
      | Var(name) when !Ctx.is_base_typ(name) =>
        /* NOTE(andrew): Currently, Typ.to_typ returns Unknown(TypeHole)
           for any type variable reference not in its ctx. So any free variables
           in the definition would be obliterated. But we need to check for free
           variables to decide whether to make a recursive type or not. So we
           tentatively add an abtract type to the ctx, representing the
           speculative rec parameter. */
        let (ty_def, ctx_def, ctx_body) = {
          switch (utyp_desugared.term) {
          | _ when List.mem(name, Typ.free_vars(utyp_desugared)) =>
            /* NOTE: When debugging type system issues it may be beneficial to
               use a different name than the alias for the recursive parameter */
            //let ty_rec = Typ.Rec("α", Typ.subst(Var("α"), name, ty_pre));
            let ty_rec =
              Rec(Var(name) |> TPat.fresh, utyp_desugared) |> Typ.temp;
            let ctx_def =
              Ctx.extend_alias(ctx, name, TPat.rep_id(typat), ty_rec);
            (ty_rec, ctx_def, ctx_def);
          | _ => (
              utyp_desugared,
              ctx,
              Ctx.extend_alias(
                ctx,
                name,
                TPat.rep_id(typat),
                utyp_desugared,
              ),
            )
          /* NOTE(yuchen): Below is an alternative implementation that attempts to
             add a rec whenever type alias is present. It may cause trouble to the
             runtime, so precede with caution. */
          // Typ.lookup_surface(ty_pre)
          //   ? {
          //     let ty_rec = Typ.Rec({item: ty_pre, name});
          //     let ctx_def = Ctx.add_alias(ctx, name, utpat_id(typat), ty_rec);
          //     (ty_rec, ctx_def, ctx_def);
          //   }
          //   : {
          //     let ty = Term.Typ.to_typ(ctx, utyp);
          //     (ty, ctx, Ctx.add_alias(ctx, name, utpat_id(typat), ty));
          //   };
          };
        };
        let ctx_body =
          switch (Typ.get_sum_constructors(ctx, ty_def)) {
          | Some(sm) => Ctx.add_ctrs(ctx_body, name, Typ.rep_id(utyp), sm)
          | None => ctx_body
          };
        let ({co_ctx, ty: ty_body, _}: Info.exp, body_elab, m) =
          go(~ctx=ctx_body, ~ana, body, m);
        /* Make sure types don't escape their scope */
        let ty_escape = Typ.subst(ty_def, typat, ty_body);
        let m = utyp_to_info_map(~ctx=ctx_def, ~ancestors, utyp, m) |> snd;
        let typ_refs =
          ModuleHelpers.collect_module_refs_in_typ(
            ctx,
            Typ.rep_id(utyp),
            utyp,
          );
        add(
          ~elab=body_elab,
          ~syn_ty=ty_escape,
          ~marks=[],
          ~co_ctx=CoCtx.union([co_ctx, typ_refs]),
          m,
        );
      | Var(_)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_) =>
        let ({co_ctx, ty: ty_body, _}: Info.exp, body_elab, m) =
          go(~ctx, ~ana, body, m);
        let m = utyp_to_info_map(~ctx, ~ancestors, utyp, m) |> snd;
        let typ_refs =
          ModuleHelpers.collect_module_refs_in_typ(
            ctx,
            Typ.rep_id(utyp),
            utyp,
          );
        add(
          ~elab=body_elab,
          ~syn_ty=ty_body,
          ~marks=[],
          ~co_ctx=CoCtx.union([co_ctx, typ_refs]),
          m,
        );
      };
    | Use(typ, body) =>
      let (typ, m) = utyp_to_info_map(~ctx, ~ancestors, typ, m);
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
          ~elab=body_elab,
          ~syn_ty=body.ty,
          ~marks=[],
          ~co_ctx=body.co_ctx,
          m,
        )
      | None when Typ.fast_equal(Unknown(Internal) |> Typ.temp, typ.user_term) =>
        add(
          ~elab=body_elab,
          ~syn_ty=body.ty,
          ~marks=[],
          ~co_ctx=body.co_ctx,
          m,
        )
      | None =>
        add(
          ~elab=body_elab,
          ~syn_ty=body.ty,
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
        ~elab=module_elab,
        ~syn_ty=actual_ty,
        ~marks=[],
        ~co_ctx=expanded_info.co_ctx,
        m,
      );
    | ModuleExp(mp, def, body) =>
      /* Expand module M = def in body → let M = def in body.
         Process the MPat for cursor info, then expand to Let and type-check. */
      let (_, _, m) = any_to_info_map(~ctx, ~ancestors, MPat(mp), m);
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
        ~elab=moduleexp_elab,
        ~syn_ty=expanded_info.ty,
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
  let mk_pat_info =
      (
        ~upat: Pat.t=upat,
        ~ctx=ctx,
        ~co_ctx=co_ctx,
        ~ana=ana,
        ~ancestors=ancestors,
        ~syn_ty: Typ.t,
        ~marks: list(Mark.t)=[],
        ~warnings: list(Warning.list_item)=[],
        ~constraint_: Coverage.Constraint.t,
        ~label_inference: option(Info.label_inference(Info.pat))=None,
        ~inferred_label: option(LabeledTuple.label)=None,
        ~label_sort=false,
        m: Id.Map.t(Info.t),
      ): Info.pat => {
    let prev_synswitch =
      switch (Id.Map.find_opt(Pat.rep_id(upat), m)) {
      | Some(Info.InfoPat({ana, ty, _})) when Typ.is_syn_plus(ana) =>
        Some(ty)
      | Some(Info.InfoPat({prev_synswitch, _})) => prev_synswitch
      | Some(_)
      | None => None
      };
    let marks =
      if (marks != []) {
        marks;
      } else {
        switch (expectation_mismatch_mark(ctx, ana, syn_ty)) {
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
            | _ => Message.Common(syn_ana_ok_common(ctx, ana, syn_ty))
            },
          );
    let cls = Cls.Pat(Pat.cls_of_term(upat.term));
    let ty = fixed_typ(ctx, ana, syn_ty);
    let warning_acc =
      warnings
      @ (
        switch (upat.term) {
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
    {
      cls,
      syn_ty,
      marks,
      prev_synswitch,
      ana,
      ty,
      message,
      warnings: warning_acc,
      ctx,
      co_ctx,
      ancestors,
      user_term: upat,
      constraint_: constraint_',
      label_inference,
      inferred_label,
      label_sort,
    };
  };
  let add =
      (
        ~syn_ty: Typ.t,
        ~marks: list(Mark.t)=[],
        ~warnings: list(Warning.list_item)=[],
        ~ctx: Ctx.t,
        ~constraint_: Coverage.Constraint.t,
        ~label_inference: option(Info.label_inference(Info.pat))=None,
        ~elab: Pat.t=upat,
        m: Id.Map.t(Info.t),
      )
      : (Info.pat, Pat.t, Map.t) => {
    let info: Info.pat =
      mk_pat_info(
        ~syn_ty,
        ~marks,
        ~warnings,
        ~ctx,
        ~co_ctx,
        ~ana,
        ~ancestors,
        ~constraint_,
        ~label_inference,
        ~inferred_label=None,
        ~label_sort=false,
        m,
      );

    (info, elab, add_info(ids, InfoPat(info), m));
  };
  let go =
      (
        ~is_synswitch=is_synswitch,
        ~ctx=ctx,
        ~co_ctx=co_ctx,
        ~ancestors=ancestors,
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
      ~ancestors,
      ~duplicate_bindings,
      ~ana,
      ~under_ascription,
      upat,
      m: Map.t,
    );
  };
  let ancestors = [Pat.rep_id(upat)] @ ancestors;
  let unknown = Unknown(is_synswitch ? SynSwitch : Internal) |> Typ.temp;

  let elaborate_singleton_tuple = (upat: Pat.t, inner_ty, l, m) =>
    LabeledTupleHelpers.autolabel_singleton_pat(
      ~analyze_original=
        (~ana, pat, m) =>
          upat_to_info_map(
            ~ctx,
            ~co_ctx,
            ~is_synswitch,
            ~ancestors,
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
            ~ancestors,
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
      let (_, _, m) = multi(~ctx, ~ancestors, m, tms);
      add(
        ~syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[IsMulti],
        ~ctx,
        ~constraint_=Coverage.Constraint.Hole(None),
        m,
      );
    | Invalid(token) =>
      add(
        ~syn_ty=SynTy.unknown_internal(),
        ~marks=[BadToken(token)],
        ~ctx,
        ~constraint_=Coverage.Constraint.Hole(None),
        m,
      )
    | EmptyHole =>
      add(
        ~syn_ty=unknown,
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
          ~elab=Atom(Nat(nat)) |> rewrap,
          ~syn_ty=Atom(Nat) |> Typ.temp,
          ~marks=[],
          ~ctx,
          ~constraint_=Coverage.Constraint.BigInt(nat),
          m,
        )
      | L(Int(int)) =>
        add(
          ~elab=Atom(Int(int)) |> rewrap,
          ~syn_ty=Atom(Int) |> Typ.temp,
          ~marks=[],
          ~ctx,
          ~constraint_=Coverage.Constraint.BigInt(int),
          m,
        )
      | L(SInt(int)) =>
        add(
          ~elab=Atom(SInt(int)) |> rewrap,
          ~syn_ty=Atom(SInt) |> Typ.temp,
          ~marks=[],
          ~ctx,
          ~constraint_=Coverage.Constraint.SInt(int),
          m,
        )
      | L(Float(float)) =>
        add(
          ~elab=Atom(Float(float)) |> rewrap,
          ~syn_ty=Atom(Float) |> Typ.temp,
          ~marks=[],
          ~ctx,
          ~constraint_=Coverage.Constraint.Float(float),
          m,
        )
      | L(Bool(bool)) =>
        add(
          ~elab=Atom(Bool(bool)) |> rewrap,
          ~syn_ty=Atom(Bool) |> Typ.temp,
          ~marks=[],
          ~ctx,
          ~constraint_=
            bool ? Coverage.Constraint.true_ : Coverage.Constraint.false_,
          m,
        )
      | L(String(string)) =>
        add(
          ~elab=Atom(String(string)) |> rewrap,
          ~syn_ty=Atom(String) |> Typ.temp,
          ~marks=[],
          ~ctx,
          ~constraint_=Coverage.Constraint.String(string),
          m,
        )
      | R(BadInt(str)) =>
        add(
          ~elab=Invalid(str) |> rewrap,
          ~syn_ty=Unknown(Internal) |> Typ.temp,
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
      let mode = MatchedTyp.list_tolerant(ctx, ana);
      let modes = List.init(List.length(ps), _ => mode);
      let (ctx, tys, cons, m, _, ps_elabs) =
        fold_patterns_with_modes(
          ~analyze=
            (~ctx, ~ana, ~duplicate_bindings, p, m) =>
              go(~ctx, ~ana, ~duplicate_bindings, p, m),
          ~ctx,
          ps,
          modes,
          m,
        );
      switch (Typ.meet_all(~empty=unknown, ctx, tys)) {
      | None =>
        let syn_no_meet = SynTy.meet_of(List, Unknown(Internal) |> Typ.temp);
        add(
          ~elab=ListLit(ps_elabs) |> rewrap,
          ~syn_ty=syn_no_meet,
          ~marks=
            should_emit_nomeet_mark(ctx, ana, syn_no_meet)
              ? [NoMeet(List, add_source(ids, tys))]
              : [],
          ~ctx,
          ~constraint_=list_constraint(cons),
          m,
        )
      | Some(ty) =>
        add(
          ~elab=ListLit(ps_elabs) |> rewrap,
          ~syn_ty=List(ty) |> Typ.temp,
          ~marks=[],
          ~ctx,
          ~constraint_=list_constraint(cons),
          m,
        )
      };
    | Cons(hd, tl) =>
      let inner_ty = MatchedTyp.list_tolerant(ctx, ana);
      let (hd, hd_elab, m) = go(~ctx, ~ana=inner_ty, hd, m);
      let (tl, tl_elab, m) =
        go(~ctx=hd.ctx, ~ana=List(inner_ty) |> Typ.fresh, tl, m);
      add(
        ~elab=Cons(hd_elab, tl_elab) |> rewrap,
        ~syn_ty=List(hd.ty) |> Typ.temp,
        ~marks=[],
        ~ctx=tl.ctx,
        ~constraint_=Coverage.Constraint.cons(hd.constraint_, tl.constraint_),
        m,
      );
    | Wild =>
      add(
        ~syn_ty=unknown,
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
            ~syn_ty=unknown,
            ~marks=[Mark.DuplicateVar(name, unknown)],
            ~ctx=Ctx.extend(ctx, entry),
            ~constraint_=Coverage.Constraint.Truth,
            m,
          );
        }
        : add(
            ~syn_ty=unknown,
            ~marks=[],
            ~ctx=Ctx.extend(ctx, entry),
            ~constraint_=Coverage.Constraint.Truth,
            m,
          );

    | TupLabel({term: ExplicitNonlabel, _} as label, p) =>
      let (p, p_elab, m) = go(~ana, ~ctx, p, m);
      /* Add info for the ExplicitNonlabel directly */
      let m =
        add_info(
          IdTagged.ids(label),
          InfoPat(
            mk_pat_info(
              ~upat=label,
              ~ctx,
              ~co_ctx,
              ~ana=syn,
              ~ancestors,
              ~syn_ty=ExplicitNonlabel |> Typ.temp,
              ~marks=[],
              ~constraint_=Coverage.Constraint.Truth,
              ~label_inference=None,
              ~inferred_label=None,
              ~label_sort=true,
              ~warnings=[],
              m,
            ),
          ),
          m,
        );
      (p, p_elab, add_info(ids, InfoPat(p), m));
    | ExplicitNonlabel =>
      add(
        ~syn_ty=Unknown(Internal) |> Typ.temp,
        ~marks=[ExplicitNonlabel],
        ~ctx,
        ~constraint_=Coverage.Constraint.Truth,
        m,
      )
    | TupLabel(label, p) =>
      let (labmode, val_mode) =
        switch (MatchedTyp.label(ctx, ana)) {
        | Some((labmode, val_mode)) => (labmode, val_mode)
        | _ => (
            Unknown(SynSwitch) |> Typ.temp,
            Unknown(Internal) |> Typ.temp,
          )
        };
      /* Analyze value child */
      let (p, _, m) = go(~ctx, ~ana=val_mode, ~duplicate_bindings, p, m);
      /* Add info for the label child directly — TupLabel owns its label */
      let (lab_name, m) =
        switch (label.term) {
        | Label(name) =>
          let lab_info =
            mk_pat_info(
              ~upat=label,
              ~ctx,
              ~co_ctx,
              ~ana=labmode,
              ~ancestors,
              ~syn_ty=Label(name) |> Typ.temp,
              ~marks=[],
              ~constraint_=Coverage.Constraint.Truth,
              ~label_inference=None,
              ~inferred_label=None,
              ~label_sort=true,
              ~warnings=[],
              m,
            );
          (
            Some(name),
            add_info(IdTagged.ids(label), InfoPat(lab_info), m),
          );
        | EmptyHole =>
          let lab_info =
            mk_pat_info(
              ~upat=label,
              ~ctx,
              ~co_ctx,
              ~ana=labmode,
              ~ancestors,
              ~syn_ty=Unknown(SynSwitch) |> Typ.temp,
              ~marks=[],
              ~constraint_=Coverage.Constraint.Truth,
              ~label_inference=None,
              ~inferred_label=None,
              ~label_sort=true,
              ~warnings=[],
              m,
            );
          (None, add_info(IdTagged.ids(label), InfoPat(lab_info), m));
        | _ =>
          /* Malformed label — analyze via go to cover sub-expression IDs */
          let (p_info, _, m) = go(~ctx, ~ana=labmode, label, m);
          let p_info =
            mk_pat_info(
              ~upat=p_info.user_term,
              ~ctx=p_info.ctx,
              ~co_ctx=p_info.co_ctx,
              ~ana=p_info.ana,
              ~ancestors=p_info.ancestors,
              ~syn_ty=p_info.syn_ty,
              ~marks=p_info.marks @ [BadLabel(Pat(label))],
              ~constraint_=p_info.constraint_,
              ~label_inference=p_info.label_inference,
              ~inferred_label=p_info.inferred_label,
              ~label_sort=true,
              ~warnings=p_info.warnings,
              m,
            );
          let m = add_info(IdTagged.ids(p_info.user_term), InfoPat(p_info), m);
          (None, m);
        };
      /* Compute TupLabel's own self */
      let (syn_tl, cms_tl) =
        switch (lab_name) {
        | Some(name) => (
            TupLabel(Label(name) |> Typ.temp, p.ty) |> Typ.temp,
            [],
          )
        | None =>
          switch (label.term) {
          | EmptyHole => (
              TupLabel(Unknown(SynSwitch) |> Typ.temp, p.ty) |> Typ.temp,
              [],
            )
          | _ => (
              TupLabel(Unknown(Internal) |> Typ.temp, p.ty) |> Typ.temp,
              [
                Mark.TupleLabelError({
                  malformed_labels: [Pat(label)],
                  duplicate_labels: [],
                  invalid_labels: [],
                  typ:
                    TupLabel(Unknown(Internal) |> Typ.temp, p.ty) |> Typ.temp,
                }),
              ],
            )
          }
        };
      add(
        ~syn_ty=syn_tl,
        ~marks=cms_tl,
        ~ctx=p.ctx,
        ~constraint_=Coverage.Constraint.Tuple([p.constraint_]),
        m,
      );
    | Tuple(ps) =>
      let expected_labels =
        switch (Typ.weak_head_normalize(ctx, ana).term) {
        | Prod(ts) =>
          Some(
            List.filter_map(
              t => Typ.match_tup_label(t) |> Option.map(fst),
              ts,
            ),
          )
        | _ => None
        };

      let original_labels =
        List.map(p => Pat.match_tup_label(p) |> Option.map(fst), ps);

      let (inferred_ps, modes) =
        MatchedTyp.prod(
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
      /* Build duplicate/invalid labels with one entry per occurrence (matching old behavior) */
      let duplicate_labels =
        List.filter_map(
          (p: Pat.t) =>
            switch (Pat.match_tup_label(p)) {
            | Some((name, _)) when List.mem(name, new_duplicate_labels) =>
              Some(name)
            | _ => None
            },
          ps,
        );
      let invalid_labels =
        switch (expected_labels) {
        | None => []
        | Some(expected) =>
          List.filter_map(
            (p: Pat.t) =>
              switch (Pat.match_tup_label(p)) {
              | Some((name, _)) when !List.mem(name, expected) => Some(name)
              | _ => None
              },
            ps,
          )
        };

      let (ctx, tys, cons, m, info_pats, ps_elabs) =
        List.fold_left2(
          ((ctx, tys, cons, m, info_all, elabs), (inferred_label, e: Pat.t), ana) =>
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
              let info =
                switch (inferred_label) {
                | Some(_) => {...info, inferred_label}
                | None => info
                };
              let m =
                switch (inferred_label) {
                | Some(_) =>
                  add_info(IdTagged.ids(info.user_term), InfoPat(info), m)
                | None => m
                };
              (
                info.ctx,
                tys @ [info.ty],
                cons @ [info.constraint_],
                m,
                info_all @ [info],
                elabs @ [elab],
              );
            | TupLabel(label, value) =>
              let (labmode, val_mode) =
                switch (MatchedTyp.label(ctx, ana)) {
                | Some((labmode, val_mode)) => (labmode, val_mode)
                | _ => (
                    Unknown(SynSwitch) |> Typ.temp,
                    Unknown(Internal) |> Typ.temp,
                  )
                };
              let (value_info, _, m) =
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
                    switch (expected_labels) {
                    | Some(expected) when !List.mem(name, expected) => (
                        SynTy.unknown_internal(),
                        [Mark.InvalidLabel(name, expected)],
                        true,
                      )
                    | _ =>
                      List.mem(name, new_duplicate_labels)
                        ? (
                            Label(name) |> Typ.temp,
                            [Mark.DuplicateLabel(name, Label(name) |> Typ.temp)],
                            false,
                          )
                        : (Label(name) |> Typ.temp, [], false)
                    };
                  let label_info =
                    mk_pat_info(
                      ~upat=label,
                      ~ctx,
                      ~co_ctx,
                      ~ana=labmode,
                      ~ancestors,
                      ~syn_ty=label_syn,
                      ~marks=label_marks,
                      ~constraint_=Coverage.Constraint.Truth,
                      ~label_inference=None,
                      ~inferred_label=None,
                      ~label_sort=true,
                      ~warnings=[],
                      m,
                    );
                  (
                    Some(name),
                    label_invalid,
                    add_info(IdTagged.ids(label), InfoPat(label_info), m),
                  );
                | EmptyHole =>
                  let label_info =
                    mk_pat_info(
                      ~upat=label,
                      ~ctx,
                      ~co_ctx,
                      ~ana=labmode,
                      ~ancestors,
                      ~syn_ty=Unknown(SynSwitch) |> Typ.temp,
                      ~marks=[],
                      ~constraint_=Coverage.Constraint.Truth,
                      ~label_inference=None,
                      ~inferred_label=None,
                      ~label_sort=true,
                      ~warnings=[],
                      m,
                    );
                  (None, false, add_info(IdTagged.ids(label), InfoPat(label_info), m));
                | _ =>
                  /* Malformed label — analyze via go to cover sub-expression IDs */
                  let (p_info, _, m) = go(~ctx, ~ana=labmode, label, m);
                  let p_info =
                    mk_pat_info(
                      ~upat=p_info.user_term,
                      ~ctx=p_info.ctx,
                      ~co_ctx=p_info.co_ctx,
                      ~ana=p_info.ana,
                      ~ancestors=p_info.ancestors,
                      ~syn_ty=p_info.syn_ty,
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
                    add_info(IdTagged.ids(p_info.user_term), InfoPat(p_info), m),
                  );
                };
              let (syn_tl, cms_tl) =
                switch (lab_name) {
                | Some(name) =>
                  let tup_syn =
                    TupLabel(Label(name) |> Typ.temp, value_info.ty) |> Typ.temp;
                  label_invalid
                    ? (
                        tup_syn,
                        [
                          Mark.TupleLabelError({
                            malformed_labels: [],
                            duplicate_labels: [],
                            invalid_labels: [name],
                            typ: tup_syn,
                          }),
                        ],
                      )
                    : List.mem(name, new_duplicate_labels)
                    ? (
                        tup_syn,
                        [
                          Mark.TupleLabelError({
                            malformed_labels: [],
                            duplicate_labels: [name],
                            invalid_labels: [],
                            typ: tup_syn,
                          }),
                        ],
                      )
                    : (tup_syn, [])
                | None =>
                  switch (label.term) {
                  | EmptyHole => (
                      TupLabel(Unknown(SynSwitch) |> Typ.temp, value_info.ty)
                      |> Typ.temp,
                      [],
                    )
                  | _ => (
                      TupLabel(Unknown(Internal) |> Typ.temp, value_info.ty)
                      |> Typ.temp,
                      [
                        Mark.TupleLabelError({
                          malformed_labels: [Pat(label)],
                          duplicate_labels: [],
                          invalid_labels: [],
                          typ:
                            TupLabel(Unknown(Internal) |> Typ.temp, value_info.ty)
                            |> Typ.temp,
                        }),
                      ],
                    )
                  }
                };
              let constraint_ = Coverage.Constraint.Tuple([value_info.constraint_]);
              let info =
                mk_pat_info(
                  ~upat=e,
                  ~ctx=value_info.ctx,
                  ~co_ctx,
                  ~ana,
                  ~ancestors,
                  ~syn_ty=syn_tl,
                  ~marks=cms_tl,
                  ~constraint_,
                  ~label_inference=None,
                  ~inferred_label,
                  ~label_sort=false,
                  ~warnings=[],
                  m,
                );
              let m = add_info(IdTagged.ids(e), InfoPat(info), m);
              (
                info.ctx,
                tys @ [info.ty],
                cons @ [info.constraint_],
                m,
                info_all @ [info],
                elabs @ [e],
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
              let info =
                switch (inferred_label) {
                | Some(_) => {...info, inferred_label}
                | None => info
                };
              let m =
                switch (inferred_label) {
                | Some(_) =>
                  add_info(IdTagged.ids(info.user_term), InfoPat(info), m)
                | None => m
                };
              (
                info.ctx,
                tys @ [info.ty],
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

      /* Collect malformed label errors from children */
      let malformed_labels =
        List.fold_left(
          (a, e: Info.pat) => {
            switch (e.user_term.term, MarkSelection.highest_ranked_mark(e.marks)) {
            | (
                TupLabel(_, _),
                Some(Mark.TupleLabelError({malformed_labels, _})),
              ) =>
              a @ malformed_labels
            | _ => a
            }
          },
          [],
          info_pats,
        );

      let ty_list = Typ.remove_duplicate_labels(~duplicate_labels, tys);
      /* Strip TupLabel wrapper from invalid labels */
      let ty_list =
        List.map(
          ty =>
            switch (Typ.match_tup_label(ty)) {
            | Some((name, inner)) when List.mem(name, invalid_labels) => inner
            | _ => ty
            },
          ty_list,
        );

      let prod_ty_pat = Prod(ty_list) |> Typ.temp;
      let (syn_tp, cms_tp) =
        List.is_empty(malformed_labels)
        && List.is_empty(duplicate_labels)
        && List.is_empty(invalid_labels)
          ? (prod_ty_pat, [])
          : (
            prod_ty_pat,
            [
              Mark.TupleLabelError({
                malformed_labels,
                duplicate_labels,
                invalid_labels,
                typ: prod_ty_pat,
              }),
            ],
          );
      add(
        ~syn_ty=syn_tp,
        ~marks=cms_tp,
        ~ctx,
        ~constraint_,
        ~label_inference=
          Some(
            derive_label_inference_info(original_labels, new_labels),
          ),
        ~elab=Tuple(ps_elabs) |> rewrap,
        m,
      );
    | Label(name) =>
      add(
        ~syn_ty=Label(name) |> Typ.temp,
        ~marks=[],
        ~ctx,
        ~constraint_=Coverage.Constraint.Truth,
        m,
      )
    | Parens(p) =>
      let (p, p_elab, m) = go(~ctx, ~ana, p, ~duplicate_bindings, m);
      add(
        ~elab=Parens(p_elab) |> rewrap,
        ~syn_ty=p.syn_ty,
        ~marks=p.marks,
        ~ctx=p.ctx,
        ~constraint_=p.constraint_,
        m,
      );
    | Projector(data, p) =>
      let (p, p_elab, m) = go(~ctx, ~ana, p, ~duplicate_bindings, m);
      add(
        ~elab=Projector(data, p_elab) |> rewrap,
        ~syn_ty=p.syn_ty,
        ~marks=p.marks,
        ~ctx=p.ctx,
        ~constraint_=p.constraint_,
        m,
      );
    | Constructor(ctr, ty) =>
      let (syn_ctr, cms_ctr) = syn_marks_ctr(ctx, ctr, ana, ty);
      let elab_ty =
        switch (ctr_ana_typ(ctx, ana, ctr), Ctx.lookup_ctr(ctx, ctr)) {
        | (Some(ana_ty), _) => Some(Typ.normalize(ctx, ana_ty))
        | (_, Some({typ: syn_ty, _})) => Some(Typ.normalize(ctx, syn_ty))
        | _ => None
        };
      add(
        ~elab=Constructor(ctr, Some(elab_ty)) |> rewrap,
        ~syn_ty=syn_ctr,
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
      let (ty_in, ty_out) = MatchedTyp.arrow_tolerant(ctx, fn'.ty);
      let (arg, arg_elab, m) = go(~ctx, ~ana=ty_in, arg, m);
      let constraint_ =
        switch (ctr) {
        | Some(ctr) => Coverage.Constraint.Ap(ctr, Some(arg.constraint_))
        | None => Coverage.Constraint.Hole(None)
        };
      add(
        ~elab=Ap(fn_elab, arg_elab) |> rewrap,
        ~syn_ty=ty_out,
        ~marks=[],
        ~ctx=arg.ctx,
        ~constraint_,
        m,
      );
    | Asc(p, ann) =>
      let (ann, m) = utyp_to_info_map(~ctx, ~ancestors, ann, m);
      /* Desugar any Sig types in the annotation without full normalization */
      let ann_ty = Typ.desugar_sig(ctx, ann.user_term);
      let (p, p_elab, m) =
        go(~ctx, ~under_ascription=true, ~ana=ann_ty, p, m);
      add(
        ~elab=Asc(p_elab, Typ.normalize(ctx, ann.user_term)) |> rewrap,
        ~syn_ty=ann_ty,
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
    (~ctx, ~expects=TypExpectation.TypeExpected, ~ancestors, utyp: Typ.t, m: Map.t)
    : (Info.typ, Map.t) => {
  open TypExpectation;
  let ids = IdTagged.ids(utyp);
  let term = IdTagged.term_of(utyp);
  let rec status_for_node = (~expects=expects, utyp: Typ.t): typ_status => {
    let ok = (o: Message.ok_typ): typ_status => {
      ([], Some(o));
    };
    let err = (m: Mark.t): typ_status => {
      ([m], None);
    };
    switch (expects, utyp.term) {
    | (_, Unknown(Hole(Invalid(token)))) => err(BadToken(token))
    | (LabelExpected(_), Unknown(Hole(EmptyHole))) => ok(Message.EmptyLabel)
    | (LabelProjectionExpected(_), Unknown(Hole(EmptyHole))) =>
      ok(Message.EmptyLabel)
    | (TypeExpected | ProductExpected, ProdProjection(pty, l)) =>
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
    | (TypeExpected | ProductExpected, ProdExtension(t1, t2)) =>
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
        ok(Message.TypeUnderdetermined(Message.ProdExtensionUnderdetermined([t2])))
      | (_, Prod(_)) =>
        ok(Message.TypeUnderdetermined(Message.ProdExtensionUnderdetermined([t1])))
      | _ =>
        ok(
          Message.TypeUnderdetermined(Message.ProdExtensionUnderdetermined([t1, t2]))
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
    | (TypeExpected, Var(name)) =>
      switch (Ctx.is_alias(ctx, name)) {
      | false =>
        switch (Ctx.is_abstract(ctx, name)) {
        | false => err(TypFreeTypeVariable(name))
        | true => ok(Message.Type(Var(name) |> Typ.temp))
        }
      | true => ok(Message.TypeAlias(name, Typ.weak_head_normalize(ctx, utyp)))
      }
    | (TypeExpected, Label(_))
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
    | (VariantExpected(_), Label(_)) => err(TypWantConstructorFoundType(utyp))
    | (LabelExpected(_), _)
    | (LabelProjectionExpected(_), _) => err(TypWantLabel)
    | (ConstructorExpected(_), _)
    | (VariantExpected(_), _) => err(TypWantConstructorFoundType(utyp))
    | (_, Parens(t)) => status_for_node(~expects, t)
    | (TypeExpected, _) => ok(Message.Type(utyp))
    };
  };
  let add = (~expects=expects, ~utyp=utyp, m) => {
    let st = status_for_node(~expects, utyp);
    let cls: Cls.t =
      switch (expects, Typ.cls_of_term(utyp.term)) {
      | (
          TypExpectation.VariantExpected(_)
          | TypExpectation.ConstructorExpected(_),
          Var,
        ) =>
        Cls.Typ(Constructor)
      | (_, cls) => Cls.Typ(cls)
      };
    let info: Info.typ = {
      cls,
      ctx,
      ancestors,
      marks: fst(st),
      message: Option.map(x => Message.TypOk(x), snd(st)),
      expects,
      warnings: [],
      user_term: utyp,
    };
    (info, add_info(ids, InfoTyp(info), m));
  };
  let ancestors = [Typ.rep_id(utyp)] @ ancestors;
  let go =
      (
        ~ctx=ctx,
        ~ancestors=ancestors,
        ~expects=TypExpectation.TypeExpected,
        t: Typ.t,
        m: Map.t,
      ) =>
    utyp_to_info_map(~ctx, ~ancestors, ~expects, t, m);
  switch (term) {
  | Unknown(Hole(MultiHole(tms))) =>
    let (_, _, m) = multi(~ctx, ~ancestors, m, tms);
    add(m);
  | Unknown(_)
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
    let ancestors = List.tl(ancestors); // Recover original ancestors

    let info: Info.typ = {
      cls: Typ(ExplicitNonlabel),
      ctx,
      ancestors,
      marks: [Mark.BadToken("_")],
      message: None,
      expects,
      user_term: utyp,
      warnings: [],
    };
    (info, add_info(ids, InfoTyp(info), m));
  | TupLabel({term: ExplicitNonlabel, _} as label, t) =>
    let (_, m) = go(t, m);

    let label_info: Info.typ = {
      cls: Typ(ExplicitNonlabel),
      ctx,
      ancestors,
      marks: [],
      message: Some(Message.TypOk(Message.EmptyLabel)),
      expects,
      user_term: utyp,
      warnings: [],
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
        variant_to_info_map(~ctx, ~ancestors, ~ty_sum=utyp),
        (m, []),
        variants,
      );
    add(m);
  | Poly({term: Var(name), _} as utpat, tbody) =>
    let body_ctx =
      Ctx.extend_tvar(
        ctx,
        {
          name,
          id: TPat.rep_id(utpat),
          kind: Abstract,
        },
      );
    let m =
      utyp_to_info_map(
        tbody,
        ~ctx=body_ctx,
        ~ancestors,
        ~expects=TypeExpected,
        m,
      )
      |> snd;
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    add(m); // TODO: check with andrew
  | Poly(utpat, tbody) =>
    let m =
      utyp_to_info_map(tbody, ~ctx, ~ancestors, ~expects=TypeExpected, m)
      |> snd;
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    add(m); // TODO: check with andrew
  | ProofOf(e) =>
    let (_, _, m) =
      uexp_to_info_map(~ctx, ~ancestors, ~ana=Atom(Bool) |> Typ.temp, e, m);
    add(m);
  | Rec({term: Var(name), _} as utpat, tbody) =>
    let body_ctx =
      Ctx.extend_tvar(
        ctx,
        {
          name,
          id: TPat.rep_id(utpat),
          kind: Singleton(utyp),
        },
      );
    let m =
      utyp_to_info_map(
        tbody,
        ~ctx=body_ctx,
        ~ancestors,
        ~expects=TypeExpected,
        m,
      )
      |> snd;
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    add(m); // TODO: check with andrew
  | Rec(utpat, tbody) =>
    let m =
      utyp_to_info_map(tbody, ~ctx, ~ancestors, ~expects=TypeExpected, m)
      |> snd;
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    add(m); // TODO: check with andrew
  | Sig(items) =>
    let m =
      List.fold_left(
        (m, item: Sig.t) => {
          let (_, _, m) = any_to_info_map(~ctx, ~ancestors, Sig(item), m);
          m;
        },
        m,
        items,
      );
    add(m);
  };
}
and utpat_to_info_map =
    (~ctx, ~ancestors, utpat: TPat.t, m: Map.t): (Info.tpat, Map.t) => {
  let ids = IdTagged.ids(utpat);
  let term = IdTagged.term_of(utpat);
  let add = m => {
    let st = derive_tpat_status(ctx, utpat);
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
  let ancestors = [TPat.rep_id(utpat)] @ ancestors;
  switch (term) {
  | MultiHole(tms) =>
    let (_, _, m) = multi(~ctx, ~ancestors, m, tms);
    add(m);
  | Invalid(_)
  | EmptyHole
  | Var(_) => add(m)
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
      (m, elab);
    },
  );

let mk = (~ana=Typ.temp(Unknown(SynSwitch)), core: CoreSettings.t, ctx, exp) =>
  core.statics ? mk(ana, ctx, exp) : (Id.Map.empty, Exp.fresh(Tuple([])));
