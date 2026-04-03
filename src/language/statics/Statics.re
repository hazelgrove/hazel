/* STATICS.re

   This module determines the statics semantics of a program.
   It makes use of the following modules:

   INFO.re: Defines the Info.t type which is used to represent the
   static STATUS of a term. This STATUS can be either OK or ERROR,
   and is determined by reconcilling two sources of typing information,
   the ANA and the SELF.

   (ana:Typ.t): Defines the Mode.t type which is used to represent the
   typing expectations imposed by a term's ancestors.

   SELF.re: Define the Self.t type which is used to represent the
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

let rec any_to_info_map =
        (~ctx: Ctx.t, ~ancestors, any: Any.t, m: Map.t): (CoCtx.t, Map.t) =>
  switch (any) {
  | Exp(e) =>
    let ({co_ctx, _}: Info.exp, _, m) =
      uexp_to_info_map(
        ~ctx,
        ~ancestors,
        ~duplicates=[],
        ~expected_labels=None,
        ~label_sort=false,
        e,
        m,
      );
    (co_ctx, m);
  | Pat(p) =>
    let (_, _, m) =
      upat_to_info_map(
        ~is_synswitch=false,
        ~co_ctx=CoCtx.empty,
        ~ancestors,
        ~duplicate_bindings=[],
        ~duplicate_labels=[],
        ~ctx,
        p,
        m,
      );
    (CoCtx.empty, m);
  | TPat(tp) => (
      CoCtx.empty,
      utpat_to_info_map(~ctx, ~ancestors, tp, m) |> snd,
    )
  | Typ(ty) => (
      CoCtx.empty,
      utyp_to_info_map(~ctx, ~ancestors, ty, m) |> snd,
    )
  | Rul(r) =>
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
      let (co_ctxs, m) = multi(~ctx, ~ancestors, m, tms);
      (CoCtx.union(co_ctxs), m);
    | Invalid(_) => (CoCtx.empty, m)
    }
  | Mod(m_term) =>
    let ids = IdTagged.ids(m_term);
    let cls = Cls.Mod(Mod.cls_of_term(m_term.term));
    let add_mod_info = m =>
      add_info(
        ids,
        InfoMod({
          id: IdTagged.rep_id(m_term),
          term: m_term,
          cls,
          sort: Mod,
          ctx,
          ancestors,
        }),
        m,
      );
    switch (m_term.term) {
    | Invalid(_)
    | EmptyHole => (CoCtx.empty, add_mod_info(m))
    | MultiHole(tms) =>
      let (co_ctxs, m) = multi(~ctx, ~ancestors, m, tms);
      (CoCtx.union(co_ctxs), add_mod_info(m));
    | ModLet(p, e) =>
      let (co_ctx_e, m) = any_to_info_map(~ctx, ~ancestors, Exp(e), m);
      let (_, m) = any_to_info_map(~ctx, ~ancestors, Pat(p), m);
      (co_ctx_e, add_mod_info(m));
    | ModType(tp, t) =>
      let (_, m) = any_to_info_map(~ctx, ~ancestors, TPat(tp), m);
      let (_, m) = any_to_info_map(~ctx, ~ancestors, Typ(t), m);
      (CoCtx.empty, add_mod_info(m));
    | ModExp(e) =>
      let (co_ctx, m) = any_to_info_map(~ctx, ~ancestors, Exp(e), m);
      (co_ctx, add_mod_info(m));
    | ModuleMod(mp, e) =>
      let (_, m) = any_to_info_map(~ctx, ~ancestors, MPat(mp), m);
      let (co_ctx, m) = any_to_info_map(~ctx, ~ancestors, Exp(e), m);
      (co_ctx, add_mod_info(m));
    };
  | Sig(s_term) =>
    let ids = IdTagged.ids(s_term);
    let cls = Cls.Sig(Sig.cls_of_term(s_term.term));
    let add_sig_info = m =>
      add_info(
        ids,
        InfoSig({
          id: IdTagged.rep_id(s_term),
          term: s_term,
          cls,
          sort: Sig,
          ctx,
          ancestors,
        }),
        m,
      );
    switch (s_term.term) {
    | Invalid(_)
    | EmptyHole => (CoCtx.empty, add_sig_info(m))
    | MultiHole(tms) =>
      let (co_ctxs, m) = multi(~ctx, ~ancestors, m, tms);
      (CoCtx.union(co_ctxs), add_sig_info(m));
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
      (CoCtx.empty, add_sig_info(m));
    | SigType(tp, t) =>
      let (_, m) = any_to_info_map(~ctx, ~ancestors, TPat(tp), m);
      let (_, m) = any_to_info_map(~ctx, ~ancestors, Typ(t), m);
      (CoCtx.empty, add_sig_info(m));
    };
  | MPat(mp_term) =>
    let ids = IdTagged.ids(mp_term);
    let cls = Cls.MPat(MPat.cls_of_term(mp_term.term));
    let add_mpat_info = m =>
      add_info(
        ids,
        InfoMPat({
          id: IdTagged.rep_id(mp_term),
          term: mp_term,
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
    | Var(_) => (CoCtx.empty, add_mpat_info(m))
    | MultiHole(tms) =>
      let (co_ctxs, m) = multi(~ctx, ~ancestors, m, tms);
      (CoCtx.union(co_ctxs), add_mpat_info(m));
    | Asc(inner, typ) =>
      let (_, m) = any_to_info_map(~ctx, ~ancestors, MPat(inner), m);
      let (_, m) = any_to_info_map(~ctx, ~ancestors, Typ(typ), m);
      (CoCtx.empty, add_mpat_info(m));
    };
  | Any () => (CoCtx.empty, m)
  }
and multi = (~ctx, ~ancestors, m, tms): (list(CoCtx.t), Map.t) =>
  List.fold_left(
    ((co_ctxs, m), any) => {
      let (co_ctx, m) = any_to_info_map(~ctx, ~ancestors, any, m);
      (co_ctxs @ [co_ctx], m);
    },
    ([], m),
    tms,
  )
and uexp_to_info_map =
    (
      ~ctx: Ctx.t,
      ~ana=syn,
      ~is_in_filter=false,
      ~ancestors,
      ~duplicates: list(string),
      ~expected_labels: option(list(string)),
      ~override_self: option(Self.exp)=?,
      ~inferred_label: option(LabeledTuple.label)=?,
      ~label_sort,
      ~dot_labels: list(string)=[],
      {annotation: {ids, _}, term} as uexp: Exp.t,
      m: Map.t,
    )
    : (Info.exp, Exp.t, Map.t) => {
  let add' =
      (
        ~elab: option(Exp.t)=?,
        ~label_inference: option(Info.label_inference(Info.exp))=?,
        ~self: Self.exp,
        ~co_ctx: CoCtx.t,
        m: Map.t,
      )
      : (Info.exp, Exp.t, Map.t) => {
    let info =
      Info.derived_exp(
        ~uexp,
        ~ctx,
        ~ana,
        ~ancestors,
        ~self=Option.value(~default=self, override_self),
        ~co_ctx,
        ~label_inference,
        ~inferred_label,
        ~label_sort,
        ~dot_labels,
      );
    let elab = Option.value(~default=uexp, elab);
    (info, elab, add_info(ids, InfoExp(info), m));
  };
  let add = (~elab=?, ~self, ~co_ctx, m) => {
    add'(~elab?, ~self=Common(self), ~co_ctx, m);
  };
  let fresh_ascription = (d: Exp.t, t: Typ.t, t': option(Typ.t)) => {
    IdTagged.FreshGrammar.Exp.(
      switch (t') {
      | Some({term: Unknown(Internal), _}) => d
      | Some(ty)
          when
            !Typ.fast_equal(
              Typ.normalize(ctx, ty),
              Typ.normalize(ctx, t),
            ) =>
        asc(d, ty)
      | _ => d
      }
    );
  };
  let (_, rewrap) = Exp.unwrap(uexp);
  let ancestors = [Exp.rep_id(uexp)] @ ancestors;
  let uexp_to_info_map =
      (
        ~ctx,
        ~ana=syn,
        ~is_in_filter=is_in_filter,
        ~ancestors=ancestors,
        ~duplicates=[],
        ~expected_labels=?,
        ~inferred_label: option(string)=?,
        ~override_self=?,
        ~label_sort=false,
        ~dot_labels=[],
        uexp: Exp.t,
        m: Map.t,
      ) => {
    uexp_to_info_map(
      ~ctx,
      ~ana,
      ~is_in_filter,
      ~ancestors,
      ~duplicates,
      ~expected_labels,
      ~override_self?,
      ~inferred_label?,
      ~label_sort,
      ~dot_labels,
      uexp,
      m,
    );
  };
  let replace_self = (m: Map.t, original_info: Info.exp, self: Self.exp) => {
    let new_info =
      Info.derived_exp(
        ~uexp=original_info.term,
        ~ctx=original_info.ctx,
        ~ana=original_info.ana,
        ~ancestors=original_info.ancestors,
        ~self,
        ~co_ctx=original_info.co_ctx,
        ~label_inference=original_info.label_inference,
        ~inferred_label=original_info.inferred_label,
        ~dot_labels=original_info.dot_labels,
        ~label_sort=original_info.label_sort,
      );
    (
      new_info,
      uexp,
      add_info(IdTagged.ids(original_info.term), InfoExp(new_info), m),
    );
  };
  let go' = uexp_to_info_map(~ancestors);
  let go:
    (
      ~ana: TermBase.typ_t=?,
      ~is_in_filter: bool=?,
      ~duplicates: list(string)=?,
      ~expected_labels: list(string)=?,
      ~inferred_label: string=?,
      ~override_self: Self.exp=?,
      ~label_sort: bool=?,
      ~dot_labels: list(string)=?,
      TermBase.exp_t,
      Map.t
    ) =>
    (Info.exp, Exp.t, Map.t) =
    go'(~ctx);
  let map_m_go = (m, ~duplicates=[]) =>
    List.fold_left2(
      (((es, elabs), m), ana, e) =>
        go(~ana, ~duplicates, e, m)
        |> (((e, elab, m)) => ((es @ [e], elabs @ [elab]), m)),
      (([], []), m),
    );
  let go_pat = upat_to_info_map(~ctx, ~ancestors);
  let go_typ = utyp_to_info_map(~ctx, ~ancestors);
  let label_to_info_map =
      (expected_labels, labmode, label: Exp.t, m: Map.t)
      : (option(string), Info.exp, Exp.t, Map.t) => {
    switch (label.term, expected_labels) {
    | (Label(name), Some(expected_labels))
        when !List.mem(name, expected_labels) =>
      let (i, i_elab, m) =
        go(
          ~ana=labmode,
          ~override_self=Common(InvalidLabel(name, expected_labels)),
          ~label_sort=true,
          ~duplicates,
          label,
          m,
        );
      (None, i, i_elab, m);
    | (Label(lab), _) =>
      let (i, i_elab, m) =
        go(~ana=labmode, ~label_sort=true, ~duplicates, label, m);
      (Some(lab), i, i_elab, m);
    | (EmptyHole, _) =>
      let (i, i_elab, m) =
        go(~ana=labmode, ~label_sort=true, ~duplicates, label, m);
      (None, i, i_elab, m);
    | _ =>
      let (i, i_elab, m) =
        go(
          ~ana=labmode,
          ~override_self=Common(BadLabel(Exp(label))),
          ~label_sort=true,
          ~duplicates,
          label,
          m,
        );
      (None, i, i_elab, m);
    };
  };
  // This lifts an expression into a singleton labeled tuple by rewriting the syntax in the Statics Map
  let autolabel_singleton_tuple = (uexp: Exp.t, inner_ty, l, m) => {
    let (term, rewrap) = Exp.unwrap(uexp);
    let original_expression = Exp.fresh(term);
    let (original_info, _, m) =
      uexp_to_info_map(
        ~ctx,
        ~ana=inner_ty,
        ~is_in_filter,
        ~ancestors,
        original_expression,
        m,
      );

    let elaborated_exp =
      rewrap(
        Tuple([
          TupLabel(Label(l) |> Exp.fresh, original_expression) |> Exp.fresh,
        ]),
      );

    // We need to reanalyze the elaborated expression to get the statics in the map for the label and tuple
    let (info, _, m) =
      uexp_to_info_map(~ctx, ~ana, ~ancestors, elaborated_exp, m);

    // We need to keep the original status of the expression to get error messages on the unelaborated expression
    let info = {
      ...info,
      status: original_info.status,
      label_inference:
        Some(
          SingletonLabelInference({
            label: l,
            pre_labeled_info: original_info,
          }),
        ),
    };

    (
      info,
      elaborated_exp,
      add_info(IdTagged.ids(elaborated_exp), InfoExp(info), m),
    );
  };

  let atomic = (~elab=?, self) => {
    // HACK: we use the co-context to check for unused variables in surrounding
    // pattern bindings, but we don't want unused variable warnings to appear
    // when there are holes present in the binding scopes. so if we detect a
    // a hole in this expression, we add a "$hole" entry to the co-context
    // that gets bubbled up to the relevant bindings and is checked for in the
    // warning logic.
    let hole_co_ctx =
      switch (term) {
      | MultiHole(_)
      | EmptyHole
      | Invalid(_) =>
        CoCtx.singleton(
          "$hole",
          Exp.rep_id(uexp),
          Unknown(Internal) |> Typ.temp,
        )
      | _ => CoCtx.empty
      };
    add(~elab?, ~self, ~co_ctx=hole_co_ctx, m);
  };
  // This is the case where we aren't a singleton labeled tuple
  let default_case = () => {
    switch (term) {
    | Closure(env, e) =>
      // TODO: implement closure type checking properly - see how dynamic type assignment does it
      let (e, e_elab, m) = go(~ana, e, m);
      add(
        ~elab=Closure(env, e_elab) |> rewrap,
        ~self=Just(e.ty),
        ~co_ctx=e.co_ctx,
        m,
      );
    | MultiHole([Exp(e1), Exp(e2)]) =>
      let (e1, e1_elab, m) = go(~ana=syn, e1, m);
      let (e2, e2_elab, m) = go(~ana=syn, e2, m);
      add(
        ~elab=Seq(e1_elab, e2_elab) |> rewrap,
        ~self=IsMulti,
        ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
        m,
      );
    | MultiHole(tms) =>
      let (co_ctxs, m) = multi(~ctx, ~ancestors, m, tms);
      let tms_elab =
        Any.map_term(
          ~f_exp=
            (_, exp) => go(~ana=syn, exp, m) |> (((_, elab, _)) => elab),
          ~f_pat=
            (_, pat) =>
              go_pat(
                ~is_synswitch=false,
                ~co_ctx=CoCtx.empty,
                ~ana=syn,
                pat,
                m,
              )
              |> (((_, elab, _)) => elab),
          _,
        )
        |> List.map(_, tms);
      add(
        ~elab=MultiHole(tms_elab) |> rewrap,
        ~self=IsMulti,
        ~co_ctx=CoCtx.union(co_ctxs),
        m,
      );
    | Asc(e, t2) =>
      let (t, m) = go_typ(t2, ~expects=Info.TypeExpected, m);
      /* Desugar any Sig types in the annotation without full normalization */
      let t_ty = Typ.desugar_sig(ctx, t.term);
      let (e, e_elab, m) = go'(~ana=t_ty, ~ctx=t.ctx, e, m);
      let typ_refs = ModuleHelpers.collect_module_refs_in_typ(ctx, Typ.rep_id(t2), t2);
      add(
        ~elab=Asc(e_elab, Typ.normalize(ctx, t2)) |> rewrap,
        ~self=Just(t_ty),
        ~co_ctx=CoCtx.union([e.co_ctx, typ_refs]),
        m,
      );
    | Invalid(token) => atomic(BadToken(token))
    | EmptyHole => atomic(Just(Unknown(Internal) |> Typ.temp))
    | Deferral(position) =>
      add'(~self=IsDeferral(position), ~co_ctx=CoCtx.empty, m)
    | Undefined => atomic(Just(Unknown(Hole(EmptyHole)) |> Typ.temp))
    | Atom(c) =>
      let c =
        Operators.replace_literal(c, Typ.is_ana_atom(ana), ctx.use_mode); // Replace literal if necessary due to `use`
      switch (c) {
      | L(c) =>
        let ty = Atom(Atom.cls_of_t(c)) |> Typ.temp;
        atomic(~elab=Atom(c) |> rewrap, Just(ty));
      | R(BadInt(str)) =>
        atomic(~elab=Invalid(str) |> rewrap, BadToken(str))
      };

    | LivelitName(name) =>
      add'(
        ~self=Self.of_exp_livelit_name(ctx, name),
        ~co_ctx=CoCtx.singleton(name, Exp.rep_id(uexp), ana),
        m,
      )
    | ListLit(es) =>
      let ids = List.map(Exp.rep_id, es);
      let inner_ana_ty = Typ.matched_list(ctx, ana);
      let anas = List.init(List.length(es), _ => inner_ana_ty);
      let ((es, es_elabs), m) = map_m_go(m, anas, es);
      let tys = List.map(Info.exp_ty, es);
      let meet_ty =
        Typ.meet_all(~empty=Unknown(Internal) |> Typ.temp, ctx, tys);
      let ds =
        List.map2((d, t) => fresh_ascription(d, t, meet_ty), es_elabs, tys);
      add(
        ~elab=ListLit(ds) |> rewrap,
        ~self=
          Self.listlit(~empty=Unknown(Internal) |> Typ.temp, ctx, tys, ids),
        ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
        m,
      );
    | Cons(hd, tl) =>
      let inner_ana_ty = Typ.matched_list(ctx, ana);
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
        ~self=Just(self_ty),
        ~co_ctx=CoCtx.union([hd.co_ctx, tl.co_ctx]),
        m,
      );
    | ListConcat(e1, e2) =>
      let inner_ana_ty = List(Typ.matched_list(ctx, ana)) |> Typ.temp;
      let ids = List.map(Exp.rep_id, [e1, e2]);
      let (e1, e1_elab, m) = go(~ana=inner_ana_ty, e1, m);
      let (e2, e2_elab, m) = go(~ana=inner_ana_ty, e2, m);
      add(
        ~elab=ListConcat(e1_elab, e2_elab) |> rewrap,
        ~self=Self.list_concat(ctx, [e1.ty, e2.ty], ids),
        ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
        m,
      );
    | Var(name) =>
      let co_ctx = CoCtx.singleton(name, Exp.rep_id(uexp), ana);
      add'(~self=Self.of_exp_var(ctx, name), ~co_ctx, m);
    | DynamicErrorHole(e, err) =>
      let (e, e_elab, m) = go(~ana, e, m);
      add'(
        ~elab=DynamicErrorHole(e_elab, err) |> rewrap,
        ~self=e.self,
        ~co_ctx=e.co_ctx,
        m,
      );
    | Parens(e) =>
      let (e, e_elab, m) = go(~ana, e, m);
      add'(
        ~elab=Parens(e_elab) |> rewrap,
        ~self=e.self,
        ~co_ctx=e.co_ctx,
        m,
      );
    | Projector(data, e) =>
      let (e, e_elab, m) = go(~ana, e, m);
      add'(
        ~elab=Projector(data, e_elab) |> rewrap,
        ~self=e.self,
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
            ~self=Just(Unknown(Internal) |> Typ.temp),
            ~co_ctx=e.co_ctx,
            m,
          );
        } else {
          add'(
            ~elab=unquote_elab,
            ~self=BadOperator("Unquote not in filter"),
            ~co_ctx=e.co_ctx,
            m,
          );
        };
      let m =
        switch (unquote_elab.term) {
        | Constructor(_, Some(Some(typ))) =>
          go_typ(typ, ~expects=Info.TypeExpected, m) |> snd
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
        let (_, _, m) = go(~ana=syn, e, m);
        add'(~self=BadOperator(msg), ~co_ctx=CoCtx.empty, m);
      | Defined(ty_in, ty_out, _) =>
        let ty_in = Atom(Atom.cls_of_kind(ty_in)) |> Typ.temp;
        let ty_out = Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp;
        let (e, e_elab, m) = go(~ana=ty_in, e, m);
        add(
          ~elab=UnOp(op, e_elab) |> rewrap,
          ~self=Just(ty_out),
          ~co_ctx=e.co_ctx,
          m,
        );
      };
    | BinOp(op, e1, e2) =>
      let op = Operators.replace_bin_op(op, ctx.use_mode); // Replace op if necessary due to `use`
      let op_semantics = Operators.semantics_of_bin_op(op);
      switch (op_semantics) {
      | Undefined(msg) =>
        let (_, _, m) = go(~ana=syn, e1, m);
        let (_, _, m) = go(~ana=syn, e2, m);
        add'(~self=BadOperator(msg), ~co_ctx=CoCtx.empty, m);
      | DefinedPoly(_) =>
        let ids = List.map(Exp.rep_id, [e1, e2]);
        let ((es, es_elabs), m) =
          map_m_go(
            m,
            [Unknown(Internal) |> Typ.temp, Unknown(Internal) |> Typ.temp],
            [e1, e2],
          );
        let tys = List.map(Info.exp_ty, es);
        add(
          ~elab=
            BinOp(op, List.nth(es_elabs, 0), List.nth(es_elabs, 1))
            |> rewrap,
          ~self=Self.poly_eq(ctx, tys, ids),
          ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
          m,
        );
      | Defined(ty1, ty2, ty_out, _) =>
        let ty1 = Atom(Atom.cls_of_kind(ty1)) |> Typ.temp;
        let ty2 = Atom(Atom.cls_of_kind(ty2)) |> Typ.temp;
        let ty_out = Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp;
        let (e1, e1_elab, m) = go(~ana=ty1, e1, m);
        let (e2, e2_elab, m) = go(~ana=ty2, e2, m);
        add(
          ~elab=BinOp(op, e1_elab, e2_elab) |> rewrap,
          ~self=Just(ty_out),
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
          let (t1, _, m) = replace_self(m, t1, TupleExtensionRequiresTuples);
          (t1, e1_elab, m);
        };
      };
      let (t2, e2_elab, m) = {
        let (t2, e2_elab, m) = go(e2, m);
        switch (Typ.normalize(ctx, t2.ty).term) {
        | Prod(_)
        | Unknown(_) => (t2, e2_elab, m)
        | _ =>
          let (t2, _, m) = replace_self(m, t2, TupleExtensionRequiresTuples);
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

        add(
          ~elab,
          ~self=Just(ty), // TODO: fix this
          ~co_ctx,
          m,
        );
      | (Unknown(_), _)
      | (_, Unknown(_)) =>
        add(
          ~elab,
          ~self=Just(IdTagged.FreshGrammar.Typ.unknown(Internal)),
          ~co_ctx,
          m,
        )
      | _ =>
        add(
          ~elab,
          ~self=Just(IdTagged.FreshGrammar.Typ.unknown(Internal)),
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
        Typ.matched_prod(
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

      let duplicate_labels =
        LabeledTuple.get_duplicate_labels(Exp.match_tup_label, es);

      let (es', es_elab, m) =
        List.fold_left2(
          ((es, es_elab, m), ana, (inferred_label, e)) => {
            go(
              ~ana,
              ~inferred_label?,
              ~duplicates=duplicate_labels,
              ~expected_labels?,
              e,
              m,
            )
            |> (((e, elab, m)) => (es @ [e], es_elab @ [elab], m))
          },
          ([], [], m),
          ana_tys,
          List.combine(inferred, es),
        );
      let ty_list = List.map(Info.exp_ty, es');

      let (malformed_labels, duplicate_labels, invalid_labels) =
        List.fold_left2(
          ((a, b, c), e: Exp.t, e_info: Info.exp) => {
            // Only collect errors from TupLabel elements
            switch (e.term, e_info.status) {
            | (
                TupLabel(_, _),
                InHole(
                  Common(
                    TupleLabelError({
                      malformed_labels,
                      duplicate_labels,
                      invalid_labels,
                      _,
                    }),
                  ),
                ),
              ) => (
                a @ malformed_labels,
                b @ duplicate_labels,
                c @ invalid_labels,
              )
            | _ => (a, b, c)
            }
          },
          ([], [], []),
          es,
          es',
        );

      let ty_list = Typ.remove_duplicate_labels(~duplicate_labels, ty_list);

      let self =
        List.is_empty(malformed_labels)
        && List.is_empty(duplicate_labels)
        && List.is_empty(invalid_labels)
          ? Self.Just(Prod(ty_list) |> Typ.temp)
          : Self.TupleLabelError({
              malformed_labels,
              duplicate_labels,
              invalid_labels,
              typ: Prod(ty_list) |> Typ.temp,
            });
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
      add'(
        ~elab=tuple_elab,
        ~self=Common(self),
        ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es')),
        ~label_inference=
          Info.derive_label_inference_info(original_labels, new_labels),
        m,
      );
    | TupLabel({term: ExplicitNonlabel, _} as label, e) =>
      let (e, _, m) = go(~ana, e, m);
      let (_, _, m) = go(~label_sort=true, label, m);
      add(~self=Just(e.ty), ~co_ctx=e.co_ctx, m);
    | TupLabel(label, e) =>
      let (lab, e, m) =
        switch (Typ.matched_label(ctx, ana)) {
        | Some((labmode, val_mode)) =>
          let (_, lab, _, m) =
            label_to_info_map(expected_labels, labmode, label, m);

          let (e, _, m) = go(~ana=val_mode, ~inferred_label?, e, m);
          (lab, e, m);
        | _ =>
          let (_, lab, _, m) =
            label_to_info_map(
              expected_labels,
              Unknown(SynSwitch) |> Typ.temp,
              label,
              m,
            );

          let (e, _, m) =
            go(~ana=Unknown(Internal) |> Typ.temp, ~inferred_label?, e, m);
          (lab, e, m);
        };

      let self =
        switch (lab.status) {
        | NotInHole(_) => Self.Just(TupLabel(lab.ty, e.ty) |> Typ.temp)
        | InHole(
            Common(
              Inconsistent(Expectation({syn: {term: Label(name), _}, _})),
            ),
          )
        | InHole(Common(NoType(InvalidLabel(name, _)))) =>
          Self.TupleLabelError({
            malformed_labels: [],
            duplicate_labels: [],
            invalid_labels: [name],
            typ: TupLabel(Label(name) |> Typ.temp, e.ty) |> Typ.temp,
          })
        | InHole(Common(DuplicateLabel(name, _))) =>
          Self.TupleLabelError({
            malformed_labels: [],
            duplicate_labels: [name],
            invalid_labels: [],
            typ: TupLabel(Label(name) |> Typ.temp, e.ty) |> Typ.temp,
          })
        | InHole(_) =>
          Self.TupleLabelError({
            malformed_labels: [Exp(label)],
            duplicate_labels: [],
            invalid_labels: [],
            typ: TupLabel(Unknown(Internal) |> Typ.temp, e.ty) |> Typ.temp,
          })
        };
      add(~self, ~co_ctx=CoCtx.union([lab.co_ctx, e.co_ctx]), m);
    | ExplicitNonlabel => atomic(ExplicitNonlabel)
    | Label(name) when label_sort =>
      let self = Self.Just(Label(name) |> Typ.temp);
      List.exists(l => name == l, duplicates)
        ? atomic(DuplicateLabel(name, self)) : atomic(self);
    | Label(name) =>
      let self = Self.UnexpectedLabelSort(name);
      atomic(self);
    | BuiltinFun(string) =>
      add'(
        ~self=Self.of_exp_var(Builtins.ctx_init(None), string),
        ~co_ctx=CoCtx.empty,
        m,
      )

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
      let (info_e2, _, m) =
        go(
          ~label_sort=true,
          ~dot_labels=available_labels,
          ~ana=Label("") |> Typ.temp,
          e2,
          m,
        );
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
            add(~elab=dot_elab, ~self=Just(typ), ~co_ctx=dot_co_ctx, m)
          | None =>
            add'(
              ~elab=dot_elab,
              ~self=LabelNotFound(name, labels),
              ~co_ctx=dot_co_ctx,
              m,
            )
          };
        | EmptyHole =>
          add(
            ~elab=dot_elab,
            ~self=Just(Unknown(Internal) |> Typ.temp),
            ~co_ctx=dot_co_ctx,
            m,
          )
        | _ =>
          add(
            ~elab=dot_elab,
            ~self=BadLabel(Exp(e2)),
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
              ~self=Just(List(typ) |> Typ.fresh),
              ~co_ctx=dot_co_ctx,
              m,
            )
          | None =>
            add'(
              ~elab=dot_elab,
              ~self=LabelNotFound(name, labels),
              ~co_ctx=dot_co_ctx,
              m,
            )
          };
        | EmptyHole =>
          add(
            ~elab=dot_elab,
            ~self=Just(Unknown(Internal) |> Typ.temp),
            ~co_ctx=dot_co_ctx,
            m,
          )
        | _ =>
          add(
            ~elab=dot_elab,
            ~self=BadLabel(Exp(e2)),
            ~co_ctx=dot_co_ctx,
            m,
          )
        };
      | List({term: Unknown(_), _}) =>
        add(
          ~elab=dot_elab,
          ~self=Just(List(Unknown(Internal) |> Typ.temp) |> Typ.temp),
          ~co_ctx=dot_co_ctx,
          m,
        )
      | _ =>
        add'(
          ~elab=dot_elab,
          ~self=DotOperatorRequiresTuple,
          ~co_ctx=dot_co_ctx,
          m,
        )
      };
    | Test(e) =>
      let (e, e_elab, m) = go(~ana=Atom(Bool) |> Typ.temp, e, m);
      add(
        ~elab=Test(e_elab) |> rewrap,
        ~self=Just(Prod([]) |> Typ.temp),
        ~co_ctx=e.co_ctx,
        m,
      );
    | HintedTest(e, hint) =>
      let (e, e_elab, m) = go(~ana=Atom(Bool) |> Typ.temp, e, m);
      let (hint, hint_elab, m) = go(~ana=Atom(String) |> Typ.temp, hint, m);
      add(
        ~elab=HintedTest(e_elab, hint_elab) |> rewrap,
        ~self=Just(Prod([]) |> Typ.temp),
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
        ~self=Just(body.ty),
        ~co_ctx=CoCtx.union([cond.co_ctx, body.co_ctx]),
        m,
      );
    | Filter(Residue(i, act), body) =>
      let (body, body_elab, m) = go(~ana, body, m);
      add(
        ~elab=Filter(Residue(i, act), body_elab) |> rewrap,
        ~self=Just(body.ty),
        ~co_ctx=CoCtx.union([body.co_ctx]),
        m,
      );
    | Seq(e1, e2) =>
      let (e1, e1_elab, m) = go(~ana=syn, e1, m);
      let (e2, e2_elab, m) = go(~ana, e2, m);
      add(
        ~elab=Seq(e1_elab, e2_elab) |> rewrap,
        ~self=Just(e2.ty),
        ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
        m,
      );
    | Constructor(ctr, ty) =>
      let self = Self.of_ctr(ctx, ctr, ana, ty);
      switch (self) {
      | FreeConstructor(name) =>
        /* If not a known constructor, try looking up as a variable.
           This supports capitalized module names like M.x where M is
           parsed as Constructor but is actually a variable binding. */
        switch (Ctx.lookup_var(ctx, name)) {
        | Some({typ, _}) =>
          let co_ctx = CoCtx.singleton(name, Exp.rep_id(uexp), ana);
          let elab = Var(name) |> rewrap;
          let (info, _, m) = add(~elab, ~self=Just(typ), ~co_ctx, m);
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
          atomic(~elab, self);
        }
      | _ =>
        let info =
          Info.derived_exp(
            ~uexp,
            ~ctx,
            ~ana,
            ~ancestors,
            ~self=Common(self),
            ~co_ctx=CoCtx.empty,
            ~label_inference=None,
            ~inferred_label,
            ~label_sort,
            ~dot_labels,
          );
        let elab =
          Constructor(ctr, Some(Some(Typ.normalize(ctx, info.ty))))
          |> rewrap;
        atomic(~elab, self);
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
          switch (expand(arg.term)) {
          | Some(expanded) =>
            let (info, elab, m) =
              add(
                ~elab=expanded,
                ~self=Just(expansion_t),
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
            add'(
              ~self=BadLivelitModel(expansion_t),
              ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]),
              m,
            )
          };

        | None =>
          let (fn, _, m) = go(~ana=Unknown(Internal) |> Typ.temp, fn, m);
          let (arg, _, m) = go(~ana=Unknown(Internal) |> Typ.temp, arg, m);
          add(
            ~self=Just(Unknown(Internal) |> Typ.temp),
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
            switch (Self.ctr_ana_typ(ctx, ana, name)) {
            | Some(ty_ana) =>
              switch (Typ.matched_arrow_strict(ctx, ty_ana)) {
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
            ~inferred_label,
            ~label_sort,
            ~ctx,
            ~ancestors,
            ~fn_info=fn,
            kind,
            (module
             {
               let uexp_to_info_map = uexp_to_info_map;
               let label_to_info_map = label_to_info_map;
               let add' = add';
             }),
            m,
            arg,
          )
        | None =>
          let (ty_in, ty_out) = Typ.matched_arrow(ctx, fn.ty);
          let (arg, arg_elab, m) = go(~ana=ty_in, arg, m);
          let self: Self.exp =
            Id.is_nullary_ap_flag(IdTagged.ids(arg.term))
            && !Typ.is_consistent(ctx, ty_in, Prod([]) |> Typ.temp)
              ? BadTrivAp(ty_in) : Common(Just(ty_out));
          let elab = Ap(dir, fn_elab, arg_elab) |> rewrap;
          add'(
            ~elab,
            ~self,
            ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]),
            m,
          );
        };
      }
    | TypAp(fn, utyp) =>
      let typfn_ana = Poly(EmptyHole |> TPat.fresh, syn) |> Typ.temp;
      let (fn, fn_elab, m) = go(~ana=typfn_ana, fn, m);
      let (_, m) = utyp_to_info_map(~ctx, ~ancestors, utyp, m);
      let elab = TypAp(fn_elab, Typ.normalize(ctx, utyp)) |> rewrap;
      let (option_name, ty_body) = Typ.matched_poly(ctx, fn.ty);
      switch (option_name) {
      | Some(name) =>
        add(
          ~elab,
          ~self=Just(Typ.subst(utyp, name, ty_body)),
          ~co_ctx=fn.co_ctx,
          m,
        )
      | None => add(~elab, ~self=Just(ty_body), ~co_ctx=fn.co_ctx, m) /* invalid name matches with no free type variables. */
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
          switch (Self.ctr_ana_typ(ctx, ana, name)) {
          | Some(ty_ana) =>
            switch (Typ.matched_arrow_strict(ctx, ty_ana)) {
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
          ~inferred_label,
          ~label_sort,
          ~ctx,
          ~ancestors,
          ~fn_info=fn,
          kind,
          (module
           {
             let uexp_to_info_map = uexp_to_info_map;
             let label_to_info_map = label_to_info_map;
             let add' = add';
           }),
          m,
          args,
        )
      | None =>
        let (ty_in, ty_out) = Typ.matched_arrow(ctx, fn.ty);
        let num_args = List.length(args);
        switch (Typ.matched_args_strict(ctx, ty_in, num_args)) {
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
            ~self=Just(Arrow(ty_in', ty_out) |> Typ.temp),
            ~co_ctx=CoCtx.union([fn.co_ctx, arg_co_ctx]),
            m,
          );
        | R(expected) =>
          let ty_ins =
            List.init(num_args, _ => Unknown(Internal) |> Typ.temp);
          let ((args, args_elabs), m) = map_m_go(m, ty_ins, args);
          let arg_co_ctx = CoCtx.union(List.map(Info.exp_co_ctx, args));
          add'(
            ~elab=DeferredAp(fn_elab, args_elabs) |> rewrap,
            ~self=
              IsBadPartialAp(
                ArityMismatch({
                  expected,
                  actual: num_args,
                }),
              ),
            ~co_ctx=CoCtx.union([fn.co_ctx, arg_co_ctx]),
            m,
          );
        };
      };
    | Fun(p, e, typ, n) =>
      let pat_typ_refs = ModuleHelpers.collect_pat_type_refs(ctx, p);
      let (mode_pat, mode_body) = Typ.matched_arrow(ctx, ana);
      let mode_pat = Option.value(~default=mode_pat, typ);
      let (p', _, _) =
        go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~ana=mode_pat, p, m);
      let (e, e_elab, m) = go'(~ctx=p'.ctx, ~ana=mode_body, e, m);
      /* add co_ctx to pattern */
      let (p, p_elab, m) =
        go_pat(~is_synswitch=false, ~co_ctx=e.co_ctx, ~ana=mode_pat, p, m);
      // TODO: factor out code
      let unwrapped_self: Self.exp =
        Common(Just(Arrow(p.ty, e.ty) |> Typ.temp));
      let Coverage.CheckMatrix.{exhaustiveness, _} =
        Coverage.check([Info.pat_constraint(p)], Typ.normalize(ctx, p.ty));
      let self =
        switch (exhaustiveness) {
        | Exhaustive => unwrapped_self
        | Inexhaustive(unseen_pattern) =>
          InexhaustiveMatch(unwrapped_self, unseen_pattern)
        };
      let elab = Fun(p_elab, e_elab, Some(p.ty), n) |> rewrap;
      add'(
        ~elab,
        ~self,
        ~co_ctx=CoCtx.union([CoCtx.mk(ctx, p.ctx, e.co_ctx), pat_typ_refs]),
        m,
      );
    | Forall(p, e) =>
      let (p, p_elab, m) =
        go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, p, m);
      let (e, e_elab, m) =
        go'(~ctx=p.ctx, ~ana=Atom(Bool) |> Typ.temp, e, m);
      add'(
        ~elab=Forall(p_elab, e_elab) |> rewrap,
        ~self=Common(Just(Atom(Bool) |> Typ.temp)),
        ~co_ctx=CoCtx.mk(ctx, p.ctx, e.co_ctx),
        m,
      );
    | TypFun(utpat, body, tfname) =>
      let (name_expected_opt, item) = Typ.matched_poly(ctx, ana);
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
      let (body, body_elab, m) = go'(~ctx=ctx_body, ~ana=mode_body, body, m);
      add(
        ~elab=TypFun(utpat, body_elab, tfname) |> rewrap,
        ~self=Just(Poly(utpat, body.ty) |> Typ.temp),
        ~co_ctx=body.co_ctx,
        m,
      );
    | Let(p, def, body) =>
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
      let (def_rec_probe, _, _) = go'(~ctx=p_syn.ctx, ~ana=p_syn.ty, def, m);
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
          let (def_base, _, _) = go'(~ctx=p_syn.ctx, ~ana=p_syn.ty, def, m);
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
          let (def_base2, _, _) = go'(~ctx=def_ctx, ~ana=p_syn.ty, def, m);
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
          let (def, def_elab, m) = go'(~ctx=def_ctx, ~ana, def, m);
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
      let (body, body_elab, m) = go'(~ctx=p_ana_ctx, ~ana, body, m);
      /* add co_ctx to pattern */
      let (p_ana, p_elab, m) =
        go_pat(~is_synswitch=false, ~co_ctx=body.co_ctx, ~ana=ty_p_ana, p, m);
      // TODO: factor out code
      let unwrapped_self: Self.exp = Common(Just(body.ty));
      let Coverage.CheckMatrix.{exhaustiveness, _} =
        Coverage.check(
          [Info.pat_constraint(p_ana)],
          Typ.normalize(ctx, p_ana.ty),
        );
      let self =
        switch (exhaustiveness) {
        | Exhaustive => unwrapped_self
        | Inexhaustive(unseen_pattern) =>
          InexhaustiveMatch(unwrapped_self, unseen_pattern)
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
      let maybe_align_def_elab = def_elab =>
        if (LabeledTupleHelpers.is_aligned_exp(ctx, p_syn.ty, def_elab)) {
          def_elab;
        } else {
          LabeledTupleHelpers.align_exp(ctx, p_syn.ty, def_elab);
        };
      let elab =
        if (!requires_fixf) {
          let def_elab =
            maybe_align_def_elab(def_elab) |> Exp.add_name(Pat.get_var(p));
          Let(p_elab, def_elab, body_elab) |> rewrap;
        } else {
          let def_elab =
            maybe_align_def_elab(def_elab)
            |> Exp.add_name(Option.map(s => s ++ "+", Pat.get_var(p)));
          let fixf =
            (FixF(p_elab, def_elab, None): Exp.term)
            |> IdTagged.fresh_deterministic(Exp.rep_id(uexp));
          Let(p_elab, fixf, body_elab) |> rewrap;
        };
      add'(
        ~elab,
        ~self,
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
      let (e1', e1_elab, m) = go'(~ctx, ~ana=Atom(Bool) |> Typ.temp, e1, m);
      let (p', _, _) =
        go_pat(
          ~is_synswitch=false,
          ~co_ctx=CoCtx.empty,
          ~ana=Typ.fresh(ProofOf(e1)),
          p,
          m,
        );
      let (e2, e2_elab, m) = go'(~ctx=p'.ctx, ~ana, e2, m);
      /* add co_ctx to pattern */
      let (p, p_elab, m) =
        go_pat(~is_synswitch=false, ~co_ctx=e2.co_ctx, ~ana=syn, p, m);
      add(
        ~elab=Theorem(p_elab, e1_elab, e2_elab) |> rewrap,
        ~self=Just(e2.ty),
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
      let (_, e1_elab, m) = go'(~ctx, ~ana=Atom(Bool) |> Typ.temp, e1, m);
      let (p', _, _) =
        go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~ana=syn, p, m);
      let (e2, e2_elab, m) = go'(~ctx=p'.ctx, ~ana, e2, m);
      /* add co_ctx to pattern */
      let (p, p_elab, m) =
        go_pat(~is_synswitch=false, ~co_ctx=e2.co_ctx, ~ana=syn, p, m);
      add'(
        ~elab=Theorem(p_elab, e1_elab, e2_elab) |> rewrap,
        ~self=BadTheorem(e2.ty),
        ~co_ctx=
          CoCtx.union([
            p'.co_ctx,
            CoCtx.mk(ctx, p.ctx, e2.co_ctx),
            pat_typ_refs,
          ]),
        m,
      );
    | ProofObject(e) =>
      let (_, e_elab, m) = go'(~ctx, ~ana=Atom(Bool) |> Typ.temp, e, m);
      add(
        ~elab=ProofObject(e_elab) |> rewrap,
        ~self=Just(Typ.temp(ProofOf(e))),
        ~co_ctx=CoCtx.empty,
        m,
      ); // TODO[Matt]: do types need coctxs now?
    | FixF(p, e, env) =>
      let (p', _, _) =
        go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~ana, p, m);
      let (e', e_elab, m) = go'(~ctx=p'.ctx, ~ana=p'.ty, e, m);
      let (p'', p_elab, m) =
        go_pat(~is_synswitch=false, ~co_ctx=e'.co_ctx, ~ana, p, m);
      let pat_typ_refs = ModuleHelpers.collect_pat_type_refs(ctx, p);
      let elab =
        FixF(p_elab, Asc(e_elab, p'.ty) |> Exp.fresh, env) |> rewrap;
      add(
        ~elab,
        ~self=Just(p'.ty),
        ~co_ctx=
          CoCtx.union([CoCtx.mk(ctx, p''.ctx, e'.co_ctx), pat_typ_refs]),
        m,
      );
    | If(e0, e1, e2) =>
      let branch_ids = List.map(Exp.rep_id, [e1, e2]);
      let (cond, cond_elab, m) = go(~ana=Atom(Bool) |> Typ.temp, e0, m);
      let (cons, cons_elab, m) = go(~ana, e1, m);
      let (alt, alt_elab, m) = go(~ana, e2, m);
      let self = Self.match(ctx, [cons.ty, alt.ty], branch_ids);
      let result_ty =
        Info.fixed_typ_exp(ctx, ana, Common(self))
        |> Typ.normalize(ctx)
        |> Typ.all_ids_temp;
      let elab =
        If(
          cond_elab,
          fresh_ascription(cons_elab, cons.ty, Some(result_ty)),
          fresh_ascription(alt_elab, alt.ty, Some(result_ty)),
        )
        |> rewrap;
      add(
        ~elab,
        ~self,
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
            go'(~ctx, ~ana, e, m)
            |> (((e, elab, m)) => (es @ [e], elabs @ [elab], m)),
          ([], [], m),
          es,
          p_ctxs,
        );

      let e_tys = List.map(Info.exp_ty, es);
      let e_co_ctxs = List.map(Info.exp_co_ctx, es);
      let unwrapped_self: Self.exp =
        Common(Self.match(ctx, e_tys, branch_ids));
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

      let self =
        switch (exhaustiveness) {
        | Exhaustive => unwrapped_self
        | Inexhaustive(unseen_pattern) =>
          InexhaustiveMatch(unwrapped_self, unseen_pattern)
        };
      let add_redundancy = (ps: list(TermBase.pat_t), redundant_rows, m) => {
        List.fold_left(
          (m, row) => {
            let p = List.nth(ps, row);
            switch (Id.Map.find(IdTagged.rep_id(p), m)) {
            | Info.InfoPat(info) =>
              let info =
                Info.derived_pat(
                  ~upat=info.term,
                  ~ctx=info.ctx,
                  ~co_ctx=info.co_ctx,
                  ~prev_synswitch=info.prev_synswitch,
                  ~ana=info.ana,
                  ~ancestors=info.ancestors,
                  ~self=Self.Redundant(info.self),
                  ~constraint_=info.constraint_,
                  ~label_inference=info.label_inference,
                  ~inferred_label=info.inferred_label,
                  ~label_sort=info.label_sort,
                );
              add_info(IdTagged.ids(p), InfoPat(info), m);
            | _ => failwith("Invalid sort for pattern.")
            };
          },
          m,
          redundant_rows,
        );
      };
      let m = add_redundancy(ps, redundant_rows, m);
      let co_ctx =
        CoCtx.union([
          scrut.co_ctx,
          ...List.map2(CoCtx.mk(ctx), p_ctxs, e_co_ctxs),
        ]);
      /* Build elaboration with ascriptions on branch bodies */
      let result_ty =
        Info.fixed_typ_exp(
          ctx,
          ana,
          Common(Self.match(ctx, e_tys, branch_ids)),
        )
        |> Typ.normalize(ctx)
        |> Typ.all_ids_temp;
      let es_elabs =
        List.map2(
          (e_elab, ty) => fresh_ascription(e_elab, ty, Some(result_ty)),
          es_elabs,
          e_tys,
        );
      let elab =
        Match(scrut_elab, List.combine(ps_elabs, es_elabs)) |> rewrap;
      add'(~elab, ~self, ~co_ctx, m);
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
          go'(~ctx=ctx_body, ~ana, body, m);
        /* Make sure types don't escape their scope */
        let ty_escape = Typ.subst(ty_def, typat, ty_body);
        let m = utyp_to_info_map(~ctx=ctx_def, ~ancestors, utyp, m) |> snd;
        let typ_refs =
          ModuleHelpers.collect_module_refs_in_typ(ctx, Typ.rep_id(utyp), utyp);
        add(
          ~elab=body_elab,
          ~self=Just(ty_escape),
          ~co_ctx=CoCtx.union([co_ctx, typ_refs]),
          m,
        );
      | Var(_)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_) =>
        let ({co_ctx, ty: ty_body, _}: Info.exp, body_elab, m) =
          go'(~ctx, ~ana, body, m);
        let m = utyp_to_info_map(~ctx, ~ancestors, utyp, m) |> snd;
        let typ_refs =
          ModuleHelpers.collect_module_refs_in_typ(ctx, Typ.rep_id(utyp), utyp);
        add(
          ~elab=body_elab,
          ~self=Just(ty_body),
          ~co_ctx=CoCtx.union([co_ctx, typ_refs]),
          m,
        );
      };
    | Use(typ, body) =>
      let (typ, m) = utyp_to_info_map(~ctx, ~ancestors, typ, m);
      let use_mode: option(Operators.mode) =
        switch (typ.term |> Typ.weak_head_normalize(ctx) |> Typ.term_of) {
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
      let (body, body_elab, m) = go'(~ctx=ctx', ~ana, body, m);
      let self: Self.exp =
        switch (use_mode) {
        | Some(_) => Common(Just(body.ty))
        | None when Typ.fast_equal(Unknown(Internal) |> Typ.temp, typ.term) =>
          Common(Just(body.ty))
        | None =>
          InvalidUseMode({
            bad_typ: typ.term,
            inner_typ: body.ty,
          })
        };
      add'(~elab=body_elab, ~self, ~co_ctx=body.co_ctx, m);
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
      /* Override expansion info for Mod item IDs: replace Exp cls with Mod cls
         so cursor inspector shows "Let declaration" instead of "Let expression".
         We keep InfoExp (not InfoMod) because the elaborator needs InfoExp
         data (self, ty, etc.) for the expanded Let/TyAlias wrapper expressions. */
      let m =
        List.fold_left(
          (m, item: Mod.t) => {
            let ids = IdTagged.ids(item);
            let mod_cls = Cls.Mod(Mod.cls_of_term(item.term));
            switch (Id.Map.find_opt(IdTagged.rep_id(item), m)) {
            | Some(Info.InfoExp(info)) =>
              add_info(
                ids,
                Info.InfoExp({
                  ...info,
                  cls: mod_cls,
                }),
                m,
              )
            | _ => m
            };
          },
          m,
          items,
        );
      /* Build actual Prod type from module's exported bindings, rather than
         using expanded_info.ty which masks width errors via fixed_typ_exp. */
      let non_shadowed = ExpandModule.compute_non_shadowed_bindings(items);
      let actual_ty = {
        let fields =
          non_shadowed
          |> List.map(((name, pat)) => {
               let ty =
                 switch (Id.Map.find_opt(Pat.rep_id(pat), m)) {
                 | Some(Info.InfoPat({ty, ctx: pat_ctx, _})) =>
                   Typ.normalize(pat_ctx, ty)
                 | _ => Typ.temp(Unknown(Internal))
                 };
               TupLabel(Label(name) |> Typ.temp, ty) |> Typ.temp;
             });
        Prod(fields) |> Typ.temp;
      };
      let module_elab =
        expanded_elab
        |> ModuleHelpers.strip_module_sig_pats
        |> ModuleHelpers.restore_module_body_id(~id=Exp.rep_id(uexp));
      add(
        ~elab=module_elab,
        ~self=Just(actual_ty),
        ~co_ctx=expanded_info.co_ctx,
        m,
      );
    | ModuleExp(mp, def, body) =>
      /* Expand module M = def in body → let M = def in body.
         Process the MPat for cursor info, then expand to Let and type-check. */
      let (_, m) = any_to_info_map(~ctx, ~ancestors, MPat(mp), m);
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
      let moduleexp_elab = {
        let (expanded_term, expanded_rewrap) = Exp.unwrap(expanded_elab);
        switch (expanded_term) {
        | Let(p_elab, _, body_elab) =>
          Let(
            ModuleHelpers.strip_module_sig_pats_in_pat(p_elab),
            def_elab_direct,
            body_elab,
          )
          |> expanded_rewrap
        | _ => ModuleHelpers.strip_module_sig_pats(expanded_elab)
        };
      };
      add(
        ~elab=moduleexp_elab,
        ~self=Just(expanded_info.ty),
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
      ~duplicate_labels: list(string)=[],
      ~expected_labels=?,
      ~ana: Typ.t=Unknown(Internal) |> Typ.temp,
      ~under_ascription: bool=false,
      ~override_self: option(Self.t)=?,
      ~inferred_label=?,
      ~label_sort=false,
      {annotation: {ids, _}, term} as upat: Pat.t,
      m: Map.t,
    )
    : (Info.pat, Pat.t, Map.t) => {
  let add' =
      (
        ~self: Self.pat,
        ~ctx: Ctx.t,
        ~constraint_: Coverage.Constraint.t,
        ~label_inference: option(Info.label_inference(Info.pat))=?,
        ~elab: option(Pat.t)=?,
        m: Id.Map.t(Info.t),
      )
      : (Info.pat, Pat.t, Map.t) => {
    let elab = Option.value(~default=upat, elab);
    let prev_synswitch =
      switch (Id.Map.find_opt(Pat.rep_id(upat), m)) {
      | Some(Info.InfoPat({ana, ty, _})) when Typ.is_syn_plus(ana) =>
        Some(ty)
      | Some(Info.InfoPat({prev_synswitch, _})) => prev_synswitch
      | Some(_)
      | None => None
      };
    let info =
      Info.derived_pat(
        ~prev_synswitch,
        ~upat,
        ~ctx,
        ~co_ctx,
        ~ana,
        ~ancestors,
        ~self=
          Option.value(
            ~default=self,
            override_self |> Option.map((s): Self.pat => Common(s)),
          ),
        ~constraint_,
        ~label_inference,
        ~inferred_label,
        ~label_sort,
      );

    (info, elab, add_info(ids, InfoPat(info), m));
  };
  let add =
      (
        ~self: Self.t,
        ~ctx: Ctx.t,
        ~constraint_: Coverage.Constraint.t,
        ~label_inference: option(Info.label_inference(Info.pat))=?,
        ~elab: option(Pat.t)=?,
        m: Id.Map.t(Info.t),
      )
      : (Info.pat, Pat.t, Map.t) => {
    add'(
      ~self=Common(self),
      ~ctx,
      ~constraint_,
      ~label_inference?,
      ~elab?,
      m,
    );
  };
  let upat_to_info_map =
      (
        ~is_synswitch,
        ~ctx,
        ~co_ctx,
        ~ancestors,
        ~duplicate_bindings=[],
        ~duplicate_labels=[],
        ~expected_labels=?,
        ~ana,
        ~under_ascription=false,
        ~override_self=?,
        ~inferred_label=?,
        ~label_sort=false,
        upat: Pat.t,
        m: Map.t,
      ) => {
    upat_to_info_map(
      ~is_synswitch,
      ~ctx,
      ~co_ctx,
      ~ancestors,
      ~duplicate_bindings,
      ~duplicate_labels,
      ~ana,
      ~under_ascription,
      ~override_self?,
      ~inferred_label?,
      ~expected_labels?,
      ~label_sort,
      upat,
      m: Map.t,
    );
  };
  let atomic = (~elab=?, self, constraint_) =>
    add(~self, ~ctx, ~constraint_, ~elab?, m);
  let (_, rewrap) = Pat.unwrap(upat);
  let ancestors = [Pat.rep_id(upat)] @ ancestors;
  let go = (~under_ascription=false) =>
    upat_to_info_map(~under_ascription, ~is_synswitch, ~ancestors, ~co_ctx);
  let unknown = Unknown(is_synswitch ? SynSwitch : Internal) |> Typ.temp;
  let ctx_fold = (ctx: Ctx.t, m, ~duplicate_bindings=[], ~duplicate_labels=[]) =>
    List.fold_left2(
      ((ctx, tys, cons, m, info_all, elabs), e, ana) =>
        go(
          ~ctx,
          ~ana,
          ~duplicate_bindings,
          ~duplicate_labels,
          ~inferred_label?,
          e,
          m,
        )
        |> (
          ((info, elab, m)) => (
            info.ctx,
            tys @ [info.ty],
            cons @ [info.constraint_],
            m,
            info_all @ [info],
            elabs @ [elab],
          )
        ),
      (ctx, [], [], m, [], []),
    );

  let hole = self => atomic(self, Coverage.Constraint.Hole(None));

  let elaborate_singleton_tuple = (upat: Pat.t, inner_ty, l, m) => {
    let (term, rewrap) = Pat.unwrap(upat);
    let original_expression = Pat.fresh(term);
    let (original_info, _, m) =
      upat_to_info_map(
        ~ctx,
        ~co_ctx,
        ~is_synswitch,
        ~ancestors,
        ~ana=inner_ty,
        original_expression,
        m,
      );
    let elaborated_pat =
      rewrap(
        Tuple([
          TupLabel(Label(l) |> Pat.fresh, original_expression) |> Pat.fresh,
        ]),
      );
    let (info, _, m) =
      upat_to_info_map(
        ~ctx,
        ~co_ctx,
        ~is_synswitch,
        ~ancestors,
        ~ana,
        elaborated_pat,
        m,
      );

    // We need to keep the original status of the expression to get error messages on the unelaborated expression
    let info = {
      ...info,
      status: original_info.status,
      label_inference:
        Some(
          SingletonLabelInference({
            label: l,
            pre_labeled_info: original_info,
          }),
        ),
    };

    (
      info,
      elaborated_pat,
      add_info(IdTagged.ids(elaborated_pat), InfoPat(info), m),
    );
  };

  let default_case = () =>
    switch (term) {
    | MultiHole(tms) =>
      let (_, m) = multi(~ctx, ~ancestors, m, tms);
      add(
        ~self=IsMulti,
        ~ctx,
        ~constraint_=Coverage.Constraint.Hole(None),
        m,
      );
    | Invalid(token) => hole(BadToken(token))
    | EmptyHole => hole(Just(unknown))
    | Atom(c) =>
      let c =
        Operators.replace_literal(c, Typ.is_ana_atom(ana), ctx.use_mode); // Replace literal if necessary due to `use`
      switch (c) {
      | L(Nat(nat)) =>
        atomic(
          ~elab=Atom(Nat(nat)) |> rewrap,
          Just(Atom(Nat) |> Typ.temp),
          Coverage.Constraint.BigInt(nat),
        )
      | L(Int(int)) =>
        atomic(
          ~elab=Atom(Int(int)) |> rewrap,
          Just(Atom(Int) |> Typ.temp),
          Coverage.Constraint.BigInt(int),
        )
      | L(SInt(int)) =>
        atomic(
          ~elab=Atom(SInt(int)) |> rewrap,
          Just(Atom(SInt) |> Typ.temp),
          Coverage.Constraint.SInt(int),
        )
      | L(Float(float)) =>
        atomic(
          ~elab=Atom(Float(float)) |> rewrap,
          Just(Atom(Float) |> Typ.temp),
          Coverage.Constraint.Float(float),
        )
      | L(Bool(bool)) =>
        atomic(
          ~elab=Atom(Bool(bool)) |> rewrap,
          Just(Atom(Bool) |> Typ.temp),
          bool ? Coverage.Constraint.true_ : Coverage.Constraint.false_,
        )
      | L(String(string)) =>
        atomic(
          ~elab=Atom(String(string)) |> rewrap,
          Just(Atom(String) |> Typ.temp),
          Coverage.Constraint.String(string),
        )
      | R(BadInt(str)) =>
        add(
          ~elab=Invalid(str) |> rewrap,
          ~self=BadToken(str),
          ~ctx,
          ~constraint_=Coverage.Constraint.Hole(None),
          m,
        )
      };
    | ListLit(ps) =>
      let ids = List.map(Pat.rep_id, ps);
      let mode = Typ.matched_list(ctx, ana);
      let modes = List.init(List.length(ps), _ => mode);
      let (ctx, tys, cons, m, _, ps_elabs) = ctx_fold(ctx, m, ps, modes);
      let rec cons_fold_list = cs =>
        switch (cs) {
        | [] => Coverage.Constraint.nil
        | [hd, ...tl] => Coverage.Constraint.cons(hd, cons_fold_list(tl))
        };
      add(
        ~elab=ListLit(ps_elabs) |> rewrap,
        ~self=Self.listlit(~empty=unknown, ctx, tys, ids),
        ~ctx,
        ~constraint_=cons_fold_list(cons),
        m,
      );
    | Cons(hd, tl) =>
      let inner_ty = Typ.matched_list(ctx, ana);
      let (hd, hd_elab, m) = go(~ctx, ~ana=inner_ty, hd, m);
      let (tl, tl_elab, m) =
        go(~ctx=hd.ctx, ~ana=List(inner_ty) |> Typ.fresh, tl, m);
      add(
        ~elab=Cons(hd_elab, tl_elab) |> rewrap,
        ~self=Just(List(hd.ty) |> Typ.temp),
        ~ctx=tl.ctx,
        ~constraint_=Coverage.Constraint.cons(hd.constraint_, tl.constraint_),
        m,
      );
    | Wild => atomic(Just(unknown), Coverage.Constraint.Truth)
    | Var(name) =>
      /* NOTE: The self type assigned to pattern variables (Unknown)
         may be SynSwitch, but SynSwitch is never added to the context;
         Unknown(Internal) is used in this case */
      let ctx_typ =
        Info.fixed_typ_pat(
          ctx,
          ana,
          Common(Just(Unknown(Internal) |> Typ.temp)),
        );
      let entry =
        Ctx.VarEntry({
          name,
          id: Pat.rep_id(upat),
          typ: ctx_typ,
          custom_statics: None,
        });

      List.exists(l => name == l, duplicate_bindings)
        ? add(
            ~self=DuplicateVar(name, Just(unknown)),
            ~ctx=Ctx.extend(ctx, entry),
            ~constraint_=Coverage.Constraint.Truth,
            m,
          )
        : add(
            ~self=Just(unknown),
            ~ctx=Ctx.extend(ctx, entry),
            ~constraint_=Coverage.Constraint.Truth,
            m,
          );

    | TupLabel({term: ExplicitNonlabel, _} as label, p) =>
      let (p, p_elab, m) = go(~ana, ~ctx, p, m);
      let (_, _, m) = go(~label_sort=true, ~ctx, ~ana=syn, label, m);
      (p, p_elab, add_info(ids, InfoPat(p), m));
    | ExplicitNonlabel => atomic(ExplicitNonlabel, Coverage.Constraint.Truth)
    | TupLabel(label, p) =>
      let (lab, p, m) =
        switch (Typ.matched_label(ctx, ana)) {
        | Some((labmode, val_mode)) =>
          let label_self: option(Self.t) =
            switch (label.term) {
            | Label(_) => None
            | EmptyHole => None
            | _ => Some(BadLabel(Pat(label)))
            };

          let (lab, _, m) =
            go(
              ~ctx,
              ~ana=labmode,
              ~override_self=?label_self,
              ~duplicate_bindings,
              ~duplicate_labels,
              ~label_sort=true,
              label,
              m,
            );
          let (p, _, m) =
            go(
              ~ctx,
              ~ana=val_mode,
              ~inferred_label?,
              ~duplicate_bindings,
              p,
              m,
            );
          (lab, p, m);
        | _ =>
          let (lab, _, m) =
            go(
              ~ctx,
              ~ana=Unknown(Internal) |> Typ.temp,
              ~label_sort=true,
              ~override_self=?
                switch (label.term, expected_labels) {
                | (Label(name), Some(expected_labels))
                    when !List.mem(name, expected_labels) =>
                  Some(InvalidLabel(name, expected_labels))
                | (Label(_), _)
                | (EmptyHole, _) => None
                | _ => Some(BadLabel(Pat(label)))
                },
              ~duplicate_bindings,
              ~duplicate_labels,
              label,
              m,
            );

          let (p, _, m) =
            go(
              ~ctx,
              ~ana=Unknown(Internal) |> Typ.temp,
              ~inferred_label?,
              p,
              m,
            );
          (lab, p, m);
        };

      let self =
        switch (lab.status) {
        | NotInHole(_) => Self.Just(TupLabel(lab.ty, p.ty) |> Typ.temp)
        | InHole(
            Common(
              Inconsistent(Expectation({syn: {term: Label(name), _}, _})),
            ),
          )
        | InHole(Common(NoType(InvalidLabel(name, _)))) =>
          Self.TupleLabelError({
            malformed_labels: [],
            duplicate_labels: [],
            invalid_labels: [name],
            typ: TupLabel(Label(name) |> Typ.temp, p.ty) |> Typ.temp,
          })
        | InHole(Common(DuplicateLabel(name, _))) =>
          Self.TupleLabelError({
            malformed_labels: [],
            duplicate_labels: [name],
            invalid_labels: [],
            typ: TupLabel(Label(name) |> Typ.temp, p.ty) |> Typ.temp,
          })
        | InHole(_) =>
          Self.TupleLabelError({
            malformed_labels: [Pat(label)],
            duplicate_labels: [],
            invalid_labels: [],
            typ: TupLabel(Unknown(Internal) |> Typ.temp, p.ty) |> Typ.temp,
          })
        };
      add(
        ~self,
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
        Typ.matched_prod(
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
      let (ctx, tys, cons, m, info_pats, ps_elabs) =
        List.fold_left2(
          ((ctx, tys, cons, m, info_all, elabs), (inferred_label, e), ana) =>
            go(
              ~ctx,
              ~ana,
              ~inferred_label?,
              // Perhaps multiple copies of something in duplicates, but probably not an issue.
              // Needed so that nested tuples can have duplicate bindings saved.
              ~duplicate_bindings=duplicate_bindings @ new_duplicate_bindings,
              ~duplicate_labels=new_duplicate_labels,
              ~expected_labels?,
              e,
              m,
            )
            |> (
              ((info, elab, m)) => (
                info.ctx,
                tys @ [info.ty],
                cons @ [info.constraint_],
                m,
                info_all @ [info],
                elabs @ [elab],
              )
            ),
          (ctx, [], [], m, [], []),
          List.combine(inferred, ps),
          modes,
        );
      let constraint_ = Coverage.Constraint.Tuple(cons);
      let (malformed_labels, duplicate_labels, invalid_labels) =
        List.fold_left(
          ((a, b, c), e: Info.pat) => {
            switch (e.term.term, e.status) {
            | (
                TupLabel(_, _),
                InHole(
                  Common(
                    TupleLabelError({
                      malformed_labels,
                      duplicate_labels,
                      invalid_labels,
                      _,
                    }),
                  ),
                ),
              ) => (
                a @ malformed_labels,
                b @ duplicate_labels,
                c @ invalid_labels,
              )
            | _ => (a, b, c)
            }
          },
          ([], [], []),
          info_pats,
        );

      let ty_list = Typ.remove_duplicate_labels(~duplicate_labels, tys);

      let self =
        List.is_empty(malformed_labels)
        && List.is_empty(duplicate_labels)
        && List.is_empty(invalid_labels)
          ? Self.Just(Prod(ty_list) |> Typ.temp)
          : Self.TupleLabelError({
              malformed_labels,
              duplicate_labels,
              invalid_labels,
              typ: Prod(ty_list) |> Typ.temp,
            });
      add(
        ~self,
        ~ctx,
        ~constraint_,
        ~label_inference=
          Info.derive_label_inference_info(original_labels, new_labels),
        ~elab=Tuple(ps_elabs) |> rewrap,
        m,
      );
    | Label(name) =>
      let self = Self.Just(Label(name) |> Typ.temp);
      List.exists(l => name == l, duplicate_labels)
        ? atomic(DuplicateLabel(name, self), Coverage.Constraint.Truth)
        : atomic(self, Coverage.Constraint.Truth);
    | Parens(p) =>
      let (p, p_elab, m) =
        go(~ctx, ~ana, p, ~duplicate_bindings, ~duplicate_labels, m);
      add'(
        ~elab=Parens(p_elab) |> rewrap,
        ~self=p.self,
        ~ctx=p.ctx,
        ~constraint_=p.constraint_,
        m,
      );
    | Projector(data, p) =>
      let (p, p_elab, m) =
        go(~ctx, ~ana, p, ~duplicate_bindings, ~duplicate_labels, m);
      add'(
        ~elab=Projector(data, p_elab) |> rewrap,
        ~self=p.self,
        ~ctx=p.ctx,
        ~constraint_=p.constraint_,
        m,
      );
    | Constructor(ctr, ty) =>
      let self = Self.of_ctr(ctx, ctr, ana, ty);
      let elab_ty =
        switch (Self.ctr_ana_typ(ctx, ana, ctr), Ctx.lookup_ctr(ctx, ctr)) {
        | (Some(ana_ty), _) => Some(Typ.normalize(ctx, ana_ty))
        | (_, Some({typ: syn_ty, _})) => Some(Typ.normalize(ctx, syn_ty))
        | _ => None
        };
      atomic(
        ~elab=Constructor(ctr, Some(elab_ty)) |> rewrap,
        self,
        Coverage.Constraint.Ap(ctr, None),
      );
    | Ap(fn, arg) =>
      let ctr = Pat.ctr_name(fn);
      let fn_ana = Arrow(syn, ana) |> Typ.temp;
      let (fn', fn_elab, m) = go(~ctx, ~ana=fn_ana, fn, m);
      let m = {
        switch (ctr) {
        | Some(_) => m
        | _ =>
          let info =
            Info.derived_pat(
              ~upat=fn'.term,
              ~ctx=fn'.ctx,
              ~co_ctx=fn'.co_ctx,
              ~prev_synswitch=fn'.prev_synswitch,
              ~ana=fn'.ana,
              ~ancestors=fn'.ancestors,
              ~self=Self.ExpectedConstructor(fn'.self),
              ~constraint_=fn'.constraint_,
              ~label_inference=fn'.label_inference,
              ~inferred_label=fn'.inferred_label,
              ~label_sort=fn'.label_sort,
            );
          add_info(IdTagged.ids(fn), InfoPat(info), m);
        };
      };
      let (ty_in, ty_out) = Typ.matched_arrow(ctx, fn'.ty);
      let (arg, arg_elab, m) = go(~ctx, ~ana=ty_in, arg, m);
      let constraint_ =
        switch (ctr) {
        | Some(ctr) => Coverage.Constraint.Ap(ctr, Some(arg.constraint_))
        | None => Coverage.Constraint.Hole(None)
        };
      add(
        ~elab=Ap(fn_elab, arg_elab) |> rewrap,
        ~self=Just(ty_out),
        ~ctx=arg.ctx,
        ~constraint_,
        m,
      );
    | Asc(p, ann) =>
      let (ann, m) = utyp_to_info_map(~ctx, ~ancestors, ann, m);
      /* Desugar any Sig types in the annotation without full normalization */
      let ann_ty = Typ.desugar_sig(ctx, ann.term);
      let (p, p_elab, m) =
        go(~ctx, ~under_ascription=true, ~ana=ann_ty, p, m);
      add(
        ~elab=Asc(p_elab, Typ.normalize(ctx, ann.term)) |> rewrap,
        ~self=Just(ann_ty),
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
      ~expects=Info.TypeExpected,
      ~ancestors,
      {annotation: {ids, _}, term} as utyp: Typ.t,
      m: Map.t,
    )
    : (Info.typ, Map.t) => {
  let add' = (~expects=expects, ~utyp=utyp, m) => {
    let info = Info.derived_typ(~utyp, ~ctx, ~ancestors, ~expects);
    (info, add_info(ids, InfoTyp(info), m));
  };
  let add = (~utyp=utyp, m) => add'(~utyp, m);
  let ancestors = [Typ.rep_id(utyp)] @ ancestors;
  let go' = utyp_to_info_map(~ctx, ~ancestors);
  let go = go'(~expects=TypeExpected);
  switch (term) {
  | Unknown(Hole(MultiHole(tms))) =>
    let (_, m) = multi(~ctx, ~ancestors, m, tms);
    add(m);
  | Unknown(_)
  | Atom(_) => add(m)
  | Var(_) =>
    /* Names are resolved in Info.status_typ */
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
              go'(
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
    let info = Info.derived_typ(~utyp, ~ctx, ~ancestors, ~expects);
    (info, add_info(ids, InfoTyp(info), m));
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
    let m = go'(~expects=LabelProjectionExpected(labels), label, m) |> snd;
    let m = go'(~expects=ProductExpected, t, m) |> snd;
    add'(~expects=TypeExpected, m);
  | ProdExtension(t1, t2) =>
    let m = go'(~expects=ProductExpected, t1, m) |> snd;
    let m = go'(~expects=ProductExpected, t2, m) |> snd;
    add(m);
  | ExplicitNonlabel =>
    let ancestors = List.tl(ancestors); // Recover original ancestors

    let info: Info.typ = {
      cls: Typ(ExplicitNonlabel),
      ctx,
      ancestors,
      status: InHole(BadToken("_")),
      expects,
      term: utyp,
      warning: None,
    };
    (info, add_info(ids, InfoTyp(info), m));
  | TupLabel({term: ExplicitNonlabel, _} as label, t) =>
    let (_, m) = go(t, m);

    let label_info: Info.typ = {
      cls: Typ(ExplicitNonlabel),
      ctx,
      ancestors,
      status: NotInHole(EmptyLabel),
      expects,
      term: utyp,
      warning: None,
    };

    let m = add_info(label.annotation.ids, InfoTyp(label_info), m);
    add'(~expects=TypeExpected, ~utyp=t, m);
  | TupLabel(label, t) =>
    let expects_label =
      switch (expects) {
      | LabelExpected(_) => expects
      | _ => LabelExpected(Unique, [])
      };
    let m = go'(~expects=expects_label, label, m) |> snd;
    let m = go(t, m) |> snd;
    add'(~expects=TypeExpected, m);
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
      uexp_to_info_map(
        ~ctx,
        ~ancestors,
        ~ana=Atom(Bool) |> Typ.temp,
        ~duplicates=[],
        ~expected_labels=None,
        ~label_sort=false,
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
          let (_, m) = any_to_info_map(~ctx, ~ancestors, Sig(item), m);
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
      ~ctx,
      ~ancestors,
      {annotation: {ids, _}, term} as utpat: TPat.t,
      m: Map.t,
    )
    : (Info.tpat, Map.t) => {
  let add = m => {
    let info = Info.derived_tpat(~utpat, ~ctx, ~ancestors);
    (info, add_info(ids, InfoTPat(info), m));
  };
  let ancestors = [TPat.rep_id(utpat)] @ ancestors;
  switch (term) {
  | MultiHole(tms) =>
    let (_, m) = multi(~ctx, ~ancestors, m, tms);
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
};

let mk =
  Core.Memo.general(
    ~cache_size_bound=1000,
    (ana, ctx, e) => {
      let (_, elab, m) =
        uexp_to_info_map(
          ~ana,
          ~ctx,
          ~ancestors=[],
          ~duplicates=[],
          ~expected_labels=None,
          ~label_sort=false,
          e,
          Id.Map.empty,
        );
      (m, elab);
    },
  );

let mk = (~ana=Typ.temp(Unknown(SynSwitch)), core: CoreSettings.t, ctx, exp) =>
  core.statics ? mk(ana, ctx, exp) : (Id.Map.empty, Exp.fresh(Tuple([])));
