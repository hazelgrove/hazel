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
        (~ctx: Ctx.t, ~ancestors, any: Any.t, m: Map.t)
        : (CoCtx.t, list(Typ.equivalence), Map.t) =>
  switch (any) {
  | Exp(e) =>
    let ({co_ctx, constraints, _}: Info.exp, m) =
      uexp_to_info_map(
        ~ctx,
        ~ancestors,
        ~duplicates=[],
        ~expected_labels=None,
        ~label_sort=false,
        e,
        m,
      );
    (co_ctx, constraints, m);
  | Pat(p) =>
    let (info, m) =
      upat_to_info_map(
        ~is_synswitch=false,
        ~co_ctx=CoCtx.empty,
        ~ancestors,
        ~duplicates=[],
        ~ctx,
        p,
        m,
      );
    (CoCtx.empty, info.typ_constraints, m);
  | TPat(tp) => (
      CoCtx.empty,
      [],
      utpat_to_info_map(~ctx, ~ancestors, tp, m) |> snd,
    )
  | Typ(ty) => (
      CoCtx.empty,
      [],
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
      let (co_ctxs, constraints, m) = multi(~ctx, ~ancestors, m, tms);
      (CoCtx.union(co_ctxs), constraints, m);
    | Invalid(_) => (CoCtx.empty, [], m)
    }
  | Any () => (CoCtx.empty, [], m)
  }
/*
 If a type is's type is consistent with the type it is being analyzed against,
 produces a constraint that the ana type and self type are consistent.
 */
and subsumption_constraints_t =
    (ana, ctx: Ctx.t, self: Self.t): list(Typ.equivalence) => {
  switch (Self.typ_of(self)) {
  | Some(typ) when Typ.is_consistent(ctx, typ, ana) => [Con(ana, typ)]
  | _ => []
  };
}
and subsumption_constraints_exp =
    (ana, ctx: Ctx.t, self: Self.exp): list(Typ.equivalence) => {
  switch (Self.typ_of_exp(self)) {
  | Some(typ) when Typ.is_consistent(ctx, typ, ana) => [Con(ana, typ)]
  | _ => []
  };
}
and multi =
    (~ctx, ~ancestors, m, tms)
    : (list(CoCtx.t), list(Typ.equivalence), Map.t) =>
  List.fold_left(
    ((co_ctxs, acc_constraints, m), any) => {
      let (co_ctx, constraints, m) =
        any_to_info_map(~ctx, ~ancestors, any, m);
      (co_ctxs @ [co_ctx], acc_constraints @ constraints, m);
    },
    ([], [], m),
    tms,
  )
// TODO: (THI) optimize
and constrain_branches = (branch_tys: list(Typ.t)): list(Typ.equivalence) => {
  let rec constrain_branches' =
          (branch_tys': list(Typ.t), constraints: list(Typ.equivalence)) => {
    switch (branch_tys') {
    | [] => constraints
    | [hd, ...tl] =>
      constrain_branches'(
        tl,
        constraints @ List.map(ty => Typ.Con(hd, ty), tl),
      )
    };
  };
  constrain_branches'(branch_tys, []);
}
and uexp_to_info_map =
    (
      ~ctx: Ctx.t,
      ~ana=anon_syn,
      ~is_in_filter=false,
      ~ancestors,
      ~duplicates: list(string),
      ~expected_labels: option(list(string)),
      ~override_self: option(Self.exp)=?,
      ~inferred_label: option(LabeledTuple.label)=?,
      ~label_sort,
      {annotation: {ids}, term} as uexp: Exp.t,
      m: Map.t,
    )
    : (Info.exp, Map.t) => {
  let add' =
      (
        ~label_inference: option(Info.label_inference(Info.exp))=?,
        ~self: Self.exp,
        ~co_ctx: CoCtx.t,
        ~constraints,
        m: Map.t,
      )
      : (Info.exp, Map.t) => {
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
        ~constraints,
      );

    (info, add_info(ids, InfoExp(info), m));
  };
  let add = (~self, ~co_ctx, m) => {
    add'(~self=Common(self), ~co_ctx, m);
  };
  let ancestors = [Exp.rep_id(uexp)] @ ancestors;
  let uexp_to_info_map =
      (
        ~ctx,
        ~ana=mk_temp_syn(),
        ~is_in_filter=is_in_filter,
        ~ancestors=ancestors,
        ~duplicates=[],
        ~expected_labels=?,
        ~inferred_label: option(string)=?,
        ~override_self=?,
        ~label_sort=false,
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
        ~label_sort=original_info.label_sort,
        ~constraints=original_info.constraints,
      );
    (
      new_info,
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
      TermBase.exp_t,
      Map.t
    ) =>
    (Info.exp, Map.t) =
    go'(~ctx);
  let map_m_go = (m, ~duplicates=[]) =>
    List.fold_left2(
      ((es, m), ana, e) =>
        go(~ana, ~duplicates, e, m) |> (((e, m)) => (es @ [e], m)),
      ([], m),
    );
  let go_pat = upat_to_info_map(~ctx, ~ancestors, ~duplicates);
  let go_typ = utyp_to_info_map(~ctx, ~ancestors);

  let subsumption_constraints_t = subsumption_constraints_t(ana, ctx);
  let subsumption_constraints_exp = subsumption_constraints_exp(ana, ctx);

  let label_to_info_map =
      (expected_labels, labmode, label: Exp.t, m: Map.t)
      : (option(string), Info.exp, Map.t) => {
    switch (label.term, expected_labels) {
    | (Label(name), Some(expected_labels))
        when !List.mem(name, expected_labels) =>
      let (i, m) =
        go(
          ~ana=labmode,
          ~override_self=Common(InvalidLabel(name, expected_labels)),
          ~label_sort=true,
          ~duplicates,
          label,
          m,
        );
      (None, i, m);
    | (Label(lab), _) =>
      let (i, m) = go(~ana=labmode, ~label_sort=true, ~duplicates, label, m);
      (Some(lab), i, m);
    | (EmptyHole, _) =>
      let (i, m) = go(~ana=labmode, ~label_sort=true, ~duplicates, label, m);
      (None, i, m);
    | _ =>
      let (i, m) =
        go(
          ~ana=labmode,
          ~override_self=Common(BadLabel(Exp(label))),
          ~label_sort=true,
          ~duplicates,
          label,
          m,
        );
      (None, i, m);
    };
  };
  // This lifts an expression into a singleton labeled tuple by rewriting the syntax in the Statics Map
  let autolabel_singleton_tuple = (uexp: Exp.t, inner_ty, l, m) => {
    let (term, rewrap) = Exp.unwrap(uexp);
    let original_expression = Exp.fresh(term);
    let (original_info, m) =
      uexp_to_info_map(
        ~ctx,
        ~ana=inner_ty,
        ~is_in_filter,
        ~ancestors,
        original_expression,
        m,
      );

    /* Special case for probes, which would otherwise lose their id association here */
    let elaborated_exp =
      switch (term) {
      | Probe(_, p) =>
        rewrap(
          Probe(
            Tuple([
              TupLabel(Label(l) |> Exp.fresh, original_expression)
              |> Exp.fresh,
            ])
            |> Exp.fresh,
            p,
          ),
        )
      | _ =>
        rewrap(
          Tuple([
            TupLabel(Label(l) |> Exp.fresh, original_expression) |> Exp.fresh,
          ]),
        )
      };

    // We need to reanalyze the elaborated expression to get the statics in the map for the label and tuple
    let (info, m) =
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

    (info, add_info(IdTagged.ids(elaborated_exp), InfoExp(info), m));
  };
  let atomic = self => {
    add(
      ~self,
      ~co_ctx=CoCtx.empty,
      ~constraints=subsumption_constraints_t(self),
      m,
    );
  };

  // This is the case where we aren't a singleton labeled tuple
  let default_case = (): (Info.exp, Map.t) => {
    switch (term) {
    | Closure(_, e) =>
      // TODO: implement closure type checking properly - see how dynamic type assignment does it
      let (e, m) = go(~ana, e, m);
      add(~self=Just(e.ty), ~co_ctx=e.co_ctx, ~constraints=e.constraints, m);
    | MultiHole(tms) =>
      let (co_ctxs, constraints, m) = multi(~ctx, ~ancestors, m, tms);
      add(
        ~self=IsMulti,
        ~co_ctx=CoCtx.union(co_ctxs),
        ~constraints=constraints @ subsumption_constraints_t(IsMulti),
        m,
      );
    | Asc(e, t2) =>
      // TODO: (THI) do acriptions need to have another constraint generated?
      let (t, m) = go_typ(t2, ~expects=Info.TypeExpected, m);
      let (e, m) = go'(~ana=t.term, ~ctx=t.ctx, e, m);
      let self: Self.t = Just(t.term);
      add(
        ~self,
        ~co_ctx=e.co_ctx,
        ~constraints=e.constraints @ subsumption_constraints_t(self),
        m,
      );
    | Invalid(token) => atomic(BadToken(token))
    | EmptyHole =>
      atomic(
        Just(
          Unknown({
            term: Internal,
            annotation: uexp.annotation,
          })
          |> Typ.temp,
        ),
      )
    | Deferral(position) =>
      // TODO: (THI) do we need to generate constraints for deferrals?
      add'(
        ~self=IsDeferral(position),
        ~co_ctx=CoCtx.empty,
        ~constraints=[],
        m,
      )
    | Undefined =>
      atomic(
        Just(
          Unknown({
            term: Hole(EmptyHole),
            annotation: uexp.annotation,
          })
          |> Typ.temp,
        ),
      )
    | Atom(c) =>
      let c =
        Operators.replace_literal(c, Typ.is_ana_atom(ana), ctx.use_mode); // Replace literal if necessary due to `use`
      switch (c) {
      | L(c) =>
        let ty = Atom(Atom.cls_of_t(c)) |> Typ.temp;
        atomic(Just(ty));
      | R(BadInt(str)) => atomic(BadToken(str))
      };
    | LivelitName(name) =>
      // TOOD: (THI) do we need to generate constraints?
      let constraints =
        switch (Ctx.lookup_livelit(ctx, name)) {
        | None =>
          subsumption_constraints_t(
            Just(
              Unknown({
                term: Internal,
                annotation: uexp.annotation,
              })
              |> Typ.temp,
            ),
          )
        | Some(livelit) => subsumption_constraints_t(Just(livelit.model_t))
        };
      add'(
        ~self=Self.of_exp_livelit_name(ctx, name),
        ~co_ctx=CoCtx.singleton(name, Exp.rep_id(uexp), ana),
        ~constraints,
        m,
      );
    | ListLit(es) =>
      let ids = List.map(Exp.rep_id, es);
      let (inner_ana_ty, list_constraints) = Typ.matched_list(ctx, ana);
      let anas = List.init(List.length(es), _ => inner_ana_ty);
      let (es, m) = map_m_go(m, anas, es);
      let tys = List.map(Info.exp_ty, es);
      let es_constraints = List.flatten(List.map(Info.exp_constraints, es));
      let (self, self_cons) =
        Self.listlit(
          ~empty=Unknown(Internal |> Prov.fresh) |> Typ.temp,
          ctx,
          tys,
          ids,
        );
      let (subsum_self, _) =
        Self.listlit(
          ~empty=
            Unknown((Internal: TermBase.type_provenance) |> IdTagged.temp)
            |> Typ.temp,
          ctx,
          tys,
          ids,
        );
      add(
        ~self,
        ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
        ~constraints=
          es_constraints
          @ list_constraints
          @ self_cons
          @ subsumption_constraints_t(subsum_self),
        m,
      );
    | Cons(hd, tl) =>
      let (inner_ana_ty, list_constraints) = Typ.matched_list(ctx, ana);
      let (hd, m) = go(~ana=inner_ana_ty, hd, m);
      let (tl, m) =
        go(
          ~ana=
            List(Typ.is_syn(inner_ana_ty) ? hd.ty : inner_ana_ty) |> Typ.temp,
          tl,
          m,
        );
      add(
        ~self=Just(List(hd.ty) |> Typ.temp),
        ~co_ctx=CoCtx.union([hd.co_ctx, tl.co_ctx]),
        ~constraints=hd.constraints @ tl.constraints @ list_constraints,
        m,
      );
    | ListConcat(e1, e2) =>
      // TODO: (THI) do we need the matched constraint?
      let inner_ana_ty = List(Typ.matched_list(ctx, ana) |> fst) |> Typ.temp;
      let ids = List.map(Exp.rep_id, [e1, e2]);
      let (e1, m) = go(~ana=inner_ana_ty, e1, m);
      let (e2, m) = go(~ana=inner_ana_ty, e2, m);
      let (self, self_cons) = Self.list_concat(ctx, [e1.ty, e2.ty], ids);
      add(
        ~self,
        ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
        ~constraints=e1.constraints @ e2.constraints @ self_cons,
        m,
      );
    | Var(name) =>
      // if the variable exists, constraint it to its type. Otherwise,
      // constrain it to an expression hole.
      let cons =
        switch (Ctx.lookup_var(ctx, name)) {
        | Some(var) => subsumption_constraints_t(Just(var.typ))
        | None =>
          subsumption_constraints_t(
            Just(
              Unknown({
                term: Internal,
                annotation: uexp.annotation,
              })
              |> Typ.temp,
            ),
          )
        };
      let self = Self.of_exp_var(ctx, name);
      add'(
        ~self,
        ~co_ctx=CoCtx.singleton(name, Exp.rep_id(uexp), ana),
        ~constraints=cons,
        m,
      );
    | DynamicErrorHole(e, _)
    | Parens(e)
    | Probe(e, _) =>
      let (e, m) = go(~ana, e, m);
      add'(~self=e.self, ~co_ctx=e.co_ctx, ~constraints=e.constraints, m);
    | UnOp(Meta(Unquote), e) when is_in_filter =>
      let e: Exp.t = {
        annotation: {
          ids: IdTagged.ids(e),
        },
        term:
          switch (e.term) {
          | Var("e") => Constructor("$e", Some(Some(mk_fresh_internal())))
          | Var("v") => Constructor("$v", Some(Some(mk_fresh_internal())))
          | _ => e.term
          },
      };
      let ty_in = Var("$Meta") |> Typ.temp;
      let ty_out = mk_fresh_internal();
      let (e, m) = go(~ana=ty_in, e, m);
      add(
        ~self=Just(ty_out),
        ~co_ctx=e.co_ctx,
        ~constraints=e.constraints,
        m,
      );
    | UnOp(Meta(Unquote), e) =>
      let (e, m) = go(~ana=anon_syn, e, m);
      add'(
        ~self=BadOperator("Unquote not in filter"),
        ~co_ctx=e.co_ctx,
        ~constraints=e.constraints,
        m,
      );
    | UnOp(op, e) =>
      let op = Operators.replace_un_op(op, ctx.use_mode); // Replace op if necessary due to `use`
      let op_semantics = Operators.semantics_of_un_op(op);
      switch (op_semantics) {
      | Undefined(msg) =>
        let (_, m) = go(~ana=anon_syn, e, m);
        add'(
          ~self=BadOperator(msg),
          ~co_ctx=CoCtx.empty,
          ~constraints=[],
          m,
        );
      | Defined(ty_in, ty_out, _) =>
        let ty_in = Atom(Atom.cls_of_kind(ty_in)) |> Typ.temp;
        let ty_out = Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp;
        let (e, m) = go(~ana=ty_in, e, m);
        add(
          ~self=Just(ty_out),
          ~co_ctx=e.co_ctx,
          ~constraints=
            e.constraints @ subsumption_constraints_t(Just(ty_out)),
          m,
        );
      };
    | BinOp(op, e1, e2) =>
      let op = Operators.replace_bin_op(op, ctx.use_mode); // Replace op if necessary due to `use`
      let op_semantics = Operators.semantics_of_bin_op(op);
      switch (op_semantics) {
      | Undefined(msg) =>
        let (_, m) = go(~ana=anon_syn, e1, m);
        let (_, m) = go(~ana=anon_syn, e2, m);
        add'(
          ~self=BadOperator(msg),
          ~co_ctx=CoCtx.empty,
          ~constraints=[],
          m,
        );
      | DefinedPoly(_) =>
        let ids = List.map(Exp.rep_id, [e1, e2]);
        let (es, m) =
          map_m_go(m, [mk_temp_internal(), mk_temp_internal()], [e1, e2]);
        let tys = List.map(Info.exp_ty, es);
        let (self, self_cons) = Self.poly_eq(ctx, tys, ids);

        add(
          ~self,
          ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
          ~constraints=
            List.flatten(List.map(Info.exp_constraints, es)) @ self_cons,
          m,
        );
      | Defined(ty1, ty2, ty_out, _) =>
        let ty1 = Atom(Atom.cls_of_kind(ty1)) |> Typ.temp;
        let ty2 = Atom(Atom.cls_of_kind(ty2)) |> Typ.temp;
        let ty_out = Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp;
        let (e1, m) = go(~ana=ty1, e1, m);
        let (e2, m) = go(~ana=ty2, e2, m);
        let self: Self.t = Just(ty_out);

        add(
          ~self,
          ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
          ~constraints=
            e1.constraints @ e2.constraints @ subsumption_constraints_t(self),
          m,
        );
      };
    | TupleExtension(e1, e2) =>
      let (t1, m) = {
        let (t1, m) = go(e1, m);
        switch (Typ.normalize(ctx, t1.ty).term) {
        | Prod(_)
        | Unknown(_) => (t1, m)
        | _ => replace_self(m, t1, TupleExtensionRequiresTuples)
        };
      };
      let (t2, m) = {
        let (t2, m) = go(e2, m);
        switch (Typ.normalize(ctx, t2.ty).term) {
        | Prod(_)
        | Unknown(_) => (t2, m)
        | _ => replace_self(m, t2, TupleExtensionRequiresTuples)
        };
      };

      // TODO: (THI) is this sufficient for constraints?
      let add = add(~constraints=t1.constraints @ t2.constraints);

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
          ~self=Just(ty), // TODO: fix this
          ~co_ctx=CoCtx.empty,
          m,
        );
      | (Unknown(_), _)
      | (_, Unknown(_)) =>
        add(
          ~self=
            Just(IdTagged.FreshGrammar.Typ.unknown(Internal |> Prov.fresh)),
          ~co_ctx=CoCtx.empty,
          m,
        )
      | _ =>
        add(
          ~self=
            Just(IdTagged.FreshGrammar.Typ.unknown(Internal |> Prov.fresh)),
          ~co_ctx=CoCtx.empty,
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

      let (inferred_es, ana_tys, constraints) =
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

      let (es', m) =
        List.fold_left2(
          ((es, m), ana, (inferred_label, e)) => {
            go(
              ~ana,
              ~inferred_label?,
              ~duplicates=duplicate_labels,
              ~expected_labels?,
              e,
              m,
            )
            |> (((e, m)) => (es @ [e], m))
          },
          ([], m),
          ana_tys,
          List.combine(inferred, es),
        );
      let ty_list = List.map(Info.exp_ty, es');
      let es_constraints =
        List.map((e: Info.exp) => e.constraints, es') |> List.flatten;

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

      add'(
        ~self=Common(self),
        ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es')),
        ~label_inference=
          Info.derive_label_inference_info(original_labels, new_labels),
        ~constraints=constraints @ es_constraints,
        m,
      );
    | TupLabel({term: ExplicitNonlabel, _} as label, e) =>
      let (e, m) = go(~ana, e, m);
      let (_, m) = go(~label_sort=true, label, m);
      add(~self=Just(e.ty), ~constraints=e.constraints, ~co_ctx=e.co_ctx, m);
    | TupLabel(label, e) =>
      let (lab, e, matched_label_constraints, m) =
        switch (Typ.matched_label(ctx, ana)) {
        | Some((labmode, val_mode, matched_label_constraints)) =>
          let (_, lab, m) =
            label_to_info_map(expected_labels, labmode, label, m);

          let (e, m) = go(~ana=val_mode, ~inferred_label?, e, m);
          (lab, e, matched_label_constraints, m);
        | _ =>
          let (_, lab, m) =
            label_to_info_map(expected_labels, mk_temp_syn(), label, m);

          let (e, m) = go(~ana=mk_temp_internal(), ~inferred_label?, e, m);
          (lab, e, [], m);
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
            typ: TupLabel(mk_temp_internal(), e.ty) |> Typ.temp,
          })
        };
      add(
        ~self,
        ~co_ctx=CoCtx.union([lab.co_ctx, e.co_ctx]),
        ~constraints=
          e.constraints
          @ lab.constraints
          @ matched_label_constraints
          @ subsumption_constraints_t(self),
        m,
      );
    | ExplicitNonlabel => atomic(ExplicitNonlabel)
    | Label(name) when label_sort =>
      let self = Self.Just(Label(name) |> Typ.temp);
      List.exists(l => name == l, duplicates)
        ? atomic(Duplicate(name, self)) : atomic(self);
    | Label(name) =>
      let self = Self.UnexpectedLabelSort(name);
      atomic(self);
    | BuiltinFun(string) =>
      add'(
        ~self=Self.of_exp_var(Builtins.ctx_init(None), string),
        ~co_ctx=CoCtx.empty,
        ~constraints=[],
        m,
      )

    | Dot(e1, e2) =>
      let (info_e1, m) = go(~ana=mk_temp_syn(), e1, m);
      let (info_e2, m) =
        go(~label_sort=true, ~ana=Label("") |> Typ.temp, e2, m);

      let (ty, m) = {
        switch (info_e1.ty.term, info_e2.ty.term) {
        | (Unknown(_), Label(name)) =>
          // This is so that the statics will result in Unknown(Internal)
          let ty =
            Prod([
              TupLabel(Label(name) |> Typ.temp, mk_temp_internal())
              |> Typ.temp,
            ])
            |> Typ.temp;
          let (_, m) = go(~ana=ty, e1, m);
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
            let self: Self.t = Just(typ);
            add(
              ~self,
              ~co_ctx=info_e2.co_ctx,
              ~constraints=
                info_e1.constraints
                @ info_e2.constraints
                @ subsumption_constraints_t(self),
              m,
            );
          | None =>
            add'(
              ~self=LabelNotFound(name, labels),
              ~co_ctx=info_e2.co_ctx,
              ~constraints=info_e1.constraints @ info_e2.constraints,
              m,
            )
          };
        | EmptyHole =>
          let self: Self.t = Just(mk_temp_internal());
          add(
            ~self,
            ~co_ctx=info_e2.co_ctx,
            ~constraints=
              info_e1.constraints
              @ info_e2.constraints
              @ subsumption_constraints_t(self),
            m,
          );
        | _ =>
          add(
            ~self=BadLabel(Exp(e2)),
            ~co_ctx=info_e2.co_ctx,
            ~constraints=info_e1.constraints @ info_e2.constraints,
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
              ~self=Just(List(typ) |> Typ.fresh),
              ~co_ctx=info_e2.co_ctx,
              ~constraints=info_e1.constraints @ info_e2.constraints,
              m,
            )
          | None =>
            add'(
              ~self=LabelNotFound(name, labels),
              ~co_ctx=info_e2.co_ctx,
              ~constraints=info_e1.constraints @ info_e2.constraints,
              m,
            )
          };
        | EmptyHole =>
          add(
            ~self=Just(Unknown(Internal |> Prov.fresh) |> Typ.temp),
            ~co_ctx=info_e2.co_ctx,
            ~constraints=info_e1.constraints @ info_e2.constraints,
            m,
          )
        | _ =>
          add(
            ~self=BadLabel(Exp(e2)),
            ~co_ctx=info_e2.co_ctx,
            ~constraints=info_e1.constraints @ info_e2.constraints,
            m,
          )
        };
      | List({term: Unknown(_), _}) =>
        add(
          ~self=Just(List(mk_temp_internal()) |> Typ.temp),
          ~co_ctx=info_e2.co_ctx,
          ~constraints=info_e1.constraints @ info_e2.constraints,
          m,
        )
      | _ =>
        add'(
          ~self=DotOperatorRequiresTuple,
          ~co_ctx=info_e2.co_ctx,
          ~constraints=info_e1.constraints @ info_e2.constraints,
          m,
        )
      };
    | Test(e) =>
      let (e, m) = go(~ana=Atom(Bool) |> Typ.temp, e, m);
      add(
        ~self=Just(Prod([]) |> Typ.temp),
        ~co_ctx=e.co_ctx,
        ~constraints=e.constraints,
        m,
      );
    | HintedTest(e, hint) =>
      let (e, m) = go(~ana=Atom(Bool) |> Typ.temp, e, m);
      let (hint, m) = go(~ana=Atom(String) |> Typ.temp, hint, m);
      add(
        ~self=Just(Prod([]) |> Typ.temp),
        ~co_ctx=CoCtx.union([e.co_ctx, hint.co_ctx]),
        ~constraints=e.constraints @ hint.constraints, // TODO: (THI) are hint constraints really necessary?
        m,
      );
    | Filter(Filter({pat: cond, _}), body) =>
      let (cond, m) = go(~ana=mk_temp_syn(), cond, m, ~is_in_filter=true);
      let (body, m) = go(~ana, body, m);
      add(
        ~self=Just(body.ty),
        ~co_ctx=CoCtx.union([cond.co_ctx, body.co_ctx]),
        ~constraints=cond.constraints @ body.constraints,
        m,
      );
    | Filter(Residue(_), body) =>
      let (body, m) = go(~ana, body, m);
      add(
        ~self=Just(body.ty),
        ~co_ctx=CoCtx.union([body.co_ctx]),
        ~constraints=body.constraints,
        m,
      );
    | Seq(e1, e2) =>
      let (e1, m) = go(~ana=mk_temp_syn(), e1, m);
      let (e2, m) = go(~ana, e2, m);
      add(
        ~self=Just(e2.ty),
        ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
        ~constraints=e1.constraints @ e2.constraints,
        m,
      );
    | Constructor(ctr, ty) => atomic(Self.of_ctr(ctx, ctr, ana, ty))
    | Ap(_, fn, arg) =>
      switch (fn.term) {
      // TODO: (THI) subsumption constraints necessary in livelit?
      | LivelitName(s) =>
        // refer to livelit context to find types
        switch (Ctx.lookup_livelit(ctx, s)) {
        | Some({expansion_t, model_t, expand, _}) =>
          let (fn, m) = go(~ana=expansion_t, fn, m);
          let (arg, m) = go(~ana=model_t, arg, m);

          // try to expand
          switch (expand(arg.term)) {
          | Some(_) =>
            add(
              ~self=Just(expansion_t),
              ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]),
              ~constraints=fn.constraints @ arg.constraints,
              m,
            )
          | None =>
            // if we can't expand, flag as improper model
            add'(
              ~self=BadLivelitModel(expansion_t),
              ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]),
              ~constraints=fn.constraints @ arg.constraints,
              m,
            )
          };

        | None =>
          let (fn, m) = go(~ana=mk_temp_internal(), fn, m);
          let (arg, m) = go(~ana=mk_temp_internal(), arg, m);
          add(
            ~self=Just(mk_temp_internal()),
            ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]),
            ~constraints=fn.constraints @ arg.constraints,
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
        // TODO: (THI) having to handle arrow provs outside of a matched arrow seems unsafe
        let syn_arrow = () =>
          Arrow(
            Unknown(LArrow(SynSwitch) |> Prov.fresh) |> Typ.temp,
            Unknown(RArrow(SynSwitch) |> Prov.fresh) |> Typ.temp,
          )
          |> Typ.temp;

        let fn_ana =
          switch (Exp.ctr_name(fn)) {
          | Some(name) =>
            switch (Self.ctr_ana_typ(ctx, ana, name)) {
            | Some(ty_ana) =>
              switch (Typ.matched_arrow_strict(ctx, ty_ana)) {
              | Some((ty1, ty2, _)) => Arrow(ty1, ty2) |> Typ.temp
              | None => syn_arrow()
              }
            | None => syn_arrow()
            }
          | None => syn_arrow()
          };

        let (fn, m) = go(~ana=fn_ana, fn, m);
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
          let (ty_in, ty_out, arr_constraints) =
            Typ.matched_arrow(ctx, fn.ty);
          let (arg, m) = go(~ana=ty_in, arg, m);
          let self: Self.exp =
            Id.is_nullary_ap_flag(IdTagged.ids(arg.term))
            && !Typ.is_consistent(ctx, ty_in, Prod([]) |> Typ.temp)
              ? BadTrivAp(ty_in) : Common(Just(ty_out));

          add'(
            ~self,
            ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]),
            ~constraints=
              arr_constraints
              @ fn.constraints
              @ arg.constraints
              @ subsumption_constraints_exp(self),
            m,
          );
        };
      }
    | TypAp(fn, utyp) =>
      let typfn_ana =
        Poly(
          Unknown(Hole(EmptyHole) |> Prov.fresh) |> TPat.fresh,
          mk_temp_syn(),
        )
        |> Typ.temp;
      let (fn, m) = go(~ana=typfn_ana, fn, m);
      let (_, m) = utyp_to_info_map(~ctx, ~ancestors, utyp, m);

      let (option_name, ty_body, poly_constraints) =
        Typ.matched_poly(ctx, fn.ty);
      switch (option_name) {
      | Some(name) =>
        let self: Self.t = Just(Typ.subst(utyp, name, ty_body));
        add(
          ~self,
          ~co_ctx=fn.co_ctx,
          ~constraints=
            fn.constraints
            @ poly_constraints
            @ subsumption_constraints_t(self),
          m,
        );
      | None =>
        let self: Self.t = Just(ty_body);
        add(
          ~self,
          ~co_ctx=fn.co_ctx,
          ~constraints=
            fn.constraints
            @ poly_constraints
            @ subsumption_constraints_t(self),
          m,
        ); /* invalid name matches with no free type variables. */
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
            | Some((ty1, ty2, _)) => Arrow(ty1, ty2) |> Typ.temp
            | None => Arrow(mk_temp_syn(), mk_temp_syn()) |> Typ.temp
            }
          | None => Arrow(mk_temp_syn(), mk_temp_syn()) |> Typ.temp
          }
        | None => Arrow(mk_temp_syn(), mk_temp_syn()) |> Typ.temp
        };
      let (fn, m) = go(~ana=fn_ana, fn, m);

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
        let (ty_in, ty_out, arr_constraints) = Typ.matched_arrow(ctx, fn.ty);
        let num_args = List.length(args);
        switch (Typ.matched_args_strict(ctx, ty_in, num_args)) {
        | L(ty_ins) =>
          let (args_infos, m) = map_m_go(m, ty_ins, args);
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
          let self: Self.t = Just(Arrow(ty_in', ty_out) |> Typ.temp);
          add(
            ~self,
            ~co_ctx=CoCtx.union([fn.co_ctx, arg_co_ctx]),
            ~constraints=
              arr_constraints
              @ fn.constraints
              @ List.flatten(List.map(Info.exp_constraints, args_infos))
              @ subsumption_constraints_t(self),
            m,
          );
        | R(expected) =>
          let ty_ins = List.init(num_args, _ => mk_temp_internal());
          let (args, m) = map_m_go(m, ty_ins, args);
          let arg_co_ctx = CoCtx.union(List.map(Info.exp_co_ctx, args));
          let self: Self.exp =
            IsBadPartialAp(
              ArityMismatch({
                expected,
                actual: num_args,
              }),
            );
          add'(
            ~self,
            ~co_ctx=CoCtx.union([fn.co_ctx, arg_co_ctx]),
            ~constraints=
              fn.constraints
              @ List.flatten(List.map(Info.exp_constraints, args))
              @ subsumption_constraints_exp(self),
            m,
          );
        };
      };
    | Fun(p, e, typ, _) =>
      let (mode_pat, mode_body, arr_constraint) =
        Typ.matched_arrow(ctx, ana);
      let mode_pat = Option.value(~default=mode_pat, typ);
      let (p', _) =
        go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~ana=mode_pat, p, m);
      let (e, m) = go'(~ctx=p'.ctx, ~ana=mode_body, e, m);
      /* add co_ctx to pattern */
      let (p, m) =
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
      add'(
        ~self,
        ~co_ctx=CoCtx.mk(ctx, p.ctx, e.co_ctx),
        ~constraints=
          arr_constraint
          @ e.constraints
          @ p.typ_constraints
          @ p'.typ_constraints,
        m,
      );
    | Forall(p, e) =>
      let (p, m) = go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, p, m);
      let (e, m) = go'(~ctx=p.ctx, ~ana=Atom(Bool) |> Typ.temp, e, m);
      add'(
        ~self=Common(Just(Atom(Bool) |> Typ.temp)),
        ~co_ctx=CoCtx.mk(ctx, p.ctx, e.co_ctx),
        ~constraints=e.constraints @ p.typ_constraints,
        m,
      );
    | TypFun(utpat, body, _) =>
      let (name_expected_opt, item, poly_constraints) =
        Typ.matched_poly(ctx, ana);
      let (mode_body, ctx_body) =
        switch (TPat.tyvar_of_utpat(utpat)) {
        | Some(name) when !Ctx.shadows_typ(ctx, name) =>
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
      let (body, m) = go'(~ctx=ctx_body, ~ana=mode_body, body, m);
      add(
        ~self=Just(Poly(utpat, body.ty) |> Typ.temp),
        ~co_ctx=body.co_ctx,
        ~constraints=body.constraints @ poly_constraints,
        m,
      );
    | Let(p, def, body) =>
      let (p_syn, _) =
        go_pat(~is_synswitch=true, ~co_ctx=CoCtx.empty, ~ana=anon_syn, p, m);
      let (def, p_ana_ctx, p_ana'_cons, m, ty_p_ana) =
        if (!is_recursive(ctx, p, def, p_syn.ty)) {
          let (def, m) = go(~ana=p_syn.ty, def, m);
          let ty_p_ana = def.ty;
          let (p_ana', _) =
            go_pat(
              ~is_synswitch=false,
              ~co_ctx=CoCtx.empty,
              ~ana=ty_p_ana,
              p,
              m,
            );
          (def, p_ana'.ctx, p_ana'.typ_constraints, m, ty_p_ana);
        } else {
          let (def_base, _) = go'(~ctx=p_syn.ctx, ~ana=p_syn.ty, def, m);
          let ty_p_ana = def_base.ty;
          /* Analyze pattern to incorporate def type into ctx */
          let (p_ana', _) =
            go_pat(
              ~is_synswitch=false,
              ~co_ctx=CoCtx.empty,
              ~ana=ty_p_ana,
              p,
              m,
            );
          let def_ctx = p_ana'.ctx;
          let (def_base2, _) = go'(~ctx=def_ctx, ~ana=p_syn.ty, def, m);
          let ana_ty_fn = ((ty_fn1, ty_fn2), ty_p) => {
            let is_ty_p_unk_synswitch =
              switch (Typ.term_of(ty_p)) {
              | Unknown({term: SynSwitch, _}) => true
              | _ => false
              };
            is_ty_p_unk_synswitch && !Typ.equal(ty_fn1, ty_fn2)
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
          let (def, m) = go'(~ctx=def_ctx, ~ana, def, m);
          (def, def_ctx, p_ana'.typ_constraints, m, ty_p_ana);
        };
      let (body, m) = go'(~ctx=p_ana_ctx, ~ana, body, m);
      /* add co_ctx to pattern */
      let (p_ana, m) =
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
      // TODO: (THI) do we need p_ana' constraints?
      add'(
        ~self,
        ~co_ctx=
          CoCtx.union([def.co_ctx, CoCtx.mk(ctx, p_ana.ctx, body.co_ctx)]),
        ~constraints=
          p_ana.typ_constraints
          @ p_ana'_cons
          @ p_syn.typ_constraints
          @ def.constraints
          @ body.constraints,
        m,
      );
    | Theorem({term: Var(_), _} as p, e1, e2) =>
      let (e1', m) = go'(~ctx, ~ana=Atom(Bool) |> Typ.temp, e1, m);
      let (p', _) =
        go_pat(
          ~is_synswitch=false,
          ~co_ctx=CoCtx.empty,
          ~ana=Typ.fresh(ProofOf(e1)),
          p,
          m,
        );
      let (e2, m) = go'(~ctx=p'.ctx, ~ana, e2, m);
      /* add co_ctx to pattern */
      let (p, m) =
        go_pat(
          ~is_synswitch=false,
          ~co_ctx=e2.co_ctx,
          ~ana=mk_temp_syn(),
          p,
          m,
        );
      add(
        ~self=Just(e2.ty),
        ~constraints=
          p.typ_constraints
          @ e2.constraints
          @ e1'.constraints
          @ p'.typ_constraints,
        ~co_ctx=
          CoCtx.union([
            p'.co_ctx,
            e1'.co_ctx,
            CoCtx.mk(ctx, p.ctx, e2.co_ctx),
          ]),
        m,
      );
    | Theorem(p, e1, e2) =>
      let (_, m) = go'(~ctx, ~ana=Atom(Bool) |> Typ.temp, e1, m);
      let (p', _) =
        go_pat(
          ~is_synswitch=false,
          ~co_ctx=CoCtx.empty,
          ~ana=mk_temp_syn(),
          p,
          m,
        );
      let (e2, m) = go'(~ctx=p'.ctx, ~ana, e2, m);
      /* add co_ctx to pattern */
      let (p, m) =
        go_pat(
          ~is_synswitch=false,
          ~co_ctx=e2.co_ctx,
          ~ana=mk_temp_syn(),
          p,
          m,
        );
      add'(
        ~self=BadTheorem(e2.ty),
        ~co_ctx=CoCtx.union([p'.co_ctx, CoCtx.mk(ctx, p.ctx, e2.co_ctx)]),
        ~constraints=p.typ_constraints @ e2.constraints @ p'.typ_constraints,
        m,
      );
    | ProofObject(e) =>
      let (_, m) = go'(~ctx, ~ana=Atom(Bool) |> Typ.temp, e, m);
      add(
        ~self=Just(Typ.temp(ProofOf(e))),
        ~constraints=[],
        ~co_ctx=CoCtx.empty,
        m,
      ); // TODO[Matt]: do types need coctxs now?
    | FixF(p, e, _) =>
      let (p', _) =
        go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~ana, p, m);
      let (e', m) = go'(~ctx=p'.ctx, ~ana=p'.ty, e, m);
      let (p'', m) =
        go_pat(~is_synswitch=false, ~co_ctx=e'.co_ctx, ~ana, p, m);
      // TODO: (THI) are p' cons necessary?
      add(
        ~self=Just(p'.ty),
        ~co_ctx=CoCtx.union([CoCtx.mk(ctx, p''.ctx, e'.co_ctx)]),
        ~constraints=p''.typ_constraints @ p'.typ_constraints @ e'.constraints,
        m,
      );
    | If(e0, e1, e2) =>
      let branch_ids = List.map(Exp.rep_id, [e1, e2]);
      let (cond, m) = go(~ana=Atom(Bool) |> Typ.temp, e0, m);
      let (cons, m) = go(~ana, e1, m);
      let (alt, m) = go(~ana, e2, m);
      let (self, self_cons) =
        Self.match(ctx, [cons.ty, alt.ty], branch_ids);
      add(
        ~self,
        ~co_ctx=CoCtx.union([cond.co_ctx, cons.co_ctx, alt.co_ctx]),
        ~constraints=
          cond.constraints
          @ cons.constraints
          @ alt.constraints
          @ constrain_branches([cons.ty, alt.ty])
          @ self_cons,
        m,
      );
    | Match(scrut, rules) =>
      let (scrut, m) = go(~ana=anon_syn, scrut, m);
      let (ps, es) = List.split(rules);
      let branch_ids = List.map(Exp.rep_id, es);
      let (ps', _) =
        map_m(
          go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~ana=scrut.ty),
          ps,
          m,
        );

      let p_ctxs = List.map(Info.pat_ctx, ps');
      let p_tys = List.map(Info.pat_ty, ps');
      let (es, m) =
        List.fold_left2(
          ((es, m), e, ctx) =>
            go'(~ctx, ~ana, e, m) |> (((e, m)) => (es @ [e], m)),
          ([], m),
          es,
          p_ctxs,
        );

      let e_tys = List.map(Info.exp_ty, es);
      let e_co_ctxs =
        List.map2(CoCtx.mk(ctx), p_ctxs, List.map(Info.exp_co_ctx, es));
      let (self, self_cons) = Self.match(ctx, e_tys, branch_ids);
      let unwrapped_self: Self.exp = Common(self);
      let (constraints, m) =
        List.fold_left(
          (
            (constraints: list(Coverage.Constraint.t), m: Map.t),
            (p, co_ctx),
          ) => {
            let (info, m) =
              go_pat(~is_synswitch=false, ~co_ctx, ~ana=scrut.ty, p, m);

            let p_constraint = Info.pat_constraint(info);
            ([p_constraint, ...constraints], m);
          },
          ([], m),
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
                  ~typ_constraints=info.typ_constraints,
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
      let ps_constraints =
        List.map((p: Info.pat) => p.typ_constraints, ps') |> List.flatten;
      let es_constraints = List.map(Info.exp_constraints, es) |> List.flatten;
      add'(
        ~self,
        ~co_ctx=CoCtx.union([scrut.co_ctx] @ e_co_ctxs),
        ~constraints=
          ps_constraints
          @ es_constraints
          @ scrut.constraints
          @ constrain_branches(e_tys)
          @ constrain_branches(p_tys @ [scrut.ty])  // scrutinee needs to be constrained to patterns
          @ self_cons,
        m,
      );
    | TyAlias(typat, utyp, body) =>
      let m = utpat_to_info_map(~ctx, ~ancestors, typat, m) |> snd;
      switch (typat.term) {
      | Var(name) when !Ctx.shadows_typ(ctx, name) =>
        /* Currently we disallow all type shadowing */
        /* NOTE(andrew): Currently, Typ.to_typ returns Unknown(TypeHole)
           for any type variable reference not in its ctx. So any free variables
           in the definition would be obliterated. But we need to check for free
           variables to decide whether to make a recursive type or not. So we
           tentatively add an abtract type to the ctx, representing the
           speculative rec parameter. */
        let (ty_def, ctx_def, ctx_body) = {
          switch (utyp.term) {
          | _ when List.mem(name, Typ.free_vars(utyp)) =>
            /* NOTE: When debugging type system issues it may be beneficial to
               use a different name than the alias for the recursive parameter */
            //let ty_rec = Typ.Rec("α", Typ.subst(Var("α"), name, ty_pre));
            let ty_rec = Rec(Var(name) |> TPat.fresh, utyp) |> Typ.temp;
            let ctx_def =
              Ctx.extend_alias(ctx, name, TPat.rep_id(typat), ty_rec);
            (ty_rec, ctx_def, ctx_def);
          | _ => (
              utyp,
              ctx,
              Ctx.extend_alias(ctx, name, TPat.rep_id(typat), utyp),
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
        let (
          {co_ctx, ty: ty_body, constraints: body_constraints, _}: Info.exp,
          m,
        ) =
          go'(~ctx=ctx_body, ~ana, body, m);
        /* Make sure types don't escape their scope */
        let ty_escape = Typ.subst(ty_def, typat, ty_body);
        let m = utyp_to_info_map(~ctx=ctx_def, ~ancestors, utyp, m) |> snd;
        let self: Self.t = Just(ty_escape);
        add(
          ~self,
          ~co_ctx,
          ~constraints=body_constraints @ subsumption_constraints_t(self),
          m,
        );
      | Var(_)
      | Unknown(_) =>
        let ({co_ctx, ty: ty_body, constraints, _}: Info.exp, m) =
          go'(~ctx, ~ana, body, m);
        let m = utyp_to_info_map(~ctx, ~ancestors, utyp, m) |> snd;
        let self: Self.t = Just(ty_body);
        add(
          ~self,
          ~co_ctx,
          ~constraints=constraints @ subsumption_constraints_t(self),
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
      let (body, m) = go'(~ctx=ctx', ~ana, body, m);
      let self: Self.exp =
        switch (use_mode) {
        | Some(_) => Common(Just(body.ty))
        | None when Typ.fast_equal(mk_temp_internal(), typ.term) =>
          Common(Just(body.ty))
        | None =>
          InvalidUseMode({
            bad_typ: typ.term,
            inner_typ: body.ty,
          })
        };
      add'(
        ~self,
        ~co_ctx=body.co_ctx,
        ~constraints=body.constraints @ subsumption_constraints_exp(self),
        m,
      );
    };
  };

  // This is for lifting single values into a singleton labeled tuple when the label is not present

  switch (Typ.weak_head_normalize(ctx, ana).term) {
  | Prod([{term: TupLabel({term: Label(l1), _}, ana_ty), _}]) =>
    // We can flatten this by pulling it up on the case match but since OCaml is strict it'll be evaluated.
    // So for performance reasons we'll just do it here.
    let (e, m) = go(~ana=anon_syn, uexp, m);

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
      ~co_ctx,
      ~ancestors: Info.ancestors,
      ~duplicates: list(string),
      ~expected_labels=?,
      ~ana: Typ.t=mk_temp_internal(),
      ~under_ascription: bool=false,
      ~override_self: option(Self.t)=?,
      ~inferred_label=?,
      ~label_sort=false,
      {annotation: {ids, _}, term} as upat: Pat.t,
      m: Map.t,
    )
    : (Info.pat, Map.t) => {
  let add' =
      (
        ~self: Self.pat,
        ~ctx: Ctx.t,
        ~typ_constraints: list(Typ.equivalence),
        ~constraint_: Coverage.Constraint.t,
        ~label_inference: option(Info.label_inference(Info.pat))=?,
        m: Id.Map.t(Info.t),
      )
      : (Info.pat, Map.t) => {
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
        ~typ_constraints,
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

    (info, add_info(ids, InfoPat(info), m));
  };
  let add =
      (
        ~self: Self.t,
        ~ctx: Ctx.t,
        ~constraint_: Coverage.Constraint.t,
        ~typ_constraints: list(Typ.equivalence),
        ~label_inference: option(Info.label_inference(Info.pat))=?,
        m: Id.Map.t(Info.t),
      )
      : (Info.pat, Map.t) => {
    add'(
      ~self=Common(self),
      ~ctx,
      ~constraint_,
      ~typ_constraints,
      ~label_inference?,
      m,
    );
  };
  let upat_to_info_map =
      (
        ~is_synswitch,
        ~ctx,
        ~co_ctx,
        ~ancestors,
        ~duplicates=[],
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
      ~duplicates,
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
  let atomic = (self, constraint_) =>
    add(~self, ~ctx, ~typ_constraints=[], ~constraint_, m);
  let ancestors = [Pat.rep_id(upat)] @ ancestors;
  let go = (~under_ascription=false) =>
    upat_to_info_map(~under_ascription, ~is_synswitch, ~ancestors, ~co_ctx);
  let unknown =
    Unknown(
      (is_synswitch ? SynSwitch : Internal: TermBase.type_provenance)
      |> IdTagged.fresh,
    )
    |> Typ.temp;
  let ctx_fold = (ctx: Ctx.t, m, ~duplicates=[]) =>
    List.fold_left2(
      ((ctx, tys, cons, m, info_all), e, ana) =>
        go(~ctx, ~ana, ~duplicates, ~inferred_label?, e, m)
        |> (
          ((info, m)) => (
            info.ctx,
            tys @ [info.ty],
            cons @ [info.constraint_],
            m,
            info_all @ [info],
          )
        ),
      (ctx, [], [], m, []),
    );
  let hole = self => atomic(self, Coverage.Constraint.Hole(None));
  let subsumption_constraints_t = subsumption_constraints_t(ana, ctx);

  let elaborate_singleton_tuple = (upat: Pat.t, inner_ty, l, m) => {
    let (term, rewrap) = Pat.unwrap(upat);
    let original_expression = Pat.fresh(term);
    let (original_info, m) =
      upat_to_info_map(
        ~ctx,
        ~co_ctx,
        ~is_synswitch,
        ~ancestors,
        ~ana=inner_ty,
        original_expression,
        m,
      );
    /* Special case for probes, which would otherwise lose their id association here */
    let elaborated_pat =
      switch (term) {
      | Probe(_, p) =>
        rewrap(
          Probe(
            Tuple([
              TupLabel(Label(l) |> Pat.fresh, original_expression)
              |> Pat.fresh,
            ])
            |> Pat.fresh,
            p,
          ),
        )
      | _ =>
        rewrap(
          Tuple([
            TupLabel(Label(l) |> Pat.fresh, original_expression) |> Pat.fresh,
          ]),
        )
      };
    let (info, m) =
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

    (info, add_info(IdTagged.ids(elaborated_pat), InfoPat(info), m));
  };

  let default_case = (): (Info.pat, Map.t) =>
    switch (term) {
    | MultiHole(tms) =>
      let (_, typ_constraints, m) = multi(~ctx, ~ancestors, m, tms);
      add(
        ~self=IsMulti,
        ~ctx,
        ~typ_constraints=typ_constraints @ subsumption_constraints_t(IsMulti),
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
          Just(Atom(Nat) |> Typ.temp),
          Coverage.Constraint.BigInt(nat),
        )
      | L(Int(int)) =>
        atomic(
          Just(Atom(Int) |> Typ.temp),
          Coverage.Constraint.BigInt(int),
        )
      | L(SInt(int)) =>
        atomic(Just(Atom(SInt) |> Typ.temp), Coverage.Constraint.SInt(int))
      | L(Float(float)) =>
        atomic(
          Just(Atom(Float) |> Typ.temp),
          Coverage.Constraint.Float(float),
        )
      | L(Bool(bool)) =>
        atomic(
          Just(Atom(Bool) |> Typ.temp),
          bool ? Coverage.Constraint.true_ : Coverage.Constraint.false_,
        )
      | L(String(string)) =>
        atomic(
          Just(Atom(String) |> Typ.temp),
          Coverage.Constraint.String(string),
        )
      | R(BadInt(str)) => hole(BadToken(str))
      };
    | ListLit(ps) =>
      let ids = List.map(Pat.rep_id, ps);
      let (mode, constraints) = Typ.matched_list(ctx, ana);
      let modes = List.init(List.length(ps), _ => mode);
      let (ctx, tys, cons, m, _) = ctx_fold(ctx, m, ps, modes);
      let rec cons_fold_list = cs =>
        switch (cs) {
        | [] => Coverage.Constraint.nil
        | [hd, ...tl] => Coverage.Constraint.cons(hd, cons_fold_list(tl))
        };
      let (self, self_constraints) =
        Self.listlit(~empty=unknown, ctx, tys, ids);
      add(
        ~self,
        ~ctx,
        ~typ_constraints=constraints @ self_constraints,
        ~constraint_=cons_fold_list(cons),
        m,
      );
    | Cons(hd, tl) =>
      let (inner_ty, list_constraints) = Typ.matched_list(ctx, ana);
      let (hd, m) = go(~ctx, ~ana=inner_ty, hd, m);
      let (tl, m) =
        go(~ctx=hd.ctx, ~ana=List(inner_ty) |> Typ.fresh, tl, m);
      add(
        ~self=Just(List(hd.ty) |> Typ.temp),
        ~ctx=tl.ctx,
        ~typ_constraints=
          list_constraints @ hd.typ_constraints @ tl.typ_constraints,
        ~constraint_=Coverage.Constraint.cons(hd.constraint_, tl.constraint_),
        m,
      );
    | Wild => atomic(Just(unknown), Coverage.Constraint.Truth)
    | Var(name) =>
      /* NOTE: The self type assigned to pattern variables (Unknown)
         may be SynSwitch, but SynSwitch is never added to the context;
         Unknown(Internal |> Prov.fresh) is used in this case */
      let (ctx_typ, ctx_typ_cons) =
        Info.fixed_typ_pat(
          ctx,
          ana,
          Common(Just(Unknown(Internal |> Prov.fresh) |> Typ.temp)),
        );
      let entry =
        Ctx.VarEntry({
          name,
          id: Pat.rep_id(upat),
          typ: ctx_typ,
          custom_statics: None,
        });
      let self: Self.t = Just(unknown);
      add(
        ~self,
        ~ctx=Ctx.extend(ctx, entry),
        ~constraint_=Coverage.Constraint.Truth,
        ~typ_constraints=subsumption_constraints_t(self) @ ctx_typ_cons,
        m,
      );
    | TupLabel({term: ExplicitNonlabel, _} as label, p) =>
      let (p, m) = go(~ana, ~ctx, p, m);
      let (_, m) = go(~label_sort=true, ~ctx, ~ana=mk_temp_syn(), label, m);
      (p, add_info(ids, InfoPat(p), m));
    | ExplicitNonlabel => atomic(ExplicitNonlabel, Coverage.Constraint.Truth)
    | TupLabel(label, p) =>
      let (lab, p, matched_label_constraints, m) =
        switch (Typ.matched_label(ctx, ana)) {
        | Some((labmode, val_mode, matched_label_constraints)) =>
          let label_self: option(Self.t) =
            switch (label.term) {
            | Label(_) => None
            | EmptyHole => None
            | _ => Some(BadLabel(Pat(label)))
            };

          let (lab, m) =
            go(
              ~ctx,
              ~ana=labmode,
              ~override_self=?label_self,
              ~duplicates,
              ~label_sort=true,
              label,
              m,
            );
          let (p, m) = go(~ctx, ~ana=val_mode, ~inferred_label?, p, m);
          (lab, p, matched_label_constraints, m);
        | _ =>
          let (lab, m) =
            go(
              ~ctx,
              ~ana=mk_temp_internal(),
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
              ~duplicates,
              label,
              m,
            );

          let (p, m) =
            go(~ctx, ~ana=mk_temp_internal(), ~inferred_label?, p, m);
          (lab, p, [], m);
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
            typ: TupLabel(mk_temp_internal(), p.ty) |> Typ.temp,
          })
        };
      add(
        ~self,
        ~ctx=p.ctx,
        ~typ_constraints=
          p.typ_constraints @ lab.typ_constraints @ matched_label_constraints,
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

      let (inferred_ps, modes, typ_constraints) =
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
      let duplicate_labels =
        LabeledTuple.get_duplicate_labels(Pat.match_tup_label, ps);
      let (ctx, tys, cons, m, info_pats) =
        List.fold_left2(
          ((ctx, tys, cons, m, info_all), (inferred_label, e), ana) =>
            go(
              ~ctx,
              ~ana,
              ~inferred_label?,
              ~duplicates=duplicate_labels,
              ~expected_labels?,
              e,
              m,
            )
            |> (
              ((info, m)) => (
                info.ctx,
                tys @ [info.ty],
                cons @ [info.constraint_],
                m,
                info_all @ [info],
              )
            ),
          (ctx, [], [], m, []),
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
        ~typ_constraints,
        ~constraint_,
        ~label_inference=
          Info.derive_label_inference_info(original_labels, new_labels),
        m,
      );
    | Label(name) =>
      let self = Self.Just(Label(name) |> Typ.temp);
      List.exists(l => name == l, duplicates)
        ? atomic(Duplicate(name, self), Coverage.Constraint.Truth)
        : atomic(self, Coverage.Constraint.Truth);
    | Parens(p)
    | Probe(p, _) =>
      let (p, m) = go(~ctx, ~ana, p, m);
      add'(
        ~self=p.self,
        ~ctx=p.ctx,
        ~typ_constraints=p.typ_constraints,
        ~constraint_=p.constraint_,
        m,
      );
    | Constructor(ctr, ty) =>
      let self = Self.of_ctr(ctx, ctr, ana, ty);
      atomic(self, Coverage.Constraint.Ap(ctr, None));
    | Ap(fn, arg) =>
      let ctr = Pat.ctr_name(fn);
      let fn_ana = Arrow(mk_temp_syn(), ana) |> Typ.temp;
      let (fn', m) = go(~ctx, ~ana=fn_ana, fn, m);
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
              ~typ_constraints=fn'.typ_constraints,
              ~constraint_=fn'.constraint_,
              ~label_inference=fn'.label_inference,
              ~inferred_label=fn'.inferred_label,
              ~label_sort=fn'.label_sort,
            );
          add_info(IdTagged.ids(fn), InfoPat(info), m);
        };
      };
      let (ty_in, ty_out, arr_constraints) = Typ.matched_arrow(ctx, fn'.ty);
      let (arg, m) = go(~ctx, ~ana=ty_in, arg, m);
      let constraint_ =
        switch (ctr) {
        | Some(ctr) => Coverage.Constraint.Ap(ctr, Some(arg.constraint_))
        | None => Coverage.Constraint.Hole(None)
        };
      add(
        ~self=Just(ty_out),
        ~ctx=arg.ctx,
        ~typ_constraints=
          fn'.typ_constraints @ arg.typ_constraints @ arr_constraints,
        ~constraint_,
        m,
      );
    | Asc(p, ann) =>
      let (ann, m) = utyp_to_info_map(~ctx, ~ancestors, ann, m);
      let (p, m) = go(~ctx, ~under_ascription=true, ~ana=ann.term, p, m);
      let self: Self.t = Just(ann.term);
      add(
        ~self,
        ~ctx=p.ctx,
        ~typ_constraints=p.typ_constraints @ subsumption_constraints_t(self),
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
      let (e, m) = go(~ana=anon_syn, ~ctx, upat, m);

      switch (Typ.weak_head_normalize(ctx, e.ty).term) {
      | Prod([{term: TupLabel({term: Label(l2), _}, _), _}]) when l1 == l2 =>
        default_case()
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
  | Unknown({term: Hole(MultiHole(tms)), _}) =>
    let (_, _, m) = multi(~ctx, ~ancestors, m, tms);
    add(m);
  | Unknown(_)
  | Atom(_) => add(m)
  | Var(_) =>
    /* Names are resolved in Info.status_typ */
    add(m)
  | List(t)
  | Parens(t) => add(go(t, m) |> snd)
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
    let (_, m) =
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
  | Unknown({term: Hole(MultiHole(tms)), _}) =>
    let (_, _, m) = multi(~ctx, ~ancestors, m, tms);
    add(m);
  | Unknown({term: Hole(Invalid(_)), _})
  | Unknown({term: Hole(EmptyHole), _})
  | Unknown({term: Hole(CycleHole), _})
  | Unknown({term: LArrow(_), _})
  | Unknown({term: RArrow(_), _})
  | Unknown({term: NProduct(_), _})
  | Unknown({term: SynSwitch, _})
  | Unknown({term: Internal, _})
  | Unknown({term: MList(_), _})
  | Unknown({term: RForall(_), _})
  | Unknown({term: TupLabel(_), _})
  | Unknown({term: TupLabelArg(_), _})
  | Unknown({term: Meet(_), _})
  | Unknown({term: TypeSubstitution(_), _})
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
  | Variant(ctr, ids, param) =>
    let m =
      go(
        ConstructorExpected(
          List.mem(ctr, ctrs) ? Duplicate : Unique,
          ty_sum,
        ),
        {
          term: Var(ctr),
          annotation: {
            ids: ids,
          },
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
      let (info, map) =
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

      // Map.show(map) |> print_endline;

      let inference_sols = Inference.go(info.constraints);
      // Inference.ProvMap.iter(
      //   (key, sol) => {
      //     print_endline(
      //       Inference.StringProv.show(key)
      //       ++ " -> "
      //       ++ Inference.Solution.show(sol),
      //     )
      //   },
      //   inference_sols,
      // );
      (map, inference_sols);
    },
  );

let mk = (~ana=mk_temp_syn(), core: CoreSettings.t, ctx, exp) =>
  core.statics
    ? mk(ana, ctx, exp) : (Id.Map.empty, Inference.TypSolutionMap.empty);
