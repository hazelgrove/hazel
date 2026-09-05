open Util.OptUtil.Syntax;
module Info = Info;

module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(Info.t);

  let empty = Id.Map.empty;
  let lookup = Id.Map.find_opt;
  let add_info = (ids: list(Id.t), info: Info.t, m: t): t =>
    ids |> List.fold_left((m, id) => Id.Map.add(id, info, m), m);

  let add_missing_info = (ids: list(Id.t), info: Info.t, m: t): t =>
    ids
    |> List.fold_left(
         (m, id) =>
           switch (Id.Map.find_opt(id, m)) {
           | Some(_) => m
           | None => Id.Map.add(id, info, m)
           },
         m,
       );

  let error_ids = (info_map: t): list(Id.t) =>
    Id.Map.fold(
      (id, info, acc) =>
        /* Second clause is to eliminate non-representative ids,
         * which will not be found in the measurements map */
        Info.is_error(info) && id == Info.id_of(info) ? [id, ...acc] : acc,
      info_map,
      [],
    );
  let warning_ids = (info_map: t): list(Id.t) =>
    Id.Map.fold(
      (id, info, acc) =>
        acc
        |> (
          Info.is_warning(info) && id == Info.id_of(info)
            ? List.cons(id) : Fun.id
        ),
      info_map,
      [],
    );

  /* The ids of binding sites for for all references in term with `id` */
  let refs_in = (m: t, id: Id.t): Binding.s =>
    switch (lookup(id, m)) {
    | Some(InfoExp({co_ctx, ctx, _})) =>
      co_ctx
      |> Util.VarMap.to_list
      |> List.map(((n, _)) => Ctx.binding_of(ctx, n))
    | _ => []
    };

  let bound_in = (m: t, id: Id.t): Binding.s =>
    switch (lookup(id, m)) {
    | Some(InfoPat({user_term, _})) => Pat.bindings(user_term)
    | _ => []
    };

  /* Collect all infos whose binding site is `binding_id`, plus `binding_id`
     itself. Deduplication is handled by accumulating into an `Id.Set`. */
  let ids_referencing_binding = (m: t, binding_id: Id.t): Id.Set.t =>
    Id.Map.fold(
      (id, info, acc) =>
        switch (Info.get_binding_site(info)) {
        | Some(id') when Id.equal(id', binding_id) => Id.Set.add(id, acc)
        | _ => acc
        },
      m,
      Id.Set.singleton(binding_id),
    );

  /* IDs to highlight for a variable/type/constructor reference:
   * all infos that resolve to the same binding site id, plus the binding id. */
  let var_highlight_ids = (m: t, ci: Info.t): list(Id.t) => {
    let binding_id =
      switch (Info.get_binding_site(ci)) {
      | Some(_) as b => b
      | None =>
        switch (ci) {
        | Info.InfoPat({user_term: {term: Var(_), _}, _})
        | Info.InfoTPat({user_term: {term: Var(_), _}, _})
        | Info.InfoTyp({
            user_term: {term: Var(_), _},
            expects:
              TypExpectation.ConstructorExpected(_, _) |
              TypExpectation.VariantExpected(_),
            _,
          }) =>
          Some(Info.id_of(ci))
        | _ => None
        }
      };
    switch (binding_id) {
    | None => []
    | Some(binding_id) =>
      Id.Set.elements(ids_referencing_binding(m, binding_id))
    };
  };

  let parent_ci_of = (map: t, id: Id.t): option(Info.t) => {
    let* ci = lookup(id, map);
    let* parent_id = Info.parent_id_of(ci);
    lookup(parent_id, map);
  };

  let parent_term_of = (map: t, id: Id.t): option(Any.t) => {
    let* ci = parent_ci_of(map, id);
    Info.any_of(ci);
  };

  /* Starting from a binding site id (possibly inside a deep pattern),
   * climb ancestor ids to find the enclosing let, and return
   * the id of its body expression. */
  let enclosing_let_of_binding =
      (~statics: t, ~binding_id: Id.t): option(Id.t) => {
    open Util.OptUtil.Syntax;
    let* ci_binder = lookup(binding_id, statics);
    let rec climb = (ancs: list(Id.t)): option(Id.t) =>
      switch (ancs) {
      | [] => None
      | [ancestor_id, ...rest] =>
        let* ci = lookup(ancestor_id, statics);
        switch (ci) {
        | InfoExp({user_term: {term: Let(pat, def, _), _}, _}) =>
          let binds = Pat.bindings(pat);
          List.exists((b: Binding.t) => b.id == binding_id, binds)
            ? Some(IdTagged.rep_id(def)) : climb(rest);
        | InfoExp(_) => None
        | _ => climb(rest)
        };
      };
    climb(Info.ancestors_of(ci_binder));
  };

  let lookup_exp = (id: Id.t, m: t): option(Info.exp) =>
    switch (lookup(id, m)) {
    | Some(InfoExp(info)) => Some(info)
    | _ => None
    };

  let lookup_pat = (id: Id.t, m: t): option(Info.pat) =>
    switch (lookup(id, m)) {
    | Some(InfoPat(info)) => Some(info)
    | _ => None
    };

  let lookup_typ = (id: Id.t, m: t): option(Info.typ) =>
    switch (lookup(id, m)) {
    | Some(InfoTyp(info)) => Some(info)
    | _ => None
    };

  let ty_of = (id: Id.t, m: t): option(Typ.t) =>
    switch (lookup(id, m)) {
    | Some(InfoExp({ty, _}))
    | Some(InfoPat({ty, _})) => Some(ty)
    | _ => None
    };

  let ctx_of = (id: Id.t, m: t): option(Ctx.t) =>
    switch (lookup(id, m)) {
    | Some(info) => Some(Info.ctx_of(info))
    | None => None
    };

  let ancestors_of = (id: Id.t, m: t): list(Id.t) =>
    switch (lookup(id, m)) {
    | Some(info) => Info.ancestors_of(info)
    | None => []
    };
};

let set_marks_exp = (m: Map.t, e: Exp.t, marks: list(Mark.t)): Map.t =>
  switch (Map.lookup(Exp.rep_id(e), m)) {
  | Some(Info.InfoExp(info)) =>
    Map.add_info(
      IdTagged.ids(info.user_term),
      InfoExp({
        ...info,
        marks,
      }),
      m,
    )
  | _ => m
  };

let append_mark_exp = (m: Map.t, e: Exp.t, extra: list(Mark.t)): Map.t =>
  switch (Map.lookup(Exp.rep_id(e), m)) {
  | Some(Info.InfoExp(info)) =>
    Map.add_info(
      IdTagged.ids(info.user_term),
      InfoExp({
        ...info,
        marks: info.marks @ extra,
      }),
      m,
    )
  | _ => m
  };

let set_label_sort_exp = (m: Map.t, e: Exp.t, label_sort: bool): Map.t =>
  switch (Map.lookup(Exp.rep_id(e), m)) {
  | Some(Info.InfoExp(info)) =>
    Map.add_info(
      IdTagged.ids(info.user_term),
      InfoExp({
        ...info,
        label_sort,
      }),
      m,
    )
  | _ => m
  };

let set_dot_labels_exp =
    (m: Map.t, e: Exp.t, dot_labels: list(string)): Map.t =>
  switch (Map.lookup(Exp.rep_id(e), m)) {
  | Some(Info.InfoExp(info)) =>
    Map.add_info(
      IdTagged.ids(info.user_term),
      InfoExp({
        ...info,
        dot_labels,
      }),
      m,
    )
  | _ => m
  };

let map_m = (f, xs, m: Map.t) =>
  List.fold_left(
    ((xs, m), x) => f(x, m) |> (((x, m)) => (xs @ [x], m)),
    ([], m),
    xs,
  );

let map_m2 = (f, xs, ys, m: Map.t) =>
  List.fold_left2(
    ((zs, m), x, y) => f(x, y, m) |> (((z, m)) => (zs @ [z], m)),
    ([], m),
    xs,
    ys,
  );

let syn = Unknown(SynSwitch) |> Typ.temp;

/* Type after hole fixing: best type consistent with analysis expectation and
   statics synthetic type (Typ.ana_meet, which admits signature subtyping at
   this analysis position). On failure, prefer syn under synthesis and ana
   under analysis. */
let fixed_typ = (ctx: Ctx.t, ana: Typ.t, elab_syn_ty: Typ.t): Typ.t =>
  switch (Typ.ana_meet(ctx, ~ana, ~syn=elab_syn_ty)) {
  | Some(ty) => ty
  | None =>
    if (Typ.is_syn_plus(ana)) {
      elab_syn_ty;
    } else {
      ana;
    }
  };

/* Patterns run the other way round: the pattern's own type is what is
   required, and the type it is analyzed against is what is provided. */
let fixed_typ_pat = (ctx: Ctx.t, ana: Typ.t, elab_syn_ty: Typ.t): Typ.t =>
  switch (Typ.ana_meet(ctx, ~ana=elab_syn_ty, ~syn=ana)) {
  | Some(ty) => ty
  | None =>
    if (Typ.is_syn_plus(ana)) {
      elab_syn_ty;
    } else {
      ana;
    }
  };

let patch_elab_syn_ty_exp = (m: Map.t, e: Exp.t, new_syn_ty: Typ.t): Map.t =>
  switch (Map.lookup(Exp.rep_id(e), m)) {
  | Some(Info.InfoExp(info)) =>
    Map.add_info(
      IdTagged.ids(info.user_term),
      InfoExp({
        ...info,
        elab_syn_ty: new_syn_ty,
        ty: fixed_typ(info.ctx, info.ana, new_syn_ty),
      }),
      m,
    )
  | _ => m
  };

/* Strip TupLabel(ExplicitNonlabel, _) wrappers on expectation. */
let rec ana_skip_explicit_nonlabel = (ty_ana: Typ.t): Typ.t =>
  switch (ty_ana.term) {
  | TupLabel({term: ExplicitNonlabel, _}, ana_inner) =>
    ana_skip_explicit_nonlabel(ana_inner)
  | _ => ty_ana
  };

let should_emit_nomeet_mark =
    (ctx: Ctx.t, ana: Typ.t, elab_syn_ty: Typ.t): bool =>
  switch (
    Typ.meet(
      ctx,
      ana_skip_explicit_nonlabel(ana),
      ana_skip_explicit_nonlabel(elab_syn_ty),
    )
  ) {
  | Some(_) => false
  | None => true
  };

let syn_ana_ok_common' =
    (~flipped, ctx: Ctx.t, ty_ana: Typ.t, elab_syn_ty: Typ.t)
    : Message.ok_common => {
  let ana = ana_skip_explicit_nonlabel(ty_ana);
  switch (ana.term) {
  | Unknown(SynSwitch) => Message.Syn(elab_syn_ty)
  | _ =>
    let met =
      flipped
        ? Typ.ana_meet(ctx, ~ana=elab_syn_ty, ~syn=ana)
        : Typ.ana_meet(ctx, ~ana, ~syn=elab_syn_ty);
    switch (met) {
    | None => Message.Syn(elab_syn_ty)
    | Some(meet) =>
      Message.Ana(
        Message.Consistent({
          ana,
          syn: elab_syn_ty,
          meet,
        }),
      )
    };
  };
};

let syn_ana_ok_common = syn_ana_ok_common'(~flipped=false);
let syn_ana_ok_common_pat = syn_ana_ok_common'(~flipped=true);

let expectation_mismatch_mark' =
    (~flipped, ctx: Ctx.t, ana: Typ.t, elab_syn_ty: Typ.t): option(Mark.t) => {
  let ana' = ana_skip_explicit_nonlabel(ana);
  let syn' = ana_skip_explicit_nonlabel(elab_syn_ty);
  switch (ana'.term) {
  | Unknown(SynSwitch) => None
  | _ =>
    let met =
      flipped
        ? Typ.ana_meet(ctx, ~ana=syn', ~syn=ana')
        : Typ.ana_meet(ctx, ~ana=ana', ~syn=syn');
    switch (met) {
    | Some(_) => None
    | None =>
      let generic =
        Mark.ExpectationMismatch({
          ana: ana',
          syn: syn',
        });
      /* An expression of signature type checked against a signature that
         declares members it lacks: name them, as a module literal does. */
      let whnf = ty => Typ.term_of(Typ.weak_head_normalize(ctx, ty));
      switch (flipped, whnf(ana'), whnf(syn')) {
      | (false, Sig(want), Sig(have)) =>
        switch (
          Sig.missing_members(
            ~want=Sig.members(want),
            ~have=Sig.members(have),
          )
        ) {
        | [] => Some(generic)
        | names => Some(Mark.ModuleMissingMembers(names))
        }
      | _ => Some(generic)
      };
    };
  };
};

let expectation_mismatch_mark = expectation_mismatch_mark'(~flipped=false);
let expectation_mismatch_mark_pat = expectation_mismatch_mark'(~flipped=true);

/* Lightweight pat update: prepend a mark and update dependent fields. */
let prepend_pat_mark =
    (
      info: Info.pat,
      mark: Mark.t,
      ~warnings: list(Warning.list_item)=info.warnings,
      (),
    )
    : Info.pat => {
  let marks = [mark, ...info.marks];
  let warning_acc =
    warnings
    @ (
      switch (info.user_term.term) {
      | Var(name) =>
        Warning.to_list(Warning.var_is_unused(info.co_ctx, name))
      | _ => []
      }
    );
  let constraint_ =
    switch (info.constraint_) {
    | Coverage.Constraint.Hole(_) => info.constraint_
    | _ => Coverage.Constraint.Hole(Some(info.constraint_))
    };
  {
    ...info,
    marks,
    message: Message.Pat(Message.Default),
    warnings: warning_acc,
    constraint_,
  };
};

/* Add an ascription wrapper if the types differ after normalization. */
let fresh_ascription = (ctx: Ctx.t, d: Exp.t, t: Typ.t, t': option(Typ.t)) => {
  IdTagged.FreshGrammar.Exp.(
    switch (t') {
    | Some({term: Unknown(Internal), _}) => d
    /* Settle the common case before resolving anything: equal types
       need no ascription, and comparing normalized forms expands every
       alias on both sides first. */
    | Some(ty) when Typ.fast_equal(ty, t) => d
    | Some(ty) when !Typ.equal_up_to_aliases(ctx, ty, t) => asc(d, ty)
    | _ => d
    }
  );
};

/* Fold patterns with expected modes using the provided analyzer callback. */
let fold_patterns_with_modes =
    (
      ~analyze:
         (
           ~ctx: Ctx.t,
           ~ana: Typ.t,
           ~duplicate_bindings: list(string),
           Pat.t,
           Map.t
         ) =>
         (Info.pat, Pat.t, Map.t),
      ~ctx: Ctx.t,
      ~duplicate_bindings=[],
      ps: list(Pat.t),
      modes,
      m,
    ) =>
  List.fold_left2(
    ((ctx, tys, cons, m, infos, elabs), p, ana) =>
      analyze(~ctx, ~ana, ~duplicate_bindings, p, m)
      |> (
        ((info, elab, m)) => (
          info.ctx,
          tys @ [info.ty],
          cons @ [info.constraint_],
          m,
          infos @ [info],
          elabs @ [elab],
        )
      ),
    (ctx, [], [], m, [], []),
    ps,
    modes,
  );

module type ExpressionStatics = {
  let uexp_to_info_map:
    (
      ~ctx: Ctx.t,
      ~ana: Typ.t=?,
      ~is_in_filter: bool=?,
      ~ancestors: Info.ancestors=?,
      Exp.t,
      Map.t
    ) =>
    (Info.exp, Exp.t, Map.t);
  let add:
    (
      ~user_term: Exp.t=?,
      ~elab_term: Exp.t,
      ~elab_syn_ty: Typ.t,
      ~marks: list(Mark.t)=?,
      ~warnings: list(Warning.list_item)=?,
      ~ctx: Ctx.t=?,
      ~ana: Typ.t=?,
      ~ancestors: Info.ancestors=?,
      ~co_ctx: CoCtx.t,
      ~probe_targets: SubexpProbeTargets.t=?,
      ~message: Message.t=?,
      ~label_inference: option(Info.label_inference(Info.exp))=?, // TODO[Matt]: combine with message
      ~inferred_label: option(string)=?,
      ~label_sort: bool=?,
      ~dot_labels: list(string)=?,
      Map.t
    ) =>
    (Info.exp, Exp.t, Map.t);
};
