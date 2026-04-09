open Util.OptUtil.Syntax;
module Info = Info;

module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(Info.t);

  [@deriving show({with_path: false})]
  type errors = Id.Map.t(list(Mark.t));

  let equal_errors: (errors, errors) => bool =
    Id.Map.equal((a: list(Mark.t), b: list(Mark.t)) => a == b);

  let show_errors = (m: errors): string =>
    Id.Map.bindings(m)
    |> List.sort((a, b) => Id.compare(fst(a), fst(b)))
    |> List.map(((id, marks)) =>
         Id.show(id) ++ " => " ++ [%derive.show: list(Mark.t)](marks)
       )
    |> String.concat("\n");

  let empty = Id.Map.empty;
  let lookup = Id.Map.find_opt;
  let filter = Id.Map.filter;
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

  let errors = (map: t): list((Id.t, list(Mark.t))) =>
    Id.Map.fold(
      (id, info: Info.t, acc) =>
        switch (Info.marks_of(info)) {
        | [] => acc
        | ms => [(id, ms), ...acc]
        },
      map,
      [],
    );

  let collect_errors = (map: t): errors =>
    Id.Map.filter_map(
      (_: Uuidm.t, info: Info.t) =>
        switch (Info.marks_of(info)) {
        | [] => None
        | ms => Some(ms)
        },
      map,
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

  let let_definition_path = (~statics: t, ~id: Id.t): list(Pat.t) => {
    let rec contains_id = (target: Id.t, ids: list(Id.t)): bool =>
      switch (ids) {
      | [] => false
      | [head, ...tail] =>
        Id.equal(head, target) || contains_id(target, tail)
      };

    let rec gather =
            (remaining: list(Id.t), seen: list(Id.t), acc: list(Pat.t))
            : list(Pat.t) =>
      switch (remaining) {
      | [] => acc
      | [current_id, ...rest] =>
        let acc' =
          switch (lookup(current_id, statics)) {
          | Some(InfoExp({user_term: {term: Let(pat, def, _), _}, _})) =>
            contains_id(IdTagged.rep_id(def), seen) ? [pat, ...acc] : acc
          | _ => acc
          };
        gather(rest, [current_id, ...seen], acc');
      };

    switch (lookup(id, statics)) {
    | Some(info) =>
      let ancestors: list(Id.t) = Info.ancestors_of(info);
      let collected: list(Pat.t) = gather(ancestors, [id], []);
      List.rev(collected);
    | _ => []
    };
  };
};

let set_marks_exp = (m: Map.t, e: Exp.t, marks: list(Mark.t)): Map.t =>
  switch (Map.lookup(Exp.rep_id(e), m)) {
  | Some(Info.InfoExp(info)) =>
    Map.add_info(
      IdTagged.ids(info.user_term),
      InfoExp({...info, marks}),
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
   statics synthetic type (Typ.meet). On meet failure, prefer syn under
   synthesis and ana under analysis. */
let fixed_typ = (ctx: Ctx.t, ana: Typ.t, syn_ty: Typ.t): Typ.t =>
  switch (Typ.meet(ctx, ana, syn_ty)) {
  | Some(ty) => ty
  | None =>
    if (Typ.is_syn_plus(ana)) {
      syn_ty;
    } else {
      ana;
    }
  };

let patch_syn_ty_exp = (m: Map.t, e: Exp.t, new_syn_ty: Typ.t): Map.t =>
  switch (Map.lookup(Exp.rep_id(e), m)) {
  | Some(Info.InfoExp(info)) =>
    Map.add_info(
      IdTagged.ids(info.user_term),
      InfoExp({
        ...info,
        syn_ty: new_syn_ty,
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

let should_emit_nomeet_mark = (ctx: Ctx.t, ana: Typ.t, syn_ty: Typ.t): bool =>
  switch (Typ.meet(ctx, ana_skip_explicit_nonlabel(ana), syn_ty)) {
  | Some(_) => false
  | None => true
  };

let syn_ana_ok_common =
    (ctx: Ctx.t, ty_ana: Typ.t, syn_ty: Typ.t): Message.ok_common => {
  let ana = ana_skip_explicit_nonlabel(ty_ana);
  switch (ana.term) {
  | Unknown(SynSwitch) => Message.Syn(syn_ty)
  | _ =>
    switch (Typ.meet(ctx, ana, syn_ty)) {
    | None => Message.Syn(syn_ty)
    | Some(meet) =>
      Message.Ana(
        Message.Consistent({
          ana,
          syn: syn_ty,
          meet,
        }),
      )
    }
  };
};

let expectation_mismatch_mark =
    (ctx: Ctx.t, ana: Typ.t, syn_ty: Typ.t): option(Mark.t) => {
  let ana' = ana_skip_explicit_nonlabel(ana);
  switch (ana'.term) {
  | Unknown(SynSwitch) => None
  | _ =>
    switch (Typ.meet(ctx, ana', syn_ty)) {
    | Some(_) => None
    | None =>
      Some(
        Mark.ExpectationMismatch({
          ana: ana',
          syn: syn_ty,
        }),
      )
    }
  };
};

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
    | Some(ty)
        when !Typ.fast_equal(Typ.normalize(ctx, ty), Typ.normalize(ctx, t)) =>
      asc(d, ty)
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
      ~syn_ty: Typ.t,
      ~marks: list(Mark.t)=?,
      ~warnings: list(Warning.list_item)=?,
      ~ctx: Ctx.t=?,
      ~ana: Typ.t=?,
      ~ancestors: Info.ancestors=?,
      ~co_ctx: CoCtx.t,
      ~message: Message.t=?,
      ~label_inference: option(Info.label_inference(Info.exp))=?, // TODO[Matt]: combine with message
      ~inferred_label: option(string)=?,
      ~label_sort: bool=?,
      ~dot_labels: list(string)=?,
      Map.t
    ) =>
    (Info.exp, Exp.t, Map.t);
};
