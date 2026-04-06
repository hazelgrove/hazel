open Util.OptUtil.Syntax;
module Info = Info;

module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(Info.t);

  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type errors = Id.Map.t(Info.error);

  let empty = Id.Map.empty;
  let lookup = Id.Map.find_opt;
  let filter = Id.Map.filter;

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

  let errors = (map: t): list((Id.t, Info.error)) =>
    Id.Map.fold(
      (id, info: Info.t, acc) =>
        Option.to_list(Info.error_of(info) |> Option.map(x => (id, x)))
        @ acc,
      map,
      [],
    );

  let collect_errors = (map: t): errors =>
    Id.Map.filter_map(
      (_: Uuidm.t, info: Info.t) => {Info.error_of(info)},
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
    | Some(InfoPat({term, _})) => Pat.bindings(term)
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
        | InfoExp({term: {term: Let(pat, def, _), _}, _}) =>
          let binds = Pat.bindings(pat);
          List.exists((b: Binding.t) => b.id == binding_id, binds)
            ? Some(IdTagged.rep_id(def)) : climb(rest);
        | InfoExp(_) => None
        | _ => climb(rest)
        };
      };
    climb(Info.ancestors_of(ci_binder));
  };
};

let let_definition_path = (~statics: Map.t, ~id: Id.t): list(Pat.t) => {
  let rec contains_id = (target: Id.t, ids: list(Id.t)): bool =>
    switch (ids) {
    | [] => false
    | [head, ...tail] => Id.equal(head, target) || contains_id(target, tail)
    };

  let rec gather =
          (remaining: list(Id.t), seen: list(Id.t), acc: list(Pat.t))
          : list(Pat.t) =>
    switch (remaining) {
    | [] => acc
    | [current_id, ...rest] =>
      let acc' =
        switch (Map.lookup(current_id, statics)) {
        | Some(InfoExp({term: {term: Let(pat, def, _), _}, _})) =>
          contains_id(IdTagged.rep_id(def), seen) ? [pat, ...acc] : acc
        | _ => acc
        };
      gather(rest, [current_id, ...seen], acc');
    };

  switch (Map.lookup(id, statics)) {
  | Some(info) =>
    let ancestors: list(Id.t) = Info.ancestors_of(info);
    let collected: list(Pat.t) = gather(ancestors, [id], []);
    List.rev(collected);
  | _ => []
  };
};

let map_m = (f, xs, m: Map.t) =>
  List.fold_left(
    ((xs, m), x) => f(x, m) |> (((x, m)) => (xs @ [x], m)),
    ([], m),
    xs,
  );

let add_info = (ids: list(Id.t), info: Info.t, m: Map.t): Map.t =>
  ids |> List.fold_left((m, id) => Id.Map.add(id, info, m), m);

let add_missing_info = (ids: list(Id.t), info: Info.t, m: Map.t): Map.t =>
  ids
  |> List.fold_left(
       (m, id) =>
         switch (Id.Map.find_opt(id, m)) {
         | Some(_) => m
         | None => Id.Map.add(id, info, m)
         },
       m,
     );

let rec is_arrow_like = (t: Typ.t) => {
  switch (t |> Typ.term_of) {
  | Unknown(_) => true
  | Arrow(_) => true
  | Poly(_, t) => is_arrow_like(t)
  | _ => false
  };
};

let is_recursive = (ctx, p, def, syn: Typ.t) => {
  switch (Pat.get_num_of_vars(p), Exp.get_num_of_functions(def)) {
  | (Some(num_vars), Some(num_fns))
      when num_vars != 0 && num_vars == num_fns =>
    let norm = Typ.normalize(ctx, syn);
    switch (norm |> Typ.term_of) {
    | Prod(syns) when List.length(syns) == num_vars =>
      syns |> List.for_all(is_arrow_like)
    | _ when is_arrow_like(norm) => num_vars == 1
    | _ => false
    };
  | _ => false
  };
};

let syn = Unknown(SynSwitch) |> Typ.temp;

/* Add an ascription wrapper if the types differ after normalization. */
let fresh_ascription =
    (ctx: Ctx.t, d: Exp.t, t: Typ.t, t': option(Typ.t)) => {
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

/* Re-derive an exp info entry with a new self type.
   Preserves all other fields from the original info. */
let replace_exp_self =
    (m: Map.t, original_info: Info.exp, self: Self.exp): (Info.exp, Map.t) => {
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
  (new_info, add_info(IdTagged.ids(original_info.term), InfoExp(new_info), m));
};

/* Patch an expression info entry to set label_sort=true and fix self.
   For Label nodes, overrides UnexpectedLabelSort with Just(Label(name)). */
let patch_label_info = (m: Map.t, info: Info.exp): Map.t => {
  let self: Self.exp =
    switch (info.term.term) {
    | Label(name) => Common(Just(Label(name) |> Typ.temp))
    | _ => info.self
    };
  let patched =
    Info.derived_exp(
      ~uexp=info.term,
      ~ctx=info.ctx,
      ~ana=info.ana,
      ~ancestors=info.ancestors,
      ~self,
      ~co_ctx=info.co_ctx,
      ~label_inference=info.label_inference,
      ~inferred_label=info.inferred_label,
      ~label_sort=true,
      ~dot_labels=info.dot_labels,
    );
  add_info(IdTagged.ids(info.term), InfoExp(patched), m);
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

/* Build a list-pattern coverage constraint from element constraints. */
let list_constraint = (cons: list(Coverage.Constraint.t)): Coverage.Constraint.t =>
  List.fold_right(
    (hd, tl) => Coverage.Constraint.cons(hd, tl),
    cons,
    Coverage.Constraint.nil,
  );

/* Add redundant-row annotations to already analyzed pattern infos. */
let add_pattern_redundancy =
    (ps: list(TermBase.pat_t), redundant_rows: list(int), m: Map.t): Map.t =>
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
            ~label_inference=info.label_inference,
            ~inferred_label=info.inferred_label,
            ~label_sort=info.label_sort,
            ~constraint_=info.constraint_,
          );
        add_info(IdTagged.ids(p), InfoPat(info), m)
      | _ => failwith("Invalid sort for pattern.")
      };
    },
    m,
    redundant_rows,
  );

module type ExpressionStatics = {
  let uexp_to_info_map:
    (
      ~ctx: Ctx.t,
      ~ana: Typ.t=?,
      ~is_in_filter: bool=?,
      ~ancestors: Info.ancestors=?,
      ~override_self: Self.exp=?,
      Exp.t,
      Map.t
    ) =>
    (Info.exp, Exp.t, Map.t);
  let add':
    (
      ~elab: Exp.t=?,
      ~label_inference: Info.label_inference(Info.exp)=?,
      ~self: Self.exp,
      ~co_ctx: CoCtx.t,
      Map.t
    ) =>
    (Info.exp, Exp.t, Map.t);
  let label_to_info_map:
    (Typ.t, Exp.t, Map.t) => (option(string), Info.exp, Exp.t, Map.t);
};
