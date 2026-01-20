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

/* Returns the ID of the expression to auto-probe based on cursor position.
 * Used by auto-probe mode to determine what to probe when cursor moves.
 *
 * Examples (| = cursor):
 *   test let a = 1 in a pass
 *     |test ...        → probe test body (let a = 1 in a)
 *     test |let ...    → probe let def (1)
 *     test let |a ...  → probe let def (1)
 *     test let a = |1  → probe let def (1)
 *     ... in |a pass   → probe test body (test overrides let when in let body)
 *
 *   let x = 5 in x + 1
 *     |let ...         → probe def (5)
 *     let |x ...       → probe def (5)
 *     let x = |5 ...   → probe def (5)
 *     ... in |x + 1    → None (in body, no probe)
 *
 * Priority:
 * 1. Test forms: If cursor is ON or INSIDE a Test/HintedTest, probe the test body.
 *    The innermost test wins and overrides any let expressions.
 * 2. Let expressions: If cursor is ON the let (delimiter) or NOT in the body
 *    (i.e., in pattern or definition), probe the definition. */
let toplevel_def_body_id = (~statics: Map.t, ~id: Id.t): option(Id.t) => {
  let rec contains_id = (target: Id.t, ids: list(Id.t)): bool =>
    switch (ids) {
    | [] => false
    | [head, ...tail] => Id.equal(head, target) || contains_id(target, tail)
    };

  /* Check if an Info is a Test/HintedTest and return its body ID */
  let test_body_of = (info: Info.t): option(Id.t) =>
    switch (info) {
    | InfoExp({term: {term: Test(body), _}, _}) =>
      Some(IdTagged.rep_id(body))
    | InfoExp({term: {term: HintedTest(body, _), _}, _}) =>
      Some(IdTagged.rep_id(body))
    | _ => None
    };

  /* Check if an Info is a Let and return its def ID */
  let let_def_of = (info: Info.t): option(Id.t) =>
    switch (info) {
    | InfoExp({term: {term: Let(_, def, _), _}, _}) =>
      Some(IdTagged.rep_id(def))
    | _ => None
    };

  /* Find innermost Test/HintedTest in a list of IDs */
  let rec find_test = (ids: list(Id.t)): option(Id.t) =>
    switch (ids) {
    | [] => None
    | [current_id, ...rest] =>
      switch (Map.lookup(current_id, statics)) {
      | Some(info) =>
        switch (test_body_of(info)) {
        | Some(body_id) => Some(body_id)
        | None => find_test(rest)
        }
      | None => find_test(rest)
      }
    };

  /* Gather def IDs for enclosing lets where cursor is NOT in body.
   * This covers: cursor in pattern or in definition (not body). */
  let rec gather =
          (remaining: list(Id.t), seen: list(Id.t), acc: list(Id.t))
          : list(Id.t) =>
    switch (remaining) {
    | [] => acc
    | [current_id, ...rest] =>
      let acc' =
        switch (Map.lookup(current_id, statics)) {
        | Some(InfoExp({term: {term: Let(_, def, body), _}, _})) =>
          /* Probe def if cursor is NOT in body */
          let in_body = contains_id(IdTagged.rep_id(body), seen);
          in_body ? acc : [IdTagged.rep_id(def), ...acc];
        | _ => acc
        };
      gather(rest, [current_id, ...seen], acc');
    };

  switch (Map.lookup(id, statics)) {
  | Some(info) =>
    let ancestors: list(Id.t) = Info.ancestors_of(info);

    /* 1. Check if cursor is directly ON a Test → probe its body */
    switch (test_body_of(info)) {
    | Some(body_id) => Some(body_id)
    | None =>
      /* 2. Check if cursor is INSIDE a Test (ancestor) → probe its body */
      switch (find_test(ancestors)) {
      | Some(body_id) => Some(body_id)
      | None =>
        /* 3. Check if cursor is directly ON a Let → probe its def */
        switch (let_def_of(info)) {
        | Some(def_id) => Some(def_id)
        | None =>
          /* 4. Check ancestor Lets where cursor is not in body */
          let def_ids = gather(ancestors, [id], []);
          /* def_ids is ordered innermost-first, so last is the top-level def */
          Util.ListUtil.last_opt(def_ids);
        }
      }
    };
  | _ => None
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

module type ExpressionStatics = {
  let uexp_to_info_map:
    (
      ~ctx: Ctx.t,
      ~ana: Typ.t=?,
      ~is_in_filter: bool=?,
      ~ancestors: Info.ancestors=?,
      ~duplicates: list(string)=?,
      ~expected_labels: list(string)=?,
      ~inferred_label: string=?,
      ~override_self: Self.exp=?,
      ~label_sort: bool=?,
      Exp.t,
      Map.t
    ) =>
    (Info.exp, Map.t);
  let add':
    (
      ~label_inference: Info.label_inference(Info.exp)=?,
      ~self: Self.exp,
      ~co_ctx: CoCtx.t,
      Map.t
    ) =>
    (Info.exp, Map.t);
  let label_to_info_map:
    (option(list(string)), Typ.t, Exp.t, Map.t) =>
    (option(string), Info.exp, Map.t);
};
