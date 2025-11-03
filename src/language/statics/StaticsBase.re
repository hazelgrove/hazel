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
  | Forall(_, t) => is_arrow_like(t)
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

let rec synth_kind = (ctx: Ctx.t, typ: Typ.t): Ctx.kind => {
  switch (typ.term) {
  | Var(name) =>
    switch (Ctx.lookup_alias(ctx, name)) {
    | Some(ty_alias) => synth_kind(ctx, ty_alias)
    | None =>
      switch (Ctx.lookup_tvar(ctx, name)) {
      | None => Ctx.Abstract
      | Some(kind) => kind
      }
    }
  | Ap(t1, t2) =>
    let k_fun = synth_kind(ctx, t1);
    let _ = synth_kind(ctx, t2); // check for well formedness, no live error check
    switch (k_fun) {
    | Ctx.Arr(_, cod) => cod
    | _ => Ctx.Abstract
    };
  | Arrow(_, _) => Ctx.Abstract
  | Prod(_) => Ctx.Abstract
  | Forall(_, body) =>
    let _ = synth_kind(ctx, body);
    Ctx.Abstract;
  | Rec(_, body) =>
    let _ = synth_kind(ctx, body);
    Ctx.Abstract;
  | Atom(_)
  | List(_)
  | Sum(_)
  | Label(_)
  | TupLabel(_, _)
  | Parens(_) => Ctx.Abstract
  | Unknown(_) => Ctx.Abstract
  | ExplicitNonlabel => Ctx.Abstract
  | ProdProjection(_, _) => Ctx.Abstract
  | ProdExtension(_, _) => Ctx.Abstract
  };
};

// Helper for counting params for dfferentiting kind instantiation
let rec count_forall_params = (typ: Typ.t): (int, Typ.t) =>
  switch (typ.term) {
  | Forall(_, body) =>
    let (count, final_body) = count_forall_params(body);
    (count + 1, final_body);
  | _ => (0, typ)
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
