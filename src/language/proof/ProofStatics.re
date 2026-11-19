open Util;

/* The theorems a theorem's proof can cite: those in scope at it, innermost
 * first, and which of them are captured, being about a variable the program
 * has rebound since (as `ProofCtx.of_theorem_ctx` reads a scope). The
 * builtin axioms are left out. */
[@deriving (show({with_path: false}), sexp, yojson)]
type namespace = {
  theorems: list(Ctx.theorem_entry),
  captured: list(Id.t),
};

let empty_namespace = {
  theorems: [],
  captured: [],
};

/* The namespace in the typing context `ctx` of a theorem. */
let namespace_of_ctx = (ctx: Ctx.t): namespace => {
  let (_, theorems, captured) =
    List.fold_left(
      ~f=
        ((rebound, theorems, captured), entry: Ctx.entry) =>
          switch (entry) {
          | VarEntry({name, _}) => ([name, ...rebound], theorems, captured)
          | TheoremEntry({prop: Some(prop), id, _} as theorem) => (
              rebound,
              [theorem, ...theorems],
              Exp.occurs_free_any(rebound, prop)
                ? [id, ...captured] : captured,
            )
          | TheoremEntry({prop: None, _})
          | ConstructorEntry(_)
          | TVarEntry(_)
          | LivelitEntry(_) => (rebound, theorems, captured)
          },
      ~init=([], [], []),
      ctx.entries,
    );
  {
    theorems: List.rev(theorems),
    captured,
  };
};

/* What the proof checker reads from the statics of the program it checks,
 * by id. The full statics map implements it, and so does the smaller map
 * the evaluator gets, so the checker runs wherever the evaluator does. */
type t = {
  /* The type of an expression or pattern, normalized: the checker has no
   * typing context to resolve an alias against. */
  typ: Id.t => option(Typ.t),
  /* The free variables of an expression. */
  co_ctx: Id.t => option(CoCtx.t),
  /* What a case pattern covers. */
  pat_constraint: Id.t => option(Coverage.Constraint.t),
  /* The namespace at a theorem's node. */
  namespace: Id.t => option(namespace),
};

let empty: t = {
  typ: _ => None,
  co_ctx: _ => None,
  pat_constraint: _ => None,
  namespace: _ => None,
};

let of_info_map = (m: StaticsBase.Map.t): t => {
  typ: id =>
    switch (StaticsBase.Map.lookup(id, m)) {
    | Some(InfoExp({ty, ctx, _}))
    | Some(InfoPat({ty, ctx, _})) => Some(Typ.normalize(ctx, ty))
    | Some(
        InfoDrv(_) | InfoTyp(_) | InfoTPat(_) | InfoMod(_) | InfoSig(_) |
        InfoMPat(_) |
        InfoProof(_) |
        Secondary(_),
      )
    | None => None
    },
  co_ctx: id =>
    StaticsBase.Map.lookup_exp(id, m)
    |> Option.map(~f=(info: Info.exp) => info.co_ctx),
  pat_constraint: id =>
    StaticsBase.Map.lookup_pat(id, m) |> Option.map(~f=Info.pat_constraint),
  namespace: id =>
    StaticsBase.Map.lookup_exp(id, m)
    |> Option.map(~f=(info: Info.exp) => namespace_of_ctx(info.ctx)),
};
