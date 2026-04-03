/* Module-related helper functions used during statics/elaboration.
   These operate on expression and pattern ASTs to handle module
   type references, signature pattern stripping, and body ID restoration. */

/* Collect module variable references from type annotations.
   For M.T (= ProdProjection(Var("M"), Label("T"))), free_vars returns ["M"].
   We filter to names that are expression variables (not type aliases). */
let collect_module_refs_in_typ = (ctx: Ctx.t, id: Id.t, typ: Typ.t): CoCtx.t => {
  Typ.free_vars(typ)
  |> List.filter_map(name =>
       switch (Ctx.lookup_var(ctx, name)) {
       | Some(_) =>
         Some(CoCtx.singleton(name, id, Unknown(Internal) |> Typ.temp))
       | None => None
       }
     )
  |> CoCtx.union;
};

/* Walk a Pat.t AST and collect module refs from any Asc(_, ann) sub-patterns. */
let rec collect_pat_type_refs = (ctx: Ctx.t, pat: Pat.t): CoCtx.t =>
  switch (pat.term) {
  | Asc(p, ann) =>
    CoCtx.union([
      collect_module_refs_in_typ(ctx, Typ.rep_id(ann), ann),
      collect_pat_type_refs(ctx, p),
    ])
  | Tuple(ps)
  | ListLit(ps) => CoCtx.union(List.map(collect_pat_type_refs(ctx), ps))
  | Cons(p1, p2)
  | TupLabel(p1, p2)
  | Ap(p1, p2) =>
    CoCtx.union([
      collect_pat_type_refs(ctx, p1),
      collect_pat_type_refs(ctx, p2),
    ])
  | Parens(p)
  | Projector(_, p) => collect_pat_type_refs(ctx, p)
  | _ => CoCtx.empty
  };

/* Recursively strip ascription annotations from module patterns
   in let/tyalias expressions. */
let rec strip_module_sig_pats = (exp: Exp.t): Exp.t => {
  let (term, rewrap) = Exp.unwrap(exp);
  switch (term) {
  | Let({term: Asc(p, _), _}, def, body) =>
    Let(
      strip_module_sig_pats_in_pat(p),
      strip_module_sig_pats(def),
      strip_module_sig_pats(body),
    )
    |> rewrap
  | Let(p, def, body) =>
    Let(
      strip_module_sig_pats_in_pat(p),
      strip_module_sig_pats(def),
      strip_module_sig_pats(body),
    )
    |> rewrap
  | TyAlias(tpat, typ, body) =>
    TyAlias(tpat, typ, strip_module_sig_pats(body)) |> rewrap
  | Parens(inner) => Parens(strip_module_sig_pats(inner)) |> rewrap
  | _ => exp
  };
}
and strip_module_sig_pats_in_pat = (pat: Pat.t): Pat.t => {
  let (term, rewrap) = Pat.unwrap(pat);
  switch (term) {
  | Asc(inner, _) => strip_module_sig_pats_in_pat(inner)
  | Parens(inner) => Parens(strip_module_sig_pats_in_pat(inner)) |> rewrap
  | _ => pat
  };
};

/* Restores a module body's ID when it's been lost during tuple elaboration.
   Walks nested Let/TyAlias/Parens until a Tuple is found, then copies the ID. */
let rec restore_module_body_id = (~id, exp: Exp.t): Exp.t => {
  let (term, rewrap) = Exp.unwrap(exp);
  switch (term) {
  | Let(p, def, body) =>
    Let(p, def, restore_module_body_id(~id, body)) |> rewrap
  | TyAlias(tpat, typ, body) =>
    TyAlias(tpat, typ, restore_module_body_id(~id, body)) |> rewrap
  | Parens(inner) => Parens(restore_module_body_id(~id, inner)) |> rewrap
  | Tuple(_) => IdTagged.fast_copy(id, exp)
  | _ => exp
  };
};
