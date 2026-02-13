/* ExpandModule.re - Transform module syntax into nested let/type + labeled tuple

      Transforms:
        { let a = 1; let b = 2 }
      Into:
        let a = 1 in let b = 2 in (a=a, b=b)

   When an ana type is available (e.g., from a type annotation on the enclosing
   let-binding), the expansion threads expected types into ModLet patterns as
   type ascriptions. This enables proper error attribution: type errors appear
   on the definition expression (which has a surface ID) rather than on synthetic
   tuple references (which have fresh IDs invisible to the editor).

      { let x = true } with ana=(x=Int)
      becomes:
        let (x : Int) = true in (x=x)
      so the error appears on `true`, not on an invisible synthetic node.
   */

/* Collect variable names bound by a pattern */
let bound_vars_of_pat = (pat: Pat.t): list(Var.t) => Pat.bound_vars(pat);

/* Collect type variable names bound by a type pattern */
let bound_vars_of_tpat = (tpat: TPat.t): list(Var.t) =>
  switch (tpat.term) {
  | Var(name) => [name]
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => []
  };

/* Convert an MPat to a Pat, preserving the MPat's ID.
   This ensures the same Pat ID is used in both expansion and binding computation. */
let rec mpat_to_pat = (mp: MPat.t): Pat.t =>
  switch (mp.term) {
  | Var(name) => IdTagged.fast_copy(MPat.rep_id(mp), Pat.fresh(Var(name)))
  | Asc(inner, typ) =>
    /* Desugar Sig types to Prod so the evaluator can handle them.
       e.g. { let x : Int } becomes (x=Int) */
    let typ = Typ.desugar_sig(Ctx.empty, typ);
    IdTagged.fast_copy(
      MPat.rep_id(mp),
      Pat.fresh(Asc(mpat_to_pat(inner), typ)),
    );
  | _ => IdTagged.fast_copy(MPat.rep_id(mp), Pat.fresh(Wild))
  };

/* Extract the variable name from an MPat, looking through Asc wrappers */
let rec mpat_names = (mp: MPat.t): list(Var.t) =>
  switch (mp.term) {
  | Var(name) => [name]
  | Asc(inner, _) => mpat_names(inner)
  | _ => []
  };

/* Collect all variable names that will be bound by later items in the module.
   Used to determine which bindings are shadowed. */
let rec collect_later_names = (items: list(Mod.t)): list(Var.t) =>
  switch (items) {
  | [] => []
  | [item, ...rest] =>
    let names =
      switch (item.term) {
      | ModLet(pat, _) => bound_vars_of_pat(pat)
      | ModuleMod(mp, _) => mpat_names(mp)
      | ModType(_, _) => [] /* Type bindings don't shadow value bindings */
      | ModExp(_) => []
      | Invalid(_)
      | EmptyHole
      | MultiHole(_) => []
      };
    names @ collect_later_names(rest);
  };

/* Compute the non-shadowed bindings: variable names and their patterns.
   Only the final binding for each name appears in the module's exported tuple. */
let compute_non_shadowed_bindings =
    (items: list(Mod.t)): list((Var.t, Pat.t)) => {
  let rec go = (items: list(Mod.t)): list((Var.t, Pat.t)) =>
    switch (items) {
    | [] => []
    | [item, ...rest] =>
      switch (item.term) {
      | ModLet(pat, _) =>
        let names = bound_vars_of_pat(pat);
        let later_names = collect_later_names(rest);
        /* Keep only names not shadowed by later items */
        let non_shadowed =
          names |> List.filter(n => !List.mem(n, later_names));
        let entries = non_shadowed |> List.map(n => (n, pat));
        entries @ go(rest);
      | ModuleMod(mp, _) =>
        let names = mpat_names(mp);
        let later_names = collect_later_names(rest);
        let non_shadowed =
          names |> List.filter(n => !List.mem(n, later_names));
        let pat = mpat_to_pat(mp);
        let entries = non_shadowed |> List.map(n => (n, pat));
        entries @ go(rest);
      | ModType(_, _)
      | ModExp(_)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_) => go(rest)
      }
    };
  go(items);
};

/* Build a labeled tuple expression: (a=a, b=b, ...)
   The tuple gets a fresh ID (NOT the Module's ID) because the Module's ID
   is already used by Statics to store info for the Module itself. Using the
   same ID would cause the statics map entry to be overwritten, leading to
   infinite loops in the Elaborator. */
let build_labeled_tuple = (bindings: list((Var.t, Pat.t))): Exp.t => {
  let fields =
    bindings
    |> List.map(((name, _pat)) => {
         let label = Exp.fresh(Label(name));
         let value = Exp.fresh(Var(name));
         Exp.fresh(TupLabel(label, value));
       });

  switch (fields) {
  | [] => Exp.fresh(Tuple([]))
  | _ => Exp.fresh(Tuple(fields))
  };
};

/* Extract label->type mappings from an ana type, if it's a labeled tuple.
   Strips Parens wrappers to handle annotated types like ((x=Int, y=Bool)). */
let extract_ana_labels = (ana: Typ.t): list((Var.t, Typ.t)) => {
  let rec strip_parens = (ty: Typ.t): Typ.t =>
    switch (ty.term) {
    | Parens(t) => strip_parens(t)
    | _ => ty
    };
  switch (strip_parens(ana).term) {
  | Prod(fields) =>
    fields
    |> List.filter_map((field: Typ.t) =>
         switch (field.term) {
         | TupLabel({term: Label(name), _}, typ) => Some((name, typ))
         | _ => None
         }
       )
  | _ => []
  };
};

/* Wrap the body with a single module item.
   ModLet becomes Let, ModType becomes TyAlias, ModExp becomes let _ = e in body.

   ID preservation:
   - ModLet/ModType: Preserve the Mod item's ID on the wrapper Let/TyAlias,
     since these correspond to surface syntax tiles users can click on.
   - ModExp: Use fresh ID. ModExp is a synthetic wrapper around an existing
     expression - the inner expression already has its own IDs for cursor info.

   Type-directed annotation:
   When ana_labels is non-empty and the pattern is a simple Var matching a label,
   wraps the pattern with Asc(pat, expected_type). This causes the Let statics to
   analyze the definition against the expected type, producing errors on the
   definition expression (which has surface IDs) rather than on synthetic nodes. */
let wrap_item =
    (~ana_labels: list((Var.t, Typ.t)), item: Mod.t, body: Exp.t): Exp.t => {
  switch (item.term) {
  | ModLet(pat, def) =>
    let item_id = Mod.rep_id(item);
    let pat =
      switch (pat.term) {
      | Var(name) =>
        switch (List.assoc_opt(name, ana_labels)) {
        | Some(expected_type) => Pat.fresh(Asc(pat, expected_type))
        | None => pat
        }
      | _ => pat
      };
    IdTagged.fast_copy(item_id, Exp.fresh(Let(pat, def, body)));
  | ModType(tpat, typ) =>
    let item_id = Mod.rep_id(item);
    IdTagged.fast_copy(item_id, Exp.fresh(TyAlias(tpat, typ, body)));
  | ModExp(e) =>
    /* Bare expression: fresh ID since ModExp is synthetic.
       The inner expression e keeps its original IDs. */
    let wild_pat = Pat.fresh(Wild);
    Exp.fresh(Let(wild_pat, e, body));
  | ModuleMod(mp, def) =>
    let item_id = Mod.rep_id(item);
    let pat = mpat_to_pat(mp);
    IdTagged.fast_copy(item_id, Exp.fresh(Let(pat, def, body)));
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) =>
    /* Error cases - just return the body, effectively ignoring the item */
    body
  };
};

/* Main expansion function: transform a module into a nested let/type expression.

      { let a = 1; let b = 2 }
      becomes:
      let a = 1 in let b = 2 in (a=a, b=b)

   When ~ana is provided and is a labeled tuple type, the expansion adds type
   annotations to matching ModLet patterns for proper error attribution.

   Note: The wrapper Let/TyAlias expressions preserve Mod item IDs for cursor
   inspector support. The final tuple gets a fresh ID (not the Module's ID)
   because the Module's ID is used by Statics to store the Module's own info.
   */
let expand = (~ana: option(Typ.t)=?, items: list(Mod.t)): Exp.t => {
  let ana_labels =
    switch (ana) {
    | Some(ana) => extract_ana_labels(ana)
    | None => []
    };

  /* 1. Compute non-shadowed bindings for the final tuple */
  let non_shadowed = compute_non_shadowed_bindings(items);

  /* 2. Build the labeled tuple body (fresh ID) */
  let tuple_body = build_labeled_tuple(non_shadowed);

  /* 3. Wrap with definitions from bottom to top (fold_right preserves order) */
  List.fold_right(wrap_item(~ana_labels), items, tuple_body);
};
