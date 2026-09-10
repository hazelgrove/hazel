---
schemaVersion: 1
prNumber: 1988
prOwner: hazelgrove
prRepo: hazel
baseSha: f625cf4bf21474456f0acf9ab6a5186472c16882
headSha: b3f9d69d84d1531a34e8554f4241a1368c69ed3c
---

# Live typing

This PR adds **live typing**: a toggleable feature that enriches static type feedback with the *dynamic* types actually observed while running the program. The editor evaluates the program, probes every expression whose static type still contains unknowns, and then re-runs statics with the collected runtime samples — so a `?` that only ever held `Int` values is displayed as `Int`, and inconsistencies that only manifest at runtime surface as a new class of "live typing" errors (decorated distinctly and listed in their own problem-panel category). The feature integrates with the existing probe pinning system, so pinning a specific function application scopes which samples feed the refinement.

The tour walks the data flow in order: the shared data model and type-lattice operations, the statics side that consumes runtime data, the dynamics side that records it, probe-target selection, the web layer that orchestrates the second statics pass, the UI surfaces, and tests. A large trailing wave of mechanical churn (a `Grammar.fn` signature cleanup and model-field plumbing) is grouped at the end.

## The LiveTyping data model and type lattice

`LiveTyping.re` is a new bridge module. Statics cannot depend on Dynamics (that would be a dependency cycle), so this file defines the dynamics-shaped data statics consumes: per-expression runtime samples (`exp_probes`) and per-type-pattern instantiations (`type_inst_probes`). `refine_typ_with_dynamics` only activates when a type still contains unknowns — it types each recorded sample and **meets** the results with the static type, so dynamic information can only make a type more precise. `extend_ctx_with_instantiations` does the opposite for type variables: it **joins** all observed instantiations of a `typfun` parameter, since multiple different instantiations mean the only safe common type is wider.

<details open>
<summary><code>src/language/statics/LiveTyping.re</code> · New module: samples + type instantiations consumable by Statics, and the refine/extend helpers</summary>

<!-- changetour:hunk file="src/language/statics/LiveTyping.re" baseBlob="771effc483f24044945d253611b3f9de033dda33" -->

```diff
@@ -0,0 +1,91 @@
+open Util;
+
+/* This file exists to define data structures that statics uses during the live typing phase.
+ * Importantly Statics cannot depend on Dynamics due to recursive dependencies, so we define these types here
+ * in a way that both Statics can depend on and can be constructed from Dynamics.
+ */
+
+[@deriving (show({with_path: false}), sexp, yojson)]
+type sample = {exp: Exp.t};
+
+/* A type instantiation records when a type variable is instantiated
+ * with a concrete type during type application evaluation */
+[@deriving (show({with_path: false}), sexp, yojson)]
+type type_instantiation = {
+  tpat_id: Id.t,
+  type_var: string,
+  instantiated_type: Typ.t,
+};
+
+module Map = {
+  [@deriving (show({with_path: false}), sexp, yojson)]
+  type entry = list(sample);
+
+  [@deriving (show({with_path: false}), sexp, yojson)]
+  type type_inst_entry = list(type_instantiation);
+
+  [@deriving (show({with_path: false}), sexp, yojson)]
+  type t = {
+    exp_probes: Id.Map.t(entry),
+    type_inst_probes: Id.Map.t(type_inst_entry),
+  };
+
+  let empty = {
+    exp_probes: Id.Map.empty,
+    type_inst_probes: Id.Map.empty,
+  };
+
+  let mk = (exp_probes, type_inst_probes): t => {
+    exp_probes,
+    type_inst_probes,
+  };
+
+  let lookup = (id, map) => Id.Map.find_opt(id, map.exp_probes);
+  let lookup_type_inst = (id, map) =>
+    Id.Map.find_opt(id, map.type_inst_probes);
+};
+
+/* Refine a synthesized/elaborated type using runtime samples gathered
+   through the LiveTyping.Map. Used by Statics to refine types of
+   expressions/patterns whose elaborated synthesis still contains unknowns. */
+let refine_typ_with_dynamics =
+    (
+      ~dynamics: Map.t,
+      ~calculate_dynamic_type: Exp.t => option(Typ.t),
+      ~ctx,
+      ~term_id: Id.t,
+      ty: Typ.t,
+    )
+    : Typ.t =>
+  if (Typ.count_unknowns(ty) > 0) {
+    switch (Map.lookup(term_id, dynamics)) {
+    | None
+    | Some([]) => ty
+    | Some(entry) =>
+      let exps = List.map((s: sample) => s.exp, entry);
+      let dyn_typs = OptUtil.traverse(calculate_dynamic_type, exps);
+      let dyn_typ = Option.bind(dyn_typs, Typ.meet_all(~empty=ty, ctx));
+      switch (dyn_typ) {
+      | None => ty
+      | Some(t) => t
+      };
+    };
+  } else {
+    ty;
+  };
+
+/* Helper to extend Ctx with type instantiations from probes */
+let extend_ctx_with_instantiations =
+    (ctx: Ctx.t, name, tpat_id: Id.t, insts: list(type_instantiation)): Ctx.t => {
+  let new_ty =
+    Typ.join_all(ctx, List.map(inst => inst.instantiated_type, insts))
+    |> Option.value(~default=Unknown(Internal) |> Typ.temp);
+  Ctx.extend_tvar(
+    ctx,
+    {
+      name,
+      id: tpat_id,
+      kind: Singleton(new_ty),
+    },
+  );
+};
```

</details>

The refinement needs lattice machinery `Typ` didn't have. `contains_unknown` was `count_unknowns(ty) > 0`; it becomes a dedicated short-circuiting traversal since it's now called on every expression's type during the live pass. The big addition is `Typ.join` — the dual of `meet`, returning the *least precise* common type (Unknown dominates: `join(?, Int) = ?`) — plus `join_all`, backed by a new `ListUtil.reduce` that handles the empty-list case.

<details>
<summary><code>src/language/term/Typ.re</code> · Remove the count-based <code>contains_unknown</code> shortcut</summary>

<!-- changetour:hunk file="src/language/term/Typ.re" baseBlob="b49aa0949f41a5ab6752a896d1c98024440c858a" -->

```diff
@@ -444,8 +444,6 @@ let rec count_unknowns = (ty: t): int =>
   | Sig(_) => 0
   };
 
-let contains_unknown = (ty: t): bool => count_unknowns(ty) > 0;
-
 let rec contains_sum_or_var = (ty: t): bool =>
   switch (ty.term) {
   | Atom(_)
```

</details>

<details open>
<summary><code>src/language/term/Typ.re</code> · Recursive, short-circuiting <code>contains_unknown</code></summary>

<!-- changetour:hunk file="src/language/term/Typ.re" baseBlob="b49aa0949f41a5ab6752a896d1c98024440c858a" -->

```diff
@@ -470,6 +468,37 @@ let rec contains_sum_or_var = (ty: t): bool =>
   | Sig(_) => false
   };
 
+let rec contains_unknown = (ty: t): bool =>
+  switch (ty.term) {
+  | Unknown(_) => true
+  | Atom(_)
+  | DrvQuoteTy(_)
+  | Var(_) => false
+  | Arrow(t1, t2) => contains_unknown(t1) || contains_unknown(t2)
+  | Prod(tys) => List.exists(contains_unknown, tys)
+  | Sum(sm) =>
+    List.exists(
+      fun
+      | ConstructorMap.BadEntry(_) => false
+      | Variant(_, _, ty) =>
+        Option.map(contains_unknown, ty) |> Option.value(~default=false),
+      sm,
+    )
+  | Rec(_, ty) => contains_unknown(ty)
+  | List(ty) => contains_unknown(ty)
+  | Parens(ty) => contains_unknown(ty)
+  | Projector(_, ty) => contains_unknown(ty)
+  | Poly(_, ty) => contains_unknown(ty)
+  | ProofOf(_) => false
+  | ProdProjection(ty1, _) => contains_unknown(ty1)
+  | ProdExtension(ty1, ty2) =>
+    contains_unknown(ty1) || contains_unknown(ty2)
+  | ExplicitNonlabel
+  | Label(_) => false
+  | TupLabel(_, ty) => contains_unknown(ty)
+  | Sig(_) => false
+  };
+
 /* Capture-avoiding substitution of `s` for `x` in `ty`.
 
    When recursing under a type binder `Poly(tp2, body)` or `Rec(tp2, body)`
```

</details>

<details open>
<summary><code>src/language/term/Typ.re</code> · New lattice <code>join</code> (dual of meet) and <code>join_all</code></summary>

<!-- changetour:hunk file="src/language/term/Typ.re" baseBlob="b49aa0949f41a5ab6752a896d1c98024440c858a" -->

```diff
@@ -996,6 +1025,95 @@ let meet_all = (~empty: t, ctx: Ctx.t, ts: list(t)): option(t) =>
 let is_consistent = (ctx: Ctx.t, ty1: t, ty2: t): bool =>
   meet(ctx, ty1, ty2) != None;
 
+/* Lattice join on types — returns the LEAST precise (widest) type that
+   is at least as imprecise as both inputs. Unknown dominates:
+   join(Unknown, Int) = Unknown. This is the dual of meet. */
+let rec join = (ctx: Ctx.t, ty1: t, ty2: t): t => {
+  let join' = join(ctx);
+  switch (term_of(ty1), term_of(ty2)) {
+  | (_, Parens(ty2)) => join'(ty1, ty2)
+  | (Parens(ty1), _) => join'(ty1, ty2)
+  | (_, Projector(_, ty2)) => join'(ty1, ty2)
+  | (Projector(_, ty1), _) => join'(ty1, ty2)
+  | (TupLabel({term: ExplicitNonlabel, _}, ty1'), _) => join'(ty1', ty2)
+  | (_, TupLabel({term: ExplicitNonlabel, _}, ty2')) => join'(ty1, ty2')
+  | (Unknown(p1), Unknown(p2)) =>
+    Unknown(meet_type_provenance(p1, p2)) |> temp
+  | (Unknown(_), _) => ty1
+  | (_, Unknown(_)) => ty2
+  | (Var(n1), Var(n2)) when n1 == n2 => ty1
+  | (Var(name), _) =>
+    switch (Ctx.lookup_alias(ctx, name)) {
+    | Some(ty_name) => join'(ty_name, ty2)
+    | None => Unknown(Internal) |> temp
+    }
+  | (_, Var(name)) =>
+    switch (Ctx.lookup_alias(ctx, name)) {
+    | Some(ty_name) => join'(ty_name, ty1)
+    | None => Unknown(Internal) |> temp
+    }
+  | (ProdProjection(_), _) => join'(weak_head_normalize(ctx, ty1), ty2)
+  | (_, ProdProjection(_)) => join'(ty1, weak_head_normalize(ctx, ty2))
+  | (ProdExtension(_), _) => join'(weak_head_normalize(ctx, ty1), ty2)
+  | (_, ProdExtension(_)) => join'(ty1, weak_head_normalize(ctx, ty2))
+  | (Rec(tp1, ty1), Rec(tp2, ty2)) =>
+    let ctx = Ctx.extend_dummy_tvar(ctx, tp1);
+    let ty1' =
+      switch (TPat.tyvar_of_utpat(tp2)) {
+      | Some(x2) => subst(Var(x2) |> temp, tp1, ty1)
+      | None => ty1
+      };
+    let ty_body = join(ctx, ty1', ty2);
+    Rec(tp1, ty_body) |> temp;
+  | (Rec(_), _) => Unknown(Internal) |> temp
+  | (Poly(x1, ty1), Poly(x2, ty2)) =>
+    let ty1' =
+      switch (TPat.tyvar_of_utpat(x2)) {
+      | Some(x2) => subst(Var(x2) |> temp, x1, ty1)
+      | None => ty1
+      };
+    let ctx = Ctx.extend_dummy_tvar(ctx, x2);
+    let ty_body = join(ctx, ty1', ty2);
+    Poly(x2, ty_body) |> temp;
+  | (Poly(_), _) => Unknown(Internal) |> temp
+  | (Atom(c1), Atom(c2)) when c1 == c2 => ty1
+  | (Atom(_), _) => Unknown(Internal) |> temp
+  | (Label(_), Label("")) => ty1
+  | (Label(""), Label(_)) => ty2
+  | (Label(name1), Label(name2))
+      when LabeledTuple.match_labels(name1, name2) => ty1
+  | (Label(_), _) => Unknown(Internal) |> temp
+  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
+    Arrow(join'(ty1, ty1'), join'(ty2, ty2')) |> temp
+  | (Arrow(_), _) => Unknown(Internal) |> temp
+  | (TupLabel(lab1, ty1'), TupLabel(lab2, ty2')) =>
+    TupLabel(join'(lab1, lab2), join'(ty1', ty2')) |> temp
+  | (TupLabel(_), _) => Unknown(Internal) |> temp
+  | (Prod(tys1), Prod(tys2)) =>
+    if (List.length(tys1) != List.length(tys2)) {
+      Unknown(Internal) |> temp;
+    } else {
+      Prod(List.map2(join', tys1, tys2)) |> temp;
+    }
+  | (Prod(_), _) => Unknown(Internal) |> temp
+  | (ProofOf(e1), ProofOf(e2)) =>
+    Equality.semantic.exp(e1, e2) ? ty1 : Unknown(Internal) |> temp
+  | (ProofOf(_), _) => Unknown(Internal) |> temp
+  | (Sum(sm1), Sum(sm2)) when ConstructorMap.equal(fast_equal, sm1, sm2) =>
+    Sum(sm1) |> temp
+  | (Sum(_), _) => Unknown(Internal) |> temp
+  | (List(ty1), List(ty2)) => List(join'(ty1, ty2)) |> temp
+  | (List(_), _) => Unknown(Internal) |> temp
+  | (ExplicitNonlabel, _) => Unknown(Internal) |> temp
+  | (Sig(_), _) => Unknown(Internal) |> temp
+  | (DrvQuoteTy(s1), DrvQuoteTy(s2)) when s1 == s2 => ty1
+  | (DrvQuoteTy(_), _) => Unknown(Internal) |> temp
+  };
+};
+
+let join_all = (ctx: Ctx.t, ts: list(t)): option(t) =>
+  ListUtil.reduce((acc, ty) => join(ctx, acc, ty), ts);
+
 /**
    * Determines if one type (`ty1`) is more precise than another type (`ty2`) within a given context (`ctx`).
    *
```

</details>

<details>
<summary><code>src/util/ListUtil.re</code> · <code>reduce</code>: fold without an initial accumulator, None on empty</summary>

<!-- changetour:hunk file="src/util/ListUtil.re" baseBlob="61c6d73f29fccbefcf9baddc6d90ae36798ca2df" -->

```diff
@@ -583,6 +583,21 @@ let rec forall2_opt =
   };
 };
 
+/**
+ * Reduces a list of elements using a binary function, returning an option.
+ * This is similar to fold_left but explicitly handles empty lists by returning None
+ * rather than requiring an initial accumulator value.
+ *
+ * @param f The binary combining function
+ * @param xs The list of elements to combine
+ * @return Some of the accumulated result if xs is non-empty, None if xs is empty
+ */
+let reduce = (f: ('a, 'a) => 'a, xs: list('a)): option('a) =>
+  switch (xs) {
+  | [] => None
+  | [x, ...xs] => Some(List.fold_left((acc, x) => f(acc, x), x, xs))
+  };
+
 let assoc_opt_by = (eq, key, assoc) => {
   let rec find = lst =>
     switch (lst) {
```

</details>

## Statics: refining types with runtime samples

`Statics.uexp_to_info_map` gains a `~dynamics: LiveTyping.Map.t` parameter, threaded through every recursive call. The actual refinement happens in `add`: each expression's elaborated synthesized type (`elab_syn_ty`) passes through `refine_typ_with_dynamics`, where `calculate_dynamic_type` types a runtime sample by recursively invoking statics on it (with empty dynamics, so this can't loop). Because the refined type then feeds the same `expectation_mismatch_mark` check as before, a dynamic-only inconsistency produces an ordinary mark — that's what becomes a live typing error downstream.

<details open>
<summary><code>src/language/statics/Statics.re</code> · <code>uexp_to_info_map</code> takes <code>~dynamics</code></summary>

<!-- changetour:hunk file="src/language/statics/Statics.re" baseBlob="29e7ec8cf0a6f2cfb9cf5bfa91cc85577c657108" -->

```diff
@@ -190,6 +190,7 @@ and drv_to_info_map =
 }
 and uexp_to_info_map =
     (
+      ~dynamics: LiveTyping.Map.t=LiveTyping.Map.empty,
       ~ctx: Ctx.t,
       ~ana=syn,
       ~is_in_filter=false,
```

</details>

<details open>
<summary><code>src/language/statics/Statics.re</code> · <code>calculate_dynamic_type</code>: type a sample by re-running statics on it</summary>

<!-- changetour:hunk file="src/language/statics/Statics.re" baseBlob="29e7ec8cf0a6f2cfb9cf5bfa91cc85577c657108" -->

```diff
@@ -199,8 +200,20 @@ and uexp_to_info_map =
       m: Map.t,
     )
     : (Info.exp, Exp.t, Map.t) => {
+  let calculate_dynamic_type = (uexp: Exp.t) => {
+    let (ie, _, _) =
+      uexp_to_info_map(
+        ~dynamics=LiveTyping.Map.empty,
+        ~ctx,
+        ~ancestors,
+        uexp,
+        StaticsBase.Map.empty,
+      );
+    Some(ie.ty);
+  };
   let ids = IdTagged.ids(uexp);
   let (term, rewrap) = Exp.unwrap(uexp);
+  let _ = ids;
   let add =
       (
         ~user_term=uexp,
```

</details>

<details open>
<summary><code>src/language/statics/Statics.re</code> · <code>add</code> refines <code>elab_syn_ty</code> before the mismatch check</summary>

<!-- changetour:hunk file="src/language/statics/Statics.re" baseBlob="29e7ec8cf0a6f2cfb9cf5bfa91cc85577c657108" -->

```diff
@@ -221,6 +234,14 @@ and uexp_to_info_map =
         m: Map.t,
       )
       : (Info.exp, Exp.t, Map.t) => {
+    let elab_syn_ty =
+      LiveTyping.refine_typ_with_dynamics(
+        ~dynamics,
+        ~calculate_dynamic_type,
+        ~ctx,
+        ~term_id=Exp.rep_id(user_term),
+        elab_syn_ty,
+      );
     let marks =
       switch (expectation_mismatch_mark(ctx, ana, elab_syn_ty)) {
       | None => marks
```

</details>

<details>
<summary><code>src/language/statics/Statics.re</code> · <code>go</code> threads <code>~dynamics</code> through recursion</summary>

<!-- changetour:hunk file="src/language/statics/Statics.re" baseBlob="29e7ec8cf0a6f2cfb9cf5bfa91cc85577c657108" -->

```diff
@@ -280,6 +301,7 @@ and uexp_to_info_map =
       )
       : (Info.exp, Exp.t, Map.t) => {
     uexp_to_info_map(
+      ~dynamics,
       ~ctx,
       ~ana,
       ~is_in_filter,
```

</details>

<details>
<summary><code>src/language/statics/Statics.re</code> · First-class-module adapter accepts (and ignores) <code>~dynamics</code></summary>

<!-- changetour:hunk file="src/language/statics/Statics.re" baseBlob="29e7ec8cf0a6f2cfb9cf5bfa91cc85577c657108" -->

```diff
@@ -1652,9 +1674,19 @@ and uexp_to_info_map =
             (module
              {
                let uexp_to_info_map =
-                   (~ctx, ~ana=?, ~is_in_filter=?, ~ancestors=?, exp, m) =>
+                   (
+                     ~dynamics as _=LiveTyping.Map.empty,
+                     ~ctx,
+                     ~ana=?,
+                     ~is_in_filter=?,
+                     ~ancestors=?,
+                     exp,
+                     m,
+                   ) =>
                  go(~ctx, ~ana?, ~is_in_filter?, ~ancestors?, exp, m);
                let add = add;
+               let dynamics = dynamics;
+               let calculate_dynamic_type = calculate_dynamic_type;
              }),
             m,
             arg,
```

</details>

<details>
<summary><code>src/language/statics/Statics.re</code> · Second first-class-module adapter, same shape</summary>

<!-- changetour:hunk file="src/language/statics/Statics.re" baseBlob="29e7ec8cf0a6f2cfb9cf5bfa91cc85577c657108" -->

```diff
@@ -1754,9 +1786,19 @@ and uexp_to_info_map =
           (module
            {
              let uexp_to_info_map =
-                 (~ctx, ~ana=?, ~is_in_filter=?, ~ancestors=?, exp, m) =>
+                 (
+                   ~dynamics as _=LiveTyping.Map.empty,
+                   ~ctx,
+                   ~ana=?,
+                   ~is_in_filter=?,
+                   ~ancestors=?,
+                   exp,
+                   m,
+                 ) =>
                go(~ctx, ~ana?, ~is_in_filter?, ~ancestors?, exp, m);
              let add = add;
+             let dynamics = dynamics;
+             let calculate_dynamic_type = calculate_dynamic_type;
            }),
           m,
           args,
```

</details>

Per-expression samples are not enough for polymorphic code: in `typfun a -> fun x : a -> x`, the parameter `x` has type `a`, and a runtime sample of `""` would look like a spurious mismatch against an abstract `a`. So when the instantiation map has entries for a type pattern, the variable enters the context as a `Singleton` of the join of its observed instantiations instead of `Abstract` — during the live pass, `a` literally means `String`. The memo key and `Statics.mk` signature grow accordingly, and `StaticsBase` gains `errors`/`has_errors` helpers (used by the live-error collection and tests) plus the extended `ExpressionStatics` module signature.

<details open>
<summary><code>src/language/statics/Statics.re</code> · <code>typfun</code> binds its type variable as a Singleton of joined instantiations</summary>

<!-- changetour:hunk file="src/language/statics/Statics.re" baseBlob="29e7ec8cf0a6f2cfb9cf5bfa91cc85577c657108" -->

```diff
@@ -1886,15 +1928,26 @@ and uexp_to_info_map =
             | _ => item
             };
           };
+          let tpat_id = TPat.rep_id(utpat);
           let ctx_body =
-            Ctx.extend_tvar(
-              ctx,
-              {
+            switch (LiveTyping.Map.lookup_type_inst(tpat_id, dynamics)) {
+            | Some([_, ..._] as insts) =>
+              LiveTyping.extend_ctx_with_instantiations(
+                ctx,
                 name,
-                id: TPat.rep_id(utpat),
-                kind: Abstract,
-              },
-            );
+                tpat_id,
+                insts,
+              )
+            | _ =>
+              Ctx.extend_tvar(
+                ctx,
+                {
+                  name,
+                  id: tpat_id,
+                  kind: Abstract,
+                },
+              )
+            };
           (mode_body, ctx_body);
         | Some(_)
         | None => (item, ctx)
```

</details>

<details>
<summary><code>src/language/statics/Statics.re</code> · Memo key now includes the dynamics map</summary>

<!-- changetour:hunk file="src/language/statics/Statics.re" baseBlob="29e7ec8cf0a6f2cfb9cf5bfa91cc85577c657108" -->

```diff
@@ -4265,9 +4318,10 @@ and mpat_to_info_map =
 let mk =
   Core.Memo.general(
     ~cache_size_bound=1000,
-    ((ana, ctx, e, probe_ids)) => {
+    ((dynamics, ana, ctx, e, probe_ids)) => {
       let (_, elab, m) =
         uexp_to_info_map(
+          ~dynamics,
           ~ana,
           ~ctx,
           ~ancestors=[],
```

</details>

<details>
<summary><code>src/language/statics/Statics.re</code> · Public <code>mk</code> accepts <code>~dynamics</code> and passes it through the gate</summary>

<!-- changetour:hunk file="src/language/statics/Statics.re" baseBlob="29e7ec8cf0a6f2cfb9cf5bfa91cc85577c657108" -->

```diff
@@ -4297,11 +4351,13 @@ let mk =
 
 let mk =
     (
+      ~dynamics=LiveTyping.Map.empty,
       ~ana=Typ.temp(Unknown(SynSwitch)),
       ~probe_ids=Id.Map.empty,
       core: CoreSettings.t,
       ctx,
       exp,
     ) =>
   core.statics
-    ? mk((ana, ctx, exp, probe_ids)) : (Id.Map.empty, Exp.fresh(Tuple([])));
+    ? mk((dynamics, ana, ctx, exp, probe_ids))
+    : (Id.Map.empty, Exp.fresh(Tuple([])));
```

</details>

<details open>
<summary><code>src/language/statics/StaticsBase.re</code> · <code>Map.errors</code> / <code>has_errors</code> helpers</summary>

<!-- changetour:hunk file="src/language/statics/StaticsBase.re" baseBlob="4402de2ca1f69db7b3f8d5d24aaedeacef602436" -->

```diff
@@ -43,6 +43,20 @@ module Map = {
       [],
     );
 
+  let errors = (map: t): list((Id.t, list(Mark.t))) =>
+    Id.Map.fold(
+      (id, info: Info.t, acc) =>
+        switch (Info.marks_of(info)) {
+        | [] => acc
+        | ms => [(id, ms), ...acc]
+        },
+      map,
+      [],
+    );
+
+  let has_errors = (map: t): bool =>
+    Id.Map.exists((_: Uuidm.t, info: Info.t) => Info.is_error(info), map);
+
   /* The ids of binding sites for for all references in term with `id` */
   let refs_in = (m: t, id: Id.t): Binding.s =>
     switch (lookup(id, m)) {
```

</details>

<details>
<summary><code>src/language/statics/StaticsBase.re</code> · <code>ExpressionStatics</code> signature gains <code>~dynamics</code></summary>

<!-- changetour:hunk file="src/language/statics/StaticsBase.re" baseBlob="4402de2ca1f69db7b3f8d5d24aaedeacef602436" -->

```diff
@@ -453,6 +467,7 @@ let fold_patterns_with_modes =
 module type ExpressionStatics = {
   let uexp_to_info_map:
     (
+      ~dynamics: LiveTyping.Map.t=?,
       ~ctx: Ctx.t,
       ~ana: Typ.t=?,
       ~is_in_filter: bool=?,
```

</details>

<details>
<summary><code>src/language/statics/StaticsBase.re</code> · …and exposes <code>dynamics</code> / <code>calculate_dynamic_type</code></summary>

<!-- changetour:hunk file="src/language/statics/StaticsBase.re" baseBlob="4402de2ca1f69db7b3f8d5d24aaedeacef602436" -->

```diff
@@ -481,4 +496,8 @@ module type ExpressionStatics = {
       Map.t
     ) =>
     (Info.exp, Exp.t, Map.t);
+
+  let dynamics: LiveTyping.Map.t;
+
+  let calculate_dynamic_type: Exp.t => option(Typ.t);
 };
```

</details>

## Dynamics: recording samples and type instantiations

Evaluation already collected probe samples; this PR adds a parallel channel for type instantiations. `Dynamics.TypeInstantiation` records the type pattern's id, the variable name, the concrete type, and the call stack at instantiation time; `TypeInstMap` indexes these by TPat id. Both samples and instantiations get `filter_by_focus`, which honors probe pinning (suffix-match on the pinned call stack) so live typing can be scoped to one pinned call. `Dynamics.t` grows the `type_inst_map` field and an `empty` value — the latter matters below, because the web model now stores a full `Dynamics.t`.

<details open>
<summary><code>src/language/dynamics/Dynamics.re</code> · <code>TypeInstantiation</code> / <code>TypeInstMap</code> with pin filtering</summary>

<!-- changetour:hunk file="src/language/dynamics/Dynamics.re" baseBlob="d98859505e80780781eab88a577869a3aa510eb7" -->

```diff
@@ -5,6 +5,62 @@ open Util;
  * static information gathering, but right now it specifically handles
  * sample gathering for probe projectors */
 
+module TypeInstantiation = {
+  /* A type instantiation records when a type variable is instantiated
+   * with a concrete type during type application evaluation */
+  [@deriving (show({with_path: false}), sexp, yojson)]
+  type t = {
+    tpat_id: Id.t, /* ID of the type pattern */
+    type_var: string, /* Variable name (e.g., "a") */
+    instantiated_type: Typ.t, /* The concrete type (e.g., String) */
+    call_stack: list(Id.t), /* Call stack at instantiation time */
+    time: float /* Timestamp */
+  };
+};
+
+module TypeInstMap = {
+  /* Type applications recorded during evaluation, indexed by the
+   * TPat ids of the type parameters */
+  [@deriving (show({with_path: false}), sexp, yojson)]
+  type t = Id.Map.t(list(TypeInstantiation.t));
+
+  let empty = Id.Map.empty;
+  let lookup = Id.Map.find_opt;
+
+  let extend = (id, inst: TypeInstantiation.t, map: t) => {
+    Id.Map.update(
+      id,
+      opt =>
+        switch (opt) {
+        | Some(a) => Some(a @ [inst])
+        | None => Some([inst])
+        },
+      map,
+    );
+  };
+  let filter_type_instantiations_by_pin =
+      (sample_focus: Sample.Focus.t, closures: list(TypeInstantiation.t))
+      : list(TypeInstantiation.t) =>
+    switch (sample_focus.pinned_stack) {
+    | Some(pinned_stack) =>
+      List.filter(
+        (closure: TypeInstantiation.t) =>
+          ListUtil.is_suffix_of(
+            Sample.ids_of_stack(pinned_stack),
+            closure.call_stack,
+          ),
+        closures,
+      )
+    | None => closures
+    };
+
+  let filter_by_focus = (sample_focus: Sample.Focus.t, map: t): t =>
+    Id.Map.map(
+      closures => filter_type_instantiations_by_pin(sample_focus, closures),
+      map,
+    );
+};
+
 module Info = {
   /* Collected samples for a given id */
   [@deriving (show({with_path: false}), sexp, yojson)]
```

</details>

<details open>
<summary><code>src/language/dynamics/Dynamics.re</code> · <code>Dynamics.t</code> gains <code>type_inst_map</code>, <code>empty</code>, <code>filter_by_focus</code></summary>

<!-- changetour:hunk file="src/language/dynamics/Dynamics.re" baseBlob="d98859505e80780781eab88a577869a3aa510eb7" -->

```diff
@@ -46,11 +102,52 @@ module Map = {
   let empty: t = Sample.Map.empty;
   let mk: t => t = Fun.id;
   let lookup = Sample.Map.lookup;
+
+  /* Apply pin filtering to all probes in the map, using the centralized
+   * Sample.Selection.filter_by_pin helper so filtering matches the
+   * semantics used by probe sample selection. */
+  let filter_by_focus = (focus: Sample.Focus.t, map: t): t =>
+    Id.Map.mapi(
+      (ap_id, samples) =>
+        Sample.Selection.filter_by_pin(
+          ~ap_id=Some(ap_id),
+          ~pinned=focus.pinned_stack,
+          samples,
+        ),
+      map,
+    );
 };
 
 [@deriving (show({with_path: false}), sexp, yojson)]
 type t = {
   probe_map: Sample.Map.t,
+  type_inst_map: TypeInstMap.t,
   test_results: TestResults.t,
   theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
 };
+
+let empty: t = {
+  probe_map: Sample.Map.empty,
+  type_inst_map: TypeInstMap.empty,
+  theorems: [],
+  test_results: {
+    test_map: [],
+    statuses: [],
+    hints: [],
+    descriptions: [],
+    total: 0,
+    passing: 0,
+    failing: 0,
+    unfinished: 0,
+  },
+};
+
+let filter_by_focus = (sample_focus: Sample.Focus.t, dyn: t): t => {
+  {
+    probe_map: Map.filter_by_focus(sample_focus, dyn.probe_map),
+    type_inst_map:
+      TypeInstMap.filter_by_focus(sample_focus, dyn.type_inst_map),
+    test_results: dyn.test_results,
+    theorems: dyn.theorems,
+  };
+};
```

</details>

Instantiations are captured as a side effect of the `TypFunAp` transition. The effect carries a closure (`Sample.call_stack => TypeInstantiation.t`) because the call stack is only known when `EvaluatorState.update` processes the effect queue. `EvaluatorState` accumulates the map and — critically for the incremental evaluator — `StateSlice` learns to diff and replay `type_insts`, so cache reuse of a `TypAp` subtree doesn't silently drop its instantiations. (The `RecordAscriptionProbe` variant moves within the `effect` type; its handler is unchanged, just relocated.)

<details open>
<summary><code>src/language/dynamics/transition/Transition.re</code> · <code>TypFunAp</code> emits <code>RecordTypeInstantiation</code> (and a stack frame)</summary>

<!-- changetour:hunk file="src/language/dynamics/transition/Transition.re" baseBlob="82fbf0a96ecdcc22d1971c467161f081e8e0a69f" -->

```diff
@@ -643,7 +643,25 @@ module Transition = (EV: EV_MODE) => {
                 name,
               ),
             ),
-          side_effects: [],
+          side_effects:
+            [EvaluatorState.RecordStackFrame(name, None, None)]
+            @ (
+              switch (TPat.tyvar_of_utpat(utpat)) {
+              | Some(var_name) => [
+                  EvaluatorState.RecordTypeInstantiation(
+                    call_stack =>
+                      Dynamics.TypeInstantiation.{
+                        tpat_id: TPat.rep_id(utpat),
+                        type_var: var_name,
+                        instantiated_type: tau,
+                        call_stack: Sample.ids_of_stack(call_stack),
+                        time: JsUtil.timestamp(),
+                      },
+                  ),
+                ]
+              | None => []
+              }
+            ),
           kind: TypFunAp,
           is_value: false,
         })
```

</details>

<details>
<summary><code>src/language/dynamics/transition/Transition.re</code> · Comment: ascription samples now serve live typing</summary>

<!-- changetour:hunk file="src/language/dynamics/transition/Transition.re" baseBlob="82fbf0a96ecdcc22d1971c467161f081e8e0a69f" -->

```diff
@@ -1187,7 +1205,7 @@ module Transition = (EV: EV_MODE) => {
          * and is_value: true to prevent re-evaluation (probes inside d' have
          * already fired via req_final above; re-evaluation would double-count
          * samples at a different call_stack). We still collect any ascription
-         * samples produced while distributing the ascription. */
+         * samples for the live-typing feature. */
         let (_peek_samples, peek) =
           Ascriptions.transition(~targets, Asc(d', t) |> rewrap);
         switch (peek) {
```

</details>

<details>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · State gains <code>type_insts</code></summary>

<!-- changetour:hunk file="src/language/dynamics/state/EvaluatorState.re" baseBlob="21b0b9abe87f141b98ceea82b09e251f97ec41fc" -->

```diff
@@ -13,6 +13,7 @@ type t = {
   theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
   tests: TestMap.t,
   probes: Sample.Map.t,
+  type_insts: Dynamics.TypeInstMap.t,
   app_args: app_args_t, /* Argument values for function applications */
   step_count: int,
   pending_probe_starts: Id.Map.t(list(int)), /* Stack per probe_id; nested recursive calls push/pop */
```

</details>

<details>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · <code>RecordAscriptionProbe</code> moves out of its old slot</summary>

<!-- changetour:hunk file="src/language/dynamics/state/EvaluatorState.re" baseBlob="21b0b9abe87f141b98ceea82b09e251f97ec41fc" -->

```diff
@@ -23,7 +24,6 @@ type t = {
 type effect =
   | RecordTest(TestMap.instance_report)
   | RecordExpProbe(Sample.capture_spec)
-  | RecordAscriptionProbe((Id.t, Sample.capture_spec, Exp.t))
   | RecordStackFrame(option(string), option(DHExp.t), option(Id.t)) /* (fn_name, arg_value, fn_def_id) */
   /* A pattern was matched against a value during evaluation. Carries the
    * pat and rhs so the incremental evaluator can decide which body-scoped
```

</details>

<details open>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · New <code>RecordTypeInstantiation</code> effect (closure over the call stack); <code>mk</code> inits the map</summary>

<!-- changetour:hunk file="src/language/dynamics/state/EvaluatorState.re" baseBlob="21b0b9abe87f141b98ceea82b09e251f97ec41fc" -->

```diff
@@ -33,12 +33,15 @@ type effect =
       rhs: DHExp.t,
       samples: PatternMatch.sample_closures,
     })
+  | RecordTypeInstantiation(Sample.call_stack => Dynamics.TypeInstantiation.t)
+  | RecordAscriptionProbe((Id.t, Sample.capture_spec, Exp.t))
   | RecordTheorem(Id.t, string, Environment.t(Exp.t), Exp.t)
   | RecordPrint(DHExp.t); /* Println for probes study */
 
 let mk = (~targets: Sample.targets): t => {
   tests: TestMap.empty,
   probes: Sample.Map.empty,
+  type_insts: Dynamics.TypeInstMap.empty,
   app_args: Id.Map.empty,
   step_count: 0,
   pending_probe_starts: Id.Map.empty,
```

</details>

<details>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · <code>get_type_insts</code> accessor</summary>

<!-- changetour:hunk file="src/language/dynamics/state/EvaluatorState.re" baseBlob="21b0b9abe87f141b98ceea82b09e251f97ec41fc" -->

```diff
@@ -89,6 +92,7 @@ let get_tests = ({tests, _}) => tests;
 
 let get_probes = ({probes, _}) => probes;
 
+let get_type_insts = ({type_insts, _}) => type_insts;
 let get_theorems = ({theorems, _}) => theorems;
 
 let get_app_args = ({app_args, _}) => app_args;
```

</details>

<details>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · <code>add_type_inst</code> extends the map</summary>

<!-- changetour:hunk file="src/language/dynamics/state/EvaluatorState.re" baseBlob="21b0b9abe87f141b98ceea82b09e251f97ec41fc" -->

```diff
@@ -206,6 +210,11 @@ let add_sample = (state: t, sample: Sample.t) => {
   };
 };
 
+let add_type_inst = (state: t, inst: Dynamics.TypeInstantiation.t) => {
+  ...state,
+  type_insts:
+    Dynamics.TypeInstMap.extend(inst.tpat_id, inst, state.type_insts),
+};
 let add_theorem = ({theorems, _} as es, id, name, env, goal) => {
   {
     ...es,
```

</details>

<details>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · Old <code>RecordAscriptionProbe</code> handler removed (relocated below)</summary>

<!-- changetour:hunk file="src/language/dynamics/state/EvaluatorState.re" baseBlob="21b0b9abe87f141b98ceea82b09e251f97ec41fc" -->

```diff
@@ -282,26 +291,6 @@ let update =
           );
         let state = clear_probe_start(state, probe_id);
         (call_stack, add_sample(state, sample));
-      | RecordAscriptionProbe((id, capture_spec, ascribed_exp)) =>
-        let step = state.step_count;
-        /* Substitute env so a Var body resolves to its runtime value. */
-        let ascribed_exp = Substitution.in_exp(env, ascribed_exp);
-        let sample =
-          Sample.mk(
-            ~step_start=step,
-            ~step_end=step,
-            id,
-            ascribed_exp,
-            env,
-            call_stack,
-            capture_spec,
-          );
-        let state = add_sample(state, sample);
-        let state = {
-          ...state,
-          step_count: state.step_count + 1,
-        };
-        (call_stack, state);
       | RecordPatMatch({samples: sample_closures, _}) =>
         /* Pattern probes are recorded at the current step, then we
          * increment to ensure patterns don't share step boundaries
```

</details>

<details open>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · Relocated ascription handler + the <code>RecordTypeInstantiation</code> handler</summary>

<!-- changetour:hunk file="src/language/dynamics/state/EvaluatorState.re" baseBlob="21b0b9abe87f141b98ceea82b09e251f97ec41fc" -->

```diff
@@ -324,6 +313,30 @@ let update =
           step_count: state.step_count + 1,
         };
         (call_stack, state);
+      | RecordAscriptionProbe((id, capture_spec, ascribed_exp)) =>
+        let step = state.step_count;
+        /* Substitute env so a Var body resolves to its runtime value. */
+        let ascribed_exp = Substitution.in_exp(env, ascribed_exp);
+        let sample =
+          Sample.mk(
+            ~step_start=step,
+            ~step_end=step,
+            id,
+            ascribed_exp,
+            env,
+            call_stack,
+            capture_spec,
+          );
+        let state = add_sample(state, sample);
+        let state = {
+          ...state,
+          step_count: state.step_count + 1,
+        };
+        (call_stack, state);
+      | RecordTypeInstantiation(type_inst_closure) => (
+          call_stack,
+          add_type_inst(state, type_inst_closure(call_stack)),
+        )
       | RecordPrint(value) =>
         /* Print happens in a single step */
         let step = state.step_count;
```

</details>

<details>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · <code>capture_slice</code> diffs <code>type_insts</code></summary>

<!-- changetour:hunk file="src/language/dynamics/state/EvaluatorState.re" baseBlob="21b0b9abe87f141b98ceea82b09e251f97ec41fc" -->

```diff
@@ -359,6 +372,11 @@ let capture_slice = (~before: t, ~after: t): StateSlice.t => {
     StateSlice.diff_theorems(~before=before.theorems, ~after=after.theorems),
   app_args:
     StateSlice.diff_app_args(~before=before.app_args, ~after=after.app_args),
+  type_insts:
+    StateSlice.diff_type_insts(
+      ~before=before.type_insts,
+      ~after=after.type_insts,
+    ),
 };
 
 /* Replay a slice into `state`: add its sample/test/theorem/app_arg entries,
```

</details>

<details open>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · <code>replay_slice</code> merges replayed instantiations</summary>

<!-- changetour:hunk file="src/language/dynamics/state/EvaluatorState.re" baseBlob="21b0b9abe87f141b98ceea82b09e251f97ec41fc" -->

```diff
@@ -405,12 +423,26 @@ let replay_slice = (slice: StateSlice.t, state: t): t => {
       slice.app_args,
       state.app_args,
     );
+  let type_insts =
+    Id.Map.fold(
+      (id, new_entries, acc) => {
+        let existing =
+          switch (Id.Map.find_opt(id, acc)) {
+          | Some(l) => l
+          | None => []
+          };
+        Id.Map.add(id, existing @ new_entries, acc);
+      },
+      slice.type_insts,
+      state.type_insts,
+    );
   {
     ...state,
     step_count: state.step_count + slice.steps,
     probes,
     tests,
     theorems,
     app_args,
+    type_insts,
   };
 };
```

</details>

<details>
<summary><code>src/language/dynamics/state/StateSlice.re</code> · Slice type gains <code>type_insts</code></summary>

<!-- changetour:hunk file="src/language/dynamics/state/StateSlice.re" baseBlob="58af7166b55b4d1ca3226cf5a4e9b076eba908ed" -->

```diff
@@ -12,6 +12,7 @@ type t = {
   tests: list((Id.t, list(TestMap.instance_report))),
   theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
   app_args: Id.Map.t(list((Sample.call_stack, Sample.Env.elided_value))),
+  type_insts: Dynamics.TypeInstMap.t,
 };
 
 let empty: t = {
```

</details>

<details>
<summary><code>src/language/dynamics/state/StateSlice.re</code> · Empty slice</summary>

<!-- changetour:hunk file="src/language/dynamics/state/StateSlice.re" baseBlob="58af7166b55b4d1ca3226cf5a4e9b076eba908ed" -->

```diff
@@ -21,6 +22,7 @@ let empty: t = {
   tests: [],
   theorems: [],
   app_args: Id.Map.empty,
+  type_insts: Dynamics.TypeInstMap.empty,
 };
 
 let diff_probes = (~before: Sample.Map.t, ~after: Sample.Map.t): Sample.Map.t =>
```

</details>

<details open>
<summary><code>src/language/dynamics/state/StateSlice.re</code> · <code>diff_type_insts</code>: new entries are the appended tail per id</summary>

<!-- changetour:hunk file="src/language/dynamics/state/StateSlice.re" baseBlob="58af7166b55b4d1ca3226cf5a4e9b076eba908ed" -->

```diff
@@ -117,3 +119,28 @@ let shift_sample = (delta: int, s: Sample.t): Sample.t => {
   step_start: s.step_start + delta,
   step_end: s.step_end + delta,
 };
+
+let diff_type_insts =
+    (~before: Dynamics.TypeInstMap.t, ~after: Dynamics.TypeInstMap.t)
+    : Dynamics.TypeInstMap.t =>
+  Id.Map.fold(
+    (id, after_entries, acc) => {
+      let before_count =
+        switch (Id.Map.find_opt(id, before)) {
+        | Some(l) => List.length(l)
+        | None => 0
+        };
+      let after_count = List.length(after_entries);
+      let new_count = after_count - before_count;
+      if (new_count > 0) {
+        /* Take the tail entries (new ones are appended in TypeInstMap.extend) */
+        let new_entries =
+          List.filteri((i, _) => i >= before_count, after_entries);
+        Id.Map.add(id, new_entries, acc);
+      } else {
+        acc;
+      };
+    },
+    after,
+    Id.Map.empty,
+  );
```

</details>

## Probe selection: deciding what to sample

`CoreSettings.live_typing` is the feature flag. The interesting decision is *what to probe*: when the flag is on, `CachedStatics.compute_targets` adds every expression and pattern whose **elaborated synthesized** type still contains unknowns. It checks `elab_syn_ty` rather than the fixed type deliberately — `1 : ?` analyzed against `String` has fixed type `String`, but the `?` is exactly the part that needs runtime feedback. This composes with explicit probes and `probe_all` via map unions. `CachedStatics.t` also gains the two outputs of the live pass (`live_typing_info_map`, `live_typing_error_ids`), filled in by the web layer below.

<details open>
<summary><code>src/language/CoreSettings.re</code> · The <code>live_typing</code> flag</summary>

<!-- changetour:hunk file="src/language/CoreSettings.re" baseBlob="e0e916a2e5399764d292de5af80e3da0758b7c7b" -->

```diff
@@ -42,6 +42,7 @@ type t = {
   elaborate: bool,
   assist: bool,
   dynamics: bool,
+  live_typing: bool,
   probe_all: bool,
   deep_reassociate: bool,
   flip_animations: bool,
```

</details>

<details>
<summary><code>src/language/CoreSettings.re</code> · Off config</summary>

<!-- changetour:hunk file="src/language/CoreSettings.re" baseBlob="e0e916a2e5399764d292de5af80e3da0758b7c7b" -->

```diff
@@ -60,6 +61,7 @@ let off: t = {
   elaborate: false,
   assist: false,
   dynamics: false,
+  live_typing: false,
   probe_all: false,
   deep_reassociate: false,
   flip_animations: false,
```

</details>

<details>
<summary><code>src/language/CoreSettings.re</code> · On config</summary>

<!-- changetour:hunk file="src/language/CoreSettings.re" baseBlob="e0e916a2e5399764d292de5af80e3da0758b7c7b" -->

```diff
@@ -73,6 +75,7 @@ let on: t = {
   elaborate: true,
   assist: true,
   dynamics: true,
+  live_typing: true,
   probe_all: false, /* Off by default even in "on" config - opt-in feature */
   deep_reassociate: false,
   flip_animations: true,
```

</details>

<details open>
<summary><code>src/haz3lcore/derived/CachedStatics.re</code> · Statics cache carries the live-pass results</summary>

<!-- changetour:hunk file="src/haz3lcore/derived/CachedStatics.re" baseBlob="56cbebd88295fc76bf301526393db47c7ef4a1c9" -->

```diff
@@ -8,7 +8,9 @@ type t = {
   info_map: Statics.Map.t,
   error_ids: list(Id.t),
   warning_ids: list(Id.t),
-  targets: Sample.targets /* Maps expr/pat IDs to capture specs for sampling */
+  targets: Sample.targets, /* Maps expr/pat IDs to capture specs for sampling */
+  live_typing_info_map: Statics.Map.t,
+  live_typing_error_ids: list(Id.t),
 };
 
 let empty: t = {
```

</details>

<details>
<summary><code>src/haz3lcore/derived/CachedStatics.re</code> · Empty cache</summary>

<!-- changetour:hunk file="src/haz3lcore/derived/CachedStatics.re" baseBlob="56cbebd88295fc76bf301526393db47c7ef4a1c9" -->

```diff
@@ -24,6 +26,8 @@ let empty: t = {
   error_ids: [],
   warning_ids: [],
   targets: Sample.no_targets,
+  live_typing_info_map: Id.Map.empty,
+  live_typing_error_ids: [],
 };
 
 let dh_err = (error: string): DHExp.t => Var(error) |> DHExp.fresh;
```

</details>

<details open>
<summary><code>src/haz3lcore/derived/CachedStatics.re</code> · <code>ids_with_unknown_types</code> (keyed on <code>elab_syn_ty</code>); <code>compute_targets</code> unions probes, probe-all, and unknown-typed ids</summary>

<!-- changetour:hunk file="src/haz3lcore/derived/CachedStatics.re" baseBlob="56cbebd88295fc76bf301526393db47c7ef4a1c9" -->

```diff
@@ -45,19 +49,57 @@ let all_probeable_ids = (info_map: Statics.Map.t): Id.Map.t(unit) =>
     Id.Map.empty,
   );
 
+/* Collect IDs of expressions/patterns with partially unknown types.
+ * Used for live_typing to automatically probe terms that need dynamic type feedback.
+ * Note: We check elab_syn_ty (synthesized) rather than the fixed ty, because
+ * an expression like `1 : ?` analyzed against String would have ty=String
+ * but elab_syn_ty=Unknown - we need dynamic feedback for the Unknown part. */
+let ids_with_unknown_types = (info_map: Statics.Map.t): Id.Map.t(unit) =>
+  Id.Map.fold(
+    (id, info, acc) =>
+      switch (info) {
+      | Info.InfoExp({elab_syn_ty: ty, _}) when Typ.contains_unknown(ty) =>
+        Id.Map.add(id, (), acc)
+      | Info.InfoPat({elab_syn_ty: ty, _}) when Typ.contains_unknown(ty) =>
+        Id.Map.add(id, (), acc)
+      | _ => acc
+      },
+    info_map,
+    Id.Map.empty,
+  );
+
 /* Compute targets from probe_ids. For each ID, determine whether it's
  * an expression or pattern target, then look up the appropriate refs to capture.
  * When probe_all is enabled, we target everything in info_map that passes
- * should_probe, ignoring the passed probe_ids (which are a subset anyway). */
+ * should_probe, ignoring the passed probe_ids (which are a subset anyway).
+ * When live_typing is enabled, we also include expressions with unknown types. */
 let compute_targets =
     (
       ~settings: CoreSettings.t,
       ~info_map: Statics.Map.t,
       ~probe_ids: Id.Map.t(unit),
     )
     : Sample.targets => {
+  /* Start with explicit probe IDs */
+  let base_ids = probe_ids;
+  /* If probe_all is enabled, include all probeable expressions */
+  let base_ids =
+    settings.probe_all
+      ? Id.Map.union(
+          (_, _, _) => Some(),
+          base_ids,
+          all_probeable_ids(info_map),
+        )
+      : base_ids;
+  /* If live_typing is enabled, include expressions with unknown types */
   let effective_probe_ids =
-    settings.probe_all ? all_probeable_ids(info_map) : probe_ids;
+    settings.live_typing
+      ? Id.Map.union(
+          (_, _, _) => Some(),
+          base_ids,
+          ids_with_unknown_types(info_map),
+        )
+      : base_ids;
   Id.Map.fold(
     (id, (), acc) => {
       let refs =
```

</details>

<details>
<summary><code>src/haz3lcore/derived/CachedStatics.re</code> · Init with empty live-pass results</summary>

<!-- changetour:hunk file="src/haz3lcore/derived/CachedStatics.re" baseBlob="56cbebd88295fc76bf301526393db47c7ef4a1c9" -->

```diff
@@ -120,6 +162,8 @@ let init_from_term =
     error_ids,
     warning_ids,
     targets,
+    live_typing_info_map: Statics.Map.empty,
+    live_typing_error_ids: [],
   };
 };
 
```

</details>

Two probe-system companions: `ProbeProj.cur_ap` extracts the application id from an info record (a helper alongside the existing var-ap lookup; nothing references it yet), and the type projector now pin-filters its samples through the shared `Sample.Selection.filter_by_pin` before inferring a dynamic type, matching the semantics of the probe sidebar.

<details>
<summary><code>src/haz3lcore/projectors/implementations/ProbeProj.re</code> · <code>cur_ap</code>: application id of an info record (currently unreferenced)</summary>

<!-- changetour:hunk file="src/haz3lcore/projectors/implementations/ProbeProj.re" baseBlob="8fe73d536ba16c86ae437fe512be595b4e202b0c" -->

```diff
@@ -196,6 +196,14 @@ module SampleLength = {
     Hashtbl.replace(lengths, id, length);
 };
 
+let cur_ap = (info: info) =>
+  switch (info.statics) {
+  | Some(InfoExp({user_term: {term: Ap(_), _} as ap, _}))
+  | Some(InfoExp({user_term: {term: TypAp(_), _} as ap, _})) =>
+    Some(Exp.rep_id(ap))
+  | _ => None
+  };
+
 /* Select samples to display, using stateful window offset.
  * This wraps Sample.Selection with WindowState for offset persistence.
  * Optionally takes pre-filtered samples to avoid redundant filtering. */
```

</details>

<details open>
<summary><code>src/haz3lcore/projectors/implementations/TypeProj.re</code> · Type projector respects probe pinning</summary>

<!-- changetour:hunk file="src/haz3lcore/projectors/implementations/TypeProj.re" baseBlob="156b5de4789c40648d7fa72dcd0cf687f042c5f7" -->

```diff
@@ -28,10 +28,17 @@ let get_dynamic_typ = (info: info): Typ.t => {
   let ctx =
     Option.map(Info.ctx_of, info.statics)
     |> Option.value(~default=Builtins.ctx_init(Some(Int)));
+  let ap_id = Option.bind(info.statics, Sample.Focus.cur_var_ap);
   info.dynamics
-  |> Option.map((d: Dynamics.Info.t) =>
-       DynamicTypInfer.dynamic_typ_of_samples_or_unknown(~ctx, d.samples)
-     )
+  |> Option.map((d: Dynamics.Info.t) => {
+       let filtered =
+         Sample.Selection.filter_by_pin(
+           ~ap_id,
+           ~pinned=d.sample_focus.pinned_stack,
+           d.samples,
+         );
+       DynamicTypInfer.dynamic_typ_of_samples_or_unknown(~ctx, filtered);
+     })
   |> Option.value(~default=Typ.fresh(Unknown(Internal)));
 };
 
```

</details>

## Web layer: running the second statics pass

`CodeWithStatics.Update.calculate` is where live typing actually happens. The model now stores a full `Dynamics.t` (not just a probe map) plus two `Calc`-cached values: the live-pass result and the current `sample_focus`. When the setting is on, the pass filters dynamics by focus (pin), converts them into `LiveTyping.Map` shape, re-runs `Statics.mk(~dynamics=...)`, and keeps only error ids statics didn't already report — those become `live_typing_error_ids`. The `Calc` incremental wrappers mean the expensive second pass reruns only when the dynamics or the focus actually change. The same hunk also fixes statics invalidation when probe ephemerals change without an edit (autoprobe cursor movement, reload).

<details>
<summary><code>src/web/app/editors/code/CodeWithStatics.re</code> · Opens for <code>Calc</code>/<code>Language</code></summary>

<!-- changetour:hunk file="src/web/app/editors/code/CodeWithStatics.re" baseBlob="ddc0a7dbe6b6c0df5ad7a2b7f250e81af94918d9" -->

```diff
@@ -1,5 +1,7 @@
-open Util.WebUtil;
+open Util;
 open Haz3lcore;
+open Language;
+open WebUtil;
 
 /* Read-only code viewer with statics, but no interaction. Notably,
    since there is no interaction, the user can see that there is an
```

</details>

<details open>
<summary><code>src/web/app/editors/code/CodeWithStatics.re</code> · Model: full <code>Dynamics.t</code> + cached <code>live_typing</code>/<code>sample_focus</code></summary>

<!-- changetour:hunk file="src/web/app/editors/code/CodeWithStatics.re" baseBlob="ddc0a7dbe6b6c0df5ad7a2b7f250e81af94918d9" -->

```diff
@@ -20,22 +22,23 @@ module Model = {
     editor: Editor.t,
     context_menu: context_menu_state,
     statics: CachedStatics.t,
-    dynamics: Language.Dynamics.Map.t,
+    dynamics: Dynamics.t,
+    live_typing: Calc.saved((StaticsBase.Map.t, list(Id.t))),
+    sample_focus: Calc.saved(Language.Sample.Focus.t),
   };
 
   let context_menu_is_open = (model: t): bool =>
     Util.Menu.is_open(model.context_menu);
 
-  let mk =
-      (
-        ~dynamics=Language.Dynamics.Map.empty,
-        ~statics=CachedStatics.empty,
-        editor,
-      ) => {
-    editor,
-    statics,
-    dynamics,
-    context_menu: None,
+  let mk = (~dynamics=Dynamics.empty, ~statics=CachedStatics.empty, editor) => {
+    {
+      editor,
+      statics,
+      dynamics,
+      context_menu: None,
+      live_typing: Calc.Pending,
+      sample_focus: Calc.Pending,
+    };
   };
 
   let mk_from_exp =
```

</details>

<details open>
<summary><code>src/web/app/editors/code/CodeWithStatics.re</code> · Cursor info now includes <code>live_typing_info</code> and per-id samples</summary>

<!-- changetour:hunk file="src/web/app/editors/code/CodeWithStatics.re" baseBlob="ddc0a7dbe6b6c0df5ad7a2b7f250e81af94918d9" -->

```diff
@@ -56,35 +59,45 @@ module Model = {
 
   let get_statics = (model: t) => model.statics;
 
-  let get_dynamics = (model: t) => model.dynamics;
-
   let get_cursor_info = (model: t): Cursor.cursor(Action.t) => {
-    info: Indicated.ci_of(model.editor.state.zipper, model.statics.info_map),
-    dynamics: None,
-    indicated_piece:
-      Indicated.for_decoration(model.editor.state.zipper)
-      |> Option.map(({piece, _}: Indicated.piece) => piece),
-    selected_text:
-      Some(
-        () => {
-          let z = model.editor.state.zipper;
-          let full =
-            Printer.of_segment(
-              ~indent=" ",
-              ~refractors=z.refractors.manuals,
-              z.selection.content,
-            );
-          Zipper.trim_selected_text(z, full);
-        },
-      ),
-    selection: Some(model.editor.state.zipper.selection.content),
-    editor: Some(model.editor),
-    editor_read_only: true,
-    editor_action: x => Some(x),
-    undo_action: None,
-    redo_action: None,
-    error_ids: model.statics.error_ids,
-    contextual_actions: [],
+    let info =
+      Indicated.ci_of(model.editor.state.zipper, model.statics.info_map);
+    let live_typing_info =
+      Indicated.ci_of(
+        model.editor.state.zipper,
+        model.statics.live_typing_info_map,
+      );
+    let id = Indicated.index(model.editor.state.zipper);
+    {
+      info,
+      live_typing_info,
+      dynamics:
+        Option.bind(id, Dynamics.Map.lookup(_, model.dynamics.probe_map)),
+      indicated_piece:
+        Indicated.for_decoration(model.editor.state.zipper)
+        |> Option.map(({piece, _}: Indicated.piece) => piece),
+      selected_text:
+        Some(
+          () => {
+            let z = model.editor.state.zipper;
+            let full =
+              Printer.of_segment(
+                ~indent=" ",
+                ~refractors=z.refractors.manuals,
+                z.selection.content,
+              );
+            Zipper.trim_selected_text(z, full);
+          },
+        ),
+      selection: Some(model.editor.state.zipper.selection.content),
+      editor: Some(model.editor),
+      editor_read_only: true,
+      editor_action: x => Some(x),
+      undo_action: None,
+      redo_action: None,
+      error_ids: model.statics.error_ids,
+      contextual_actions: [],
+    };
   };
 
   [@deriving (show({with_path: false}), sexp, yojson)]
```

</details>

<details open>
<summary><code>src/web/app/editors/code/CodeWithStatics.re</code> · Recompute statics when probe ephemerals change, not just on edit</summary>

<!-- changetour:hunk file="src/web/app/editors/code/CodeWithStatics.re" baseBlob="ddc0a7dbe6b6c0df5ad7a2b7f250e81af94918d9" -->

```diff
@@ -143,23 +156,58 @@ module Update = {
   /* Calculates the statics for the editor. */
   let calculate =
       (
-        ~settings,
+        ~settings: CoreSettings.t,
         ~autoprobe_mode=false,
         ~is_edited,
         ~statics_mode=StaticsNormal,
         ~ctx=?,
         ~stitch,
-        ~dynamics: Language.Dynamics.Map.t,
+        ~dynamics: Calc.t(Dynamics.t),
         ~is_dynamic_term,
         ~ana=?,
-        {editor, statics, context_menu, _}: Model.t,
+        {
+          editor,
+          statics,
+          live_typing,
+          sample_focus,
+          context_menu,
+          dynamics: _,
+        }: Model.t,
       )
       : Model.t => {
-    /* Throttle gate: decide whether to do a full statics recompute this
-     * frame. When we reuse, `statics` keeps its ref — CachedSyntax.calculate
-     * then skips the shape pass via phys-eq on info_map/elaborated. */
+    let dynamics_map = Calc.map(dynamics, (d: Dynamics.t) => d.probe_map);
+    /* Capture ephemerals before editor calculation to detect auto probe changes */
+    let old_ephemerals = editor.state.zipper.refractors.multis.ephemerals;
+
+    let editor =
+      Editor.Update.calculate(
+        ~settings,
+        ~autoprobe_mode,
+        ~is_edited,
+        statics,
+        dynamics_map |> Calc.get_value,
+        editor,
+      );
+
+    /* Ephemerals can change without an explicit edit in several cases:
+     * (1) cursor movement in autoprobe mode (cursor crosses into a new
+     *     top-level definition), and
+     * (2) on reload, when add_ids_from_multi_term rebuilds ephemerals
+     *     from persisted multis.ids once the info_map becomes available.
+     * In both cases we must recalculate statics so probe targets match
+     * the new ephemerals and the evaluator collects samples for them. */
+    let probes_changed =
+      !
+        Id.Map.equal(
+          Refractors.equal_entry,
+          old_ephemerals,
+          editor.state.zipper.refractors.multis.ephemerals,
+        );
+
     let statics =
-      statics_mode == StaticsForce || is_edited && statics_mode != StaticsDefer
+      statics_mode == StaticsForce
+      || (is_edited || probes_changed)
+      && statics_mode != StaticsDefer
         ? CachedStatics.init(
             ~settings,
             ~stitch,
```

</details>

<details open>
<summary><code>src/web/app/editors/code/CodeWithStatics.re</code> · The live pass: filter by focus, build LiveTyping map, re-run statics, dedupe error ids, persist Calc-saved fields</summary>

<!-- changetour:hunk file="src/web/app/editors/code/CodeWithStatics.re" baseBlob="ddc0a7dbe6b6c0df5ad7a2b7f250e81af94918d9" -->

```diff
@@ -170,26 +218,94 @@ module Update = {
             editor.state.zipper,
           )
         : statics;
+    /* Refresh `statics.targets` against the post-probe-effects refractors.
+     * Cheap O(|probe_ids|) fold; only this field depends on refractors, so
+     * the rest of statics stays valid. */
+    let statics =
+      CachedStatics.with_targets(~settings, editor.state.zipper, statics);
+
+    let ctx_init: Ctx.t = Builtins.ctx_init(Some(Int));
+
+    // Track the current sample focus state
+    let current_sample_focus = editor.state.zipper.refractors.sample_focus;
+    let sample_focus_calc =
+      Calc.set(~eq=Sample.Focus.equal, current_sample_focus, sample_focus);
+
+    let live_typing =
+      if (settings.live_typing) {
+        Calc.Syntax.(
+          live_typing
+          |> {
+            let.calc dyn = dynamics
+            and.calc curr_sample_focus = sample_focus_calc;
+
+            let filtered_dynamics =
+              Language.Dynamics.filter_by_focus(curr_sample_focus, dyn);
+
+            let dynamic_expressions: Id.Map.t(LiveTyping.Map.entry) =
+              Id.Map.map(
+                List.map((sample: Sample.t): LiveTyping.sample =>
+                  {exp: sample.value}
+                ),
+                filtered_dynamics.probe_map,
+              );
+
+            let type_inst_probes: Id.Map.t(LiveTyping.Map.type_inst_entry) =
+              Id.Map.map(
+                List.map(
+                  (inst: Dynamics.TypeInstantiation.t): LiveTyping.type_instantiation =>
+                  {
+                    tpat_id: inst.tpat_id,
+                    type_var: inst.type_var,
+                    instantiated_type: inst.instantiated_type,
+                  }
+                ),
+                filtered_dynamics.type_inst_map,
+              );
+
+            let (live_typing_info_map, _) =
+              Statics.mk(
+                ~dynamics={
+                  exp_probes: dynamic_expressions,
+                  type_inst_probes,
+                },
+                settings,
+                ctx_init,
+                statics.term,
+              );
+
+            let live_typing_error_ids =
+              StaticsBase.Map.error_ids(live_typing_info_map)
+              |> List.filter(id => !List.mem(id, statics.error_ids));
+
+            (live_typing_info_map, live_typing_error_ids);
+          }
+        );
+      } else {
+        Calc.set((StaticsBase.Map.empty, []), live_typing);
+      };
+
+    let statics: CachedStatics.t = {
+      ...statics,
+      live_typing_info_map: live_typing |> Calc.get_value |> fst,
+      live_typing_error_ids: live_typing |> Calc.get_value |> snd,
+    };
 
     let editor =
       Editor.Update.calculate(
         ~settings,
         ~autoprobe_mode,
         ~is_edited,
         statics,
-        dynamics,
+        dynamics_map |> Calc.get_value,
         editor,
       );
-
-    /* Refresh `statics.targets` against the post-probe-effects refractors.
-     * Cheap O(|probe_ids|) fold; only this field depends on refractors, so
-     * the rest of statics stays valid. */
-    let statics =
-      CachedStatics.with_targets(~settings, editor.state.zipper, statics);
     {
       editor,
       statics,
-      dynamics,
+      dynamics: Calc.get_value(dynamics),
+      live_typing: Calc.save(live_typing),
+      sample_focus: Calc.save(sample_focus_calc),
       context_menu,
     };
   };
```

</details>

<details>
<summary><code>src/web/app/editors/code/CodeWithStatics.re</code> · Drop a stale comment</summary>

<!-- changetour:hunk file="src/web/app/editors/code/CodeWithStatics.re" baseBlob="ddc0a7dbe6b6c0df5ad7a2b7f250e81af94918d9" -->

```diff
@@ -219,7 +335,7 @@ module View = {
         ~term_data,
         ~buffer_ids=Selection.is_buffer(z.selection) ? selection_ids : [],
         ~shape_map,
-        ~refractor_shape_map=Id.Map.empty, //Id.Map.map(_ => 2, z.refractors.map),
+        ~refractor_shape_map=Id.Map.empty,
         ~refine_sort,
         segment,
       );
```

</details>

<details open>
<summary><code>src/web/app/editors/code/CodeWithStatics.re</code> · Render live-typing error decorations alongside errors/warnings</summary>

<!-- changetour:hunk file="src/web/app/editors/code/CodeWithStatics.re" baseBlob="ddc0a7dbe6b6c0df5ad7a2b7f250e81af94918d9" -->

```diff
@@ -234,20 +350,29 @@ module View = {
       globals.settings.core.display_warnings ? model.statics.warning_ids : [];
     let warning_decos =
       Arms.Errors.of_ids(
+        ~kind=Warning,
         ~refine_sort,
-        ~is_warning=true,
         ~font_metrics=globals.font_metrics,
         ~syntax=model.editor.syntax,
         warning_ids,
       );
+    let live_typing_decos =
+      Arms.Errors.of_ids(
+        ~kind=LiveTypingError,
+        ~refine_sort,
+        ~font_metrics=globals.font_metrics,
+        ~syntax=model.editor.syntax,
+        model.statics.live_typing_error_ids,
+      );
     let container_classes =
       ["code-container"]
       @ (globals.meta_down ? ["meta-down"] : [])
       @ (globals.settings.show_row_lines ? ["show-row-lines"] : []);
     Node.div(
       ~attrs=[Attr.classes(container_classes)],
       // errors after warnings to prioritize errors over warnings
-      [code_text_view, warning_decos, error_decos] @ overlays,
+      [code_text_view, warning_decos, error_decos, live_typing_decos]
+      @ overlays,
     );
   };
 };
```

</details>

<details>
<summary><code>src/util/Calc.re</code> · <code>Calc.map</code> (with a non-incrementality warning)</summary>

<!-- changetour:hunk file="src/util/Calc.re" baseBlob="b501716189d45cc3a7903870aeedf27bcb58e1b7" -->

```diff
@@ -60,6 +60,13 @@ let is_new = (x: t('a)): bool =>
   | NewValue(_) => true
   };
 
+// WARNING: This function is applied on old and new values alike. So it's not incremental.
+let map = (x: t('a), f: 'a => 'b): t('b) =>
+  switch (x) {
+  | OldValue(x) => OldValue(f(x))
+  | NewValue(x) => NewValue(f(x))
+  };
+
 let old_if_same = (~eq: ('a, 'a) => bool=(==), x: 'a, y: t('a)): t('a) =>
   switch (y) {
   | NewValue(y) when eq(x, y) => OldValue(x)
```

</details>

Feeding that pass required reshaping `EvalResult`: it previously cached a separate `dynamics` field derived from the evaluation result; that field is gone, and `dynamics` is now *derived on demand* from `result` via the new `Calc.map`, with `probe_results` / `test_results` / `type_inst_map` / `dynamics_full` as projections. The value flowing into editor calculation changes from a bare probe map to `Calc.t(Dynamics.t)` — preserving old/new provenance so the live pass can tell whether dynamics changed — which `CellEditor`, `CodeEditable`, `StepperEditor`, and `Theorems` all adapt to.

<details>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · Drop the cached <code>dynamics</code> field</summary>

<!-- changetour:hunk file="src/web/app/editors/result/EvalResult.re" baseBlob="f20ee423b901ee8a1fcf114dfafd1147c1e2a6f8" -->

```diff
@@ -21,7 +21,6 @@ module Model = {
     elab: Calc.saved(Exp.t),
     cached_targets: Calc.saved(Sample.targets), /* Input targets for cache invalidation */
     result: Calc.t(ProgramResult.t(ProgramResult.inner)),
-    dynamics: Calc.saved(option(Dynamics.t)),
     incr_eval: Calc.saved(IncrEval.t),
     display,
     theorems: Theorems.Model.t,
```

</details>

<details>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · Init without it</summary>

<!-- changetour:hunk file="src/web/app/editors/result/EvalResult.re" baseBlob="f20ee423b901ee8a1fcf114dfafd1147c1e2a6f8" -->

```diff
@@ -38,7 +37,6 @@ module Model = {
     elab: Calc.Pending,
     cached_targets: Calc.Pending,
     result: Calc.NewValue(ProgramResult.ResultPending),
-    dynamics: Calc.Pending,
     incr_eval: Calc.Pending,
     display: Evaluation(Calc.Pending),
     theorems: Theorems.Model.init,
```

</details>

<details>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · Unpersist without it</summary>

<!-- changetour:hunk file="src/web/app/editors/result/EvalResult.re" baseBlob="f20ee423b901ee8a1fcf114dfafd1147c1e2a6f8" -->

```diff
@@ -61,7 +59,6 @@ module Model = {
         elab: Calc.Pending,
         cached_targets: Calc.Pending,
         result: Calc.NewValue(ProgramResult.ResultPending),
-        dynamics: Calc.Pending,
         incr_eval: Calc.Pending,
         display: Stepper(StepperView.Model.unpersist(stepper)),
         theorems,
```

</details>

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · Derive <code>dynamics</code> from <code>result</code>; projection accessors</summary>

<!-- changetour:hunk file="src/web/app/editors/result/EvalResult.re" baseBlob="f20ee423b901ee8a1fcf114dfafd1147c1e2a6f8" -->

```diff
@@ -73,21 +70,53 @@ module Model = {
     };
   };
 
-  let probe_results = (model: t): option(Sample.Map.t) =>
-    model.dynamics
-    |> Calc.get_saved(None)
-    |> Option.map((d: Dynamics.t) => d.probe_map);
-
-  let test_results = (model: t): option(TestResults.t) =>
-    model.dynamics
-    |> Calc.get_saved(None)
-    |> Option.map((d: Dynamics.t) => d.test_results);
-
-  let dynamics = (model: t): Dynamics.Map.t =>
-    switch (probe_results(model)) {
-    | Some(dynamics_map) => Dynamics.Map.mk(dynamics_map)
-    | None => Dynamics.Map.mk(Sample.Map.empty)
-    };
+  let dynamics = (model: t) =>
+    model.result
+    |> Calc.map(
+         _,
+         fun
+         | ProgramResult.ResultPending => None
+         | ProgramResult.ResultFail(_) => None
+         | ProgramResult.ResultOk({state, _}) =>
+           Some(
+             Dynamics.{
+               probe_map: state |> EvaluatorState.get_probes,
+               test_results:
+                 state |> EvaluatorState.get_tests |> TestResults.mk_results,
+               type_inst_map: state |> EvaluatorState.get_type_insts,
+               theorems: state |> EvaluatorState.get_theorems,
+             },
+           ),
+       );
+
+  let probe_results = (model: t): Calc.t(option(Dynamics.Map.t)) =>
+    model
+    |> dynamics
+    |> Calc.map(_, Option.map((d: Dynamics.t) => d.probe_map));
+
+  let test_results = (model: t): Calc.t(option(TestResults.t)) =>
+    model
+    |> dynamics
+    |> Calc.map(_, Option.map((d: Dynamics.t) => d.test_results));
+  let type_inst_map = (model: t): Calc.t(Dynamics.TypeInstMap.t) =>
+    model
+    |> dynamics
+    |> Calc.map(_, s =>
+         switch (s) {
+         | Some(d) => d.type_inst_map
+         | None => Dynamics.TypeInstMap.empty
+         }
+       );
+
+  let dynamics_full = (model: t): Calc.t(Dynamics.t) =>
+    model
+    |> dynamics
+    |> Calc.map(_, s =>
+         switch (s) {
+         | Some(m) => m
+         | None => Dynamics.empty
+         }
+       );
 
   let incr_eval = (model: t): IncrEval.t =>
     model.incr_eval |> Calc.get_saved(IncrEval.empty);
```

</details>

<details>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · Destructure without the field</summary>

<!-- changetour:hunk file="src/web/app/editors/result/EvalResult.re" baseBlob="f20ee423b901ee8a1fcf114dfafd1147c1e2a6f8" -->

```diff
@@ -175,7 +204,6 @@ module Update = {
           elab,
           cached_targets,
           result,
-          dynamics,
           incr_eval,
           display,
           theorems,
```

</details>

<details>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · Annotate the result calculation</summary>

<!-- changetour:hunk file="src/web/app/editors/result/EvalResult.re" baseBlob="f20ee423b901ee8a1fcf114dfafd1147c1e2a6f8" -->

```diff
@@ -205,7 +233,8 @@ module Update = {
         ~probe_all=Calc.get_value(settings).probe_all,
         statics.info_map,
       );
-    let result =
+    // Calculate the result
+    let result: Calc.t(ProgramResult.t(ProgramResult.inner)) =
       result
       |> {
         let.calc_t elab = elab
```

</details>

<details>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · Remove the old state-to-dynamics conversion (now in <code>Model.dynamics</code>)</summary>

<!-- changetour:hunk file="src/web/app/editors/result/EvalResult.re" baseBlob="f20ee423b901ee8a1fcf114dfafd1147c1e2a6f8" -->

```diff
@@ -246,26 +275,6 @@ module Update = {
         };
       };
 
-    // Turn state into dynamics map
-    let dynamics =
-      dynamics
-      |> {
-        let.calc result = result;
-        switch (result) {
-        | ProgramResult.ResultPending => dynamics |> Calc.get_saved(None)
-        | ProgramResult.ResultFail(_) => dynamics |> Calc.get_saved(None)
-        | ProgramResult.ResultOk({state, _}) =>
-          Some(
-            Dynamics.{
-              probe_map: state |> EvaluatorState.get_probes,
-              test_results:
-                state |> EvaluatorState.get_tests |> TestResults.mk_results,
-              theorems: state |> EvaluatorState.get_theorems,
-            },
-          )
-        };
-      };
-
     let incr_eval =
       incr_eval
       |> {
```

</details>

<details>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · Statics freshness hack now keys off <code>result</code></summary>

<!-- changetour:hunk file="src/web/app/editors/result/EvalResult.re" baseBlob="f20ee423b901ee8a1fcf114dfafd1147c1e2a6f8" -->

```diff
@@ -311,7 +320,7 @@ module Update = {
                    ~settings=settings |> Calc.get_value,
                    ~is_dynamic_term=true,
                    ~stitch=_ => exp,
-                   ~dynamics=Dynamics.Map.empty,
+                   ~dynamics=Calc.OldValue(Dynamics.empty),
                    ~is_edited=is_edited || result_changed,
                    editor,
                  ),
```

</details>

<details>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · Theorems consume <code>result</code> directly</summary>

<!-- changetour:hunk file="src/web/app/editors/result/EvalResult.re" baseBlob="f20ee423b901ee8a1fcf114dfafd1147c1e2a6f8" -->

```diff
@@ -339,15 +348,14 @@ module Update = {
 
     // HACK[Matt]: say that statics is updated iff dynamics is updated
     let statics: Calc.t('a) =
-      switch (dynamics) {
+      switch (result) {
       | NewValue(_) => NewValue(statics)
       | OldValue(_) => OldValue(statics)
       };
 
     let theorems =
       Calc.get_value(settings).dynamics
-        ? theorems
-          |> Theorems.Update.calculate(~settings, ~statics, ~dynamics)
+        ? theorems |> Theorems.Update.calculate(~settings, ~statics, ~result)
         : theorems;
 
     (
```

</details>

<details>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · Save without the field</summary>

<!-- changetour:hunk file="src/web/app/editors/result/EvalResult.re" baseBlob="f20ee423b901ee8a1fcf114dfafd1147c1e2a6f8" -->

```diff
@@ -356,7 +364,6 @@ module Update = {
         elab: elab |> Calc.save,
         cached_targets: targets |> Calc.save,
         result: result |> Calc.make_old,
-        dynamics: dynamics |> Calc.save,
         incr_eval: incr_eval |> Calc.save,
         display,
         theorems,
```

</details>

<details>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · Probe overlay reads <code>.probe_map</code></summary>

<!-- changetour:hunk file="src/web/app/editors/result/EvalResult.re" baseBlob="f20ee423b901ee8a1fcf114dfafd1147c1e2a6f8" -->

```diff
@@ -452,7 +459,7 @@ module View = {
                 focus: selected ? Some() : None,
               }),
             ~globals,
-            ~dynamics=editor.dynamics,
+            ~dynamics=editor.dynamics.probe_map,
             editor,
           ),
         editor,
```

</details>

<details>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · Test overlay unwraps the Calc</summary>

<!-- changetour:hunk file="src/web/app/editors/result/EvalResult.re" baseBlob="f20ee423b901ee8a1fcf114dfafd1147c1e2a6f8" -->

```diff
@@ -593,7 +600,7 @@ module View = {
         result_kind == `JustTheorems
           ? [] : footer(~globals, ~signal, ~inject, ~selected, ~locked, model);
       let test_overlay = (editor: Haz3lcore.Editor.t) =>
-        switch (Model.test_results(model)) {
+        switch (Model.test_results(model) |> Calc.get_value) {
         | Some(result) => [
             test_result_layer(
               ~font_metrics=globals.font_metrics,
```

</details>

<details>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · Same, deferred-overlay path</summary>

<!-- changetour:hunk file="src/web/app/editors/result/EvalResult.re" baseBlob="f20ee423b901ee8a1fcf114dfafd1147c1e2a6f8" -->

```diff
@@ -653,7 +660,7 @@ module View = {
         [node],
         (
           (editor: Haz3lcore.Editor.t) =>
-            switch (Model.test_results(model)) {
+            switch (Model.test_results(model) |> Calc.get_value) {
             | Some(result) => [
                 test_result_layer(
                   ~font_metrics=globals.font_metrics,
```

</details>

<details>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · Same, school-mode path</summary>

<!-- changetour:hunk file="src/web/app/editors/result/EvalResult.re" baseBlob="f20ee423b901ee8a1fcf114dfafd1147c1e2a6f8" -->

```diff
@@ -668,9 +675,9 @@ module View = {
 
     // Just showing test results (school mode)
     | `TestResults =>
-      let test_results = Model.test_results(model);
+      let test_results = Model.test_results(model) |> Calc.get_value;
       let test_overlay = (editor: Haz3lcore.Editor.t) =>
-        switch (Model.test_results(model)) {
+        switch (Model.test_results(model) |> Calc.get_value) {
         | Some(result) => [
             test_result_layer(
               ~font_metrics=globals.font_metrics,
```

</details>

<details>
<summary><code>src/web/app/editors/cell/CellEditor.re</code> · Open <code>Util</code></summary>

<!-- changetour:hunk file="src/web/app/editors/cell/CellEditor.re" baseBlob="89cab0452d8522e804510ada828d4220676dfd3b" -->

```diff
@@ -1,3 +1,4 @@
+open Util;
 open Haz3lcore;
 open Virtual_dom.Vdom;
 open Node;
```

</details>

<details open>
<summary><code>src/web/app/editors/cell/CellEditor.re</code> · Init with full <code>Dynamics.empty</code> + pending Calc fields</summary>

<!-- changetour:hunk file="src/web/app/editors/cell/CellEditor.re" baseBlob="89cab0452d8522e804510ada828d4220676dfd3b" -->

```diff
@@ -16,8 +17,10 @@ module Model = {
     editor: {
       editor,
       statics: CachedStatics.empty,
-      dynamics: Language.Dynamics.Map.empty,
+      dynamics: Language.Dynamics.empty,
       context_menu: None,
+      live_typing: Pending,
+      sample_focus: Pending,
     },
     result: EvalResult.Model.init,
   };
```

</details>

<details open>
<summary><code>src/web/app/editors/cell/CellEditor.re</code> · Editor calculation consumes <code>dynamics_full</code> (a Calc, pre-result)</summary>

<!-- changetour:hunk file="src/web/app/editors/cell/CellEditor.re" baseBlob="89cab0452d8522e804510ada828d4220676dfd3b" -->

```diff
@@ -116,12 +119,13 @@ module Update = {
         ~is_edited,
         ~statics_mode,
         ~stitch,
-        ~dynamics=EvalResult.Model.dynamics(result),
+        ~dynamics=EvalResult.Model.dynamics_full(result),
         ~is_dynamic_term=false,
         editor,
       );
     /* Save probe results reference before result calculation */
-    let probes_before = EvalResult.Model.probe_results(result);
+    let probes_before =
+      EvalResult.Model.probe_results(result) |> Calc.get_value;
     /* Calculate result (may produce new dynamics from worker) */
     let result =
       EvalResult.Update.calculate(
```

</details>

<details>
<summary><code>src/web/app/editors/cell/CellEditor.re</code> · Probe-change detection unwraps the Calc</summary>

<!-- changetour:hunk file="src/web/app/editors/cell/CellEditor.re" baseBlob="89cab0452d8522e804510ada828d4220676dfd3b" -->

```diff
@@ -136,7 +140,8 @@ module Update = {
       );
     /* Detect if dynamics changed (ensures cursor aligns with render-time dynamics).
      * Compare inner maps, not Option wrappers (Option.map creates new Some each call) */
-    let probes_after = EvalResult.Model.probe_results(result);
+    let probes_after =
+      EvalResult.Model.probe_results(result) |> Calc.get_value;
     let dynamics_changed =
       switch (probes_before, probes_after) {
       | (None, None) => false
```

</details>

<details>
<summary><code>src/web/app/editors/cell/CellEditor.re</code> · Re-resolve cursor with <code>dynamics_full</code></summary>

<!-- changetour:hunk file="src/web/app/editors/cell/CellEditor.re" baseBlob="89cab0452d8522e804510ada828d4220676dfd3b" -->

```diff
@@ -159,7 +164,7 @@ module Update = {
           ~autoprobe_mode,
           ~is_edited=false, /* Not an edit, just resolving pending focus/cursor */
           ~stitch,
-          ~dynamics=EvalResult.Model.dynamics(result),
+          ~dynamics=EvalResult.Model.dynamics_full(result),
           ~is_dynamic_term=false,
           editor,
         );
```

</details>

<details>
<summary><code>src/web/app/editors/cell/CellEditor.re</code> · View reads probe results out of the Calc</summary>

<!-- changetour:hunk file="src/web/app/editors/cell/CellEditor.re" baseBlob="89cab0452d8522e804510ada828d4220676dfd3b" -->

```diff
@@ -278,7 +283,10 @@ module View = {
                 }),
           ~overlays=overlays(model.editor.editor),
           ~lines,
-          ~dynamics=EvalResult.Model.dynamics(model.result),
+          ~dynamics=
+            EvalResult.Model.probe_results(model.result)
+            |> Util.Calc.get_value
+            |> Option.value(~default=Language.Dynamics.Map.empty),
           ~incr_eval=EvalResult.Model.incr_eval(model.result),
           model.editor,
         ),
```

</details>

<details>
<summary><code>src/web/app/editors/code/CodeEditable.re</code> · Perform actions against <code>.probe_map</code></summary>

<!-- changetour:hunk file="src/web/app/editors/code/CodeEditable.re" baseBlob="91a5c605531b2911c468a9b78b83d6e9019bab76" -->

```diff
@@ -35,7 +35,7 @@ module Update = {
         ~settings=settings.core,
         action,
         model.statics,
-        model.dynamics,
+        model.dynamics.probe_map,
         model.editor,
       )
       |> (
```

</details>

<details>
<summary><code>src/web/app/editors/code/CodeEditable.re</code> · Preserve the new model fields on update</summary>

<!-- changetour:hunk file="src/web/app/editors/code/CodeEditable.re" baseBlob="91a5c605531b2911c468a9b78b83d6e9019bab76" -->

```diff
@@ -46,6 +46,8 @@ module Update = {
             statics: model.statics,
             dynamics: model.dynamics,
             context_menu: None,
+            live_typing: model.live_typing,
+            sample_focus: model.sample_focus,
           }
         | Error(err) => raise(Action.Failure.Exception(err))
       )
```

</details>

<details>
<summary><code>src/web/app/editors/result/StepperEditor.re</code> · Signature: <code>Calc.t(Dynamics.t)</code></summary>

<!-- changetour:hunk file="src/web/app/editors/result/StepperEditor.re" baseBlob="6b0548f03b70ffb41848929de7e723464dd2d22c" -->

```diff
@@ -39,7 +39,7 @@ module Update = {
         ~settings,
         ~is_edited,
         ~stitch,
-        ~dynamics: Language.Dynamics.Map.t,
+        ~dynamics: Calc.t(Language.Dynamics.t),
         ~ana,
         {editor, taken_steps, next_steps, refls}: Model.t,
       )
```

</details>

<details open>
<summary><code>src/web/app/editors/result/Theorems.re</code> · Theorems take <code>~result</code> instead of <code>~dynamics</code></summary>

<!-- changetour:hunk file="src/web/app/editors/result/Theorems.re" baseBlob="faa920a10396851f0e6c5e5bb7efb0bef0793eb0" -->

```diff
@@ -148,7 +148,7 @@ module Update = {
       (
         ~settings: Calc.t(CoreSettings.t),
         ~statics: Calc.t(Haz3lcore.CachedStatics.t),
-        ~dynamics: Calc.t(option(Dynamics.t)),
+        ~result: Calc.t(ProgramResult.t(ProgramResult.inner)),
         {thm_map, thms}: Model.t,
       ) => {
     let settings' = {
```

</details>

<details>
<summary><code>src/web/app/editors/result/Theorems.re</code> · Read theorems from evaluator state</summary>

<!-- changetour:hunk file="src/web/app/editors/result/Theorems.re" baseBlob="faa920a10396851f0e6c5e5bb7efb0bef0793eb0" -->

```diff
@@ -167,11 +167,11 @@ module Update = {
     let thms =
       thms
       |> {
-        let.calc dynamics = dynamics;
+        let.calc result = result;
         let theorems =
-          switch (dynamics) {
-          | None => []
-          | Some(d) => d.theorems
+          switch (result) {
+          | ResultOk({state, _}) => state.theorems
+          | _ => []
           };
         let theorems =
           List.map(
```

</details>

<details>
<summary><code>src/web/app/editors/result/Theorems.re</code> · Same for the stepper map</summary>

<!-- changetour:hunk file="src/web/app/editors/result/Theorems.re" baseBlob="faa920a10396851f0e6c5e5bb7efb0bef0793eb0" -->

```diff
@@ -187,12 +187,12 @@ module Update = {
 
     // Calculate visible steppers
     let thm_map =
-      dynamics
+      result
       |> Calc.get_value
       |> (
         fun
-        | None => []
-        | Some(x) => x.theorems
+        | ResultOk({state, _}) => state.theorems
+        | _ => []
       )
       |> List.map(((a, b, c, d)) => {
            let d' =
```

</details>

## Surfacing feedback in the UI

The cursor inspector is the main consumer. The cursor record gains `live_typing_info` — the indicated term's info from the live pass. `view_type` compares static vs. dynamic synthesized types; when they differ, `PadIds.compute_dynamic_ids` tags exactly the differing tiles with a `dynamic` CSS class (which is why `CodeViewable` learns an optional per-id `~classes` hook). When the static info is clean but the live pass reports an error, the inspector displays the dynamic info instead, flagged with a ⚡ badge whose tooltip explains the error comes from observed runtime types.

<details open>
<summary><code>src/web/app/Cursor.re</code> · Cursor carries <code>live_typing_info</code></summary>

<!-- changetour:hunk file="src/web/app/Cursor.re" baseBlob="adc729f2250d95abbb3beb3d7c0e67c116bb2965" -->

```diff
@@ -2,6 +2,7 @@ open Haz3lcore;
 open Language;
 type cursor('update) = {
   info: option(Info.t),
+  live_typing_info: option(Info.t),
   dynamics: option(list(Sample.t)),
   selected_text: option(unit => string),
   selection: option(Segment.t),
```

</details>

<details>
<summary><code>src/web/app/Cursor.re</code> · Empty cursor</summary>

<!-- changetour:hunk file="src/web/app/Cursor.re" baseBlob="adc729f2250d95abbb3beb3d7c0e67c116bb2965" -->

```diff
@@ -35,6 +36,7 @@ let map_opt = (f: 'a => option('b), cursor) => {
 
 let empty = {
   info: None,
+  live_typing_info: None,
   dynamics: None,
   selected_text: None,
   selection: None,
```

</details>

<details>
<summary><code>src/web/app/inspector/CursorInspector.re</code> · <code>term_view</code> takes <code>~is_live_typing_error</code></summary>

<!-- changetour:hunk file="src/web/app/inspector/CursorInspector.re" baseBlob="821b656286ee534a1788ab05702fe57f8dc3ab78" -->

```diff
@@ -54,7 +54,8 @@ let ctx_toggle = (~globals: Globals.t): Node.t =>
     //[text("Γ")],
   );
 
-let term_view = (~globals: Globals.t, ~force_error=false, ci) => {
+let term_view =
+    (~globals: Globals.t, ~is_live_typing_error=false, ~force_error=false, ci) => {
   /* Drv(_) sorts have verbose type-level names like "DrvJdmt"/"DrvProp"
      via Sort.to_string (needed for pretty-printing `DrvQuoteTy`). For the
      inspector header we prefer the terse form ("Jdmt", "Prop", ...),
```

</details>

<details open>
<summary><code>src/web/app/inspector/CursorInspector.re</code> · The ⚡ badge</summary>

<!-- changetour:hunk file="src/web/app/inspector/CursorInspector.re" baseBlob="821b656286ee534a1788ab05702fe57f8dc3ab78" -->

```diff
@@ -83,6 +84,17 @@ let term_view = (~globals: Globals.t, ~force_error=false, ci) => {
     ],
     [
       ctx_toggle(~globals),
+      is_live_typing_error
+        ? div(
+            ~attrs=[
+              Attr.title(
+                "Live typing error - this error is based on the actual types observed during program evaluation, which fill in unknown static types",
+              ),
+              clss(["dynamic-icon"]),
+            ],
+            [text({js|⚡|js})],
+          )
+        : div_empty,
       div(~attrs=[clss(["term-tag"])], [text(sort_text)]),
       div(~attrs=[clss(["divider"])], [text("/")]),
       cls_view(ci),
```

</details>

<details open>
<summary><code>src/web/app/inspector/CursorInspector.re</code> · <code>view_type</code> highlights tiles where the dynamic type differs</summary>

<!-- changetour:hunk file="src/web/app/inspector/CursorInspector.re" baseBlob="821b656286ee534a1788ab05702fe57f8dc3ab78" -->

```diff
@@ -119,10 +131,23 @@ let view_any = (~globals, any: Any.t) =>
   |> CodeViewable.view_any(~globals, ~settings=code_view_settings)
   |> code_box_container;
 
-let view_type = (~globals, typ: Typ.t) =>
-  typ
-  |> CodeViewable.view_typ(~globals, ~settings=code_view_settings)
+let view_type = (~globals, ~live_typing_info: option(Info.t)=?, typ: Typ.t) => {
+  let dyn_type =
+    switch (live_typing_info) {
+    | Some(InfoExp({elab_syn_ty, _})) => Some(elab_syn_ty)
+    | Some(InfoPat({elab_syn_ty, _})) => Some(elab_syn_ty)
+    | _ => None
+    };
+  let (classes, display_typ) =
+    switch (dyn_type) {
+    | Some(dynamic_typ) when !Typ.fast_equal(typ, dynamic_typ) =>
+      Haz3lcore.PadIds.compute_dynamic_ids(~static_typ=typ, ~dynamic_typ, ())
+    | _ => ((_ => []), typ)
+    };
+  display_typ
+  |> CodeViewable.view_typ(~globals, ~settings=code_view_settings, ~classes)
   |> code_box_container;
+};
 
 let core_mark_err_view =
     (
```

</details>

<details>
<summary><code>src/web/app/inspector/CursorInspector.re</code> · <code>common_ok_view</code>: dynamic-aware view for synthesized types only</summary>

<!-- changetour:hunk file="src/web/app/inspector/CursorInspector.re" baseBlob="821b656286ee534a1788ab05702fe57f8dc3ab78" -->

```diff
@@ -305,9 +330,11 @@ let common_ok_view =
       ~lifted_ty: option(Typ.t),
       ~inferred_label: option(LabeledTuple.label),
       ~label_sort: bool,
+      ~live_typing_info: option(Info.t)=None,
       cls: Cls.t,
       ok: Message.ok_common,
     ) => {
+  let view_syn_type = view_type(~globals, ~live_typing_info?);
   let view_type = view_type(~globals);
   (
     switch (cls, ok) {
```

</details>

<details>
<summary><code>src/web/app/inspector/CursorInspector.re</code> · Use it for Syn</summary>

<!-- changetour:hunk file="src/web/app/inspector/CursorInspector.re" baseBlob="821b656286ee534a1788ab05702fe57f8dc3ab78" -->

```diff
@@ -335,7 +362,7 @@ let common_ok_view =
     | (_, Syn(syn)) =>
       switch (syn.term) {
       | Label(l) => [label_view(l)]
-      | _ => colon_prefix(show_type_colon) @ [view_type(syn)]
+      | _ => colon_prefix(show_type_colon) @ [view_syn_type(syn)]
       }
     | (Pat(Var) | Pat(Wild) | Pat(ApFunc), Ana(Consistent({ana, _}))) =>
       /* Pat(ApFunc) is only produced by the `let f(args) = ...` function
```

</details>

<details>
<summary><code>src/web/app/inspector/CursorInspector.re</code> · …and consistent-Ana</summary>

<!-- changetour:hunk file="src/web/app/inspector/CursorInspector.re" baseBlob="821b656286ee534a1788ab05702fe57f8dc3ab78" -->

```diff
@@ -348,7 +375,7 @@ let common_ok_view =
       | Label(l) => [label_view(l), text(" is a valid label")]
       | _ =>
         colon_prefix(show_type_colon)
-        @ [view_type(syn)]
+        @ [view_syn_type(syn)]
         @ [text("equals expected type")]
         @ (
           switch (lifted_ty) {
```

</details>

<details>
<summary><code>src/web/app/inspector/CursorInspector.re</code> · …and consistent-with-expected</summary>

<!-- changetour:hunk file="src/web/app/inspector/CursorInspector.re" baseBlob="821b656286ee534a1788ab05702fe57f8dc3ab78" -->

```diff
@@ -382,7 +409,7 @@ let common_ok_view =
         | Label(l) => [code(l), text(" is a valid label")]
         | _ =>
           colon_prefix(show_type_colon)
-          @ [view_type(syn), text("consistent with expected type")]
+          @ [view_syn_type(syn), text("consistent with expected type")]
         }
       )
       @ [view_type(ana)]
```

</details>

<details>
<summary><code>src/web/app/inspector/CursorInspector.re</code> · Thread through <code>exp_view</code></summary>

<!-- changetour:hunk file="src/web/app/inspector/CursorInspector.re" baseBlob="821b656286ee534a1788ab05702fe57f8dc3ab78" -->

```diff
@@ -770,6 +797,7 @@ let exp_view =
     (
       ~globals,
       ~show_type_colon=true,
+      ~live_typing_info: option(Info.t)=None,
       cls: Cls.t,
       message: Message.t,
       info: Info.exp,
```

</details>

<details>
<summary><code>src/web/app/inspector/CursorInspector.re</code> · …to Syn messages</summary>

<!-- changetour:hunk file="src/web/app/inspector/CursorInspector.re" baseBlob="821b656286ee534a1788ab05702fe57f8dc3ab78" -->

```diff
@@ -806,6 +834,7 @@ let exp_view =
           ~introduced_labels,
           ~inferred_label,
           ~label_sort=info.label_sort,
+          ~live_typing_info,
           cls,
           Message.Syn(info.elab_syn_ty),
         ),
```

</details>

<details>
<summary><code>src/web/app/inspector/CursorInspector.re</code> · …and ok messages</summary>

<!-- changetour:hunk file="src/web/app/inspector/CursorInspector.re" baseBlob="821b656286ee534a1788ab05702fe57f8dc3ab78" -->

```diff
@@ -822,6 +851,7 @@ let exp_view =
           ~introduced_labels,
           ~inferred_label,
           ~label_sort=info.label_sort,
+          ~live_typing_info,
           cls,
           ok,
         ),
```

</details>

<details>
<summary><code>src/web/app/inspector/CursorInspector.re</code> · Thread through <code>pat_view</code></summary>

<!-- changetour:hunk file="src/web/app/inspector/CursorInspector.re" baseBlob="821b656286ee534a1788ab05702fe57f8dc3ab78" -->

```diff
@@ -916,6 +946,7 @@ let pat_view =
     (
       ~globals,
       ~show_type_colon=true,
+      ~live_typing_info: option(Info.t)=None,
       cls: Cls.t,
       message: Message.t,
       info: Info.pat,
```

</details>

<details>
<summary><code>src/web/app/inspector/CursorInspector.re</code> · …to pattern ok messages</summary>

<!-- changetour:hunk file="src/web/app/inspector/CursorInspector.re" baseBlob="821b656286ee534a1788ab05702fe57f8dc3ab78" -->

```diff
@@ -960,6 +991,7 @@ let pat_view =
           ~introduced_labels,
           ~inferred_label,
           ~label_sort=info.label_sort,
+          ~live_typing_info,
           cls,
           ok,
         );
```

</details>

<details>
<summary><code>src/web/app/inspector/CursorInspector.re</code> · <code>view_of_info</code> passes both along</summary>

<!-- changetour:hunk file="src/web/app/inspector/CursorInspector.re" baseBlob="821b656286ee534a1788ab05702fe57f8dc3ab78" -->

```diff
@@ -1044,17 +1076,21 @@ let tpat_view =
 
 let secondary_view = (cls: Cls.t) => div_ok([text(cls |> Cls.show)]);
 
-let view_of_info = (~globals, ci): list(Node.t) => {
-  let wrapper = status_view => [term_view(~globals, ci), status_view];
+let view_of_info =
+    (~globals, ~live_typing_info, ~is_live_typing_error, ci): list(Node.t) => {
+  let wrapper = status_view => [
+    term_view(~globals, ~is_live_typing_error, ci),
+    status_view,
+  ];
   switch (ci) {
   | Secondary(_) => wrapper(div([]))
   | InfoMod({cls, _}) => wrapper(div_ok([text(cls |> Cls.show)]))
   | InfoSig({cls, _}) => wrapper(div_ok([text(cls |> Cls.show)]))
   | InfoMPat({cls, _}) => wrapper(div_ok([text(cls |> Cls.show)]))
   | InfoExp({cls, message, _} as ie) =>
-    wrapper(exp_view(~globals, cls, message, ie))
+    wrapper(exp_view(~globals, ~live_typing_info, cls, message, ie))
   | InfoPat({cls, message, _} as ip) =>
-    wrapper(pat_view(~globals, cls, message, ip))
+    wrapper(pat_view(~globals, ~live_typing_info, cls, message, ip))
   | InfoTyp({cls, marks, message, _}) =>
     wrapper(typ_view(~globals, cls, ~marks, ~message))
   | InfoTPat({cls, marks, message, _}) =>
```

</details>

<details open>
<summary><code>src/web/app/inspector/CursorInspector.re</code> · Prefer dynamic info when only it has an error; style accordingly</summary>

<!-- changetour:hunk file="src/web/app/inspector/CursorInspector.re" baseBlob="821b656286ee534a1788ab05702fe57f8dc3ab78" -->

```diff
@@ -1063,19 +1099,38 @@ let view_of_info = (~globals, ci): list(Node.t) => {
   };
 };
 
-let inspector_view = (~globals: Globals.t, ci): Node.t =>
+let inspector_view = (~globals: Globals.t, ~live_typing_info, ci): Node.t => {
+  /* If the static info is error-free but the dynamic (live-typing) info
+     reports an error, show the dynamic info with the lightning badge. */
+  let (display_info, is_live_typing_error) =
+    if (Info.is_error(ci)) {
+      (ci, false);
+    } else {
+      switch (live_typing_info) {
+      | Some(di) when Info.is_error(di) => (di, true)
+      | _ => (ci, false)
+      };
+    };
   div(
     ~attrs=[
       Attr.id("cursor-inspector"),
       clss([
-        Info.is_error(ci)
+        Info.is_error(display_info)
           ? errc
-          : Info.is_warning(ci) && globals.settings.core.display_warnings
+          : Info.is_warning(display_info)
+            && globals.settings.core.display_warnings
               ? warnc : okc,
+        is_live_typing_error ? "live-typing-error" : "",
       ]),
     ],
-    view_of_info(~globals, ci),
+    view_of_info(
+      ~globals,
+      ~live_typing_info,
+      ~is_live_typing_error,
+      display_info,
+    ),
   );
+};
 
 let projector_error_inspector =
     (
```

</details>

<details>
<summary><code>src/web/app/inspector/CursorInspector.re</code> · Top-level view feeds <code>cursor.live_typing_info</code></summary>

<!-- changetour:hunk file="src/web/app/inspector/CursorInspector.re" baseBlob="821b656286ee534a1788ab05702fe57f8dc3ab78" -->

```diff
@@ -1119,7 +1174,14 @@ let view = (~globals: Globals.t, cursor: Cursor.cursor(Editors.Update.t)) => {
     switch (projector_err) {
     | Some((_, err)) when !Info.is_error(ci) =>
       bar_view([projector_error_inspector(~globals, ci, err)])
-    | _ => bar_view([inspector_view(~globals, ci)])
+    | _ =>
+      bar_view([
+        inspector_view(
+          ~globals,
+          ~live_typing_info=cursor.live_typing_info,
+          ci,
+        ),
+      ])
     }
   };
 };
```

</details>

<details>
<summary><code>src/web/app/editors/code/CodeViewable.re</code> · <code>view</code> accepts per-id <code>~classes</code></summary>

<!-- changetour:hunk file="src/web/app/editors/code/CodeViewable.re" baseBlob="63939e7dc2f3921c81b81e68dfe5c42a6ec80200" -->

```diff
@@ -13,6 +13,7 @@ let view =
       ~shape_map,
       ~refractor_shape_map,
       ~refine_sort: (Id.t, Sort.t) => Sort.t=(_, sort) => sort,
+      ~classes=(_: Id.t) => [],
       segment,
     )
     : Node.t => {
```

</details>

<details>
<summary><code>src/web/app/editors/code/CodeViewable.re</code> · Thread into segment view</summary>

<!-- changetour:hunk file="src/web/app/editors/code/CodeViewable.re" baseBlob="63939e7dc2f3921c81b81e68dfe5c42a6ec80200" -->

```diff
@@ -26,12 +27,14 @@ let view =
       ~term_data,
       ~refine_sort,
       ~buffer_ids,
+      ~classes,
       segment,
     );
   div_c("code", [span_c("code-text", code)]);
 };
 
-let view_segment = (~globals: Globals.t, segment: Segment.t) => {
+let view_segment =
+    (~globals: Globals.t, ~classes=(_: Id.t) => [], segment: Segment.t) => {
   let shape_map = ProjectorCore.Shape.Map.empty; // assume no projectors
   let refractor_shape_map = Id.Map.empty; //assume no refractors
   let term_data = TermData.empty; //assume no indication/selection decoratinos
```

</details>

<details open>
<summary><code>src/web/app/editors/code/CodeViewable.re</code> · <code>view_typ</code>/<code>view_any</code> expose the hook</summary>

<!-- changetour:hunk file="src/web/app/editors/code/CodeViewable.re" baseBlob="63939e7dc2f3921c81b81e68dfe5c42a6ec80200" -->

```diff
@@ -42,12 +45,29 @@ let view_segment = (~globals: Globals.t, segment: Segment.t) => {
     ~buffer_ids=[],
     ~shape_map,
     ~refractor_shape_map,
+    ~classes,
     segment,
   );
 };
 
-let view_typ = (~globals: Globals.t, ~settings, typ: Language.Typ.t) =>
-  typ |> ExpToSegment.typ_to_segment(~settings) |> view_segment(~globals);
+let view_typ =
+    (
+      ~globals: Globals.t,
+      ~settings,
+      ~classes=(_: Id.t) => [],
+      typ: Language.Typ.t,
+    ) =>
+  typ
+  |> ExpToSegment.typ_to_segment(~settings)
+  |> view_segment(~globals, ~classes);
 
-let view_any = (~globals: Globals.t, ~settings, any: Language.Any.t) =>
-  any |> ExpToSegment.any_to_segment(~settings) |> view_segment(~globals);
+let view_any =
+    (
+      ~globals: Globals.t,
+      ~settings,
+      ~classes=(_: Id.t) => [],
+      any: Language.Any.t,
+    ) =>
+  any
+  |> ExpToSegment.any_to_segment(~settings)
+  |> view_segment(~globals, ~classes);
```

</details>

<details>
<summary><code>src/web/www/style/cursor-inspector.css</code> · Badge + status-indicator styling</summary>

<!-- changetour:hunk file="src/web/www/style/cursor-inspector.css" baseBlob="a30680aee52043c66c2722f0d369827929c4eee5" -->

```diff
@@ -410,4 +410,57 @@
 
 .context-inspector .context-entry .seperator {
   color: var(--context-inspector-colon);
-}
\ No newline at end of file
+}
+
+.dynamic-icon {
+  display: inline-flex;
+  align-items: center;
+  justify-content: center;
+  height: 100%;
+}
+
+/* Status indicator - wrapper provides centering in corner area */
+.status-indicator {
+  display: flex;
+  align-items: center;
+  justify-content: center;
+  width: 2rem;
+  cursor: default;
+  user-select: none;
+}
+
+/* The circular badge itself */
+.status-indicator span {
+  display: flex;
+  align-items: center;
+  justify-content: center;
+  width: 16px;
+  height: 16px;
+  border-radius: 50%;
+  font-weight: 600;
+  color: white;
+  transition: background-color 0.2s ease;
+  scale: 0.8;
+}
+
+.status-indicator.no-errors span {
+  background-color: oklch(70% 0.15 150);
+  font-size: 11px;
+}
+
+.status-indicator.has-errors span {
+  background-color: oklch(40% 0.3 30);
+}
+
+/* Font size adjustments based on digit count */
+.status-indicator.digits-1 span {
+  font-size: 10px;
+}
+
+.status-indicator.digits-2 span {
+  font-size: 8px;
+}
+
+.status-indicator.digits-3 span {
+  font-size: 6px;
+}
```

</details>

Live typing errors also get their own problem category and code decoration. `ProblemCollection` adds a `LiveTyping` category populated from the new error-id list (re-validated against the live info map); the sidebar names, colors, and ranks it like a static error. `Arms.Errors` generalizes its `is_warning` boolean into a three-way `kind` so the decoration layer can render live-typing errors with a distinct pulsing stroke in a new accent color.

<details open>
<summary><code>src/haz3lcore/ProblemCollection.re</code> · New <code>LiveTyping</code> problem category</summary>

<!-- changetour:hunk file="src/haz3lcore/ProblemCollection.re" baseBlob="ac85763729c7f169a2eb8983ec87b3efcf679c75" -->

```diff
@@ -9,6 +9,7 @@ type problem_category =
   | Hole
   | Static
   | Warning
+  | LiveTyping
   | Projector;
 
 /* ---------- Problem data types ---------- */
```

</details>

<details>
<summary><code>src/haz3lcore/ProblemCollection.re</code> · Context carries the error list</summary>

<!-- changetour:hunk file="src/haz3lcore/ProblemCollection.re" baseBlob="ac85763729c7f169a2eb8983ec87b3efcf679c75" -->

```diff
@@ -33,6 +34,7 @@ type problem_context = {
   concave_holes: list(Grout.t),
   static_error_ids: list((Id.t, Info.t)),
   warning_ids: list((Id.t, Info.t)),
+  live_typing_error_ids: list((Id.t, Info.t)),
   projector_errors: list((Id.t, ProjectorKind.t, ProjectorBase.error)),
   segment: Segment.t,
   measured: Measured.t,
```

</details>

<details open>
<summary><code>src/haz3lcore/ProblemCollection.re</code> · Collect live-typing errors from the live info map</summary>

<!-- changetour:hunk file="src/haz3lcore/ProblemCollection.re" baseBlob="ac85763729c7f169a2eb8983ec87b3efcf679c75" -->

```diff
@@ -127,6 +129,16 @@ let make_problem_context =
     } else {
       [];
     };
+  /* Collect live typing error ids from dynamic statics */
+  let live_typing_error_ids =
+    List.filter_map(
+      id =>
+        switch (Statics.Map.lookup(id, statics.live_typing_info_map)) {
+        | Some(ci) when Info.is_error(ci) => Some((id, ci))
+        | _ => None
+        },
+      statics.live_typing_error_ids,
+    );
   /* Collect holes once and partition into convex (empty holes) and concave (missing operators) */
   let all_holes = Segment.holes(syntax.segment);
   let (hole_ids, concave_holes) =
```

</details>

<details>
<summary><code>src/haz3lcore/ProblemCollection.re</code> · Populate the context</summary>

<!-- changetour:hunk file="src/haz3lcore/ProblemCollection.re" baseBlob="ac85763729c7f169a2eb8983ec87b3efcf679c75" -->

```diff
@@ -149,6 +161,7 @@ let make_problem_context =
     concave_holes,
     static_error_ids,
     warning_ids,
+    live_typing_error_ids,
     projector_errors,
     segment: syntax.segment,
     measured,
```

</details>

<details>
<summary><code>src/haz3lcore/ProblemCollection.re</code> · Category collector</summary>

<!-- changetour:hunk file="src/haz3lcore/ProblemCollection.re" baseBlob="ac85763729c7f169a2eb8983ec87b3efcf679c75" -->

```diff
@@ -237,6 +250,16 @@ let collect_category =
            source: FromInfo(ci),
          }
        )
+  | LiveTyping =>
+    ctx.live_typing_error_ids
+    |> List.to_seq
+    |> Seq.map(((id, ci)) =>
+         {
+           id,
+           category: LiveTyping,
+           source: FromInfo(ci),
+         }
+       )
   | Projector =>
     ctx.projector_errors
     |> List.to_seq
```

</details>

<details>
<summary><code>src/haz3lcore/ProblemCollection.re</code> · Include in all-problems</summary>

<!-- changetour:hunk file="src/haz3lcore/ProblemCollection.re" baseBlob="ac85763729c7f169a2eb8983ec87b3efcf679c75" -->

```diff
@@ -252,7 +275,7 @@ let collect_category =
 /* ---------- Convenience: all problems ---------- */
 
 let collect_all_problems = (ctx: problem_context): list(problem) => {
-  [Syntax, Hole, Static, Warning, Projector]
+  [Syntax, Hole, Static, Warning, LiveTyping, Projector]
   |> List.concat_map(cat => collect_category(ctx, cat) |> List.of_seq);
 };
 
```

</details>

<details open>
<summary><code>src/web/app/sidebar/SidebarModel.re</code> · Sidebar mirrors the category enum</summary>

<!-- changetour:hunk file="src/web/app/sidebar/SidebarModel.re" baseBlob="6222cd12ec4f3b2f187afd36c95c52fdc3e03895" -->

```diff
@@ -13,7 +13,7 @@ module Settings = {
   [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
   type problem_category =
     Haz3lcore.ProblemCollection.problem_category =
-      | Syntax | Hole | Static | Warning | Projector;
+      | Syntax | Hole | Static | Warning | LiveTyping | Projector;
 
   /* Base CSS class for a category */
   let category_cls = cat =>
```

</details>

<details>
<summary><code>src/web/app/sidebar/SidebarModel.re</code> · CSS class</summary>

<!-- changetour:hunk file="src/web/app/sidebar/SidebarModel.re" baseBlob="6222cd12ec4f3b2f187afd36c95c52fdc3e03895" -->

```diff
@@ -22,6 +22,7 @@ module Settings = {
     | Hole => "hole"
     | Static => "static"
     | Warning => "warning"
+    | LiveTyping => "live-typing"
     | Projector => "projector-error"
     };
 
```

</details>

<details>
<summary><code>src/web/app/sidebar/SidebarModel.re</code> · Display name</summary>

<!-- changetour:hunk file="src/web/app/sidebar/SidebarModel.re" baseBlob="6222cd12ec4f3b2f187afd36c95c52fdc3e03895" -->

```diff
@@ -32,6 +33,7 @@ module Settings = {
     | Hole => "Holes"
     | Static => "Static Errors"
     | Warning => "Warnings"
+    | LiveTyping => "Live Typing Errors"
     | Projector => "Projector Errors"
     };
 
```

</details>

<details>
<summary><code>src/web/app/sidebar/SidebarModel.re</code> · Singular label</summary>

<!-- changetour:hunk file="src/web/app/sidebar/SidebarModel.re" baseBlob="6222cd12ec4f3b2f187afd36c95c52fdc3e03895" -->

```diff
@@ -42,6 +44,7 @@ module Settings = {
     | Hole => "Hole"
     | Static => "Static"
     | Warning => "Warning"
+    | LiveTyping => "Live Typing"
     | Projector => "Projector"
     };
 
```

</details>

<details>
<summary><code>src/web/app/sidebar/SidebarModel.re</code> · Severity ranks with errors</summary>

<!-- changetour:hunk file="src/web/app/sidebar/SidebarModel.re" baseBlob="6222cd12ec4f3b2f187afd36c95c52fdc3e03895" -->

```diff
@@ -50,7 +53,8 @@ module Settings = {
   let category_badge_severity = cat =>
     switch (cat) {
     | Syntax
-    | Static => 2
+    | Static
+    | LiveTyping => 2
     | Projector
     | Warning => 1
     | Hole => 0
```

</details>

<details>
<summary><code>src/web/app/sidebar/SidebarModel.re</code> · Tab-status class</summary>

<!-- changetour:hunk file="src/web/app/sidebar/SidebarModel.re" baseBlob="6222cd12ec4f3b2f187afd36c95c52fdc3e03895" -->

```diff
@@ -61,6 +65,7 @@ module Settings = {
     switch (cat) {
     | Syntax
     | Static => "has-errors"
+    | LiveTyping => "has-live-typing"
     | Projector
     | Warning => "has-warnings"
     | Hole => "has-holes"
```

</details>

<details>
<summary><code>src/web/app/sidebar/SidebarModel.re</code> · Tooltip noun</summary>

<!-- changetour:hunk file="src/web/app/sidebar/SidebarModel.re" baseBlob="6222cd12ec4f3b2f187afd36c95c52fdc3e03895" -->

```diff
@@ -71,6 +76,7 @@ module Settings = {
     switch (cat) {
     | Syntax
     | Static => "error"
+    | LiveTyping => "live typing error"
     | Projector
     | Warning => "warning"
     | Hole => "hole"
```

</details>

<details open>
<summary><code>src/web/app/editors/decoration/Arms.re</code> · <code>Errors.kind</code>: Error / Warning / LiveTypingError</summary>

<!-- changetour:hunk file="src/web/app/editors/decoration/Arms.re" baseBlob="fb72ddec01516b21f41a670b4b9d390bae6222d4" -->

```diff
@@ -375,16 +375,39 @@ let term_range = (~syntax: CachedSyntax.t, p: Piece.t) => {
 open Util.WebUtil;
 
 module Errors = {
+  type kind =
+    | Error
+    | Warning
+    | LiveTypingError;
+
+  let piece_cls =
+    fun
+    | Error => "errors-piece"
+    | Warning => "warnings-piece"
+    | LiveTypingError => "live-typing-errors-piece";
+
+  let group_cls =
+    fun
+    | Error => "errors"
+    | Warning => "warnings"
+    | LiveTypingError => "live-typing-errors";
+
+  let shard_cls =
+    fun
+    | Error
+    | LiveTypingError => "error"
+    | Warning => "warning";
+
   let of_id =
       (
+        ~kind=Error,
         ~refine_sort: (Id.t, Sort.t) => Sort.t=(_, sort) => sort,
-        ~is_warning=false,
         ~font_metrics: FontMetrics.t,
         ~syntax: CachedSyntax.t,
         id: Id.t,
       ) =>
     div_c(
-      is_warning ? "warnings-piece" : "errors-piece",
+      piece_cls(kind),
       switch (Id.Map.find_opt(id, syntax.projectors)) {
       | Some(p) =>
         /* Special case for projectors as they are not in tile map */
```

</details>

<details>
<summary><code>src/web/app/editors/decoration/Arms.re</code> · Shard class by kind</summary>

<!-- changetour:hunk file="src/web/app/editors/decoration/Arms.re" baseBlob="fb72ddec01516b21f41a670b4b9d390bae6222d4" -->

```diff
@@ -396,7 +419,7 @@ module Errors = {
                 tips: p |> ProjectorCore.shapes |> ShardDec.tips_of_shapes,
                 measurement,
               },
-              [is_warning ? "warning" : "error"],
+              [shard_cls(kind)],
             ),
           ]
         | None =>
```

</details>

<details>
<summary><code>src/web/app/editors/decoration/Arms.re</code> · <code>of_ids</code> takes the kind</summary>

<!-- changetour:hunk file="src/web/app/editors/decoration/Arms.re" baseBlob="fb72ddec01516b21f41a670b4b9d390bae6222d4" -->

```diff
@@ -416,16 +439,16 @@ module Errors = {
 
   let of_ids =
       (
+        ~kind=Error,
         ~refine_sort: (Id.t, Sort.t) => Sort.t=(_, sort) => sort,
-        ~is_warning=false,
         ~font_metrics: FontMetrics.t,
         ~syntax: CachedSyntax.t,
         error_ids,
       ) =>
     div_c(
-      is_warning ? "warnings" : "errors",
+      group_cls(kind),
       List.map(
-        of_id(~refine_sort, ~is_warning, ~font_metrics, ~syntax),
+        of_id(~kind, ~refine_sort, ~font_metrics, ~syntax),
         error_ids,
       ),
     );
```

</details>

<details open>
<summary><code>src/web/www/style/editor.css</code> · Pulsing live-typing error stroke</summary>

<!-- changetour:hunk file="src/web/www/style/editor.css" baseBlob="99778bad83a2faf63d58dd6c71a3d0e8440663a6" -->

```diff
@@ -358,21 +386,46 @@ svg.shard.selected-expanded > path {
   fill: var(--warning-hole-fill);
 }
 
+@keyframes stroke-pulse {
+  0%, 100% { stroke-dashoffset: 0; opacity: 1; }
+  50% { stroke-dashoffset: 4; opacity: 0.7; }
+}
+
+/* Live typing errors get a different color to distinguish them */
+.live-typing-errors .live-typing-errors-piece svg.shard {
+  z-index: var(--err-hole-z);
+  filter: none;
+}
+
+.live-typing-errors .live-typing-errors-piece svg.shard>path,
+.live-typing-errors .live-typing-errors-piece svg .child-line {
+  stroke: var(--live-typing-error-hole-stroke);
+  stroke-width: 0.75px;
+  stroke-dasharray: 1, 1;
+  stroke-linecap: butt;
+  animation: stroke-pulse 1.6s ease-in-out infinite;
+}
+.live-typing-errors .live-typing-errors-piece svg.shard>path {
+  fill: var(--error-hole-fill);
+}
+
+
 /* Code completion decoration */
 
 svg.shard.selected.buffer-unparsed {
   filter: none;
 }
-svg.shard.selected.buffer-unparsed > path {
+
+svg.shard.selected.buffer-unparsed>path {
   fill: #0000;
   stroke-width: 0;
 }
+
 svg.shard.selected.buffer-parsed {
-  filter: drop-shadow(
-    var(--off-x) var(--off-y) var(--blur) var(--shard-lines-exp)
-  );
+  filter: drop-shadow(var(--off-x) var(--off-y) var(--blur) var(--shard-lines-exp));
 }
-svg.shard.selected.buffer-parsed > path {
+
+svg.shard.selected.buffer-parsed>path {
   fill: var(--T1);
   stroke: var(--T1);
 }
```

</details>

<details>
<summary><code>src/web/www/style/sidebar.css</code> · Tab badge color</summary>

<!-- changetour:hunk file="src/web/www/style/sidebar.css" baseBlob="8df24ed7a42ba9465420cf880f258deb2d3b8dc8" -->

```diff
@@ -126,6 +126,10 @@
   font-size: 12px;
 }
 
+.tab-status-indicator.has-live-typing span {
+  background-color: var(--live-typing-stroke);
+}
+
 .tab-status-indicator.digits-1 span {
   font-size: 11px;
 }
```

</details>

<details>
<summary><code>src/web/www/style/sidebar.css</code> · Problem-row styling</summary>

<!-- changetour:hunk file="src/web/www/style/sidebar.css" baseBlob="8df24ed7a42ba9465420cf880f258deb2d3b8dc8" -->

```diff
@@ -327,6 +331,21 @@
   box-shadow: inset 0 0 0 1px var(--warning-hole-stroke);
 }
 
+.problem-row.live-typing {
+  border-left: 3px solid color-mix(in srgb, var(--live-typing-stroke) 60%, transparent);
+  background-color: color-mix(in srgb, var(--live-typing-fill) 50%, transparent);
+}
+
+.problem-row.live-typing:hover {
+  background-color: var(--live-typing-fill);
+}
+
+.problem-row.live-typing.active {
+  border-left: 5px solid var(--live-typing-active);
+  background-color: color-mix(in srgb, var(--live-typing-fill) 80%, transparent);
+  box-shadow: inset 0 0 0 1px var(--live-typing-stroke);
+}
+
 .problem-description {
   font-size: 0.9em;
 }
```

</details>

<details>
<summary><code>src/web/www/style/sidebar.css</code> · Legend swatch</summary>

<!-- changetour:hunk file="src/web/www/style/sidebar.css" baseBlob="8df24ed7a42ba9465420cf880f258deb2d3b8dc8" -->

```diff
@@ -368,6 +387,10 @@
   background-color: var(--warning-hole-stroke);
 }
 
+.legend-swatch.live-typing {
+  background-color: var(--live-typing-stroke);
+}
+
 .legend-swatch.projector-error {
   background-color: var(--R1);
 }
```

</details>

<details>
<summary><code>src/web/www/style/variables.css</code> · Live-typing error stroke variable</summary>

<!-- changetour:hunk file="src/web/www/style/variables.css" baseBlob="0462024823da293df0928549bcf60fb26259f999" -->

```diff
@@ -98,6 +98,7 @@
 
   --caret-color: var(--R1);
   --error-hole-stroke: var(--R1);
+  --live-typing-error-hole-stroke: oklch(0.4 0.3 326);
 
   /* CODE TOKENS */
 
```

</details>

<details>
<summary><code>src/web/www/style/variables.css</code> · Sidebar accent variables</summary>

<!-- changetour:hunk file="src/web/www/style/variables.css" baseBlob="0462024823da293df0928549bcf60fb26259f999" -->

```diff
@@ -168,6 +169,11 @@
   --hole-fill: oklch(92% 0.04 250);
   --hole-active: oklch(50% 0.2 250);
 
+  /* LIVE TYPING DECO (sidebar) */
+  --live-typing-stroke: oklch(0.4 0.3 326);
+  --live-typing-fill: oklch(92% 0.04 326);
+  --live-typing-active: oklch(0.35 0.3 326);
+
   /* BACKPACK DECO */
 
   --backpack-selection: var(--shard-selected);
```

</details>

Finally, the toggle itself: it lives in the nut menu's Semantics group with a ⚠️ performance warning — live typing runs statics a second time per recompute. Supporting that icon meant adding a `warning` field to the `setting_item` record and `Widgets.toggle_named`, which forces a `warning: None` on every other settings item (the churn in the NutMenu hunks below). There's also the `Settings.Update.LiveTyping` action and a command-palette entry.

<details>
<summary><code>src/web/app/common/Widgets.re</code> · <code>toggle_named</code> accepts <code>~warning</code></summary>

<!-- changetour:hunk file="src/web/app/common/Widgets.re" baseBlob="add26ae5116b5ab4afb03f2edd02099c4c1bcb6a" -->

```diff
@@ -49,7 +49,7 @@ let toggle = (~tooltip="", label, active, action) =>
     [div(~attrs=[clss(["toggle-knob"])], [text(label)])],
   );
 
-let toggle_named = (~name="", ~tooltip=?, icon, active, action) => {
+let toggle_named = (~name="", ~tooltip=?, ~warning=?, icon, active, action) => {
   let tooltip_attrs =
     switch (tooltip) {
     | Some(t) => [Attr.title(t)]
```

</details>

<details open>
<summary><code>src/web/app/common/Widgets.re</code> · Render the ⚠️ tooltip</summary>

<!-- changetour:hunk file="src/web/app/common/Widgets.re" baseBlob="add26ae5116b5ab4afb03f2edd02099c4c1bcb6a" -->

```diff
@@ -66,7 +66,17 @@ let toggle_named = (~name="", ~tooltip=?, icon, active, action) => {
       toggle(~tooltip=Option.value(~default="", tooltip), icon, active, _ =>
         Effect.Ignore
       ),
-      div([text(name)]),
+      div([
+        text(name),
+        switch (warning) {
+        | Some(msg) =>
+          span(
+            ~attrs=[Attr.title(msg)],
+            [text("\xE2\x9A\xA0\xEF\xB8\x8F")],
+          )
+        | None => none
+        },
+      ]),
     ],
   );
 };
```

</details>

<details>
<summary><code>src/web/Settings.re</code> · Web default: off</summary>

<!-- changetour:hunk file="src/web/Settings.re" baseBlob="9a30875360c8884874bbc4264dcb2a0d7f5ffc18" -->

```diff
@@ -33,6 +33,7 @@ module Model = {
       elaborate: false,
       assist: true,
       dynamics: true,
+      live_typing: false,
       probe_all: false,
       deep_reassociate: true,
       flip_animations: true,
```

</details>

<details>
<summary><code>src/web/Settings.re</code> · <code>LiveTyping</code> update action</summary>

<!-- changetour:hunk file="src/web/Settings.re" baseBlob="9a30875360c8884874bbc4264dcb2a0d7f5ffc18" -->

```diff
@@ -151,6 +152,7 @@ module Update = {
     | SelectionChunkiness
     | Assist
     | Elaborate
+    | LiveTyping
     | Benchmark
     | ContextInspector
     | InstructorMode
```

</details>

<details open>
<summary><code>src/web/Settings.re</code> · Toggle the core flag</summary>

<!-- changetour:hunk file="src/web/Settings.re" baseBlob="9a30875360c8884874bbc4264dcb2a0d7f5ffc18" -->

```diff
@@ -242,6 +244,13 @@ module Update = {
             flip_animations: !settings.core.flip_animations,
           },
         }
+      | LiveTyping => {
+          ...settings,
+          core: {
+            ...settings.core,
+            live_typing: !settings.core.live_typing,
+          },
+        }
       | DisplayWarnings => {
           ...settings,
           core: {
```

</details>

<details>
<summary><code>src/web/view/NutMenu.re</code> · <code>setting_item</code> gains <code>warning</code></summary>

<!-- changetour:hunk file="src/web/view/NutMenu.re" baseBlob="0baf0fe82058b6ff229509ab11853a6bbd181482" -->

```diff
@@ -8,6 +8,7 @@ type setting_item = {
   active: bool,
   setting: Settings.Update.t,
   tooltip: option(string),
+  warning: option(string),
 };
 
 // COMPONENTS
```

</details>

<details>
<summary><code>src/web/view/NutMenu.re</code> · Pass it to the toggle widget</summary>

<!-- changetour:hunk file="src/web/view/NutMenu.re" baseBlob="0baf0fe82058b6ff229509ab11853a6bbd181482" -->

```diff
@@ -31,8 +32,8 @@ let submenu = (~tooltip, ~icon, menu) =>
 // SETTINGS MENU
 
 let settings_group = (~globals: Globals.t, name: string, ts) => {
-  let toggle = ({name, active, setting, tooltip}) =>
-    toggle_named("", ~name, ~tooltip?, active, _ =>
+  let toggle = ({name, active, setting, tooltip, warning}) =>
+    toggle_named("", ~name, ~tooltip?, ~warning?, active, _ =>
       globals.inject_global(Set(setting))
     );
   div_c(
```

</details>

<details open>
<summary><code>src/web/view/NutMenu.re</code> · Semantics group gains the Live Typing toggle, with performance warning</summary>

<!-- changetour:hunk file="src/web/view/NutMenu.re" baseBlob="0baf0fe82058b6ff229509ab11853a6bbd181482" -->

```diff
@@ -54,25 +55,36 @@ let semantics_group = (~globals) => {
         active: globals.settings.core.statics,
         setting: Statics,
         tooltip: Some("Enable static typing"),
+        warning: None,
       },
       {
         name: "Completion",
         active: globals.settings.core.assist,
         setting: Assist,
         tooltip:
           Some("Enable type-directed code completion and assistive features"),
+        warning: None,
       },
       {
         name: "Evaluation",
         active: globals.settings.core.dynamics,
         setting: Dynamics,
         tooltip: Some("Evaluate expressions and show results"),
+        warning: None,
       },
       {
         name: "Docs",
         active: globals.settings.sidebar.show,
         setting: Sidebar(ToggleShow),
         tooltip: Some("Show documentation sidebar"),
+        warning: None,
+      },
+      {
+        name: "Live Typing",
+        active: globals.settings.core.live_typing,
+        setting: LiveTyping,
+        tooltip: Some("Enrich static types with information from evaluation"),
+        warning: Some("May slow down editor performance"),
       },
     ],
   );
```

</details>

<details>
<summary><code>src/web/view/NutMenu.re</code> · <code>warning: None</code> churn</summary>

<!-- changetour:hunk file="src/web/view/NutMenu.re" baseBlob="0baf0fe82058b6ff229509ab11853a6bbd181482" -->

```diff
@@ -89,30 +101,35 @@ let values_group = (~globals: Globals.t) => {
         active: s.show_fn_bodies,
         setting: Evaluation(ShowFnBodies),
         tooltip: Some("Show function bodies in evaluated results"),
+        warning: None,
       },
       {
         name: "Cases",
         active: s.show_case_clauses,
         setting: Evaluation(ShowCaseClauses),
         tooltip: Some("Show case clauses in evaluated results"),
+        warning: None,
       },
       {
         name: "Fixpoints",
         active: s.show_fixpoints,
         setting: Evaluation(ShowFixpoints),
         tooltip: Some("Show fixpoint expressions in evaluated results"),
+        warning: None,
       },
       {
         name: "Tables",
         active: s.project_tables,
         setting: Evaluation(ProjectTables),
         tooltip: Some("Project tables in evaluated results"),
+        warning: None,
       },
       {
         name: "Ascriptions",
         active: s.show_ascriptions,
         setting: Evaluation(ShowAscriptions),
         tooltip: Some("Show type ascriptions in evaluated results"),
+        warning: None,
       },
     ],
   );
```

</details>

<details>
<summary><code>src/web/view/NutMenu.re</code> · <code>warning: None</code> churn</summary>

<!-- changetour:hunk file="src/web/view/NutMenu.re" baseBlob="0baf0fe82058b6ff229509ab11853a6bbd181482" -->

```diff
@@ -129,36 +146,42 @@ let stepper_group = (~globals: Globals.t) => {
         active: s.show_lookup_steps,
         setting: Evaluation(ShowLookups),
         tooltip: Some("Show variable lookup steps in the stepper"),
+        warning: None,
       },
       {
         name: "Show hidden",
         active: s.show_hidden_steps,
         setting: Evaluation(ShowHiddenSteps),
         tooltip: Some("Show hidden intermediate steps in the stepper"),
+        warning: None,
       },
       {
         name: "Show filters",
         active: s.show_stepper_filters,
         setting: Evaluation(ShowFilters),
         tooltip: Some("Show stepper filter controls"),
+        warning: None,
       },
       {
         name: "Show Ascription Steps",
         active: s.show_ascription_steps,
         setting: Evaluation(ShowAscriptionSteps),
         tooltip: Some("Show type ascription steps in the stepper"),
+        warning: None,
       },
       {
         name: "Show Case Steps",
         active: s.show_case_steps,
         setting: Evaluation(ShowCaseSteps),
         tooltip: Some("Show case expression steps in the stepper"),
+        warning: None,
       },
       {
         name: "Proof Steps (experimental)",
         active: s.enable_proof,
         setting: Evaluation(EnableProof),
         tooltip: Some("Enable proof-based stepping mode (experimental)"),
+        warning: None,
       },
     ],
   );
```

</details>

<details>
<summary><code>src/web/view/NutMenu.re</code> · <code>warning: None</code> churn</summary>

<!-- changetour:hunk file="src/web/view/NutMenu.re" baseBlob="0baf0fe82058b6ff229509ab11853a6bbd181482" -->

```diff
@@ -174,24 +197,28 @@ let dev_group = (~globals: Globals.t) => {
         active: globals.settings.benchmark,
         setting: Settings.Update.Benchmark,
         tooltip: Some("Display performance benchmarks"),
+        warning: None,
       },
       {
         name: "Elaboration",
         active: globals.settings.core.elaborate,
         setting: Elaborate,
         tooltip: Some("Show elaborated (internal) expressions"),
+        warning: None,
       },
       {
         name: "Probe All",
         active: globals.settings.core.probe_all,
         setting: ProbeAll,
         tooltip: Some("Enable probes on all top-level definitions"),
+        warning: None,
       },
       {
         name: "Deep Reassociate",
         active: globals.settings.core.deep_reassociate,
         setting: DeepReassociate,
         tooltip: Some("Enable deep reassociation of syntax"),
+        warning: None,
       },
       {
         name: "Character-level mouse",
```

</details>

<details>
<summary><code>src/web/view/NutMenu.re</code> · <code>warning: None</code> churn</summary>

<!-- changetour:hunk file="src/web/view/NutMenu.re" baseBlob="0baf0fe82058b6ff229509ab11853a6bbd181482" -->

```diff
@@ -201,30 +228,35 @@ let dev_group = (~globals: Globals.t) => {
           Some(
             "When on, mouse drag selects by character. When off (default), mouse drag selects by character inside a token and by whole token beyond; holding Alt (Mac) / Ctrl (PC) while dragging does the reverse. Keyboard Shift+Arrow is always character-level (hold Alt/Ctrl for whole-token).",
           ),
+        warning: None,
       },
       {
         name: "Cap Undo Stack",
         active: globals.settings.cap_undo_stack,
         setting: CapUndoStack,
         tooltip: Some("Cap the undo history stack size"),
+        warning: None,
       },
       {
         name: "Ruled Lines",
         active: globals.settings.show_row_lines,
         setting: ShowRowLines,
         tooltip: Some("Show horizontal lines between each row of code"),
+        warning: None,
       },
       {
         name: "Incremental Reuse",
         active: globals.settings.show_incremental_deco,
         setting: ShowIncrementalDeco,
         tooltip: Some("Show incremental evaluator cache hits"),
+        warning: None,
       },
       {
         name: "Debug Sidebar",
         active: globals.settings.show_debug_panel,
         setting: ShowDebugPanel,
         tooltip: Some("Show the debug info sidebar panel"),
+        warning: None,
       },
     ]
     @ (
```

</details>

<details>
<summary><code>src/web/view/NutMenu.re</code> · <code>warning: None</code> churn</summary>

<!-- changetour:hunk file="src/web/view/NutMenu.re" baseBlob="0baf0fe82058b6ff229509ab11853a6bbd181482" -->

```diff
@@ -235,6 +267,7 @@ let dev_group = (~globals: Globals.t) => {
             active: globals.settings.show_log_panel,
             setting: ShowLogPanel,
             tooltip: Some("Show the debug log panel"),
+            warning: None,
           },
         ]
         : []
```

</details>

<details>
<summary><code>src/web/view/NutMenu.re</code> · <code>warning: None</code> churn</summary>

<!-- changetour:hunk file="src/web/view/NutMenu.re" baseBlob="0baf0fe82058b6ff229509ab11853a6bbd181482" -->

```diff
@@ -252,18 +285,21 @@ let code_display_group = (~globals: Globals.t) => {
         active: globals.settings.secondary_icons,
         setting: Settings.Update.SecondaryIcons,
         tooltip: Some("Show whitespace indicator icons"),
+        warning: None,
       },
       {
         name: "Animations",
         active: globals.settings.core.flip_animations,
         setting: FlipAnimations,
         tooltip: Some("Enable flip animations for code changes"),
+        warning: None,
       },
       {
         name: "Line Numbers",
         active: globals.settings.line_numbers,
         setting: ToggleLineNumbers,
         tooltip: None,
+        warning: None,
       },
     ]
     @ (
```

</details>

<details>
<summary><code>src/web/view/NutMenu.re</code> · <code>warning: None</code> churn</summary>

<!-- changetour:hunk file="src/web/view/NutMenu.re" baseBlob="0baf0fe82058b6ff229509ab11853a6bbd181482" -->

```diff
@@ -274,6 +310,7 @@ let code_display_group = (~globals: Globals.t) => {
             active: globals.settings.relative_line_numbers,
             setting: ToggleRelativeLineNumbers,
             tooltip: Some("Show line numbers relative to cursor position"),
+            warning: None,
           },
         ]
         : []
```

</details>

<details>
<summary><code>src/web/app/Page.re</code> · Command palette: Toggle Live Typing</summary>

<!-- changetour:hunk file="src/web/app/Page.re" baseBlob="e2c0ebfa84cc8f9a2f703ad32f79cadfc3f06321" -->

```diff
@@ -539,6 +539,12 @@ module Selection = {
            ~action=inject(Globals(Set(Dynamics))),
            "Toggle Dynamics",
          ),
+         mk(
+           ~section="Settings",
+           ~mdIcon="tune",
+           ~action=inject(Globals(Set(LiveTyping))),
+           "Toggle Live Typing",
+         ),
          mk(
            ~section="Settings",
            ~mdIcon="tune",
```

</details>

## Tests

`Test_Evaluator_LiveTyping.re` exercises the full pipeline: parse → statics → evaluate (collecting probes and instantiations) → re-run statics with the dynamics — then asserts a per-node error annotation (`NoError` / `StaticError` / `DynamicError`) over the whole expression tree, so a test pins down exactly *which* node gets the dynamic error. It includes a QCheck property that the live pass only ever refines the synthesized type (never widens it — the remaining unchecked todo in the PR description), the polymorphic-instantiation cases (`typfun` with matching and mismatching instantiations), and a case confirming inconsistent call sites produce no spurious feedback.

<details open>
<summary><code>test/evaluator/Test_Evaluator_LiveTyping.re</code> · End-to-end live typing suite (~590 lines)</summary>

<!-- changetour:hunk file="test/evaluator/Test_Evaluator_LiveTyping.re" baseBlob="c419b9d8c455d01503add152cafabb863f83b5fc" -->

```diff
@@ -0,0 +1,590 @@
+open Alcotest;
+open Test_Evaluator_Prelude;
+open Language;
+
+[@deriving show({with_path: false})]
+type error =
+  | NoError
+  | StaticError(list(Mark.t))
+  | DynamicError(list(Mark.t));
+
+/* Equality on Mark.t for tests.
+   We can't use structural `==` on marks because some constructors carry
+   `Typ.t` payloads whose `annotation.ids` are freshly generated by statics
+   but are zeros in the test-built expected values. We compare via the
+   derived show, which renders ids opaque, normalizing them away.
+   INCOMPLETE: only the cases exercised by these tests are handled; extend
+   here when adding tests that produce other Mark constructors. */
+let mark_equal = (a: Language.Mark.t, b: Language.Mark.t): bool =>
+  switch (a, b) {
+  | (
+      ExpectationMismatch({ana: ana1, syn: syn1}),
+      ExpectationMismatch({ana: ana2, syn: syn2}),
+    ) =>
+    Typ.show(ana1) == Typ.show(ana2) && Typ.show(syn1) == Typ.show(syn2)
+  | (ExpectationMismatch(_), _)
+  | (_, ExpectationMismatch(_)) => false
+  | _ =>
+    failwith(
+      "Test_Evaluator_LiveTyping.mark_equal: unhandled Mark constructor; "
+      ++ "extend mark_equal to cover this case.",
+    )
+  };
+
+let testable_error =
+  testable(Fmt.using(show_error, Fmt.string), (a: error, b: error) =>
+    switch (a, b) {
+    | (NoError, NoError) => true
+    | (StaticError(e1), StaticError(e2)) => List.equal(mark_equal, e1, e2)
+    | (DynamicError(e1), DynamicError(e2)) => List.equal(mark_equal, e1, e2)
+    | _ => false
+    }
+  );
+module FError =
+  Grammar.Factory({
+    type t = error;
+    let default_value = (): error => {
+      NoError;
+    };
+  });
+
+/**
+ * Helper function to assemble live typing data map from samples and type instantiations.
+ * This logic is shared between multiple test cases.
+ */
+let mk_live_typing =
+    (
+      probe_data: Id.Map.t(list(Sample.t)),
+      type_insts: Dynamics.TypeInstMap.t,
+    )
+    : LiveTyping.Map.t => {
+  LiveTyping.Map.mk(
+    Id.Map.map(
+      samples =>
+        List.map(
+          (s: Sample.t): LiveTyping.sample => {exp: s.value},
+          samples,
+        ),
+      probe_data,
+    ),
+    Id.Map.map(
+      List.map(
+        (inst: Dynamics.TypeInstantiation.t): LiveTyping.type_instantiation =>
+        {
+          tpat_id: inst.tpat_id,
+          type_var: inst.type_var,
+          instantiated_type: inst.instantiated_type,
+        }
+      ),
+      type_insts,
+    ),
+  );
+};
+
+/**
+ * Maps static and live typing error information to error annotations.
+ * Simplifies the nested switch logic with pattern matching.
+ */
+let map_error_annotation = (static_info, live_typing_info) => {
+  let static_error =
+    Option.map(Info.marks_of, static_info)
+    |> Option.bind(_, ms =>
+         switch (ms) {
+         | [] => None
+         | _ => Some(ms)
+         }
+       );
+  let live_typing_error =
+    Option.map(Info.marks_of, live_typing_info)
+    |> Option.bind(_, ms =>
+         switch (ms) {
+         | [] => None
+         | _ => Some(ms)
+         }
+       );
+
+  switch (static_error, live_typing_error) {
+  | (Some(e), _) => StaticError(e)
+  | (None, Some(e)) => DynamicError(e)
+  | (None, None) => NoError
+  };
+};
+
+/**
+ * Reusable test function for live typing validation.
+ * Takes an expected expression with error annotations and verifies
+ * that the live typing system correctly identifies errors.
+ */
+let test_live_typing = (~test_name=?, expected_exp: FError.exp) => {
+  // Create expression with fresh IDs for static analysis
+  let exp_with_ids: Exp.t =
+    Grammar.map_exp_annotation(_ => IdTagged.IdTag.fresh(), expected_exp);
+
+  let test_name =
+    Util.OptUtil.get(
+      () => {
+        Haz3lcore.(
+          Printer.of_segment(
+            ~holes="?",
+            ExpToSegment.exp_to_segment(
+              ~settings=
+                ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
+              exp_with_ids,
+            ),
+          )
+        )
+      },
+      test_name,
+    );
+
+  // Perform initial static analysis (also produces elaborated expression).
+  let (initial_statics, elaborated_exp) =
+    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp_with_ids);
+
+  let original_errors =
+    StaticsBase.Map.errors(initial_statics) |> List.map(snd);
+  Alcotest.check(
+    Alcotest.int,
+    "Expect no static errors initially",
+    0,
+    List.length(original_errors),
+  );
+
+  // Compute targets for expressions with unknown types (needed for live typing)
+  let targets =
+    Haz3lcore.CachedStatics.compute_targets(
+      ~settings=CoreSettings.on,
+      ~info_map=initial_statics,
+      ~probe_ids=Id.Map.empty,
+    );
+
+  // Evaluate the elaborated expression to collect dynamic information
+  let (_, evaluation_state) =
+    Evaluator.evaluate(~targets, ~env=Builtins.env_init, elaborated_exp);
+
+  // Extract probe data and type instantiations from the evaluation state
+  let probe_data = EvaluatorState.get_probes(evaluation_state);
+  let type_insts = EvaluatorState.get_type_insts(evaluation_state);
+
+  // Convert probe closures and type instantiations to dynamic expressions for static re-analysis
+  let dynamic_expressions = mk_live_typing(probe_data, type_insts);
+
+  // Re-run static analysis with dynamic information
+  let (live_typing_statics, _) =
+    Statics.mk(
+      ~dynamics=dynamic_expressions,
+      CoreSettings.on,
+      Builtins.ctx_init(Some(Int)),
+      exp_with_ids,
+    );
+
+  // Map the expression to annotate errors based on static and live typing
+  let actual_exp: FError.exp =
+    Grammar.map_exp_annotation(
+      id_tag => {
+        let static_info =
+          StaticsBase.Map.lookup(
+            IdTagged.IdTag.rep_id(id_tag),
+            initial_statics,
+          );
+        let live_typing_info =
+          StaticsBase.Map.lookup(
+            IdTagged.IdTag.rep_id(id_tag),
+            live_typing_statics,
+          );
+
+        map_error_annotation(static_info, live_typing_info);
+      },
+      exp_with_ids,
+    );
+
+  let testable_annotated: testable(Grammar.exp_t(error)) =
+    testable(
+      Fmt.using([%derive.show: Grammar.exp_t(error)], Fmt.string),
+      Grammar.equal_exp_t((a: error, b: error) =>
+        switch (a, b) {
+        | (NoError, NoError) => true
+        | (StaticError(e1), StaticError(e2)) =>
+          List.equal(mark_equal, e1, e2)
+        | (DynamicError(e1), DynamicError(e2)) =>
+          List.equal(mark_equal, e1, e2)
+        | _ => false
+        }
+      ),
+    );
+  // Verify that the actual error annotations match expectations
+  check(testable_annotated, test_name, expected_exp, actual_exp);
+};
+/* Adapter: previous tests wrote `inconsistent_exp(Expectation({ana, syn}))`.
+   In the new architecture, type-mismatch errors are represented as the single
+   Mark `ExpectationMismatch({ana, syn})`. */
+type inconsistent_kind =
+  | Expectation({
+      ana: Typ.t,
+      syn: Typ.t,
+    });
+let inconsistent_exp = (kind: inconsistent_kind): list(Mark.t) =>
+  switch (kind) {
+  | Expectation({ana, syn}) => [
+      Mark.ExpectationMismatch({
+        ana,
+        syn,
+      }),
+    ]
+  };
+
+/* Property: for every expression-info id, the elab_syn_ty produced by static
+   analysis run *with* live-typing dynamics is more precise than (or equal to)
+   the elab_syn_ty produced by the static-only analysis. Refinement only ever
+   moves down the precision lattice. */
+let precision_property = (exp: Exp.t): bool =>
+  try({
+    let exp_with_ids =
+      Grammar.map_exp_annotation(_ => IdTagged.IdTag.fresh(), exp);
+    let ctx = Builtins.ctx_init(Some(Int));
+    let (static_map, elaborated) =
+      Statics.mk(CoreSettings.on, ctx, exp_with_ids);
+    if (StaticsBase.Map.has_errors(static_map)) {
+      true;
+    } else {
+      let targets =
+        Haz3lcore.CachedStatics.compute_targets(
+          ~settings=CoreSettings.on,
+          ~info_map=static_map,
+          ~probe_ids=Id.Map.empty,
+        );
+      let (_, state) =
+        Evaluator.evaluate(~targets, ~env=Builtins.env_init, elaborated);
+      let dynamics =
+        mk_live_typing(
+          EvaluatorState.get_probes(state),
+          EvaluatorState.get_type_insts(state),
+        );
+      let (live_map, _) =
+        Statics.mk(~dynamics, CoreSettings.on, ctx, exp_with_ids);
+      Id.Map.for_all(
+        (id, info_static) =>
+          switch (info_static, StaticsBase.Map.lookup(id, live_map)) {
+          | (Info.InfoExp(s), Some(Info.InfoExp(l))) =>
+            Typ.is_more_precise(ctx, l.elab_syn_ty, s.elab_syn_ty)
+          | _ => true
+          },
+        static_map,
+      );
+    };
+  }) {
+  | _ => true
+  };
+
+let precision_property_test =
+  QCheck_alcotest.to_alcotest(
+    QCheck.Test.make(
+      ~name="Live type refines synthesized static type",
+      ~count=500,
+      QCheck_Util.arb_exp(~minimal_idents=true, 10),
+      precision_property,
+    ),
+  );
+
+let tests = (
+  "Evaluator.LiveTyping",
+  [
+    precision_property_test,
+    test_case(
+      "dynamic in-editor feedback",
+      `Slow,
+      () => {
+        let program = {hazel|
+let unique = (fun xs ->
+  fold_left(xs, fun (seen, x) ->if mem(seen,x) then seen else seen @[x], []))in
+let pivot_table = (fun (table, new_col, index, value) ->
+  let indices = map(table, index) |> unique in
+  let new_cols = map(table, new_col) |> unique in
+
+  map(indices, fun idx ->
+    (index=idx) ...
+    (map(new_cols, fun col ->
+      (label=col,
+        value=filter(table, fun r -> index(r) == idx && new_col(r) == col)
+        |>value)
+    ) |> from_lvs)
+  ))in
+
+
+
+let results =
+  pivot_table(
+    [(a=1,b=2,c=3), (a=1,b=2,c=3), (a=1,b=2,c=3), (a=1,b=2,c=3) ,(a=1,b=2,c=3)]
+    @ [(a=1,b=2,c=3), (a=1,b=2,c=3), (a=1,b=2,c=3), (a=1,b=2,c=3) ,(a=1,b=2,c=3)],
+    fun r -> r.a |> string_of_int,
+    fun r -> r.b,
+    fun r -> fold_left(r.c, int_plus, 0))
+in
+
+(results).`2`
+|hazel};
+        let exp = parse_exp(program);
+        let (_, elaborated) =
+          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp);
+        let (result, state: EvaluatorState.t) =
+          Evaluator.evaluate(~env=Builtins.env_init, elaborated);
+
+        let dynamics = EvaluatorState.get_probes(state);
+        let type_insts = EvaluatorState.get_type_insts(state);
+
+        // Convert probe closures and type instantiations to dynamic expressions for static re-analysis
+        let dynamic_expressions = mk_live_typing(dynamics, type_insts);
+        let _static_feedback =
+          Statics.mk(
+            ~dynamics=dynamic_expressions,
+            CoreSettings.on,
+            Builtins.ctx_init(Some(Int)),
+            exp,
+          );
+
+        let expected_exp =
+          parse_exp({hz|
+          [(index=2, `1`=30).`2`]
+          |hz});
+        check(testable_exp(), "Result of execution", expected_exp, result);
+      },
+    ),
+    test_case(
+      "Type flows through unknown ascription dynamically",
+      `Quick,
+      () => {
+        // Create expected expression with dynamic error annotation
+        // This tests that int(1) : ? : String correctly identifies
+        // the type inconsistency when ? is resolved to int but expected as string
+        let expected_exp: FError.exp =
+          FError.(
+            Exp.(
+              asc(
+                asc(
+                  ~ann=
+                    DynamicError(
+                      inconsistent_exp(
+                        Test_Statics_Prelude.FTemp.Typ.(
+                          Expectation({
+                            ana: string(),
+                            syn: int(),
+                          })
+                        ),
+                      ),
+                    ),
+                  int(1),
+                  Typ.unknown(Internal),
+                ),
+                Typ.string(),
+              )
+            )
+          );
+
+        test_live_typing(expected_exp);
+      },
+    ),
+    /* DISABLED: live-typing duplicates the inner branch's ExpectationMismatch
+          onto the surrounding `If` expression. Tracked in
+          https://github.com/hazelgrove/hazel/issues/2265. Re-enable once the
+          extra mark on the If is removed.
+       test_case(
+         "Conditional uses runtime type information",
+         `Quick,
+         () => {
+           open FError;
+           open Exp;
+
+           test_live_typing(
+             bin_op(
+               String(Concat),
+               if_(
+                 bool(true),
+                 asc(
+                   ~ann=
+                     DynamicError(
+                       inconsistent_exp(
+                         Test_Statics_Prelude.FTemp.Typ.(
+                           Expectation({
+                             ana: string(),
+                             syn: int(),
+                           })
+                         ),
+                       ),
+                     ),
+                   int(1),
+                   Typ.unknown(Internal),
+                 ),
+                 asc(string("World"), Typ.unknown(Internal)),
+               ),
+               string("World"),
+             ),
+           );
+           test_live_typing(
+             bin_op(
+               String(Concat),
+               if_(
+                 bool(false),
+                 asc(int(1), Typ.unknown(Internal)),
+                 asc(string("Hello"), Typ.unknown(Internal)),
+               ),
+               string("World"),
+             ),
+           );
+         },
+       ),
+       */
+    test_case(
+      "Unannotated lambda applied to string causes dynamic error",
+      `Quick,
+      () => {
+        open FError;
+        open Exp;
+        let exp: FError.exp =
+          ap(
+            Forward,
+            fn(
+              Pat.var("y"),
+              bin_op(
+                Int(Plus),
+                var(
+                  ~ann=
+                    DynamicError(
+                      inconsistent_exp(
+                        Test_Statics_Prelude.FTemp.Typ.(
+                          Expectation({
+                            ana: int(),
+                            syn: string(),
+                          })
+                        ),
+                      ),
+                    ),
+                  "y",
+                ),
+                int(1),
+              ),
+            ),
+            string(""),
+          );
+        test_live_typing(exp);
+      },
+    ),
+    test_case(
+      "Unannotated lambda called with inconsistent types gives no feedback",
+      `Quick,
+      () => {
+        let program = {|let f = fun x -> x @ [""] in f(1);f(2.0)|};
+        let exp = parse_exp(program);
+        let no_errors = Grammar.map_exp_annotation(_ => NoError, exp);
+
+        test_live_typing(no_errors);
+      },
+    ),
+    test_case(
+      "typfun uses dynamic type env with correct type",
+      `Quick,
+      () => {
+        let program = {|(typfun a -> fun x : a -> (x : a))@<String>("")|};
+        let exp = parse_exp(program);
+        let no_errors = Grammar.map_exp_annotation(_ => NoError, exp);
+
+        test_live_typing(
+          ~test_name={|(typfun a -> fun x : a -> (x : a))@<String>("")|},
+          no_errors,
+        );
+      },
+    ),
+    test_case(
+      "typfun uses dynamic type env with incorrect type",
+      `Quick,
+      () => {
+        open FError;
+        open Exp;
+
+        let exp: FError.exp =
+          ap(
+            Forward,
+            typ_ap(
+              typ_fun(
+                TPat.var("a"),
+                fn(
+                  Pat.var("x"),
+                  asc(
+                    asc(
+                      ~ann=
+                        DynamicError(
+                          inconsistent_exp(
+                            Test_Statics_Prelude.FTemp.Typ.(
+                              Expectation({
+                                ana: var("a"),
+                                syn: string(),
+                              })
+                            ),
+                          ),
+                        ),
+                      var("x"),
+                      Typ.unknown(Hole(EmptyHole)),
+                    ),
+                    Typ.var("a"),
+                  ),
+                ),
+                None,
+              ),
+              Typ.int(),
+            ),
+            string(""),
+          );
+        test_live_typing(
+          ~test_name={|(typfun a -> fun x -> (x : ? : a))@<Int>("")|},
+          exp,
+        );
+      },
+    ),
+    test_case(
+      "typfun with matching type instantiation: no error",
+      `Quick,
+      () => {
+        open FError;
+        open Exp;
+        /* Same shape as the previous test, but instantiated to <String>
+           with a String argument: a := String, x := "", so the inner
+           Asc body has type String which matches `a`. Live-typing must
+           consult the type-instantiation map to resolve `a` to `String`;
+           if it leaves `a` Abstract, meet(Var("a"), String) fails and a
+           spurious DynamicError is reported. */
+        let exp: FError.exp =
+          ap(
+            Forward,
+            typ_ap(
+              typ_fun(
+                TPat.var("a"),
+                fn(
+                  Pat.var("x"),
+                  asc(
+                    asc(var("x"), Typ.unknown(Hole(EmptyHole))),
+                    Typ.var("a"),
+                  ),
+                ),
+                None,
+              ),
+              Typ.string(),
+            ),
+            string(""),
+          );
+        test_live_typing(
+          ~test_name={|(typfun a -> fun x -> (x : ? : a))@<String>("")|},
+          exp,
+        );
+      },
+    ),
+    test_case(
+      "Polymorphism with dynamic type environment in unevaluated code",
+      `Quick,
+      () => {
+        let program = {|(typfun a -> fun (g) -> (fun () -> g : a))@<String>("")|};
+        let exp = parse_exp(program);
+        let no_errors = Grammar.map_exp_annotation(_ => NoError, exp);
+        test_live_typing(~test_name=program, no_errors);
+      },
+    ),
+  ],
+);
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator.re</code> · Register the suite</summary>

<!-- changetour:hunk file="test/evaluator/Test_Evaluator.re" baseBlob="c26499d09af31682fd5bd0e9e321218218d5cf95" -->

```diff
@@ -26,6 +26,7 @@ let tests =
     Test_StepperBase.tests,
     Test_Evaluator_Properties.tests,
     Test_Evaluator_Performance.tests,
+    Test_Evaluator_LiveTyping.tests,
     Test_Evaluator_Modules.tests,
     Test_Evaluator_Incremental.tests,
   ];
```

</details>

Supporting suites: unit tests for `Typ.join` (a join-precision QCheck property is written but deliberately not registered — it fails for forall/rec types, per the TODO); a regression test that type instantiations survive incremental-evaluator cache reuse (the `StateSlice` work above); probe tests pinning down sample collection at nested ascriptions, which mirror the live-typing test shapes; and unit tests for the `WriterMonad` that ascription distribution threads.

<details open>
<summary><code>test/Test_Typ.re</code> · <code>Typ.join</code> unit tests + unregistered precision property</summary>

<!-- changetour:hunk file="test/Test_Typ.re" baseBlob="785d1c404970d858641e8aed46cb7b0f28f477d7" -->

```diff
@@ -166,6 +166,86 @@ let meet_tests = (
   ],
 );
 
+// TODO We want this property but it's not currently passing for forall and rec types so it's not included below
+let join_precision_property =
+  QCheck_alcotest.to_alcotest(
+    QCheck.Test.make(
+      ~name="Typ.join is less precise than inputs",
+      ~count=100000,
+      QCheck.(
+        QCheck_Util.(
+          pair(
+            arb_typ(~minimal_idents=true, 10),
+            arb_typ(~minimal_idents=true, 10),
+          )
+        )
+      ),
+      ((t1, t2)) => {
+        let ctx = Builtins.ctx_init(Some(Int));
+        let m = Typ.join(ctx, t1, t2);
+        Typ.is_more_precise(ctx, Typ.normalize(ctx, t1), m)
+        && Typ.is_more_precise(ctx, Typ.normalize(ctx, t2), m);
+      },
+    ),
+  );
+
+let join_tests = (
+  "Typ.join",
+  IdTagged.FreshGrammar.Typ.[
+    test_case(
+      "equal atomic types",
+      `Quick,
+      () => {
+        let t = Typ.join(Builtins.ctx_init(None), int(), int());
+        check(typ, "join of equal atomic types", int(), t);
+      },
+    ),
+    test_case(
+      "Unknown and atomic type",
+      `Quick,
+      () => {
+        let t = Typ.join(Builtins.ctx_init(None), unknown(Internal), int());
+        check(typ, "join of Unknown and atomic type", unknown(Internal), t);
+      },
+    ),
+    test_case(
+      "Sum type with same variants",
+      `Quick,
+      () => {
+        let t =
+          Typ.join(
+            Builtins.ctx_init(None),
+            sum([
+              Variant("A", ConstructorMap.empty_variant_ann, Some(int())),
+              Variant("B", ConstructorMap.empty_variant_ann, Some(bool())),
+            ]),
+            sum([
+              Variant("A", ConstructorMap.empty_variant_ann, Some(int())),
+              Variant("B", ConstructorMap.empty_variant_ann, Some(bool())),
+            ]),
+          );
+        check(
+          typ,
+          "Join of sum types with same variants",
+          sum([
+            Variant("A", ConstructorMap.empty_variant_ann, Some(int())),
+            Variant("B", ConstructorMap.empty_variant_ann, Some(bool())),
+          ]),
+          t,
+        );
+      },
+    ),
+    test_case(
+      "Unbound variables",
+      `Quick,
+      () => {
+        let t = Typ.join(Builtins.ctx_init(None), var("a"), var("b"));
+        check(typ, "Join of unbound variables", unknown(Internal), t);
+      },
+    ),
+  ],
+);
+
 let fast_equal_tests = (
   "Typ.fast_equal",
   [
```

</details>

<details>
<summary><code>test/Test_Typ.re</code> · Register join tests</summary>

<!-- changetour:hunk file="test/Test_Typ.re" baseBlob="785d1c404970d858641e8aed46cb7b0f28f477d7" -->

```diff
@@ -493,4 +573,4 @@ let diff_tests = (
   ],
 );
 
-let tests = [meet_tests, fast_equal_tests, diff_tests];
+let tests = [join_tests, fast_equal_tests, meet_tests, diff_tests];
```

</details>

<details open>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · Regression: instantiations survive incremental reuse</summary>

<!-- changetour:hunk file="test/evaluator/Test_Evaluator_Incremental.re" baseBlob="c6e0cecb6e4c2dac208d2962b630b753dd79cf32" -->

```diff
@@ -1278,6 +1278,38 @@ let test_reuse_provenance_distinguishes_pattern_shapes = () => {
   );
 };
 
+/* Regression: incremental cache reuse of a TypAp subtree must replay
+ * the RecordTypeInstantiation effect so the resulting state.type_insts
+ * has the instantiation entries. If the StateSlice doesn't capture/replay
+ * type_insts, cache reuse silently drops them. */
+let test_typ_inst_survives_incremental_reuse = () => {
+  /* Two type applications of the same typfun. Running twice with the same
+   * Exp.t simulates a no-op edit cycle: the second run should reuse the
+   * cached entries (typAps included), and state.type_insts on the second
+   * run must still contain both instantiations. */
+  let src = {|let f = typfun a -> fun i -> (i : ? : a) in let _ = f@<String>("") in f@<Int>("")|};
+  let exp = parse_exp(src);
+  let (_, state1, incr1) = eval_incr(exp);
+  let n1 =
+    Id.Map.fold((_, l, acc) => acc + List.length(l), state1.type_insts, 0);
+  check(
+    bool,
+    "First run records at least 2 type instantiations",
+    true,
+    n1 >= 2,
+  );
+  let (_, state2, incr2) = eval_incr(~prev=incr1, exp);
+  check(bool, "Second run actually reused entries", true, incr2.reused != []);
+  let n2 =
+    Id.Map.fold((_, l, acc) => acc + List.length(l), state2.type_insts, 0);
+  check(
+    int,
+    "Second run preserves type_inst count under incremental reuse",
+    n1,
+    n2,
+  );
+};
+
 /* Regression test: builtins must participate in reuse.
  *
  * A subexpression that references a builtin (e.g. `string_length`) has that
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · Register the case</summary>

<!-- changetour:hunk file="test/evaluator/Test_Evaluator_Incremental.re" baseBlob="c6e0cecb6e4c2dac208d2962b630b753dd79cf32" -->

```diff
@@ -1347,6 +1379,11 @@ n|};
 let tests = (
   "Evaluator.Incremental",
   [
+    test_case(
+      "Typ instantiations survive incremental reuse",
+      `Quick,
+      test_typ_inst_survives_incremental_reuse,
+    ),
     test_case(
       "DIAG module in unchanged rhs tuple lands in frozen",
       `Quick,
```

</details>

<details open>
<summary><code>test/evaluator/Test_Evaluator_Probes.re</code> · Nested-ascription probe survival cases</summary>

<!-- changetour:hunk file="test/evaluator/Test_Evaluator_Probes.re" baseBlob="6059292600e1eb532ed43b1bfe669ce72c5738d2" -->

```diff
@@ -859,6 +859,42 @@ in f(0)|},
 in f([1, 2])|},
     [(0, ["[1, 2, 0]"])],
   ),
+  /* Nested ascription probe survival.
+     `"" : String : String` parses as `Asc(Asc("", String), String)`.
+     Probing the outer Asc should always work (sanity). Probing the inner
+     Asc is the case under suspicion: if elaboration collapses or rebuilds
+     the inner Asc with a fresh id, its probe target is lost and we see
+     zero samples. */
+  probe_line_test(
+    "Probe on outer of nested same-type ascription",
+    {|^^probe("" : String : String)|},
+    [(0, ["\"\""])],
+  ),
+  probe_line_test(
+    "Probe on inner of nested same-type ascription",
+    {|^^probe("" : String) : String|},
+    [(0, ["\"\""])],
+  ),
+  /* Mirrors the shape of Test_Evaluator_LiveTyping case 002
+     (`(int(1) : ?) : String`) and case 007's inner Asc
+     (`(x : ?) : a`). If the inner probe survives here but live-typing
+     case 007 still sees no refinement, the issue is in sample
+     resolution, not in elaboration eating the inner Asc. */
+  probe_line_test(
+    "Probe on inner of nested unknown-then-known ascription",
+    {|^^probe("" : ?) : String|},
+    [(0, ["\"\""])],
+  ),
+  /* The exact inner shape of LiveTyping case 007: a Var (not a literal)
+     inside the inner Asc. If this test passes (1 sample of `""`) the
+     probe is reaching the inner Asc and recording the runtime value of
+     x; if it fails or yields the symbol `x`, it confirms a variable-
+     specific issue at the inner-Asc probe site. */
+  probe_line_test(
+    "Probe on inner of nested ascription whose body is a Var",
+    {|let x = "" in ^^probe(x : ?) : String|},
+    [(0, ["\"\""])],
+  ),
 ];
 
 /* Tests that probe samples are not duplicated when values flow through
```

</details>

<details>
<summary><code>test/Test_WriterMonad.re</code> · WriterMonad unit tests (new file)</summary>

<!-- changetour:hunk file="test/Test_WriterMonad.re" baseBlob="c37aab961bb9410e2c82a61a1f545bed4e759b43" -->

```diff
@@ -0,0 +1,124 @@
+open Alcotest;
+open Util;
+
+module StringWriter = {
+  [@deriving sexp]
+  type t = string;
+  let empty = "";
+  let append = (s1, s2) => s1 ++ s2;
+};
+
+module StringWriterMonad = Util.WriterMonad.Make(StringWriter);
+
+let tests = (
+  "WriterMonad",
+  [
+    test_case(
+      "return produces empty writer",
+      `Quick,
+      () => {
+        let result = StringWriterMonad.return(42);
+        check(
+          pair(string, int),
+          "return with empty writer",
+          ("", 42),
+          result,
+        );
+      },
+    ),
+    test_case(
+      "tell adds to writer",
+      `Quick,
+      () => {
+        let result = StringWriterMonad.tell("hello");
+        check(
+          pair(string, unit),
+          "tell adds message",
+          ("hello", ()),
+          result,
+        );
+      },
+    ),
+    test_case(
+      "bind combines writers",
+      `Quick,
+      () => {
+        let computation =
+          StringWriterMonad.Syntax.(
+            let* () = StringWriterMonad.tell("start ");
+            let* () = StringWriterMonad.tell("middle ");
+            let* () = StringWriterMonad.tell("end");
+            StringWriterMonad.return("done")
+          );
+        check(
+          pair(string, string),
+          "bind combines writers",
+          ("start middle end", "done"),
+          computation,
+        );
+      },
+    ),
+    test_case(
+      "listen captures writer",
+      `Quick,
+      () => {
+        let computation =
+          StringWriterMonad.Syntax.(
+            let* () = StringWriterMonad.tell("log1 ");
+            let* () = StringWriterMonad.tell("log2");
+            StringWriterMonad.return(123)
+          );
+        let result = StringWriterMonad.listen(computation);
+        check(
+          pair(string, pair(int, string)),
+          "listen captures writer",
+          ("log1 log2", (123, "log1 log2")),
+          result,
+        );
+      },
+    ),
+    test_case(
+      "pass modifies writer",
+      `Quick,
+      () => {
+        let computation =
+          StringWriterMonad.Syntax.(
+            let* () = StringWriterMonad.tell("original");
+            StringWriterMonad.return(("result", w => "[" ++ w ++ "]"))
+          );
+        let result = StringWriterMonad.pass(computation);
+        check(
+          pair(string, string),
+          "pass modifies writer",
+          ("[original]", "result"),
+          result,
+        );
+      },
+    ),
+    test_case(
+      "complex computation with let syntax",
+      `Quick,
+      () => {
+        let computation =
+          StringWriterMonad.Syntax.(
+            let* () = StringWriterMonad.tell("Begin ");
+            let* x = StringWriterMonad.return(10);
+            let* () =
+              StringWriterMonad.tell(
+                "Processing " ++ string_of_int(x) ++ " ",
+              );
+            let* y = StringWriterMonad.return(x * 2);
+            let* () =
+              StringWriterMonad.tell("Result: " ++ string_of_int(y) ++ " ");
+            StringWriterMonad.return(y + 5)
+          );
+        check(
+          pair(string, int),
+          "complex computation",
+          ("Begin Processing 10 Result: 20 ", 25),
+          computation,
+        );
+      },
+    ),
+  ],
+);
```

</details>

<details>
<summary><code>test/haz3ltest.re</code> · Register WriterMonad tests</summary>

<!-- changetour:hunk file="test/haz3ltest.re" baseBlob="9d2b0a99bf5fafdad035b72b460c29c1cffd5219" -->

```diff
@@ -27,6 +27,7 @@ let (suite, _) =
       Test_Menhir.tests,
       Test_StringUtil.tests,
       Test_PatternMatch.tests,
+      Test_WriterMonad.tests,
       Test_Equality.tests,
       Test_Substitution.tests,
     ]
```

</details>

<details>
<summary><code>test/statics/Test_Statics_Prelude.re</code> · <code>error_exp</code> alias</summary>

<!-- changetour:hunk file="test/statics/Test_Statics_Prelude.re" baseBlob="9e44f98b23181b7f7bbaf91c5d04a1d4d05efba7" -->

```diff
@@ -190,6 +190,8 @@ let annotated_exp: testable(Grammar.exp_t(option(issue))) =
     Grammar.equal_exp_t(Option.equal(equal_issue)),
   );
 
+let error_exp: testable(Grammar.exp_t(option(issue))) = annotated_exp;
+
 let fresh = (exp: Grammar.exp_t(unit)): TermBase.exp_t => {
   Grammar.map_exp_annotation(
     (_annotation): IdTagged.IdTag.t => IdTagged.IdTag.mk_internal([Id.mk()]),
```

</details>

<details>
<summary><code>test/statics/Test_Statics_Prelude.re</code> · Use the alias</summary>

<!-- changetour:hunk file="test/statics/Test_Statics_Prelude.re" baseBlob="9e44f98b23181b7f7bbaf91c5d04a1d4d05efba7" -->

```diff
@@ -213,7 +215,7 @@ let annotated_tree_test = (name, expected_type, expected_error_tree) => {
   let annotated: Grammar.exp_t(option(issue)) =
     annotate_static_errors(term, s);
   let typ = type_of(~static_map=s, term);
-  Alcotest.check(annotated_exp, name, expected_error_tree, annotated);
+  Alcotest.check(error_exp, name, expected_error_tree, annotated);
   Alcotest.check(
     testable_typ,
     "Expected Type",
```

</details>

## Miscellaneous <!-- collapsed -->

Everything here is mechanical. The largest wave: `Grammar.fn` changes from two positional `option` arguments (`fn(p, e, typ, name)`) to optional labeled ones (`fn(~typ=?, ~name=?, p, e)`), eliminating the `None, None` noise at every function-construction site — builtins, table transforms, the menhir conversion, and many tests.

<details>
<summary><code>src/language/term/Grammar.re</code> · <code>fn</code> takes <code>~typ</code>/<code>~name</code> as labeled optionals</summary>

<!-- changetour:hunk file="src/language/term/Grammar.re" baseBlob="cb88698e746cbb4b666277b5c724b38e6d4b412b" -->

```diff
@@ -620,8 +620,8 @@ module Factory = (DefaultAnnotation: DefaultAnnotation) => {
       term: Constructor(s, t),
       annotation: default_annotation(ann),
     };
-    let fn = (~ann=?, p, e, t, v): exp_t(DefaultAnnotation.t) => {
-      term: Fun(p, e, t, v),
+    let fn = (~ann=?, ~typ=?, ~name=?, p, e): exp_t(DefaultAnnotation.t) => {
+      term: Fun(p, e, typ, name),
       annotation: default_annotation(ann),
     };
     let typ_fun = (~ann=?, p, e, v): exp_t(DefaultAnnotation.t) => {
```

</details>

<details>
<summary><code>src/menhirParser/Conversion.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="src/menhirParser/Conversion.re" baseBlob="0b01e13f1a5f0da685e654787dd172802f959b3b" -->

```diff
@@ -258,8 +258,8 @@ module rec Exp: {
     | Fun(p, e, name_opt) =>
       switch (name_opt) {
       | Some(name_str) =>
-        fn(Pat.of_menhir_ast(p), of_menhir_ast(e), None, Some(name_str))
-      | None => fn(Pat.of_menhir_ast(p), of_menhir_ast(e), None, None)
+        fn(Pat.of_menhir_ast(p), of_menhir_ast(e), ~name=name_str)
+      | None => fn(Pat.of_menhir_ast(p), of_menhir_ast(e))
       }
     | ApExp(e1, args) =>
       switch (args) {
```

</details>

<details>
<summary><code>src/haz3lcore/zipper/action/Introduce.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="src/haz3lcore/zipper/action/Introduce.re" baseBlob="cd2cca09e524892a2aafbb87133679504c49b003" -->

```diff
@@ -114,7 +114,7 @@ module IntroduceExp: Introducable with type t = Exp.t = {
         | Arrow(_, _) =>
           let cursor_pat = Pat.empty_hole();
           Some((
-            fn(cursor_pat, empty_hole(), None, None),
+            fn(cursor_pat, empty_hole()),
             List.hd(cursor_pat.annotation.ids),
             false,
           ));
```

</details>

<details>
<summary><code>src/language/dynamics/transition/Ascriptions.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="src/language/dynamics/transition/Ascriptions.re" baseBlob="163fe5e598e43421b351e37d78fd3277e785ae4a" -->

```diff
@@ -75,7 +75,14 @@ let rec transition =
           IdTagged.fast_copy(
             DHExp.rep_id(e),
             IdTagged.FreshGrammar.(
-              Exp.(fn(Pat.(asc(p, t1)), asc(body, t2), closure_ty, name))
+              Exp.(
+                fn(
+                  Pat.(asc(p, t1)),
+                  asc(body, t2),
+                  ~typ=?closure_ty,
+                  ~name?,
+                )
+              )
             ),
           ),
         ),
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsADT.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsADT.re" baseBlob="9933ed43e4524191aa68d1c8fb73e29fcf24603c" -->

```diff
@@ -92,8 +92,7 @@ module Option = {
                     ),
                   ],
                 ),
-                None,
-                Some("option_map+"),
+                ~name="option_map+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsADT.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsADT.re" baseBlob="9933ed43e4524191aa68d1c8fb73e29fcf24603c" -->

```diff
@@ -126,8 +125,7 @@ module Option = {
                     ),
                   ],
                 ),
-                None,
-                Some("option_bind+"),
+                ~name="option_bind+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsADT.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsADT.re" baseBlob="9933ed43e4524191aa68d1c8fb73e29fcf24603c" -->

```diff
@@ -160,8 +158,7 @@ module Option = {
                     ),
                   ],
                 ),
-                None,
-                Some("option_to_list+"),
+                ~name="option_to_list+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -37,8 +37,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("length+"),
+                ~name="length+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -82,8 +81,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("map+"),
+                ~name="map+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -132,8 +130,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("filter+"),
+                ~name="filter+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -186,8 +183,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("fold_left+"),
+                ~name="fold_left+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -222,14 +218,11 @@ let builtins =
                         var("acc"),
                         ap(Forward, var("f"), var("x")),
                       ),
-                      None,
-                      None,
                     ),
                     list_lit([]),
                   ]),
                 ),
-                None,
-                Some("flat_map+"),
+                ~name="flat_map+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -280,8 +273,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("zip+"),
+                ~name="zip+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -327,8 +319,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("unzip+"),
+                ~name="unzip+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -365,8 +356,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("reverse+"),
+                ~name="reverse+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -415,8 +405,7 @@ let builtins =
                     ],
                   ),
                 ),
-                None,
-                Some("take+"),
+                ~name="take+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -461,8 +450,7 @@ let builtins =
                     ],
                   ),
                 ),
-                None,
-                Some("drop+"),
+                ~name="drop+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -497,8 +485,7 @@ let builtins =
                     ),
                   ),
                 ),
-                None,
-                Some("range+"),
+                ~name="range+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -549,15 +536,12 @@ let builtins =
                           ),
                         ],
                       ),
-                      None,
-                      None,
                     ),
                     None,
                   ),
                   tuple([var("xs"), int(0)]),
                 ),
-                None,
-                Some("enumerate+"),
+                ~name="enumerate+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -599,8 +583,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("any+"),
+                ~name="any+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -642,8 +625,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("all+"),
+                ~name="all+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -688,8 +670,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("intersperse+"),
+                ~name="intersperse+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -710,8 +691,7 @@ let builtins =
               fn(
                 Pat.tuple([Pat.var("x"), Pat.var("xs")]),
                 cons(var("x"), var("xs")),
-                None,
-                Some("cons+"),
+                ~name="cons+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -741,8 +721,7 @@ let builtins =
                     (Pat.cons(Pat.var("x"), Pat.wild()), var("x")),
                   ],
                 ),
-                None,
-                Some("head+"),
+                ~name="head+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -772,8 +751,7 @@ let builtins =
                     (Pat.cons(Pat.wild(), Pat.var("xs")), var("xs")),
                   ],
                 ),
-                None,
-                Some("tail+"),
+                ~name="tail+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -803,8 +781,7 @@ let builtins =
                     (Pat.wild(), bool(false)),
                   ],
                 ),
-                None,
-                Some("is_empty+"),
+                ~name="is_empty+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -848,8 +825,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("nth+"),
+                ~name="nth+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -901,8 +877,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("fold_right+"),
+                ~name="fold_right+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -923,8 +898,7 @@ let builtins =
               fn(
                 Pat.tuple([Pat.var("xs"), Pat.var("ys")]),
                 list_concat(var("xs"), var("ys")),
-                None,
-                Some("append+"),
+                ~name="append+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -960,8 +934,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("concat+"),
+                ~name="concat+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1024,15 +997,12 @@ let builtins =
                           ),
                         ],
                       ),
-                      None,
-                      None,
                     ),
                     None,
                   ),
                   tuple([var("xs"), var("f"), int(0)]),
                 ),
-                None,
-                Some("mapi+"),
+                ~name="mapi+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1107,15 +1077,13 @@ let builtins =
                           ),
                         ],
                       ),
-                      None,
-                      Some("filteri_helper+"),
+                      ~name="filteri_helper+",
                     ),
                     None,
                   ),
                   tuple([var("xs"), var("f"), int(0)]),
                 ),
-                None,
-                Some("filteri+"),
+                ~name="filteri+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1143,13 +1111,10 @@ let builtins =
                     fn(
                       Pat.var("t"),
                       bin_op(Poly(Equals), var("x"), var("t")),
-                      None,
-                      None,
                     ),
                   ]),
                 ),
-                None,
-                Some("mem+"),
+                ~name="mem+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1205,8 +1170,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("partition+"),
+                ~name="partition+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1243,8 +1207,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("rev_append+"),
+                ~name="rev_append+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1316,8 +1279,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("fold_left2+"),
+                ~name="fold_left2+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1393,8 +1355,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("fold_right2+"),
+                ~name="fold_right2+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1457,8 +1418,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("map2+"),
+                ~name="map2+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1516,8 +1476,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("all2+"),
+                ~name="all2+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1578,8 +1537,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("any2+"),
+                ~name="any2+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1621,8 +1579,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("find+"),
+                ~name="find+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1667,8 +1624,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("take_while+"),
+                ~name="take_while+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1710,8 +1666,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("drop_while+"),
+                ~name="drop_while+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1774,8 +1729,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("filter_map+"),
+                ~name="filter_map+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1819,8 +1773,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("nth_opt+"),
+                ~name="nth_opt+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1862,8 +1815,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("find_opt+"),
+                ~name="find_opt+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1920,15 +1872,13 @@ let builtins =
                           ),
                         ],
                       ),
-                      None,
-                      Some("find_index_helper+"),
+                      ~name="find_index_helper+",
                     ),
                     None,
                   ),
                   tuple([var("xs"), var("pred"), int(0)]),
                 ),
-                None,
-                Some("find_index+"),
+                ~name="find_index+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -1984,8 +1934,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("find_map+"),
+                ~name="find_map+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2060,15 +2009,13 @@ let builtins =
                           ),
                         ],
                       ),
-                      None,
-                      Some("find_mapi_helper+"),
+                      ~name="find_mapi_helper+",
                     ),
                     None,
                   ),
                   tuple([var("xs"), var("f"), int(0)]),
                 ),
-                None,
-                Some("find_mapi+"),
+                ~name="find_mapi+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2120,15 +2067,13 @@ let builtins =
                           ),
                         ),
                       ),
-                      None,
-                      Some("init_helper+"),
+                      ~name="init_helper+",
                     ),
                     None,
                   ),
                   tuple([var("n"), var("f"), int(0)]),
                 ),
-                None,
-                Some("init+"),
+                ~name="init+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2176,8 +2121,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("assoc+"),
+                ~name="assoc+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2225,8 +2169,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("assoc_opt+"),
+                ~name="assoc_opt+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2274,8 +2217,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("mem_assoc+"),
+                ~name="mem_assoc+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2330,8 +2272,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("remove_assoc+"),
+                ~name="remove_assoc+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2401,8 +2342,7 @@ let builtins =
                     ),
                   ],
                 ),
-                None,
-                Some("partition_map+"),
+                ~name="partition_map+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2555,8 +2495,7 @@ let go: ([?], [?], [?]) -> [?] =
                               ),
                             ],
                           ),
-                          None,
-                          Some("go+"),
+                          ~name="go+",
                         ),
                         None,
                       ),
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2566,8 +2505,7 @@ let go: ([?], [?], [?]) -> [?] =
                         tuple([var("xs"), var("ys"), list_lit([])]),
                       ),
                     ),
-                    None,
-                    Some("merge+"),
+                    ~name="merge+",
                   ),
                   let_(
                     Pat.var("split"),
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2602,8 +2540,7 @@ let go: ([?], [?], [?]) -> [?] =
                             ),
                           ],
                         ),
-                        None,
-                        Some("split+"),
+                        ~name="split+",
                       ),
                       None,
                     ),
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2650,17 +2587,15 @@ let go: ([?], [?], [?]) -> [?] =
                               ),
                             ],
                           ),
-                          None,
-                          Some("merge_sort+"),
+                          ~name="merge_sort+",
                         ),
                         None,
                       ),
                       ap(Forward, var("merge_sort"), var("xs")),
                     ),
                   ),
                 ),
-                None,
-                Some("sort+"),
+                ~name="sort+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2696,8 +2631,7 @@ let go: ([?], [?], [?]) -> [?] =
                     bin_op(Int(Minus), var("end"), var("start")),
                   ]),
                 ),
-                None,
-                Some("slice+"),
+                ~name="slice+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2730,8 +2664,7 @@ let go: ([?], [?], [?]) -> [?] =
                     ),
                   ],
                 ),
-                None,
-                Some("hd_opt+"),
+                ~name="hd_opt+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2764,8 +2697,7 @@ let go: ([?], [?], [?]) -> [?] =
                     ),
                   ],
                 ),
-                None,
-                Some("tl_opt+"),
+                ~name="tl_opt+",
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2913,14 +2845,10 @@ let go: ([?], [?], [?]) -> [?] =
                           tuple([var("seen"), list_lit([var("x")])]),
                         ),
                       ),
-                      None,
-                      None,
                     ),
                     list_lit([]),
                   ]),
                 ),
-                None,
-                None,
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -2991,8 +2919,6 @@ let go: ([?], [?], [?]) -> [?] =
                     var("col"),
                   ),
                 ),
-                None,
-                None,
               ),
             ]),
           );
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -3015,10 +2941,7 @@ let go: ([?], [?], [?]) -> [?] =
             ap(
               Forward,
               var("map"),
-              tuple([
-                var("new_cols"),
-                fn(Pat.var("col"), lvs, None, None),
-              ]),
+              tuple([var("new_cols"), fn(Pat.var("col"), lvs)]),
             ),
           );
 
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -3034,8 +2957,6 @@ let go: ([?], [?], [?]) -> [?] =
                   tuple([tup_label(label("index"), var("idx"))]),
                   from_lvs,
                 ),
-                None,
-                None,
               ),
             ]),
           );
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -3053,8 +2974,6 @@ let go: ([?], [?], [?]) -> [?] =
               indices,
               let_(Pat.var("new_cols"), new_cols, mapped),
             ),
-            None,
-            None,
           );
         fix_f(Pat.var("pivot_table"), fn, None);
       },
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -3079,6 +2998,7 @@ let go: ([?], [?], [?]) -> [?] =
             fix_f(
               Pat.var("group_on_key"),
               fn(
+                ~name="group_on_key+",
                 Pat.tuple([Pat.var("xs"), Pat.var("f")]),
                 ap(
                   Forward,
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -3090,6 +3010,7 @@ let go: ([?], [?], [?]) -> [?] =
                       fix_f(
                         Pat.var("update_groups"),
                         fn(
+                          ~name="update_groups+",
                           Pat.tuple([
                             Pat.var("acc"),
                             Pat.var("key"),
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -3144,8 +3065,6 @@ let go: ([?], [?], [?]) -> [?] =
                               ),
                             ],
                           ),
-                          None,
-                          Some("update_groups+"),
                         ),
                         None,
                       ),
```

</details>

<details>
<summary><code>src/language/builtins/BuiltinsList.re</code> · Drop <code>None</code>s / use <code>~name</code></summary>

<!-- changetour:hunk file="src/language/builtins/BuiltinsList.re" baseBlob="280b15136db2919b7cd58b07173a225bb81f6b5b" -->

```diff
@@ -3160,15 +3079,11 @@ let go: ([?], [?], [?]) -> [?] =
                             var("x"),
                           ]),
                         ),
-                        None,
-                        None,
                       ),
                     ),
                     list_lit([]),
                   ]),
                 ),
-                None,
-                Some("group_on_key+"),
               ),
               None,
             )
```

</details>

<details>
<summary><code>src/haz3lcore/projectors/implementations/TableTransforms.re</code> · Drop <code>None, None</code> from <code>fn</code> calls</summary>

<!-- changetour:hunk file="src/haz3lcore/projectors/implementations/TableTransforms.re" baseBlob="49971738c88c6cc6a3f56b9db7bfa53f801864d5" -->

```diff
@@ -164,8 +164,6 @@ let convert_column = (column: string, conversion_fn: string): transform => {
               ),
             ]),
           ),
-          None,
-          None,
         )
       ),
     )
```

</details>

<details>
<summary><code>src/haz3lcore/projectors/implementations/TableTransforms.re</code> · Drop <code>None, None</code> from <code>fn</code> calls</summary>

<!-- changetour:hunk file="src/haz3lcore/projectors/implementations/TableTransforms.re" baseBlob="49971738c88c6cc6a3f56b9db7bfa53f801864d5" -->

```diff
@@ -188,8 +186,6 @@ let rename_column = (old_name: string, new_name: string): transform => {
               tup_label(label(new_name), dot(var("r"), label(old_name))),
             ]),
           ),
-          None,
-          None,
         )
       ),
     )
```

</details>

<details>
<summary><code>src/haz3lcore/projectors/implementations/TableTransforms.re</code> · Drop <code>None, None</code> from <code>fn</code> calls</summary>

<!-- changetour:hunk file="src/haz3lcore/projectors/implementations/TableTransforms.re" baseBlob="49971738c88c6cc6a3f56b9db7bfa53f801864d5" -->

```diff
@@ -208,8 +204,6 @@ let add_column = (): transform =>
             var("r"),
             tuple([tup_label(empty_hole(), empty_hole())]),
           ),
-          None,
-          None,
         )
       ),
     )
```

</details>

<details>
<summary><code>src/haz3lcore/projectors/implementations/TableTransforms.re</code> · Drop <code>None, None</code> from <code>fn</code> calls</summary>

<!-- changetour:hunk file="src/haz3lcore/projectors/implementations/TableTransforms.re" baseBlob="49971738c88c6cc6a3f56b9db7bfa53f801864d5" -->

```diff
@@ -225,8 +219,6 @@ let clear_column = (column: string): transform => {
             var("r"),
             tuple([tup_label(label(column), empty_hole())]),
           ),
-          None,
-          None,
         )
       ),
     )
```

</details>

<details>
<summary><code>src/haz3lcore/projectors/implementations/TableTransforms.re</code> · Drop <code>None, None</code> from <code>fn</code> calls</summary>

<!-- changetour:hunk file="src/haz3lcore/projectors/implementations/TableTransforms.re" baseBlob="49971738c88c6cc6a3f56b9db7bfa53f801864d5" -->

```diff
@@ -245,8 +237,6 @@ let noop_column = (column: string): transform => {
               tup_label(label(column), dot(var("r"), label(column))),
             ]),
           ),
-          None,
-          None,
         )
       ),
     )
```

</details>

<details>
<summary><code>src/haz3lcore/projectors/implementations/TableTransforms.re</code> · Drop <code>None, None</code> from <code>fn</code> calls</summary>

<!-- changetour:hunk file="src/haz3lcore/projectors/implementations/TableTransforms.re" baseBlob="49971738c88c6cc6a3f56b9db7bfa53f801864d5" -->

```diff
@@ -261,12 +251,7 @@ let group_by_column = (column: string): transform => {
           var("group_on_key"),
           [
             deferral(InAp),
-            fn(
-              Pat.var("row"),
-              dot(var("row"), label(column)),
-              None,
-              None,
-            ),
+            fn(Pat.var("row"), dot(var("row"), label(column))),
           ],
         )
       ),
```

</details>

<details>
<summary><code>src/haz3lcore/projectors/implementations/TableTransforms.re</code> · Drop <code>None, None</code> from <code>fn</code> calls</summary>

<!-- changetour:hunk file="src/haz3lcore/projectors/implementations/TableTransforms.re" baseBlob="49971738c88c6cc6a3f56b9db7bfa53f801864d5" -->

```diff
@@ -285,8 +270,6 @@ let filter_by_column = (op, column: string): transform => {
             fn(
               Pat.var("row"),
               bin_op(op, dot(var("row"), label(column)), empty_hole()),
-              None,
-              None,
             ),
           ],
         )
```

</details>

<details>
<summary><code>src/haz3lcore/projectors/implementations/TableTransforms.re</code> · Drop <code>None, None</code> from <code>fn</code> calls</summary>

<!-- changetour:hunk file="src/haz3lcore/projectors/implementations/TableTransforms.re" baseBlob="49971738c88c6cc6a3f56b9db7bfa53f801864d5" -->

```diff
@@ -303,7 +286,7 @@ let custom_filter = (): transform =>
       Exp.(
         deferred_ap(
           var("filter"),
-          [deferral(InAp), fn(Pat.var("row"), empty_hole(), None, None)],
+          [deferral(InAp), fn(Pat.var("row"), empty_hole())],
         )
       ),
     )
```

</details>

<details>
<summary><code>src/haz3lcore/projectors/implementations/TableTransforms.re</code> · Drop <code>None, None</code> from <code>fn</code> calls</summary>

<!-- changetour:hunk file="src/haz3lcore/projectors/implementations/TableTransforms.re" baseBlob="49971738c88c6cc6a3f56b9db7bfa53f801864d5" -->

```diff
@@ -326,8 +309,6 @@ let string_match_filter = (column: string): transform =>
                 var("string_match"),
                 tuple([empty_hole(), dot(var("row"), label(column))]),
               ),
-              None,
-              None,
             ),
           ],
         )
```

</details>

<details>
<summary><code>src/haz3lcore/projectors/implementations/TableTransforms.re</code> · Drop <code>None, None</code> from <code>fn</code> calls</summary>

<!-- changetour:hunk file="src/haz3lcore/projectors/implementations/TableTransforms.re" baseBlob="49971738c88c6cc6a3f56b9db7bfa53f801864d5" -->

```diff
@@ -356,13 +337,9 @@ let drop_nones_column = (column: string): transform => {
                       var("row"),
                       tuple([tup_label(label(column), var("v"))]),
                     ),
-                    None,
-                    None,
                   ),
                 ]),
               ),
-              None,
-              None,
             ),
           ],
         )
```

</details>

<details>
<summary><code>src/haz3lcore/projectors/implementations/TableTransforms.re</code> · Drop <code>None, None</code> from <code>fn</code> calls</summary>

<!-- changetour:hunk file="src/haz3lcore/projectors/implementations/TableTransforms.re" baseBlob="49971738c88c6cc6a3f56b9db7bfa53f801864d5" -->

```diff
@@ -395,8 +372,6 @@ let provide_default_column = (column: string): transform => {
               ),
             ]),
           ),
-          None,
-          None,
         )
       ),
     )
```

</details>

<details>
<summary><code>src/haz3lcore/projectors/implementations/TableTransforms.re</code> · Drop <code>None, None</code> from <code>fn</code> calls</summary>

<!-- changetour:hunk file="src/haz3lcore/projectors/implementations/TableTransforms.re" baseBlob="49971738c88c6cc6a3f56b9db7bfa53f801864d5" -->

```diff
@@ -481,12 +456,7 @@ let sort_column =
             deferred_ap(
               var("sort"),
               [
-                fn(
-                  Pat.tuple([Pat.var("r1"), Pat.var("r2")]),
-                  body,
-                  None,
-                  None,
-                ),
+                fn(Pat.tuple([Pat.var("r1"), Pat.var("r2")]), body),
                 deferral(InAp),
               ],
             )
```

</details>

<details>
<summary><code>test/Test_Elaboration.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/Test_Elaboration.re" baseBlob="8ce7f277ceae06fe2494b0a24a1ed850c4eba285" -->

```diff
@@ -163,18 +163,14 @@ module PlainTests = {
     );
 
   // x => 4 + 5
-  let f =
-    Exp.(
-      fn(Pat.var("x"), bin_op(Int(Plus), int(4), int(5)), None, None)
-    );
+  let f = Exp.(fn(Pat.var("x"), bin_op(Int(Plus), int(4), int(5))));
 
   let f' =
     Exp.(
       fn(
         Pat.var("x"),
         bin_op(Int(Plus), int(4), int(5)),
-        Some(Typ.unknown(Hole(EmptyHole))),
-        None,
+        ~typ=Typ.unknown(Hole(EmptyHole)),
       )
     );
   let unapplied_function = () =>
```

</details>

<details>
<summary><code>test/Test_Elaboration.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/Test_Elaboration.re" baseBlob="8ce7f277ceae06fe2494b0a24a1ed850c4eba285" -->

```diff
@@ -217,12 +213,7 @@ module PlainTests = {
     Exp.(
       let_(
         Pat.(asc(var("f"), Typ.arrow(Typ.int(), Typ.int()))),
-        fn(
-          Pat.var("x"),
-          bin_op(Int(Plus), int(1), var("x")),
-          None,
-          None,
-        ),
+        fn(Pat.var("x"), bin_op(Int(Plus), int(1), var("x"))),
         int(55),
       )
     );
```

</details>

<details>
<summary><code>test/Test_Elaboration.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/Test_Elaboration.re" baseBlob="8ce7f277ceae06fe2494b0a24a1ed850c4eba285" -->

```diff
@@ -234,8 +225,8 @@ module PlainTests = {
         fn(
           Pat.var("x"),
           bin_op(Int(Plus), int(1), var("x")),
-          Some(Typ.int()),
-          Some("f"),
+          ~typ=Typ.int(),
+          ~name="f",
         ),
         int(55),
       )
```

</details>

<details>
<summary><code>test/Test_Elaboration.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/Test_Elaboration.re" baseBlob="8ce7f277ceae06fe2494b0a24a1ed850c4eba285" -->

```diff
@@ -632,8 +623,7 @@ module PlainTests = {
                 tuple([tup_label(label("a"), asc(var("x"), Typ.int()))])
               ),
               var("x"),
-              Some(Typ.(prod([tup_label(label("a"), int())]))),
-              None,
+              ~typ=Typ.(prod([tup_label(label("a"), int())])),
             ),
             tuple([tup_label(label("a"), int(1))]),
           )
```

</details>

<details>
<summary><code>test/Test_Elaboration.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/Test_Elaboration.re" baseBlob="8ce7f277ceae06fe2494b0a24a1ed850c4eba285" -->

```diff
@@ -655,8 +645,7 @@ module PlainTests = {
                 tuple([tup_label(label("a"), asc(var("x"), Typ.int()))])
               ),
               var("x"),
-              Some(Typ.(prod([tup_label(label("a"), Typ.int())]))),
-              None,
+              ~typ=Typ.(prod([tup_label(label("a"), Typ.int())])),
             ),
             tuple([tup_label(label("a"), int(1))]),
           )
```

</details>

<details>
<summary><code>test/Test_Elaboration.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/Test_Elaboration.re" baseBlob="8ce7f277ceae06fe2494b0a24a1ed850c4eba285" -->

```diff
@@ -706,10 +695,7 @@ module PlainTests = {
             fn(
               Pat.(tuple([tup_label(label("a"), var("x"))])),
               var("x"),
-              Some(
-                Typ.(prod([tup_label(label("a"), unknown(Internal))])),
-              ),
-              None,
+              ~typ=Typ.(prod([tup_label(label("a"), unknown(Internal))])),
             ),
             tuple([tup_label(label("a"), int(1))]),
           )
```

</details>

<details>
<summary><code>test/Test_Grammar.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/Test_Grammar.re" baseBlob="f98a99208c1fc4cb13bd674a8248d3433765c485" -->

```diff
@@ -46,7 +46,7 @@ let sample_expression = (cls_exp: Exp.cls): Grammar.UnitGrammar.exp => {
       | DrvQuote => drv_exp(DrvGrammar.placeholder(), DrvSort.Jdmt)
       | ListLit => list_lit([])
       | Constructor => constructor("A", None)
-      | Fun => fn(Pat.var("x"), var("x"), None, None)
+      | Fun => fn(Pat.var("x"), var("x"))
       | TypFun => typ_fun(TPat.var("x"), empty_hole(), None)
       | Label => label("label")
       | ExplicitNonlabel => explicit_non_label()
```

</details>

<details>
<summary><code>test/Test_Introduce.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/Test_Introduce.re" baseBlob="768fe73f3b27a4e28b3cdc545cfd9cc7d55c4c90" -->

```diff
@@ -76,7 +76,7 @@ let tests =
           check(
             option(exp),
             "Function",
-            Some(Exp.(fn(Pat.empty_hole(), empty_hole(), None, None))),
+            Some(Exp.(fn(Pat.empty_hole(), empty_hole()))),
             introduce_expression(Typ.(arrow(int(), int()))),
           )
         }),
```

</details>

<details>
<summary><code>test/Test_MakeTerm.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/Test_MakeTerm.re" baseBlob="c0499e817ff49e22e3c9599111014cb7c918dad3" -->

```diff
@@ -45,7 +45,7 @@ let tests =
         exp_check(
           let_(
             Pat.var("f"),
-            fn(Pat.var("x"), var("x"), None, None), // It seems as though the function naming happens during elaboration and not during parsing
+            fn(Pat.var("x"), var("x")), // It seems as though the function naming happens during elaboration and not during parsing
             int(1),
           ),
           "let f = fun x -> x in 1",
```

</details>

<details>
<summary><code>test/Test_MakeTerm.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/Test_MakeTerm.re" baseBlob="c0499e817ff49e22e3c9599111014cb7c918dad3" -->

```diff
@@ -55,7 +55,7 @@ let tests =
         exp_check(
           let_(
             Pat.empty_hole(),
-            fn(Pat.var("x"), empty_hole(), None, None),
+            fn(Pat.var("x"), empty_hole()),
             empty_hole(),
           ),
           "let    = fun x ->   in  ",
```

</details>

<details>
<summary><code>test/Test_MakeTerm.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/Test_MakeTerm.re" baseBlob="c0499e817ff49e22e3c9599111014cb7c918dad3" -->

```diff
@@ -227,8 +227,6 @@ let tests =
           fn(
             Pat.(tuple([tup_label(label("a"), empty_hole())])),
             empty_hole(),
-            None,
-            None,
           ),
           {|fun (`a`=?) -> ?|},
         )
```

</details>

<details>
<summary><code>test/Test_Menhir.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/Test_Menhir.re" baseBlob="cbdd6abbb7f4d7fd14101f1c93ae9bd96d8f8993" -->

```diff
@@ -207,11 +207,7 @@ let tests =
     "MenhirParser",
     Exp.[
       full_parser_test("Integer Literal", int(8), "8"),
-      full_parser_test(
-        "Fun",
-        fn(Pat.var("x"), var("x"), None, None),
-        "fun x -> x",
-      ),
+      full_parser_test("Fun", fn(Pat.var("x"), var("x")), "fun x -> x"),
       full_parser_test(
         "String Literal",
         string("Hello World"),
```

</details>

<details>
<summary><code>test/Test_Menhir.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/Test_Menhir.re" baseBlob="cbdd6abbb7f4d7fd14101f1c93ae9bd96d8f8993" -->

```diff
@@ -363,12 +359,7 @@ let tests =
       ),
       menhir_only_test(
         "named_function",
-        fn(
-          Pat.var("x"),
-          bin_op(Int(Plus), var("x"), int(5)),
-          None,
-          Some("f"),
-        ),
+        fn(Pat.var("x"), bin_op(Int(Plus), var("x"), int(5)), ~name="f"),
         "named_fun f x -> x + 5",
       ),
       full_parser_test(
```

</details>

<details>
<summary><code>test/Test_Menhir.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/Test_Menhir.re" baseBlob="cbdd6abbb7f4d7fd14101f1c93ae9bd96d8f8993" -->

```diff
@@ -420,8 +411,6 @@ let tests =
             ),
           ),
           empty_hole(),
-          None,
-          None,
         ),
         "fun (b : ? -> ?) -> ?",
       ),
```

</details>

<details>
<summary><code>test/Test_Substitution.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/Test_Substitution.re" baseBlob="f3271bc5d09baf25cbde6f75461e8a97847c8c8d" -->

```diff
@@ -31,9 +31,9 @@ let tests = (
       () => {
         let env =
           Environment.of_list([("x", Exp.var("x")), ("y", Exp.var("x"))]);
-        let expr = Exp.fn(Pat.var("x"), Exp.var("y"), None, None);
+        let expr = Exp.fn(Pat.var("x"), Exp.var("y"));
         let result = Substitution.in_exp(env, expr);
-        let expected = Exp.fn(Pat.var("x'"), Exp.var("x"), None, None);
+        let expected = Exp.fn(Pat.var("x'"), Exp.var("x"));
         check(exp, "x -> x in fn x. x", expected, result);
       },
     ),
```

</details>

<details>
<summary><code>test/evaluator/Test_Stepper.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/evaluator/Test_Stepper.re" baseBlob="fbfb505841aed301e06a55a01034cb2e937ff161" -->

```diff
@@ -33,12 +33,7 @@ let tests = (
           full_small_step_reduction(
             ap(
               Forward,
-              fn(
-                Pat.var("x"),
-                bin_op(Int(Plus), var("x"), int(1)),
-                None,
-                None,
-              ),
+              fn(Pat.var("x"), bin_op(Int(Plus), var("x"), int(1))),
               int(5),
             ),
           );
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Function.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/evaluator/Test_Evaluator_Function.re" baseBlob="2cd03859ee9622fd115e646734636e4fae35b34a" -->

```diff
@@ -75,7 +75,7 @@ let tests = (
           int(5),
           let_(
             Pat.(var("f")),
-            fn(Pat.(tuple([])), var("u"), None, None),
+            fn(Pat.(tuple([])), var("u")),
             let_(
               Pat.(var("u")),
               int(3),
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Function.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/evaluator/Test_Evaluator_Function.re" baseBlob="2cd03859ee9622fd115e646734636e4fae35b34a" -->

```diff
@@ -89,7 +89,7 @@ let tests = (
       evaluation_test(
         "(fun x -> x)(x)",
         var("x"),
-        ap(Forward, fn(Pat.(var("x")), var("x"), None, None), var("x")),
+        ap(Forward, fn(Pat.(var("x")), var("x")), var("x")),
       )
     ),
     test_case(
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Function.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/evaluator/Test_Evaluator_Function.re" baseBlob="2cd03859ee9622fd115e646734636e4fae35b34a" -->

```diff
@@ -104,12 +104,7 @@ let tests = (
             typ_ap(
               typ_fun(
                 TPat.(var("T")),
-                fn(
-                  Pat.(asc(var("x"), Typ.var("T"))),
-                  var("x"),
-                  None,
-                  None,
-                ),
+                fn(Pat.(asc(var("x"), Typ.var("T"))), var("x")),
                 None,
               ),
               Typ.int(),
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Function.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/evaluator/Test_Evaluator_Function.re" baseBlob="2cd03859ee9622fd115e646734636e4fae35b34a" -->

```diff
@@ -167,10 +162,7 @@ Ok(Lam("yo", Var("yo"))))|},
         int(42),
         ap(
           Forward,
-          deferred_ap(
-            fn(Pat.(var("f")), var("f"), None, None),
-            [deferral(InAp)],
-          ),
+          deferred_ap(fn(Pat.(var("f")), var("f")), [deferral(InAp)]),
           int(42),
         ),
       )
```

</details>

<details>
<summary><code>test/statics/Test_Statics_Tuples.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/statics/Test_Statics_Tuples.re" baseBlob="28c7e8b41333a60569123d3033c999e95c0e8015" -->

```diff
@@ -1088,8 +1088,6 @@ let tests = (
                   )
                 ),
                 int(1),
-                None,
-                None,
               )
             )
           ),
```

</details>

<details>
<summary><code>test/statics/Test_Statics_Types.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/statics/Test_Statics_Types.re" baseBlob="74983034380c5cf45dcd5e4cca055cbea2a4e1ea" -->

```diff
@@ -510,8 +510,6 @@ let tests = (
                   )
                 ),
                 list_lit([]),
-                None,
-                None,
               )
             ),
           );
```

</details>

<details>
<summary><code>test/statics/Test_Statics_Types.re</code> · Adopt labeled <code>fn</code></summary>

<!-- changetour:hunk file="test/statics/Test_Statics_Types.re" baseBlob="74983034380c5cf45dcd5e4cca055cbea2a4e1ea" -->

```diff
@@ -554,8 +552,6 @@ let tests = (
                   )
                 ),
                 float(132032.832758),
-                None,
-                None,
               )
             ),
           );
```

</details>

Second wave: model-shape plumbing. Every construction site of a `CodeWithStatics.Model` gains the two new fields, callers of `Statics.uexp_to_info_map` pass an empty dynamics map, and consumers of the old bare probe map go through `.probe_map` or unwrap the `Calc`.

<details>
<summary><code>src/haz3lcore/CompositionCore/GeneralTreeUtils.re</code> · Pass empty dynamics to statics</summary>

<!-- changetour:hunk file="src/haz3lcore/CompositionCore/GeneralTreeUtils.re" baseBlob="23dfadf6c5686eb2f488d4852745ba03622ed7a8" -->

```diff
@@ -44,6 +44,7 @@ let subtree_of =
             ? {
               let (_, _, m) =
                 Statics.uexp_to_info_map(
+                  ~dynamics=LiveTyping.Map.empty,
                   ~ctx=def_info.ctx,
                   ~ana=def_info.ana,
                   ~is_in_filter=false,
```

</details>

<details>
<summary><code>src/haz3lcore/CompositionCore/GeneralTreeUtils.re</code> · Pass empty dynamics to statics</summary>

<!-- changetour:hunk file="src/haz3lcore/CompositionCore/GeneralTreeUtils.re" baseBlob="23dfadf6c5686eb2f488d4852745ba03622ed7a8" -->

```diff
@@ -60,6 +61,7 @@ let subtree_of =
             ? {
               let (_, _, m) =
                 Statics.uexp_to_info_map(
+                  ~dynamics=LiveTyping.Map.empty,
                   ~ctx=body_info.ctx,
                   ~ana=body_info.ana,
                   ~is_in_filter=false,
```

</details>

<details>
<summary><code>src/haz3lcore/CompositionCore/GeneralTreeUtils.re</code> · Pass empty dynamics to statics</summary>

<!-- changetour:hunk file="src/haz3lcore/CompositionCore/GeneralTreeUtils.re" baseBlob="23dfadf6c5686eb2f488d4852745ba03622ed7a8" -->

```diff
@@ -104,6 +106,7 @@ let subtree_of =
             ? {
               let (_, _, m) =
                 Statics.uexp_to_info_map(
+                  ~dynamics=LiveTyping.Map.empty,
                   ~ctx=body_info.ctx,
                   ~ana=body_info.ana,
                   ~is_in_filter=false,
```

</details>

<details>
<summary><code>src/haz3lcore/CompositionCore/GeneralTreeUtils.re</code> · Pass empty dynamics to statics</summary>

<!-- changetour:hunk file="src/haz3lcore/CompositionCore/GeneralTreeUtils.re" baseBlob="23dfadf6c5686eb2f488d4852745ba03622ed7a8" -->

```diff
@@ -131,6 +134,7 @@ let subtree_of =
             ? {
               let (_, _, m) =
                 Statics.uexp_to_info_map(
+                  ~dynamics=LiveTyping.Map.empty,
                   ~ctx=def_info.ctx,
                   ~ana=def_info.ana,
                   ~is_in_filter=false,
```

</details>

<details>
<summary><code>src/haz3lcore/CompositionCore/GeneralTreeUtils.re</code> · Pass empty dynamics to statics</summary>

<!-- changetour:hunk file="src/haz3lcore/CompositionCore/GeneralTreeUtils.re" baseBlob="23dfadf6c5686eb2f488d4852745ba03622ed7a8" -->

```diff
@@ -147,6 +151,7 @@ let subtree_of =
             ? {
               let (_, _, m) =
                 Statics.uexp_to_info_map(
+                  ~dynamics=LiveTyping.Map.empty,
                   ~ctx=body_info.ctx,
                   ~ana=body_info.ana,
                   ~is_in_filter=false,
```

</details>

<details>
<summary><code>src/web/app/explainthis/ExplainThis.re</code> · Model fields</summary>

<!-- changetour:hunk file="src/web/app/explainthis/ExplainThis.re" baseBlob="8f7628e2303558ccef4da61d22ff5a35fcf498fe" -->

```diff
@@ -701,8 +701,10 @@ let get_doc =
           {
             editor,
             statics: CachedStatics.empty,
-            dynamics: Dynamics.Map.empty,
+            dynamics: Dynamics.empty,
             context_menu: None,
+            live_typing: Pending,
+            sample_focus: Pending,
           },
         );
       let example_view =
```

</details>

<details>
<summary><code>src/web/derivation/DrvExplainThis.re</code> · Model fields</summary>

<!-- changetour:hunk file="src/web/derivation/DrvExplainThis.re" baseBlob="63a77293bb7441f51893ac06006c881b00781d0c" -->

```diff
@@ -45,8 +45,10 @@ let exp_show =
     {
       editor,
       statics,
-      dynamics: Dynamics.Map.empty,
+      dynamics: Dynamics.empty,
       context_menu: None,
+      live_typing: Util.Calc.Pending,
+      sample_focus: Util.Calc.Pending,
     },
   );
 };
```

</details>

<details>
<summary><code>src/web/derivation/DrvExplainThis.re</code> · Model fields</summary>

<!-- changetour:hunk file="src/web/derivation/DrvExplainThis.re" baseBlob="63a77293bb7441f51893ac06006c881b00781d0c" -->

```diff
@@ -79,8 +81,10 @@ let test_show =
       {
         editor,
         statics: CachedStatics.empty,
-        dynamics: Dynamics.Map.empty,
+        dynamics: Dynamics.empty,
         context_menu: None,
+        live_typing: Util.Calc.Pending,
+        sample_focus: Util.Calc.Pending,
       },
     );
   };
```

</details>

<details>
<summary><code>src/web/view/AgentCore/Agent.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/AgentCore/Agent.re" baseBlob="bc555e01a8268ef0d66717f0d5cc9a0095cf557e" -->

```diff
@@ -1456,7 +1456,7 @@ module Agent = {
             ~settings=settings.core,
             action,
             editor.statics,
-            editor.dynamics,
+            editor.dynamics.probe_map,
             editor.editor,
           );
         switch (updated_editor) {
```

</details>

<details>
<summary><code>src/web/view/AgentCore/Agent.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/AgentCore/Agent.re" baseBlob="bc555e01a8268ef0d66717f0d5cc9a0095cf557e" -->

```diff
@@ -1467,6 +1467,8 @@ module Agent = {
               editor: updated_editor,
               statics: editor.statics,
               dynamics: editor.dynamics,
+              live_typing: editor.live_typing,
+              sample_focus: editor.sample_focus,
               context_menu: editor.context_menu,
             },
           ))
```

</details>

<details>
<summary><code>src/web/view/AgentCore/Agent.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/AgentCore/Agent.re" baseBlob="bc555e01a8268ef0d66717f0d5cc9a0095cf557e" -->

```diff
@@ -1761,7 +1763,7 @@ module Agent = {
       let cws = cell_editor.editor;
       let agent_editor_view_string =
         CompositionView.Public.print(
-          ~probe_map=cws.dynamics,
+          ~probe_map=cws.dynamics.probe_map,
           cws.editor,
           curr_chat.agent_view,
         );
```

</details>

<details>
<summary><code>src/web/view/AgentCore/Agent.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/AgentCore/Agent.re" baseBlob="bc555e01a8268ef0d66717f0d5cc9a0095cf557e" -->

```diff
@@ -1772,7 +1774,7 @@ module Agent = {
         |> String.concat("\n");
       let test_results_info_string =
         test_results_string(
-          EvalResult.Model.test_results(cell_editor.result),
+          EvalResult.Model.test_results(cell_editor.result) |> Calc.get_value,
         );
       Message.Utils.mk_context_message(
         agent_editor_view_string,
```

</details>

<details>
<summary><code>src/web/view/AgentCore/Agent.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/AgentCore/Agent.re" baseBlob="bc555e01a8268ef0d66717f0d5cc9a0095cf557e" -->

```diff
@@ -2108,7 +2110,7 @@ module Agent = {
       let curr_chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
       let agent_editor_view_string =
         CompositionView.Public.print(
-          ~probe_map=editor.dynamics,
+          ~probe_map=editor.dynamics.probe_map,
           editor.editor,
           curr_chat.agent_view,
         );
```

</details>

<details>
<summary><code>src/web/view/AgentCore/Agent.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/AgentCore/Agent.re" baseBlob="bc555e01a8268ef0d66717f0d5cc9a0095cf557e" -->

```diff
@@ -2602,7 +2604,9 @@ module Agent = {
       | SendMessage(message, chat_id) =>
         let model =
           update_context(
-            ~test_results=?EvalResult.Model.test_results(editor.result),
+            ~test_results=?
+              EvalResult.Model.test_results(editor.result)
+              |> Util.Calc.get_value,
             model,
             editor.editor,
             chat_id,
```

</details>

<details>
<summary><code>src/web/view/AgentCore/Agent.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/AgentCore/Agent.re" baseBlob="bc555e01a8268ef0d66717f0d5cc9a0095cf557e" -->

```diff
@@ -2804,7 +2808,9 @@ module Agent = {
       | DoRetryApiSend(chat_id, attempt) =>
         let model =
           update_context(
-            ~test_results=?EvalResult.Model.test_results(editor.result),
+            ~test_results=?
+              EvalResult.Model.test_results(editor.result)
+              |> Util.Calc.get_value,
             model,
             editor.editor,
             chat_id,
```

</details>

<details>
<summary><code>src/web/view/AgentView/ChatMessagesView.re</code> · Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/AgentView/ChatMessagesView.re" baseBlob="a3abf555e50a63dc41efb915f00cfde46b863e99" -->

```diff
@@ -151,7 +151,8 @@ module ViewComponents = {
         ],
       );
     let test_section = {
-      let test_results_opt = EvalResult.Model.test_results(eval_result);
+      let test_results_opt =
+        EvalResult.Model.test_results(eval_result) |> Calc.get_value;
       div(
         ~attrs=[clss(["agent-context-section"])],
         [
```

</details>

<details>
<summary><code>src/web/view/CodeExerciseMode.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/CodeExerciseMode.re" baseBlob="0e62ba86a954051960ada6bfdac855b7aeba0c28" -->

```diff
@@ -463,8 +463,11 @@ module Update = {
             editor: {
               editor,
               statics: cell.editor.statics,
-              dynamics: EvalResult.Model.dynamics(cell.result),
+              dynamics:
+                EvalResult.Model.dynamics_full(cell.result) |> Calc.get_value,
               context_menu: cell.editor.context_menu,
+              live_typing: cell.editor.live_typing,
+              sample_focus: cell.editor.sample_focus,
             },
             result: cell.result,
           }
```

</details>

<details>
<summary><code>src/web/view/CodeExerciseMode.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/CodeExerciseMode.re" baseBlob="0e62ba86a954051960ada6bfdac855b7aeba0c28" -->

```diff
@@ -538,20 +541,20 @@ module Update = {
         prelude:
           calculate(
             cells.prelude.editor.statics,
-            cells.prelude.editor.dynamics,
+            cells.prelude.editor.dynamics.probe_map,
             model.editors.prelude,
           ),
         correct_impl:
           calculate(
             cells.test_validation.editor.statics,
-            cells.test_validation.editor.dynamics,
+            cells.test_validation.editor.dynamics.probe_map,
             model.editors.correct_impl,
           ),
         your_tests: {
           tests:
             calculate(
               cells.user_tests.editor.statics,
-              cells.user_tests.editor.dynamics,
+              cells.user_tests.editor.dynamics.probe_map,
               model.editors.your_tests.tests,
             ),
           required: model.editors.your_tests.required,
```

</details>

<details>
<summary><code>src/web/view/CodeExerciseMode.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/CodeExerciseMode.re" baseBlob="0e62ba86a954051960ada6bfdac855b7aeba0c28" -->

```diff
@@ -560,7 +563,7 @@ module Update = {
         your_impl:
           calculate(
             cells.user_impl.editor.statics,
-            cells.user_impl.editor.dynamics,
+            cells.user_impl.editor.dynamics.probe_map,
             model.editors.your_impl,
           ),
         hidden_bugs:
```

</details>

<details>
<summary><code>src/web/view/CodeExerciseMode.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/CodeExerciseMode.re" baseBlob="0e62ba86a954051960ada6bfdac855b7aeba0c28" -->

```diff
@@ -571,7 +574,7 @@ module Update = {
                 impl:
                   calculate(
                     cell.editor.statics,
-                    cell.editor.dynamics,
+                    cell.editor.dynamics.probe_map,
                     editor.impl,
                   ),
                 hint: editor.hint,
```

</details>

<details>
<summary><code>src/web/view/CodeExerciseMode.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/CodeExerciseMode.re" baseBlob="0e62ba86a954051960ada6bfdac855b7aeba0c28" -->

```diff
@@ -583,7 +586,7 @@ module Update = {
           tests:
             calculate(
               cells.hidden_tests.editor.statics,
-              cells.hidden_tests.editor.dynamics,
+              cells.hidden_tests.editor.dynamics.probe_map,
               model.editors.hidden_tests.tests,
             ),
           hints: model.editors.hidden_tests.hints,
```

</details>

<details>
<summary><code>src/web/view/CodeExerciseMode.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/CodeExerciseMode.re" baseBlob="0e62ba86a954051960ada6bfdac855b7aeba0c28" -->

```diff
@@ -685,7 +688,7 @@ module View = {
     let stitched_tests =
       CodeExercise.map_stitched(
         (_, cell_editor: CellEditor.Model.t) =>
-          cell_editor.result |> EvalResult.Model.test_results,
+          cell_editor.result |> EvalResult.Model.test_results |> Calc.get_value,
         model.cells,
       );
 
```

</details>

<details>
<summary><code>src/web/view/CodeExerciseMode.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/CodeExerciseMode.re" baseBlob="0e62ba86a954051960ada6bfdac855b7aeba0c28" -->

```diff
@@ -832,8 +835,10 @@ module View = {
       editor: {
         editor: editor.editor.editor,
         statics: editor.editor.statics,
-        dynamics: Language.Dynamics.Map.empty,
+        dynamics: Language.Dynamics.empty,
         context_menu: editor.editor.context_menu,
+        live_typing: editor.editor.live_typing,
+        sample_focus: editor.editor.sample_focus,
       },
       result: editor.result,
     };
```

</details>

<details>
<summary><code>src/web/view/DerivationExerciseMode.re</code> · Model fields / <code>.probe_map</code></summary>

<!-- changetour:hunk file="src/web/view/DerivationExerciseMode.re" baseBlob="fc1311542e5aa7642dce0725b287adda6f0ddaf1" -->

```diff
@@ -427,6 +427,8 @@ module Update = {
                   statics: cell.editor.statics,
                   dynamics: cell.editor.dynamics,
                   context_menu: cell.editor.context_menu,
+                  live_typing: cell.editor.live_typing,
+                  sample_focus: cell.editor.sample_focus,
                 },
                 result: cell.result,
               };
```

</details>

<details>
<summary><code>src/web/view/DerivationExerciseMode.re</code> · Model fields / <code>.probe_map</code></summary>

<!-- changetour:hunk file="src/web/view/DerivationExerciseMode.re" baseBlob="fc1311542e5aa7642dce0725b287adda6f0ddaf1" -->

```diff
@@ -496,13 +498,13 @@ module Update = {
         prelude:
           calculate(
             cells.prelude.editor.statics,
-            cells.prelude.editor.dynamics,
+            cells.prelude.editor.dynamics.probe_map,
             model.editors.prelude,
           ),
         setup:
           calculate(
             cells.setup.editor.statics,
-            cells.setup.editor.dynamics,
+            cells.setup.editor.dynamics.probe_map,
             model.editors.setup,
           ),
         trees: {
```

</details>

<details>
<summary><code>src/web/view/DerivationExerciseMode.re</code> · Model fields / <code>.probe_map</code></summary>

<!-- changetour:hunk file="src/web/view/DerivationExerciseMode.re" baseBlob="fc1311542e5aa7642dce0725b287adda6f0ddaf1" -->

```diff
@@ -521,7 +523,7 @@ module Update = {
                          jdmt:
                            calculate(
                              di.editor.statics,
-                             di.editor.dynamics,
+                             di.editor.dynamics.probe_map,
                              jdmt,
                            ),
                          rule,
```

</details>

<details>
<summary><code>src/web/view/TutorialMode.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/TutorialMode.re" baseBlob="ee8b33b9cff189094b271facfca0c2f3850d958d" -->

```diff
@@ -2,7 +2,7 @@ open Haz3lcore;
 open Virtual_dom.Vdom;
 open Node;
 // open ExplainThisUpdate;
-// open Util;
+open Util;
 /* The exercises mode interface for a single exercise. Composed of multiple editors and results. */
 /* This file follows conventions in [docs/ui-architecture.md] */
 module Model = {
```

</details>

<details>
<summary><code>src/web/view/TutorialMode.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/TutorialMode.re" baseBlob="ee8b33b9cff189094b271facfca0c2f3850d958d" -->

```diff
@@ -57,7 +57,7 @@ module Model = {
     let test_results =
       Tutorial.map_stitched(
         (_, cell_editor: CellEditor.Model.t) =>
-          cell_editor.result |> EvalResult.Model.test_results,
+          cell_editor.result |> EvalResult.Model.test_results |> Calc.get_value,
         exercise.cells,
       );
 
```

</details>

<details>
<summary><code>src/web/view/TutorialMode.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/TutorialMode.re" baseBlob="ee8b33b9cff189094b271facfca0c2f3850d958d" -->

```diff
@@ -71,7 +71,7 @@ module Model = {
     let test_results =
       Tutorial.map_stitched(
         (_, cell_editor: CellEditor.Model.t) =>
-          cell_editor.result |> EvalResult.Model.test_results,
+          cell_editor.result |> EvalResult.Model.test_results |> Calc.get_value,
         exercise.cells,
       );
 
```

</details>

<details>
<summary><code>src/web/view/TutorialMode.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/TutorialMode.re" baseBlob="ee8b33b9cff189094b271facfca0c2f3850d958d" -->

```diff
@@ -263,8 +263,11 @@ module Update = {
             editor: {
               editor,
               statics: cell.editor.statics,
-              dynamics: EvalResult.Model.dynamics(cell.result),
+              dynamics:
+                EvalResult.Model.dynamics_full(cell.result) |> Calc.get_value,
               context_menu: cell.editor.context_menu,
+              live_typing: cell.editor.live_typing,
+              sample_focus: cell.editor.sample_focus,
             },
             result: cell.result,
           }
```

</details>

<details>
<summary><code>src/web/view/TutorialMode.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/TutorialMode.re" baseBlob="ee8b33b9cff189094b271facfca0c2f3850d958d" -->

```diff
@@ -328,15 +331,15 @@ module Update = {
         your_impl:
           calculate(
             cells.user_impl.editor.statics,
-            cells.user_impl.editor.dynamics,
+            cells.user_impl.editor.dynamics.probe_map,
             model.editors.your_impl,
           ),
         display_hint: model.editors.display_hint,
         hidden_tests: {
           tests:
             calculate(
               cells.hidden_tests.editor.statics,
-              cells.hidden_tests.editor.dynamics,
+              cells.hidden_tests.editor.dynamics.probe_map,
               model.editors.hidden_tests.tests,
             ),
           hints: model.editors.hidden_tests.hints,
```

</details>

<details>
<summary><code>src/web/view/TutorialMode.re</code> · Model fields / <code>.probe_map</code> / Calc unwrap</summary>

<!-- changetour:hunk file="src/web/view/TutorialMode.re" baseBlob="ee8b33b9cff189094b271facfca0c2f3850d958d" -->

```diff
@@ -432,7 +435,7 @@ module View = {
     let stitched_tests =
       Tutorial.map_stitched(
         (_, cell_editor: CellEditor.Model.t) =>
-          cell_editor.result |> EvalResult.Model.test_results,
+          cell_editor.result |> EvalResult.Model.test_results |> Calc.get_value,
         model.cells,
       );
     let test_count =
```

</details>

<details>
<summary><code>src/web/app/probesystem/ProbeSidebar.re</code> · <code>.probe_map</code></summary>

<!-- changetour:hunk file="src/web/app/probesystem/ProbeSidebar.re" baseBlob="e379a1ffdc7dcfb37f28d2c0fdb80d54fd417df7" -->

```diff
@@ -570,7 +570,8 @@ let run_button = (~explain_this_inject, ~editor: CodeEditable.Model.t) => {
       clss(["run-button"]),
       Attr.title("Run and refresh print output"),
       Attr.on_click(_ => {
-        let entries = collect_print_entries(editor.dynamics, measured);
+        let entries =
+          collect_print_entries(editor.dynamics.probe_map, measured);
         cached_print_entries := List.is_empty(entries) ? None : Some(entries);
         explain_this_inject(ExplainThisUpdate.SpecificityOpen(true));
       }),
```

</details>

<details>
<summary><code>src/web/app/probesystem/ProbeSidebar.re</code> · <code>.probe_map</code></summary>

<!-- changetour:hunk file="src/web/app/probesystem/ProbeSidebar.re" baseBlob="e379a1ffdc7dcfb37f28d2c0fdb80d54fd417df7" -->

```diff
@@ -604,7 +605,7 @@ let printarium = (~explain_this_inject, ~editor: CodeEditable.Model.t) => {
   let entries =
     switch (eval_mode_ref^) {
     | Auto =>
-      let es = collect_print_entries(editor.dynamics, measured);
+      let es = collect_print_entries(editor.dynamics.probe_map, measured);
       List.is_empty(es) ? Option.none : Option.some(es);
     | Manual => cached_print_entries^
     };
```

</details>

<details>
<summary><code>src/web/app/editors/stepper/InductionCase.re</code> · Pass <code>Calc.OldValue(Dynamics.empty)</code></summary>

<!-- changetour:hunk file="src/web/app/editors/stepper/InductionCase.re" baseBlob="da2bdcf8abcfbc575a45c30ed73731a95e900b4c" -->

```diff
@@ -112,7 +112,7 @@ module F = (Stepper: STEPPER) => {
     let pattern =
       CodeEditable.Update.calculate(
         ~settings=Calc.get_value(settings),
-        ~dynamics=Dynamics.Map.empty,
+        ~dynamics=Calc.OldValue(Dynamics.empty),
         ~is_edited=true, // This editor technically edits Exps, but we want a Pat, so we put it in a function to emulate that.
         ~stitch=
           x =>
```

</details>

<details>
<summary><code>src/web/app/editors/stepper/InductionStep.re</code> · Pass <code>Calc.OldValue(Dynamics.empty)</code></summary>

<!-- changetour:hunk file="src/web/app/editors/stepper/InductionStep.re" baseBlob="9af5ff9c71b4b5fd4272804799178068afe83ce4" -->

```diff
@@ -197,7 +197,7 @@ module F =
       CodeEditable.Update.calculate(
         ~settings=Calc.get_value(settings),
         ~ctx=Calc.get_value(ctx).ctx,
-        ~dynamics=Dynamics.Map.empty,
+        ~dynamics=Calc.OldValue(Dynamics.empty),
         ~is_edited=true,
         ~stitch=x => x,
         ~is_dynamic_term=true,
```

</details>

<details>
<summary><code>src/web/app/editors/stepper/MissingStep.re</code> · Pass <code>Calc.OldValue(Dynamics.empty)</code></summary>

<!-- changetour:hunk file="src/web/app/editors/stepper/MissingStep.re" baseBlob="76a725de0b038c753a81c5cbde03ce6608cdd12b" -->

```diff
@@ -246,7 +246,7 @@ module Update = {
             ~settings,
             ~is_edited=true,
             ~is_dynamic_term=true,
-            ~dynamics=Dynamics.Map.empty,
+            ~dynamics=Calc.OldValue(Dynamics.empty),
             ~stitch=x => x,
             ~ctx=Calc.get_value(ctx) |> SemanticCtx.get_ctx,
             editor,
```

</details>

<details>
<summary><code>src/web/app/editors/stepper/StepperBase.re</code> · Pass <code>NewValue(Dynamics.empty)</code></summary>

<!-- changetour:hunk file="src/web/app/editors/stepper/StepperBase.re" baseBlob="d44e2e84680f1794ef9680c5304a4ddcfdd46918" -->

```diff
@@ -833,7 +833,7 @@ and Stepper: {
         ~settings=Calc.get_value(settings),
         ~is_edited=true,
         ~ctx=Calc.get_value(ctx) |> SemanticCtx.get_ctx,
-        ~dynamics=Dynamics.Map.empty,
+        ~dynamics=NewValue(Dynamics.empty),
         ~ana=Calc.get_value(ana),
         ~stitch=_ => Calc.get_value(expr),
         Calc.get_value(editor),
```

</details>

Odds and ends: CSS auto-formatter noise in `editor.css` (selector spacing, comment placement), a module-list reorder in `Util.re`, type annotations in `ValueChecker`, a TODO marker in `Unboxing`, and a small `OptUtil.or_else` helper (currently unreferenced).

<details>
<summary><code>src/web/www/style/editor.css</code> · Formatter noise</summary>

<!-- changetour:hunk file="src/web/www/style/editor.css" baseBlob="99778bad83a2faf63d58dd6c71a3d0e8440663a6" -->

```diff
@@ -82,12 +82,15 @@
 .code .token.Any {
   color: var(--token-any);
 }
+
 .code .token.Exp {
   color: var(--token-exp);
 }
+
 .code .token.Pat {
   color: var(--token-pat);
 }
+
 .code .token.Typ {
   color: var(--token-typ);
 }
```

</details>

<details>
<summary><code>src/web/www/style/editor.css</code> · Formatter noise</summary>

<!-- changetour:hunk file="src/web/www/style/editor.css" baseBlob="99778bad83a2faf63d58dd6c71a3d0e8440663a6" -->

```diff
@@ -97,6 +100,7 @@
 .code .token.Rul {
   color: var(--token-rul);
 }
+
 .code .token.TPat {
   color: var(--token-tpat);
 }
```

</details>

<details>
<summary><code>src/web/www/style/editor.css</code> · Formatter noise</summary>

<!-- changetour:hunk file="src/web/www/style/editor.css" baseBlob="99778bad83a2faf63d58dd6c71a3d0e8440663a6" -->

```diff
@@ -179,30 +183,44 @@
 @keyframes rainbow {
   0% {
     color: #4477ff;
-  } /* Start with muted blue */
+  }
+
+  /* Start with muted blue */
   17% {
     color: #9944ff;
-  } /* Muted purple */
+  }
+
+  /* Muted purple */
   33% {
     color: #ff4488;
-  } /* Muted pink */
+  }
+
+  /* Muted pink */
   50% {
     color: #ff7744;
-  } /* Muted orange */
+  }
+
+  /* Muted orange */
   67% {
     color: #88aa44;
-  } /* Muted green */
+  }
+
+  /* Muted green */
   83% {
     color: #44aaff;
-  } /* Muted cyan */
+  }
+
+  /* Muted cyan */
   100% {
     color: #4477ff;
-  } /* Back to muted blue */
+  }
+
+  /* Back to muted blue */
 }
 
 /* TOKEN BACKING DECOS */
 
-svg.shard > path {
+svg.shard>path {
   vector-effect: non-scaling-stroke;
 }
 
```

</details>

<details>
<summary><code>src/web/www/style/editor.css</code> · Formatter noise</summary>

<!-- changetour:hunk file="src/web/www/style/editor.css" baseBlob="99778bad83a2faf63d58dd6c71a3d0e8440663a6" -->

```diff
@@ -215,25 +233,25 @@ svg.shard {
 svg.shard.indicated.Any {
   filter: drop-shadow(var(--off-x) var(--off-y) var(--blur) var(--shadow-any));
 }
+
 svg.shard.indicated.Exp {
-  filter: drop-shadow(
-    var(--off-x) var(--off-y) var(--blur) var(--shard-lines-exp)
-  );
+  filter: drop-shadow(var(--off-x) var(--off-y) var(--blur) var(--shard-lines-exp));
 }
+
 svg.shard.indicated.Pat {
   filter: drop-shadow(var(--off-x) var(--off-y) var(--blur) var(--token-pat));
 }
+
 svg.shard.indicated.Typ {
   filter: drop-shadow(var(--off-x) var(--off-y) var(--blur) var(--token-typ));
 }
 svg.shard.indicated.Drv {
   filter: drop-shadow(var(--off-x) var(--off-y) var(--blur) var(--token-drv));
 }
 svg.shard.indicated.Rul {
-  filter: drop-shadow(
-    var(--off-x) var(--off-y) var(--blur) var(--shard-lines-rul)
-  );
+  filter: drop-shadow(var(--off-x) var(--off-y) var(--blur) var(--shard-lines-rul));
 }
+
 svg.shard.indicated.TPat {
   filter: drop-shadow(var(--off-x) var(--off-y) var(--blur) var(--token-tpat));
 }
```

</details>

<details>
<summary><code>src/web/www/style/editor.css</code> · Formatter noise</summary>

<!-- changetour:hunk file="src/web/www/style/editor.css" baseBlob="99778bad83a2faf63d58dd6c71a3d0e8440663a6" -->

```diff
@@ -247,25 +265,29 @@ svg.shard.indicated.MPat {
   filter: drop-shadow(var(--off-x) var(--off-y) var(--blur) var(--token-mpat));
 }
 
-svg.shard.indicated.Any > path {
+svg.shard.indicated.Any>path {
   fill: var(--shard-any);
 }
-svg.shard.indicated.Exp > path {
+
+svg.shard.indicated.Exp>path {
   fill: var(--shard-exp);
 }
-svg.shard.indicated.Pat > path {
+
+svg.shard.indicated.Pat>path {
   fill: var(--shard-pat);
 }
-svg.shard.indicated.Typ > path {
+
+svg.shard.indicated.Typ>path {
   fill: var(--shard-typ);
 }
 svg.shard.indicated.Drv > path {
   fill: var(--shard-drv);
 }
-svg.shard.indicated.Rul > path {
+svg.shard.indicated.Rul>path {
   fill: var(--shard-rul);
 }
-svg.shard.indicated.TPat > path {
+
+svg.shard.indicated.TPat>path {
   fill: var(--shard-tpat);
 }
 svg.shard.indicated.Mod > path {
```

</details>

<details>
<summary><code>src/web/www/style/editor.css</code> · Formatter noise</summary>

<!-- changetour:hunk file="src/web/www/style/editor.css" baseBlob="99778bad83a2faf63d58dd6c71a3d0e8440663a6" -->

```diff
@@ -278,16 +300,19 @@ svg.shard.indicated.MPat > path {
   fill: var(--shard-mpat);
 }
 
-svg.shard.indicated.caret.Exp > path {
+svg.shard.indicated.caret.Exp>path {
   fill: var(--shard-caret-exp);
 }
-svg.shard.indicated.caret.Pat > path {
+
+svg.shard.indicated.caret.Pat>path {
   fill: var(--shard-caret-pat);
 }
-svg.shard.indicated.caret.Typ > path {
+
+svg.shard.indicated.caret.Typ>path {
   fill: var(--shard-caret-typ);
 }
-.svg.shard.indicated.caret.TPat > path {
+
+.svg.shard.indicated.caret.TPat>path {
   fill: var(--shard-caret-tpat);
 }
 svg.shard.indicated.caret.Mod > path {
```

</details>

<details>
<summary><code>src/web/www/style/editor.css</code> · Formatter noise</summary>

<!-- changetour:hunk file="src/web/www/style/editor.css" baseBlob="99778bad83a2faf63d58dd6c71a3d0e8440663a6" -->

```diff
@@ -304,6 +329,7 @@ svg.shard.selected {
   z-index: var(--select-z);
   filter: drop-shadow(1px 1px 0 var(--shadow-selected));
 }
+
 svg.shard.indicated {
   z-index: var(--tile-z);
 }
```

</details>

<details>
<summary><code>src/web/www/style/editor.css</code> · Formatter noise</summary>

<!-- changetour:hunk file="src/web/www/style/editor.css" baseBlob="99778bad83a2faf63d58dd6c71a3d0e8440663a6" -->

```diff
@@ -336,14 +362,16 @@ svg.shard.selected-expanded > path {
   z-index: var(--err-hole-z);
   filter: none;
 }
-.errors .errors-piece svg.shard > path,
+
+.errors .errors-piece svg.shard>path,
 .errors .errors-piece svg .child-line {
   stroke: var(--error-hole-stroke);
   stroke-width: 0.75px;
   stroke-dasharray: 1, 1;
   stroke-linecap: butt;
 }
-.errors .errors-piece svg.shard > path {
+
+.errors .errors-piece svg.shard>path {
   fill: var(--error-hole-fill);
 }
 
```

</details>

<details>
<summary><code>src/web/www/style/editor.css</code> · Formatter noise</summary>

<!-- changetour:hunk file="src/web/www/style/editor.css" baseBlob="99778bad83a2faf63d58dd6c71a3d0e8440663a6" -->

```diff
@@ -416,7 +469,7 @@ svg.shard.selected.buffer-parsed > path {
 
 /* EMPTY HOLE DECO */
 
-.empty-hole > path {
+.empty-hole>path {
   fill: var(--empty-hole-fill);
   stroke: var(--empty-hole-stroke);
   stroke-width: 0.75px;
```

</details>

<details>
<summary><code>src/web/www/style/editor.css</code> · Formatter noise</summary>

<!-- changetour:hunk file="src/web/www/style/editor.css" baseBlob="99778bad83a2faf63d58dd6c71a3d0e8440663a6" -->

```diff
@@ -465,12 +518,15 @@ svg:has(.child-line) {
 .child-line.Any {
   stroke: var(--shadow-any);
 }
+
 .child-line.Exp {
   stroke: var(--shard-lines-exp);
 }
+
 .child-line.Pat {
   stroke: var(--token-pat);
 }
+
 .child-line.Typ {
   stroke: var(--token-typ);
 }
```

</details>

<details>
<summary><code>src/web/www/style/editor.css</code> · Formatter noise</summary>

<!-- changetour:hunk file="src/web/www/style/editor.css" baseBlob="99778bad83a2faf63d58dd6c71a3d0e8440663a6" -->

```diff
@@ -480,6 +536,7 @@ svg:has(.child-line) {
 .child-line.Rul {
   stroke: var(--shard-lines-rul);
 }
+
 .child-line.TPat {
   stroke: var(--token-tpat);
 }
```

</details>

<details>
<summary><code>src/web/www/style/editor.css</code> · Formatter noise</summary>

<!-- changetour:hunk file="src/web/www/style/editor.css" baseBlob="99778bad83a2faf63d58dd6c71a3d0e8440663a6" -->

```diff
@@ -499,7 +556,7 @@ svg:has(.child-line) {
   z-index: var(--backpack-z);
 }
 
-.backpack.cant-put-down > * {
+.backpack.cant-put-down>* {
   opacity: 40%;
 }
 
```

</details>

<details>
<summary><code>src/util/Util.re</code> · Module re-export reorder</summary>

<!-- changetour:hunk file="src/util/Util.re" baseBlob="f477f21f977623afa296b3f5c338180125b277db" -->

```diff
@@ -13,8 +13,8 @@ module PairUtil = PairUtil;
 module CsvUtil = CsvUtil;
 module Result = Result;
 module StateMonad = StateMonad;
-module StringUtil = StringUtil;
 module WriterMonad = WriterMonad;
+module StringUtil = StringUtil;
 module Tree = Tree;
 module TimeUtil = TimeUtil;
 module TupleUtil = TupleUtil;
```

</details>

<details>
<summary><code>src/util/OptUtil.re</code> · <code>or_else</code> helper (currently unreferenced)</summary>

<!-- changetour:hunk file="src/util/OptUtil.re" baseBlob="091290bb7f57245426793a0ae92865196d347889" -->

```diff
@@ -64,6 +64,13 @@ let filter = (f: 'a => bool, o: option('a)): option('a) =>
   | Some(a) => f(a) ? Some(a) : None
   };
 
+/* Returns the first option if it is Some, otherwise returns the second option. */
+let or_else = (o1: option('a), o2: option('a)): option('a) =>
+  switch (o1) {
+  | Some(_) => o1
+  | None => o2
+  };
+
 let value_exn = (~none, o) => get(() => raise(none), o);
 
 module Syntax = {
```

</details>

<details>
<summary><code>src/language/dynamics/ValueChecker.re</code> · Add env type annotations</summary>

<!-- changetour:hunk file="src/language/dynamics/ValueChecker.re" baseBlob="71824118a7108fd5b5fe40c055996386463f5fdd" -->

```diff
@@ -49,10 +49,10 @@ module ValueCheckerEVMode: {
 
 module CV = Transition(ValueCheckerEVMode);
 
-let rec check_value = (~in_closure=?, env, d) =>
+let rec check_value = (~in_closure=?, env: Environment.t(Exp.t), d) =>
   CV.transition(check_value, ~mode=`Environment, ~in_closure?, env, d);
 
-let rec check_value_mod_ctx = (~in_closure=?, env, d) =>
+let rec check_value_mod_ctx = (~in_closure=?, env: Environment.t(Exp.t), d) =>
   switch (DHExp.term_of(d)) {
   | Var(x) =>
     switch (Environment.lookup(env, x)) {
```

</details>

<details>
<summary><code>src/language/dynamics/transition/Unboxing.re</code> · TODO marker</summary>

<!-- changetour:hunk file="src/language/dynamics/transition/Unboxing.re" baseBlob="a43f74fc9ccc6ecb30d64c9d9c836656b3ff9cf4" -->

```diff
@@ -130,6 +130,7 @@ let rec unbox: type a. (unbox_request(a), DHExp.t) => unboxed(a) =
     | (LabeledTupleEntries, Tuple(ds)) =>
       let unbox_tup_label =
           (d: Exp.t): option((option(LabeledTuple.label), Exp.t)) => {
+        // TODO Samples
         switch (
           snd(Ascriptions.transition_multiple(~targets=Sample.no_targets, d)).
             term
```

</details>
