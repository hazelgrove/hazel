---
schemaVersion: 1
prNumber: 1388
prOwner: hazelgrove
prRepo: hazel
baseSha: fca48094c3a00e5ed97fee603eb820aac148ca9d
headSha: e7930d6993876e35c49fbea45e775429e56b16be
---

# refactor: use debug .. in for filter expression

This PR replaces the four separate filter keyword forms (`hide`, `eval`, `pause`, `debug`) with a single unified `debug action(pattern) in body` surface syntax. It also introduces two first-class expression variants — `FilterAction` and `FilterSelector` — so filter actions and the `$e`/`$v` selectors are proper typed expressions rather than magic constructor strings. The four actions are renamed for the new syntax: `hide` and `eval` keep their names, while `pause` becomes `stop` and the old `debug` action becomes `step`. The change touches the grammar, both parsers (tile-based and menhir), statics, dynamics, pretty-printer, ExplainThis docs, and the test suite.

## New expression variants: `FilterAction` and `FilterSelector`

Previously, filter actions like `eval` and `pause` were represented as `Constructor("$e", …)` strings in the expression tree — a fragile convention. This refactor promotes them to proper first-class variants `FilterAction` and `FilterSelector` in `Grammar.re`'s `exp_term` type and `FilterSelector.re`'s new module. The `filter` record also gains an `ids` field to carry annotation data through elaboration. The `Unresolved` constructor is added to `stepper_filter_kind_t` to hold the not-yet-resolved parsed expression before statics lifts it into a resolved `Filter`.

<details open>
<summary><code>src/language/term/FilterSelector.re</code> · [@deriving (show({with_path: false}), sexp, yojson, eq)]</summary>

<!-- changetour:hunk file=src/language/term/FilterSelector.re level=2 baseBlob=c3817869efea01caccb5af2a70b42011f9804572 -->

```diff
@@ -0,0 +1,4 @@
+[@deriving (show({with_path: false}), sexp, yojson, eq)]
+type t =
+  | Exp
+  | Val;
```

</details>

<details open>
<summary><code>src/language/term/Grammar.re</code> · | FilterAction(FilterAction.t)</summary>

<!-- changetour:hunk file=src/language/term/Grammar.re level=2 baseBlob=4ea8e9c4742a8e75182c60b21068b8f017fc458b -->

```diff
@@ -48,6 +48,8 @@ and exp_term('a) =
   | Dot(exp_t('a), exp_t('a))
   | LivelitName(string)
   | Var(Var.t)
+  | FilterAction(FilterAction.t)
+  | FilterSelector(FilterSelector.t)
   | Let(pat_t('a), exp_t('a), exp_t('a))
   | Theorem(pat_t('a), exp_t('a), exp_t('a))
   | ProofObject(exp_t('a))
```

</details>

Because actions are now ordinary expressions applied to a pattern, they need surface names that read as function calls. `FilterAction.re` defines the canonical string mapping — `stop`/`step`/`hide`/`eval` — which both parsers use to decide whether an applied variable is a filter action.

<details open>
<summary><code>src/language/term/FilterAction.re</code> · let string_of_t / t_of_string: stop, step, hide, eval</summary>

<!-- changetour:hunk file=src/language/term/FilterAction.re level=2 baseBlob=753d2a79ece157605328a192a7b46af126d2b52d -->

```diff
@@ -10,3 +10,22 @@ type count =
 
 [@deriving (show({with_path: false}), sexp, yojson, eq)]
 type t = (action, count);
+
+let string_of_t = action => {
+  switch (action) {
+  | (Step, One) => "stop"
+  | (Step, All) => "step"
+  | (Eval, One) => "hide"
+  | (Eval, All) => "eval"
+  };
+};
+
+let t_of_string = s => {
+  switch (s) {
+  | "stop" => Some((Step, One))
+  | "step" => Some((Step, All))
+  | "hide" => Some((Eval, One))
+  | "eval" => Some((Eval, All))
+  | _ => None
+  };
+};
```

</details>

Call sites that previously cooked up `Constructor("$e", …)` by hand now use the real variant: the equality engine's expression-wildcard cases match on `FilterSelector` directly, and the exercise/tutorial harnesses build their implicit wrapping filter with `FilterSelector(Exp)`.

<details open>
<summary><code>src/language/term/Equality.re</code> · | (FilterSelector(Val), _) when Option.is_some(use_expr_wildcards)</summary>

<!-- changetour:hunk file=src/language/term/Equality.re level=2 baseBlob=3d7614689421a30a16704f7f3759cbb7aef41942 -->

```diff
@@ -145,11 +145,11 @@ let equality =
       exp'(e1, e2)
 
     // Expression Wildcards:
-    | (Constructor("$v", _), _) when Option.is_some(use_expr_wildcards) =>
+    | (FilterSelector(Val), _) when Option.is_some(use_expr_wildcards) =>
       let check_value = Option.get(use_expr_wildcards);
       check_value(Option.value(env2, ~default=Environment.empty), e2);
     | (EmptyHole, _) when Option.is_some(use_expr_wildcards) => true
-    | (Constructor("$e", _), _) when Option.is_some(use_expr_wildcards) =>
+    | (FilterSelector(Exp), _) when Option.is_some(use_expr_wildcards) =>
       true
 
     /* These variable cases are quite complicated because they account for a lot of concerns.
```

</details>

<details open>
<summary><code>src/web/exercises/CodeExercise.re</code> · wrap_filter builds pat with FilterSelector(Exp)</summary>

<!-- changetour:hunk file=src/web/exercises/CodeExercise.re level=2 baseBlob=a87e01d7be8bb06b38b70affae3177417e54c847 -->

```diff
@@ -658,13 +658,10 @@ let wrap_filter =
       Filter({
         act: Language.FilterAction.(act, One),
         pat: {
-          term:
-            Constructor(
-              "$e",
-              Some(Some(Unknown(Internal) |> Language.Typ.fresh)),
-            ),
+          term: FilterSelector(Exp),
           annotation: Language.IdTagged.IdTag.fresh(),
         },
+        ids: Language.IdTagged.IdTag.fresh(),
       }),
       term,
     ),
```

</details>

<details open>
<summary><code>src/web/exercises/Tutorial.re</code> · wrap_filter builds pat with FilterSelector(Exp)</summary>

<!-- changetour:hunk file=src/web/exercises/Tutorial.re level=2 baseBlob=9ad096e098b9942c385cf2711848a718e1c189c1 -->

```diff
@@ -260,13 +260,13 @@ let wrap_filter =
       Filter({
         act: Language.FilterAction.(act, One),
         pat: {
-          term:
-            Constructor(
-              "$e",
-              Some(Some(Unknown(Internal) |> Language.Typ.fresh)),
-            ),
+          term: FilterSelector(Exp),
           annotation: Language.IdTagged.IdTag.fresh(),
         },
+        ids: {
+          ids: [Id.mk()],
+          secondary: Language.IdTagged.IdTag.empty_secondary,
+        },
       }),
       term,
     ),
```

</details>

Structural equality and proof-side expression matching get cases for the new variants, and filter equality learns to compare `Unresolved` filters (the new `ids` field is deliberately ignored when comparing resolved ones).

<details open>
<summary><code>src/language/term/Equality.re</code> · | (FilterAction(act1), FilterAction(act2)) =&gt; FilterAction.equal(act1, act2)</summary>

<!-- changetour:hunk file=src/language/term/Equality.re level=2 baseBlob=3d7614689421a30a16704f7f3759cbb7aef41942 -->

```diff
@@ -432,6 +432,12 @@ let equality =
     | (ModuleExp(_, _, _), _) => false
     | (DrvQuote(d1, s1), DrvQuote(d2, s2)) => s1 == s2 && d1 == d2
     | (DrvQuote(_, _), _) => false
+    | (FilterAction(act1), FilterAction(act2)) =>
+      FilterAction.equal(act1, act2)
+    | (FilterAction(_), _) => false
+    | (FilterSelector(sel1), FilterSelector(sel2)) =>
+      FilterSelector.equal(sel1, sel2)
+    | (FilterSelector(_), _) => false
     };
   }
   /* Compare patterns with literal variable names (no alpha-renaming).
```

</details>

<details open>
<summary><code>src/language/term/Equality.re</code> · | (Unresolved(e1), Unresolved(e2)) =&gt; exp'(e1, e2)</summary>

<!-- changetour:hunk file=src/language/term/Equality.re level=2 baseBlob=3d7614689421a30a16704f7f3759cbb7aef41942 -->

```diff
@@ -837,7 +843,9 @@ let equality =
       : bool => {
     let exp' = exp(alphas_exp, alphas_typ);
     switch (f1, f2) {
-    | (Filter({pat: pat1, act: act1}), Filter({pat: pat2, act: act2})) =>
+    | (Unresolved(e1), Unresolved(e2)) => exp'(e1, e2)
+    | (Unresolved(_), _) => false
+    | (Filter({pat: pat1, act: act1, _}), Filter({pat: pat2, act: act2, _})) =>
       exp'(pat1, pat2) && act1 == act2
     | (Filter(_), _) => false
     | (Residue(_), Residue(_)) => f1 == f2
```

</details>

<details open>
<summary><code>src/language/proof/MatchExp.re</code> · | (FilterAction(act1), FilterAction(act2)) when FilterAction.equal(act1, act2)</summary>

<!-- changetour:hunk file=src/language/proof/MatchExp.re level=2 baseBlob=a55332853209c95e4ca0f235e61cd55fa0775cf3 -->

```diff
@@ -113,6 +113,14 @@ let rec match_exp =
   | (Atom(SInt(_)), _) => None
   | (Atom(Nat(i1)), Atom(Nat(i2))) when i1 == i2 => Some(ctx)
   | (Atom(Nat(_)), _) => None
+  | (FilterAction(act1), FilterAction(act2))
+      when FilterAction.equal(act1, act2) =>
+    Some(ctx)
+  | (FilterAction(_), _) => None
+  | (FilterSelector(sel1), FilterSelector(sel2))
+      when FilterSelector.equal(sel1, sel2) =>
+    Some(ctx)
+  | (FilterSelector(_), _) => None
   | (ListLit(xs), ListLit(ys)) when List.length(xs) == List.length(ys) =>
     ListUtil.fold_left_opt(
       (ctx, (x, y)) => match_exp(alphas, ctx, x, y),
```

</details>

## Statics: resolve & typing debug filter

We added a new Unresolved variant to Filter so that we can postpone the resolution (pattern-matching, to be more precise) on the expression inside `debug <expr> in ...` and get the action and the filter pattern out, and populating the info map properly, so that facilities like ExplainThis can correctly display information about these language constructs.

<details open>
<summary><code>src/language/term/Grammar.re</code> · | Unresolved(exp_t('a))</summary>

<!-- changetour:hunk file=src/language/term/Grammar.re level=2 baseBlob=4ea8e9c4742a8e75182c60b21068b8f017fc458b -->

```diff
@@ -152,6 +154,7 @@ and mpat_term('a) =
   | Asc(mpat_t('a), typ_t('a))
 and mpat_t('a) = Annotated.t(mpat_term('a), 'a)
 and stepper_filter_kind_t('a) =
+  | Unresolved(exp_t('a))
   | Filter(filter('a))
   | Residue(int, FilterAction.t)
 and type_hole('a) =
```

</details>

<details open>
<summary><code>src/language/term/Grammar.re</code> · ids: 'a,</summary>

<!-- changetour:hunk file=src/language/term/Grammar.re level=2 baseBlob=4ea8e9c4742a8e75182c60b21068b8f017fc458b -->

```diff
@@ -165,6 +168,7 @@ and type_provenance('a) =
 and filter('a) = {
   pat: exp_t('a),
   act: FilterAction.t,
+  ids: 'a,
 };
 
 
```

</details>

<details open>
<summary><code>src/language/statics/Statics.re</code> · | Filter(Filter({pat: cond, act}), body) =&gt;</summary>

<!-- changetour:hunk file=src/language/statics/Statics.re level=2 baseBlob=7460016f3171a971b6523bcaf81914bc77b4c068 -->

```diff
@@ -1451,7 +1467,44 @@ and uexp_to_info_map =
           SubexpProbeTargets.union_all([e.probe_targets, hint.probe_targets]),
         m,
       );
-    | Filter(Filter({pat: cond, act}), body) =>
+    | Filter(Unresolved(fexp), body) =>
+      /* Recognize the parsed Ap form `act(pat)` as a resolved filter; lift
+         it into Filter(Filter({...})). Anything else stays Unresolved
+         (typed as syn for display) and we still descend into body. */
+      switch (fexp.term) {
+      | Ap(Forward, {term: FilterAction(act), annotation: act_ann}, pat) =>
+        let (_, _, m) = go(~ana=syn, fexp, m, ~is_in_filter=true);
+        let (cond, cond_elab, m) = go(~ana=syn, pat, m, ~is_in_filter=true);
+        let (body, body_elab, m) = go(~ana, body, m);
+        add(
+          ~elab_term=
+            Filter(
+              Filter({
+                act,
+                pat: cond_elab,
+                ids: act_ann,
+              }),
+              body_elab,
+            )
+            |> rewrap,
+          ~elab_syn_ty=body.elab_syn_ty,
+          ~marks=[],
+          ~co_ctx=CoCtx.union([cond.co_ctx, body.co_ctx]),
+          m,
+        );
+      | _ =>
+        let (fexp_info, fexp_elab, m) =
+          go(~ana=syn, fexp, m, ~is_in_filter=true);
+        let (body, body_elab, m) = go(~ana, body, m);
+        add(
+          ~elab_term=Filter(Unresolved(fexp_elab), body_elab) |> rewrap,
+          ~elab_syn_ty=body.elab_syn_ty,
+          ~marks=[],
+          ~co_ctx=CoCtx.union([fexp_info.co_ctx, body.co_ctx]),
+          m,
+        );
+      }
+    | Filter(Filter({pat: cond, act, ids: filter_ids}), body) =>
       let (cond, cond_elab, m) = go(~ana=syn, cond, m, ~is_in_filter=true);
       let (body, body_elab, m) = go(~ana, body, m);
       add(
```

</details>

The new leaf expressions themselves are typed as internal unknowns — they only ever appear inside a filter pattern position, so there is nothing meaningful to synthesize — and the already-resolved `Filter` branch simply threads the new `ids` field through elaboration.

<details open>
<summary><code>src/language/statics/Statics.re</code> · | FilterSelector(sel) =&gt; add(~elab_syn_ty=SynTy.unknown_internal(), …)</summary>

<!-- changetour:hunk file=src/language/statics/Statics.re level=2 baseBlob=7460016f3171a971b6523bcaf81914bc77b4c068 -->

```diff
@@ -672,6 +672,22 @@ and uexp_to_info_map =
         ~co_ctx,
         m,
       );
+    | FilterSelector(sel) =>
+      add(
+        ~elab_term=FilterSelector(sel) |> rewrap,
+        ~elab_syn_ty=SynTy.unknown_internal(),
+        ~marks=[],
+        ~co_ctx=CoCtx.empty,
+        m,
+      )
+    | FilterAction(act) =>
+      add(
+        ~elab_term=FilterAction(act) |> rewrap,
+        ~elab_syn_ty=SynTy.unknown_internal(),
+        ~marks=[],
+        ~co_ctx=CoCtx.empty,
+        m,
+      )
     | DynamicErrorHole(e, err) =>
       let (e, e_elab, m) = go(~ana, e, m);
       add(
```

</details>

<details open>
<summary><code>src/language/statics/Statics.re</code> · ids: filter_ids, threaded through elaborated Filter</summary>

<!-- changetour:hunk file=src/language/statics/Statics.re level=2 baseBlob=7460016f3171a971b6523bcaf81914bc77b4c068 -->

```diff
@@ -1460,6 +1513,7 @@ and uexp_to_info_map =
             Filter({
               act,
               pat: cond_elab,
+              ids: filter_ids,
             }),
             body_elab,
           )
```

</details>

## Parser & lexer: unified `debug .. in` syntax

The four separate keyword tokens (`PAUSE`, `HIDE`, `EVAL`, `DEBUG`) and their grammar rules collapse into a single `DEBUG … IN` form. The action and pattern are now ordinary expressions — `action(pattern)` — so the parser simply parses them as an expression argument. Two new lexer tokens `$e` and `$v` are added for the `FilterSelector` values, and the `AST.re` representation of `Filter` drops the explicit `filter_action` constructor argument.

<details open>
<summary><code>src/menhirParser/Lexer.mll</code> · (* Filter Selectors *)</summary>

<!-- changetour:hunk file=src/menhirParser/Lexer.mll level=2 baseBlob=fffdcd461d55d4d8fe0faf6f2ae260f49eb02b84 -->

```diff
@@ -98,6 +98,9 @@ rule token =
     | "!=." { NOT_EQUAL_FLOAT }
     (* String Ops *)
     | "++" { STRING_CONCAT }
+    (* Filter Selectors *)
+    | "$e" { FILTER_SELECTOR_EXP }
+    | "$v" { FILTER_SELECTOR_VAL }
     (* Bool ops *)
     | "&&" { L_AND }
     | "||" { L_OR }
```

</details>

<details open>
<summary><code>src/menhirParser/Lexer.mll</code> · | "pause" {PAUSE}</summary>

<!-- changetour:hunk file=src/menhirParser/Lexer.mll level=2 baseBlob=fffdcd461d55d4d8fe0faf6f2ae260f49eb02b84 -->

```diff
@@ -116,10 +119,7 @@ rule token =
     (* DHExp Annotations *)
     | "()" { UNIT }
     (* Filters *)
-    | "pause" {PAUSE}
     | "debug" {DEBUG}
-    | "hide" {HIDE}
-    | "eval" {EVAL}
     (* Other *)
     | ";" {SEMI_COLON}
     | "test" {TEST}
```

</details>

<details open>
<summary><code>src/menhirParser/Parser.mly</code> · %token PAUSE</summary>

<!-- changetour:hunk file=src/menhirParser/Parser.mly level=2 baseBlob=ecdf9249734ce222f90e9fe0e450914038940514 -->

```diff
@@ -23,10 +23,7 @@ open AST
 %token TYP_AP_SYMBOL
 %token CONS
 %token TEST
-%token PAUSE
 %token DEBUG
-%token HIDE
-%token EVAL
 %token <string> IDENT
 %token <string> CONSTRUCTOR_IDENT
 %token <string> STRING
```

</details>

<details open>
<summary><code>src/menhirParser/Parser.mly</code> · (* filter selector *)</summary>

<!-- changetour:hunk file=src/menhirParser/Parser.mly level=2 baseBlob=ecdf9249734ce222f90e9fe0e450914038940514 -->

```diff
@@ -110,6 +107,9 @@ open AST
 
 %token SEMI_COLON
 
+(* filter selector *)
+%token FILTER_SELECTOR_EXP
+%token FILTER_SELECTOR_VAL
 
 
 (* Precedences *)
```

</details>

<details open>
<summary><code>src/menhirParser/Parser.mly</code> · filterAction:</summary>

<!-- changetour:hunk file=src/menhirParser/Parser.mly level=2 baseBlob=ecdf9249734ce222f90e9fe0e450914038940514 -->

```diff
@@ -314,12 +314,6 @@ funExp:
 %inline ifExp:
     | IF; e1 = exp; THEN; e2 = exp; ELSE; e3 = exp { If (e1, e2, e3) } %prec IF_EXP
 
-filterAction:
-    | PAUSE { Pause }
-    | DEBUG { Debug }
-    | HIDE { Hide }
-    | EVAL { Eval }
-
 tpat:
     | TP_TPAT; s = STRING {InvalidTPat(s)}
     | p = PROJECTOR_INVOKE {InvalidTPat(p)}
```

</details>

<details open>
<summary><code>src/menhirParser/Parser.mly</code> · | a = filterAction; cond = exp; IN; body = exp { Filter(a,…</summary>

<!-- changetour:hunk file=src/menhirParser/Parser.mly level=2 baseBlob=ecdf9249734ce222f90e9fe0e450914038940514 -->

```diff
@@ -369,7 +363,9 @@ exp:
     | FIX;  p = funPat; DASH_ARROW; e = exp { FixF(p, e) }
     | TYP_FUN; t = tpat; DASH_ARROW; e = exp {TypFun(t, e)}
     | QUESTION { EmptyHole }
-    | a = filterAction; cond = exp; IN; body = exp { Filter(a, cond, body)} %prec LET_EXP
+    | DEBUG; cond = exp; IN; body = exp { Filter(cond, body) } %prec LET_EXP
+    | FILTER_SELECTOR_EXP { FilterSelector(Exp) }
+    | FILTER_SELECTOR_VAL { FilterSelector(Val) }
     | TEST; e = exp; END { Test(e) }
     | e1 = exp; AT_SYMBOL; e2 = exp { ListConcat(e1, e2) }
     | e1 = exp; CONS; e2 = exp { Cons(e1, e2) }
```

</details>

<details open>
<summary><code>src/menhirParser/AST.re</code> · | FilterAction(Language.FilterAction.t)</summary>

<!-- changetour:hunk file=src/menhirParser/AST.re level=2 baseBlob=cbba2ceb101656668dd6b9badc523f67823ce889 -->

```diff
@@ -138,6 +162,8 @@ and deferral_pos =
 and exp =
   | Atom(Language.Atom.t)
   | Var(string)
+  | FilterAction(Language.FilterAction.t)
+  | FilterSelector(Language.FilterSelector.t)
   | Constructor(string, option(option(typ)))
   | ListExp(list(exp))
   | TupleExp(list(exp))
```

</details>

<details open>
<summary><code>src/menhirParser/AST.re</code> · | Filter(filter_action, exp, exp)</summary>

<!-- changetour:hunk file=src/menhirParser/AST.re level=2 baseBlob=cbba2ceb101656668dd6b9badc523f67823ce889 -->

```diff
@@ -157,7 +183,7 @@ and exp =
   | FixF(pat, exp)
   | Asc(exp, typ)
   | EmptyHole
-  | Filter(filter_action, exp, exp)
+  | Filter(exp, exp)
   | BuiltinFun(string)
   | Undefined
   | Seq(exp, exp)
```

</details>

The conversion layer does the action-recognition work at parse time: when the filter condition is an application `Var(name)(pat)` and `name` is one of the four action names, the `Var` is upgraded to a `FilterAction` inside the `Unresolved` payload; anything else is kept verbatim for statics to deal with. The legacy `filter_action` AST type keeps its own string mapping for round-tripping, and `of_core` only supports the `Unresolved` direction (resolved filters and residues are evaluator-internal).

<details open>
<summary><code>src/menhirParser/AST.re</code> · string_of_filter_action / filter_action_of_string + filter_selector type</summary>

<!-- changetour:hunk file=src/menhirParser/AST.re level=2 baseBlob=cbba2ceb101656668dd6b9badc523f67823ce889 -->

```diff
@@ -7,6 +7,30 @@ type filter_action =
   | Hide
   | Eval;
 
+let string_of_filter_action = action => {
+  switch (action) {
+  | Pause => "stop"
+  | Debug => "step"
+  | Hide => "hide"
+  | Eval => "eval"
+  };
+};
+
+let filter_action_of_string = string => {
+  switch (string) {
+  | "stop" => Some(Pause)
+  | "step" => Some(Debug)
+  | "hide" => Some(Hide)
+  | "eval" => Some(Eval)
+  | _ => None
+  };
+};
+
+[@deriving (show({with_path: false}), sexp, qcheck, eq)]
+type filter_selector =
+  | Exp
+  | Val;
+
 [@deriving (show({with_path: false}), sexp, qcheck, eq)]
 type op_bin_float =
   | Plus
```

</details>

<details open>
<summary><code>src/menhirParser/Conversion.re</code> · | Filter(cond, body) =&gt; recognize act(pat), build filter_unresolved</summary>

<!-- changetour:hunk file=src/menhirParser/Conversion.re level=2 baseBlob=724cbdde489c993c4a1da7feb5c9d88713c4b15b -->

```diff
@@ -306,17 +308,28 @@ module rec Exp: {
     | Cons(e1, e2) => cons(of_menhir_ast(e1), of_menhir_ast(e2))
     | ListConcat(e1, e2) =>
       list_concat(of_menhir_ast(e1), of_menhir_ast(e2))
-    | Filter(a, cond, body) =>
+    | Filter(cond, body) =>
       let dcond = of_menhir_ast(cond);
       let dbody = of_menhir_ast(body);
-      let act = FilterAction.of_menhir_ast(a);
-      filter(
-        Filter({
-          pat: dcond,
-          act,
-        }),
-        dbody,
-      );
+      switch (dcond.term) {
+      | Ap(Forward, {term: Var(name), annotation}, pat) =>
+        switch (Language.FilterAction.t_of_string(name)) {
+        | Some(act) =>
+          filter_unresolved(
+            ap(
+              Forward,
+              {
+                term: FilterAction(act),
+                annotation,
+              },
+              pat,
+            ),
+            dbody,
+          )
+        | None => filter_unresolved(dcond, dbody)
+        }
+      | _ => filter_unresolved(dcond, dbody)
+      };
     | TypAp(e, ty) => typ_ap(of_menhir_ast(e), Typ.of_menhir_ast(ty))
     | UnOp(op, e) =>
       un_op(Operators.op_un_of_menhir_ast(op), of_menhir_ast(e))
```

</details>

<details open>
<summary><code>src/menhirParser/Conversion.re</code> · | FilterAction(a) =&gt; filter_action(a)</summary>

<!-- changetour:hunk file=src/menhirParser/Conversion.re level=2 baseBlob=724cbdde489c993c4a1da7feb5c9d88713c4b15b -->

```diff
@@ -215,6 +215,8 @@ module rec Exp: {
     | InvalidExp(s) => invalid(s)
     | Atom(c) => basic(c)
     | Var(x) => var(x)
+    | FilterAction(a) => filter_action(a)
+    | FilterSelector(a) => filter_selector(a)
     | Constructor(x, ty) =>
       constructor(x, Option.map(Option.map(Typ.of_menhir_ast), ty))
     | Deferral => deferral(InAp)
```

</details>

<details open>
<summary><code>src/menhirParser/Conversion.re</code> · of_core: FilterAction / FilterSelector passthrough</summary>

<!-- changetour:hunk file=src/menhirParser/Conversion.re level=2 baseBlob=724cbdde489c993c4a1da7feb5c9d88713c4b15b -->

```diff
@@ -348,6 +361,8 @@ module rec Exp: {
     switch (exp.term) {
     | Invalid(_) => InvalidExp("Invalid")
     | Atom(c) => Atom(c)
+    | FilterAction(a) => FilterAction(a)
+    | FilterSelector(a) => FilterSelector(a)
     | Var(x) => Var(x)
     | LivelitName(_) => InvalidExp("Not supported")
     | Deferral(InAp) => Deferral
```

</details>

<details open>
<summary><code>src/menhirParser/Conversion.re</code> · of_core: Filter(Unresolved(e), body); resolved filters unsupported</summary>

<!-- changetour:hunk file=src/menhirParser/Conversion.re level=2 baseBlob=724cbdde489c993c4a1da7feb5c9d88713c4b15b -->

```diff
@@ -382,8 +397,9 @@ module rec Exp: {
     | HintedTest(e, hint) => HintedTest(of_core(e), of_core(hint))
     | Cons(e1, e2) => Cons(of_core(e1), of_core(e2))
     | ListConcat(e1, e2) => ListConcat(of_core(e1), of_core(e2))
-    | Filter(Filter({pat, act}), body) =>
-      Filter(FilterAction.of_core(act), of_core(pat), of_core(body))
+    | Filter(Unresolved(e), body) => Filter(of_core(e), of_core(body))
+    | Filter(Filter(_), _)
+    | Filter(Residue(_), _) => raise(Failure("Residue not supported"))
     | TypAp(e, ty) => TypAp(of_core(e), Typ.of_core(ty))
     | UnOp(op, e) => UnOp(Operators.of_core_op_un(op), of_core(e))
     | DynamicErrorHole(e, s) =>
```

</details>

<details open>
<summary><code>src/menhirParser/Conversion.re</code> · of_core: drop duplicate Residue case</summary>

<!-- changetour:hunk file=src/menhirParser/Conversion.re level=2 baseBlob=724cbdde489c993c4a1da7feb5c9d88713c4b15b -->

```diff
@@ -392,7 +408,6 @@ module rec Exp: {
         Sexplib.Sexp.to_string(Language.InvalidOperationError.sexp_of_t(s)),
       )
     | Deferral(_) => Deferral
-    | Filter(Residue(_), _) => raise(Failure("Residue not supported"))
     | MultiHole([Exp(e)]) => of_core(e) // unwrap single exp multi-holes. just used for label parse failure
     | MultiHole(_) => raise(Failure("MultiHole not supported"))
     | Closure(_) => raise(Failure("Closure not supported"))
```

</details>

## Tile grammar & MakeTerm: the structured editor path

The structured editor parses through its own pipeline — `Form.re` defines the tile forms and `MakeTerm.re` lowers tiles to terms — so it needs the same collapse. The four compound forms become one `Filter` form labeled `debug … in`, `$e`/`$v` become recognized atomic tokens, and `MakeTerm` mirrors the menhir conversion: the filter condition stays `Unresolved`, with an applied `Var` upgraded to `FilterAction` when its name matches one of the four actions.

<details open>
<summary><code>src/haz3lcore/lang/Form.re</code> · | Filter replaces FilterHide/FilterEval/FilterPause/FilterDebug</summary>

<!-- changetour:hunk file=src/haz3lcore/lang/Form.re level=2 baseBlob=d78f37ca99d83475cf94f574dfe32953c9ce906c -->

```diff
@@ -397,10 +397,7 @@ type compound_form =
   | Rule
   | Pipeline
   // DOUBLE DELIMITERS
-  | FilterHide
-  | FilterEval
-  | FilterPause
-  | FilterDebug
+  | Filter
   | Use
   // Drv
   | Drv(drv_compound_form)
```

</details>

<details open>
<summary><code>src/haz3lcore/lang/Form.re</code> · | Filter =&gt; mk_pre_c(L, ["debug", "in"], …)</summary>

<!-- changetour:hunk file=src/haz3lcore/lang/Form.re level=2 baseBlob=d78f37ca99d83475cf94f574dfe32953c9ce906c -->

```diff
@@ -505,10 +502,7 @@ let get: compound_form => t =
     mk(L, ["|", "=>"], Mold.mk_bin'(P.rule_sep, Rul, Exp, [Pat], Exp))
   | Pipeline => mk_infix("|>", Exp, P.eqs) // in OCaml, pipeline precedence is in same class as '=', '<', etc.
   // DOUBLE DELIMITERS
-  | FilterHide => mk_pre_c(L, ["hide", "in"], P.let_, Exp, [Exp])
-  | FilterEval => mk_pre_c(L, ["eval", "in"], P.let_, Exp, [Exp])
-  | FilterPause => mk_pre_c(L, ["pause", "in"], P.let_, Exp, [Exp])
-  | FilterDebug => mk_pre_c(L, ["debug", "in"], P.let_, Exp, [Exp])
+  | Filter => mk_pre_c(L, ["debug", "in"], P.let_, Exp, [Exp])
   | Use => mk_pre_c(L, ["use", "in"], P.let_, Exp, [Typ])
   // Drv
   | Drv(drv_compound_form) => drv_get(drv_compound_form)
```

</details>

<details open>
<summary><code>src/haz3lcore/lang/MakeTerm.re</code> · (["$e"], []) =&gt; ret(FilterSelector(Exp))</summary>

<!-- changetour:hunk file=src/haz3lcore/lang/MakeTerm.re level=2 baseBlob=0a404099dad6fa14179c256846e2940514144c2f -->

```diff
@@ -658,6 +658,8 @@ and exp_term: unsorted => (Exp.term, list(Id.t)) = {
         ret(Atom(Float(float_of_string(t))))
       | ([t], []) when Token.is_livelit(t) =>
         ret(LivelitName(Token.parse_livelit(t)))
+      | (["$e"], []) => ret(FilterSelector(Exp))
+      | (["$v"], []) => ret(FilterSelector(Val))
       | ([t], []) when Token.is_var(t) => ret(Var(t))
       | ([t], []) when Token.is_ctr(t) => ret(Constructor(t, None))
       | (["{", "}"], [Mod(body)]) =>
```

</details>

<details open>
<summary><code>src/haz3lcore/lang/MakeTerm.re</code> · (["debug", "in"], [Exp(filter)]) =&gt; Filter(Unresolved(…), r)</summary>

<!-- changetour:hunk file=src/haz3lcore/lang/MakeTerm.re level=2 baseBlob=0a404099dad6fa14179c256846e2940514144c2f -->

```diff
@@ -760,38 +762,30 @@ and exp_term: unsorted => (Exp.term, list(Id.t)) = {
           ModuleExp(mp, def, r)
         | (["theorem", "=", "in"], [Pat(pat), Exp(thm)]) =>
           Theorem(pat, thm, r)
-        | (["hide", "in"], [Exp(filter)]) =>
-          Filter(
-            Filter({
-              act: (Eval, One),
-              pat: filter,
-            }),
-            r,
-          )
-        | (["eval", "in"], [Exp(filter)]) =>
-          Filter(
-            Filter({
-              act: (Eval, All),
-              pat: filter,
-            }),
-            r,
-          )
-        | (["pause", "in"], [Exp(filter)]) =>
-          Filter(
-            Filter({
-              act: (Step, One),
-              pat: filter,
-            }),
-            r,
-          )
         | (["debug", "in"], [Exp(filter)]) =>
-          Filter(
-            Filter({
-              act: (Step, All),
-              pat: filter,
-            }),
-            r,
-          )
+          switch (filter.term) {
+          | Ap(Forward, {term: Var(name), annotation}, pat) =>
+            switch (FilterAction.t_of_string(name)) {
+            | Some(act) =>
+              Filter(
+                Unresolved({
+                  ...filter,
+                  term:
+                    Ap(
+                      Forward,
+                      {
+                        term: FilterAction(act),
+                        annotation,
+                      },
+                      pat,
+                    ),
+                }),
+                r,
+              )
+            | None => Filter(Unresolved(filter), r)
+            }
+          | _ => Filter(Unresolved(filter), r)
+          }
         | (["use", "in"], [Typ(ty)]) => Use(ty, r)
         | (["type", "=", "in"], [TPat(tpat), Typ(def)]) =>
           TyAlias(tpat, def, r)
```

</details>

## Pretty-printing back to surface syntax

Printing is the inverse of the parse-time resolution. A resolved `Filter({pat, act, ids})` is printed by reconstructing the `act(pat)` application — reusing the stored `ids` as the application's annotation so tile ids stay stable across round-trips — while an `Unresolved` filter prints its expression verbatim. Both render under the single `Form.Filter` (`debug … in`) form, and the new leaf variants print as their bare tokens.

<details open>
<summary><code>src/haz3lcore/pretty/ExpToSegment.re</code> · parenthesize: Filter(Unresolved(…)) and ids threading</summary>

<!-- changetour:hunk file=src/haz3lcore/pretty/ExpToSegment.re level=2 baseBlob=25767daf953032cfbcb06a72a389c805b5b7768a -->

```diff
@@ -356,11 +360,18 @@ let rec parenthesize =
   // Forms that currently need to stripped before outputting
   | Closure(_, x)
   | DynamicErrorHole(x, _) => parenthesize(x)
-  | Filter(Filter({pat, act}), x) =>
+  | Filter(Unresolved(exp), x) =>
+    Filter(
+      Unresolved(parenthesize(exp) |> paren_at(Precedence.min)),
+      parenthesize(x) |> paren_at(Precedence.let_),
+    )
+    |> rewrap
+  | Filter(Filter({pat, act, ids}), x) =>
     Filter(
       Filter({
         pat: parenthesize(pat),
         act,
+        ids,
       }),
       parenthesize(x),
     )
```

</details>

<details open>
<summary><code>src/haz3lcore/pretty/ExpToSegment.re</code> · exp_to_pretty: rebuild act(pat) Ap, print via Form.Filter</summary>

<!-- changetour:hunk file=src/haz3lcore/pretty/ExpToSegment.re level=2 baseBlob=25767daf953032cfbcb06a72a389c805b5b7768a -->

```diff
@@ -1705,22 +1716,37 @@ let rec exp_to_pretty = (~settings: Settings.t, exp: Exp.t): pretty => {
   // Assume these have been removed by the parenthesizer
   | DynamicErrorHole(_)
   | Filter(Residue(_), _) => failwith("printing these not implemented yet")
-  | Filter(Filter({pat, act}), e) =>
+  | Filter(Unresolved(filt_exp), e) =>
+    let id = exp |> Exp.rep_id;
+    let* p = go(filt_exp);
+    let+ e = go(e);
+    wrap(
+      exp,
+      settings.show_filters
+        ? {
+          [mk_form(Form.Filter, id, [p])] @ e;
+        }
+        : e,
+    );
+  | Filter(Filter({pat, act, ids}), e) =>
     let id = exp |> Exp.rep_id;
-    let* p = go(pat);
+    let filter =
+      Ap(
+        Forward,
+        {
+          term: FilterAction(act),
+          annotation: ids,
+        },
+        pat,
+      )
+      |> Exp.fresh;
+    let* p = go(filter);
     let+ e = go(e);
     wrap(
       exp,
       settings.show_filters
         ? {
-          let form =
-            switch (act) {
-            | (Step, One) => Form.FilterPause
-            | (Step, All) => Form.FilterDebug
-            | (Eval, One) => Form.FilterHide
-            | (Eval, All) => Form.FilterEval
-            };
-          [mk_form(form, id, [p])] @ e;
+          [mk_form(Form.Filter, id, [p])] @ e;
         }
         : e,
     );
```

</details>

<details open>
<summary><code>src/haz3lcore/pretty/ExpToSegment.re</code> · FilterAction / FilterSelector print as bare tokens</summary>

<!-- changetour:hunk file=src/haz3lcore/pretty/ExpToSegment.re level=2 baseBlob=25767daf953032cfbcb06a72a389c805b5b7768a -->

```diff
@@ -1761,6 +1787,22 @@ let rec exp_to_pretty = (~settings: Settings.t, exp: Exp.t): pretty => {
       | TPat => OfAlfaTPat
       };
     [mk_form(Drv(form), exp |> Exp.rep_id, [d])];
+  | FilterAction(act) =>
+    wrap(
+      exp,
+      text_to_pretty(
+        exp |> Exp.rep_id,
+        Sort.Exp,
+        FilterAction.string_of_t(act),
+      ),
+    )
+  | FilterSelector(sel) =>
+    let token =
+      switch (sel) {
+      | Exp => "$e"
+      | Val => "$v"
+      };
+    wrap(exp, text_to_pretty(exp |> Exp.rep_id, Sort.Exp, token));
   // TODO: Make sure types are correct
   | Constructor(c, _t) =>
     // let id = Id.mk();
```

</details>

## Folded fixpoint labels hide the recursive marker

A small stepper-display fix that rode along: folded function labels for fixpoints carried the internal `+` recursion marker (e.g. `<fac+>`). When the `hide_fixpoints` setting is on, the marker is now stripped from the fold label, in both the `Parens(FixF…)` and bare `FixF` printing paths, with a test pinning the rendered label.

<details open>
<summary><code>src/haz3lcore/pretty/ExpToSegment.re</code> · strip trailing "+" from folded Parens(FixF) label</summary>

<!-- changetour:hunk file=src/haz3lcore/pretty/ExpToSegment.re level=2 baseBlob=25767daf953032cfbcb06a72a389c805b5b7768a -->

```diff
@@ -1892,11 +1934,18 @@ let rec exp_to_pretty = (~settings: Settings.t, exp: Exp.t): pretty => {
       |> fold_fun_if(settings.fold_fn_bodies, name, _, inner_exp),
     );
   | Parens({term: FixF(p, e, _), _} as inner_exp) =>
+    // TODO: Add optional newlines
     let id = inner_exp |> Exp.rep_id;
     let+ p = pat_to_pretty(~settings: Settings.t, p)
     and+ e = go(e);
+    let name = Exp.get_fn_name(inner_exp) |> Option.value(~default="fun");
     let name =
-      "<" ++ (Exp.get_fn_name(exp) |> Option.value(~default="fun")) ++ ">";
+      if (settings.hide_fixpoints && String.ends_with(~suffix="+", name)) {
+        String.sub(name, 0, String.length(name) - 1);
+      } else {
+        name;
+      };
+    let name = "<" ++ name ++ ">";
     let fix_form = [mk_form(Fix, id, [p])] @ e;
     wrap(
       exp,
```

</details>

<details open>
<summary><code>src/haz3lcore/pretty/ExpToSegment.re</code> · strip trailing "+" from folded FixF label</summary>

<!-- changetour:hunk file=src/haz3lcore/pretty/ExpToSegment.re level=2 baseBlob=25767daf953032cfbcb06a72a389c805b5b7768a -->

```diff
@@ -2070,8 +2119,14 @@ let rec exp_to_pretty = (~settings: Settings.t, exp: Exp.t): pretty => {
     let id = exp |> Exp.rep_id;
     let+ p = pat_to_pretty(~settings: Settings.t, p)
     and+ e = go(e);
+    let name = Exp.get_fn_name(exp) |> Option.value(~default="fun");
     let name =
-      "<" ++ (Exp.get_fn_name(exp) |> Option.value(~default="fun")) ++ ">";
+      if (settings.hide_fixpoints && String.ends_with(~suffix="+", name)) {
+        String.sub(name, 0, String.length(name) - 1);
+      } else {
+        name;
+      };
+    let name = "<" ++ name ++ ">";
     wrap(
       exp,
       [mk_form(Fix, id, [p])]
```

</details>

<details open>
<summary><code>test/Test_ExpToSegment.re</code> · test: Folded FixF hides recursive marker</summary>

<!-- changetour:hunk file=test/Test_ExpToSegment.re level=2 baseBlob=772a770bcf2fb177151f9a27cd8666c4566a8e81 -->

```diff
@@ -137,6 +137,45 @@ let tests = (
         );
       },
     ),
+    test_case(
+      "Folded FixF hides recursive marker",
+      `Quick,
+      () => {
+        open IdTagged.FreshGrammar;
+        let settings = {
+          ...exp_to_segment_settings,
+          fold_fn_bodies: `Fold,
+          hide_fixpoints: true,
+        };
+        let exp =
+          Exp.(
+            fix_f(
+              Pat.var("fac"),
+              fn(Pat.var("n"), var("n"), None, Some("fac+")),
+              None,
+            )
+          );
+        let expected_model =
+          FoldProj.sexp_of_t({
+            text: "<fac>",
+            expanded: false,
+            always_render: true,
+          })
+          |> Sexplib.Sexp.to_string;
+        let seg = ExpToSegment.exp_to_segment(~settings, exp);
+        switch (seg) {
+        | [Projector({kind: Fold, model, _})] =>
+          check(string, "fold label", expected_model, model);
+        | _ => Alcotest.fail("expected folded FixF projector")
+        };
+        let seg = ExpToSegment.exp_to_segment(~settings, Exp.parens(exp));
+        switch (seg) {
+        | [Projector({kind: Fold, model, _})] =>
+          check(string, "parenthesized fold label", expected_model, model)
+        | _ => Alcotest.fail("expected folded Parens(FixF) projector")
+        };
+      },
+    ),
     test_case(
       "Tuple",
       `Quick,
```

</details>

## Dynamics: CompleteFilter fix and user settings override

We here first check if the eval object is already CompleteFilter, and just skip over it; otherwise we will get more and more residue and can never return. Also here we adjust the guard expression, so that user settings will be properly respected in the stepper.

<details open>
<summary><code>src/language/dynamics/stepper/EvaluatorStep.re</code> · (~settings, x: EvalObj.t): (FilterAction.action, EvalObj.t)…</summary>

<!-- changetour:hunk file=src/language/dynamics/stepper/EvaluatorStep.re level=2 baseBlob=2f4f24523d3455e3ab329319d338a17846bb5c51 -->

```diff
@@ -213,29 +216,34 @@ let rec matches =
 };
 
 let should_hide_eval_obj =
-    (~settings, x: EvalObj.t): (FilterAction.action, EvalObj.t) =>
-  if (should_hide_step_kind(~settings, x.knd)) {
-    (Eval, x);
-  } else {
-    let (act, _, ctx) =
+    (~settings, x: EvalObj.t): (FilterAction.action, EvalObj.t) => {
+  switch (x.knd) {
+  | CompleteFilter =>
+    /* CompleteFilter removes internal filter bookkeeping. Running filter
+       matching on this step can re-introduce a Residue wrapper around the
+       expression it just unwrapped, causing an infinite sequence of hidden
+       CompleteFilter steps. */
+    (Eval, x)
+  | _ =>
+    let (act, idx, ctx) =
       matches(Environment.empty, [], x.ctx, x.d_loc, (Step, One), 0);
+    let x = {
+      ...x,
+      ctx,
+    };
     switch (act) {
-    | (Eval, _) => (
+    | (Eval, _) => (Eval, x)
+    // Skip over Ascription
+    | (Step, _) when step_kind_is_unrenderable(~settings, x.knd) => (
         Eval,
-        {
-          ...x,
-          ctx,
-        },
-      )
-    | (Step, _) => (
-        Step,
-        {
-          ...x,
-          ctx,
-        },
+        x,
       )
+    | (Step, _) when idx > 0 => (Step, x)
+    | (Step, _) when should_hide_step_kind(~settings, x.knd) => (Eval, x)
+    | (Step, _) => (Step, x)
     };
   };
+};
 
 module Decompose = {
   module Result = {
```

</details>

The reordered guards rely on a new predicate, `step_kind_is_unrenderable`: ascription steps have no surface piece to draw on when `show_ascriptions` is off, so they are force-evaluated even when a user filter says `Step` — otherwise the stepper UI would show pauses with zero clickable boxes. Filter matching itself also learns to walk through `Filter(Unresolved(…))` frames; an unresolved filter contributes nothing to the filter environment.

<details open>
<summary><code>src/language/dynamics/transition/Transition.re</code> · let step_kind_is_unrenderable = (~settings, kind)</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/Transition.re level=2 baseBlob=3f0ab4db396e6e7e09b57108d246279919f4de20 -->

```diff
@@ -1255,6 +1257,15 @@ module Transition = (EV: EV_MODE) => {
   };
 };
 
+let step_kind_is_unrenderable =
+    (~settings: CoreSettings.Evaluation.t, kind: step_kind) =>
+  switch (kind) {
+  | Ascription
+  | AscriptionAp
+  | AscriptionTypAp => !settings.show_ascriptions
+  | _ => false
+  };
+
 let should_hide_step_kind = (~settings: CoreSettings.Evaluation.t) =>
   fun
   | LetBind(_)
```

</details>

<details open>
<summary><code>src/language/dynamics/stepper/EvaluatorStep.re</code> · matches: walk through Filter(Unresolved(…)) frames</summary>

<!-- changetour:hunk file=src/language/dynamics/stepper/EvaluatorStep.re level=2 baseBlob=2f4f24523d3455e3ab329319d338a17846bb5c51 -->

```diff
@@ -72,6 +72,9 @@ let rec matches =
       | Closure(env, ctx) =>
         let+ ctx = matches(env, flt, ctx, exp, act, idx);
         Closure(env, ctx) |> rewrap;
+      | Filter(Unresolved(exp), ctx) =>
+        let+ ctx = matches(env, flt, ctx, exp, act, idx);
+        Filter(Unresolved(exp), ctx) |> rewrap;
       | Filter(Filter(flt'), ctx) =>
         let flt = flt |> FilterEnvironment.extends(flt');
         let+ ctx = matches(env, flt, ctx, exp, act, idx);
```

</details>

Relatedly, `refresh_step` (which re-locates a persisted step in a re-decomposed expression) depends on `ProofHacks`'s occurrence search. That search previously used an equality that treated filters as transparent, so a `Filter(Residue(…))` bookkeeping wrapper compared equal to the expression underneath it and the search stopped at the wrapper instead of the real target. The equality is now configured with `ignore_filters: false`, and `refresh_step` is simplified to match.

<details open>
<summary><code>src/language/proof/ProofHacks.re</code> · structurally_equal: ignore_filters: false</summary>

<!-- changetour:hunk file=src/language/proof/ProofHacks.re level=2 baseBlob=a77134f0ee5a3f306ec78d931b03d39e2b443019 -->

```diff
@@ -6,6 +6,23 @@ exception Found(Exp.t);
 // Find a subexpression by id (delegates to Exp.find_by_id)
 let find_exp_id = Exp.find_by_id;
 
+// Equality used to compare candidate occurrences against the target. We keep
+// every tolerance of `Equality.ignoring_ascriptions` (alpha-equivalence,
+// parens, ascriptions, function names, hole provenance, …) so the proof/axiom
+// call sites keep locating user-selected subexpressions as before, and flip
+// only `ignore_filters`. `ignore_filters` is precisely what made the
+// `Filter(Residue(...))` bookkeeping wrapper compare equal to the expression
+// underneath it: the search matched the wrapper and stopped before reaching the
+// real target id. With filters no longer transparent, nested same-kind wrappers
+// (`eval ... in pause ... in ...`) likewise stop matching spuriously.
+let structurally_equal: (Exp.t, Exp.t) => bool =
+  Equality.equality({
+    ...Equality.semantic_settings,
+    ignore_ascriptions: true,
+    ignore_filters: false,
+  }).
+    exp;
+
 // Given an expression e1 that appears in e2, count how many
 // times e1 appears with a different id before e1 in e2.
 let exp_idx = (e1: Exp.t, e2: Exp.t) => {
```

</details>

<details open>
<summary><code>src/language/proof/ProofHacks.re</code> · exp_idx uses structurally_equal</summary>

<!-- changetour:hunk file=src/language/proof/ProofHacks.re level=2 baseBlob=a77134f0ee5a3f306ec78d931b03d39e2b443019 -->

```diff
@@ -16,7 +33,7 @@ let exp_idx = (e1: Exp.t, e2: Exp.t) => {
         (cont, exp) =>
           if (Exp.rep_id(exp) == Exp.rep_id(e1)) {
             raise(Found(exp));
-          } else if (Equality.ignoring_ascriptions.exp(exp, e1)) {
+          } else if (structurally_equal(exp, e1)) {
             n := n^ + 1;
             exp;
           } else {
```

</details>

<details open>
<summary><code>src/language/proof/ProofHacks.re</code> · nth_exp uses structurally_equal</summary>

<!-- changetour:hunk file=src/language/proof/ProofHacks.re level=2 baseBlob=a77134f0ee5a3f306ec78d931b03d39e2b443019 -->

```diff
@@ -42,7 +59,7 @@ let nth_exp = (e1: Exp.t, n: int, e2: Exp.t) => {
     Exp.map_term(
       ~f_exp=
         (cont, exp) =>
-          if (Equality.ignoring_ascriptions.exp(exp, e1)) {
+          if (structurally_equal(exp, e1)) {
             if (count^ == n) {
               raise(Found(exp));
             } else {
```

</details>

<details open>
<summary><code>src/language/dynamics/stepper/EvaluatorStep.re</code> · refresh_step: simplified target lookup</summary>

<!-- changetour:hunk file=src/language/dynamics/stepper/EvaluatorStep.re level=2 baseBlob=2f4f24523d3455e3ab329319d338a17846bb5c51 -->

```diff
@@ -450,13 +458,10 @@ let refresh_step =
   let eos =
     decompose(exp, env)
     |> List.map(should_hide_eval_obj(~settings=settings.evaluation)); // NOTE: should_hide_eval_obj actually changes the eval obj to do filter bookkeeping!!!
-  let* desired_id =
-    ProofHacks.nth_exp(step.at_exp, step.exp_idx, exp)
-    |> Option.map(IdTagged.ids);
-  let* (h, x) =
-    List.find_opt(
-      ((_, step': step)) => IdTagged.ids(step'.d_loc) == desired_id,
-      eos,
-    );
-  Some((h, x));
+  let* found = ProofHacks.nth_exp(step.at_exp, step.exp_idx, exp);
+  let desired_id = IdTagged.ids(found);
+  List.find_opt(
+    ((_, step': step)) => IdTagged.ids(step'.d_loc) == desired_id,
+    eos,
+  );
 };
```

</details>

## ExplainThis & documentation

The ExplainThis data is rewritten around the unified form: a single `FilterExp` group with four forms (`eval`/`hide`/`step`/`stop`) replaces the four per-keyword groups, and `FilterAction` and `FilterSelector` get their own doc entries with worked examples in the new syntax. The doc lookup handles both resolved filters and the parsed `Unresolved` shape, coloring the action, pattern, and body separately via the new `ids` annotation.

<details open>
<summary><code>src/web/app/explainthis/ExplainThisForm.re</code> · filter_action_examples / filter_selector_examples</summary>

<!-- changetour:hunk file=src/web/app/explainthis/ExplainThisForm.re level=2 baseBlob=031a7d1f72d95c08cec101ed085947febc8766a6 -->

```diff
@@ -61,6 +61,18 @@ type let_examples =
   | Ctr
   | Ap;
 
+[@deriving (show({with_path: false}), sexp, yojson)]
+type filter_action_examples =
+  | Step
+  | Eval
+  | Hide
+  | Stop;
+
+[@deriving (show({with_path: false}), sexp, yojson)]
+type filter_selector_examples =
+  | Exp
+  | Val;
+
 [@deriving (show({with_path: false}), sexp, yojson)]
 type numeric_bin_op_examples =
   | Plus
```

</details>

<details open>
<summary><code>src/web/app/explainthis/ExplainThisForm.re</code> · example_id: Filter(filter_action_examples)</summary>

<!-- changetour:hunk file=src/web/app/explainthis/ExplainThisForm.re level=2 baseBlob=031a7d1f72d95c08cec101ed085947febc8766a6 -->

```diff
@@ -140,11 +152,8 @@ type example_id =
   | CaseBool
   | VoidAbsurd
   | Pipeline1
-  | FilterStep
-  | FilterEval
-  | FilterHide
-  | FilterDebug
-  | FilterSelector
+  | Filter(filter_action_examples)
+  | FilterSelector(filter_selector_examples)
   | Undefined1
   | Undefined2
   | Asc1
```

</details>

<details open>
<summary><code>src/web/app/explainthis/ExplainThisForm.re</code> · form_id: FilterExp(Language.FilterAction.t)</summary>

<!-- changetour:hunk file=src/web/app/explainthis/ExplainThisForm.re level=2 baseBlob=031a7d1f72d95c08cec101ed085947febc8766a6 -->

```diff
@@ -287,11 +296,9 @@ type form_id =
   | MultiHoleTPat
   | VarTPat
   | PipelineExp
-  | FilterPause
-  | FilterEval
-  | FilterDebug
-  | FilterHide
-  | FilterSelector
+  | FilterExp(Language.FilterAction.t)
+  | FilterAction
+  | FilterSelector(Language.FilterSelector.t)
   | AscExp
   | TupleExtensionExp
   | ModuleExp
```

</details>

<details open>
<summary><code>src/web/app/explainthis/ExplainThisForm.re</code> · group_id: FilterExp | FilterAction | FilterSelector</summary>

<!-- changetour:hunk file=src/web/app/explainthis/ExplainThisForm.re level=2 baseBlob=031a7d1f72d95c08cec101ed085947febc8766a6 -->

```diff
@@ -414,10 +421,8 @@ type group_id =
   | EmptyHoleTPat
   | MultiHoleTPat
   | VarTPat
-  | FilterPause
-  | FilterEval
-  | FilterDebug
-  | FilterHide
+  | FilterExp
+  | FilterAction
   | FilterSelector
   | ModuleExp
   | ModLetDecl
```

</details>

<details>
<summary><code>src/web/app/explainthis/data/FilterExp.re</code> · rewritten filter docs: one group, four action forms, selector docs</summary>

<!-- changetour:hunk file=src/web/app/explainthis/data/FilterExp.re level=2 baseBlob=2d273f3a020b04664ba8b2e75963eb35daa0427e -->

```diff
@@ -2,96 +2,247 @@ open Haz3lcore;
 open ExplainThisForm;
 open Example;
 
-let filter_pause = (~p_id: Id.t, ~body_id: Id.t): Simple.t => {
-  group_id: FilterPause,
-  form_id: FilterPause,
-  abstract:
-    Simple.mk_2(("p", p_id), ("e_body", body_id), (p', e_body') =>
-      [mk_pause([[space(), p', space()]]), linebreak(), e_body']
+let filter_hide_example = {
+  sub_id: Filter(Hide),
+  term:
+    mk_example(
+      {|# skip-over eval of all expressions #
+debug eval($e) in
+let fib : Int -> Int = fun n ->
+  case n
+    | 0 => 1
+    | 1 => 2
+    | n => fib(n - 1) + fib(n - 2)
+  end
+in
+# stop at application of fib function to a value #
+debug stop(fib($v)) in
+ # skip-over the evaluation of fib(2) #
+debug hide(fib(2)) in
+fib(3)|},
     ),
-  explanation:
-    Printf.sprintf(
-      "Pause filter for stepper. The evaluation of all subexpressions within [*body*](%s) that match the [*pattern*](%s) will be paused during evaluation",
-      body_id |> Id.to_string,
-      p_id |> Id.to_string,
-    ),
-  examples: [
-    {
-      sub_id: FilterStep,
-      term: mk_example("eval $e + $e in\n(1 + 2) * (3 + 4)"),
-      message: "The expression (1 * 2) + (3 * 4) is guarded by a pause filter expression pause $v + $v, which instruct the evaluator to pause the evaluation when it sees a value is added to another value. After evaluating subterms (1 * 2) and (3 * 4), the expression turns into 2 + 12. 2 matches the first $v pattern, and 12 matches the second $v pattern. Therefore, the evaluator stops when the expression steps to 2 + 12",
-    },
-  ],
-};
-
-let filter_eval = (~p_id: Id.t, ~body_id: Id.t): Simple.t => {
-  group_id: FilterEval,
-  form_id: FilterEval,
-  abstract:
-    Simple.mk_2(("p", p_id), ("e_body", body_id), (p', e_body') =>
-      [mk_eval([[space(), p', space()]]), linebreak(), e_body']
-    ),
-  explanation:
-    Printf.sprintf(
-      "Full evaluation filter for stepper. All subexpressions within [*body*](%s) that match the [*pattern*](%s) will get evaluated in one go",
-      body_id |> Id.to_string,
-      p_id |> Id.to_string,
-    ),
-  examples: [
-    {
-      sub_id: FilterEval,
-      term:
-        mk_example(
-          "pause $e in\nhide let = in in\nlet x = 1 in\nlet y = 2 in\nx + y",
-        ),
-      message: "pause $e in instruct the evaluator to act like a single-stepper, e.g. stop at every step. The hide filter expression instructs the evaluator to skip over all evaluator steps that destructs perform substitution on a let-expression. Here, the substitution of variable x and y is skipped over and we directly got 1 + 2 in the result area.",
-    },
-  ],
-};
-
-let filter_hide = (~p_id: Id.t, ~body_id: Id.t): Simple.t => {
-  group_id: FilterHide,
-  form_id: FilterHide,
-  abstract:
-    Simple.mk_2(("p", p_id), ("e_body", body_id), (p', e_body') =>
-      [mk_hide([[space(), p', space()]]), linebreak(), e_body']
-    ),
-  explanation:
-    Printf.sprintf(
-      "Step hiding filter for stepper. The elimination of all language constructs (like binary operator + or let .. = .. in) within [*body*](%s) that match the [*pattern*](%s) will get skipped.",
-      body_id |> Id.to_string,
-      p_id |> Id.to_string,
+  message: "Here `hide` means we want to hide the evaluation of such expression, but not skip them entirely. This action is especially useful if you want to un-stop certain expressions.",
+};
+
+let filter_eval_example = {
+  sub_id: Filter(Eval),
+  term: mk_example("debug eval($e) in\n1 + 2 + 3 + 4"),
+  message: "Here `eval` means we want to skip the evaluation of all expression.",
+};
+
+let filter_stop_example = {
+  sub_id: Filter(Stop),
+  term:
+    mk_example(
+      {|# skip-over eval of all expressions #
+debug eval($e) in
+let fib : Int -> Int = fun n ->
+  case n
+    | 0 => 1
+    | 1 => 2
+    | n => fib(n - 1) + fib(n - 2)
+  end
+in
+ # stop at application of fib function to a value #
+debug stop(fib($v)) in
+fib(3)|},
     ),
-  examples: [
-    {
-      sub_id: FilterHide,
-      term:
-        mk_example(
-          "pause $e in\nhide let = in in\nlet x = 1 in\nlet y = 2 in\nx + y",
-        ),
-      message: "pause $e in instruct the evaluator to act like a single-stepper, e.g. stop at every step. The hide filter expression instructs the evaluator to skip over all evaluator steps that destructs perform substitution on a let-expression. Here, the substitution of variable x and y is skipped over and we directly got 1 + 2 in the result area.",
-    },
-  ],
-};
-
-let filter_debug = (~p_id: Id.t, ~body_id: Id.t): Simple.t => {
-  group_id: FilterDebug,
-  form_id: FilterDebug,
-  abstract:
-    Simple.mk_2(("p", p_id), ("e_body", body_id), (p', e_body') =>
-      [mk_debug([[space(), p', space()]]), linebreak(), e_body']
+  message: "Here `stop` means we want to stop at the evaluation of such expression, and resume immediately.",
+};
+
+let filter_step_example = {
+  sub_id: Filter(Step),
+  term:
+    mk_example(
+      {|# skip-over eval of all expressions #
+debug eval($e) in
+let fib : Int -> Int = fun n ->
+  case n
+    | 0 => 1
+    | 1 => 2
+    | n => fib(n - 1) + fib(n - 2)
+  end
+in
+# stop at application of fib function, and resume after evaluation of current expression. #
+debug step(fib(2)) in
+fib(3)|},
     ),
-  explanation:
-    Printf.sprintf(
-      "Debug filter for stepper. All matched sub-expression within [*body*](%s) that match the [*pattern*](%s) will be stepped through.",
-      body_id |> Id.to_string,
-      p_id |> Id.to_string,
+  message: "Here `step` means we want to step through the evaluation of such expression. Once such expression finish evaluating, the stepper will resume to the stepping behavior it used to have.",
+};
+
+let _pat = exp("pat");
+
+let _act = exp("act");
+
+let _body = exp("e_body");
+
+let filter_hide_exp: form = {
+  let _hide = exp("hide");
+  let explanation = "The stepper will [*skip-over/hide*](%s) the first step of evaluation of any expression that matches the [*pattern*](%s) inside [*body*](%s).";
+  let form = [
+    mk_filter([[space(), _hide, mk_ap_exp([[_pat]]), space()]]),
+    linebreak(),
+    _body,
+  ];
+  {
+    id: FilterExp((Eval, One)),
+    syntactic_form: form,
+    expandable_id: Some((Piece.id(_hide), [_hide])),
+    explanation,
+    examples: [filter_hide_example],
+  };
+};
+
+let filter_action_exp = (act: Language.FilterAction.t): form => {
+  id: FilterAction,
+  syntactic_form: [Language.FilterAction.string_of_t(act) |> exp],
+  expandable_id: None,
+  explanation: "Filter action, can be one of `eval`, `hide`, `step`, or `stop`.",
+  examples: [],
+};
+
+let filter_action_exps = (act: Language.FilterAction.t): group => {
+  {
+    id: FilterAction,
+    forms: [filter_action_exp(act)],
+  };
+};
+
+let filter_eval_exp: form = {
+  let _eval = exp("eval");
+  let explanation = "The stepper will [*skip-over/eval*](%s) the evaluation of any expression that matches the [*pattern*](%s) inside [*body*](%s).";
+  let form = [
+    mk_filter([[space(), _eval, mk_ap_exp([[_pat]]), space()]]),
+    linebreak(),
+    _body,
+  ];
+  {
+    id: FilterExp((Eval, All)),
+    syntactic_form: form,
+    expandable_id: Some((Piece.id(_eval), [_eval])),
+    explanation,
+    examples: [filter_eval_example],
+  };
+};
+
+let filter_stop_exp: form = {
+  let _stop = exp("stop");
+  let explanation = "The stepper will [*stop*](%s) at any expression that matches the [*pattern*](%s) inside [*body*](%s), and will resume immediately.";
+  let form = [
+    mk_filter([[space(), _stop, mk_ap_exp([[_pat]]), space()]]),
+    linebreak(),
+    _body,
+  ];
+  {
+    id: FilterExp((Step, One)),
+    syntactic_form: form,
+    expandable_id: Some((Piece.id(_stop), [_stop])),
+    explanation,
+    examples: [filter_stop_example],
+  };
+};
+
+let filter_step_exp: form = {
+  let _step = exp("step");
+  let explanation = "The stepper will [*step-through*](%s) at any expression that matches the [*pattern*](%s) inside [*body*](%s), and will resume stepping after evaluating that expression.";
+  let form = [
+    mk_filter([[space(), _step, mk_ap_exp([[_pat]]), space()]]),
+    linebreak(),
+    _body,
+  ];
+  {
+    id: FilterExp((Step, All)),
+    syntactic_form: form,
+    expandable_id: Some((Piece.id(_step), [_step])),
+    explanation,
+    examples: [filter_step_example],
+  };
+};
+
+let _filter_exp_coloring_ids =
+    (
+      sf_act_id: Id.t,
+      sf_pat_id: Id.t,
+      sf_body_id: Id.t,
+      ~act_id: Id.t,
+      ~pat_id: Id.t,
+      ~body_id: Id.t,
+    )
+    : list((Id.t, Id.t)) => {
+  [(sf_act_id, act_id), (sf_pat_id, pat_id), (sf_body_id, body_id)];
+};
+
+let filter_exp_coloring_ids =
+  _filter_exp_coloring_ids(
+    Piece.id(_act),
+    Piece.id(_pat),
+    Piece.id(_body),
+  );
+
+let filter_exp: group = {
+  {
+    id: FilterExp,
+    forms: [
+      filter_eval_exp,
+      filter_hide_exp,
+      filter_step_exp,
+      filter_stop_exp,
+    ],
+  };
+};
+
+let filter_selector_exp_example = {
+  sub_id: FilterSelector(Exp),
+  term: mk_example("debug eval($e) in\n1 + 2 + 3 + 4"),
+  message: "Here `$e` matches any expression when applying `eval` filter to an expression, which is `1 + 2` in this case, as it is the immediately next expression to be evaluated.",
+};
+
+let filter_selector_exp_exp: form = {
+  let _e = exp("$e");
+  {
+    id: FilterSelector(Exp),
+    syntactic_form: [_e],
+    expandable_id: Some((Piece.id(_e), [_e])),
+    explanation: "Matches expression, i.e. anything when apply filters to an expression.",
+    examples: [filter_selector_exp_example],
+  };
+};
+
+let filter_selector_val_example = {
+  sub_id: FilterSelector(Val),
+  term:
+    mk_example(
+      "debug eval($e) in\ndebug step($v + $v) in\n(1 + 2) * (3 + 4)",
     ),
-  examples: [
-    {
-      sub_id: FilterDebug,
-      term: mk_example("eval $e in\ndebug $v + $v + $v in\n1 + 2 + 3"),
-      message: "The debug filter pattern $v + $v + $v matches 1 + 2 + 3, therefore, the evaluator will step into the evaluation of the matched sub-expression 1 + 2 + 3.",
-    },
-  ],
+  message: {|Here `$v` matches any value when applying `step` filter to an expression.
+
+For example, `$v` matches `3`, but does not match `1 + 2` because it is an expression, not a value;
+
+`$v * $v` matches `3 * 4`, but does not match `(1 + 2) * (3 + 4)` because it both left hand side and right hand side of the multiplication are expressions, not values.
+
+In this case, the two `$v`s match the two values which are `3` (first `$v`) and `7` (second `$v`), thus the stepper stops when it evaluates to `3 * 7`.|},
+};
+
+let filter_selector_val_exp: form = {
+  let _v = exp("$v");
+  {
+    id: FilterSelector(Val),
+    syntactic_form: [_v],
+    expandable_id: Some((Piece.id(_v), [_v])),
+    explanation: "Matches value, i.e. fully evaluated expressions when apply filters to an expression.",
+    examples: [filter_selector_val_example],
+  };
+};
+
+let filter_selector_exps = (sel: Language.FilterSelector.t): group => {
+  switch (sel) {
+  | Exp => {
+      id: FilterSelector,
+      forms: [filter_selector_exp_exp, filter_selector_val_exp],
+    }
+  | Val => {
+      id: FilterSelector,
+      forms: [filter_selector_val_exp, filter_selector_exp_exp],
+    }
+  };
 };
```

</details>

<details open>
<summary><code>src/web/app/explainthis/ExplainThis.re</code> · doc lookup for FilterAction / FilterSelector</summary>

<!-- changetour:hunk file=src/web/app/explainthis/ExplainThis.re level=2 baseBlob=3d60f6f33d7fa2c6c70bd69d76de575de47e4d88 -->

```diff
@@ -809,6 +809,9 @@ let get_doc =
           TyAliasExp.tyalias_exps,
         );
       | Undefined => get_message(UndefinedExp.undefined_exps)
+      | FilterAction(act) => get_message(FilterExp.filter_action_exps(act))
+      | FilterSelector(sel) =>
+        get_message(FilterExp.filter_selector_exps(sel))
       | Deferral(_) => get_message(TerminalExp.deferral_exps)
       | ExplicitNonlabel => simple("Explicitly unlabeled entry")
       | Atom(Bool(b)) => get_message(TerminalExp.bool_exps(b))
```

</details>

<details open>
<summary><code>src/web/app/explainthis/ExplainThis.re</code> · Ap head doc: FilterAction case</summary>

<!-- changetour:hunk file=src/web/app/explainthis/ExplainThis.re level=2 baseBlob=3d60f6f33d7fa2c6c70bd69d76de575de47e4d88 -->

```diff
@@ -2063,6 +2066,7 @@ let get_doc =
           );
         };
         switch (x.term) {
+        | FilterAction(act) => get_message(FilterExp.filter_action_exps(act))
         | Constructor(v, _) =>
           basic(
             AppExp.conaps,
```

</details>

<details open>
<summary><code>src/web/app/explainthis/ExplainThis.re</code> · unified Filter doc with act/pat/body colorings</summary>

<!-- changetour:hunk file=src/web/app/explainthis/ExplainThis.re level=2 baseBlob=3d60f6f33d7fa2c6c70bd69d76de575de47e4d88 -->

```diff
@@ -2173,33 +2177,33 @@ let get_doc =
             ),
           SeqExp.seqs,
         );
-      | Filter(Filter({act: (Step, One), pat}), body) =>
-        message_single(
-          FilterExp.filter_pause(
-            ~p_id=Exp.rep_id(pat),
-            ~body_id=Exp.rep_id(body),
-          ),
-        )
-      | Filter(Filter({act: (Step, All), pat}), body) =>
-        message_single(
-          FilterExp.filter_debug(
-            ~p_id=Exp.rep_id(pat),
-            ~body_id=Exp.rep_id(body),
-          ),
-        )
-      | Filter(Filter({act: (Eval, All), pat}), body) =>
-        message_single(
-          FilterExp.filter_eval(
-            ~p_id=Exp.rep_id(pat),
-            ~body_id=Exp.rep_id(body),
-          ),
-        )
-      | Filter(Filter({act: (Eval, One), pat}), body) =>
-        message_single(
-          FilterExp.filter_hide(
-            ~p_id=Exp.rep_id(pat),
-            ~body_id=Exp.rep_id(body),
-          ),
+      | Filter(Filter({act: _, pat, ids: act_ids}), body)
+      | Filter(
+          Unresolved({
+            term:
+              Ap(Forward, {term: FilterAction(_), annotation: act_ids}, pat),
+            _,
+          }),
+          body,
+        ) =>
+        get_message(
+          ~colorings=
+            FilterExp.filter_exp_coloring_ids(
+              ~act_id=List.nth(act_ids.ids, 0),
+              ~pat_id=List.nth(IdTagged.ids(pat), 0),
+              ~body_id=List.nth(IdTagged.ids(body), 0),
+            ),
+          ~format=
+            Some(
+              msg =>
+                Printf.sprintf(
+                  Scanf.format_from_string(msg, "%s%s%s"),
+                  Id.to_string(List.nth(act_ids.ids, 0)),
+                  Id.to_string(List.nth(IdTagged.ids(pat), 0)),
+                  Id.to_string(List.nth(IdTagged.ids(body), 0)),
+                ),
+            ),
+          FilterExp.filter_exp,
         )
       | Filter(_) => simple("Internal expression")
       | Test(body) =>
```

</details>

<details open>
<summary><code>src/web/app/explainthis/Example.re</code> · mk_filter replaces mk_hide/mk_eval/mk_pause/mk_debug</summary>

<!-- changetour:hunk file=src/web/app/explainthis/Example.re level=2 baseBlob=845dc6521fa1f4517f6ac2f4ad9fd986c5503cad -->

```diff
@@ -75,10 +75,7 @@ let mk_test = mk_tile(Form.get(Test));
 let mk_hinted_test = mk_tile(Form.get(HintedTest));
 let mk_case = mk_tile(Form.get(Case));
 let mk_rule = mk_tile(Form.get(Rule));
-let mk_hide = mk_tile(Form.get(FilterHide));
-let mk_eval = mk_tile(Form.get(FilterEval));
-let mk_pause = mk_tile(Form.get(FilterPause));
-let mk_debug = mk_tile(Form.get(FilterDebug));
+let mk_filter = mk_tile(Form.get(Filter));
 let mk_theorem = mk_tile(Form.get(Theorem));
 let mk_proof_object = mk_tile(Form.get(ProofObject));
 let mk_forall = mk_tile(Form.get(Forall));
```

</details>

A new "Stepper Filters" documentation slide is added and registered in `Init.re`. The slide file is serialized editor state produced by the Hazel serializer, so the diff body is not meant to be read line-by-line — the source program walks through hiding evaluation, stopping at `fac(3)`, and stepping through a `map` call.

<details open>
<summary><code>src/web/init/Init.re</code> · register Filters.out documentation slide</summary>

<!-- changetour:hunk file=src/web/init/Init.re level=2 baseBlob=336e40cad584ef097540831870586b99fc3f7095 -->

```diff
@@ -20,6 +20,7 @@ let documentation_slides: list((string, PersistentSegment.t)) =
     Polymorphism.out,
     Cards.out,
     Probes.out,
+    Filters.out,
     Livelits.out,
   ]
   @ B2t2.Slides.all_slides;
```

</details>

<details>
<summary><code>src/web/init/docs/Filters.ml</code> · new "Stepper Filters" slide (serialized editor state)</summary>

<!-- changetour:hunk file=src/web/init/docs/Filters.ml level=2 baseBlob=82e3471bc6c3a8a511fc185ecea7fa089e4366d8 -->

```diff
@@ -0,0 +1,614 @@
+let out : string * Haz3lcore.PersistentSegment.t =
+  ( "Stepper Filters",
+    {
+      segment =
+        "((Secondary((id \
+         c1477c44-05e9-4810-b969-67375d23d23d)(content(Comment\"# We want to \
+         skip over the evaluation of most expressions, ... \
+         #\"))))(Secondary((id \
+         7b7c03f4-7a5b-43dc-89c6-1780df1286a7)(content(Whitespace\"\\n\"))))(Tile((id \
+         4ea94418-af04-4ee8-8b84-49202c3b3c1f)(label(debug in))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
+         45))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
+         d8b193fd-d3b3-4c99-a9d2-0b19811e79d3)(content(Whitespace\" \
+         \"))))(Tile((id \
+         cb38906b-f3bb-496e-8211-e38002e321e1)(label(hide))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         7bbad8d8-be5b-4f38-ab14-47583854fb55)(label(\"(\"\")\"))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
+         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
+         337c541f-5300-48c5-9617-74c29248e403)(label($e))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children()))))))))(Secondary((id \
+         ec618cff-c0de-4182-8d6a-74fa344c971e)(content(Whitespace\" \
+         \")))))))))(Secondary((id \
+         649ec23b-9a13-4fec-8219-0e3085ba4bf3)(content(Whitespace\"\\n\"))))(Secondary((id \
+         73d4e493-1802-403f-8020-8e112e46947f)(content(Comment\"# So that we \
+         can explicitly stop at some point in program execution. \
+         #\"))))(Secondary((id \
+         fe4cfe9f-2765-4daf-b202-bd18a7ef8956)(content(Whitespace\"\\n\"))))(Secondary((id \
+         053fca60-0086-48c9-a40c-386751366ba4)(content(Whitespace\"\\n\"))))(Secondary((id \
+         1c691f5c-a172-4f33-b973-3f2fdaf10884)(content(Comment\"# Here is a \
+         buggy factorial implementaiton. We know that fac(3) is problematic. \
+         #\"))))(Secondary((id \
+         9c8a8cca-ced6-4298-a3bf-f483728f3a35)(content(Whitespace\"\\n\"))))(Tile((id \
+         d525bd72-1860-4342-b878-3d16a1664197)(label(let = in))(mold((out \
+         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
+         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
+         d1f80e28-5a70-41d0-8cd7-f0265623684b)(content(Whitespace\" \
+         \"))))(Tile((id \
+         ddfe2ade-8cda-4d9e-82c4-bafa9c01cafd)(label(fac))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Secondary((id \
+         cde6d7f7-d2e1-4f2a-a432-d67f480fca5a)(content(Whitespace\" \
+         \"))))(Tile((id \
+         18d3568b-973c-43ce-b87b-3433e77dfdb1)(label(:))(mold((out \
+         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
+         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
+         e78d6212-1e77-42d1-af8b-e5a31206f4d8)(content(Whitespace\" \
+         \"))))(Tile((id \
+         9127f76c-6c53-4715-be15-e07b6d2ce62f)(label(Int))(mold((out \
+         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
+         Typ))))))(shards(0))(children())))(Secondary((id \
+         fb4b1c22-20b6-44b6-9f37-3bf1548f8fbe)(content(Whitespace\" \
+         \"))))(Tile((id \
+         f56fb730-0d31-497d-a3ef-d6b41474ca14)(label(->))(mold((out \
+         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
+         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
+         fa9554bd-9a1b-4adb-b78f-6cfdb7688718)(content(Whitespace\" \
+         \"))))(Tile((id \
+         f4580623-3369-46e9-9143-8ecc7e11664a)(label(Int))(mold((out \
+         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
+         Typ))))))(shards(0))(children())))(Secondary((id \
+         f8d937e1-e69c-4bb7-a0e1-ab85f463c675)(content(Whitespace\" \
+         \")))))((Secondary((id \
+         07e9b7a3-d18b-4f94-83d0-90e154137561)(content(Whitespace\"\\n\"))))(Tile((id \
+         44eb0357-4f1f-43e3-9ac0-ba09ae7b3ff8)(label(fun ->))(mold((out \
+         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
+         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
+         7a0612fa-5c4e-4c45-bc2a-1688bbb59777)(content(Whitespace\" \
+         \"))))(Tile((id \
+         2ece9893-ad91-4b66-b8d5-c8fb4cf33db1)(label(n))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Secondary((id \
+         9fa636c5-cc89-4bad-bc6c-6c9208b19ba0)(content(Whitespace\" \
+         \")))))))))(Secondary((id \
+         7c958ee4-ed39-45af-90b1-7eebff8da804)(content(Whitespace\"\\n\"))))(Tile((id \
+         f759980f-34ae-47a9-a4d0-1ae102383ba6)(label(case end))(mold((out \
+         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0 1))(children(((Secondary((id \
+         aa9774ff-2c92-4d6a-a190-8dade16c7f3b)(content(Whitespace\" \
+         \"))))(Tile((id \
+         ca76db09-c42f-4496-b813-157137f50da8)(label(n))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Secondary((id \
+         b0bd92ec-49d2-4285-9640-f42f90f60fa5)(content(Whitespace\"\\n\"))))(Tile((id \
+         8457bacc-6461-42d7-8542-611ef19a2933)(label(| =>))(mold((out \
+         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
+         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
+         5f81c133-965f-49db-af58-83715ae13e71)(content(Whitespace\" \
+         \"))))(Tile((id \
+         8f998190-b8f0-49d0-b7f6-78c3d149d4b6)(label(0))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Secondary((id \
+         bbb8c133-fb0b-4e18-aca3-df5bb4ab7759)(content(Whitespace\" \
+         \")))))))))(Secondary((id \
+         88242498-653c-4365-953c-959dca8c1fa6)(content(Whitespace\" \
+         \"))))(Tile((id \
+         62d0ee85-851e-44dc-b836-391cbf38758f)(label(0))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Secondary((id \
+         a99eeafe-8d87-4d2a-a330-0665c7cca3d2)(content(Whitespace\"\\n\"))))(Tile((id \
+         c21d7b6d-62c5-4922-8118-7930e02eee32)(label(| =>))(mold((out \
+         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
+         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
+         dcac906c-e16c-4b10-a4bf-512250c7dd5d)(content(Whitespace\" \
+         \"))))(Tile((id \
+         0f1a9e27-1090-4c36-9734-79cdc41a3825)(label(n))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Secondary((id \
+         d276a59e-1466-4393-b0bb-bc7c2b44a83a)(content(Whitespace\" \
+         \")))))))))(Secondary((id \
+         302d66fc-b4d1-4e3b-bb82-b7562e3b3a5a)(content(Whitespace\" \
+         \"))))(Tile((id \
+         43979646-4aca-472e-a403-762c1710c78e)(label(n))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Secondary((id \
+         bebb245f-b07a-411a-83e6-99ea3b8c7587)(content(Whitespace\" \
+         \"))))(Tile((id \
+         c3739d1f-8af4-446a-9eb0-23c0e605e65a)(label(*))(mold((out \
+         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
+         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
+         aefe08dd-86b0-4804-9b44-bcb5dfd91c48)(content(Whitespace\" \
+         \"))))(Tile((id \
+         66277136-eb09-49c2-a93b-206a47ec6a24)(label(fac))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         d141398f-bdbe-461c-8541-b084e296c615)(label(\"(\"\")\"))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
+         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
+         4296fc5c-2994-49b7-9fc5-5c634583eca3)(label(n))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Secondary((id \
+         838dfc12-c41e-423d-ad84-456bd07070be)(content(Whitespace\" \
+         \"))))(Tile((id \
+         1da8cf77-c5f3-4030-bd5e-7f087fcee907)(label(-))(mold((out \
+         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
+         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
+         4f214c3d-7823-433e-a5cd-18a079202242)(content(Whitespace\" \
+         \"))))(Tile((id \
+         e3f98377-2459-4a4b-b9f8-9e692b8c1bf0)(label(1))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children()))))))))(Secondary((id \
+         081f9a64-190c-4915-abd7-de468964c09e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
+         63cc058c-fd83-47e7-bbbe-84c569df3439)(content(Whitespace\"\\n\")))))))))(Secondary((id \
+         7a09f2f5-791b-44c4-b940-17fbf2b8bdcf)(content(Whitespace\"\\n\"))))(Secondary((id \
+         3750ff4d-cf3b-4d90-be0b-79612b394b63)(content(Whitespace\"\\n\"))))(Tile((id \
+         8c34b01a-a611-49d8-b52c-487e46b0591b)(label(let = in))(mold((out \
+         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
+         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
+         576fbc21-48d7-43dc-86c4-b007d74f8eec)(content(Whitespace\" \
+         \"))))(Tile((id \
+         fdabce1d-1a49-4b55-b599-babaf9c86b02)(label(stop_at_fac_3))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Secondary((id \
+         f79770ea-2f87-4ed0-80cb-5863dc390076)(content(Whitespace\" \
+         \")))))((Secondary((id \
+         cc7d7088-b43a-4e9a-9483-6aa5d56a521c)(content(Whitespace\" \
+         \"))))(Tile((id ec8d0bea-d355-4bab-a0e6-0feedb92b2fa)(label(fun \
+         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
+         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
+         1))(children(((Secondary((id \
+         8dd2e019-d4bd-4c01-b31c-876d6141298d)(content(Whitespace\" \
+         \"))))(Tile((id \
+         ebfea817-8df3-48a5-9f2e-1c83d4d03764)(label(\"()\"))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Secondary((id \
+         4d974f25-b529-4229-8d94-3af6e3222501)(content(Whitespace\" \
+         \")))))))))(Secondary((id \
+         662d0cae-9664-4d5c-825b-a4fb41a76b5c)(content(Whitespace\"\\n\"))))(Secondary((id \
+         697e26b7-bc13-4b15-b657-3f1a26c0a236)(content(Comment\"# Therefore, \
+         we can stop at the step where the program is about to evaluate \
+         fac(3): #\"))))(Secondary((id \
+         5a7bf3e1-97fe-4621-b623-a466c9c76a08)(content(Whitespace\"\\n\"))))(Tile((id \
+         4d768e07-3b1c-44d7-998b-952e63511ef1)(label(debug in))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
+         45))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
+         b42c453b-81a6-4cd9-95c0-ccb5135da05f)(content(Whitespace\" \
+         \"))))(Tile((id \
+         6d786dc1-3c2c-4a3d-b165-41d6045765b6)(label(step))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         776d5970-9145-4269-84b0-4eb00c3c45a1)(label(\"(\"\")\"))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
+         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
+         f0432083-e925-4f83-87c3-0aa9edaa4378)(label(fac))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         af6b2115-7711-467e-aff9-50d1f68eadd6)(label(\"(\"\")\"))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
+         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
+         59b882be-a618-49a0-be1a-f2d079f5b74a)(label(3))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
+         a2b621eb-c509-44eb-a64d-3d8415875c8a)(content(Whitespace\" \
+         \")))))))))(Secondary((id \
+         5ec97e0d-b747-453b-a9e0-dfa302c4cb15)(content(Whitespace\"\\n\"))))(Secondary((id \
+         f55b51a8-e8e8-43c3-aaed-397bf3e07480)(content(Comment\"# We run our \
+         debug-expression through the evaluation of fac(5) \
+         #\"))))(Secondary((id \
+         8bc52018-bcc4-4335-a77e-0633bfead0ca)(content(Whitespace\"\\n\"))))(Tile((id \
+         8a3d587a-5949-454a-9b37-89ebeeb069af)(label(fac))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         856f3054-472f-41b5-8767-a189406cf261)(label(\"(\"\")\"))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
+         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
+         717146f4-656e-4664-9614-98a2b031af8b)(label(5))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children()))))))))(Secondary((id \
+         ae609dee-f518-4e1c-99b2-65f2ea2f0b7a)(content(Whitespace\"\\n\"))))(Secondary((id \
+         0731e38c-4e10-49a0-af57-bd2e88bb0fce)(content(Comment\"# The program \
+         will stop at 5 * (4 * fac(3)), and we can take over and start to \
+         stepping through the evaluation of fac(3) manually. \
+         #\"))))(Secondary((id \
+         6b3a4e83-5004-46f1-a1bd-e78c710a1d2f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
+         54392ad8-cfd6-48b3-8ba3-fb101cd13a69)(content(Whitespace\"\\n\"))))(Secondary((id \
+         6100fb91-811e-4020-a159-e6497e88d0ed)(content(Whitespace\"\\n\"))))(Secondary((id \
+         c40afa93-77f9-4bd9-8e28-60cecd086efb)(content(Comment\"# Now, here is \
+         a correctly implemented map function that applies function f over all \
+         elements in array xs. #\"))))(Secondary((id \
+         df85adb1-49a9-4ee9-9fbf-b54c306bac2a)(content(Whitespace\"\\n\"))))(Tile((id \
+         0ab2ba9f-5f85-4182-944d-76396840b1a3)(label(let = in))(mold((out \
+         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
+         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
+         aa2ea586-8c22-4590-af43-43a0cc1fa8af)(content(Whitespace\" \
+         \"))))(Tile((id \
+         23fad498-e1a0-4585-91cb-29af3fc8b783)(label(map))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Secondary((id \
+         f43799d0-acc6-40d7-b46d-9b5630cabe19)(content(Whitespace\" \
+         \"))))(Tile((id \
+         ddfeb10c-5d86-4ee3-895f-af7e68148e02)(label(:))(mold((out \
+         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
+         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
+         55cc8969-cd10-44a3-92fa-1bd0b5d91f43)(content(Whitespace\" \
+         \"))))(Tile((id \
+         c0b27679-c688-49a8-a52f-edf870717020)(label(\"(\"\")\"))(mold((out \
+         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
+         Typ))))))(shards(0 1))(children(((Tile((id \
+         d4240403-2541-4083-bb71-a8be4ae47a5b)(label([ ]))(mold((out \
+         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
+         Typ))))))(shards(0 1))(children(((Tile((id \
+         fe5ec5a1-87fe-4c90-ba7a-cf298ca00422)(label(Int))(mold((out \
+         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
+         Typ))))))(shards(0))(children()))))))))(Tile((id \
+         09497ef4-c75b-4533-a423-836c27af3d30)(label(,))(mold((out \
+         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
+         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
+         e051c46f-b65b-49db-b708-51a07600a088)(content(Whitespace\" \
+         \"))))(Tile((id \
+         40c2c753-e8d3-42e9-aaec-b41fbae82e46)(label(Int))(mold((out \
+         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
+         Typ))))))(shards(0))(children())))(Secondary((id \
+         e79e9601-0314-4bf4-ba84-ab61527c2f98)(content(Whitespace\" \
+         \"))))(Tile((id \
+         7804411c-adc9-4400-a195-3431b78f5c71)(label(->))(mold((out \
+         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
+         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
+         301a4207-189e-4b5e-a211-4958c395b424)(content(Whitespace\" \
+         \"))))(Tile((id \
+         49dcc917-de72-40e5-9ba5-968f39f6dfb4)(label(Int))(mold((out \
+         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
+         Typ))))))(shards(0))(children()))))))))(Secondary((id \
+         3f89fadd-9c26-47c1-8852-1adae5693a07)(content(Whitespace\" \
+         \"))))(Tile((id \
+         2b6371b2-9fd6-49cb-b374-d48fdb4674e8)(label(->))(mold((out \
+         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
+         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
+         d2f686b5-d2fc-4b81-98cd-2b570480a1ff)(content(Whitespace\" \
+         \"))))(Tile((id 121b2fc1-d223-4169-a56c-fd7696a75a75)(label([ \
+         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
+         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
+         004c02e8-0547-464d-b29e-4d464f098236)(label(Int))(mold((out \
+         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
+         Typ))))))(shards(0))(children()))))))))(Secondary((id \
+         40c560ea-9623-46dd-9c3f-58eac1829a21)(content(Whitespace\" \
+         \")))))((Secondary((id \
+         04f209a0-2ca3-4424-8ea1-33d64d80d05d)(content(Whitespace\"\\n\"))))(Tile((id \
+         2e8c8064-2f9e-42c8-a8ea-493bba6efcd6)(label(fun ->))(mold((out \
+         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
+         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
+         aa9319a5-bf21-403c-80cd-6a9b00da1c5b)(content(Whitespace\" \
+         \"))))(Tile((id \
+         177680c8-1d6f-40fe-9075-97f7880ba73c)(label(xs))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Tile((id \
+         96747217-7751-4683-ae75-060e292341ab)(label(,))(mold((out \
+         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
+         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
+         fc9c127f-47e4-4f95-acc7-f7edc6eff183)(content(Whitespace\" \
+         \"))))(Tile((id \
+         8bfa5a26-a066-44c7-b589-1ae210236eb1)(label(f))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Secondary((id \
+         b40057e0-0cda-41d3-9626-474171b08c00)(content(Whitespace\" \
+         \")))))))))(Secondary((id \
+         adec0b6f-a926-4d5b-b2b7-d2cc28d3cd53)(content(Whitespace\"\\n\"))))(Tile((id \
+         494253b1-346c-4db0-83f6-1dcbcbbcff93)(label(case end))(mold((out \
+         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0 1))(children(((Secondary((id \
+         b646c80b-5852-452b-9b78-f6354a5e5e0d)(content(Whitespace\" \
+         \"))))(Tile((id \
+         ab1ec388-d320-48fc-8b82-6da4d68f1dbd)(label(xs))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Secondary((id \
+         a48c412c-9049-4b68-aedd-c5ea48dc14a3)(content(Whitespace\"\\n\"))))(Tile((id \
+         34ee3285-f5a9-42a1-aa24-48b4d7591e7f)(label(| =>))(mold((out \
+         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
+         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
+         e0df73fc-76c6-4f74-b723-519ae7b6258d)(content(Whitespace\" \
+         \"))))(Tile((id \
+         f5db8446-5803-43b5-9856-7d3cf439d76e)(label([]))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Secondary((id \
+         877d645f-a928-419a-aff6-df6218921c02)(content(Whitespace\" \
+         \")))))))))(Secondary((id \
+         944eaf9c-9f05-4e79-97d1-98d7b989ae9e)(content(Whitespace\" \
+         \"))))(Tile((id \
+         008d10fe-8285-4926-8241-3d14463acc97)(label([]))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Secondary((id \
+         93e72964-e7c5-4afa-8ec4-5a5f2475a665)(content(Whitespace\"\\n\"))))(Tile((id \
+         89ccbe20-9d76-4946-8cc2-66f46a26dbed)(label(| =>))(mold((out \
+         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
+         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
+         648a84bf-f88a-41c9-a0f8-ab21bfbb3492)(content(Whitespace\" \
+         \"))))(Tile((id \
+         6a0f1e53-442a-4357-b542-18cc18cc0bc6)(label(hd))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Tile((id \
+         da88f4f0-bb12-4d12-835a-80993ee674b7)(label(::))(mold((out \
+         Pat)(in_())(nibs(((shape(Concave 29))(sort Pat))((shape(Concave \
+         29))(sort Pat))))))(shards(0))(children())))(Tile((id \
+         85909875-98a5-49fc-b497-e0a2dcdecfbe)(label(tl))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Secondary((id \
+         dcd841e9-099c-480e-a75d-dd8e2d4af9bf)(content(Whitespace\" \
+         \")))))))))(Secondary((id \
+         5b5d33b1-6367-4349-91ec-dd6f59d468e6)(content(Whitespace\" \
+         \"))))(Tile((id \
+         561e66d5-1c8a-4f9f-806c-91d619b37221)(label(f))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         1119a1c2-a1b8-4d13-a9da-763a16a01e1e)(label(\"(\"\")\"))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
+         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
+         c1bad901-0ea7-4972-a6ea-b2860f1af932)(label(hd))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children()))))))))(Tile((id \
+         5c4cbc9c-648a-4910-b3b7-9fc340e16937)(label(::))(mold((out \
+         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
+         29))(sort Exp))))))(shards(0))(children())))(Tile((id \
+         184edcf9-f1c6-4685-9f53-a9597c59e5db)(label(map))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         cc73f31c-e4de-407b-9e90-22eb5f96d3f2)(label(\"(\"\")\"))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
+         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
+         e0b9ed6a-93fa-46d4-a48e-3b81c2385511)(label(tl))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         5e93061a-868e-475c-b78e-30c644cee55b)(label(,))(mold((out \
+         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
+         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
+         98d1e64b-fdc8-47d4-8428-78debb1f3435)(content(Whitespace\" \
+         \"))))(Tile((id \
+         d189b6d2-3b13-4581-92f6-62ff7c823eb9)(label(f))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children()))))))))(Secondary((id \
+         5fc6e82d-acd3-45d4-b9b5-5830f331f053)(content(Whitespace\"\\n\")))))))))(Secondary((id \
+         f95eafc7-f69f-4e0d-bdf7-25a8bbdd9ee0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
+         ae50bf00-a782-42b7-8854-ab4523bb1608)(content(Whitespace\"\\n\"))))(Tile((id \
+         47eb7cd2-6c46-4aa3-96a9-36fe6d4d6a1a)(label(let = in))(mold((out \
+         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
+         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
+         3a64a3d7-7cdd-4652-953a-8dc989782a9b)(content(Whitespace\" \
+         \"))))(Tile((id \
+         251fb5bc-64a0-4e60-9ad8-dad09cd69057)(label(square))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Secondary((id \
+         721ef875-daaa-4a10-9514-499895eb7572)(content(Whitespace\" \
+         \"))))(Tile((id \
+         ce983e8a-573d-4870-84b1-26c22bc7cfe7)(label(:))(mold((out \
+         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
+         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
+         0344b649-801c-4dbe-abae-667eec7d8bd6)(content(Whitespace\" \
+         \"))))(Tile((id \
+         827929bf-b196-4bb4-8fd0-3ed5cfa4d882)(label(Int))(mold((out \
+         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
+         Typ))))))(shards(0))(children())))(Secondary((id \
+         8b5f5686-9189-4249-8eba-3dddef95fcb3)(content(Whitespace\" \
+         \"))))(Tile((id \
+         b08ec0c8-816f-4697-b2bd-069b4cb2b77e)(label(->))(mold((out \
+         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
+         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
+         5f8da8b7-ae1d-4db2-92a7-22c73cde97e3)(content(Whitespace\" \
+         \"))))(Tile((id \
+         09395b09-6698-4916-8dc5-34f5d6240e1d)(label(Int))(mold((out \
+         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
+         Typ))))))(shards(0))(children())))(Secondary((id \
+         227ece1b-7bb1-4740-8e3a-944ff672d4f3)(content(Whitespace\" \
+         \")))))((Secondary((id \
+         3f5916ad-2fec-450d-94a8-ed4aad04ca10)(content(Whitespace\" \
+         \"))))(Tile((id 74eacfd8-8988-4925-9917-ae3deb2f0c49)(label(fun \
+         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
+         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
+         1))(children(((Secondary((id \
+         294ddb7d-29c2-4605-814f-329907ec5766)(content(Whitespace\" \
+         \"))))(Tile((id \
+         0f897ecd-b4ae-4ecc-99b2-fd81ef78a41c)(label(x))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Secondary((id \
+         abf61520-7dbc-4ddf-9558-076bd40eafcc)(content(Whitespace\" \
+         \")))))))))(Secondary((id \
+         944d5552-157b-4236-a2f4-19c0962db5f6)(content(Whitespace\" \
+         \"))))(Tile((id \
+         da0a1e05-4253-4407-a978-98c33ff6a1ad)(label(x))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Secondary((id \
+         6d84d6c2-3496-4cd4-8dd7-e7c7aca0eb8a)(content(Whitespace\" \
+         \"))))(Tile((id \
+         9df85cb0-4b3b-4eed-bff7-fbda0d3c61ff)(label(*))(mold((out \
+         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
+         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
+         ce70c735-c90b-4f29-8a7c-3ad6a3f9762c)(content(Whitespace\" \
+         \"))))(Tile((id \
+         ced5b077-9c30-4744-b3f9-74eeafd47fc6)(label(x))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Secondary((id \
+         52935d53-831d-41a6-8b7e-ff79590b02b0)(content(Whitespace\" \
+         \")))))))))(Secondary((id \
+         e2d1ac53-40a7-492d-a241-34e61d88e733)(content(Whitespace\"\\n\"))))(Secondary((id \
+         e751acef-3602-458a-b992-106d46a9325f)(content(Whitespace\"\\n\"))))(Secondary((id \
+         3fc9fdef-e3fd-4e65-a5b6-9afef17db56c)(content(Comment\"# One can \
+         easily verify the function actually does such thing, by ... \
+         #\"))))(Secondary((id \
+         6e5c937a-a6d4-40e1-94af-c966a366ae1d)(content(Whitespace\"\\n\"))))(Tile((id \
+         421eec6a-ea21-44a7-861b-0e05a42de3be)(label(let = in))(mold((out \
+         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
+         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
+         ee6e1232-eb3d-4031-85d2-837f34d852b4)(content(Whitespace\" \
+         \"))))(Tile((id \
+         a0e0ada1-2b2b-4799-bfda-ae741ba7c9b8)(label(stop_at_square))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Secondary((id \
+         6c1860ba-32d3-45fb-b220-24539a0a4727)(content(Whitespace\" \
+         \")))))((Secondary((id \
+         412ce9dd-7c1e-4dc4-a4d8-4b7214d21f34)(content(Whitespace\" \
+         \"))))(Tile((id 82161c22-8194-4419-9747-051e26a8180e)(label(fun \
+         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
+         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
+         1))(children(((Secondary((id \
+         cf1229c7-f090-4eac-b9dd-fd663038121f)(content(Whitespace\" \
+         \"))))(Tile((id \
+         f6b884e9-189c-47a9-ac55-5b77e6e2f0fe)(label(\"()\"))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Secondary((id \
+         027fce10-448b-409d-95f0-0b4bc30db1f0)(content(Whitespace\" \
+         \")))))))))(Secondary((id \
+         56ae5a09-681c-41e0-b817-0bd5a75c984a)(content(Whitespace\"\\n\"))))(Secondary((id \
+         abb91184-94c3-45b1-8776-7f794f6c0b0b)(content(Comment\"# Stopping at \
+         each application of the function square #\"))))(Secondary((id \
+         146a064b-cd6f-43f9-9131-caa79f9a8fbd)(content(Whitespace\"\\n\"))))(Tile((id \
+         4fb760bf-3569-4140-a4a1-59fb9226fbca)(label(debug in))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
+         45))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
+         68164552-487e-446f-b8ac-ef38a8789372)(content(Whitespace\" \
+         \"))))(Tile((id \
+         4eeaebd9-62cc-469a-97d6-6f4206142781)(label(stop))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         7f077b69-3a9d-4109-8171-801386840cd6)(label(\"(\"\")\"))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
+         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
+         1535e4f5-328b-4126-9746-71d88874f21b)(label(square))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         1dea4a81-ed9d-4826-a830-a930bd4b4d5a)(label(\"(\"\")\"))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
+         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
+         bc8db55f-482f-40de-adf4-29bcfd28b764)(label($v))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
+         17415fbc-d297-44cf-bf63-8c08e769ac5d)(content(Whitespace\" \
+         \")))))))))(Secondary((id \
+         cefcca6f-31b1-40f0-94ac-6c44bd3db8ac)(content(Whitespace\"\\n\"))))(Tile((id \
+         da1694b4-6336-4cd3-aa7f-cc781e35f194)(label(map))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         9eaa8713-9b34-4286-83c7-3258bdecc03e)(label(\"(\"\")\"))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
+         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
+         19f58516-73eb-4631-9636-e8099abca427)(label([ ]))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0 1))(children(((Tile((id \
+         3fbc580d-b244-4c33-bf8c-e8cc380afeb6)(label(1))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         727017eb-f021-4a32-b2c3-7bfedabe018a)(label(,))(mold((out \
+         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
+         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
+         7498d104-2a6c-4262-84fa-486d01cc10f6)(content(Whitespace\" \
+         \"))))(Tile((id \
+         3dd00361-c5ff-4704-a66d-0ebc57b5cfc9)(label(2))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         8763a6fc-a278-48b4-8482-2570976b42ba)(label(,))(mold((out \
+         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
+         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
+         810eaa96-453b-4f8c-a35c-552918a901f6)(content(Whitespace\" \
+         \"))))(Tile((id \
+         7a5175ca-1fa3-4c6f-b6af-06250ed90ccb)(label(3))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children()))))))))(Tile((id \
+         b83a1e7f-76f4-4111-84e6-3aa265633ff0)(label(,))(mold((out \
+         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
+         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
+         894db2c8-4d33-4eee-9f71-0e0feef85903)(content(Whitespace\" \
+         \"))))(Tile((id d7414812-5dda-4ffd-9c1a-68001bbca082)(label(fun \
+         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
+         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
+         1))(children(((Secondary((id \
+         e4209a3c-ed9a-48f2-a6c4-e699a27e7676)(content(Whitespace\" \
+         \"))))(Tile((id \
+         91011fbe-024b-4e53-a371-82924ca0a8de)(label(x))(mold((out \
+         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
+         Pat))))))(shards(0))(children())))(Secondary((id \
+         64d0f38c-f073-44f0-b5eb-0accb56154f4)(content(Whitespace\" \
+         \")))))))))(Secondary((id \
+         356acc9a-85b5-4d43-833f-75ea84fe61e3)(content(Whitespace\" \
+         \"))))(Tile((id \
+         1c3e8a12-05b6-4842-bde0-0c6167d005e7)(label(square))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         b5431302-bebc-4a88-8771-de33b48830aa)(label(\"(\"\")\"))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
+         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
+         97f4f1ef-27fe-40d6-a205-5e39b83e7220)(label(x))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
+         a1753ef6-873d-4dec-9710-f14bacabb47d)(content(Whitespace\"\\n\"))))(Secondary((id \
+         0e304ad2-e548-449c-8279-ffeb7ae3b718)(content(Comment\"# The program \
+         will stop at [square(1), square(2), square(3)] #\"))))(Secondary((id \
+         138cd1be-42de-4fed-bb32-369e3c8b8b4a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
+         01b81139-0c67-42ee-8452-63c59ebd13d4)(content(Whitespace\"\\n\"))))(Secondary((id \
+         8b56667b-7fb8-496d-8a3b-1f40b5e9d879)(content(Whitespace\"\\n\"))))(Tile((id \
+         ffce14e5-d862-4b38-9618-c16f344f54f0)(label(\"(\"\")\"))(mold((out \
+         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0 1))(children(((Secondary((id \
+         f1c0bfa2-6654-4c09-ba5b-61e5140a2ff6)(content(Whitespace\"\\n\"))))(Tile((id \
+         e0ba3725-c951-478d-83dd-00aef4aef1af)(label(stop_at_fac_3))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         fdfd05f6-6c4e-43f4-9d6b-521d43a78dc2)(label(\"()\"))(mold((out \
+         Exp)(in_())(nibs(((shape(Concave 23))(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         79bef5cf-726f-43a6-a467-92171c33546d)(label(,))(mold((out \
+         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
+         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
+         accc2952-c31d-4f62-996f-247e7d80c95a)(content(Whitespace\"\\n\"))))(Tile((id \
+         2600d2df-8daa-4867-8678-18000b3a5124)(label(stop_at_square))(mold((out \
+         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Tile((id \
+         ce54f633-d6ec-45cc-b3e8-dd1c51019907)(label(\"()\"))(mold((out \
+         Exp)(in_())(nibs(((shape(Concave 23))(sort Exp))((shape Convex)(sort \
+         Exp))))))(shards(0))(children())))(Secondary((id \
+         c135c7f8-86d3-49bc-812d-4faee1eed56d)(content(Whitespace\"\\n\"))))))))))";
+      backup_text =
+        "# We want to skip over the evaluation of most expressions, ... #\n\
+         debug hide($e) in\n\
+         # So that we can explicitly stop at some point in program execution. \
+         #\n\n\
+         # Here is a buggy factorial implementaiton. We know that fac(3) is \
+         problematic. #\n\
+         let fac : Int -> Int =\n\
+         fun n ->\n\
+         case n\n\
+         | 0 => 0\n\
+         | n => n * fac(n - 1)\n\
+         end\n\
+         in\n\n\
+         let stop_at_fac_3 = fun () ->\n\
+         # Therefore, we can stop at the step where the program is about to \
+         evaluate fac(3): #\n\
+         debug step(fac(3)) in\n\
+         # We run our debug-expression through the evaluation of fac(5) #\n\
+         fac(5)\n\
+         # The program will stop at 5 * (4 * fac(3)), and we can take over and \
+         start to stepping through the evaluation of fac(3) manually. #\n\
+         in\n\n\
+         # Now, here is a correctly implemented map function that applies \
+         function f over all elements in array xs. #\n\
+         let map : ([Int], Int -> Int) -> [Int] =\n\
+         fun xs, f ->\n\
+         case xs\n\
+         | [] => []\n\
+         | hd::tl => f(hd)::map(tl, f)\n\
+         end\n\
+         in\n\
+         let square : Int -> Int = fun x -> x * x in\n\n\
+         # One can easily verify the function actually does such thing, by ... #\n\
+         let stop_at_square = fun () ->\n\
+         # Stopping at each application of the function square #\n\
+         debug stop(square($v)) in\n\
+         map([1, 2, 3], fun x -> square(x))\n\
+         # The program will stop at [square(1), square(2), square(3)] #\n\
+         in\n\n\
+         (\n\
+         stop_at_fac_3(),\n\
+         stop_at_square()\n\
+         )";
+      refractors = "()";
+    } )
```

</details>

## Tests

Elaboration tests check that the parsed `Unresolved(act(pat))` shape elaborates to the resolved `Filter({act, pat})` for all four actions; the old single menhir filter test is superseded by per-action parser tests in `Test_Menhir`, which also cover `$e`/`$v` selector patterns.

<details open>
<summary><code>test/Test_Elaboration.re</code> · filter_detection: Unresolved act(pat) elaborates to resolved Filter</summary>

<!-- changetour:hunk file=test/Test_Elaboration.re level=2 baseBlob=e26a0b9a691bc35cec076519c2499dd2f1d8a3cb -->

```diff
@@ -475,6 +475,21 @@ module PlainTests = {
         ();
       }
     });
+  let filter_detection = (act, ()) => {
+    let filter_detection_program: Exp.t =
+      Exp.filter_unresolved(
+        Exp.ap(Forward, Exp.filter_action(act), Exp.int(1)),
+        Exp.int(0),
+      );
+    alco_check(
+      "Filter detection (" ++ FilterAction.string_of_t(act) ++ ")",
+      {
+        Exp.filter(~act, ~pat=Exp.int(1), Exp.int(0));
+      },
+      dhexp_of_uexp(filter_detection_program),
+    );
+  };
+
   let tests = [
     test_case("Single integer", `Quick, single_integer),
     test_case("Empty hole", `Quick, empty_hole),
```

</details>

<details open>
<summary><code>test/Test_Elaboration.re</code> · filter detection cases for eval/hide/step/stop</summary>

<!-- changetour:hunk file=test/Test_Elaboration.re level=2 baseBlob=e26a0b9a691bc35cec076519c2499dd2f1d8a3cb -->

```diff
@@ -881,6 +896,26 @@ in 1|},
         }
       }),
     ),
+    test_case(
+      "Filter detection (eval)",
+      `Quick,
+      filter_detection((Eval, All)),
+    ),
+    test_case(
+      "Filter detection (hide)",
+      `Quick,
+      filter_detection((Eval, One)),
+    ),
+    test_case(
+      "Filter detection (step)",
+      `Quick,
+      filter_detection((Step, All)),
+    ),
+    test_case(
+      "Filter detection (stop)",
+      `Quick,
+      filter_detection((Step, One)),
+    ),
   ];
 };
 module MenhirElaborationTests = {
```

</details>

<details open>
<summary><code>test/Test_Elaboration.re</code> · drop old "eval 1 in 0" menhir filter fixture</summary>

<!-- changetour:hunk file=test/Test_Elaboration.re level=2 baseBlob=e26a0b9a691bc35cec076519c2499dd2f1d8a3cb -->

```diff
@@ -1035,18 +1070,6 @@ module MenhirElaborationTests = {
   let test_menhir = () =>
     alco_check_menhir("Test failed (menhir)", test_str, test_uexp);
 
-  let filter_str = "eval 1 in 0";
-  let stepper_filter_kind: TermBase.stepper_filter_kind_t =
-    StepperFilter.(
-      filter({
-        pat: Exp.int(1),
-        act: (FilterAction.Eval, FilterAction.All),
-      })
-    );
-  let filter_uexp: Exp.t = Exp.(filter(stepper_filter_kind, int(0)));
-  let filter_menhir = () =>
-    alco_check_menhir("Filter test (menhir)", filter_str, filter_uexp);
-
   let undefined_str = "
 undef
 ";
```

</details>

<details open>
<summary><code>test/Test_Elaboration.re</code> · drop old menhir filter test case</summary>

<!-- changetour:hunk file=test/Test_Elaboration.re level=2 baseBlob=e26a0b9a691bc35cec076519c2499dd2f1d8a3cb -->

```diff
@@ -1113,7 +1136,6 @@ x
     alco_check_menhir("FixF test (menhir)", fixf_str, fixf_uexp);
 
   let tests = [
-    test_case("Filter test (menhir)", `Quick, filter_menhir),
     test_case("Test failed (menhir)", `Quick, test_menhir),
     test_case(
       "Dynamic error hole (menhir)",
```

</details>

<details open>
<summary><code>test/Test_Menhir.re</code> · full parser tests: debug eval/hide/step/stop(…) in …, $e/$v patterns</summary>

<!-- changetour:hunk file=test/Test_Menhir.re level=2 baseBlob=8fc5811b82f108c69e23b4c4aa7024a957e26a60 -->

```diff
@@ -292,15 +292,60 @@ let tests =
         "test 3 == 3 end",
       ),
       full_parser_test(
-        "Filter",
-        filter(
-          Filter({
-            act: (Eval, All),
-            pat: int(3),
-          }),
+        "Filter (eval)",
+        filter_unresolved(
+          ap(Forward, filter_action((Eval, All)), int(3)),
           int(3),
         ),
-        "eval 3 in 3" // TODO Use other filter commands
+        "debug eval(3) in 3" // TODO Use other filter commands
+      ),
+      full_parser_test(
+        "Filter (hide)",
+        filter_unresolved(
+          ap(Forward, filter_action((Eval, One)), int(3)),
+          int(3),
+        ),
+        "debug hide(3) in 3" // TODO Use other filter commands
+      ),
+      full_parser_test(
+        "Filter (step)",
+        filter_unresolved(
+          ap(Forward, filter_action((Step, All)), int(3)),
+          int(3),
+        ),
+        "debug step(3) in 3" // TODO Use other filter commands
+      ),
+      full_parser_test(
+        "Filter (stop)",
+        filter_unresolved(
+          ap(Forward, filter_action((Step, One)), int(3)),
+          int(3),
+        ),
+        "debug stop(3) in 3" // TODO Use other filter commands
+      ),
+      full_parser_test(
+        "Filter selector exp in pattern",
+        filter_unresolved(
+          ap(Forward, filter_action((Eval, All)), filter_selector(Exp)),
+          bin_op(Int(Plus), int(1), int(2)),
+        ),
+        "debug eval($e) in 1 + 2",
+      ),
+      full_parser_test(
+        "Filter selector val pattern",
+        filter_unresolved(
+          ap(
+            Forward,
+            filter_action((Step, One)),
+            bin_op(Int(Plus), filter_selector(Val), filter_selector(Val)),
+          ),
+          bin_op(
+            Int(Plus),
+            bin_op(Int(Plus), int(1), int(2)),
+            bin_op(Int(Plus), int(3), int(4)),
+          ),
+        ),
+        "debug stop($v + $v) in (1 + 2) + (3 + 4)",
       ),
       full_parser_test(
         "List Concat",
```

</details>

Pretty-printer and round-trip tests move to the new syntax. The dedicated `unresolved_filter_ids_test` pins an id-stability property: the printed `debug … in` tile must reuse the outer filter expression's id (not the inner condition's), and no tile id may appear twice in the printed segment.

<details open>
<summary><code>test/Test_ExpToSegment.re</code> · pause prints as debug stop(1) in 2</summary>

<!-- changetour:hunk file=test/Test_ExpToSegment.re level=2 baseBlob=772a770bcf2fb177151f9a27cd8666c4566a8e81 -->

```diff
@@ -260,18 +299,12 @@ let tests = (
         let segment =
           exp_to_segment(
             IdTagged.FreshGrammar.Exp.(
-              filter(
-                Filter({
-                  pat: int(1),
-                  act: (Step, One),
-                }),
-                int(2),
-              )
+              filter(~pat=int(1), ~act=(Step, One), int(2))
             ),
           );
         let serialized = print_seg(segment);
 
-        check(string, "Pause", serialized, {|pause 1 in 2|});
+        check(string, "Pause", serialized, {|debug stop(1) in 2|});
       },
     ),
     test_case(
```

</details>

<details open>
<summary><code>test/Test_ExpToSegment.re</code> · unresolved filter: outer id reused, all tile ids unique</summary>

<!-- changetour:hunk file=test/Test_ExpToSegment.re level=2 baseBlob=772a770bcf2fb177151f9a27cd8666c4566a8e81 -->

```diff
@@ -515,6 +548,78 @@ let exp_to_segment_roundtrip_settings: ExpToSegment.Settings.t = {
 let exp_to_segment_roundtrip =
   ExpToSegment.exp_to_segment(~settings=exp_to_segment_roundtrip_settings);
 
+let rec tile_ids = (seg: Segment.t): list(Id.t) =>
+  seg
+  |> List.concat_map(
+       fun
+       | Tile(t) => [t.id, ...List.concat_map(tile_ids, t.children)]
+       | _ => [],
+     );
+
+let rec find_tile_id = (~label: Label.t, seg: Segment.t): option(Id.t) => {
+  let rec find_in_children = (children: list(Segment.t)): option(Id.t) =>
+    switch (children) {
+    | [] => None
+    | [child, ...rest] =>
+      switch (find_tile_id(~label, child)) {
+      | Some(_) as found => found
+      | None => find_in_children(rest)
+      }
+    };
+
+  switch (seg) {
+  | [] => None
+  | [Tile({id, label: tile_label, children, _}), ...rest] =>
+    if (tile_label == label) {
+      Some(id);
+    } else {
+      switch (find_in_children(children)) {
+      | Some(_) as found => found
+      | None => find_tile_id(~label, rest)
+      };
+    }
+  | [_, ...rest] => find_tile_id(~label, rest)
+  };
+};
+
+let unresolved_filter_ids_test =
+  test_case({|Filter: unresolved uses outer id|}, `Quick, () => {
+    switch (Parser.to_term({|debug unknown($e) in x|}, ~root=Exp)) {
+    | Some(term) =>
+      switch (Exp.term_of(term)) {
+      | Filter(Unresolved(filt_exp), _) =>
+        let seg = exp_to_segment_roundtrip(term);
+        switch (find_tile_id(~label=["debug", "in"], seg)) {
+        | Some(debug_id) =>
+          check(
+            bool,
+            "debug tile uses the outer filter id",
+            true,
+            Id.equal(debug_id, Exp.rep_id(term)),
+          );
+          check(
+            bool,
+            "debug tile does not reuse the child filter id",
+            false,
+            Id.equal(debug_id, Exp.rep_id(filt_exp)),
+          );
+        | None => Alcotest.fail("Missing debug/in tile")
+        };
+
+        let ids = tile_ids(seg);
+        let unique_ids = List.sort_uniq(Id.compare, ids);
+        check(
+          int,
+          "pretty-printed filter tile ids are unique",
+          List.length(ids),
+          List.length(unique_ids),
+        );
+      | _ => Alcotest.fail("Expected unresolved filter term")
+      }
+    | None => Alcotest.fail("Failed to parse unresolved filter")
+    }
+  });
+
 /* Test that a string round-trips through segment → term → segment */
 let roundtrip_test = (name: string, input: string) =>
   test_case(name, `Quick, () => {
```

</details>

<details open>
<summary><code>test/Test_ExpToSegment.re</code> · round-trips for debug action(…) in and $e/$v</summary>

<!-- changetour:hunk file=test/Test_ExpToSegment.re level=2 baseBlob=772a770bcf2fb177151f9a27cd8666c4566a8e81 -->

```diff
@@ -721,15 +826,24 @@ in f(42)|},
         };
       },
     ),
-    /* Filter expressions (hide/eval/pause/debug ... in) and unquote ($) */
-    roundtrip_test({|Filter: hide|}, {|hide 1 in 2|}),
-    roundtrip_test({|Filter: hide spaced|}, {|hide 1  in  2|}),
-    roundtrip_test({|Filter: eval|}, {|eval 1 in 2|}),
-    roundtrip_test({|Filter: eval spaced|}, {|eval 1  in  2|}),
-    roundtrip_test({|Filter: pause|}, {|pause 1 in 2|}),
-    roundtrip_test({|Filter: pause spaced|}, {|pause 1  in  2|}),
-    roundtrip_test({|Filter: debug|}, {|debug 1 in 2|}),
-    roundtrip_test({|Filter: debug spaced|}, {|debug 1  in  2|}),
+    /* Filter expressions: debug <action>(<pat>) in <body> */
+    roundtrip_test({|Filter: hide|}, {|debug hide(1) in 2|}),
+    roundtrip_test({|Filter: hide spaced|}, {|debug hide(1)  in  2|}),
+    roundtrip_test({|Filter: eval|}, {|debug eval(1) in 2|}),
+    roundtrip_test({|Filter: eval spaced|}, {|debug eval(1)  in  2|}),
+    roundtrip_test({|Filter: stop|}, {|debug stop(1) in 2|}),
+    roundtrip_test({|Filter: stop spaced|}, {|debug stop(1)  in  2|}),
+    roundtrip_test({|Filter: step|}, {|debug step(1) in 2|}),
+    roundtrip_test({|Filter: step spaced|}, {|debug step(1)  in  2|}),
+    /* Filter selector ($e, $v) within filter expressions */
+    roundtrip_test({|FilterSelector: $e in eval|}, {|debug eval($e) in x|}),
+    roundtrip_test(
+      {|FilterSelector: $e preserves spacing|},
+      {|debug eval( $e ) in x|},
+    ),
+    roundtrip_test({|FilterSelector: $v in hide|}, {|debug hide($v) in 2|}),
+    roundtrip_test({|FilterSelector: in step|}, {|debug step($v) in 2|}),
+    unresolved_filter_ids_test,
     roundtrip_test(
       {|QuotedLabel: label needing quotes (has dash)|},
       {|(`the-answer`=42)|},
```

</details>

<details open>
<summary><code>test/Test_PrettyPrint.re</code> · debug hide(1 + 2) in chains with let</summary>

<!-- changetour:hunk file=test/Test_PrettyPrint.re level=2 baseBlob=a580ca0824b99c35434959780910f2ba46a19958 -->

```diff
@@ -750,12 +750,12 @@ let x = 1 in
 x|},
     (),
   ),
-  /* hide/in: chains with let */
+  /* debug/in: chains with let (filter syntax). */
   test_format_seg(
     ~name="Hide/in chains with let",
     ~width=40,
-    ~input="hide 1 + 2 in let x = 1 in x",
-    ~expected={|hide 1 + 2 in
+    ~input="debug hide(1 + 2) in let x = 1 in x",
+    ~expected={|debug hide(1 + 2) in
 let x = 1 in
 x|},
     (),
```

</details>

<details open>
<summary><code>test/Test_Grammar.re</code> · sample Filter expression via new residue factory</summary>

<!-- changetour:hunk file=test/Test_Grammar.re level=2 baseBlob=75b17aacb8fa59e12bd52c4830c0503bd0a385cb -->

```diff
@@ -76,8 +76,7 @@ let sample_expression = (cls_exp: Exp.cls): Grammar.UnitGrammar.exp => {
       | Seq => seq(empty_hole(), empty_hole())
       | Test => test(empty_hole())
       | HintedTest => hinted_test(empty_hole(), empty_hole())
-      | Filter =>
-        filter(StepperFilter.residue(0, (Step, One)), empty_hole())
+      | Filter => residue(~lvl=0, ~act=(Step, One), empty_hole())
       | Closure =>
         module M = {
           include VarBstMap.Ordered;
```

</details>

<details open>
<summary><code>test/Test_MatchExp.re</code> · filter selectors match themselves, not each other</summary>

<!-- changetour:hunk file=test/Test_MatchExp.re level=2 baseBlob=e895383c53b2a9ab2dd6e9126c1602a7614d73af -->

```diff
@@ -57,6 +57,21 @@ let tests = [
   (
     "MatchExp",
     [
+      test_case(
+        "Filter selector matches same selector",
+        `Quick,
+        match_check("$e", "$e", Some([])),
+      ),
+      test_case(
+        "Filter selector matches same val selector",
+        `Quick,
+        match_check("$v", "$v", Some([])),
+      ),
+      test_case(
+        "Filter selector does not match different selector",
+        `Quick,
+        match_check("$e", "$v", None),
+      ),
       test_case(
         "Match a variable",
         `Quick,
```

</details>

Stepper behavior tests cover each action end-to-end — auto-stepping under `eval`, one-shot `hide`, visible pauses for `stop`/`step` — plus regressions for the two dynamics fixes: a user `stop` filter must override settings-based pre-filtering (visible `fac` pauses), while unrenderable ascription steps must never surface, and persist/refresh round-trips must not loop or overflow.

<details>
<summary><code>test/evaluator/Test_Stepper.re</code> · end-to-end stepper filter behavior + regression tests</summary>

<!-- changetour:hunk file=test/evaluator/Test_Stepper.re level=2 baseBlob=b8a9463ea2644c3cbdecaab2618b6c1a5a14625b -->

```diff
@@ -2,9 +2,394 @@ open Alcotest;
 open Language;
 open Test_Evaluator_Prelude;
 
+let step_status = exp =>
+  EvaluatorStep.get_status(~settings=CoreSettings.on, exp, Environment.empty);
+
+let rec steps_until_available = (~limit, exp) =>
+  if (limit <= 0) {
+    Alcotest.fail("expected available steps before step limit");
+  } else {
+    switch (step_status(exp)) {
+    | AutoStep(step) =>
+      switch (EvaluatorStep.take_step(step)) {
+      | None => Alcotest.fail("expected auto step")
+      | Some(exp') => steps_until_available(~limit=limit - 1, exp')
+      }
+    | AvailableSteps(steps) => steps
+    };
+  };
+
+let rec count_available_steps = (~limit, exp, count) =>
+  if (limit <= 0) {
+    Alcotest.fail("step count exceeded limit");
+  } else {
+    switch (step_status(exp)) {
+    | AutoStep(step) =>
+      switch (EvaluatorStep.take_step(step)) {
+      | None => count
+      | Some(exp') => count_available_steps(~limit=limit - 1, exp', count)
+      }
+    | AvailableSteps(steps) =>
+      switch (steps) {
+      | [] => count
+      | [step, ..._] =>
+        switch (EvaluatorStep.take_step(step)) {
+        | None => count
+        | Some(exp') =>
+          count_available_steps(~limit=limit - 1, exp', count + 1)
+        }
+      }
+    };
+  };
+
 let tests = (
   "Evaluator.Stepper",
   [
+    test_case(
+      "Eval filter auto-steps",
+      `Quick,
+      () => {
+        let exp =
+          parse_exp("debug eval($e) in (1 + 2) + (3 + 4)") |> elaborate;
+        switch (step_status(exp)) {
+        | AutoStep(step) =>
+          switch (EvaluatorStep.take_step(step)) {
+          | None => Alcotest.fail("expected auto step")
+          | Some(exp') =>
+            switch (step_status(exp')) {
+            | AutoStep(_) => ()
+            | AvailableSteps(_) =>
+              Alcotest.fail("expected auto step to continue")
+            }
+          }
+        | AvailableSteps(_) => Alcotest.fail("expected AutoStep")
+        };
+      },
+    ),
+    test_case(
+      "Hide filter only auto-steps once",
+      `Quick,
+      () => {
+        let exp =
+          parse_exp("debug hide(1 + 2) in (1 + 2) + (3 + 4)") |> elaborate;
+        /* Auto-step through all hidden steps (RemoveParens, filter match, etc.)
+           until we get AvailableSteps. The hide filter with (Eval, One) should
+           cause stepping to stop after one filter-matched step. */
+        let steps = steps_until_available(~limit=20, exp);
+        check(bool, "expected visible steps", true, steps != []);
+      },
+    ),
+    test_case(
+      "Hide filter does not match non-values",
+      `Quick,
+      () => {
+        let exp = parse_exp("debug hide($v) in 1 + 2") |> elaborate;
+        /* $v should not match the non-value expression 1 + 2, so after
+           auto-stepping through any hidden steps, we should get visible steps */
+        let steps = steps_until_available(~limit=20, exp);
+        check(bool, "expected visible steps", true, steps != []);
+      },
+    ),
+    test_case(
+      "Stop filter yields visible steps",
+      `Quick,
+      () => {
+        let exp =
+          parse_exp("debug stop($v + $v) in (1 + 2) + (3 + 4)") |> elaborate;
+        /* stop = (Step, One): after auto-stepping through hidden steps
+           (RemoveParens etc.), we should get visible steps */
+        let steps = steps_until_available(~limit=20, exp);
+        check(bool, "expected visible steps", true, steps != []);
+      },
+    ),
+    test_case(
+      "Step filter yields visible steps",
+      `Quick,
+      () => {
+        let exp =
+          parse_exp("debug step($v + $v) in (1 + 2) + (3 + 4)") |> elaborate;
+        /* step = (Step, All): after auto-stepping through hidden steps,
+           we should get visible steps */
+        let steps = steps_until_available(~limit=20, exp);
+        check(bool, "expected visible steps", true, steps != []);
+      },
+    ),
+    test_case(
+      "Stop filter on map hits square application",
+      `Quick,
+      () => {
+        let program = {|
+debug hide($e) in
+let map =
+  fun xs, f ->
+    case xs
+      | [] => []
+      | hd :: tl => f(hd) :: map(tl, f)
+    end
+in
+let square = fun x -> x * x in
+debug stop(square($v)) in
+map([1, 2, 3], square)|};
+        let exp = parse_exp(program) |> elaborate;
+        let steps = steps_until_available(~limit=200, exp);
+        check(bool, "expected visible steps", true, steps != []);
+      },
+    ),
+    test_case(
+      "Stop filter map requires multiple steps",
+      `Quick,
+      () => {
+        let program = {|
+debug hide($e) in
+let map =
+  fun xs, f ->
+    case xs
+      | [] => []
+      | hd :: tl => f(hd) :: map(tl, f)
+    end
+in
+let square = fun x -> x * x in
+debug stop(square($v)) in
+map([1, 2, 3], square)|};
+        let exp = parse_exp(program) |> elaborate;
+        let steps = count_available_steps(~limit=500, exp, 0);
+        check(int, "expected exact 3 steps", 3, steps);
+      },
+    ),
+    test_case(
+      "Stop on 1+2 with repeated subterms: persist+refresh roundtrip",
+      `Quick,
+      () => {
+        let program = {|
+debug eval($e) in
+debug stop(1 + 2) in
+1 + 2 + 3 + (1 + 2 + 3 + (1 + 2 + 3))|};
+        let exp = parse_exp(program) |> elaborate;
+        let rec loop = (n, exp) =>
+          if (n <= 0) {
+            ();
+          } else {
+            switch (step_status(exp)) {
+            | AutoStep(step) =>
+              switch (EvaluatorStep.take_step(step)) {
+              | None => ()
+              | Some(exp') => loop(n - 1, exp')
+              }
+            | AvailableSteps(steps) =>
+              List.iter(
+                (step: EvaluatorStep.step) => {
+                  let persistent = EvaluatorStep.persist(step);
+                  switch (
+                    EvaluatorStep.refresh_step(
+                      ~settings=CoreSettings.on,
+                      exp,
+                      Environment.empty,
+                      persistent,
+                    )
+                  ) {
+                  | Some(_) => ()
+                  | None =>
+                    Alcotest.fail("refresh_step returned None after persist")
+                  };
+                  ();
+                },
+                steps,
+              );
+              switch (steps) {
+              | [] => ()
+              | [step, ..._] =>
+                switch (EvaluatorStep.take_step(step)) {
+                | None => ()
+                | Some(exp') => loop(n - 1, exp')
+                }
+              };
+            };
+          };
+        loop(500, exp);
+        check(bool, "no failure", true, true);
+      },
+    ),
+    test_case(
+      "Stop filter on fac overrides settings-based pre-filter (regression)",
+      `Quick,
+      () => {
+        let program = {|
+debug hide($e) in
+let fac : Int -> Int =
+  fun n ->
+    if n < 2 then 1 else n * fac(n - 1)
+in
+debug stop(fac($v)) in
+fac(3)|};
+        let exp = parse_exp(program) |> elaborate;
+        /* CoreSettings.on has show_fixpoints=false, so FixUnwrap steps are
+           pre-filtered out of the default trace. The user-written
+           debug stop(fac($v)) must still produce a visible pause at each
+           fac(v) redex (fac(3), fac(2), fac(1) -- the recursive base case
+           returns directly). Before user filters were allowed to override
+           settings-based pre-filtering in should_hide_eval_obj, the count
+           was 0 because the filter never got a chance to see the FixUnwrap
+           redexes. */
+        let steps = count_available_steps(~limit=500, exp, 0);
+        check(int, "expected 3 visible fac calls", 3, steps);
+      },
+    ),
+    test_case(
+      "No user filter: settings still hide FixUnwrap (no regression)",
+      `Quick,
+      () => {
+        let program = {|
+let fac : Int -> Int =
+  fun n ->
+    if n < 2 then 1 else n * fac(n - 1)
+in
+fac(3)|};
+        let exp = parse_exp(program) |> elaborate;
+        /* With no user filter, CoreSettings.on still silences FixUnwrap and
+           other pre-filtered step kinds. Evaluation should reach a final
+           value via auto-stepping; the few visible pauses (if any) come from
+           non-pre-filtered kinds (e.g. user-visible arithmetic). The key
+           guarantee: the fac unrolls do NOT become visible just because we
+           reordered the matches/settings check. */
+        let steps = count_available_steps(~limit=2000, exp, 0);
+        check(
+          bool,
+          "evaluation terminates without runaway visible steps",
+          true,
+          steps < 50,
+        );
+      },
+    ),
+    test_case(
+      "step filter does NOT surface unrenderable Asc steps (regression)",
+      `Quick,
+      () => {
+        /* When `step($v + $v)` matches an arithmetic expression that's
+           wrapped in ascriptions, the Asc transitions inside should not
+           surface as visible (clickable) steps under default settings
+           (show_ascriptions=false). Otherwise the stepper UI shows zero
+           green boxes for them — they have no surface piece to draw on.
+           The user's `step` filter should still pause at the arithmetic
+           itself, but auto-take the surrounding Asc transitions. */
+        let program = {|
+debug step($v + $v) in
+let f : Int -> Int = fun x -> x + x in
+f(3)|};
+        let exp = parse_exp(program) |> elaborate;
+        let steps = steps_until_available(~limit=200, exp);
+        let kinds =
+          List.map(
+            s =>
+              Transition.stepper_justification(
+                EvaluatorStep.get_step_kind(s),
+              ),
+            steps,
+          );
+        let has_asc = List.exists(k => k == "ascription transition", kinds);
+        check(
+          bool,
+          "no unrenderable Asc step should be exposed as AvailableStep",
+          false,
+          has_asc,
+        );
+      },
+    ),
+    test_case(
+      "Stop filter on fac: persist then refresh_step roundtrip",
+      `Quick,
+      () => {
+        let program = {|
+debug eval($e) in
+let fac : Int -> Int =
+  fun n ->
+    if n < 2 then 1 else n * fac(n - 1)
+in
+debug stop(fac($v)) in
+fac(3)|};
+        let exp = parse_exp(program) |> elaborate;
+        let rec loop = (n, exp) =>
+          if (n <= 0) {
+            ();
+          } else {
+            switch (step_status(exp)) {
+            | AutoStep(step) =>
+              switch (EvaluatorStep.take_step(step)) {
+              | None => ()
+              | Some(exp') => loop(n - 1, exp')
+              }
+            | AvailableSteps(steps) =>
+              List.iter(
+                (step: EvaluatorStep.step) => {
+                  let persistent = EvaluatorStep.persist(step);
+                  switch (
+                    EvaluatorStep.refresh_step(
+                      ~settings=CoreSettings.on,
+                      exp,
+                      Environment.empty,
+                      persistent,
+                    )
+                  ) {
+                  | Some(_) => ()
+                  | None =>
+                    Alcotest.fail("refresh_step returned None after persist")
+                  };
+                  ();
+                },
+                steps,
+              )
+            };
+          };
+        loop(500, exp);
+        check(bool, "no failure", true, true);
+      },
+    ),
+    test_case(
+      "Stop filter on fac: persist each step and take manual steps",
+      `Quick,
+      () => {
+        let program = {|
+debug eval($e) in
+let fac : Int -> Int =
+  fun n ->
+    if n < 2 then 1 else n * fac(n - 1)
+in
+debug stop(fac($v)) in
+fac(3)|};
+        let exp = parse_exp(program) |> elaborate;
+        let rec loop = (n, exp) =>
+          if (n <= 0) {
+            ();
+          } else {
+            switch (step_status(exp)) {
+            | AutoStep(step) =>
+              let _ = EvaluatorStep.persist(step);
+              switch (EvaluatorStep.take_step(step)) {
+              | None => ()
+              | Some(exp') => loop(n - 1, exp')
+              };
+            | AvailableSteps(steps) =>
+              List.iter(
+                (step: EvaluatorStep.step) => {
+                  let _ = EvaluatorStep.persist(step);
+                  ();
+                },
+                steps,
+              );
+              // Simulate user clicking the first available step.
+              switch (steps) {
+              | [] => ()
+              | [step, ..._] =>
+                switch (EvaluatorStep.take_step(step)) {
+                | None => ()
+                | Some(exp') => loop(n - 1, exp')
+                }
+              };
+            };
+          };
+        loop(500, exp);
+        check(bool, "no failure during manual stepping", true, true);
+      },
+    ),
     test_case(
       "Simple arithmetic",
       `Quick,
```

</details>

<details>
<summary><code>test/evaluator/Test_StepperBase.re</code> · stepper-view harness + fac stop program</summary>

<!-- changetour:hunk file=test/evaluator/Test_StepperBase.re level=2 baseBlob=e418a2b3b485bdba1ce3a06cc130c5f00dcebe68 -->

```diff
@@ -260,6 +260,112 @@ let run_step_chain =
   };
 };
 
+let fac_stop_program = {|
+debug eval($e) in
+let fac : Int -> Int =
+  fun n ->
+    if n < 2 then 1 else n * fac(n - 1)
+in
+debug stop(fac($v)) in
+fac(3)|};
+
+let stepper_ctx =
+  SemanticCtx.of_ctx_and_env(Builtins.ctx_init(None), Builtins.closure_env);
+
+let calculate_stepper_view = (~fresh, elab, model) =>
+  Web.StepperView.Update.calculate(
+    ~settings=Calc.OldValue(CoreSettings.on),
+    ~ctx=Calc.OldValue(stepper_ctx),
+    fresh ? Calc.NewValue(elab) : Calc.OldValue(elab),
+    model,
+  );
+
+let update_stepper_view = (action, model) =>
+  Web.StepperView.Update.update(
+    ~settings=Web.Settings.Model.init,
+    action,
+    model,
+  ).
+    model;
+
+let missing_available_steps = (m: Web.MissingStep.Model.t) =>
+  m.next_steps
+  |> Calc.get_saved_exc(~print="expected calculated missing step")
+  |> (
+    fun
+    | EvaluatorStep.AutoStep(_) => []
+    | EvaluatorStep.AvailableSteps(steps) => steps
+  );
+
+let action_at_deepest_available =
+    (model: StepperBase.step_model): option(StepperBase.step_action) => {
+  let action_at_current = (model: StepperBase.step_model) =>
+    switch (model.step_kind) {
+    | StepperBase.MissingStep(m) =>
+      switch (missing_available_steps(m)) {
+      | [_, ..._] => Some(StepperBase.StepForward(0))
+      | [] => None
+      }
+    | _ => None
+    };
+
+  let rec loop = (model: StepperBase.step_model) =>
+    switch (model.next_step) {
+    | Some(next) =>
+      switch (loop(next)) {
+      | Some(action) => Some(StepperBase.NextStep(action))
+      | None => action_at_current(model)
+      }
+    | None => action_at_current(model)
+    };
+
+  loop(model);
+};
+
+let apply_deepest_available_action = (model: Web.StepperView.Model.t) =>
+  switch (action_at_deepest_available(model.root)) {
+  | Some(action) => update_stepper_view(action, model)
+  | None => Alcotest.fail("expected an available step")
+  };
+
+let stepper_pure_exp = elab =>
+  elab |> Substitution.in_exp(Builtins.env_init) |> Exp.replace_all_ids;
+
+let step_status_with_stepper_env = exp =>
+  EvaluatorStep.get_status(
+    ~settings=CoreSettings.on,
+    exp,
+    Builtins.closure_env,
+  );
+
+let rec count_available_steps_with_env = (~limit, ~exp, ~count) =>
+  if (limit <= 0) {
+    Alcotest.fail("step count exceeded limit");
+  } else {
+    switch (step_status_with_stepper_env(exp)) {
+    | AutoStep(step) =>
+      switch (EvaluatorStep.take_step(step)) {
+      | None => count
+      | Some(exp') =>
+        count_available_steps_with_env(~limit=limit - 1, ~exp=exp', ~count)
+      }
+    | AvailableSteps(steps) =>
+      switch (steps) {
+      | [] => count
+      | [step, ..._] =>
+        switch (EvaluatorStep.take_step(step)) {
+        | None => count
+        | Some(exp') =>
+          count_available_steps_with_env(
+            ~limit=limit - 1,
+            ~exp=exp',
+            ~count=count + 1,
+          )
+        }
+      }
+    };
+  };
+
 let tests = (
   "StepperBase",
   [
```

</details>

<details open>
<summary><code>test/evaluator/Test_StepperBase.re</code> · nth_exp finds residue target; recursive stop does not overflow</summary>

<!-- changetour:hunk file=test/evaluator/Test_StepperBase.re level=2 baseBlob=e418a2b3b485bdba1ce3a06cc130c5f00dcebe68 -->

```diff
@@ -282,6 +388,76 @@ let tests = (
         );
       },
     ),
+    test_case(
+      "nth_exp can find residue filter target",
+      `Quick,
+      () => {
+        let target =
+          Exp.fresh(
+            Filter(
+              Residue(1, (FilterAction.Eval, FilterAction.All)),
+              Exp.fresh(Atom(Int(Bigint.of_int(18)))),
+            ),
+          );
+        let exp =
+          Exp.fresh(
+            Filter(
+              Filter({
+                act: (FilterAction.Eval, FilterAction.All),
+                pat: Exp.fresh(FilterSelector(FilterSelector.Exp)),
+                ids: IdTagged.IdTag.fresh(),
+              }),
+              target,
+            ),
+          );
+        check(
+          int,
+          "residue target index",
+          0,
+          ProofHacks.exp_idx(target, exp),
+        );
+        switch (ProofHacks.nth_exp(target, 0, exp)) {
+        | Some(found) =>
+          check(
+            bool,
+            "found residue target by id",
+            true,
+            Exp.rep_id(found) == Exp.rep_id(target),
+          )
+        | None => Alcotest.fail("expected nth_exp to find residue target")
+        };
+      },
+    ),
+    test_case(
+      "taking recursive stop step does not overflow",
+      `Quick,
+      () => {
+        let elab = fac_stop_program |> parse_exp |> elaborate;
+        let pure_count =
+          count_available_steps_with_env(
+            ~limit=1000,
+            ~exp=stepper_pure_exp(elab),
+            ~count=0,
+          );
+        check(int, "pure evaluator visible recursive stops", 3, pure_count);
+        let model =
+          Web.StepperView.Model.init
+          |> calculate_stepper_view(~fresh=true, elab);
+        let model =
+          model
+          |> apply_deepest_available_action
+          |> calculate_stepper_view(~fresh=false, elab);
+        let model =
+          model
+          |> apply_deepest_available_action
+          |> calculate_stepper_view(~fresh=false, elab);
+        let _model =
+          model
+          |> apply_deepest_available_action
+          |> calculate_stepper_view(~fresh=false, elab);
+        check(bool, "step calculate completed", true, true);
+      },
+    ),
     // ============================================================
     // SingleStep tests
     // ============================================================
```

</details>

New statics tests assert that every surface id of a `debug action(pat) in body` program lands in the info map — the property that motivated keeping an info entry for the `Ap` wrapper during resolution (otherwise the cursor between the action and its argument would have nothing to show in ExplainThis).

<details open>
<summary><code>test/statics/Test_Statics_Filter.re</code> · info_map_preserves_ids for all four actions and selector patterns</summary>

<!-- changetour:hunk file=test/statics/Test_Statics_Filter.re level=2 baseBlob=fe209a661c27744709193508e1f7baf898b6bc81 -->

```diff
@@ -0,0 +1,57 @@
+/* Tests for statics on `debug action(pat) in body` filter expressions.
+
+   The Filter form parses to `Filter(Ap(action, pat), body)` with `action`
+   resolving to a `FilterAction`. The Statics handler lifts this into
+   `Filter(Filter({act, pat, ids}), body)` for elaboration. While doing so
+   it must still write an info entry for the `Ap` wrapper itself —
+   otherwise the cursor between `action` and `(pat)` has no info to
+   resolve, and ExplainThis falls back to "Whitespace or Comment". */
+
+open Alcotest;
+open Test_Statics_Prelude;
+open Language;
+
+let collect_ids = (exp: Exp.t): list(Id.t) => {
+  let acc = ref([]);
+  let collect = (a: IdTagged.IdTag.t) => {
+    acc := a.ids @ acc^;
+    a;
+  };
+  let _ = Grammar.map_exp_annotation(collect, exp);
+  acc^;
+};
+
+let info_map_preserves_ids = (name, src) =>
+  test_case(
+    name,
+    `Quick,
+    () => {
+      let exp = parse_exp(src);
+      let m = statics(exp);
+      let missing =
+        collect_ids(exp)
+        |> List.filter(id =>
+             !Id.equal(id, Id.invalid)
+             && Option.is_none(Statics.Map.lookup(id, m))
+           );
+      Alcotest.(check(list(string)))(
+        src ++ " — every surface id appears in the info map",
+        [],
+        List.map(Id.show, missing),
+      );
+    },
+  );
+
+let tests = (
+  "Statics.Filter",
+  [
+    info_map_preserves_ids("hide", "debug hide(1) in 2"),
+    info_map_preserves_ids("eval", "debug eval(1) in 2"),
+    info_map_preserves_ids("step", "debug step(1) in 2"),
+    info_map_preserves_ids("stop", "debug stop(1) in 2"),
+    info_map_preserves_ids(
+      "hide with filter-selector pattern",
+      "debug hide($e) in 1 + 2",
+    ),
+  ],
+);
```

</details>

<details open>
<summary><code>test/statics/Test_Statics.re</code> · register Test_Statics_Filter</summary>

<!-- changetour:hunk file=test/statics/Test_Statics.re level=2 baseBlob=9c26a3645a163cb64e63fd935966c0c424c47044 -->

```diff
@@ -15,4 +15,5 @@ let tests =
     Test_Statics_Fixpoint.tests,
     Test_Statics_Properties.tests,
     Test_Statics_Parens.tests,
+    Test_Statics_Filter.tests,
   ];
```

</details>

## Miscellaneous

Mechanical fallout from the two new `exp_term` variants and the reshaped `stepper_filter_kind_t`: every exhaustive match over expressions gains `FilterAction`/`FilterSelector` cases (all treated as inert leaves), annotation-mapping and factory plumbing picks up the `ids` field and `Unresolved` case, and the menhir AST's QCheck generators/shrinkers drop the removed `filter_action` argument.

<details>
<summary><code>src/language/term/Grammar.re</code> · map_exp_annotation: new leaf cases</summary>

<!-- changetour:hunk file=src/language/term/Grammar.re level=2 baseBlob=4ea8e9c4742a8e75182c60b21068b8f017fc458b -->

```diff
@@ -206,6 +210,8 @@ let rec map_exp_annotation: type a b. (a => b, exp_t(a)) => exp_t(b) =
         | Dot(e1, e2) =>
           Dot(map_exp_annotation(f, e1), map_exp_annotation(f, e2))
         | Var(v) => Var(v)
+        | FilterAction(act) => FilterAction(act)
+        | FilterSelector(sel) => FilterSelector(sel)
         | Let(p, e1, e2) =>
           Let(
             map_pat_annotation(f, p),
```

</details>

<details>
<summary><code>src/language/term/Grammar.re</code> · map_stepper_filter_kind_annotation: Unresolved + ids</summary>

<!-- changetour:hunk file=src/language/term/Grammar.re level=2 baseBlob=4ea8e9c4742a8e75182c60b21068b8f017fc458b -->

```diff
@@ -498,10 +504,12 @@ and map_stepper_filter_kind_annotation:
  =
   (f, e) => {
     switch (e) {
+    | Unresolved(exp) => Unresolved(map_exp_annotation(f, exp))
     | Filter(filter) =>
       Filter({
         pat: map_exp_annotation(f, filter.pat),
         act: filter.act,
+        ids: f(filter.ids),
       })
     | Residue(i, act) => Residue(i, act)
     };
```

</details>

<details>
<summary><code>src/language/term/Grammar.re</code> · factory: filter_action / filter_selector constructors</summary>

<!-- changetour:hunk file=src/language/term/Grammar.re level=2 baseBlob=4ea8e9c4742a8e75182c60b21068b8f017fc458b -->

```diff
@@ -656,6 +664,14 @@ module Factory = (DefaultAnnotation: DefaultAnnotation) => {
       term: Var(v),
       annotation: default_annotation(ann),
     };
+    let filter_action = (~ann=?, act): exp_t(DefaultAnnotation.t) => {
+      term: FilterAction(act),
+      annotation: default_annotation(ann),
+    };
+    let filter_selector = (~ann=?, sel): exp_t(DefaultAnnotation.t) => {
+      term: FilterSelector(sel),
+      annotation: default_annotation(ann),
+    };
     let livelit_name = (~ann=?, s): exp_t(DefaultAnnotation.t) => {
       term: LivelitName(s),
       annotation: default_annotation(ann),
```

</details>

<details>
<summary><code>src/language/term/Grammar.re</code> · factory: filter_unresolved / filter / residue</summary>

<!-- changetour:hunk file=src/language/term/Grammar.re level=2 baseBlob=4ea8e9c4742a8e75182c60b21068b8f017fc458b -->

```diff
@@ -720,8 +736,24 @@ module Factory = (DefaultAnnotation: DefaultAnnotation) => {
       term: HintedTest(e, h),
       annotation: default_annotation(ann),
     };
-    let filter = (~ann=?, k, e): exp_t(DefaultAnnotation.t) => {
-      term: Filter(k, e),
+    let filter_unresolved = (~ann=?, p, e): exp_t(DefaultAnnotation.t) => {
+      term: Filter(Unresolved(p), e),
+      annotation: default_annotation(ann),
+    };
+    let filter = (~ann=?, ~act, ~pat, e): exp_t(DefaultAnnotation.t) => {
+      term:
+        Filter(
+          Filter({
+            act,
+            pat,
+            ids: default_annotation(None),
+          }),
+          e,
+        ),
+      annotation: default_annotation(ann),
+    };
+    let residue = (~ann=?, ~lvl, ~act, e): exp_t(DefaultAnnotation.t) => {
+      term: Filter(Residue(lvl, act), e),
       annotation: default_annotation(ann),
     };
     let closure = (~ann=?, env, e): exp_t(DefaultAnnotation.t) => {
```

</details>

<details>
<summary><code>src/language/term/Grammar.re</code> · StepperFilter.filter takes pat/act/ids</summary>

<!-- changetour:hunk file=src/language/term/Grammar.re level=2 baseBlob=4ea8e9c4742a8e75182c60b21068b8f017fc458b -->

```diff
@@ -1082,10 +1114,13 @@ module Factory = (DefaultAnnotation: DefaultAnnotation) => {
   };
 
   module StepperFilter = {
-    let filter = (f): stepper_filter_kind_t(DefaultAnnotation.t) => {
+    let filter =
+        (pat: exp_t('a), act: FilterAction.t, ids: DefaultAnnotation.t)
+        : stepper_filter_kind_t(DefaultAnnotation.t) => {
       Filter({
-        pat: map_exp_annotation(x => x, f.pat),
-        act: f.act,
+        pat: map_exp_annotation(x => x, pat),
+        act,
+        ids,
       });
     };
     let residue = (i, act): stepper_filter_kind_t(DefaultAnnotation.t) => {
```

</details>

<details>
<summary><code>src/language/term/TermBase.re</code> · any_map_term: new leaf cases</summary>

<!-- changetour:hunk file=src/language/term/TermBase.re level=2 baseBlob=2e7ea5efbfdac36021282479377a377d86562d10 -->

```diff
@@ -214,6 +214,8 @@ and Exp: {
         | ExplicitNonlabel
         | Deferral(_)
         | Var(_)
+        | FilterAction(_)
+        | FilterSelector(_)
         | LivelitName(_)
         | Undefined => term
         | MultiHole(things) => MultiHole(List.map(any_map_term, things))
```

</details>

<details>
<summary><code>src/language/term/TermBase.re</code> · StepperFilterKind.map: Unresolved + ids</summary>

<!-- changetour:hunk file=src/language/term/TermBase.re level=2 baseBlob=2e7ea5efbfdac36021282479377a377d86562d10 -->

```diff
@@ -779,10 +781,12 @@ and StepperFilterKind: {
 
   let map = (mapper, filter: t): t => {
     switch (filter) {
-    | Filter({act, pat}) =>
+    | Unresolved(exp) => Unresolved(mapper(exp))
+    | Filter({act, pat, ids}) =>
       Filter({
         act,
         pat: mapper(pat),
+        ids,
       })
     | Residue(idx, act) => Residue(idx, act)
     };
```

</details>

<details>
<summary><code>src/language/term/TermBase.re</code> · StepperFilterKind.map_term: Unresolved + ids</summary>

<!-- changetour:hunk file=src/language/term/TermBase.re level=2 baseBlob=2e7ea5efbfdac36021282479377a377d86562d10 -->

```diff
@@ -801,10 +805,12 @@ and StepperFilterKind: {
       Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
     (
       fun
-      | Filter({pat: e, act}) =>
+      | Unresolved(exp) => Unresolved(exp_map_term(exp))
+      | Filter({pat, act, ids}) =>
         Filter({
-          pat: exp_map_term(e),
+          pat: exp_map_term(pat),
           act,
+          ids,
         })
       | Residue(i, a) => Residue(i, a):
         t => t
```

</details>

<details>
<summary><code>src/language/term/Exp.re</code> · cls_of_term: new variants classed as Filter</summary>

<!-- changetour:hunk file=src/language/term/Exp.re level=2 baseBlob=ed3820a686d1a6a82645d02b920533b8f1774317 -->

```diff
@@ -143,7 +143,9 @@ let rec cls_of_term: type a. Grammar.exp_term(a) => cls =
   | LivelitName(_) => LivelitName
   | Asc(_) => Asc
   | Module(_) => Module
-  | ModuleExp(_) => ModuleExp;
+  | ModuleExp(_) => ModuleExp
+  | FilterAction(_) => Filter
+  | FilterSelector(_) => Filter;
 
 let show_cls: cls => string =
   fun
```

</details>

<details>
<summary><code>src/language/term/Exp.re</code> · is_fun: new leaf cases</summary>

<!-- changetour:hunk file=src/language/term/Exp.re level=2 baseBlob=ed3820a686d1a6a82645d02b920533b8f1774317 -->

```diff
@@ -285,7 +287,9 @@ let rec is_fun = (e: t) => {
   | LivelitName(_)
   | Constructor(_)
   | Module(_)
-  | ModuleExp(_) => false
+  | ModuleExp(_)
+  | FilterAction(_)
+  | FilterSelector(_) => false
   };
 };
 
```

</details>

<details>
<summary><code>src/language/term/Exp.re</code> · is_tuple_of_functions: new leaf cases</summary>

<!-- changetour:hunk file=src/language/term/Exp.re level=2 baseBlob=ed3820a686d1a6a82645d02b920533b8f1774317 -->

```diff
@@ -354,7 +358,9 @@ let rec is_tuple_of_functions = (e: t) =>
     | LivelitName(_)
     | Constructor(_)
     | Module(_)
-    | ModuleExp(_) => false
+    | ModuleExp(_)
+    | FilterAction(_)
+    | FilterSelector(_) => false
     }
   );
 
```

</details>

<details>
<summary><code>src/language/term/Exp.re</code> · get_num_of_functions: new leaf cases</summary>

<!-- changetour:hunk file=src/language/term/Exp.re level=2 baseBlob=ed3820a686d1a6a82645d02b920533b8f1774317 -->

```diff
@@ -422,7 +428,9 @@ let rec get_num_of_functions = (e: t) =>
     | Constructor(_)
     | Module(_)
     | ModuleExp(_)
-    | DrvQuote(_) => None
+    | DrvQuote(_)
+    | FilterAction(_)
+    | FilterSelector(_) => None
     };
   };
 
```

</details>

<details>
<summary><code>src/language/term/Abbreviate.re</code> · abbreviate_exp: new leaf cases</summary>

<!-- changetour:hunk file=src/language/term/Abbreviate.re level=2 baseBlob=296d586440c4e1a8cc2dc6f4c00137167809baf7 -->

```diff
@@ -563,6 +563,8 @@ let rec abbreviate_exp = (exp: Exp.t): Exp.t => {
           Atom(String(str));
         };
       | DrvQuote(_, _) => Invalid("<drv term>")
+      | FilterAction(act) => FilterAction(act)
+      | FilterSelector(sel) => FilterSelector(sel)
       | Var(v) => Var(abbreviate_str(available^, v))
       | Label(v) =>
         switch (abbreviate_label(v)) {
```

</details>

<details>
<summary><code>src/language/dynamics/DHExp.re</code> · ty_subst: new leaf cases</summary>

<!-- changetour:hunk file=src/language/dynamics/DHExp.re level=2 baseBlob=cc9eb8023ff844dae668f00b4a53ead5ff66c6f1 -->

```diff
@@ -108,6 +108,8 @@ let ty_subst = (s: Typ.t, tpat: TPat.t, exp: t): t => {
           | LivelitName(_)
           | DynamicErrorHole(_)
           | Filter(_)
+          | FilterAction(_)
+          | FilterSelector(_)
           | If(_)
           | EmptyHole
           | Invalid(_)
```

</details>

<details>
<summary><code>src/language/dynamics/DHExp.re</code> · ty_comparable: new leaf cases</summary>

<!-- changetour:hunk file=src/language/dynamics/DHExp.re level=2 baseBlob=cc9eb8023ff844dae668f00b4a53ead5ff66c6f1 -->

```diff
@@ -166,7 +168,9 @@ let rec ty_comparable = (d1, d2) => {
   | (Fun(_), _)
   | (BuiltinFun(_), _)
   | (TypFun(_), _)
-  | (TupleExtension(_), _) => false
+  | (TupleExtension(_), _)
+  | (FilterAction(_), _)
+  | (FilterSelector(_), _) => false
   | (Parens(d1), _) => ty_comparable(d1, d2)
   | (_, Parens(d2)) => ty_comparable(d1, d2)
   | (Projector(_, d1), _) => ty_comparable(d1, d2)
```

</details>

<details>
<summary><code>src/language/dynamics/DHExp.re</code> · poly_equal: new leaf cases</summary>

<!-- changetour:hunk file=src/language/dynamics/DHExp.re level=2 baseBlob=cc9eb8023ff844dae668f00b4a53ead5ff66c6f1 -->

```diff
@@ -271,7 +275,9 @@ let rec poly_equal = (d1, d2): option(bool) => {
   | (Fun(_), _)
   | (TypFun(_), _)
   | (Use(_), _)
-  | (BuiltinFun(_), _) => None
+  | (BuiltinFun(_), _)
+  | (FilterAction(_), _)
+  | (FilterSelector(_), _) => None
 
   // Wrapping forms: just look through them
   | (Parens(d1), _) => poly_equal(d1, d2)
```

</details>

<details>
<summary><code>src/language/dynamics/Substitution.re</code> · in_exp: new leaf cases</summary>

<!-- changetour:hunk file=src/language/dynamics/Substitution.re level=2 baseBlob=647649c8ff5e94c3164e5626578fe0cdf4219915 -->

```diff
@@ -96,6 +96,8 @@ let rec in_exp = (env: Environment.t(Exp.t), exp: Exp.t) =>
         | Test(_)
         | HintedTest(_)
         | Filter(_)
+        | FilterAction(_)
+        | FilterSelector(_)
         | Parens(_)
         | Projector(_)
         | Cons(_)
```

</details>

<details>
<summary><code>src/language/dynamics/transition/Transition.re</code> · transition: new variants are Indet</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/Transition.re level=2 baseBlob=3f0ab4db396e6e7e09b57108d246279919f4de20 -->

```diff
@@ -814,6 +814,8 @@ module Transition = (EV: EV_MODE) => {
       let. _ = otherwise(env, d);
       Indet;
     | Atom(_)
+    | FilterAction(_)
+    | FilterSelector(_)
     | LivelitName(_)
     | Label(_)
     | ExplicitNonlabel
```

</details>

<details>
<summary><code>src/language/dynamics/transition/Ascriptions.re</code> · ascription transition: new leaf cases</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/Ascriptions.re level=2 baseBlob=8d1f399dd75b6bcae1bda4dd484ebc900ebd325f -->

```diff
@@ -273,6 +273,8 @@ let rec transition = (~recursive=false, d: DHExp.t): option(DHExp.t) => {
     | (Label(_), _)
     | (ExplicitNonlabel, _)
     | (Var(_), _)
+    | (FilterAction(_), _)
+    | (FilterSelector(_), _)
     | (Ap(_), _)
     | (DeferredAp(_), _)
     | (Deferral(_), _)
```

</details>

<details>
<summary><code>src/language/dynamics/transition/Unboxing.re</code> · unbox: new leaf cases</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/Unboxing.re level=2 baseBlob=4ee617f4199ad966d75cd0006e1160e18f80f39b -->

```diff
@@ -235,6 +235,8 @@ let rec unbox: type a. (unbox_request(a), DHExp.t) => unboxed(a) =
         Invalid(_) | Undefined | EmptyHole | MultiHole(_) | DynamicErrorHole(_) |
         ExplicitNonlabel |
         Var(_) |
+        FilterAction(_) |
+        FilterSelector(_) |
         Let(_) |
         Theorem(_) |
         Forall(_) |
```

</details>

<details>
<summary><code>src/language/proof/ProofHacks.re</code> · replace_exp: new leaf cases</summary>

<!-- changetour:hunk file=src/language/proof/ProofHacks.re level=2 baseBlob=a77134f0ee5a3f306ec78d931b03d39e2b443019 -->

```diff
@@ -462,6 +479,8 @@ let rec replace_exp =
         | ProofObject(_)
         | Asc(_, _)
         | ExplicitNonlabel
+        | FilterAction(_)
+        | FilterSelector(_)
         | Module(_)
         | ModuleExp(_) => continue(exp)
         };
```

</details>

<details>
<summary><code>src/menhirParser/AST.re</code> · qcheck gen: Filter loses action argument</summary>

<!-- changetour:hunk file=src/menhirParser/AST.re level=2 baseBlob=cbba2ceb101656668dd6b9badc523f67823ce889 -->

```diff
@@ -440,10 +466,9 @@ let rec gen_exp_sized = (~minimal_idents: bool, n: int): QCheck.Gen.t(exp) => {
             FixF(p, e);
           },
           {
-            let* fa = gen_filter_action;
             let* e1 = self((n - 1) / 2);
             let+ e2 = self((n - 1) / 2);
-            Filter(fa, e1, e2);
+            Filter(e1, e2);
           },
           {
             let* e1 = self((n - 1) / 2);
```

</details>

<details>
<summary><code>src/menhirParser/AST.re</code> · shrink: new leaf cases</summary>

<!-- changetour:hunk file=src/menhirParser/AST.re level=2 baseBlob=cbba2ceb101656668dd6b9badc523f67823ce889 -->

```diff
@@ -717,6 +742,8 @@ let rec shrink_exp: QCheck.Shrink.t(exp) =
             | _ => Iter.empty
             }
           )
+        | FilterAction(_) => Iter.empty
+        | FilterSelector(_) => Iter.empty
         | Var(x) =>
           return(TupleExp([]))
           <+> (shrink_non_empty_string(x) >|= ((x: string) => Var(x))) // TODO This isn't great for vars
```

</details>

<details>
<summary><code>src/menhirParser/AST.re</code> · shrink Filter without action argument</summary>

<!-- changetour:hunk file=src/menhirParser/AST.re level=2 baseBlob=cbba2ceb101656668dd6b9badc523f67823ce889 -->

```diff
@@ -942,17 +969,17 @@ let rec shrink_exp: QCheck.Shrink.t(exp) =
             let* shrunk = shrink_typ(t);
             return(Asc(e, shrunk));
           }
-        | Filter(fa, e1, e2) =>
+        | Filter(e1, e2) =>
           {
             of_list([e1, e2]);
           }
           <+> {
             let* shrunk = shrink_exp(e1);
-            return(Filter(fa, shrunk, e2));
+            return(Filter(shrunk, e2));
           }
           <+> {
             let* shrunk = shrink_exp(e2);
-            return(Filter(fa, e1, shrunk));
+            return(Filter(e1, shrunk));
           }
         | Seq(e1, e2) =>
           {
```

</details>

<details>
<summary><code>src/haz3lcore/CompositionCore/HighLevelNodeMap.re</code> · children_of: new leaf cases</summary>

<!-- changetour:hunk file=src/haz3lcore/CompositionCore/HighLevelNodeMap.re level=2 baseBlob=a016b735a4772843ca51b7bda816fc6934e258f2 -->

```diff
@@ -117,7 +117,9 @@ module Utils = {
     | Forall(_, _)
     | Projector(_, _)
     | Var(_)
-    | Module(_) => []
+    | Module(_)
+    | FilterAction(_)
+    | FilterSelector(_) => []
     | ModuleExp(_, def, body) => [def, body]
     | DrvQuote(_) => []
     };
```

</details>

<details>
<summary><code>src/haz3lcore/pretty/ExpToSegment.re</code> · external_precedence: new leaves at max</summary>

<!-- changetour:hunk file=src/haz3lcore/pretty/ExpToSegment.re level=2 baseBlob=25767daf953032cfbcb06a72a389c805b5b7768a -->

```diff
@@ -123,7 +123,9 @@ let rec external_precedence = (exp: Exp.t): Precedence.t => {
   | Label(_)
   | Constructor(_)
   | LivelitName(_)
-  | TupLabel(_) => Precedence.max
+  | TupLabel(_)
+  | FilterAction(_)
+  | FilterSelector(_) => Precedence.max
 
   // Same goes for forms which are already surrounded
   | Parens(_)
```

</details>

<details>
<summary><code>src/haz3lcore/pretty/ExpToSegment.re</code> · parenthesize: new variants indivisible</summary>

<!-- changetour:hunk file=src/haz3lcore/pretty/ExpToSegment.re level=2 baseBlob=25767daf953032cfbcb06a72a389c805b5b7768a -->

```diff
@@ -343,6 +345,8 @@ let rec parenthesize =
   | Invalid(_)
   | Atom(_)
   | DrvQuote(_)
+  | FilterAction(_)
+  | FilterSelector(_)
   | EmptyHole
   | LivelitName(_)
   //| Constructor(_) // Not indivisible because of the type annotation!
```

</details>

<details>
<summary><code>src/web/app/editors/EditorUtil.re</code> · append_exp: new leaf cases</summary>

<!-- changetour:hunk file=src/web/app/editors/EditorUtil.re level=2 baseBlob=7e090a4d7aa9dbe0af082b57d6190f26484708e1 -->

```diff
@@ -24,6 +24,8 @@ let rec append_exp = (e1: Language.Exp.t, e2: Language.Exp.t): Language.Exp.t =>
   | Deferral(_)
   | Atom(_)
   | DrvQuote(_)
+  | FilterAction(_)
+  | FilterSelector(_)
   | ListLit(_)
   | TupleExtension(_)
   | ExplicitNonlabel
```

</details>

<details>
<summary><code>src/web/exercises/CodeExercise.re</code> · append_exp: new leaf cases</summary>

<!-- changetour:hunk file=src/web/exercises/CodeExercise.re level=2 baseBlob=a87e01d7be8bb06b38b70affae3177417e54c847 -->

```diff
@@ -689,6 +686,8 @@ let rec append_exp = (e1: Language.Exp.t, e2: Language.Exp.t): Language.Exp.t =>
   | Deferral(_)
   | Atom(_)
   | DrvQuote(_)
+  | FilterAction(_)
+  | FilterSelector(_)
   | ListLit(_)
   | TupleExtension(_)
   | Constructor(_)
```

</details>

<details>
<summary><code>src/web/exercises/SyntaxTest.re</code> · find_fn: new leaf cases</summary>

<!-- changetour:hunk file=src/web/exercises/SyntaxTest.re level=2 baseBlob=6fed159be4a2b4f55524fe113026e93eec3433bd -->

```diff
@@ -160,6 +160,8 @@ let rec find_fn = (name: string, uexp: Exp.t, l: list(Exp.t)): list(Exp.t) => {
   | Constructor(_)
   | Undefined
   | BuiltinFun(_)
+  | FilterAction(_)
+  | FilterSelector(_)
   | Var(_) => l
   };
 };
```

</details>

<details>
<summary><code>src/web/exercises/SyntaxTest.re</code> · var_mention: new leaf cases</summary>

<!-- changetour:hunk file=src/web/exercises/SyntaxTest.re level=2 baseBlob=6fed159be4a2b4f55524fe113026e93eec3433bd -->

```diff
@@ -216,6 +218,8 @@ let rec var_mention = (name: string, uexp: Exp.t): bool => {
   | Invalid(_)
   | MultiHole(_)
   | Atom(_)
+  | FilterAction(_)
+  | FilterSelector(_)
   | Label(_)
   | DrvQuote(_)
   | ExplicitNonlabel
```

</details>

<details>
<summary><code>src/web/exercises/SyntaxTest.re</code> · var_applied: new leaf cases</summary>

<!-- changetour:hunk file=src/web/exercises/SyntaxTest.re level=2 baseBlob=6fed159be4a2b4f55524fe113026e93eec3433bd -->

```diff
@@ -306,6 +310,8 @@ let rec var_applied = (name: string, uexp: Exp.t): bool => {
   | Invalid(_)
   | MultiHole(_)
   | Atom(_)
+  | FilterAction(_)
+  | FilterSelector(_)
   | Label(_)
   | DrvQuote(_)
   | ExplicitNonlabel
```

</details>

<details>
<summary><code>src/web/exercises/SyntaxTest.re</code> · tail_check: new leaf cases</summary>

<!-- changetour:hunk file=src/web/exercises/SyntaxTest.re level=2 baseBlob=6fed159be4a2b4f55524fe113026e93eec3433bd -->

```diff
@@ -426,6 +432,8 @@ let rec tail_check = (name: string, uexp: Exp.t): bool => {
   | MultiHole(_)
   | DynamicErrorHole(_)
   | Atom(_)
+  | FilterAction(_)
+  | FilterSelector(_)
   | Label(_)
   | DrvQuote(_)
   | Constructor(_)
```

</details>

<details>
<summary><code>src/web/exercises/Tutorial.re</code> · append_exp: new variants sequence</summary>

<!-- changetour:hunk file=src/web/exercises/Tutorial.re level=2 baseBlob=9ad096e098b9942c385cf2711848a718e1c189c1 -->

```diff
@@ -323,7 +323,9 @@ let rec append_exp = (e1: Language.Exp.t, e2: Language.Exp.t): Language.Exp.t =>
   | Asc(_)
   | ProofObject(_)
   | Forall(_)
-  | Match(_) => {
+  | Match(_)
+  | FilterAction(_)
+  | FilterSelector(_) => {
       term: Seq(e1, e2),
       annotation: Language.IdTagged.IdTag.fresh(),
     }
```

</details>
