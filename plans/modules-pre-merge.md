# Modules: Pre-Merge TODO

Work items to complete before merging the modules branch into dev.

See `docs/modules.md` for documentation of what's implemented.
See `plans/modules-future.md` for post-merge future work.

---

## 1. Menhir Parser: Multi-Item Modules — DONE

Fixed via `%nonassoc MOD_ITEM_EXP` precedence in Parser.mly.
See `plans/menhir-module-semicolon.md` for full writeup.

---

## 2. `module` Keyword + Capitalized Module Names — DONE

### Overview

The `module` keyword and support for capitalized module names are complete.

Users can write:

```hazel
module M = { let x = 1; let y = 2 } in M.x + M.y
```

With type annotations (MPat Asc form):

```hazel
module M : { let x : Int } = { let x = 42 } in M.x
module M : (x=Int) = { let x = 1 } in M.x
```

And inside module bodies:

```hazel
{
  module Inner = { let z = 42 };
  let result = Inner.z
}
```

Both capitalized and lowercase names are accepted after `module`.

### The problem with `let` + capitalized names

In `let M = { ... } in M.x`:
1. Pattern `M`: `Token.is_ctr("M")` is true → parsed as
   `Constructor("M", None)` → statics treats as pattern match, not binding.
2. Expression `M`: same thing → `Constructor("M", None)` → statics calls
   `Ctx.lookup_ctr`, not `Ctx.lookup_var` → lookup fails.

The `module` keyword solves the binding side with a new sort (MPat) that
treats all identifiers as variable names. The expression side is solved
with a statics fallback: if a capitalized name isn't a known constructor,
check if it's a variable.

---

### 2.1 New MPat Sort — DONE

A restricted sort for module name patterns. Supports names (any case)
and optional type annotation (MPat.Asc).

**Sort.re**: Add `MPat` to the sort type.

```reason
type t = Any | Pat | Typ | TPat | Rul | Exp | Mod | Sig | MPat;
```

**Grammar.re / TermBase**: Add term type.

```reason
type mpat_term('a) =
  | Invalid(string)
  | EmptyHole
  | MultiHole(list(any_t('a)))
  | Var(Var.t);

type mpat_t('a) = Annotated.t(mpat_term('a), 'a);
```

Note: `Asc` variant was added for `module M : T = ...` syntax.
MPatTypeann form uses `mk_infix(":", MPat, ~l=MPat, ~r=Typ, P.asc)`.
Required adding Typ sort transition handling to `remold_mpat`/`remold_mpat_uni`
in Segment.re (was missing, causing multi-token types like `Int -> Int` to fail).

**Grammar.re any_t**: Add `MPat(mpat_t('a))` variant to `any_t`.

**MakeTerm.re**: Add `mpat` parsing function.

```reason
and mpat_term: unsorted => TermBase.MPat.term = {
  fun
  | Op(tiles) =>
    switch (tiles) {
    | ([(_id, ([t], []))], []) when Token.is_var(t) || Token.is_ctr(t) =>
      ret(Var(t))
    | ([(_id, ([t], []))], []) when Token.is_empty_hole(t) =>
      ret(EmptyHole)
    | _ => ret(hole(tm))
    }
  | _ => ret(hole(tm))
}
```

Key: both `is_var` and `is_ctr` tokens map to `Var`. This is the whole
point — in MPat sort, capitalized names are bindings, not constructors.

**Info.re**: Add `InfoMPat` variant (minimal).

```reason
type mpat_ = {
  id: Id.t,
  term: MPat.t,
  cls: Cls.t,
  sort: Sort.t,
  ctx: Ctx.t,
  ancestors,
};

type t = ... | InfoMPat(mpat_) | ...;
```

**Cls.re**: Add `MPat(MPat.cls)` variant.

**MPat.re** (new file): Module pattern term, parallel to Mod.re/Sig.re.

```reason
type cls = Invalid | EmptyHole | MultiHole | Var;

let cls_of_term = fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | Var(_) => Var;

let show_cls = fun
  | Invalid => "Invalid module name"
  | EmptyHole => "Empty module name hole"
  | MultiHole => "Incomplete module name"
  | Var => "Module name";
```

**Statics.re**: Handle MPat in the main statics traversal. Adds the
variable name to context (like Pat.Var does). When the module keyword
form is encountered, the MPat's name gets bound.

**Other files to update with MPat variant**: Anywhere that matches on
Sort.t or any_t exhaustively. Key ones:
- Sort.re (to_string, etc.)
- Grammar.re (map functions)
- MakeTerm.re (any_of, the main go function)
- Segment.re (remolding — add remold_mpat, can be minimal)
- Info.re (all helper functions that match on Info.t)
- Statics.re
- Cls.re
- CursorInspector.re (show "Module name" for MPat)
- ErrorPrint.re, ChatLSP.re (match on Info.t)

---

### 2.2 `module` Keyword Forms — DONE

Two compound forms, one for each context:

**Form.re**: Add to `compound_form` enum and `get` function.

```reason
(* Enum *)
| ModuleExp    (* module M = e in body — Exp context *)
| ModuleMod    (* module M = e — Mod context *)

(* Definitions *)
| ModuleExp => mk_pre_c'(L, ["module", "=", "in"], P.let_, Exp, [MPat, Exp], Exp)
| ModuleMod => mk_pre_c'(L, ["module", "="], P.let_, Mod, [MPat], Exp)
```

Both use `mk_pre_c'` (heterogeneous prefix) because:
- ModuleExp: output=Exp, tile operands=[MPat, Exp], body=Exp
- ModuleMod: output=Mod, tile operands=[MPat], body=Exp

These parallel Let/ModLet:
- `Let`:    `mk_pre_c(L, ["let", "=", "in"], P.let_, Exp, [Pat, Exp])`
- `ModLet`: `mk_pre_c'(L, ["let", "="], P.let_, Mod, [Pat], Exp)`

**Precedence**: Same as `let_` (45). Same binding behavior.

**Keyword registration**: "module" will be a new keyword token. It only
expands in Exp and Mod contexts (via sorted_expansions from the nib sorts).

---

### 2.3 New Term Variants — DONE

Distinct term variants preserved (not desugared in MakeTerm) for cursor
inspector and ExplainThis support.

**Grammar.re exp_term**: Add `ModuleExp` variant.

```reason
| ModuleExp(mpat_t('a), exp_t('a), exp_t('a))
  (* module M = def in body *)
```

**Grammar.re mod_term**: Add `ModuleMod` variant.

```reason
| ModuleMod(mpat_t('a), exp_t('a))
  (* module M = def — inside a module body *)
```

**Exp.re cls**: Add `ModuleExp`.
**Mod.re cls**: Add `ModuleMod`.

**MakeTerm.re exp_term**: Match the `module` form.

```reason
| (["module", "=", "in"], [MPat(mp), Exp(def)]) =>
  ModuleExp(mp, def, r)   (* r is the body, like Let *)
```

**MakeTerm.re mod_term**: Match the `module` form in Mod context.

```reason
| Pre(([(_id, (["module", "="], [MPat(mp)]))], []), Exp(def)) =>
  ret(ModuleMod(mp, def))
```

---

### 2.4 Expansion in Statics — DONE

Expanded `ModuleExp` and `ModuleMod` to their `let` equivalents during
statics, following the same pattern as `Module(items)` → nested lets.

**Helper function** (in ExpandModule.re or a new utility):

```reason
(* Convert MPat to Pat for expansion *)
let mpat_to_pat = (mp: MPat.t): Pat.t =>
  switch (mp.term) {
  | Var(name) => IdTagged.fast_copy(MPat.rep_id(mp), Pat.fresh(Var(name)))
  | EmptyHole => Pat.fresh(EmptyHole)
  | Invalid(s) => Pat.fresh(Invalid(s))
  | MultiHole(_) => Pat.fresh(EmptyHole)
  };
```

Note: `fast_copy` preserves the MPat's ID on the Pat so cursor inspector
can identify the source.

**Statics.re ModuleExp case**:

```reason
| ModuleExp(mp, def, body) =>
  (* 1. Type-check the MPat (adds binding to context) *)
  let (mp_info, m) = go_mpat(mp, m);
  (* 2. Expand to Let for type-checking *)
  let pat = mpat_to_pat(mp);
  let expanded = IdTagged.fast_copy(
    Exp.rep_id(e),
    Exp.fresh(Let(pat, def, body))
  );
  let (expanded_info, m) = go(expanded, m);
  (* 3. Override cls to show "Module binding" *)
  let m = override_cls(Exp.rep_id(e), Cls.Exp(ModuleExp), m);
  add(~self=expanded_info.self, ~co_ctx=expanded_info.co_ctx, m);
```

The pattern mirrors how `Module(items)` works:
1. Expand to existing form (Let)
2. Type-check the expanded form (reuses all Let statics)
3. Override cls for cursor inspector
4. ID preservation via `fast_copy`

**Statics.re ModuleMod case** (in Mod statics):

Similar to ModLet handling. Expand `ModuleMod(mp, def)` to
`ModLet(mpat_to_pat(mp), def)` and type-check.

**Elaborator**: Should see the expanded Let/ModLet, not the ModuleExp/
ModuleMod. The expansion happens before elaboration (same as Module).

**Evaluator/Dynamics**: Never sees ModuleExp/ModuleMod — already expanded.
Add a fallback case (`| ModuleExp(_) | ModuleMod(_) => Indet`) for safety,
same pattern as `Module(items)`.

---

### 2.5 Capitalized Names in Expression Context — DONE

When `module M = ...` binds `M` as a variable, expressions like `M.x`
work via statics fallback.

**Fix: statics fallback** in Exp Constructor case (Statics.re ~line 880):

```reason
| Constructor(ctr, ty) =>
  let self = Self.of_ctr(ctx, ctr, ana, ty);
  switch (self) {
  | FreeConstructor(_) =>
    (* Fallback: check if it's a variable binding (e.g., from module keyword) *)
    switch (Ctx.lookup_var(ctx, ctr)) {
    | Some(var_entry) => /* treat as Var, use var_entry.typ */
      atomic(Just(var_entry.typ))
    | None => atomic(self)
    }
  | _ => atomic(self)
  };
```

This is ~5 lines. The logic: if constructor lookup produces FreeConstructor
(not found), try variable lookup. If found (because `module M = ...` bound
it), use the variable's type. Otherwise, keep the FreeConstructor error.

**Cursor inspector enhancement**: When a capitalized name resolves to a
variable (via this fallback), the cursor inspector should indicate this.
The cls can be overridden to show "Module variable" or "Variable" instead
of "Free constructor". This uses existing cls-override infrastructure
(same pattern as Mod items getting cls overrides in Statics.re).

**Pat Constructor fallback**: Similarly, in Pat statics for Constructor,
if the constructor is free and the parent context suggests a module binding,
treat as Var. However, if we use MPat sort for the module keyword, this
isn't needed — MPat already handles it. This fallback would only matter if
someone writes `let M = ...` without the `module` keyword, which we're
intentionally not supporting.

---

### 2.6 Menhir Parser Support — DONE

**Decision change**: Menhir now preserves `ModuleExp`/`ModuleMod` structure
(matching MakeTerm) instead of desugaring to `Let`/`ModItemLet`. This
enables `menhir_maketerm_equivalent_test` for module keyword forms and
ensures consistent AST structure.

Added `AST.ModuleExp(pat, exp, exp)` and `AST.ModItemModule(pat, exp)`.
Conversion.re has `mpat_of_pat`/`pat_of_mpat` helpers for bidirectional
mapping. Annotated module forms (`module M : T = ...`) supported via
`AscPat` in both Menhir and Conversion.

**All Menhir limitations resolved** — 73/73 tests pass, 0 skipped:
- Singleton labeled tuple types `(x=Int)` now parse (added grammar rule)
- Capitalized names after dot handled via Constructor→Label in Conversion.re
- `QUOTED_LABEL` added as exp production for backtick-quoted labels
- QCheck round-trip test normalizes both sides through Conversion

**Lexer.mll**: MODULE token added.

```ocaml
| "module" { MODULE }
```

**Parser.mly**: Add module rules.

```
(* Exp context *)
| MODULE; m = IDENT; SINGLE_EQUAL; e1 = exp; IN; e2 = exp
  { Let(VarPat(m), e1, e2) } %prec LET_EXP
| MODULE; m = CONSTRUCTOR_IDENT; SINGLE_EQUAL; e1 = exp; IN; e2 = exp
  { Let(VarPat(m), e1, e2) } %prec LET_EXP
```

Both IDENT and CONSTRUCTOR_IDENT produce VarPat — the `module` keyword
disambiguates. In the Menhir parser, this can desugar directly to Let
(no need for a separate ModuleExp AST node) since Menhir doesn't have
cursor inspector concerns.

For Mod context, add modItem rules:

```
| MODULE; m = IDENT; SINGLE_EQUAL; e = modItemExp
  { ModItemLet(VarPat(m), e) }
| MODULE; m = CONSTRUCTOR_IDENT; SINGLE_EQUAL; e = modItemExp
  { ModItemLet(VarPat(m), e) }
```

**Conversion.re**: No changes needed if Menhir desugars to Let/ModItemLet.

---

### 2.7 Remolding and Insert.re — DONE

**Insert.re effective_sort**: The `module` keyword expands in Exp context
(via left nib sort of ModuleExp form) and Mod context (via left nib sort
of ModuleMod form). No special cases needed in effective_sort — the
sorted_expansions system handles it automatically based on nib sorts.

**Segment.re remolding**: Add `remold_mpat` function. It's simple — MPat
only has atomic forms (identifiers), no compound forms to remold. Just
check if the tile matches an MPat atomic and return it.

Mod remolding: when encountering `["module", "="]` tiles, produce
ModuleMod. This parallels how `["let", "="]` produces ModLet.

---

### 2.8 Implementation Order — ALL DONE

All 8 steps completed. Additional work done beyond original plan:
- MPat type annotation form (`:` for `module M : T = ...`)
- Segment.re remold_mpat Typ sort transition (required for multi-token types)
- ExpandModule.re Sig type desugaring in mpat_to_pat (Sig → Prod for evaluator)
- ExplainThis data files for ModuleKeywordExp and ModuleKeywordDecl
- Cursor inspector cls override: Constructor→Var fallback shows "Variable"

---

## 3. Other Pre-Merge Items

### Update doc slide (Modules.ml)

The in-editor documentation slide (`src/web/init/docs/Modules.ml`) has 26
examples. Review whether it needs updates to reflect final state. Add
examples using the `module` keyword.

### Verify test suite

**Status**: Menhir (73/73, 0 skipped), Statics.Modules (58/58 + 10 keyword),
Evaluator.Modules (17/17). All 1036 tests pass across all test groups.
