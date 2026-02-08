# Plan: Remove eager normalize from elaborated_type

## Problem

`Typ.normalize` is called in `elaborated_type` and `elaborated_pat_type` for
EVERY expression and pattern node. For full_app this is 1.54M recursive normalize
calls taking 469ms (67% of elaboration). But most expressions don't need a
normalized type — they just pass it through to the parent.

## What elaborated_type currently does

```
1. Info map lookup (Id.Map.find_opt)     — negligible
2. Typ.match_synswitch(ana_ty, self_ty)  — 0.7ms total, negligible
3. Typ.normalize(ctx, elab_ty)           — 469ms, THE cost
```

## Plan: Remove normalize from elaborated_type, push to use sites

### Step 1: Remove normalize from elaborated_type / elaborated_pat_type

Both functions return the `match_synswitch` result WITHOUT normalizing.
This makes them essentially free (map lookup + trivial synswitch).

### Step 2: Normalize at the final escape point

In `uexp_elab`, normalize the returned type once:
```reason
| (d, ty) =>
    ElaborationResult.Elaborates(d, Typ.normalize(ctx_for_top, ty));
```
This is ONE normalize call for the entire program — replaces ~10K per-expression calls.

Need to get the top-level ctx. Can look it up from the info map entry for `uexp` itself.

### Step 3: Internalize normalization into fresh_ascription

`fresh_ascription(d, t, t')` currently assumes `t'` is normalized. Change it to:
- First try `Typ.fast_equal(t, t')` on unnormalized types
- If they're equal (common case), return `d` — no normalization needed
- If they're not equal, normalize both and compare again
- Only insert ascription if truly different after normalization

This handles the If/Match/Cons cases (lines 436, 472, 511) without requiring
the caller to pre-normalize.

Note: fresh_ascription also receives the child's type `t` (from the recursive
`elaborate` call), which is also now unnormalized. So both sides are unnormalized,
and fast_equal on matching Vars (like `Var("HTML") == Var("HTML")`) will still
return true. The normalize-then-compare fallback handles cases where structural
comparison is needed.

IMPORTANT: fresh_ascription needs a `ctx` parameter now, for the fallback
normalize. It can get this from the info map or from the caller.

### Step 4: get_labels — no change needed

`Typ.get_labels` already calls `weak_head_normalize` internally (line 941 of
Typ.re). It doesn't need pre-normalized input. No change required.

### Step 5: Case-specific normalize calls — review each

These are explicit `Typ.normalize` calls in specific expression/pattern cases.
They are NOT in `elaborated_type` — they're in the case-specific elaboration code.
They are comparatively cheap (only fire for their specific syntactic form).

| # | Line | Form | Call | Action |
|---|------|------|------|--------|
| A | 265 | Exp Asc(e, t) | `Typ.normalize(ctx, t)` | KEEP — writes normalized type into AST for evaluator casts |
| B | 298 | Exp Constructor | `Typ.normalize(ctx, ty)` | KEEP — writes constructor return type into AST |
| C | 358 | Exp Let (labels) | `Typ.normalize(ctx, ty1)` | CHANGE to `weak_head_normalize` — only needs outermost Prod structure |
| D | 428 | Exp TypAp | `Typ.normalize(ctx, ut)` | KEEP — writes type arg into AST for polymorphic instantiation |
| E | 189 | Pat Asc(p, t) | `Typ.normalize(ctx, t)` | KEEP — same as A |
| F | 198-9 | Pat Constructor | `Typ.normalize(ctx, ana/syn_ty)` | KEEP — same as B |

Only C changes (normalize → weak_head_normalize).

### Step 6: No changes to non-Elaborator normalize calls

Statics.re, Info.re, etc. have their own normalize calls. Those are separate
and unaffected by this change.

## After the change: complete accounting of normalize calls in Elaborator

### Typ.normalize calls (full normalize):

| Location | When it fires | Why needed |
|----------|---------------|------------|
| uexp_elab return | Once per program | Final type escapes elaborator |
| Exp Asc(e, t) | Per user type annotation | Annotation goes into AST |
| Exp Constructor | Per constructor expression | Return type goes into AST |
| Exp TypAp | Per type application | Type arg goes into AST |
| Pat Asc(p, t) | Per pattern annotation | Annotation goes into AST |
| Pat Constructor | Per pattern constructor | Constructor type goes into AST |
| fresh_ascription (fallback) | When fast_equal returns false | Ascription type goes into AST |

### Typ.weak_head_normalize calls (outermost expansion only):

| Location | When it fires | Why needed |
|----------|---------------|------------|
| Exp Let (label rearrange) | Per let binding with tuple pattern | Need to see Prod structure |
| get_labels (internal) | Per Tuple expression/pattern | Need to see Prod structure |

### Calls REMOVED:

| Location | Was | Savings |
|----------|-----|---------|
| elaborated_type | Per EVERY expression | ~469ms for full_app |
| elaborated_pat_type | Per EVERY pattern | (included in above) |

## Expected impact

- full_app elab: 703ms → ~234ms (the "other" traversal time)
- Normalize cost: 469ms → ~10-20ms (only case-specific calls)
- Total speedup from original: 1,273ms → ~234ms (5.4x)

Combined with the all_ids_temp removal (already done):
- Original: 1,273ms
- After all_ids_temp removal: 703ms (1.8x)
- After lazy normalize: ~234ms (5.4x from original)

## Risk assessment

LOW RISK. The normalized type was only used for:
1. Label inference (get_labels) — already handles unnormalized input
2. Ascription insertion (fresh_ascription) — internalizing normalization
3. Propagation to parent — just pass-through, doesn't need normalization
4. Final return — normalizing once at the end

The case-specific normalize calls (A-F) are unchanged except C (Let labels)
which switches to weak_head_normalize (sufficient for seeing Prod structure).
