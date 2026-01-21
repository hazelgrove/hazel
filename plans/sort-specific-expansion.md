# Sort-Specific Expansion

## Background

### Current Expansion System

When you type certain delimiter tokens (like `let`, `case`, `(`), they "expand" into multi-delimiter forms. For example:
- Typing `let` expands to a tile with label `["let", "=", "in"]` with only the first shard present
- Typing `(` expands to `["(", ")"]` with only the opening paren present

This expansion is controlled by `Form.Expansion.get(sort, token)` which returns `(Label.t, Direction.t)` - the full label and which direction the expansion happens.

### The Goal

Make expansion sort-aware so that:
- `let` in an Exp context expands to `["let", "=", "in"]` (expression let)
- `let` in a Pat context does NOT expand (stays as monotile `["let"]`)
- `[` in Exp context expands to `ListLitExp`
- `[` in Pat context expands to `ListLitPat`

## Implementation

### Key Design Decision: Nib Sort vs Mold.out

For registering which sort context a delimiter expands in, we use **nib sort** rather than **mold.out**:

- **mold.out**: The sort the form *produces* (e.g., Rule produces Rul)
- **nib sort**: The sort the form *expects* on each side (e.g., Rule expects Exp on its left)

We use nib sort because it reflects the context you're actually typing in. For example:
- Rule `["|", "=>"]` has `out=Rul` but `left_nib=Exp`
- You type `|` after an expression (the scrutinee or previous rule body), so the lookup sort is Exp

```ocaml
(* Form.re - sorted_expanding_of uses nib sorts *)
let sorted_expanding_of = ({expansion, label, mold}: t): option(sorted_expansions) => {
  let (l_nib, r_nib) = mold.nibs;
  switch (expansion, label) {
  | (L, [hd, ..._]) => Some([(hd, l_nib.sort, label, Direction.Left)])
  | (LT, [hd, ..._]) =>
    Some([
      (hd, l_nib.sort, label, Left),
      (ListUtil.last(label), r_nib.sort, label, Right),
    ])
  | _ => None
  };
};
```

**Open question**: Whether we should register BOTH nib sort AND mold.out to handle more cases automatically. Currently we use special cases instead.

### Special Case: Rul Context is Permissive

The `Rul` sort context falls back to any available expansion when no sort-specific match is found:

```ocaml
(* Form.re - Expansion.get *)
| None =>
  switch (sort) {
  | Rul =>
    (* Rul context: fall back to any expansion since rules contain
       Exp/Pat operands but have no direct operand forms. *)
    let any_match =
      sorted_expansions |> List.find_opt(((tok, _, _, _)) => tok == t);
    ...
  | _ => ([t], Right)
  }
```

**Why this is needed**: Inside a case expression, the sort context is Rul (case expects Rul children). But you actually type expressions (like `4` in `case 4 | ...`) which become operands of rules. Rul context has no operand forms of its own (no ParensRul, etc.), so it must delegate to Exp forms.

### Special Case: `|` Inside Case (Insert.re)

The `|` delimiter is handled entirely in `Insert.re` when inside a case expression:

```ocaml
| "|" when before_case_shard(z) || inside_case(z) =>
  (* SPECIAL CASE: Case rule delimiter.
     Inside a case, always expand | to Rule form regardless of local sort.

     Why this is needed: The Rule form's left nib is Exp (it expects an
     expression). But rule bodies can have type ascriptions like `expr : Type`,
     which means Relatives.sort returns Typ even though semantically we have
     an expression. Sort-specific expansion would fail to find | for Typ.

     This bypasses Form.Expansion.get entirely for | inside case expressions,
     hardcoding the Rule form label. *)
  (["|", "=>"], Left)
| "|" =>
  (* Outside case: | has no meaning, don't expand *)
  ([t], Left)
```

## Case/Rule Structure Analysis

Understanding why these special cases are needed:

**Case form:**
```ocaml
| Case => mk_op_c(L, ["case", "end"], Exp, [Rul])
```
- out = Exp (produces expression)
- children = [Rul] (expects rules inside)

**Rule form:**
```ocaml
| Rule => mk(L, ["|", "=>"], Mold.mk_bin'(P.rule_sep, Rul, Exp, [Pat], Exp))
```
- out = Rul (produces a rule)
- left nib = Exp, right nib = Exp (binary operator on expressions)
- inner child = Pat (the pattern between | and =>)

**The structure:**
```
case <Rul> end

where a Rule is:
<Exp> | <Pat> => <Exp>
  ^              ^
  left operand   right operand
  (scrutinee/    (body)
   prev body)
```

**The tension**: Inside case, context is Rul, but you type Exp stuff. The Exp stuff becomes operands of Rules, which produce Rul. This requires Rul context to be permissive.

**The ascription problem**: After `expr : Type`, the local sort is Typ (from the Type's right nib), even though semantically an expression just completed. Forms like `|` that expect Exp can't find a match. Hence the Insert.re special case.

## Known Limitations

1. **Ascription mismatch**: Ascriptions `expr : Type` produce Exp but have Typ right nib. This causes a mismatch between "semantic sort" and "nib sort" that requires special handling.

2. **Test coverage**: The Forall→Poly rename revealed a test passing by accident under sort-agnostic expansion. More edge cases may exist.

3. **Not fully principled**: We have working code with documented special cases, but haven't found a fully general solution. Possible future directions:
   - Register delimiters under multiple sorts (both nib and out)
   - Track "semantic sort" separately from "nib sort"
   - More sophisticated context analysis

## Files Changed

- `Form.re`: `sorted_expanding_of` uses nib sorts, `Expansion.get` has Rul fallback
- `Insert.re`: `|` handled entirely here when inside case
- `TyDiForms.re`: Updated to pass sort to `Form.Expansion.get`
