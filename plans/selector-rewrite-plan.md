# Selector Resolver Rewrite Plan

## Goal

Replace the existing flat-list resolver in `Selector.re` with a clean,
form-generic implementation based on the spec's recursive selector type.
The new resolver should:

1. Work homogeneously across ALL sorts (Exp, Pat, Typ, Mod, Sig, TPat, MPat)
2. Be extensible to new forms by adding only a `decompose` case
3. Pass all existing tests (~250+ selector tests, ~50+ edit tests)
4. Follow the spec's recursive type structure exactly

## Architecture Overview

```
Selector string  →  [Tokenizer]  →  tokens
                 →  [Parser]     →  selector (recursive tree)
                 →  [Resolver]   →  list(match_result)
                                        ↑
                              uses [Decompose] to map terms to spines
                              uses [SpineMatcher] to match spine patterns
                              uses [Children] for descent
                              uses [Binders] for name lookup
                              uses [Atoms] for leaf matching
```

### Key separation of concerns

- **Decompose**: Per-form knowledge lives ONLY here. Maps each term variant
  to a flat spine of tokens and children.
- **SpineMatcher**: Generic. Matches a `spine_elem list` against a
  `spine_pos list`. No form-specific logic.
- **Resolver**: Dispatches on selector constructors (Focus, Spine, Descend,
  ChildIdx, EnterBinder). Calls SpineMatcher for Spine, Children for
  Descend, etc.
- **Tokenizer/Parser**: Converts selector strings to the recursive type.
  Handles sugars (implicit focus, trailing ellipsis, chain expansion,
  implicit separators).

## Phase 1: Core Types

### 1.1 Selector type (from spec)

```reason
type binder_ref =
  | Name(string)
  | NameIdx(string, int);

type selector =
  | Focus(option(selector))
      /* Focus(None) = terminal. Focus(Some(k)) = focus on this node
         if k matches it (k's results discarded, used as predicate).
         % let x  →  Focus(Some(Spine([Token("let"), ChildNamed("x")])))
         %        →  Focus(None) */
  | Spine(spine)
  | Descend(selector)
  | ChildIdx(int, selector)
  | EnterBinder(binder_ref, selector);

type spine = list(spine_elem)

and spine_elem =
  | Token(string)
  | TokenWild
  | ChildWild
  | ChildNamed(binder_ref)
  | ChildSel(option(binder_ref), selector)
  | Ellipsis
  | Atom(string);
```

### 1.2 Focus target (existing, keep as-is)

```reason
type focus_target =
  | FocusExp(Exp.t)
  | FocusPat(Pat.t)
  | FocusTyp(Typ.t)
  | FocusMod(Mod.t);
```

Potentially extend with `FocusSig(Sig.t)` and `FocusTPat(TPat.t)` if
needed for deep navigation into those sorts.

### 1.3 Decomposed spine

```reason
type spine_pos =
  | PosToken(string)
  | PosChild(focus_target);

type decomposed =
  | Form(list(spine_pos))     /* compound form with token/child spine */
  | AtomNode(string)          /* leaf node, matchable by string */
  | Hole                      /* empty hole, matches nothing */
  | Transparent(focus_target) /* look-through (Parens, Projector, etc.) */
```

### 1.4 Match result (existing, keep as-is)

```reason
type match_result = {
  focused: focus_target,
  focused_id: Id.t,
  breadcrumb: string,
};
```

## Phase 2: Decompose

The `decompose` function maps each term variant to its spine structure.
This is the ONLY place with per-form knowledge.

### 2.1 Exp decomposition

```reason
let decompose_exp = (e: Exp.t): decomposed =>
  switch (Exp.term_of(e)) {
  /* Binding forms */
  | Let(pat, def, body) =>
    Form([T("let"), C(Pat(pat)), T("="), C(Exp(def)), T("in"), C(Exp(body))])
  | TyAlias(tpat, typ, body) =>
    Form([T("type"), C(TPat(tpat)), T("="), C(Typ(typ)), T("in"), C(Exp(body))])
  | ModuleExp(mpat, def, body) =>
    Form([T("module"), C(MPat(mpat)), T("="), C(Exp(def)), T("in"), C(Exp(body))])

  /* Functions */
  | Fun(pat, body, _, _) =>
    Form([T("fun"), C(Pat(pat)), T("->"), C(Exp(body))])
  | FixF(pat, body, _) =>
    Form([T("fix"), C(Pat(pat)), T("->"), C(Exp(body))])
  | TypFun(tpat, body, _) =>
    Form([T("typfun"), C(TPat(tpat)), T("->"), C(Exp(body))])

  /* Control flow */
  | If(cond, then_, else_) =>
    Form([T("if"), C(Exp(cond)), T("then"), C(Exp(then_)), T("else"), C(Exp(else_))])
  | Match(scrut, rules) =>
    Form([T("case"), C(Exp(scrut))]
      @ List.concat_map(((pat, body)) =>
          [T("|"), C(Pat(pat)), T("=>"), C(Exp(body))], rules)
      @ [T("end")])

  /* Collections */
  | Tuple(items) =>
    Form([T("(")] @ intersperse(T(","), List.map(e => C(Exp(e)), items)) @ [T(")")])
  | ListLit(items) =>
    Form([T("[")] @ intersperse(T(","), List.map(e => C(Exp(e)), items)) @ [T("]")])
  | Module(items) =>
    Form([T("{")] @ intersperse(T(";"), List.map(m => C(Mod(m)), items)) @ [T("}")])

  /* Operators */
  | BinOp(op, e1, e2) =>
    Form([C(Exp(e1)), T(op_to_string(op)), C(Exp(e2))])
  | UnOp(op, e1) =>
    Form([T(unop_to_string(op)), C(Exp(e1))])
  | Cons(hd, tl) =>
    Form([C(Exp(hd)), T("::"), C(Exp(tl))])

  /* Application */
  | Ap(_, fn, arg) =>
    Form([C(Exp(fn)), T("("), C(Exp(arg)), T(")")])

  /* Type annotation */
  | Asc(expr, typ) =>
    Form([C(Exp(expr)), T(":"), C(Typ(typ))])

  /* Test */
  | Test(body) =>
    Form([T("test"), C(Exp(body)), T("end")])

  /* Sequence */
  | Seq(e1, e2) =>
    Form([C(Exp(e1)), T(";"), C(Exp(e2))])

  /* Dot access */
  | Dot(obj, field) =>
    Form([C(Exp(obj)), T("."), C(Exp(field))])

  /* Transparent wrappers */
  | Parens(inner) => Transparent(FocusExp(inner))
  | Projector(_, inner) => Transparent(FocusExp(inner))

  /* Atoms */
  | Var(name) => AtomNode(name)
  | Constructor(name, _) => AtomNode(name)
  | Atom(c) => AtomNode(Atom.to_literal(c))
  | Label(s) => AtomNode(s)

  /* Holes */
  | EmptyHole => Hole
  | Invalid(_) | MultiHole(_) => Hole

  /* Others — extend as needed */
  | _ => Hole
  };
```

### 2.2 Pat decomposition

```reason
let decompose_pat = (p: Pat.t): decomposed =>
  switch (Pat.term_of(p)) {
  | Var(name) => AtomNode(name)
  | Constructor(name, _) => AtomNode(name)
  | Atom(c) => AtomNode(Atom.to_literal(c))
  | Wild => AtomNode("_")
  | Tuple(items) =>
    Form([T("(")] @ intersperse(T(","), List.map(p => C(Pat(p)), items)) @ [T(")")])
  | ListLit(items) =>
    Form([T("[")] @ intersperse(T(","), List.map(p => C(Pat(p)), items)) @ [T("]")])
  | Cons(hd, tl) =>
    Form([C(Pat(hd)), T("::"), C(Pat(tl))])
  | Ap(ctor, arg) =>
    Form([C(Pat(ctor)), T("("), C(Pat(arg)), T(")")])
  | Asc(inner, typ) =>
    Form([C(Pat(inner)), T(":"), C(Typ(typ))])
  | Parens(inner) => Transparent(FocusPat(inner))
  | Projector(_, inner) => Transparent(FocusPat(inner))
  | EmptyHole => Hole
  | _ => Hole
  };
```

### 2.3 Typ decomposition

```reason
let decompose_typ = (t: Typ.t): decomposed =>
  switch (Typ.term_of(t)) {
  | Atom(c) => AtomNode(Atom.cls_to_string(c))
  | Var(name) => AtomNode(name)
  | Arrow(t1, t2) =>
    Form([C(Typ(t1)), T("->"), C(Typ(t2))])
  | Prod(items) =>
    Form([T("(")] @ intersperse(T(","), List.map(t => C(Typ(t)), items)) @ [T(")")])
  | List(inner) =>
    Form([T("["), C(Typ(inner)), T("]")])
  | Sum(ctors) => /* sum type decomposition */
    Form(sum_to_spine(ctors))
  | Rec(tpat, body) =>
    Form([T("rec"), C(TPat(tpat)), T("->"), C(Typ(body))])
  | Poly(tpat, body) =>
    Form([T("poly"), C(TPat(tpat)), T("->"), C(Typ(body))])
  | Sig(items) =>
    Form([T("{")] @ intersperse(T(";"), List.map(s => C(Sig(s)), items)) @ [T("}")])
  | Parens(inner) => Transparent(FocusTyp(inner))
  | Unknown(Hole(_)) => Hole
  | _ => Hole
  };
```

### 2.4 Mod decomposition

```reason
let decompose_mod = (m: Mod.t): decomposed =>
  switch (Mod.term_of(m)) {
  | ModLet(pat, def) =>
    Form([T("let"), C(Pat(pat)), T("="), C(Exp(def))])
  | ModType(tpat, typ) =>
    Form([T("type"), C(TPat(tpat)), T("="), C(Typ(typ))])
  | ModuleMod(mpat, def) =>
    Form([T("module"), C(MPat(mpat)), T("="), C(Exp(def))])
  | ModExp(e) => Transparent(FocusExp(e))
  | EmptyHole => Hole
  | _ => Hole
  };
```

### 2.5 Sig, TPat, MPat decomposition

```reason
let decompose_sig = (s: Sig.t): decomposed =>
  switch (Sig.term_of(s)) {
  | SigLet(pat) =>
    Form([T("let"), C(Pat(pat))])
  | SigType(tpat, typ) =>
    Form([T("type"), C(TPat(tpat)), T("="), C(Typ(typ))])
  | EmptyHole => Hole
  | _ => Hole
  };

let decompose_tpat = (tp: TPat.t): decomposed =>
  switch (TPat.term_of(tp)) {
  | Var(name) => AtomNode(name)
  | EmptyHole => Hole
  | _ => Hole
  };

let decompose_mpat = (mp: MPat.t): decomposed =>
  switch (MPat.term_of(mp)) {
  | Var(name) => AtomNode(name)
  | Asc(inner, typ) =>
    Form([C(MPat(inner)), T(":"), C(Typ(typ))])
  | EmptyHole => Hole
  | _ => Hole
  };
```

### 2.6 Unified decompose

```reason
let decompose = (target: focus_target): decomposed =>
  switch (target) {
  | FocusExp(e) => decompose_exp(e)
  | FocusPat(p) => decompose_pat(p)
  | FocusTyp(t) => decompose_typ(t)
  | FocusMod(m) => decompose_mod(m)
  /* extend for Sig, TPat, MPat if focus_target grows */
  };
```

### 2.7 Transparency resolution

When decompose returns `Transparent(inner)`, the resolver looks through
it — decomposing the inner term instead. This handles Parens, Projector,
and other wrapper forms uniformly.

```reason
let rec decompose_through = (target: focus_target): (focus_target, decomposed) =>
  switch (decompose(target)) {
  | Transparent(inner) => decompose_through(inner)
  | d => (target, d)
  };
```

**Key design decision**: Parens are transparent for spine matching but
NOT for child indexing. `#0` on `(1, 2, 3)` enters Parens and gets `Tuple`,
then `#0` on Tuple gets `1`. This matches existing test behavior.

## Phase 3: Spine Matcher

The spine matcher is the core algorithm. It takes a `spine_elem list`
pattern and matches it against a `spine_pos list` from decomposition.

### 3.1 Core matching algorithm

```reason
/* Returns all successful match continuations.
   Each success returns (remaining_pattern, remaining_positions, results) */
let rec match_spine =
  (pattern: list(spine_elem),
   positions: list(spine_pos),
   children_so_far: int)  /* for name extraction from children */
  : list(list(match_result)) =>

  switch (pattern) {
  /* Empty pattern: success (trailing positions implicitly wild) */
  | [] => [[]]

  /* Token must match next token position */
  | [Token(t), ...rest] =>
    skip_children_to_token(t, positions)
    |> Option.map(remaining => match_spine(rest, remaining, children_so_far))
    |> Option.value(~default=[])

  /* TokenWild matches any token position */
  | [TokenWild, ...rest] =>
    skip_children_to_any_token(positions)
    |> Option.map((_, remaining) => match_spine(rest, remaining, children_so_far))
    |> Option.value(~default=[])

  /* ChildWild skips one child */
  | [ChildWild, ...rest] =>
    skip_tokens_to_child(positions)
    |> Option.map((_, remaining) => match_spine(rest, remaining, children_so_far + 1))
    |> Option.value(~default=[])

  /* ChildNamed matches a child by name */
  | [ChildNamed(ref), ...rest] =>
    skip_tokens_to_child(positions)
    |> Option.bind((child, remaining) =>
         name_matches(ref, child) ? Some(remaining) : None)
    |> Option.map(remaining => match_spine(rest, remaining, children_so_far + 1))
    |> Option.value(~default=[])

  /* ChildSel: enter child with continuation selector */
  | [ChildSel(name_opt, k), ...rest] =>
    skip_tokens_to_child(positions)
    |> Option.bind((child, remaining) =>
         switch (name_opt) {
         | Some(ref) when !name_matches(ref, child) => None
         | _ => Some((child, remaining))
         })
    |> Option.map(((child, remaining)) => {
         let inner_results = resolve(k, child);
         /* If inner selector matched, check that rest of spine matches too */
         inner_results == []
           ? []
           : inner_results  /* rest is implicitly wild if omitted */
       })
    |> Option.value(~default=[])

  /* Ellipsis: try skipping 0, 1, 2, ... positions */
  | [Ellipsis, ...rest] =>
    try_ellipsis(rest, positions, children_so_far)

  /* Atom: match leaf node by string */
  | [Atom(s), ...rest] =>
    switch (atom_string_of(target)) {
    | Some(s') when s == s' =>
      match_spine(rest, [], children_so_far)  /* atom consumes everything */
    | _ => []
    }
  };
```

### 3.2 Ellipsis handling

Ellipsis tries matching the rest of the pattern starting from each
remaining position. It's greedy-last (tries skipping more first, but
practically any order works since we collect all matches).

```reason
let rec try_ellipsis = (rest_pattern, positions, children_so_far) =>
  /* Try matching rest_pattern at current position */
  let here = match_spine(rest_pattern, positions, children_so_far);
  /* Try skipping one position and recursing */
  let skip = switch (positions) {
    | [] => []
    | [PosToken(_), ...remaining] =>
      try_ellipsis(rest_pattern, remaining, children_so_far)
    | [PosChild(_), ...remaining] =>
      try_ellipsis(rest_pattern, remaining, children_so_far + 1)
    };
  here @ skip;
```

### 3.3 Separator transparency

The spec says separator tokens between children are implicitly matched.
When two child-matching elements are adjacent in the pattern (e.g.,
`ChildWild, ChildSel`), an intervening separator token in the positions
is automatically skipped.

Implementation: when we're looking for a child position and the next
position is a token, skip it if it's a separator (`,`, `;`, `|`).
This is handled by `skip_tokens_to_child`.

```reason
let skip_tokens_to_child = (positions) =>
  switch (positions) {
  | [PosChild(c), ...rest] => Some((c, rest))
  | [PosToken(sep), PosChild(c), ...rest]
    when is_separator(sep) => Some((c, rest))
  | _ => None
  };

let is_separator = (t) =>
  List.mem(t, [",", ";", "|"]);
```

## Phase 4: Resolver

### 4.1 Top-level resolve

```reason
let rec resolve = (sel: selector, target: focus_target): list(match_result) => {
  /* Look through transparent wrappers */
  let (actual_target, decomposed) = decompose_through(target);

  switch (sel) {
  | Focus(None) =>
    [mk_result(actual_target)]

  | Focus(Some(constraint)) =>
    /* Focus with predicate: apply constraint to same node.
       If it produces any results, focus on the node itself. */
    switch (resolve(constraint, actual_target)) {
    | [] => []
    | [_, ..._] => [mk_result(actual_target)]
    }

  | Spine(spine) =>
    switch (decomposed) {
    | Form(positions) =>
      match_spine(spine, positions)
    | AtomNode(s) =>
      /* Spine can match an atom if pattern is just [Atom(s)] */
      match_spine_atom(spine, s, actual_target)
    | Hole => []
    | Transparent(_) => assert(false)  /* resolved above */
    }

  | Descend(inner) =>
    let here = resolve(inner, actual_target);
    let below =
      children_of(actual_target)
      |> List.concat_map(child => resolve(Descend(inner), child));
    here @ below

  | ChildIdx(n, k) =>
    switch (nth_child(n, actual_target)) {
    | Some(child) => resolve(k, child)
    | None => []
    }

  | EnterBinder(ref, k) =>
    find_binder_defs(ref, actual_target)
    |> List.concat_map(def => resolve(k, def))
  };
};
```

### 4.2 Children enumeration (for Descend)

```reason
let children_of = (target: focus_target): list(focus_target) =>
  switch (decompose(target)) {
  | Form(positions) =>
    positions |> List.filter_map(fun
      | PosChild(c) => Some(c)
      | PosToken(_) => None)
  | Transparent(inner) => [inner]
  | AtomNode(_) | Hole => []
  };
```

This is elegant: `children_of` is derived FROM `decompose`. No separate
enumeration needed. Adding a form to `decompose` automatically makes it
work with descent.

### 4.3 Nth child (for ChildIdx)

```reason
let nth_child = (n: int, target: focus_target): option(focus_target) => {
  let children = children_of(target);
  List.nth_opt(children, n);
};
```

### 4.4 Binder search (for EnterBinder / chains)

Walks through Let chains and module items looking for bindings with
a matching name. Returns the definition(s).

```reason
let rec find_binder_defs =
  (ref: binder_ref, target: focus_target): list(focus_target) =>
  switch (target) {
  | FocusExp(e) =>
    switch (Exp.term_of(e)) {
    | Let(pat, def, body) =>
      let here = pat_matches_ref(ref, pat) ? [FocusExp(def)] : [];
      here @ find_binder_defs(ref, FocusExp(body))
    | TyAlias(tpat, typ, body) =>
      let here = tpat_matches_ref(ref, tpat) ? [FocusTyp(typ)] : [];
      here @ find_binder_defs(ref, FocusExp(body))
    | ModuleExp(mpat, def, body) =>
      let here = mpat_matches_ref(ref, mpat) ? [FocusExp(def)] : [];
      here @ find_binder_defs(ref, FocusExp(body))
    | Module(items) =>
      items |> List.concat_map(m => find_binder_defs(ref, FocusMod(m)))
    | Parens(inner) => find_binder_defs(ref, FocusExp(inner))
    | _ => []
    }
  | FocusMod(m) =>
    switch (Mod.term_of(m)) {
    | ModLet(pat, def) =>
      pat_matches_ref(ref, pat) ? [FocusExp(def)] : []
    | ModType(tpat, typ) =>
      tpat_matches_ref(ref, tpat) ? [FocusTyp(typ)] : []
    | ModuleMod(mpat, def) =>
      mpat_matches_ref(ref, mpat) ? [FocusExp(def)] : []
    | _ => []
    }
  | _ => []
  };
```

### 4.5 Name matching utilities

```reason
let name_of_target = (target: focus_target): option(string) =>
  switch (target) {
  | FocusPat(p) => pat_name(p)
  | FocusTyp(_) => None  /* types don't have names in binder sense */
  | FocusExp(_) => None
  | FocusMod(m) =>
    switch (Mod.term_of(m)) {
    | ModLet(pat, _) => pat_name(pat)
    | ModType(tpat, _) => tpat_name(tpat)
    | ModuleMod(mpat, _) => mpat_name(mpat)
    | _ => None
    }
  };

/* For matching ChildNamed: extract name from a child position.
   The child's name depends on what it is in context — for a Let,
   the first child (pat) carries the name. We need to match the
   FIRST child of the form that precedes this position. */
let name_matches = (ref: binder_ref, child: focus_target): bool =>
  switch (ref) {
  | Name(name) =>
    switch (name_of_focus(child)) {
    | Some(n) => String.equal(n, name)
    | None => false
    }
  | NameIdx(name, idx) =>
    switch (name_of_focus(child)) {
    | Some(n) => String.equal(n, name)  /* idx checked at binder level */
    | None => false
    }
  };

/* Extract a name from pat, looking through Asc/Parens wrappers */
let rec pat_name = (p: Pat.t): option(string) =>
  switch (Pat.term_of(p)) {
  | Var(name) => Some(name)
  | Asc(inner, _) => pat_name(inner)
  | Parens(inner) => pat_name(inner)
  | Projector(_, inner) => pat_name(inner)
  | TupLabel(_, inner) => pat_name(inner)
  | _ => None
  };
```

### 4.6 Atom string extraction

```reason
let atom_string = (target: focus_target): option(string) =>
  switch (decompose(target)) {
  | AtomNode(s) => Some(s)
  | _ => None
  };
```

## Phase 5: Tokenizer & Parser

### 5.1 Tokenizer

The existing tokenizer can be mostly reused. It splits the selector
string on whitespace and classifies tokens. Key tokens:

```
%        → Focus marker
_        → Wildcard
_...     → Ellipsis
\...     → Descent
#N       → ChildIndex(N)
name/    → ChainStep(name)
name#N   → IndexedName(name, N)
keyword  → Keyword (let, fun, if, case, module, type, test, etc.)
delimiter→ Delimiter (=, ->, =>, |, :, [, ], (, ), {, }, ;, ,, etc.)
operator → Operator (+, -, *, **, &&, ||, ::, ++, ==, etc.)
name     → Name (anything else)
```

### 5.2 Parser: tokens → selector

The parser converts the flat token list into the recursive selector type.
Key logic:

1. **Chain expansion**: `a/b/c/` → `EnterBinder(a, EnterBinder(b, EnterBinder(c, Focus(None))))`
2. **Implicit focus**: If no `%`, insert before last name or at end
3. **Spine construction**: Group tokens between chain steps into spine
   patterns, nesting `ChildSel` for the active child
4. **Descent wrapping**: `\...` wraps the following selector in `Descend`

The parser builds the selector tree from left to right. The key insight
is that `ChildSel` carries the CONTINUATION, so the parser needs to
collect the sub-pattern for the active child and nest it.

**Parsing algorithm sketch:**

```
parse(tokens):
  1. Split on chain steps (name/)
  2. For each non-chain segment, build a Spine:
     a. Scan left-to-right
     b. Keywords/delimiters → Token(t)
     c. _ → ChildWild
     d. _... → Ellipsis
     e. % → mark position for ChildSel
     f. name → either ChildNamed or Token depending on context
     g. After scanning, insert ChildSel at the marked position
        with the continuation being everything that follows
  3. Chain segments: wrap in EnterBinder
  4. \... prefix: wrap in Descend
```

**Critical detail: ChildSel continuation nesting**

In `let x = % in _`, the ChildSel for `%` has continuation `Focus(None)`,
and the trailing `in _` is part of the OUTER spine (implicitly wild).

In `{ let x = % }`, the ChildSel for `%` enters a module item child,
and the sub-spine `let x = %` becomes the continuation:
```
Spine([
  Token("{"),
  ChildSel(None,
    Spine([Token("let"), ChildNamed("x"), Token("="), ChildSel(None, Focus(None))]))
])
```

The parser needs to recognize when a spine pattern should be split into
outer/inner based on the form structure. This is one of the trickier
parts of the parser — recognizing form boundaries.

**Approach**: The parser doesn't need to know form structure. Instead:
- Tokens that are "opening" brackets (`{`, `[`, `(`, `case`) start a
  nesting level. Everything until the matching close is the inner spine.
- Keywords after opening brackets are part of the inner (item) spine.
- The `%` position determines which child gets `ChildSel`.

Actually, a simpler approach: the parser builds a flat spine and the
RESOLVER handles nesting. When matching `Token("{"), Token("let"), ...`
against a Module's positions `[Token("{"), Child(ModLet...), ...]`, the
resolver sees that `Token("let")` doesn't match `Child(...)`, so it
tries entering the child and matching the remaining pattern against the
child's decomposed spine. This is the **spine descent** behavior.

This means the parser CAN stay flat — no form-awareness needed. The
resolver's spine matcher handles the nesting by trying to match pattern
elements against child sub-spines when direct matching fails.

### 5.3 Spine descent in the matcher

When the pattern has `Token("let")` but the next position is
`PosChild(m)`, the matcher tries decomposing `m` and matching
the remaining pattern against `m`'s spine. This is how `{ let x = %`
works without the parser knowing about module item structure.

```reason
/* In match_spine, when Token doesn't match PosChild: */
| [Token(t), ...rest_pattern] =>
  switch (positions) {
  | [PosChild(child), ...rest_pos] =>
    /* Try matching the full remaining pattern inside this child */
    let child_results = match_spine_in(pattern, child);
    /* Also try skipping this child and continuing */
    let skip_results = match_spine(pattern, rest_pos, children_so_far + 1);
    child_results @ skip_results
  | [PosToken(t'), ...rest_pos] when t == t' =>
    match_spine(rest_pattern, rest_pos, children_so_far)
  | _ => []
  }
```

This "try entering children" behavior is what makes the flat parser
work with nested structures. The cost is some extra matching attempts,
but selectors are short and terms are finite, so this is fine.

**Important**: This auto-descent into children should only happen for
compound children (children whose decomposition is a Form), not for
atoms. And it should respect the rule that exactly one ChildSel exists
per logical spine level.

## Phase 6: Canonical Selectors

### 6.1 Numeric canonical

Walk from root to target ID using `#N` at each level:

```reason
let rec canonical_numeric = (target_id: Id.t, node: focus_target): option(selector) =>
  if (id_of(node) == target_id) {
    Some(Focus(None));
  } else {
    children_of(node)
    |> List.mapi((i, child) =>
         canonical_numeric(target_id, child)
         |> Option.map(k => ChildIdx(i, k)))
    |> List.find_map(x => x);
  };
```

### 6.2 Named canonical

Prefer names/keywords over indices. For each level, try to produce
a human-readable selector using the form's tokens and child names.

```reason
let canonical_named = (target_id: Id.t, node: focus_target): option(selector) =>
  if (id_of(node) == target_id) {
    Some(Focus(None));
  } else {
    /* Try named path first (using form tokens and binder names) */
    try_named_canonical(target_id, node)
    /* Fallback to numeric */
    |> or_else(() => canonical_numeric(target_id, node));
  };
```

### 6.3 Deparse

Convert selector back to surface string. Straightforward recursive
walk over the selector type.

## Phase 7: Diagnostics

### 7.1 Error tracking

During matching, track:
- How many spine elements matched before failure
- Which element failed
- Available names at the failure point
- The closest name to a failed ChildNamed (for "did you mean?")

```reason
type match_diagnostic = {
  matched_up_to: list(string),  /* tokens matched so far */
  failed_at: string,            /* the element that failed */
  available_names: list(string),/* names visible at failure point */
  suggestions: list(string),    /* close matches for typos */
};
```

### 7.2 "Did you mean?" suggestions

Use edit distance to suggest close matches when a name doesn't match:

```reason
let suggest = (target: string, available: list(string)): list(string) =>
  available
  |> List.filter(name => edit_distance(target, name) <= 2)
  |> List.sort_by(name => edit_distance(target, name));
```

## Phase 8: Integration

### 8.1 Public API (preserve existing signatures)

```reason
/* Core query functions */
let query: (string, Exp.t) => result(list(match_result), string)
let query_unique: (string, Exp.t) => result(match_result, string)

/* For edit actions */
let selector_update: (string, string, Exp.t) => result(Exp.t, string)
let selector_delete: (string, Exp.t) => result(Exp.t, string)
let selector_insert_before: (string, string, Exp.t) => result(Exp.t, string)
let selector_insert_after: (string, string, Exp.t) => result(Exp.t, string)

/* Canonical */
let canonical_numeric: (Id.t, Exp.t) => option(string)
let canonical_named: (Id.t, Exp.t) => option(string)

/* Deparse */
let deparse: selector => string
```

### 8.2 Integration with CompositionGo.re

The existing dispatch in CompositionGo.re calls into Selector for
SelectorUpdate/Delete/InsertBefore/InsertAfter actions. The public
API should stay compatible.

### 8.3 Integration with ActionExplorer.re

The developer UI exposes selector actions and GetCanonical. Should
continue working with the new implementation.

### 8.4 Test coupling to internal types

The deparse and canonical tests directly construct `sem_step` values
(e.g., `Selector.MatchFocus`, `Selector.MatchKeyword("let")`, etc.).
These ~15 tests are coupled to the internal `sem_step` type.

**Options:**
A. Keep `sem_step` as the representation for `canonical_*` and `deparse`,
   even though the resolver internally uses the recursive `selector` type.
   Canonical and deparse would convert to/from sem_step.
B. Update these tests to use the new recursive selector type.
C. Have `canonical_*` return `selector`, add `deparse_selector` for the
   new type, and update the ~15 tests.

**Recommended**: Option C. The recursive type IS the canonical
representation. These are simple test updates. The `sem_step` type can
be removed entirely if no other code depends on it.

## Implementation Order

1. **Types** (Phase 1): Define selector, spine_elem, spine_pos, decomposed
2. **Decompose** (Phase 2): All sort decompositions, starting with Exp
3. **SpineMatcher** (Phase 3): Core matching algorithm with ellipsis
4. **Resolver** (Phase 4): Top-level resolve, children_of, nth_child
5. **Binders** (Phase 4.4): find_binder_defs, name matching
6. **Tokenizer** (Phase 5.1): Reuse/adapt existing
7. **Parser** (Phase 5.2): Build recursive selector from tokens
8. **Canonical** (Phase 6): Numeric and named canonical selectors
9. **Diagnostics** (Phase 7): Error messages, suggestions
10. **Integration** (Phase 8): Wire up to existing API, run tests

Each phase should be testable independently. Start running existing
tests after Phase 7; they should all pass by end of Phase 8.

## Test Coverage Gaps

Comparing spec examples against existing tests, these areas need
additional test coverage:

### Already well-tested
- Let spine (basic, annotated, shadowed, indexed)
- If/then/else spine
- Fun spine
- Case arms (named, wildcard, by constructor)
- Module chains (A/B/C)
- Module spine ({ %, { _ %, { _... %)
- Descent (\...)
- Child index (#N, deep, cross-sort)
- BinOp spine (+ - && == ++ ::)
- Atom matching (\... 42, \... true, bare names)
- Focus positions (%, let % x, % let x)
- Implicit focus
- Error diagnostics
- Edit actions (update, delete, insert)
- Canonical selectors (numeric, named, roundtrip)

### Needs additional tests
1. **Typ sort navigation**: `T1 -> %` (arrow right), `[%]` (list inner),
   `(T1, %)` (product element). No tests currently exercise Typ-sort
   spine matching directly (only through `let x : %` which is FocusTyp
   but doesn't match within the type).
2. **Pat sort navigation**: Matching within complex patterns, e.g.,
   `let (x, %) = ...` — tuple patterns, cons patterns `p1 :: p2`.
3. **Sig items**: `{ let %, type T = % }` within signature bodies.
4. **TokenWild**: `_ x = %` — explicitly test that TokenWild matches
   any keyword (`let`, `module`, `type`).
5. **More operators**: `**`, `>=`, `<=`, `||`, `!=` spine matching.
6. **Nested descent + spine**: `\... _ + %` (find all right operands
   of + anywhere in the program).
7. **Fun with type annotation**: `fun (x : Int) -> %` — type annotation
   on function parameter.
8. **Empty sequences**: `[]` list literal, `()` empty tuple, `{}` empty module.
9. **String atom matching**: `\... "hello"` for string literals.
10. **Cross-sort child index**: `#0 #1` where #0 enters an Exp child
    and #1 enters a Typ child (e.g., ascription).

## Risk Areas & Edge Cases

(Updated after consulting existing implementation)

### Critical — must get right or tests break

1. **Parens(Tuple) transparency**: In the AST, `(a, b, c)` is
   `Parens(Tuple([a, b, c]))`. The `(` token in selectors must match
   BOTH `Tuple(items)` directly AND `Parens({term: Tuple(items), _})`.
   The decompose for Parens should be `Transparent`, but when the
   selector has `(` as a token, the matcher needs to look through
   Parens to find the Tuple inside. This means `Transparent` alone
   isn't enough — the matcher needs a "peek through Parens to find
   the form whose opening token we're looking for" behavior.

   **Approach**: When matching `Token("(")` against `Transparent(inner)`,
   unwrap and try matching against inner. This works because
   `decompose(Parens(Tuple([a,b])))` = `Transparent(FocusExp(Tuple([a,b])))`,
   and `decompose(Tuple([a,b]))` = `Form([T("("), C(a), T(","), C(b), T(")")])`.
   The matcher resolves Transparent before trying Token matches.

2. **Type-annotated let patterns**: `let x : Int = 42` parses as
   `Let(Asc(Var("x"), Int), 42, ...)`. The pattern is `Asc`, not `Var`.
   - Name extraction (`pat_name`) must unwrap Asc/Parens/Projector/TupLabel
   - `let x : %` must focus the Typ child of the Asc
   - `let x : _ = %` must skip the type annotation, focus the def
   - `let x = %` must work EVEN when pat has Asc (find def regardless)

3. **Match rule virtual indexing**: `Match(scrut, rules)` has children:
   `#0` = scrutinee, `#1` = first rule pair, `#2` = second rule pair, etc.
   Inside a rule pair: `#0` = pattern, `#1` = body.
   This means `#1 #0` = first rule's pattern, `#1 #1` = first rule's body.
   The decompose must produce rule pairs as children with their own
   decomposition: `Form([T("|"), C(Pat(pat)), T("=>"), C(Exp(body))])`.

4. **Module item FocusMod vs FocusExp**: When a bare chain step `M/x`
   has no trailing slash, it should focus the whole ModLet item as
   `FocusMod(ModLet(pat, def))`, not just `FocusExp(def)`. With trailing
   slash `M/x/`, it enters the definition. The `find_binder_defs` function
   must return the MODULE ITEM for bare name matches, and the DEFINITION
   for chain-enter matches. This is a key distinction.

5. **Implicit focus rules** (context-sensitive):
   - No `%` + last element is a name → insert `%` before it (pattern focus)
   - No `%` + last element is `=` or `->` etc → append `%` (def focus)
   - `let x` → `let %x` (FocusPat), `let x =` → `let x = %` (FocusExp)
   - `fun x` → `fun %x`, `type T` → `type %T`, `module M` → `module %M`
   - `| A` → `| %A` (FocusPat for arm pattern)

6. **Float atom matching**: `\... 3.14` must match `Float(3.14)` even
   though `Atom.to_literal` may return `"3.140000"`. Need fallback to
   numeric comparison: parse both as float, compare values.

7. **Deduplication by focused_id**: When ellipsis or descent produces
   the same node via multiple paths, deduplicate by `focused_id`.
   Current impl uses a Hashtbl to track seen IDs. Must preserve
   "first occurrence wins" ordering (direct match before descended).

### Important — affects correctness

8. **Constructor/applied constructor pattern matching**: `| A => %`
   must match both nullary `A` and applied `A(x, y)` constructors.
   The pattern head is what matters, not the full pattern structure.
   `ListLit([])` must match as `[]`.

9. **Chain through shadows**: `a/a` enters each binding named `a` and
   searches for nested `a` inside. With `let a = (let a = 0 in 4)`,
   `a/a = %` finds the inner `0`. Must try ALL bindings, not just first.

10. **Shadowed binding multi-match**: `a = %` without index matches ALL
    bindings named `a`. `a#0 = %` matches only the first. Indexing is
    0-based and relative to the Let chain traversal order.

11. **Division `/` conflict**: The `/` token is chain syntax, so `/`
    (division) as an operator is NOT supported. Use `#0`/`#1` for
    division operands.

12. **BinOp string mapping**: Must use `Operators.bin_op_to_string` to
    convert `op_bin` variants to strings. Includes: `+`, `-`, `**`, `<`,
    `<=`, `>`, `>=`, `==`, `!=`, `&&`, `||`, `++`, `$==` and float
    variants (`+.`, `-.`, `*.`, `**.`, `/.`, `==.`, etc.).

13. **Double descent collapse**: `\... \...` should behave identically
    to `\...`. Handle in parser (collapse adjacent Descend wrappers).

14. **Ellipsis + separator transparency**: Ellipsis in sequences must
    skip both children AND separator tokens. When `_...` precedes a
    ChildNamed or Token, it tries matching at each position, skipping
    items and their separators.

### Design decisions — informed by existing impl

15. **Canonical named for BinOp**: Only generates operator-based path
    for direct operands (`e1` or `e2` is the target). Deeper nesting
    falls back to numeric. E.g., in `(1+2)+3`, canonical for `2` is
    numeric, not `_ + #0 _ + %`.

16. **Canonical shadowed names**: When multiple bindings share a name,
    canonical generates `x#0 = %`, `x#1 = %` etc. Uses `count_before`
    (how many same-named bindings precede this one) and `count_all`
    (total). Only adds index when `count_all > 1`.

17. **find_all_lets synthesizes module items**: When searching for
    `let x` inside `Module(items)`, the resolver synthesizes
    Let-like nodes from ModLet items with EmptyHole body. This is a
    smell but works. The rewrite should handle this cleanly by
    having `find_binder_defs` work with FocusMod directly.

18. **TPat → Pat conversion**: Type patterns (`TPat.Var("T")`) are
    converted to `Pat.fresh(Var("T"))` for display. This is needed
    because focus_target only has FocusPat, not FocusTPat.

19. **Filter/Projector/Closure transparency in child indexing**:
    These wrappers have 1 child and are transparent for `#N`.
    The decompose should handle them as `Transparent`.

### Spine descent (from plan Phase 5.3)

20. **Flat parser + spine descent in matcher**: The parser produces
    flat spine patterns. When matching `{ let x = %` against
    `Module([ModLet(...)])`, the matcher sees `Token("{")` matches
    the module's opening brace, then `Token("let")` doesn't match
    `PosChild(FocusMod(...))`. At this point the matcher tries
    entering the child and matching the remaining pattern against
    the child's decomposed spine. This is the key mechanism that
    makes the flat parser work.

    **Critical nuance**: The matcher must track that it has "entered"
    a child, so it knows that the ChildSel for this level is
    consumed. Otherwise it might try entering multiple children.

21. **Keyword matching order in modules**: Inside `{ let x = 1; let y = 2 }`,
    the selector `{ let x = %` matches x's definition. But `{ let y = %`
    must skip x to find y. The current impl handles this via
    `walk_seq_spine` which iterates through items. The rewrite handles
    this via spine positions + ellipsis/skip logic.

22. **Whole-form focus vs child focus**: `% let x` focuses the entire
    Let expression. `let x = %` focuses just the definition. These use
    different selector constructors:
    - `% let x` → `Focus(Some(Spine([Token("let"), ChildNamed("x")])))` — node itself
    - `let x = %` → `Spine([Token("let"), ChildNamed("x"), Token("="), ChildSel(None, Focus(None))])` — enters child
