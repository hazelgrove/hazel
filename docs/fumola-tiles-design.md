# Fumola in Hazel, as tiles

A design for `fumola-tiles-mvp`. Status: proposal, nothing implemented yet.

This branch integrates Fumola with Hazel the way `blackboard-mvp` integrates
Blackboard: as a sort of its own in Hazel's tile grammar, with its own AST
hanging off `Grammar.any_t`. It keeps the evaluation story of
`fumola-livelit-mvp`: Fumola source goes to the Fumola wasm runtime through
the `window.fumola` shim, and results come back as Hazel values.

## What actually changes

The two branches this draws on differ in *where Hazel holds the Fumola
program*.

In `fumola-livelit-mvp`, a Fumola program is a `string` in a livelit's model.
Hazel never looks inside it. The program reaches Fumola's own lalrpop parser,
and when it does not parse, the parser says so and the livelit shows a
message. Hazel's editor has nothing to say about a half-written `thunk {`,
because as far as Hazel is concerned the model holds a string that is
currently unhappy.

In this branch a Fumola program is a *term*: tiles in the editor, a
`FumolaGrammar.t` after `MakeTerm`, and a printed string only at the last
moment, on the way to the runtime. So:

- Unmatched delimiters are Hazel's business. `thunk {` with no `}` is a tile
  in the backpack, and the editor guides the fix, rather than a
  `ParseError` reported after the fact.
- Errors have locations in the editor, because every node carries an `Id`.
- The cursor inspector can say what a Fumola form is, the way
  `BbCursorInspector` does for Blackboard.
- Structure editing applies: a Fumola program can be built by inserting
  forms, not only by typing characters that happen to lex.

### Three corrections to the framing

**1. There is no parser to write.** "Re-implement the parser with Reason
tooling" is the right instinct but not quite the right shape. In the tile
route you do not write a parser at all — Hazel's tile engine is the parser.
What you author is:

| What | Where | Analogue on `blackboard-mvp` |
|---|---|---|
| the token/delimiter/precedence table | `Form.re`, `Precedence.re` | `Form.bb_get`, `P.bb_*` |
| the sort, so Fumola tiles stay closed | `Sort.re`, `Insert.effective_sort` | `Sort.Bb`, `Sort.is_bb` |
| segment → AST | `MakeTerm.re` | the `Bb` cases |
| AST → segment (pretty printing into the editor) | `ExpToSegment.re` | the `Bb` cases |

Blackboard *does* have a hand-written text parser, `BbParse.re`, but it is
not on the editor path — it reads the paper's line-based documents for the
kernel tests. The equivalent for Fumola (reading existing `.fumola` files
into tiles) is useful but optional; see M3.

**2. The obligation moves from parsing to printing.** The livelit route sends
text and lets Fumola's parser judge it. This route never sends text that
could fail to parse: the editor holds a well-sorted tree and a printer emits
concrete syntax from it. That makes the printer the thing that has to be
right, and it creates a contract the livelit route never had:

> Everything Hazel prints must be accepted by `fumola_parser`, and must mean
> there what it means here.

That is a testable claim and it should be tested against the real parser, not
against our belief about it — a golden corpus of Fumola terms, printed by
Hazel, parsed by the Rust parser in CI, compared structurally. A
pretty-printer that quietly drops a parenthesis is a class of bug the livelit
route could not have.

**3. The VM instance needs a home, and a tile tree has no model.** A Fumola
program runs against a **Fumola VM instance** — "instance" for short. The word
is not new here: the wasm API already types its handle as `FumolaInstanceId`,
and `fumola_create` / `fumola_has` / `fumola_drop` / `fumola_instance_count`
are all about instances. This document uses the boundary's own term.

The instance is where the adapton store lives, so it is the thing that has to
survive an edit. There is no Blackboard precedent, because `blackboard … end`
is inert: unknown type, checker run only from tests. Fumola is not inert.

On `fumola-livelit-mvp` the livelit model carries `instance_id` (a key into
the runtime's `sigma`) and `thunk_name`, and both persist across edits, which
is what lets a thunk keep its history. A `fumola … end` expression in the tile
tree has no model to carry them, and deriving a name from the expression's
`Id` reintroduces exactly the failure the livelit comment warns about: a name
taken from a Hazel id starts a new thunk whenever that id changes, losing the
history the thunk exists to keep.

**Decided: name the instance in the syntax.** The program says which instance
it runs against, and says it in text the programmer wrote:

```
fumola store in
  thunk { ... }
end
```

Stable by construction, visible in the program, survivable across a reload,
and it makes two blocks sharing one instance expressible — which the
alternatives (a livelit that owns the runtime, or one instance per editor)
either complicate or forbid.

Two spellings are still open and are cheap to change later:

- the middle delimiter, `in`, chosen to parallel Hazel's `let … in` and
  `use … in`. The form is then
  `mk_op_c(L, ["fumola", "in", "end"], Exp, [_, Fumola(Exp)])`.
- the sort of the instance name. `Pat` reuses existing tiles and TyDi and
  reads as a name, at the cost of looking like a binder it is not; a
  Fumola-sorted identifier tile is more honest and costs a form.

## What the Rust toolchain costs to iterate on

This was measured to inform the decision, not to justify it. The case for
tiles is the editing affordances above; these numbers are context for how the
day-to-day feels, and they are worth writing down because the difference is
larger than it feels from inside either workflow.

Measured on this laptop, 2026-09-11, caches warm throughout. A no-op build of
the wasm target is 0.1 s, so each figure is work the edit caused.

| Change | Command | Wall |
|---|---|---|
| Fumola grammar, parser crate only | touch `parser.lalrpop`; `cargo build -p fumola_parser` | **89.7 s** |
| Fumola grammar, through to the wasm artifact Hazel loads | touch `parser.lalrpop`; release wasm build of `fumola_wasm` | **104.1 s** |
| **A new prim** — one arm in `PrimFunction::resolve`, grammar untouched | same release wasm build | **109.5 s** |
| Hazel form table, rippling downstream | new binding in `Form.re`; `dune build src --profile dev` | **21.6 s** |
| Hazel, change confined to a module body | comment in `Form.re`; same | **0.66 s** |

The prim row is the one that matters, because it is the edit that actually
happens — roughly weekly — while Fumola's *syntax* stays mostly put. Adding a
prim does not touch `parser.lalrpop` at all, and lalrpop does not regenerate
its table: `build.rs` sets `rerun-if-changed` on the grammar file only. But
`fumola_parser` still recompiles, because it depends on `fumola_syntax`, and
what it recompiles is the generated LR code for an 834-line grammar. So a
one-line prim addition pays essentially the full grammar-edit price: 109.5 s,
reproduced on the revert.

### The human factor

Two minutes is not a number you feel as two minutes. It is long enough to
lose the thread and go read something else, and short enough that you sit
there. Paid once it is nothing; paid on every iteration of a prim you are
designing by trying, it is the thing that quietly decides how many variations
you try. Sub-second turnaround on the Reason side changes what kind of
exploration is practical — not because the total time is smaller, but because
it stays under the threshold where attention survives.

This cuts differently for agent-driven work, where nobody is losing a train of
thought and a two-minute build is a two-minute build. So this is a real
consideration, but a human-workflow one, and it is worth being clear about
which of the two is doing the work at the time.

Two things this does *not* claim. It does not compare running programs — both
routes need the wasm runtime, and that cost does not go away. And Hazel's
21.6 s is a dev-profile incremental build; `dune build @src/fmt` and the test
suite cost more.

## Scope: which Fumola

Fumola's surface is Motoko-derived. `parser.lalrpop` is 834 lines and covers
objects, classes, actors, generics, candid, async/await, quoted ASTs, and a
full type sublanguage. Tiles will not cover that, and should not try.

Three features of the concrete syntax are actively hostile to tiles, and are
worth naming now because they shape the subset:

- **`#` is overloaded three ways**: variant prefix (`#tag`), the concatenation
  binop, and the attribute opener `#[`. Tiles pick a form by token, so these
  have to be distinguished by adjacent delimiters or respelled.
- **`<` and `>` are both relops and type-argument brackets.** lalrpop resolves
  this with context the tile engine does not have.
- **`" << "` is a token with literal spaces in it** — a lexer hack with no
  tile equivalent.

### Milestones

**M0 — kernel and printer, no tiles.** `FumolaGrammar` (the editor AST,
polymorphic in the annotation), `FumolaTermBase` (the same at `IdTagged`),
and `FumolaPrint` (AST → concrete Fumola source). Round-trip tests against
the real `fumola_parser`. This is the piece everything else depends on and
the piece that can be tested hardest.

**M1 — the sort and its tiles.** `FumolaSort`, forms in `Form.re`,
precedences, `MakeTerm` and `ExpToSegment` cases, `fumola <instance> in …
end` as an expression form, closed under `Insert.effective_sort`. Subset:

- literals, variables, tuples, parens
- `#tag` and `#tag e`; `?e`
- arithmetic, comparison, `not`
- `let p = e; …`, blocks `{ … }`
- `func f(p) { … }`, application `f(e)`, projection `e.x` and `e.0`
- `if`/`else`, `switch … { case … }`
- arrays `[…]`, index `e[i]`
- **the adapton core**: `thunk { e }`, `force e`, `e := e`, `@ e`,
  `do @ e1 e2`, `do goto d e { … }`, `do within d e { … }`, `` `t ``

Deferred: modules, classes, actors, `import`, attributes, the type
sublanguage, quoted ASTs, async. (`import` and `module` come back in M3 if we
want to load the existing `.fumola` corpus.)

**M2 — evaluation and the value bridge.** `fumola <instance> in … end` prints
its program, hands it to the shim against the named instance, and gets a value
back. The instance is created on first use and looked up by name thereafter,
so the store survives every edit that leaves the name alone. `FumolaValue.re` and
`FumolaSource.re` port over from `fumola-livelit-mvp` unchanged — they are
about values, not livelits, and carry no livelit dependency. Statics: give
the expression a type rather than leaving it `Unknown`, which is where
Blackboard stopped.

**M3 — the event list.** A section of the right-hand panel, beside errors and
probes, showing the adapton event list of the instance the cursor is in --
`addNode`, `addEdge`, `updateEdge`, `removeEdge`, and the Begin and End of
each force -- and, when the cursor is not in one, how to make one.

The events are fetched by running `prim "adaptonPeekHistory" ()` in the
instance through the shim that is already there, rather than by adding a wasm
export. That keeps the boundary source text, as everything else here does,
and avoids the ~110 s Rust turnaround measured above for what is a UI change.

This is why the Fumola runtime stays on the main thread: the panel is a
main-thread view of an instance, and it can ask the runtime directly. Putting
the runtime in the worker -- the only way to let a `hazel … end` carry a
*bound* variable rather than a value written in place -- would put the store
behind a worker round trip, and shape this panel's whole design around it.
That trade is written up in src/language/fumola/README.md.

**M4 — reading existing Fumola.** A text parser, `FumolaParse.re`, so the
`.fumola` corpus can be opened as tiles. Only worth doing once M0's printer
is trusted, because parse-then-print is the round trip that proves both.

## Branch

`fumola-tiles-mvp`, off `dev`, in `~/hazel-fumola-tiles`.

Off `dev` rather than off `fumola-livelit-mvp`: that branch carries ~950
lines of livelit changes this feature does not want, and the two pieces we do
want (`FumolaValue.re`, `FumolaSource.re`, and the `window.fumola` shim in
`prebundle.js`) are self-contained and can be cherry-picked in M2.

## Open questions

1. Which spelling for the instance name — the middle delimiter, and whether
   the name is `Pat` or a Fumola-sorted identifier (above). Does not block
   M0.
2. Does `fumola … end` evaluate eagerly at every keystroke, as the livelit
   did, or on demand? The livelit's answer was "every edit, and suppress
   syntax errors as noise". Here there are no syntax errors to suppress, so
   eager evaluation becomes more defensible — and more expensive.
3. Do we want Fumola's *type* syntax as tiles at all, or does Fumola stay
   untyped in Hazel for now and let the runtime complain?
4. Is the subset in M1 enough to write the examples we care about, or does
   `module { … }` have to come earlier because every real `.fumola` file
   starts with one?
