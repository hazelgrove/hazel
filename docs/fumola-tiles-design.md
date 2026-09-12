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

### Milestones: the plan, and what happened

All five landed. What each cost, and what it turned up, since the surprises
are the part worth keeping:

| | what it took | what it turned up |
|---|---|---|
| **M0** | `FumolaGrammar`, `FumolaTermBase`, `FumolaPrint`, 56 tests, a round-trip script | three grammar facts intuition gets wrong (above); and a first version of the script that compared each program *to itself* and so proved nothing |
| **M1** | a sort, a form table, a precedence ladder, `MakeTerm`, `ExpToSegment`, 55 tests | `#tag` is untokenizable; application has no token; only one form per opening keyword; `fumola` shadowed `fun` in completions |
| **M2** | `FumolaRun`, `FumolaValue`, `FumolaSource`, `FumolaTools`, the wasm shim | instance-by-name verified against the real shim; changing a mode resets the store; the livelit's pointer widget has no tile counterpart and was dropped |
| **M2.5** | attempted: run during evaluation so escapes carry bound variables | **abandoned.** The worker computes the result and has no shim. The traversal fix it needed was kept; the move was not |
| **M3** | `FumolaEvents`, `FumolaSidebar`, a panel, CSS | history reachable as a prim, so no wasm export; a cursor on whitespace names no term, which emptied the panel while typing |
| **M4** | `FumolaParse`, 107 tests | the round trip checks *structure* but is blind to a wrong precedence level, because parser and printer share the ladder; the corpus needs types, not a parser |
| **docs** | four slides, a `Fumola (Tiles)` deck | two slides shipped Fumola that had never been run; a third broke on `#` inside a Hazel comment |

Three things were reverted or redone rather than shipped: the M2.5 move, a
pretty-printer that rendered unbuildable forms as their nearest printable
part (an `if` as its condition), and a round-trip check with no power. Each
is recorded where it happened rather than tidied away.

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

**M2 — evaluation and the value bridge.** `fumola <mode> as <instance> in … end` prints
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

**M4 — reading Fumola back.** `FumolaParse.re`: a lexer and a
precedence-climbing parser over the M1 subset, sharing the printer's ladder.
Its value is the round trip -- `print(parse(s)) == s` and `parse(print(t)) ==
t` -- which checks *structure*, where the script against the real Fumola
parser checks grammaticality and meaning. Because the ladder is shared, the
round trip cannot see a wrong precedence level; only the script can. Neither
replaces the other.

Reading the shipped `.fumola` corpus turns out not to be a parser problem.
All fourteen files begin `module` or `import` and are made of `public func`s
with type annotations, and modules, imports, attributes and the type
sublanguage are all absent from this AST by design. That waits on types,
which is a decision of its own rather than a milestone.

## Branch

`fumola-tiles-mvp`, off `dev`, in `~/hazel-fumola-tiles`.

Off `dev` rather than off `fumola-livelit-mvp`: that branch carries ~950
lines of livelit changes this feature does not want, and the two pieces we do
want (`FumolaValue.re`, `FumolaSource.re`, and the `window.fumola` shim in
`prebundle.js`) are self-contained and can be cherry-picked in M2.

## Implementation notes

Things that were not visible from the design, found while building it. Each
was measured or checked against the real thing, not reasoned out.

### Fumola's grammar, where intuition is wrong

- **`|`, `&` and `^` bind tighter than `+` and `*`.** The chain is
  `or < and < rel < add < mul < bitor < bitand < xor < shift < pow`, so
  `1 | 2 + 3` is `(1 | 2) + 3`. C says the opposite.
- **The adapton forms are looser than every operator.** `force e`, `@ e`,
  `thunk { … }`, `e := e` and the `do` family live at `ExpNonDec`, below the
  whole binary chain — so `force x + 1` is a *syntax error*, not a misparse,
  and parentheses there are required for the program to parse at all.
- **`{ … }` means two different things by position.** A block after
  `if`/`else`/`thunk`/`do`/`func`; an object literal everywhere else, so
  `let x = 5; { x }` is the record `{x = 5}`. Both parse. Only evaluating
  them tells them apart.
- **`:=` returns a pointer, and `@` reads one.** `@ 0` is a type error; a put
  and a read are two steps joined by the pointer the put hands back. Two
  documentation slides had this wrong because they were written and not run.

### What Hazel's tokenizer forbids

- **`#tag` cannot be a tile.** `#` is Hazel's comment delimiter, so `#tag` is
  neither an operand nor an operator token. Measured against
  `Token.is_potential_operand`: `$tag`, `'tag`, `?tag` and `^tag` are
  operands; `#tag`, `@tag` and `+tag` are not. `?` and `^` are already holes
  and the livelit prefix, which leaves `$`. The printer puts the `#` back.
- **The same delimiter bites the documentation.** A Hazel comment is
  delimited by `#`, so a Fumola tag cannot be written inside one — which
  broke a slide that tried to explain tags.
- **Application is juxtaposition, which has no token to hang a tile on.** The
  tile is `f(a)`; Fumola reads that as application to a parenthesized
  argument, so it means the same thing.
- **Only one form can expand from a token.** `Form.Expansion` resolves a
  token and sort with `find_opt`, so `fumola … in … end` and
  `fumola … as … in … end` cannot both be reachable by typing. That is why
  the mode is a slot rather than an option.
- **A new keyword can shadow a Hazel one.** `fumola` sorts before `fun`, so
  typing `fu` completed to `fumola` until the Fumola keyword was kept out of
  Hazel's completions. The full suite caught this; nothing else would have.

### Case conversion, which is a boundary problem and not a detail

Fumola inherits Motoko's convention — a variant tag is lowerCamelCase,
`#leaf`, `#addNode`, `#forceBegin` — and Hazel's constructors are
UpperCamelCase, `Leaf`, `AddNode`, `ForceBegin`. So every value crossing the
boundary is recased, and that has to happen in **both** directions or the
crossing is not a round trip.

It did not. `FumolaValue` capitalised on the way in and `FumolaSource` left
the name alone on the way out, so a value read from Fumola as `#leaf`, shown
in Hazel as `Leaf`, went back as `#Leaf` — a different tag, silently. Neither
direction was tested against the other, so nothing caught it. `FumolaCase` is
now the one place the convention lives, and both directions go through it.

This is the narrowest instance of a friction that recurs wherever these
languages meet — Motoko, Rust and Fumola have three conventions between them
(lowerCamel, UpperCamel, snake_case) and no single answer about which
crossings are lossless. What makes *this* crossing tractable is that only the
first letter moves. `FumolaCase.round_trips` says when even that is lossy: a
Fumola tag that already begins upper-case (`#Leaf`) comes back as `#leaf`,
and a caller that cares should ask rather than assume.

A related case is visible in the editor rather than the bridge: a Fumola
value arriving as `Leaf(1)` is marked *unbound constructor* unless a Hazel
type in scope declares it. Declaring `type Tree = + Leaf(Int) + Bin((Tree,
Tree))` both silences the mark and tells the bridge which sum to build, since
the expected type is what `resolve_ctr` consults.

### Where things run, and what that costs

- **A cell's result is computed in a web worker**, which has no
  `window.fumola`. Instrumenting the evaluator reports `runtime=absent
  ctx=worker` for the shown result and `runtime=present ctx=main-thread`
  beside it. `async_evaluation: false` does not mean "no worker".
- So a Fumola program runs during **elaboration**, on the main thread. The
  cost is that a `hazel … end` carries a value written in place and not a
  bound variable, since nothing has been substituted yet. The benefit is one
  runtime, on the thread where the event panel can ask it.
- **The traversals had to be taught to enter a Fumola term.** `TermBase`
  skipped them, so substitution never reached the Hazel expressions inside a
  `hazel … end`. That is fixed independently of where running happens.
- **Changing an instance's mode resets it**, discarding the store:
  `ensureMode` answers `reset: false` for the mode an instance already has
  and `reset: true` for a different one. A hole in the mode slot therefore
  means *leave this instance alone*, not *apply the default*.

### The library is reachable, and the tiles cannot reach it yet

The wasm runtime ships **18 modules**, `fumola/collections/levelTree` among
them, and `import L "fumola/collections/levelTree"` works through the shim
today — `L.fromList` builds a real level tree from random input. What the
tiles lack is the syntax to say it: `import`, string literals, and `.`
projection are not tile forms. That gap, not the runtime, is what stands
between the documentation and real library examples.

### Build costs, for choosing where to put a change

| change | wall |
|---|---|
| a new Fumola prim (grammar untouched) → wasm | 109.5 s |
| `parser.lalrpop` → wasm | 104.1 s |
| Hazel `Form.re`, rippling downstream | 21.6 s |
| Hazel, change confined to a module body | 0.66 s |

This is why the event panel fetches history by running
`prim "adaptonPeekHistory" ()` through the existing shim rather than by
adding a wasm export.

### Smaller traps

- **CSS: one rule per sort, and a missing one is invisible.** Fumola had no
  rule, so its tokens inherited the ground's near-white and an indicated
  shard filled **black** — an SVG `path` with no `fill` is black.
- **`python -m http.server` sends no cache headers**, so a browser will not
  revalidate a changed stylesheet. Editing CSS and reloading shows the old
  one.
- **Doc slides are cached in IndexedDB by name.** A changed slide keeps
  serving its old text until the `doc:` keys are deleted — including
  `doc:_meta`, which is what hides newly added slides.

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
