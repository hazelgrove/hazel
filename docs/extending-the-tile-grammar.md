# Extending the tile grammar to an exotic sub-language

Written for Cyrus, and for anyone who wants to put another language's concrete
syntax into Hazel's editor as *tiles* rather than as a string in a widget.

Three sub-languages now do this: the ALFA derivation language, Blackboard, and
Fumola. Fumola is the newest and the most foreign — a Motoko-derived surface
with its own operator ladder, its own literal forms, and an adapton core with
no Hazel counterpart — so it has hit the most walls, and this is the record of
which walls are real, which are cheap, and what we decided when we hit them.

It is the design document for the work tracked in
[#2538](https://github.com/hazelgrove/hazel/issues/2538).

## What landing one form actually costs

This is the encouraging part. A form already modelled in the sub-language's
AST costs four small edits and no new semantics:

1. a constructor in the form table (`Form.re`) — the tile's tokens, its sort,
   its precedence
2. a case in `MakeTerm` — tiles to AST
3. a case in `ExpToSegment` — AST back to tiles
4. tests: a corpus entry, and for anything with precedence, a run of the
   round-trip script against the real parser

Twelve forms landed in one pass this way: arrays and indexing, concatenation,
`e!`, `not`, `assert`, `ignore`, `return`, `prim`, `if`/`else`, quoted names.
The constraint is never the semantics. It is always the **concrete syntax**.

`switch` and its patterns landed later and cost slightly more — one
constructor in the sub-language's own AST — for a reason worth reading before
you add a form with sub-structure of its own; see *A form with structure
inside it* below.

## The real constraint: Hazel's tokenizer is shared

A sub-language does not get its own lexer. Every token in the document is
lexed by one tokenizer before any sort is known, so the sub-language may only
spell a form in a way Hazel can already lex. Four rules, learned the hard way:

**1. `#` is Hazel's comment delimiter, and Fumola uses it twice.** Fumola
writes a variant `#tag` and concatenation `e1 # e2`. Neither can be a tile.
The variant is spelled `$tag`, and the printer puts the `#` back. That trick
is now load-bearing and generalizes: *a tile spelling need not be the
sub-language's spelling, as long as the printer is the only thing the runtime
sees.*

It also has a cost nobody predicts: **a `#` cannot appear in slide prose
either**, because a Hazel comment is delimited by it. Writing *about* a Fumola
tag inside a documentation slide ends the comment and turns the rest of the
sentence into code. That has now broken three slides, twice in a slide whose
subject was this exact problem.

The same wall means **a slide cannot cite an issue number**. A reference slide
that wants to say "the rest is tracked in" has to name a file path and let
that file carry the link.

**2. Not every punctuation mark can be an operator.** `Token.ascii_operator_chars`
is a fixed set, and `$` is not in it — so although `$tag` lexes fine as an
*operand*, a bare `$` cannot lex as an *operator* at all. When we needed a
spelling for concatenation, `$` was the obvious candidate for consistency with
`$tag`, and it was not available. Adding `$` to that set would change how every
token in every Hazel program lexes, which is not a trade worth making for one
operator in one sub-language.

The rule costs more than `$`. Fumola's xor is `^`, which is not an operator
character either — it leads Hazel's livelit and projector prefixes — so that
operator has no reachable spelling at all: `1 ^ 2` lexes as `1 _ ^ _ 2`, with
grout where the operator should be. A character in this position lexes
perfectly well and still cannot be an operator, which is why the failure looks
like nothing in particular.

We took `++`: free in Fumola, reads as concatenation, and shared with Hazel's
own `++` — which is fine, because **forms resolve by token *and* sort**. `+`
was already shared this way. A closed sort may reuse a spelling.

**3. Some tokens are reserved by an existing ambiguity.** Every token
beginning with `>` is restricted by `Token.is_potential_token` to a fixed list,
to keep type application (`map@<a>`) unambiguous. So Fumola's `>>` is not
merely unimplemented — it is **blocked** until that ambiguity is resolved:
`1 >> 2` lexes as `1 > _ > 2`, two operators with a grout hole between them.
Its mirror `<<` is available, as are `<<>` and `<>>`.

Which spelling falls on which side cannot be read off by inspection, and we
got it wrong. This paragraph and the comment in `ExpToSegment` both named
`>>` and `<>>` as the blocked pair until the predicate was run over all six
operators: `<>>` types cleanly and lexes as one token, and the operator
genuinely blocked alongside `>>` is `^`, by rule 2 above. An asymmetry like
that is worth **measuring** before promising a milestone, and measuring it
takes twenty lines against `Token.is_potential_token` and `Parser.to_segment`.

**4. One token expands to one form per sort.** `Form.Expansion` resolves a
token and a sort with `find_opt`, so `fumola … in … end` and
`fumola … as … in … end` cannot both be reachable by typing. That is why the
adapton mode is a *slot* rather than an option. The same rule makes `?e`
impossible: `?` is Hazel's explicit-hole token.

Unary minus was listed here too and does not belong. `-x` is a prefix tile and
works; what a leading `-` does is join a *numeral*, since the int token is
`^-?\d+[0-9_]*$`, so `-1` is the literal rather than a negation of `1`. Those
mean the same thing, so nothing is lost — but the claim that the form was
unreachable was not measured, which is the mistake rule 3 is about.

## Decisions we made, and why

**Invent a tile spelling rather than bend the tokenizer.** `$tag` for `#tag`,
`++` for `#`, `` `t` `` for `` `t ``. The printer is the contract; the runtime
never sees the tile spelling. The cost is that a reader meets a spelling the
sub-language's own documentation does not use, so each one has to be said out
loud on a slide.

**Borrow Hazel's keyword when the sub-language has no token to borrow.**
Fumola's conditional is `if c { t } else { e }` — no `then`. A prefix tile
needs a token between the condition and the first branch, so the tile is
`if c then t else e` and the printer emits Fumola's form. Borrowing `then` is
less to explain than inventing a delimiter.

**Render an unbuildable form as a hole, never as a nearest printable part.**
An early version rendered an `if` as its condition when no `if` tile existed.
It was reverted: it drops program structure silently, and the editor cannot be
talked out of the wrong reading afterwards. A hole is honest, and both the
editor and `has_hole` already treat it as incomplete.

**Keep the AST ahead of the tiles.** Every form listed as missing in #2538 is
already in `FumolaGrammar` with a printer case and round-trip coverage. That
is why landing one is cheap. The exception proves the value of the rule: `do ?`
is *not* in the AST, so it needs a constructor before it needs a tile — and
that distinction is invisible unless the tiers are written down.

## A form with structure inside it

Every form above is flat: an operator, a prefix, a bracket pair. `switch` was
the first with structure of its own — a *sequence* of *pattern*-and-body pairs
— and the instinct was to model both notions with new sorts, on Hazel's own
pattern: a `Fumola(Pat)` sort for the patterns and a `Fumola(Rul)` sort to
chain the cases, mirroring Hazel's `Pat` and `Rul`.

Both were written and both were deleted, because both notions already existed
in a usable form:

- **a pattern position** is `Fumola(Exp)` read as a pattern by
  `MakeTerm.fumola_pat_of` — which is what the binding position of a `let`
  already was;
- **a sequence** is a `;`-chain of declarations — which is what a block
  already was, with `fumola_decs` already reading one.

So a case is spelled as a *declaration*: `case p => b`, separated by `;`
exactly as a block's contents are. The whole form cost **one constructor** in
the sub-language's AST (`DCase`) and **no new sorts**, and one machine reads a
switch's cases and a block's contents alike.

**Look for the existing reading before adding a sort.** Two sorts would have
meant two answers to "what is a pattern here" and two to "what is a sequence",
and the tile grammar is small enough that a second answer is a liability
rather than a generalization.

It helped that Fumola separates its own cases with `;`, so that spelling is
its own rather than borrowed. `=>` is borrowed from Hazel by rule 1 above.

### The invariant that was true only because a form was missing

`has_hole` never looked at a pattern. It did not need to: a `let` binder that
is not a name is already a hole at the term level, so no pattern position
could hold one. A case pattern can. It printed as `case ?□ …` — which the real
parser rejects — while `has_hole` reported the program printable, so the guard
that checks every shipped slide would have waved it through.

This is the failure mode the tier lists in
[#2538](https://github.com/hazelgrove/hazel/issues/2538) cannot capture: not a
form that is missing, but **an invariant that held only because a form was
missing**. Worth asking of any form you add: what was true of every existing
form, only because this one did not exist?

## What the tests can and cannot see

Three layers, and they catch different things. Using only one gives false
confidence, which has happened here more than once.

| check | catches | blind to |
|---|---|---|
| `print(parse(s)) == s` round trip | a form either side mishandles | a wrong precedence *level* — parser and printer share the ladder |
| `scripts/check-fumola-roundtrip.sh` (real `fumola_parser`) | ungrammatical output, and misgrouping (it evaluates both a minimal and a fully-parenthesized spelling and compares) | anything that parses but does not reduce |
| the app | what a reader actually meets | nothing is automatic |

Two concrete misses worth remembering. A slide shipped whose example escaped a
bound Hazel variable — the text round-trip passed, because the text was fine;
what was broken was what the text *meant*. And `fumola check` accepts
`if 1 < 2 10 else 20` while it evaluates to nothing, because Fumola's condition
must be an atom — acceptance is not meaning, which is exactly why the
round-trip script evaluates as well as parses.

There is now a guard asserting no shipped slide holds a program the printer
refuses. It needs no runtime, because the printer refuses *before* the runtime
is consulted and says something the runtime cannot — which is what separates
*this slide is wrong* from *this runner has no Fumola*.

## The boundary that is not syntax

Two things cost more than any form, and neither is a tokenizer problem.

**Values crossing back.** A sub-language's values have to become Hazel values.
Fumola does not reduce every operator — `1 | 2 + 3` stays symbolic — so the
translation must handle a symbolic tree, not just literals
([#2539](https://github.com/hazelgrove/hazel/issues/2539)). And when a symbol
becomes a *name*, its text must be **injective**, not pretty: rendered without
parentheses, `(1 | 2) + 3` and `1 | (2 + 3)` collide on one name and two
distinct cells become one.

**Names crossing at all.** This was the deepest open question here and it is
now closed, which is worth recording because of *how* it was closed
([#2537](https://github.com/hazelgrove/hazel/issues/2537)).

Fumola ran during elaboration, before substitution, so `hazel … end` carried a
value written in place and not a bound variable. Lifting it looked like it
meant moving the runtime into the worker — putting the store behind an async
boundary and out of reach of the event panel — because a cell's result is
computed in a worker and the worker has no `window.fumola`. That measurement
was right. The inference from it was not: **the worker is a choice per cell,
not a fact.** `EvalResult.calculate` has always taken
`~queue_worker: option(…)`, and `None` routes a cell through
`WorkerServer.evaluate_sync` on the main thread. A cell whose statics pass saw
a Fumola term now goes that way; every other cell keeps the worker.

The lesson is not about tiles at all: a measurement of what *is* happening had
been read as a constraint on what *could*. It cost a milestone
(`M2.5` in `docs/fumola-tiles-design.md`, recorded as abandoned and then done).

What remains open at this boundary is narrower and worth knowing: a reference
that comes into Hazel can go back out, but there is no way to *say* "the value
it holds" ([#2548](https://github.com/hazelgrove/hazel/issues/2548)), and
instance names are not first-class, so Hazel code over Fumola cannot be reused
across instances ([#2549](https://github.com/hazelgrove/hazel/issues/2549)).

## If you are adding the next sub-language

- Model the AST first and completely, with a printer and a round trip against
  the real parser. Tiles after. The AST is where the language's shape lives;
  tiles are a spelling question.
- Write down which forms you are withholding, in tiers — *in the AST with no
  tile* versus *not modelled at all* — because the two cost very different
  amounts and readers cannot tell them apart.
- Budget for the tokenizer, not the grammar. Every wall hit so far was a
  lexing wall.
- Give the sort a colour and a cursor-inspector class early. A sort with no
  CSS rule renders an indicated shard **black**, and a term with no info-map
  entry reports as *Whitespace or Comment* — both look like deep bugs and are
  one-line omissions.
- Prefix your slide filenames. Every `.hz` is copied into one flat directory
  and blobbed by basename, so `overview.hz` is a name, not a path.
