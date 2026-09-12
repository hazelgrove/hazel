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

**2. Not every punctuation mark can be an operator.** `Token.ascii_operator_chars`
is a fixed set, and `$` is not in it — so although `$tag` lexes fine as an
*operand*, a bare `$` cannot lex as an *operator* at all. When we needed a
spelling for concatenation, `$` was the obvious candidate for consistency with
`$tag`, and it was not available. Adding `$` to that set would change how every
token in every Hazel program lexes, which is not a trade worth making for one
operator in one sub-language.

We took `++`: free in Fumola, reads as concatenation, and shared with Hazel's
own `++` — which is fine, because **forms resolve by token *and* sort**. `+`
was already shared this way. A closed sort may reuse a spelling.

**3. Some tokens are reserved by an existing ambiguity.** Every token
beginning with `>` is restricted by `Token.is_potential_token` to a fixed list,
to keep type application (`map@<a>`) unambiguous. So Fumola's `>>` and `<>>`
are not merely unimplemented — they are **blocked** until that ambiguity is
resolved, while their mirror images `<<` and `<<>` are available. An
asymmetry like that is worth discovering before promising a milestone.

**4. One token expands to one form per sort.** `Form.Expansion` resolves a
token and a sort with `find_opt`, so `fumola … in … end` and
`fumola … as … in … end` cannot both be reachable by typing. That is why the
adapton mode is a *slot* rather than an option. The same rule made `?e`
impossible — `?` is Hazel's explicit-hole token — and makes unary minus
unreachable, since a leading `-` lexes into the numeric literal before any
form is consulted.

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

**Names crossing at all.** Fumola runs during elaboration, before substitution,
so `hazel … end` carries a value written in place and not a bound variable
([#2537](https://github.com/hazelgrove/hazel/issues/2537)). Lifting that means
moving the runtime into the worker, which puts the store behind an async
boundary and takes it out of reach of the event panel. That is the deepest
open question here, and it is about *evaluation order*, not about tiles.

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
