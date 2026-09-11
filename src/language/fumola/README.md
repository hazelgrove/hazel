# Fumola in Hazel (M0)

The editor's Fumola terms, and the printer that turns them into Fumola
concrete syntax. No dependence on tiles or the web layer yet; the sort and its
forms are M1. Design: [`docs/fumola-tiles-design.md`](../../../docs/fumola-tiles-design.md).

| Module | Role |
|---|---|
| `FumolaGrammar` | the shape of editor terms, polymorphic in the annotation |
| `FumolaTermBase` | the same at `IdTagged`, i.e. with ids |
| `FumolaPrint` | terms to Fumola source, and `has_hole` for terms that have none |

This is the M1 subset of Fumola, not all of it. Absent on purpose: objects,
classes, actors, generics, candid, async, quoted ASTs, and the type
sublanguage.

## The printer is the contract

In the livelit route Fumola's own parser judged the program text, and said so
when it could not parse. Here the editor holds a well-sorted tree and nothing
ungrammatical can reach the runtime by accident — provided the printer is
right. So the printer carries the whole obligation:

> Everything printed must be accepted by `crates/fumola_parser`, and must mean
> there what it means here.

Three facts about Fumola's grammar shape the printer, and each was confirmed
against the real parser rather than read off the `.lalrpop` and believed:

1. **`|`, `&` and `^` bind tighter than `+` and `*`.** The chain is
   `or < and < rel < add < mul < bitor < bitand < xor < shift < pow`, so
   `1 | 2 + 3` is `(1 | 2) + 3`. C says the opposite.

2. **The adapton forms are looser than every operator.** `force e`, `@ e`,
   `thunk { … }`, `e := e` and the `do` family live at `ExpNonDec`, below the
   whole binary chain. They are not merely low-precedence: `force x + 1` and
   `1 + thunk { 2 }` are *syntax errors*, because an operator's operands come
   from a nonterminal that cannot reach them. Parentheses there are required
   for the program to parse at all.

3. **`{ … }` means two different things by position.** After `if`, `else`,
   `thunk`, `do` or `func` it is a block. Anywhere else it is an object
   literal: `let x = 5; { x }` evaluates to the record `{x = 5}`, not to `5`.
   Both parse, so a check that only asks "does this parse?" cannot see the
   difference. The printer therefore never emits bare braces outside a nest
   position — a block elsewhere goes out as `do { … }`, which is a block
   wherever it stands.

## Where the tile surface differs from Fumola's (M1)

The tile spelling is not always Fumola's spelling, because Hazel's tokenizer
forbids some of it. The printer translates, and the round-trip script is what
holds the translation honest.

| Fumola writes | the tile is | why |
|---|---|---|
| `#tag`, `#tag e` | `$tag`, `$tag(e)` | `#` is Hazel's comment delimiter, so `#tag` is neither an operand nor an operator token and cannot be a tile at all. `$` is a name character here and is unused in Fumola's grammar. |
| `f a` | `f(a)` | tiles need a token for every form, and juxtaposition has none. Fumola reads `f(a)` as application to a parenthesized argument, so the two mean the same thing. |
| `{ … }` as a block | `{ … }` | same spelling, but only a block in a nest position; see above. |

Checked, not assumed: `#tag` is `neither` to `Token.is_potential_operand` and
`is_potential_operator`, while `$tag`, `'tag`, `?tag` and `^tag` are all
operands. `?` and `^` are already spoken for -- holes, and the livelit and
projector prefixes -- which leaves `$` and `'`.

Note that Fumola's `#tag` is a *value*, an injection into a variant, not a
declaration of one. Hazel's `+` infix declares the arms of a sum *type*, which
is a different level and belongs with the type sublanguage, if that ever
arrives.

## Hazel inside Fumola (`hazel … end`)

`fumola <instance> in … end` takes Hazel into Fumola; `hazel … end` takes it
back. Inside a Fumola program it is a Fumola form whose child is sort `Exp`, so
what is written there is Hazel, edited as Hazel, with Hazel's statics and
completion. `FumolaSource` renders the value as Fumola source on the way out.

This is what the livelit's `input` was, with two differences that follow from
being a form rather than a model slot. The livelit carried one value, at the
boundary of a string Hazel could not see into. A `hazel … end` is a tile
subtree, it can stand anywhere a Fumola term can, and a program can have as
many as it likes:

```
fumola store in hazel 1 end + hazel 2 end end     -->  (1) + (2)
fumola store in force hazel 1 end end             -->  force (1)
```

The rendered value is always parenthesized, so that whatever it produces
cannot regroup the Fumola around it.

Two things had to give way for this to work:

- **`Exp` is a subsort of `Fumola(Exp)`** in `Segment.subsort_of`. Without it
  the remolding template keeps going in Fumola past the escape, and a Hazel
  tuple written inside one comes back as a *Fumola* tuple wrapped in a hole --
  which type-checks, prints, and is the wrong program. Blackboard never needed
  this, because `blackboard … end` only goes one way.
- **`FumolaGrammar` is generic in the host term it embeds.** Hazel's `Grammar`
  already names `FumolaGrammar`, so naming `Grammar` here would close a cycle;
  instead the AST takes the host expression as a type parameter and `Grammar`
  supplies its own `exp_t`. `FumolaPrint` correspondingly takes the function
  that renders one. `Fumola.re` is where Hazel joins the two, and is what
  callers in Hazel should use.

## How the contract is checked

Two checks, because agreeing with ourselves is not evidence.

`test/fumola/Test_FumolaPrint.re` ties the expectations to the printer: each
corpus term prints to the string the test claims. Pure and fast, runs with the
suite.

[`scripts/check-fumola-roundtrip.sh`](../../../scripts/check-fumola-roundtrip.sh)
ties the printer to Fumola, using the two corpus files the test writes:

- **`fumola check` accepts every printed line.** Catches ungrammatical output,
  such as a missing parenthesis around an adapton form.
- **`fumola eval` agrees between the minimal and the fully explicit spelling
  of each term.** Catches a precedence level read wrong, where both spellings
  parse and only their meanings differ — invisible to the first check.

Both corpus files are written by the printer, not from the test's
expectations; writing the expectations would have the script checking that
file rather than the code.

The second check was verified to have power by breaking it on purpose: setting
`p_bitor` to the C-like level makes it report

```
  printed:  1 | 2 + 3
     => 1|2+3
  explicit: (1 | (2 + 3))
     => 1|5
```

An earlier version of that check compared each program against itself wrapped
in parentheses, which passes for every input and proved nothing. If the check
is ever weakened again, the `agree == 0` guard at the end is the thing that
notices.

Running it needs a built Fumola binary:

```
cd ~/fumola && cargo build -p fumola
dune build test/haz3ltest.bc.js && bash test/run_node.sh test FumolaPrint
./scripts/check-fumola-roundtrip.sh
```
