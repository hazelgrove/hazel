# Experimental language integrations

Three sub-languages embedded in Hazel — Blackboard and Fumola twice over — in
one build, on top of the user-defined-livelit and modules work. This is the
design document for the branch `experimental-lang-integration`
([#2536](https://github.com/hazelgrove/hazel/pull/2536)); it is written for
whoever picks the work up next, human or agent, and it records what the
integration turned up rather than only what it intends.

## The split, and why it only goes one way

There are two work products, and the dependency is deliberate.

**The mainstream one** is
[#2526](https://github.com/hazelgrove/hazel/pull/2526), *"user-defined
livelits and modules, integrated."* — user-defined livelits, Modules II parts
1–2, and the hazel-html substrate as opt-in `Html` / `Attr` / `Cmd` / `Sub`
modules. It contains **no** experimental language integration; that was
audited, and there are zero Fumola or Blackboard files among its 186. It is
the branch to read if you want to experience user-defined livelits with the
new module design.

**This one predicts that one's success.** It takes #2526 as given and asks
whether three independent sub-language integrations survive on top of it. If
they do, that is evidence the design holds under load it was not written for.
If they do not, the failures are specific and early rather than discovered
after a merge.

The point of keeping them apart is that the first has to be reviewable on its
own terms. The point of building the second on the first is that a prediction
you can run is worth more than one you argue about.

## What is in the build

| | what it is | deck |
|---|---|---|
| user-defined livelits + modules | the substrate, inherited from #2526 | — |
| **Blackboard MVP** | the core logic of the Blackboard proof assistant, as its own tile sort | `Blackboard / 0..5` |
| **Fumola (Livelits)** | Fumola through four builtin livelits | `Fumola (Livelits) / 0..8` |
| **Fumola (Tiles)** | Fumola as a sort in the tile grammar | `Fumola (Tiles) / ...` |

Twenty documentation slides across four decks, in one slide list, in one live
Hazel notebook.

## What the merges cost

Two of the three merges were not textual, and the differences are the
interesting part of this branch.

### The Fumola livelit MVP was written against the design #2526 replaces

`Livelit.re` diverged in opposite directions. #2526 **deleted** the `Slider`
and `Emotion` builtins — 306 lines — because they are user-defined livelits
now, shipped as `.hz` programs. The Fumola livelit branch kept them and added
~900 lines of Fumola builtins beside them.

Resolved as **`Js` plus the four Fumola livelits, and no `Slider` or `Emotion`
builtin.** The line that draws is: *a language integration is builtin, a
widget is user-defined.* Keeping `Slider` as a builtin here would have quietly
routed around the thing this branch exists to test.

Three contracts widened on the way through, each one the Fumola side's need
reaching code that predates it:

- **`expand` now takes `~id`, `~ana`, `~tools`.** A user-defined livelit needs
  none of the three — it applies its own `expand` member, and whatever that
  member needs it takes from the program's own scope — so it accepts and
  ignores them. That is what lets both kinds share one table.
- **`view` now takes `~id`**, so a livelit can tell two live occurrences
  apart. The Fumola livelits use it to name their VM instance.
- **`requires_annotation`** is false for user-defined livelits and true for
  the Fumola ones, which cannot know what to produce without the type the
  value is being read at.

None of the three is a concession. Each is the builtin/user-defined boundary
being asked a question it had not been asked before, and answering.

### Blackboard was almost a pure union

Thirty-six conflicts, nearly all of them both sides appending adjacent cases
to the same exhaustive matches, the same form table, the same per-sort colour
block — because Blackboard embeds its logic as its own sort exactly the way
Fumola does, and the ALFA derivation language did before either. That is worth
stating positively: **three sub-languages coexist in the tile grammar without
arbitration.** The pattern scales.

The one place that is *not* a union is `Precedence.re`, where associativity is
keyed by level **number** and shared across every sort: a level one sort marks
left-associative is left-associative for any other sort that reuses it. Both
ladders were checked against each other rather than merged on faith.

### Two answers the merge changed

Putting the two Fumola integrations in one tree made them disagree out loud,
which is the integration doing its job.

**Recasing.** The livelit route rendered a Hazel `Circle` as Fumola's
`#Circle`, on the reasoning that Fumola accepts a capitalised tag, so the
capital added on the way *in* could survive the way out. Grammatical — and not
a round trip: a value read from Fumola as `#circle` came back as `#Circle`, a
different tag, silently. The tiles route found that and put the convention in
one place, `FumolaCase`, which both directions go through. The merged tree has
one `FumolaSource`, so the tiles answer wins, and the livelit tests were
asserting the bug. `FumolaCase.round_trips` says which names still do not
survive: a Fumola tag that already begins upper-case is lossy in exactly this
way.

**A slide name.** `Test_SlideReconcile` filtered the shipped deck for
`"Fumola / 0. Big picture"`, which the tiles deck renamed to `"Fumola
(Livelits) / 0. Big picture"` when it needed a name of its own. The filter
removed nothing, so the test's own guard — *"the stale list really is missing
it"* — fired. The guard working.

## The two Fumola routes, compared

The two decks were written independently and neither pointed at the other.
These are the differences that survived an adversarial check against the code
— for each, an agent was asked to *refute* the claim, and to confirm that the
other route genuinely has the capability, since a limitation is only worth
documenting as one-sided if it is.

**What the tiles route lacks, and the livelits have**

- **Roughly half of Fumola's grammar has no tile.** `if`, `switch`/`case`,
  `func`, `var`, arrays, `?e`, unary operators, `not`, unquote, `e!`,
  `assert`, `ignore`, `return`, quoted names, `prim`, `do @`, `do goto` /
  `do within`, six of the fourteen binary operators (`#`, `^`, `<<`, `>>`,
  `<<>`, `<>>`), Float and Char literals, and every pattern other than a bare
  name and `_`. A livelit's program is a string handed to Fumola's own parser,
  so it accepts the whole surface language.
- **The Hazel expression inside `hazel … end` gets no info-map entry** — no
  type, no context, no unbound-variable mark, no completion, nothing under the
  cursor for any tile inside the escape. The livelit's Hazel input is an
  ordinary expression slot and gets full statics. (The `hazel … end` form
  *itself* does get an entry, so the inspector still names the form on its
  delimiters, and the child is genuinely edited as Hazel — it is the statics
  that stop at the boundary.)

**What the livelits route lacks, and the tiles have**

- **A Fumola program is opaque text.** It is a Hazel string literal in the
  livelit's model, edited through a one-line HTML `<input type="text">` that
  writes the whole string back, not through Hazel's edit actions. No tiles, no
  molds, no highlighting, no structure.
- **Exactly one Hazel input slot**, spliced as the fixed binder
  `let input = …`, and only `^fumola_with` has it at all. On the tiles route
  `hazel … end` is a production of the Fumola grammar: it can stand anywhere a
  Fumola term can, as many times as the program likes.
- **The expansion is spliced in without being traversed,** and the one type
  check that could fire is vacuous because the expansion's type is set to the
  expected type itself — so a translation that disagrees with its annotation
  raises no mark, and the mismatch surfaces at evaluation if at all. The tiles
  route runs its translation through the ordinary analysis path and typechecks
  it.

The shape of that list is the point. The livelit route buys **the whole
language** at the cost of **the whole editor**; the tiles route buys the
editor — structure, statics, completion, a cursor inspector that names every
form — at the cost of a grammar subset. Neither is strictly better, and a
reader who only sees one deck will not know that. Each deck should say so
where the difference bites, and name the other.

One such pointer is written: the `Fumola (Tiles) / The round trip` slide says
that `hazel … end` carries a value written in place and not a bound variable,
and sends the reader to `Fumola (Livelits) / 5. Input`, where a bound variable
works. The rest are still to write.

## Traps this tree has already sprung

Every one of these cost a test run or a shipped mistake.

- **Slide filenames are global.** Every `.hz` is copied into one flat
  directory and blobbed by basename, so `overview.hz` is a name, not a path.
  The Blackboard slides are `blackboard-*.hz` and the Fumola tiles slides
  `fumola-tiles-*.hz` for that reason. Prefix anything you add.
- **A Hazel comment is delimited by `#`,** so a Fumola tag cannot be written
  inside slide prose. It has cost two test runs.
- **A slide's text round-tripping is not a slide working.** The round-trip
  slide shipped with an example that escaped a *bound* variable —
  `hazel leaf end` — which cannot run, because a Fumola program runs during
  elaboration, before anything has been substituted. The text test passed,
  because the text was fine; what was broken was what the text meant. There is
  now a guard (`Test_FumolaTiles`, *"every slide's Fumola programs are
  printable"*) that asserts no shipped slide holds a Fumola program the
  printer refuses. It needs no wasm shim, because the printer refuses before
  the runtime is consulted and says something the runtime cannot — which is
  what separates *this slide is wrong* from *this runner has no Fumola*.
- **Doc slides are cached in IndexedDB by name.** A new slide will not appear
  for a returning browser until the `doc:` keys — including `doc:_meta` — are
  deleted.
- **Neither sub-language has menhir productions,** so their slides load
  through the typing parser and are ledgered in `test/Test_FastParseCorpus.re`.
  Remove those entries if either gains menhir support.
- **A red QCheck property test is usually a draw, not a regression.**
  `Elaboration tests` #42 runs 10,000 random elaborations. It failed once
  locally and not on CI; twelve further local runs with fresh seeds gave zero
  failures. Establish the rate before chasing one.

## Open

- The two Fumola decks still need their cross-references written, beyond the
  one on the round-trip slide. The comparison above is the source material.
- The Blackboard deck has not been read against the Fumola decks for overlap
  or for contrasts worth drawing.
- A `Tile.reassemble` assertion was seen once in the app on the tiles branch
  and is not reproduced. Every prefix of every tile program parses clean, so
  it is on the deletion or movement path, not insertion.

Corrected while writing this: `docs/fumola-tiles-design.md` and
`src/language/fumola/README.md` both said the Fumola repo ships *fourteen*
`.fumola` files, where it ships seventeen, and the tiles design doc said its
deck has four slides, where it has five. The claim those passages make about
the corpus was unaffected — every one of the seventeen is a `module` of
`public func`s with type annotations — so what waits on types is unchanged.
