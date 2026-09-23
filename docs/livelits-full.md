# Full livelits: the three operations

Goal of this branch. A livelit is "full" — the PLDI 2021 design, not an
approximation of it — when all three of these are true of one working example:

1. **`init` makes its splices**, with `splice_new`.
2. **`view` reads and places them**, with `splice_eval` for the value and the
   existing splice placement for where the client's editor appears.
3. **`update` writes them**, with `splice_set`.

When that holds for one simple example, the design is in. More examples follow,
to find out whether the implementation is actually right.

## Naming

The paper spells these `new_splice`, `set_splice`, `eval_splice`. This branch
uses the prefix-grouped `splice_new` / `splice_set` / `splice_eval` as the
member names, for the same reason `expand_fun` / `expand_mac` are spelled that
way: they read as one family when explaining them to someone.

| here | PLDI 2021, Figure 3 |
|---|---|
| `splice_new` | `new_splice : (Typ, Maybe(Exp)) -> UpdateCmd(SpliceRef)` |
| `splice_set` | `set_splice` |
| `splice_eval` | `eval_splice : SpliceRef -> ViewCmd(Maybe(Result))`, `Result = Val \| Indet` |

Existing prose in `docs/livelits.md`, `docs/livelits-pldi2021-status.md` and the
deck slides keeps the paper's spelling. Those sentences are statements *about
the paper* and stay true; only the member names differ.

## What is already here

This branch starts from `integration/livelits-splicerefs` (increment 1), where:

- `SpliceRef` is an inhabited type and a spliced model field carries both the
  handle and the value: `lo=(ref=SpliceRef, value=Int)`.
- `view` names the splice it means — `Html.splice(m.lo.ref)`, not by position.
- Splices are **transparent to statics**: a splice's content is typed in the
  client's scope and evaluates in place.

That transparency is why increment 1 needed no monad. It is also what makes the
three operations below genuinely new work rather than renaming.

## 1. `init` uses `splice_new`

Today `init` cannot mention splices at all: nothing has made one when `init` is
used, and making one is an effect. That is why the slide's `init` has no
`: Model` annotation — `init` supplies values, the use site supplies refs, and
the two types are not the same.

It is weaker than "init is a value", which is what this section first said.
`init` is **syntax**: its source text is pasted into your program when you type
the livelit's name, before statics exist. See "Where this lands in the code"
below — that is what makes `init : UpdateCmd(Model)` a real project rather than
a type change.

`splice_new` closes that gap, and in doing so changes `init`'s type from a value
to a command. This is the structural change on which the other two depend:

- `init : UpdateCmd(Model)` rather than `init : Model`.
- A splice gets a **declared type** at creation. Today a splice is typed only by
  where it sits in the model tuple; for `between` that happens to give `Int`,
  but by position rather than by any mechanism.
- The splice count stops being fixed by the program text, which is what makes an
  editable **list** of splices expressible for the first time.

## 2. `view` uses `splice_eval`

Increment 1 hands the view a value directly, alongside the ref. That is cheaper
than the paper and strictly weaker in one way: **there is no way to say a bound
has no value.** Delete a bound and the result renders as a hole because nothing
asked; the widget never gets to decide what to show.

`splice_eval` answers `Maybe(Result)` with `Val | Indet`, so the widget chooses.
Getting `Indet` right means knowing when a splice does not reduce, which is the
part that needs care rather than plumbing.

Placement is the half that already works: the view says where the client's
editor appears, and that is unchanged.

## 3. `update` uses `splice_set`

Today `update` **cannot** write a spliced field, by construction — that is the
ownership property the current slide argues for, and `Action` deliberately has
no `SetLo`. `splice_set` makes writing possible, which means ownership stops
being structural and becomes a thing the author may choose to respect or not.

Worth being deliberate about rather than treating as a pure gain: the argument
that "a livelit may READ the client's code; it may not rewrite it" is currently
enforced by there being no operation. With `splice_set` it becomes a convention.

## What "done" looks like

One example — most likely `between`, since it is already the smallest thing that
needs splices — where:

- `init` calls `splice_new` to create `lo` and `hi` at a declared type,
- `view` calls `splice_eval` on each and renders `Indet` differently from a
  value, and places both editors,
- `update` calls `splice_set` on at least one of them,

and the example still round-trips through save/load. Splice content currently
does **not** survive a save/load round trip — only the marker parens do, which
is why `init` rebuilds the splices on every load. `splice_new` in `init` may
subsume that, or may not; that is a question to answer rather than assume.

## Known walls, carried forward

- **No quoted-code type.** `Exp` is registered and is an empty sum, so
  `expand_mac : Model -> (Exp, List(SpliceRef))` remains uninhabitable. The
  three operations here do not need it; the macro form does.
- **Capture avoidance.** The paper passes splices as arguments to a quoted
  function, so a binder inside the expansion cannot capture them. `expand_fun`
  returns a value from bounds already evaluated in the client's scope, so the
  problem does not arise — we avoid it rather than solve it. That stays true
  until `expand_mac` is reachable.

## Ports

| port | branch | what it is |
|---|---|---|
| 8011 | `integration/livelits-splices` | splices, no refs (#2595) |
| 8111 | `integration/livelits-splicerefs` | increment 1: refs in the model (#2596) |
| 8311 | `integration/livelits-splicerefs-full` | this branch |

## Where this lands in the code

Read before writing any of it, so the plan above is grounded rather than
aspirational. Facts first, with references; the reading follows and is marked
as a reading.

**`init` is not a value. It is syntax, consumed before statics exist.**

- `UserLivelit.re:85` — `required_members = ["init", "update", "view", "expand_fun"]`
- `UserLivelit.re:481` — init's declared member type is `model_t`, plainly.
- `UserLivelit.re:576` — `model_default: Exp.replace_all_ids(List.assoc("init", members))`.
  The *expression* is taken and stored; nothing evaluates it.
- `Triggers.re:200-216` — `expand_livelit` turns `ll.model_default` into a
  **segment** with `exp_to_seg` and splices that program text in at the caret
  when you type `^between ` (`Tuple` gets unparenthesized on the way). The
  comment there says it outright: *"No statics available at trigger time."*

So typing a livelit's name pastes init's source into your program. That is a
much stronger constraint than "init is a value", and it is the real obstacle
to `init : UpdateCmd(Model)`: there is no evaluator, and no statics, at the
only moment init is used.

**There is no `UpdateCmd` / `ViewCmd` machinery to build on.** The single
occurrence of either name in `src/` is a comment at `UserLivelit.re:399`
describing what the paper has. Both monads would be built from nothing.

**A reading, not a fact.** Two ways out, and they are very different projects:

1. *Run it.* Give the trigger path statics and evaluation so a command can
   actually execute. This is the faithful reading of Figure 3 and the larger
   one — it puts an evaluator on the editor's trigger path.
2. *Interpret `splice_new` syntactically.* Today a splice is recovered from
   program text by the **parens the author wrote**, on every load. If
   `splice_new(Int, Some(0))` in init's text elaborates to exactly that
   marker, then `splice_new` becomes the NAMED form of the marker that already
   exists, and init stays syntax. This buys the declared type immediately —
   the `Int` argument is the thing currently missing, since a splice is typed
   only by its position in the model tuple — while an editable *list* of
   splices still needs (1), because a list needs runtime creation.

(2) is cheaper and is a real increment; (1) is what "fully reflects the paper"
eventually means. Worth deciding deliberately rather than drifting into (2)
because it is nearer.

`splice_eval` and `splice_set` are not blocked by this. `view` already runs in
the main evaluation and `update` already runs at event time in the builtin
environment (`UserLivelit.re:500`, `LivelitProj.re:643` for view,
`LivelitProj.re:710` for update), so both have an evaluator available where
`init` does not.
