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

Today `init` is a **value**, and it cannot mention splices at all: nothing has
made one when `init` runs, and making one is an effect. That is why the slide's
`init` has no `: Model` annotation — `init` supplies values, the use site
supplies refs, and the two types are not the same.

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
