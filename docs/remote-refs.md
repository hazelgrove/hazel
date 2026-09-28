# RemoteRef: a splice that a Fumola instance can write

*Design note, 2026-09-28. Branch `remote-splices`. Option A's first cut is
built: see "What the first cut does" at the end.*

## The idea

A `SpliceRef` is a handle to a piece of the client's code, held in a livelit's
model. Two parties can write that code today: **the client**, typing in the
splice's editor, and **the livelit**, through `set_splice` in `update`.

A `RemoteRef` adds a third writer: **a Fumola instance**. A Fumola program,
running in the instance's wasm runtime, can push a new value into the ref, and
Hazel takes it the way it takes a `set_splice`: the ref's code in the program
text is replaced, so the write is visible, undoable, saved with the program, and
seen by everything downstream of the use.

So a RemoteRef is a SpliceRef plus a **writer**. The editor stays: the client
can still read and edit the code, and the livelit can still `set_splice` it.

## What exists to build on

**On the Hazel side**, a splice write already has one path into the program:

1. `update` returns an `UpdateCmd`. A `set_splice(r, code)` in it is an effect.
2. The projector (`LivelitProj`) runs the command and commits. It writes the new
   model into the use's text, with each ref's code in place at its splice
   (`SpliceStore.write_model`), so the code lives in the client's program as
   `(code : T)`.
3. Statics and evaluation re-run on the new text.

A RemoteRef write should enter at step 2, as an effect the projector commits. It
should not bypass the text, so there is still one source of truth.

**On the Fumola side**, the bridge is **pull only**. Hazel calls six wasm
exports (`fumola_create`, `fumola_has`, `fumola_realize`, `fumola_eval`,
`fumola_eval_top`, `fumola_ensure_mode`; see `docs/fumola-runtime-changes.md`).
Nothing in the runtime calls back into Hazel. The Fumola panel reads history by
running `prim "adaptonPeekHistory" ()`, through the same shim. So a writer needs
a way for a write to reach Hazel, and there are two.

## Two ways a write can reach Hazel

**A. Pull: Hazel reads a named cell after each run (no runtime change).** The
RemoteRef names a cell in an instance: the instance's name and a symbol. The
Fumola program writes that cell as it would any other. After a Fumola run,
Hazel reads the cell with `fumola_eval_top` (a `peek`, so reading is not recorded
as a read). If it differs from the ref's code, Hazel commits it as a
`set_splice`.

- For: works with the published runtime today; writes are just Fumola writes;
  the history panel already shows them.
- Against: Hazel only learns of a write when it next reads, so a write made
  between Hazel runs waits for the next one. It also polls: one extra
  `eval_top` per RemoteRef per run.

**B. Push: a Fumola prim that notifies Hazel (runtime change).** A new prim,
`prim "hazelWrite" (ref, value)`, calls a JavaScript callback Hazel registers
on `window.fumola`. The callback dispatches a Hazel action that commits the
write.

- For: immediate, and no polling.
- Against: a change in `Adapton/fumola` (a new prim, and a callback on the
  wasm-bindgen side), a new runtime release, and the write happens *during* a
  Fumola run that Hazel's evaluation started, so the commit has to be deferred
  until evaluation finishes (re-entrancy).

**Recommendation: start with A.** It needs no runtime work, it uses Fumola's own
notion of a cell (so the write is in the instance's history), and it keeps the
single path into the program text. B can come later as an optimization with the
same Hazel-side commit.

## Proposed API

Beside the splice commands:

```
new_remote : (Typ, Option(Exp), Instance, Symbol) -> UpdateCmd(RemoteRef)
set_splice : (RemoteRef, Exp) -> UpdateCmd(())      // as for SpliceRef
eval_splice : RemoteRef -> ViewCmd(Maybe(Result))    // as for SpliceRef
editor      : (RemoteRef, Dim) -> ViewCmd(Html)      // as for SpliceRef
```

- `new_remote` makes a splice as `new_splice` does, and binds it to a Fumola
  cell: an instance, named as the Fumola tiles name one, and a symbol in it.
- A RemoteRef is usable wherever a SpliceRef is, so a livelit can treat its
  RemoteRefs as splices. Whether it is a subtype, or `SpliceRef` gains an
  optional binding, is open (below).
- In the program text, a RemoteRef's splice is written like any other,
  `(code : T)`, with its binding beside it in the model:
  `remote=(instance="i", cell="`c", code=(0 : Int))`.

## The write, step by step (option A)

1. The client's program runs. A Fumola program in it writes `` `c `` in
   instance `i`.
2. Evaluation finishes. For each RemoteRef in each livelit use, Hazel peeks
   `` `c `` in `i` and shapes the Fumola value into a Hazel value at the ref's
   declared type, as a `fumola … end` result is shaped today
   (`FumolaValue.exp_of_json`, which gives a Hazel term). That term is the code
   the commit writes.
3. If the peeked value differs from the ref's current code, Hazel commits a
   `set_splice(r, <that code>)` through the use's projector, as if `update` had
   returned it.
4. The text changes, so statics and evaluation run again, and the Fumola
   program runs again. It must not write a *different* value every run, or this
   loops. Step 3's "differs" check stops a write of the same value.

## Open questions

1. **Loops.** If the Fumola program's write depends on the ref's own value, step
   4 never settles. Bound it (one commit per run, and a counter), or require
   that a remote cell not read its own ref?
2. **Who wins.** The client is typing in the splice when a write arrives. Drop
   the write, queue it until the editor loses focus, or overwrite?
3. **Types.** A Fumola value has a Hazel type only by the type it is read at
   (`FumolaCtx`). The ref's declared type (`new_remote`'s `Typ`) is the natural
   one to read it at. What happens to a value that does not fit, a hole with a
   mark?
4. **Undo.** A remote write is an edit. Is it undoable like a keystroke, and
   what does undoing it mean when the Fumola cell still holds the value?
5. **SpliceRef or a new type.** Is `RemoteRef` its own type (so a livelit's
   Model says which refs are remote), or is it a `SpliceRef` with an optional
   binding (so every livelit that takes splices can take remote ones)?
6. **Where instances come from.** The Fumola tiles name an instance in the
   program. Does `new_remote` take that name as a string, or a value the
   program already has?

## A first example

A **Fumola counter** slide: a livelit whose model holds one RemoteRef, bound to
cell `` `count `` in instance `demo`. A Fumola program on the same slide
increments `` `count `` each time a Hazel value it reads changes. The livelit's
view shows the ref's editor, so the client sees the count arrive in the code,
and can also type a new count, which the next Fumola run reads.

It exercises every part: the binding, the pull, the commit, the loop check (the
increment must be guarded), and both writers.

## What the first cut does

- **A RemoteRef is a record in a livelit's model**, `(instance = "demo",
  cell = "count", code = <a SpliceRef>)`, recognized by its shape. There is no
  `new_remote` yet and no new type.
- **The pull** (`FumolaRun.peek_cell`) runs `` peek(`count)! `` in the instance,
  at the latest moment: every Hazel run happens at a named moment, and a read
  at Fumola's `Now` sees none of them. The symbol is Fumola's own spelling, a
  leading backtick only; `` `count` `` is the tile's.
- **The commit** happens in the livelit's view, after a run: when the cell holds
  something other than the ref's code, the view schedules `commit_model` with a
  `set_splice` effect. A cell that holds the same value writes nothing, which
  is what stops the re-run. A table of pending writes keeps a redraw from
  scheduling one twice.
- **Outputs only.** Fumola writes, Hazel reads. Feedback, where Fumola writes
  something it also reads through Hazel, is open research: it is not handled,
  and the example avoids it (Fumola reads `items`, not `count`).
- **Writes into Fumola** (input refs, later) are to be buffered and applied only
  once the instance has settled, meaning its stack is empty, never mid-run.
- **The example** is Livelits / Remote Refs / Fumola Counter: Fumola counts a
  Hazel list into `` `count `` in `demo`, and the livelit's code follows. In the
  browser it goes from `0` to `5` after the first run, and stays there.
