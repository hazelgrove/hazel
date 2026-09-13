/* What a Fumola run needs from Hazel's statics, handed from the pass that
   has it to the pass that needs it.

   Running moved from elaboration to evaluation so that a `hazel … end` sees
   the *value* of what it names rather than the expression -- which is the
   whole point of the escape. Reading the result back still needs Hazel's
   types: which sum a Fumola tag belongs to, and what an alias unfolds to.
   Those live in the typing context, and the typing context cannot travel
   with the elaborated term: Ctx holds LivelitCtx entries, which are OCaml
   closures and do not cross postMessage. Hence a side channel rather than a
   field of the term.

   That is not a second constraint on top of the runtime's; it is the same
   one. `window.fumola` does not exist in the worker either. A cell holding a
   Fumola program therefore evaluates on the main thread, where both the
   runtime and the context are in reach, and this channel is installed around
   that evaluation. In the worker it is simply empty, and a run there
   degrades to "no Fumola runtime available" rather than to a wrong answer.

   Scope is one evaluation: [with_resolve] installs, runs, and restores, so
   nothing accumulates and nothing outlives the pass that knew it. */

type entry = {
  /* The type expected of the program's result, which is what decides the
     shape a Fumola value takes on the way into Hazel. */
  ana: TermBase.Typ.t,
  tools: FumolaTools.t,
};

type resolve = Id.t => option(entry);

let current: ref(option(resolve)) = ref(None);

let with_resolve = (resolve: resolve, f: unit => 'a): 'a => {
  let saved = current^;
  current := Some(resolve);
  switch (f()) {
  | result =>
    current := saved;
    result;
  | exception exn =>
    current := saved;
    raise(exn);
  };
};

/* Absent context is a real case, not a bug: the worker has none, and so does
   the test runner. The caller falls back to an unknown expected type, which
   costs the result its shape and nothing else. */
let lookup = (id: Id.t): option(entry) =>
  switch (current^) {
  | None => None
  | Some(resolve) => resolve(id)
  };
