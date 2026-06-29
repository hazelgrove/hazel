open Util;
open ProjectorBase;

/* Seed projector: `^^seed(N)` marks an integer literal N as a *default* random
   seed to be resolved at initialization time. Choosing the seed is a tooling
   step (the CLI may prompt or draw OS entropy), which is what lets a run use a
   genuinely random seed a pure program could never produce on its own; the
   substituted value is an ordinary integer literal, so the language stays pure.

   Resolution mirrors CSVProjector: `init` parks the default in `Pending(N)`, and
   the initialization phase consults the injected SeedChoose hook and splices the
   chosen integer in as the projector's syntax. The CLI resolves seeds directly
   as Exp in its fast path (Cli.resolve_program); the web uses `initialize`,
   where the pure default keeps N. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model_t =
  | Pending(int) // source default, not yet chosen
  | Chosen(int); // the seed actually used

[@deriving (show({with_path: false}), sexp, yojson)]
type action_t = unit;

let seed_of = (m: model_t): int =>
  switch (m) {
  | Pending(n)
  | Chosen(n) => n
  };

module M: Projector with type model = model_t and type action = action_t = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = model_t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = action_t;

  let init = (a: Language.Any.t): option(model) =>
    switch (a) {
    // ^^seed(N) — an integer literal is the default seed
    | Exp({term: Atom(Int(n)), _}) =>
      switch (Bigint.to_int(n)) {
      | Some(i) => Some(Pending(i))
      | None => None
      }
    | _ => None
    };

  let dynamics = false;
  let elaborate_syntax = false;
  let placeholder = (m, _) =>
    ProjectorCore.Shape.inline(String.length(string_of_int(seed_of(m))));
  let update = (m, _, _) => m;
  let error = (_, _): option(ProjectorBase.error) => None;

  /* Initialization phase: resolve the default into the chosen seed via the
   * SeedChoose hook and hand back the chosen integer as the expansion Exp.
   * Already-chosen models need no work. Synchronous, so it calls [k] and
   * returns true. */
  let initialize =
    Some(
      (
        model: model,
        _info,
        ~k: (option(model), option(Language.Exp.t)) => unit,
      ) =>
        switch (model) {
        | Pending(default) =>
          let chosen = SeedChoose.choose^(~default);
          k(
            Some(Chosen(chosen)),
            Some(Language.IdTagged.FreshGrammar.Exp.int(chosen)),
          );
          true;
        | Chosen(_) => false
        },
    );
};
