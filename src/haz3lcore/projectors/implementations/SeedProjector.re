open Util;
open ProjectorBase;

/* Seed projector: `^^seed(N)` marks an integer literal N as a *default* random
   seed to be resolved after parse. Choosing the seed is a tooling step (the CLI
   may prompt or draw OS entropy), which is what lets a run use a genuinely
   random seed a pure program could never produce on its own; the chosen value is
   an ordinary integer literal, so the language stays pure.

   `init` parks the default in Pending(N); `effect` asks the frontend to choose
   (the driver consults the injected SeedChoose hook); the chosen int returns as
   a Resolved action (-> Chosen); `expand` is that integer literal. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model_t =
  | Pending(int) // source default, not yet chosen
  | Chosen(int); // the seed actually used

[@deriving (show({with_path: false}), sexp, yojson)]
type action_t =
  | Resolved(int); // the chosen seed

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
  let update = (m, _, action: action) =>
    switch (action) {
    | Resolved(n) =>
      switch (m) {
      | Pending(_) => Chosen(n)
      | Chosen(_) => m // stale result; ignore
      }
    };
  let error = (_, _): option(ProjectorBase.error) => None;

  /* A Pending(default) asks the injected SeedChoose hook to choose a seed (the
   * CLI prompts / draws entropy; the web keeps the default) and folds it back as
   * a Resolved action. Chosen models need no resolution. */
  let resolve = (m: model): option(resolution(action)) =>
    switch (m) {
    | Pending(default) =>
      Some(k => k(Resolved(SeedChoose.choose^(~default))))
    | Chosen(_) => None
    };

  /* The chosen seed, as an integer literal. */
  let expand = (m: model, _info): option(Language.Exp.t) =>
    switch (m) {
    | Chosen(n) => Some(Language.IdTagged.FreshGrammar.Exp.int(n))
    | Pending(_) => None
    };
};
