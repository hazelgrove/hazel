open Bonsai;

/* Maintains a globally-scoped register of all components that
   support undo/redo/logging, and effects we can use to undo/redo/apply
   actions on them. */

type register_entry = {
  undo: Effect.t(unit),
  redo: Effect.t(unit),
  apply: Sexplib.Sexp.t => Effect.t(unit),
};

let reg: Hashtbl.t(string, register_entry) = Hashtbl.create(100);

let undo_at: string => Effect.t(unit) =
  component =>
    switch (Hashtbl.find_opt(reg, component)) {
    | Some({undo, _}) => undo
    | None => Effect.Ignore
    };

let redo_at: string => Effect.t(unit) =
  component =>
    switch (Hashtbl.find_opt(reg, component)) {
    | Some({redo, _}) => redo
    | None => Effect.Ignore
    };

let apply_at: (string, Sexplib.Sexp.t) => Effect.t(unit) =
  (component, action) =>
    switch (Hashtbl.find_opt(reg, component)) {
    | Some({apply, _}) => apply(action)
    | None => Effect.Ignore
    };

let add =
    (
      component: string,
      undo: Effect.t(unit),
      redo: Effect.t(unit),
      apply: Sexplib.Sexp.t => Effect.t(unit),
    ) => {
  Hashtbl.add(
    reg,
    component,
    {
      undo,
      redo,
      apply,
    },
  );
};

let remove = (component: string) => {
  Hashtbl.remove(reg, component);
};
