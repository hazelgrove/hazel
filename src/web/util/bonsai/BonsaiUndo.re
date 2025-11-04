open Bonsai;
open Sexplib;
open Sexplib.Std;
open Bonsai.Let_syntax;

/* This module is used to combine individual state machines into a collective with
   a combined undo/redo history. UndoScope manages the shared history, and Undoable
   provides a helper to create state machines inside that collective. Any Undoable
   state machine is also added to the log. */

module UndoScope = {
  module Model = {
    [@deriving (sexp, eq)]
    type history_entry =
      | Action({
          component: string,
          action: Sexp.t,
        })
      | Undo({component: string})
      | Redo({component: string});

    [@deriving (sexp, eq)]
    type t = list(history_entry);
  };

  module Action = {
    [@deriving (sexp, eq)]
    type t =
      | AddAction({
          component: string,
          action: Sexp.t,
        })
      | AddUndo
      | AddRedo;
  };

  let scope =
    Bonsai.Dynamic_scope.create(
      ~name="UndoScope",
      ~fallback=(_: Action.t) => Ui_effect.Ignore,
      (),
    );

  let get_undo = m => {
    let rec go = (n: int, acc: Model.t) => {
      switch (acc) {
      | [] => None
      | [Action({component, _}), ..._] when n == 0 => Some(component)
      | [Action(_), ...xs] => go(n - 1, xs)
      | [Undo(_), ...xs] => go(n + 1, xs)
      | [Redo(_), ...xs] => go(n - 1, xs)
      };
    };
    go(0, m);
  };

  let get_redo = m => {
    let rec go = (n: int, acc: Model.t) => {
      switch (acc) {
      | [] => None
      | [Undo({component, _}), ..._] when n == 0 => Some(component)
      | [Undo(_), ...xs] => go(n - 1, xs)
      | [Redo(_), ...xs] => go(n + 1, xs)
      | [Action(_), ..._] => None
      };
    };
    go(0, m);
  };

  let create = () =>
    Bonsai.state_machine0(
      (module Model),
      (module Action),
      ~default_model=[],
      ~apply_action=(~inject as _, ~schedule_event, history, action) => {
      switch (action) {
      | Action.AddAction({component, action}) =>
        Log.update(component, action);
        [
          Model.Action({
            component,
            action,
          }),
          ...history,
        ];
      | Action.AddUndo =>
        switch (get_undo(history)) {
        | None => history
        | Some(component) =>
          schedule_event(BonsaiRegister.undo_at(component));
          [Model.Undo({component: component}), ...history];
        }
      | Action.AddRedo =>
        switch (get_redo(history)) {
        | None => history
        | Some(component) =>
          schedule_event(BonsaiRegister.redo_at(component));
          [Model.Redo({component: component}), ...history];
        }
      }
    });

  let set =
      (new_scope, inside: Bonsai.Computation.t('a))
      : Bonsai.Computation.t('a) => {
    let inject = {
      let%map (_, inject) = new_scope;
      inject;
    };
    Bonsai.Dynamic_scope.set(scope, inject, ~inside);
  };
};

/* A helper module to create undoable Bonsai state machines */
module Undoable = {
  module HistoricModel = (Model: Bonsai.Model, Action: Bonsai.Model) => {
    [@deriving (sexp, eq)]
    type t = {
      current: Model.t,
      past: list(Model.t),
      future: list(Model.t),
    };
  };

  [@deriving (sexp, eq)]
  type historic_action('a) =
    | Action('a)
    | Undo
    | Redo;

  module HistoricAction = (Action: Bonsai.Model) => {
    [@deriving (sexp, eq)]
    type t = historic_action(Action.t);
  };

  let state_machine_with_undo =
      (
        type model,
        type action,
        type input,
        module Model: Bonsai.Model with type t = model,
        module Action: Bonsai.Model with type t = action,
        ~default_model: model,
        ~apply_action:
           (
             ~inject: action => Effect.t(unit),
             ~schedule_event: Effect.t(unit) => unit,
             Computation_status.t(input),
             model,
             action
           ) =>
           model,
        ~can_undo: action => bool=_ => true,
        input: Value.t(input),
      )
      : Computation.t((model, action => Effect.t(unit))) => {
    module Model' = HistoricModel(Model, Action);
    module Action' = HistoricAction(Action);
    let%sub scope = Bonsai.Dynamic_scope.lookup(UndoScope.scope);
    let%sub path = path_id;
    let input' = {
      let%map path = path
      and input = input
      and scope = scope;
      (input, path, scope);
    };
    let%sub sm =
      state_machine1(
        (module Model'),
        (module Action'),
        ~default_model={
          current: default_model,
          past: [],
          future: [],
        },
        ~apply_action=
          (~inject, ~schedule_event, input, {current, past, future}, action) => {
            let (input, path, scope) =
              switch (input) {
              | Computation_status.Active((input, path, scope)) => (
                  Computation_status.Active(input),
                  path,
                  scope,
                )
              | Computation_status.Inactive => (
                  Computation_status.Inactive,
                  "INVALID_COMPONENT",
                  (
                    _ => {
                      print_endline("Ignoring undo action");
                      Ui_effect.Ignore;
                    }
                  ),
                )
              };
            switch (action) {
            | Action(action) =>
              let current' =
                apply_action(
                  ~inject=x => inject(Action(x)),
                  ~schedule_event,
                  input,
                  current,
                  action,
                );
              if (can_undo(action)) {
                print_endline("Scheduling undoable action");
                schedule_event(
                  scope(
                    UndoScope.Action.AddAction({
                      component: path,
                      action: action |> Action.sexp_of_t,
                    }),
                  ),
                );
                {
                  current: current',
                  past: [current, ...past],
                  future: [],
                };
              } else {
                {
                  current: current',
                  past,
                  future,
                };
              };
            | Undo when !List.is_empty(past) => {
                current: past |> List.hd,
                past: past |> List.tl,
                future: [current, ...future],
              }
            | Undo => {
                current,
                past,
                future,
              }
            | Redo when !List.is_empty(future) => {
                current: future |> List.hd,
                past: [current, ...past],
                future: future |> List.tl,
              }
            | Redo => {
                current,
                past,
                future,
              }
            };
          },
        input',
      );
    let on_activate = {
      let%map path = path
      and (_, inject) = sm;
      Effect.of_sync_fun(
        () =>
          BonsaiRegister.add(path, inject(Undo), inject(Redo), action_sexp =>
            inject(Action(Action.t_of_sexp(action_sexp)))
          ),
        (),
      );
    };
    let on_deactivate = {
      let%map path = path;
      Effect.of_sync_fun(() => BonsaiRegister.remove(path), ());
    };
    let%sub () = Bonsai.Edge.lifecycle(~on_deactivate, ~on_activate, ());
    let%arr (model, inject) = sm;
    (model.current, x => inject(Action(x)));
  };
};

let create_scope = UndoScope.create;
let set_scope = UndoScope.set;

let state_machine_with_undo = Undoable.state_machine_with_undo;
