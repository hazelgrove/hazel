open Bonsai;
open Sexplib;
open Sexplib.Std;
open Bonsai.Let_syntax;

module UndoController = {
  type register_entry = {
    undo: Effect.t(unit),
    redo: Effect.t(unit),
  };

  let register: Hashtbl.t(string, register_entry) = Hashtbl.create(100);

  type history_entry =
    | Action({
        component: string,
        action: Sexp.t,
      })
    | Undo({component: string})
    | Redo({component: string});

  let history: ref(list(history_entry)) = ref([]);

  let get_undo = () => {
    let rec go = (n: int, acc: list(history_entry)) => {
      switch (acc) {
      | [] => None
      | [Action({component, _}), ..._] when n == 0 =>
        Some(Hashtbl.find(register, component).undo)
      | [Action(_), ...xs] => go(n - 1, xs)
      | [Undo(_), ...xs] => go(n + 1, xs)
      | [Redo(_), ...xs] => go(n - 1, xs)
      };
    };
    go(0, history^);
  };

  let get_redo = () => {
    let rec go = (n: int, acc: list(history_entry)) => {
      switch (acc) {
      | [] => None
      | [Undo({component, _}), ..._] when n == 0 =>
        Some(Hashtbl.find(register, component).undo)
      | [Undo(_), ...xs] => go(n - 1, xs)
      | [Redo(_), ...xs] => go(n + 1, xs)
      | [Action(_), ..._] => None
      };
    };
    go(0, history^);
  };

  let register_component =
      (component: string, undo: Effect.t(unit), redo: Effect.t(unit)) => {
    Hashtbl.add(
      register,
      component,
      {
        undo,
        redo,
      },
    );
  };

  let unregister_component = (component: string) => {
    Hashtbl.remove(register, component);
  };

  let add_action = (component: string, action: Sexp.t) => {
    history :=
      [
        Action({
          component,
          action,
        }),
        ...history^,
      ];
  };

  let add_undo = (component: string) => {
    history := [Undo({component: component}), ...history^];
  };

  let add_redo = (component: string) => {
    history := [Redo({component: component}), ...history^];
  };
};

[@deriving (sexp, eq)]
type historic_action('a) =
  | Action('a)
  | Undo
  | Redo;

module HistoricModel = (Model: Bonsai.Model, Action: Bonsai.Model) => {
  [@deriving (sexp, eq)]
  type t = {
    current: Model.t,
    past: list(Model.t),
    future: list(Model.t),
  };
};

module HistoricAction = (Action: Bonsai.Model) => {
  [@deriving (sexp, eq)]
  type t = historic_action(Action.t);
};

module UndoRedo = {
  type t = {
    undo: Effect.t(unit),
    redo: Effect.t(unit),
  };

  let sexp_of_t = (_: t) => Sexp.Atom("UndoRedo");
  let t_of_sexp = (_: Sexp.t) => {
    undo: Effect.Ignore,
    redo: Effect.Ignore,
  };
  let equal = (x, y) => x.undo == y.undo && x.redo == y.redo;
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
  let%sub path = path_id;
  let input' = {
    let%map path = path
    and input = input;
    (input, path);
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
          let (input, path) =
            switch (input) {
            | Computation_status.Active((input, path)) => (
                Computation_status.Active(input),
                path,
              )
            | Computation_status.Inactive => (
                Computation_status.Inactive,
                "INVALID_COMPONENT",
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
              UndoController.add_action(path, action |> Action.sexp_of_t);
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
          | Undo when !List.is_empty(past) =>
            UndoController.add_undo(path);
            {
              current: past |> List.hd,
              past: past |> List.tl,
              future: [current, ...future],
            };
          | Undo => {
              current,
              past,
              future,
            }
          | Redo when !List.is_empty(future) =>
            UndoController.add_redo(path);
            {
              current: future |> List.hd,
              past: [current, ...past],
              future: future |> List.tl,
            };
          | Redo => {
              current,
              past,
              future,
            }
          };
        },
      input',
    );
  let undo_redo = {
    let%map (_, inject) = sm;
    UndoRedo.{
      undo: inject(Undo),
      redo: inject(Redo),
    };
  };
  let callback = {
    let%map path = path;
    (UndoRedo.{undo, redo}) => {
      Effect.of_sync_fun(
        () => UndoController.register_component(path, undo, redo),
        (),
      );
    };
  };
  let%sub () = Edge.on_change((module UndoRedo), undo_redo, ~callback);
  let%arr (model, inject) = sm;
  (model.current, x => inject(Action(x)));
};
