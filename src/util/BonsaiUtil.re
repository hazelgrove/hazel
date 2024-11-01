open Core;
open Bonsai;
open Bonsai.Let_syntax;
open Js_of_ocaml;

module Alarm = {
  module Action = {
    [@deriving sexp]
    type t =
      | SetAlarm(Time_ns.Alternate_sexp.t)
      | SnoozeAlarm(Time_ns.Alternate_sexp.t)
      | UnsetAlarm;
  };

  let alarm =
    state_machine0(
      (module Time_ns.Alternate_sexp),
      (module Action),
      ~default_model=Time_ns.max_value_representable,
      ~apply_action=(~inject as _, ~schedule_event as _, model, action) => {
      switch (action) {
      | SetAlarm(time) => time
      | SnoozeAlarm(time) => Time_ns.max(time, model)
      | UnsetAlarm => Time_ns.max_value_representable
      }
    });

  let listen = (alarm, ~event) => {
    let%sub before_or_after = Clock.at(alarm |> Value.map(~f=fst));
    Edge.on_change(
      (module Clock.Before_or_after),
      before_or_after,
      ~callback={
        open Clock.Before_or_after;
        let%map (_, inject) = alarm
        and event = event;
        fun
        | After => Effect.Many([inject(Action.UnsetAlarm), event])
        | Before => Effect.Ignore;
      },
    );
  };
};

module OnStartup = {
  let on_startup = (effect: Value.t(Effect.t(unit))) => {
    let%sub startup_completed = Bonsai.toggle'(~default_model=false);
    let%sub after_display = {
      switch%sub (startup_completed) {
      | {state: false, set_state, _} =>
        let%arr effect = effect
        and set_state = set_state;
        Bonsai.Effect.Many([set_state(true), effect]);
      | {state: true, _} => Bonsai.Computation.return(Ui_effect.Ignore)
      };
    };
    Edge.after_display(after_display);
  };
};

module SizeObserver = {
  module Size = {
    [@deriving sexp]
    type t = {
      width: float,
      height: float,
    };

    let equal = phys_equal;
  };

  let observer =
      (node: unit => Js.t(Dom_html.element), ~default: Size.t)
      : Computation.t(Size.t) => {
    let%sub (size, update) = state((module Size), ~default_model=default);
    let startup = {
      let%map update = update;
      Effect.of_sync_fun(
        () => {
          let _ =
            ResizeObserver.observe(
              ~node=node(),
              ~f=
                (entries, _) => {
                  let rect = Js.to_array(entries)[0]##.contentRect;
                  Size.{
                    width: rect##.right -. rect##.left,
                    height: rect##.bottom -. rect##.top,
                  }
                  |> update
                  |> Effect.Expert.handle;
                },
              (),
            );
          ();
        },
        (),
      );
    };
    let%sub () = OnStartup.on_startup(startup);
    let%arr size = size;
    size;
  };
};
