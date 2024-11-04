open Core;
open Bonsai;
open Bonsai.Let_syntax;

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
