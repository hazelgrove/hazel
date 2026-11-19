open Core;
/* The legacy Value/Computation ("proc") API, which jane street keeps
   supporting alongside the graph-passing [Bonsai.Cont] API. */
module Bonsai = Bonsai_web_proc.Bonsai;
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
    state_machine(
      ~sexp_of_model=Time_ns.Alternate_sexp.sexp_of_t,
      ~sexp_of_action=Action.sexp_of_t,
      ~equal=Time_ns.equal,
      ~default_model=Time_ns.max_value_representable,
      ~apply_action=(_ctx, model, action) => {
        switch (action) {
        | SetAlarm(time) => time
        | SnoozeAlarm(time) => Time_ns.max(time, model)
        | UnsetAlarm => Time_ns.max_value_representable
        }
      },
      (),
    );

  let listen = (alarm, ~event) => {
    let%sub before_or_after = Clock.at(alarm |> Value.map(~f=fst));
    Edge.on_change(
      ~sexp_of_model=Clock.Before_or_after.sexp_of_t,
      ~equal=Clock.Before_or_after.equal,
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
    let%sub startup_completed = Bonsai.toggle'(~default_model=false, ());
    let%sub after_display = {
      switch%sub (startup_completed) {
      | {state: false, set_state, _} =>
        let%arr effect = effect
        and set_state = set_state;
        Bonsai.Effect.Many([set_state(true), effect]);
      | {state: true, _} => Bonsai.const(Ui_effect.Ignore)
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
    let%sub (size, update) =
      state(~sexp_of_model=Size.sexp_of_t, ~equal=Size.equal, default);
    let startup = {
      let%map update = update;
      Effect.of_sync_fun(
        () => {
          let _ =
            ResizeObserver.observe(
              ~node=node(),
              ~f=
                (_, _) => {
                  let el = node();
                  let rect = el##getBoundingClientRect;
                  Size.{
                    width: Js.to_float(rect##.right) -. Js.to_float(rect##.left),
                    height: Js.to_float(rect##.bottom) -. Js.to_float(rect##.top),
                  }
                  |> update
                  |> Effect.Expert.handle(~on_exn=raise);
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
