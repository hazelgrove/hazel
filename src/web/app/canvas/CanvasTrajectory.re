/* CanvasTrajectory — record an agent run as the sequence of LLM replies
   (their tool calls, with arguments) and agent busy/idle edges, with
   timestamps; replay it later through the SAME handler the real reply
   goes through (AgentAction.ReplayToolCalls), no LLM involved. This is
   how canvas choreography gets validated against a real trajectory
   instead of typing trials (plans/agent-canvas-principles.md E2).

   Console:  copy(__constellationTrajectory())   — the recording as JSON
             __constellationTrajectoryClear()
             __replayTrajectory(jsonText, speed)  — speed 1 = real time */

open Js_of_ocaml;

type event =
  | Reply(list((string, Yojson.Safe.t))) /* (tool name, arguments) */
  | Busy(bool)
  | Tick; /* a streamed-reasoning chunk arrived (a render while thinking) */

type stamped = {
  t: float, /* ms since the recording's first event */
  ev: event,
};

let events: ref(list(stamped)) = ref([]); /* newest first */
let t0: ref(option(float)) = ref(None);
let replaying: ref(bool) = ref(false);

let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();

let stamp = (ev: event): unit =>
  if (! replaying^) {
    let t = now();
    let base =
      switch (t0^) {
      | Some(b) => b
      | None =>
        t0 := Some(t);
        t;
      };
    events :=
      [
        {
          t: t -. base,
          ev,
        },
        ...events^,
      ];
  };

let reply = (calls: list((string, Yojson.Safe.t))): unit =>
  stamp(Reply(calls));
let busy = (b: bool): unit => stamp(Busy(b));
/* stream chunks arrive many times a second; one tick per 100 ms is enough
   to reproduce the renders they cause */
let last_tick: ref(float) = ref(0.);
let tick = (): unit => {
  let t = now();
  if (t -. last_tick^ >= 100.) {
    last_tick := t;
    stamp(Tick);
  };
};
let clear = (): unit => {
  events := [];
  t0 := None;
};

let to_json = (): Yojson.Safe.t =>
  `List(
    List.rev_map(
      ({t, ev}) =>
        switch (ev) {
        | Reply(calls) =>
          `Assoc([
            ("t", `Float(t)),
            ("kind", `String("reply")),
            (
              "tools",
              `List(
                List.map(
                  ((name, args)) =>
                    `Assoc([("name", `String(name)), ("args", args)]),
                  calls,
                ),
              ),
            ),
          ])
        | Busy(b) =>
          `Assoc([
            ("t", `Float(t)),
            ("kind", `String("busy")),
            ("on", `Bool(b)),
          ])
        | Tick => `Assoc([("t", `Float(t)), ("kind", `String("tick"))])
        },
      events^,
    ),
  );

let of_json = (j: Yojson.Safe.t): list(stamped) => {
  let num = j =>
    switch (j) {
    | `Float(f) => f
    | `Int(i) => float_of_int(i)
    | _ => 0.
    };
  switch (j) {
  | `List(items) =>
    List.filter_map(
      item =>
        switch (item) {
        | `Assoc(fields) =>
          let t =
            num(
              Option.value(~default=`Int(0), List.assoc_opt("t", fields)),
            );
          switch (List.assoc_opt("kind", fields)) {
          | Some(`String("reply")) =>
            let tools =
              switch (List.assoc_opt("tools", fields)) {
              | Some(`List(ts)) =>
                List.filter_map(
                  tj =>
                    switch (tj) {
                    | `Assoc(tf) =>
                      switch (List.assoc_opt("name", tf)) {
                      | Some(`String(name)) =>
                        Some((
                          name,
                          Option.value(
                            ~default=`Assoc([]),
                            List.assoc_opt("args", tf),
                          ),
                        ))
                      | _ => None
                      }
                    | _ => None
                    },
                  ts,
                )
              | _ => []
              };
            Some({
              t,
              ev: Reply(tools),
            });
          | Some(`String("busy")) =>
            let on =
              switch (List.assoc_opt("on", fields)) {
              | Some(`Bool(b)) => b
              | _ => false
              };
            Some({
              t,
              ev: Busy(on),
            });
          | Some(`String("tick")) =>
            Some({
              t,
              ev: Tick,
            })
          | _ => None
          };
        | _ => None
        },
      items,
    )
  | _ => []
  };
};

/* ---- replay ---- */

/* installed by Main: dispatch one synthetic reply's tool calls through the
   agent's own response handler; mark the agent busy/idle for the avatar */
let dispatch_reply: ref(list((string, Yojson.Safe.t)) => unit) =
  ref(_ => ());
let set_busy: ref(bool => unit) = ref(_ => ());
let dispatch_tick: ref(unit => unit) = ref(() => ());
/* opens the replay as a user turn: the chat display assumes agent
   messages answer one, and the chat is autosaved */
let dispatch_begin: ref(string => unit) = ref(_ => ());

let later = (ms: float, f: unit => unit): unit =>
  ignore(Js.Unsafe.global##setTimeout(Js.Unsafe.callback(f), ms));

let replay = (~speed: float=1., text: string): unit =>
  switch (Yojson.Safe.from_string(text)) {
  | exception _ => CanvasLog.log("replay: could not parse the trajectory")
  | j =>
    let steps = of_json(j);
    let speed = speed <= 0. ? 1. : speed;
    let n_replies =
      List.length(
        List.filter(
          s =>
            switch (s.ev) {
            | Reply(_) => true
            | Busy(_)
            | Tick => false
            },
          steps,
        ),
      );
    let last = List.fold_left((m, s) => max(m, s.t), 0., steps);
    CanvasLog.log(
      Printf.sprintf(
        "replay: %d step(s), %d repl%s, %.1fs at %.1fx",
        List.length(steps),
        n_replies,
        n_replies == 1 ? "y" : "ies",
        last /. 1000.,
        speed,
      ),
    );
    replaying := true;
    dispatch_begin^(
      Printf.sprintf(
        "[replay] %d repl%s over %.1fs, at %.1fx",
        n_replies,
        n_replies == 1 ? "y" : "ies",
        last /. 1000.,
        speed,
      ),
    );
    List.iter(
      s =>
        later(s.t /. speed, () =>
          switch (s.ev) {
          | Reply(calls) =>
            CanvasLog.log(
              Printf.sprintf(
                "replay: reply with %s",
                String.concat(", ", List.map(fst, calls)),
              ),
            );
            dispatch_reply^(calls);
          | Busy(b) =>
            CanvasLog.log("replay: agent " ++ (b ? "busy" : "idle"));
            set_busy^(b);
          | Tick => dispatch_tick^()
          }
        ),
      steps,
    );
    later(
      (last +. 1500.) /. speed,
      () => {
        replaying := false;
        set_busy^(false);
        CanvasLog.log("replay: done");
      },
    );
  };

let install_testers = (): unit => {
  let g = Js.Unsafe.global;
  if (!Js.Optdef.test(Js.Unsafe.get(g, "__constellationTrajectory"))) {
    Js.Unsafe.set(
      g,
      "__constellationTrajectory",
      Js.Unsafe.callback(() => Js.string(Yojson.Safe.to_string(to_json()))),
    );
    Js.Unsafe.set(
      g,
      "__constellationTrajectoryClear",
      Js.Unsafe.callback(() => clear()),
    );
    Js.Unsafe.set(
      g,
      "__replayTrajectory",
      Js.Unsafe.callback(
        (text: Js.t(Js.js_string), speed: Js.Optdef.t(float)) =>
        replay(~speed=Js.Optdef.get(speed, () => 1.), Js.to_string(text))
      ),
    );
  };
};
