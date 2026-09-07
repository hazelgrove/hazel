/* CanvasTrajectory — record every agent run as a TRACE: the run's header
   (slide, prompt, starting program, pane size) plus the timed sequence
   of LLM replies (their tool calls, with arguments), busy/idle edges,
   streamed-reasoning ticks and follow-up prompts. A trace replays later
   through the SAME handler the real reply goes through
   (AgentAction.ReplayToolCalls), no LLM involved — the canvas
   choreography is validated against real trajectories instead of typing
   trials (plans/agent-canvas-principles.md E2).

   Runs segment themselves (a prompt or busy edge after idle opens one;
   6 s of idle after the last busy edge closes it) and persist in the app
   database (HazelDB kv, `canvas:trace:<id>`, index `canvas:traces`), the
   last 20 kept. The traces panel (record dot in the canvas toolbar, or
   __constellationTraces()) lists them: replay, copy, keep to the repo
   folder (vite /__traces), delete.

   Replay has a virtual clock: pause/resume/step, speed changes, and
   marks (journal lines stamped with the trace time — every journal line
   carries `[R t]` while a replay runs, so observations by trace time
   land on the same clock as the score).

   Console: __constellationTrajectory() (current or last run as JSON),
   __replayTrajectory(json, speed), __replayPause/Resume/Step/Stop(),
   __replaySpeed(x), __replayMark(note), __constellationTraces(),
   __simulateRun(json, speed) (a replay that IS recorded, for testing). */

open Js_of_ocaml;

type event =
  | Reply(list((string, Yojson.Safe.t))) /* (tool name, arguments) */
  | Busy(bool)
  | Tick /* a streamed-reasoning chunk arrived (a render while thinking) */
  | Prompt(string); /* the user's prompt (first one is the header's) */

type stamped = {
  t: float, /* ms since the run began */
  ev: event,
};

type header = {
  id: string, /* ms epoch of the run start, as text */
  started_at: string, /* ISO */
  slide: string,
  prompt: string,
  program: option(string), /* the slide's program when the run began */
  pane: (int, int) /* canvas pane size, for frame decisions */
};

type run = {
  header,
  mutable events: list(stamped), /* newest first */
  t0: float,
  mutable last_t: float,
  mutable finalize_pending: bool,
};

/* ---- environment hooks (installed by Main / the sidebar) ---- */

let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();
let iso_now = (): string => {
  let d = Js.Unsafe.new_obj(Js.Unsafe.global##._Date, [||]);
  Js.to_string(Js.Unsafe.meth_call(d, "toISOString", [||]));
};
let later = (ms: float, f: unit => unit): unit =>
  ignore(Js.Unsafe.global##setTimeout(Js.Unsafe.callback(f), ms));

let slide_name: ref(string) = ref("");
let program_text: ref(unit => option(string)) = ref(() => None);
/* something about the recording/replay state changed: re-render */
let on_change: ref(unit => unit) = ref(() => ());
let pane_size = (): (int, int) =>
  switch (Util.JsUtil.get_elem_by_id_opt("canvas-scroll")) {
  | Some(el) => (
      Js.Unsafe.get(el, "clientWidth"),
      Js.Unsafe.get(el, "clientHeight"),
    )
  | None => (0, 0)
  };

/* ---- recording ---- */

let current: ref(option(run)) = ref(None);
let last_finalized: ref(option(run)) = ref(None);
/* a replay never calls the LLM (AgentSend checks this) ... */
let replaying: ref(bool) = ref(false);
/* ... and is not recorded, unless simulating a run */
let recording_off: ref(bool) = ref(false);

let index_key = "canvas:traces";
let trace_key = (id: string) => "canvas:trace:" ++ id;
let keep_last = 20;

type summary = {
  s_id: string,
  s_started_at: string,
  s_slide: string,
  s_prompt: string,
  s_secs: float,
  s_replies: int,
  s_kept: bool,
  s_live: bool /* still recording when last saved (a crash leaves it so) */
};

let summary_to_json = (s: summary): Yojson.Safe.t =>
  `Assoc([
    ("id", `String(s.s_id)),
    ("started_at", `String(s.s_started_at)),
    ("slide", `String(s.s_slide)),
    ("prompt", `String(s.s_prompt)),
    ("secs", `Float(s.s_secs)),
    ("replies", `Int(s.s_replies)),
    ("kept", `Bool(s.s_kept)),
    ("live", `Bool(s.s_live)),
  ]);

let str = (fields, k) =>
  switch (List.assoc_opt(k, fields)) {
  | Some(`String(s)) => s
  | _ => ""
  };
let num = (fields, k) =>
  switch (List.assoc_opt(k, fields)) {
  | Some(`Float(f)) => f
  | Some(`Int(i)) => float_of_int(i)
  | _ => 0.
  };

let summary_of_json = (j: Yojson.Safe.t): option(summary) =>
  switch (j) {
  | `Assoc(f) =>
    Some({
      s_id: str(f, "id"),
      s_started_at: str(f, "started_at"),
      s_slide: str(f, "slide"),
      s_prompt: str(f, "prompt"),
      s_secs: num(f, "secs"),
      s_replies: int_of_float(num(f, "replies")),
      s_kept:
        switch (List.assoc_opt("kept", f)) {
        | Some(`Bool(b)) => b
        | _ => false
        },
      s_live:
        switch (List.assoc_opt("live", f)) {
        | Some(`Bool(b)) => b
        | _ => false
        },
    })
  | _ => None
  };

let index = (): list(summary) =>
  switch (HazelDB.kv_get(index_key)) {
  | None => []
  | Some(text) =>
    switch (Yojson.Safe.from_string(text)) {
    | exception _ => []
    | `List(items) => List.filter_map(summary_of_json, items)
    | _ => []
    }
  };
let save_index = (s: list(summary)): unit =>
  HazelDB.kv_save(
    index_key,
    Yojson.Safe.to_string(`List(List.map(summary_to_json, s))),
  );

let n_replies = (events: list(stamped)): int =>
  List.length(
    List.filter(
      s =>
        switch (s.ev) {
        | Reply(_) => true
        | _ => false
        },
      events,
    ),
  );

let run_to_json = (r: run): Yojson.Safe.t => {
  let h = r.header;
  `Assoc([
    ("version", `Int(2)),
    ("id", `String(h.id)),
    ("started_at", `String(h.started_at)),
    ("slide", `String(h.slide)),
    ("prompt", `String(h.prompt)),
    (
      "program",
      switch (h.program) {
      | Some(p) => `String(p)
      | None => `Null
      },
    ),
    ("pane", `List([`Int(fst(h.pane)), `Int(snd(h.pane))])),
    (
      "events",
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
            | Tick =>
              `Assoc([("t", `Float(t)), ("kind", `String("tick"))])
            | Prompt(text) =>
              `Assoc([
                ("t", `Float(t)),
                ("kind", `String("prompt")),
                ("text", `String(text)),
              ])
            },
          r.events,
        ),
      ),
    ),
  ]);
};

/* write the run and its index entry; ~live marks a run still recording,
   so a crash mid-run leaves the trace up to its last event (saved at most
   once a second) instead of losing it */
let save_run = (~live: bool, r: run): unit => {
  HazelDB.kv_save(
    trace_key(r.header.id),
    Yojson.Safe.to_string(run_to_json(r)),
  );
  let kept =
    switch (List.find_opt(x => x.s_id == r.header.id, index())) {
    | Some(x) => x.s_kept
    | None => false
    };
  let s = {
    s_id: r.header.id,
    s_started_at: r.header.started_at,
    s_slide: r.header.slide,
    s_prompt: r.header.prompt,
    s_secs: r.last_t /. 1000.,
    s_replies: n_replies(r.events),
    s_kept: kept,
    s_live: live,
  };
  let all = [s, ...List.filter(x => x.s_id != s.s_id, index())];
  /* the cap spares kept traces */
  let (keep, drop) =
    List.fold_left(
      ((k, d), x) =>
        List.length(k) < keep_last || x.s_kept
          ? (k @ [x], d) : (k, d @ [x]),
      ([], []),
      all,
    );
  List.iter(x => HazelDB.kv_delete(trace_key(x.s_id)), drop);
  save_index(keep);
};
let last_checkpoint: ref(float) = ref(0.);
let checkpoint = (r: run): unit =>
  if (now() -. last_checkpoint^ > 1000.) {
    last_checkpoint := now();
    save_run(~live=true, r);
  };

let finalize = (): unit =>
  switch (current^) {
  | None => ()
  | Some(r) =>
    current := None;
    if (r.events != []) {
      last_finalized := Some(r);
      save_run(~live=false, r);
      CanvasLog.log(
        Printf.sprintf(
          "trace: run saved (%d repl%s, %.0fs)",
          n_replies(r.events),
          n_replies(r.events) == 1 ? "y" : "ies",
          r.last_t /. 1000.,
        ),
      );
    };
    on_change^();
  };

let begin_run = (~prompt: string): run => {
  let t = now();
  let r = {
    header: {
      id: Printf.sprintf("%.0f", t),
      started_at: iso_now(),
      slide: slide_name^,
      prompt,
      program:
        switch (program_text^()) {
        | Some(p) when String.trim(p) == "" || String.trim(p) == "?" => None
        | p => p
        },
      pane: pane_size(),
    },
    events: [],
    t0: t,
    last_t: 0.,
    finalize_pending: false,
  };
  current := Some(r);
  CanvasLog.log(
    "trace: run started"
    ++ (
      prompt == ""
        ? ""
        : " — " ++ String.sub(prompt, 0, min(40, String.length(prompt)))
    ),
  );
  on_change^();
  r;
};

let idle_close_ms = 6000.;

let rec schedule_finalize = (r: run): unit =>
  if (!r.finalize_pending) {
    r.finalize_pending = true;
    later(
      idle_close_ms,
      () => {
        r.finalize_pending = false;
        switch (current^) {
        | Some(cur) when cur === r =>
          let idle =
            switch (r.events) {
            | [{ev: Busy(false), _}, ..._] => true
            | [] => true
            | _ => false
            };
          if (idle && now() -. (r.t0 +. r.last_t) >= idle_close_ms -. 500.) {
            finalize();
          } else if (idle) {
            schedule_finalize(r);
          };
        | _ => ()
        };
      },
    );
  };

let stamp = (ev: event): unit =>
  if (! recording_off^) {
    let r =
      switch (current^, ev) {
      | (Some(r), _) => r
      | (None, Prompt(p)) => begin_run(~prompt=p)
      | (None, _) => begin_run(~prompt="")
      };
    let t = now() -. r.t0;
    r.last_t = t;
    r.events = [
      {
        t,
        ev,
      },
      ...r.events,
    ];
    checkpoint(r);
    switch (ev) {
    | Busy(false) => schedule_finalize(r)
    | _ => ()
    };
  };

let reply = (calls: list((string, Yojson.Safe.t))): unit =>
  stamp(Reply(calls));
let busy = (b: bool): unit => stamp(Busy(b));
/* a prompt after the agent went idle opens a new run */
let prompt = (text: string): unit => {
  switch (current^) {
  | Some(r) =>
    switch (r.events) {
    | [{ev: Busy(false), _}, ..._] => finalize()
    | _ => ()
    }
  | None => ()
  };
  stamp(Prompt(text));
};
/* stream chunks arrive many times a second; one tick per 100 ms is enough
   to reproduce the renders they cause */
let last_tick: ref(float) = ref(0.);
let tick = (): unit => {
  let t = now();
  if (t -. last_tick^ >= 100.) {
    last_tick := t;
    switch (current^) {
    | Some(_) => stamp(Tick)
    | None => ()
    };
  };
};
let clear = (): unit => {
  current := None;
  on_change^();
};

/* the run in progress, else the last one finished */
let latest_json = (): string =>
  switch (current^, last_finalized^) {
  | (Some(r), _)
  | (None, Some(r)) => Yojson.Safe.to_string(run_to_json(r))
  | (None, None) => "[]"
  };

/* ---- parsing (v2 object, or a legacy bare event list) ---- */

let events_of_json = (j: Yojson.Safe.t): list(stamped) =>
  switch (j) {
  | `List(items) =>
    List.filter_map(
      item =>
        switch (item) {
        | `Assoc(fields) =>
          let t = num(fields, "t");
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
          | Some(`String("prompt")) =>
            Some({
              t,
              ev: Prompt(str(fields, "text")),
            })
          | _ => None
          };
        | _ => None
        },
      items,
    )
  | _ => []
  };

let parse = (text: string): option((option(header), list(stamped))) =>
  switch (Yojson.Safe.from_string(text)) {
  | exception _ => None
  | `List(_) as j => Some((None, events_of_json(j)))
  | `Assoc(f) =>
    let pane =
      switch (List.assoc_opt("pane", f)) {
      | Some(`List([`Int(w), `Int(h)])) => (w, h)
      | _ => (0, 0)
      };
    Some((
      Some({
        id: str(f, "id"),
        started_at: str(f, "started_at"),
        slide: str(f, "slide"),
        prompt: str(f, "prompt"),
        program:
          switch (List.assoc_opt("program", f)) {
          | Some(`String(p)) => Some(p)
          | _ => None
          },
        pane,
      }),
      Option.value(~default=`List([]), List.assoc_opt("events", f))
      |> events_of_json,
    ));
  | _ => None
  };

/* ---- replay ---- */

/* installed by Main: dispatch one synthetic reply's tool calls through the
   agent's own response handler; mark the agent busy/idle for the avatar */
let dispatch_reply: ref(list((string, Yojson.Safe.t)) => unit) =
  ref(_ => ());
let set_busy: ref(bool => unit) = ref(_ => ());
let dispatch_tick: ref(unit => unit) = ref(() => ());
/* opens the replay (and each recorded prompt) as a user turn: the chat
   display assumes agent messages answer one, and the chat is autosaved */
let dispatch_begin: ref(string => unit) = ref(_ => ());
/* a trace with a starting program replays on a fresh slide */
let dispatch_new_slide: ref(unit => unit) = ref(() => ());
let dispatch_paste: ref(string => unit) = ref(_ => ());

type player = {
  steps: array(stamped),
  mutable idx: int,
  mutable speed: float,
  mutable paused: bool,
  mutable t_trace: float, /* trace ms at wall_anchor */
  mutable wall_anchor: float,
  mutable timer: option(Js.Unsafe.any),
  mutable finished: bool,
  total: float,
  label: string,
  header: option(header),
};

let player: ref(option(player)) = ref(None);

let trace_now = (p: player): float =>
  p.paused || p.finished
    ? p.t_trace : p.t_trace +. (now() -. p.wall_anchor) *. p.speed;

let cancel_timer = (p: player): unit =>
  switch (p.timer) {
  | Some(id) =>
    ignore(Js.Unsafe.global##clearTimeout(id));
    p.timer = None;
  | None => ()
  };

let dispatch = (s: stamped): unit =>
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
  | Prompt(text) =>
    CanvasLog.log("replay: prompt — " ++ text);
    dispatch_begin^(text);
  };

let finish = (p: player): unit =>
  if (!p.finished) {
    p.finished = true;
    p.t_trace = trace_now(p);
    cancel_timer(p);
    replaying := false;
    recording_off := false;
    set_busy^(false);
    CanvasLog.log("replay: done");
    on_change^();
  };

let rec schedule = (p: player): unit => {
  cancel_timer(p);
  if (p.finished || p.paused) {
    ();
  } else if (p.idx >= Array.length(p.steps)) {
    /* let the last beats land before the agent reads as idle */
    p.timer =
      Some(
        Js.Unsafe.global##setTimeout(
          Js.Unsafe.callback(() => finish(p)),
          1500. /. p.speed,
        ),
      );
  } else {
    let wait = (p.steps[p.idx].t -. trace_now(p)) /. p.speed;
    p.timer =
      Some(
        Js.Unsafe.global##setTimeout(
          Js.Unsafe.callback(() => fire(p)),
          max(0., wait),
        ),
      );
  };
}
and fire = (p: player): unit => {
  p.timer = None;
  let t = trace_now(p) +. 1.;
  while (p.idx < Array.length(p.steps) && p.steps[p.idx].t <= t) {
    dispatch(p.steps[p.idx]);
    p.idx = p.idx + 1;
  };
  on_change^();
  schedule(p);
};

let pause = (): unit =>
  switch (player^) {
  | Some(p) when !p.finished && !p.paused =>
    p.t_trace = trace_now(p);
    p.paused = true;
    cancel_timer(p);
    CanvasLog.log("replay: paused");
    on_change^();
  | _ => ()
  };
let resume = (): unit =>
  switch (player^) {
  | Some(p) when !p.finished && p.paused =>
    p.wall_anchor = now();
    p.paused = false;
    CanvasLog.log("replay: resumed");
    schedule(p);
    on_change^();
  | _ => ()
  };
/* while paused: play exactly the next event, the clock jumping to it */
let step = (): unit =>
  switch (player^) {
  | Some(p) when !p.finished && p.paused && p.idx < Array.length(p.steps) =>
    let s = p.steps[p.idx];
    p.t_trace = s.t;
    dispatch(s);
    p.idx = p.idx + 1;
    on_change^();
  | _ => ()
  };
let set_speed = (speed: float): unit =>
  switch (player^) {
  | Some(p) when !p.finished =>
    p.t_trace = trace_now(p);
    p.wall_anchor = now();
    p.speed = speed <= 0. ? 1. : speed;
    CanvasLog.log(Printf.sprintf("replay: speed %.1fx", p.speed));
    schedule(p);
    on_change^();
  | _ => ()
  };
let stop = (): unit =>
  switch (player^) {
  | Some(p) when !p.finished =>
    CanvasLog.log("replay: stopped");
    finish(p);
  | _ => ()
  };
let mark = (note: string): unit =>
  CanvasLog.log("MARK " ++ (note == "" ? "(here)" : note));

/* the trace clock for the journal, while a replay runs */
let trace_clock = (): option(string) =>
  switch (player^) {
  | Some(p) when !p.finished =>
    Some(Printf.sprintf("%.1f", trace_now(p) /. 1000.))
  | _ => None
  };
let clock_text = (p: player): string => {
  let s = trace_now(p) /. 1000.;
  let m = int_of_float(s /. 60.);
  Printf.sprintf("%d:%04.1f", m, s -. float_of_int(m) *. 60.);
};
/* the HUD clock is written straight to the DOM, not through the vdom */
let clock_interval: ref(option(Js.Unsafe.any)) = ref(None);
let start_clock = (): unit =>
  if (clock_interval^ == None) {
    clock_interval :=
      Some(
        Js.Unsafe.global##setInterval(
          Js.Unsafe.callback(() =>
            switch (player^, Util.JsUtil.get_elem_by_id_opt("replay-clock")) {
            | (Some(p), Some(el)) =>
              Js.Unsafe.set(el, "textContent", Js.string(clock_text(p)));
              if (p.finished) {
                switch (clock_interval^) {
                | Some(id) =>
                  ignore(Js.Unsafe.global##clearInterval(id));
                  clock_interval := None;
                | None => ()
                };
              };
            | _ => ()
            }
          ),
          100.,
        ),
      );
  };

let replay = (~speed: float=1., ~record: bool=false, text: string): unit =>
  switch (parse(text)) {
  | None => CanvasLog.log("replay: could not parse the trajectory")
  | Some((header, steps)) =>
    switch (player^) {
    | Some(p) when !p.finished => finish(p)
    | _ => ()
    };
    let speed = speed <= 0. ? 1. : speed;
    let total = List.fold_left((m, s) => max(m, s.t), 0., steps);
    let n = n_replies(steps);
    let label =
      Printf.sprintf(
        "[replay] %d repl%s over %.1fs, at %.1fx",
        n,
        n == 1 ? "y" : "ies",
        total /. 1000.,
        speed,
      );
    CanvasLog.log(
      Printf.sprintf(
        "replay: %d step(s), %d repl%s, %.1fs at %.1fx%s",
        List.length(steps),
        n,
        n == 1 ? "y" : "ies",
        total /. 1000.,
        speed,
        record ? " (recording)" : "",
      ),
    );
    replaying := true;
    recording_off := !record;
    switch (header) {
    | Some({program: Some(prog), slide, _}) =>
      CanvasLog.log("replay: fresh slide with the program from " ++ slide);
      dispatch_new_slide^();
      dispatch_paste^(prog);
    | _ => ()
    };
    dispatch_begin^(
      switch (header) {
      | Some({prompt, _}) when prompt != "" => label ++ "\n\n" ++ prompt
      | _ => label
      },
    );
    let p = {
      steps: Array.of_list(List.sort((a, b) => compare(a.t, b.t), steps)),
      idx: 0,
      speed,
      paused: false,
      t_trace: 0.,
      wall_anchor: now(),
      timer: None,
      finished: false,
      total,
      label,
      header,
    };
    player := Some(p);
    start_clock();
    schedule(p);
    on_change^();
  };

/* ---- the traces panel ---- */

let panel_open: ref(bool) = ref(false);
let refresh_hook: ref(unit => unit) = ref(() => ());
let toggle_panel = (): unit => {
  panel_open := ! panel_open^;
  if (panel_open^) {
    refresh_hook^();
  };
  on_change^();
};

/* traces kept in the repo folder, via the dev server's /__traces */
let repo_traces: ref(list(string)) = ref([]);
let fetch_text =
    (url: string, ~init: option(Js.Unsafe.any)=?, cb: (bool, string) => unit)
    : unit => {
  let args =
    switch (init) {
    | Some(i) => [|Js.Unsafe.inject(Js.string(url)), i|]
    | None => [|Js.Unsafe.inject(Js.string(url))|]
    };
  let p = Js.Unsafe.fun_call(Js.Unsafe.global##.fetch, args);
  ignore(
    Js.Unsafe.meth_call(
      p,
      "then",
      [|
        Js.Unsafe.inject(
          Js.Unsafe.callback(resp => {
            let ok: bool = Js.to_bool(Js.Unsafe.get(resp, "ok"));
            let tp = Js.Unsafe.meth_call(resp, "text", [||]);
            ignore(
              Js.Unsafe.meth_call(
                tp,
                "then",
                [|
                  Js.Unsafe.inject(
                    Js.Unsafe.callback(t => cb(ok, Js.to_string(t))),
                  ),
                |],
              ),
            );
          }),
        ),
        Js.Unsafe.inject(Js.Unsafe.callback(_ => cb(false, ""))),
      |],
    ),
  );
};
let refresh_repo_traces = (): unit =>
  fetch_text("/__traces", (ok, text) =>
    if (ok) {
      repo_traces :=
        (
          switch (Yojson.Safe.from_string(text)) {
          | exception _ => []
          | `List(items) =>
            List.filter_map(
              fun
              | `Assoc(f) => Some(str(f, "name"))
              | _ => None,
              items,
            )
          | _ => []
          }
        );
      on_change^();
    }
  );

refresh_hook := refresh_repo_traces;

let sanitize_name = (s: string): string =>
  String.map(
    c =>
      switch (c) {
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '-'
      | '_'
      | '.' => c
      | _ => '-'
      },
    s,
  );

let file_name = (s: summary): string => {
  let when_ =
    String.length(s.s_started_at) >= 19
      ? String.sub(s.s_started_at, 0, 19) : s.s_started_at;
  sanitize_name(s.s_slide) ++ "-" ++ sanitize_name(when_) ++ ".json";
};

let keep = (id: string): unit =>
  switch (
    List.find_opt(s => s.s_id == id, index()),
    HazelDB.kv_get(trace_key(id)),
  ) {
  | (Some(s), Some(body)) =>
    let init =
      Js.Unsafe.obj([|
        ("method", Js.Unsafe.inject(Js.string("POST"))),
        (
          "headers",
          Js.Unsafe.inject(
            Js.Unsafe.obj([|
              (
                "Content-Type",
                Js.Unsafe.inject(Js.string("application/json")),
              ),
            |]),
          ),
        ),
        ("body", Js.Unsafe.inject(Js.string(body))),
      |]);
    fetch_text(
      "/__traces/" ++ file_name(s),
      ~init,
      (ok, _) => {
        if (ok) {
          save_index(
            List.map(
              x =>
                x.s_id == id
                  ? {
                    ...x,
                    s_kept: true,
                  }
                  : x,
              index(),
            ),
          );
          CanvasLog.log("trace: kept as trajectories/" ++ file_name(s));
          refresh_repo_traces();
        } else {
          CanvasLog.log(
            "trace: keep FAILED (is the dev server's /__traces up?)",
          );
        };
        on_change^();
      },
    );
  | _ => ()
  };

let delete = (id: string): unit => {
  HazelDB.kv_delete(trace_key(id));
  save_index(List.filter(s => s.s_id != id, index()));
  on_change^();
};

let replay_saved = (~speed: float, id: string): unit =>
  switch (HazelDB.kv_get(trace_key(id))) {
  | Some(text) => replay(~speed, text)
  | None => CanvasLog.log("trace: not found " ++ id)
  };

let replay_repo = (~speed: float, name: string): unit =>
  fetch_text("/trajectories/" ++ name, (ok, text) =>
    if (ok) {
      replay(~speed, text);
    } else {
      CanvasLog.log("trace: could not load trajectories/" ++ name);
    }
  );

let copy_saved = (id: string): unit =>
  switch (HazelDB.kv_get(trace_key(id))) {
  | Some(text) =>
    ignore(
      Js.Unsafe.meth_call(
        Js.Unsafe.get(Js.Unsafe.global##.navigator, "clipboard"),
        "writeText",
        [|Js.Unsafe.inject(Js.string(text))|],
      ),
    );
    CanvasLog.log("trace: copied to the clipboard");
  | None => ()
  };

let install_testers = (): unit => {
  CanvasLog.trace_clock := trace_clock;
  let g = Js.Unsafe.global;
  if (!Js.Optdef.test(Js.Unsafe.get(g, "__constellationTrajectory"))) {
    let set = (name, f) => Js.Unsafe.set(g, name, Js.Unsafe.callback(f));
    set("__constellationTrajectory", () => Js.string(latest_json()));
    set("__constellationTrajectoryClear", () => clear());
    /* __streamBurst(n, ms): n stream ticks ms apart — the live
       token-rate render load, which recorded runs throttle to 10/s */
    set("__streamBurst", (n: float, ms: float) => {
      let left = ref(int_of_float(n));
      let rec go = () =>
        if (left^ > 0) {
          decr(left);
          dispatch_tick^();
          ignore(
            Js.Unsafe.meth_call(
              Js.Unsafe.global##.window,
              "setTimeout",
              [|
                Js.Unsafe.inject(Js.Unsafe.callback(go)),
                Js.Unsafe.inject(ms),
              |],
            ),
          );
        };
      go();
    });
    set(
      "__replayTrajectory",
      (text: Js.t(Js.js_string), speed: Js.Optdef.t(float)) =>
      replay(~speed=Js.Optdef.get(speed, () => 1.), Js.to_string(text))
    );
    set(
      "__simulateRun", (text: Js.t(Js.js_string), speed: Js.Optdef.t(float)) =>
      replay(
        ~record=true,
        ~speed=Js.Optdef.get(speed, () => 1.),
        Js.to_string(text),
      )
    );
    set("__replayPause", () => pause());
    set("__replayResume", () => resume());
    set("__replayStep", () => step());
    set("__replayStop", () => stop());
    set("__replaySpeed", (s: float) => set_speed(s));
    set("__replayMark", (note: Js.Optdef.t(Js.t(Js.js_string))) =>
      mark(Js.Optdef.case(note, () => "", n => Js.to_string(n)))
    );
    set("__constellationTraces", () => toggle_panel());
  };
};
