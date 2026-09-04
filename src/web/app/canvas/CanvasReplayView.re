/* CanvasReplayView — the replay HUD (trace clock, step, speed, pause,
   marks) and the traces panel, both floating over the canvas in a
   zero-height row. Hidden unless a replay runs or the panel is open; the
   record dot in the toolbar (red while a run is being recorded) opens
   the panel. State lives in CanvasTrajectory. */

open Virtual_dom.Vdom;
open Node;
open Js_of_ocaml;

let clss = Attr.classes;

let btn = (~cls="", ~tip="", label: string, f: unit => unit): Node.t =>
  div(
    ~attrs=[
      clss(["canvas-tool-btn", "replay-btn"] @ (cls == "" ? [] : [cls])),
      Attr.title(tip),
      Attr.on_click(_ => {
        f();
        Effect.Stop_propagation;
      }),
    ],
    [text(label)],
  );

let short = (n: int, s: string): string => {
  let s = String.map(c => c == '\n' ? ' ' : c, s);
  String.length(s) > n ? String.sub(s, 0, n - 1) ++ {js|…|js} : s;
};

let current_tools = (p: CanvasTrajectory.player): string =>
  if (p.idx == 0) {
    "";
  } else {
    switch (p.steps[p.idx - 1].ev) {
    | Reply(calls) => String.concat(", ", List.map(fst, calls))
    | Busy(true) => "thinking"
    | Busy(false) => "idle"
    | Tick => "streaming"
    | Prompt(_) => "prompt"
    };
  };

let hud = (): list(Node.t) =>
  switch (CanvasTrajectory.player^) {
  | None => []
  | Some(p) =>
    let n = Array.length(p.steps);
    let speed_btn = (s: float, label) =>
      btn(
        ~cls=Float.abs(p.speed -. s) < 0.01 ? "tool-active" : "",
        ~tip="replay speed",
        label,
        () =>
        CanvasTrajectory.set_speed(s)
      );
    [
      div(
        ~attrs=[
          clss(["canvas-replay-hud"] @ (p.finished ? ["replay-done"] : [])),
        ],
        [
          p.finished
            ? btn(~tip="dismiss", {js|✕|js}, () => {
                CanvasTrajectory.player := None;
                CanvasTrajectory.on_change^();
              })
            : p.paused
                ? btn(
                    ~tip="resume (trace clock runs again)",
                    {js|▶|js},
                    CanvasTrajectory.resume,
                  )
                : btn(
                    ~tip="pause between events",
                    {js|❚❚|js},
                    CanvasTrajectory.pause,
                  ),
          btn(
            ~cls=p.paused && !p.finished ? "" : "tool-disabled",
            ~tip="while paused: play the next event only",
            {js|⏭|js},
            CanvasTrajectory.step,
          ),
          span(
            ~attrs=[Attr.id("replay-clock"), clss(["replay-clock"])],
            [text(CanvasTrajectory.clock_text(p))],
          ),
          span(
            ~attrs=[clss(["replay-step"])],
            [
              text(
                Printf.sprintf(
                  "%d/%d %s",
                  p.idx,
                  n,
                  p.finished ? "done" : short(28, current_tools(p)),
                ),
              ),
            ],
          ),
          speed_btn(0.5, {js|½|js}),
          speed_btn(1., "1"),
          speed_btn(2., "2"),
          speed_btn(4., "4"),
          Node.input(
            ~attrs=[
              clss(["replay-note"]),
              Attr.placeholder("note, Enter = mark"),
              Attr.title(
                "type a note and press Enter: a MARK line with the trace time lands in the journal (copy(__constellationLogText()))",
              ),
              Attr.on_keydown(evt => {
                let key = Js.to_string(Js.Unsafe.get(evt, "key"));
                if (key == "Enter") {
                  let target = Js.Unsafe.get(evt, "target");
                  let v = Js.to_string(Js.Unsafe.get(target, "value"));
                  CanvasTrajectory.mark(v);
                  Js.Unsafe.set(target, "value", Js.string(""));
                };
                Effect.Stop_propagation;
              }),
              Attr.on_click(_ => Effect.Stop_propagation),
            ],
            (),
          ),
          btn(~tip="mark this moment in the journal", "mark", () =>
            CanvasTrajectory.mark("")
          ),
        ]
        /* (a Node.none here broke the vdom patch: "insertBefore ... not of
           type Node" killed the render loop) */
        @ (
          p.finished
            ? []
            : [
              btn(~tip="stop the replay", {js|■|js}, CanvasTrajectory.stop),
            ]
        ),
      ),
    ];
  };

let panel = (): list(Node.t) =>
  if (! CanvasTrajectory.panel_open^) {
    [];
  } else {
    let saved = CanvasTrajectory.index();
    let recording =
      switch (CanvasTrajectory.current^) {
      | Some(r) => Some(r)
      | None => None
      };
    let row_saved = (s: CanvasTrajectory.summary) =>
      div(
        ~attrs=[clss(["trace-row"])],
        [
          div(
            ~attrs=[clss(["trace-meta"])],
            [
              span(
                ~attrs=[clss(["trace-when"])],
                [
                  text(
                    String.length(s.s_started_at) >= 16
                      ? String.sub(s.s_started_at, 5, 11) : s.s_started_at,
                  ),
                ],
              ),
              span(~attrs=[clss(["trace-slide"])], [text(s.s_slide)]),
              span(
                ~attrs=[clss(["trace-stats"])],
                [
                  text(
                    Printf.sprintf(
                      "%.0fs · %d repl%s%s",
                      s.s_secs,
                      s.s_replies,
                      s.s_replies == 1 ? "y" : "ies",
                      s.s_kept ? " · kept" : "",
                    ),
                  ),
                ],
              ),
            ],
          ),
          div(
            ~attrs=[clss(["trace-prompt"]), Attr.title(s.s_prompt)],
            [
              text(s.s_prompt == "" ? "(no prompt)" : short(60, s.s_prompt)),
            ],
          ),
          div(
            ~attrs=[clss(["trace-actions"])],
            [
              btn(~tip="replay at 1x on a fresh slide", {js|▶|js}, () =>
                CanvasTrajectory.replay_saved(~speed=1., s.s_id)
              ),
              btn(~tip="replay at 2x", "2x", () =>
                CanvasTrajectory.replay_saved(~speed=2., s.s_id)
              ),
              btn(~tip="copy the trace JSON", "copy", () =>
                CanvasTrajectory.copy_saved(s.s_id)
              ),
              btn(
                ~cls=s.s_kept ? "tool-disabled" : "",
                ~tip=
                  "write it into the repo's trajectories folder (dev server)",
                "keep",
                () =>
                CanvasTrajectory.keep(s.s_id)
              ),
              btn(~tip="delete this recording", {js|✕|js}, () =>
                CanvasTrajectory.delete(s.s_id)
              ),
            ],
          ),
        ],
      );
    let row_repo = (name: string) =>
      div(
        ~attrs=[clss(["trace-row", "trace-repo"])],
        [
          div(~attrs=[clss(["trace-prompt"])], [text(name)]),
          div(
            ~attrs=[clss(["trace-actions"])],
            [
              btn(~tip="replay at 1x", {js|▶|js}, () =>
                CanvasTrajectory.replay_repo(~speed=1., name)
              ),
              btn(~tip="replay at 2x", "2x", () =>
                CanvasTrajectory.replay_repo(~speed=2., name)
              ),
            ],
          ),
        ],
      );
    [
      div(
        ~attrs=[
          clss(["canvas-traces-panel"]),
          Attr.on_click(_ => Effect.Stop_propagation),
        ],
        [
          div(
            ~attrs=[clss(["traces-head"])],
            [
              span(~attrs=[clss(["traces-title"])], [text("traces")]),
              span(
                ~attrs=[clss(["traces-note"])],
                [
                  text(
                    switch (recording) {
                    | Some(r) =>
                      Printf.sprintf(
                        "recording: %d event(s)",
                        List.length(r.events),
                      )
                    | None => "every agent run is recorded"
                    },
                  ),
                ],
              ),
              btn(~tip="close", {js|✕|js}, CanvasTrajectory.toggle_panel),
            ],
          ),
          div(
            ~attrs=[clss(["traces-list"])],
            saved == []
              ? [
                div(
                  ~attrs=[clss(["trace-empty"])],
                  [text("no recorded runs yet")],
                ),
              ]
              : List.map(row_saved, saved),
          ),
          div(
            ~attrs=[clss(["traces-head", "traces-sub"])],
            [
              span(~attrs=[clss(["traces-title"])], [text("repo")]),
              span(
                ~attrs=[clss(["traces-note"])],
                [text("src/web/www/trajectories (kept traces land here)")],
              ),
              btn(
                ~tip="refresh the list",
                {js|↻|js},
                CanvasTrajectory.refresh_repo_traces,
              ),
            ],
          ),
          div(
            ~attrs=[clss(["traces-list"])],
            CanvasTrajectory.repo_traces^ == []
              ? [
                div(
                  ~attrs=[clss(["trace-empty"])],
                  [text("nothing listed (dev server only)")],
                ),
              ]
              : List.map(row_repo, CanvasTrajectory.repo_traces^),
          ),
        ],
      ),
    ];
  };

/* the record dot for the toolbar: red while a run is being recorded */
let rec_dot = (): Node.t => {
  let recording = CanvasTrajectory.current^ != None;
  div(
    ~attrs=[
      clss(
        ["canvas-rec-dot"]
        @ (recording ? ["rec-on"] : [])
        @ (CanvasTrajectory.panel_open^ ? ["rec-open"] : []),
      ),
      Attr.title(
        (recording ? "recording this agent run" : "agent runs are recorded")
        ++ " — click for the traces panel (replay, keep, marks)",
      ),
      Attr.on_click(_ => {
        CanvasTrajectory.toggle_panel();
        if (CanvasTrajectory.panel_open^) {
          CanvasTrajectory.refresh_repo_traces();
        };
        Effect.Stop_propagation;
      }),
    ],
    [],
  );
};

/* the zero-height row over the canvas holding the HUD and the panel */
let overlay = (): Node.t =>
  div(~attrs=[clss(["canvas-replay-row"])], hud() @ panel());
