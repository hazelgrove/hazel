open Virtual_dom.Vdom;
open Node;
open Util;

/* View as agent's banner and its Sent and Trajectory panels. On screen,
   text reads tidied (JSON indented, sections folded); every copy button
   gives the bytes as sent, and each section can show itself raw. */

module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

/* a size in tokens, at 4 characters each; the characters on hover */
let size = (~cls=[], s: string): Node.t => {
  let n = Trajectory.chars(s);
  let tokens = (n + 3) / 4;
  span(
    ~attrs=[
      Attr.classes(cls),
      Attr.title(
        Haz3lcore.CompositionView.plural(n, "character", "characters"),
      ),
    ],
    [
      text(
        tokens < 1000
          ? Haz3lcore.CompositionView.plural(tokens, "token", "tokens")
          : Printf.sprintf("%.1fk tokens", float_of_int(tokens) /. 1000.),
      ),
    ],
  );
};

/* stroked in the text colour: two rounded squares, the back one showing
   past the front; once copied, a check */
let stroked_icon = (cls: string, ds: list(string)): Node.t =>
  create_svg(
    "svg",
    ~attrs=[
      Attr.classes([cls]),
      Attr.create("viewBox", "0 0 16 16"),
      Attr.create("aria-hidden", "true"),
    ],
    List.map(
      d => create_svg("path", ~attrs=[Attr.create("d", d)], []),
      ds,
    ),
  );

let copy_icon =
  stroked_icon(
    "lens-copy-icon",
    [
      "M3.5 5.5h5a2 2 0 0 1 2 2v5a2 2 0 0 1-2 2h-5a2 2 0 0 1-2-2v-5a2 2 0 0 1 2-2z",
      "M5.5 4v-.5a2 2 0 0 1 2-2h5a2 2 0 0 1 2 2v5a2 2 0 0 1-2 2H12",
    ],
  );

let copied_icon =
  stroked_icon("lens-copied-icon", ["M3 8.5l3.2 3.2L13 4.8"]);

/* a copy button's icon turns to a check for a second; on the element, as
   copying changes nothing in the model */
let flash_copied = (evt: Js.t(Dom_html.mouseEvent)): unit =>
  switch (Js.Opt.to_option(evt##.currentTarget)) {
  | Some(el) =>
    el##.classList##add(Js.string("copied"));
    ignore(
      Dom_html.window##setTimeout(
        Js.wrap_callback(() => el##.classList##remove(Js.string("copied"))),
        1000.0,
      ),
    );
  | None => ()
  };

let copy_button = (~cls=[], ~label=?, ~title, s: string): Node.t =>
  button(
    ~attrs=[
      Attr.classes(["lens-copy", ...cls]),
      Attr.title(title),
      Attr.on_click(evt => {
        JsUtil.copy_text(s);
        flash_copied(evt);
        Effect.Stop_propagation;
      }),
    ],
    [copy_icon, copied_icon]
    @ (
      switch (label) {
      | Some(l) => [text(l)]
      | None => []
      }
    ),
  );

let banner =
    (~go: Workspace.Action.t => Effect.t(unit), l: AgentLens.t): Node.t => {
  let agent = a => go(AgentAction(a));
  let button = (~cls=[], ~title, ~enabled=true, label, on) =>
    button(
      ~attrs=
        [
          Attr.classes(["lens-button", ...cls]),
          Attr.title(title),
          Attr.on_click(_ => on),
        ]
        @ (enabled ? [] : [Attr.disabled]),
      [text(label)],
    );
  let trajectory = l.mode == Trajectory || l.mode == Window;
  let segment = (mode: AgentLens.mode, label, title) =>
    Node.button(
      ~attrs=[
        Attr.classes([
          "lens-segment",
          ...l.mode == mode || mode == Trajectory && trajectory
               ? ["active"] : [],
        ]),
        Attr.title(title),
        /* Trajectory, chosen again, keeps its exact-window toggle */
        Attr.on_click(_ =>
          go(LensMode(mode == Trajectory && trajectory ? l.mode : mode))
        ),
      ],
      [text(label)],
    );
  div(
    ~attrs=[Attr.classes(["agent-lens-banner"])],
    [
      span(
        ~attrs=[Attr.classes(["lens-title"])],
        [text("Viewing as the agent")],
      ),
      span(
        ~attrs=[Attr.classes(["lens-which"])],
        [
          text(
            Printf.sprintf(
              "request %d of %d%s",
              l.seq,
              l.count,
              l.live ? {js| · live|js} : "",
            ),
          ),
        ],
      ),
      span(
        ~attrs=[
          Attr.classes(["lens-summary"]),
          Attr.title("The map's summary, as the agent saw it"),
        ],
        [text(l.summary)],
      ),
      div(
        ~attrs=[Attr.classes(["lens-controls"])],
        [
          div(
            ~attrs=[Attr.classes(["lens-segments"])],
            [
              segment(
                Stacked,
                "Stacked",
                "The open entries as cells, the outline folded as the map",
              ),
              segment(Sent, "Sent view", "The view's text, exactly as sent"),
              segment(
                Trajectory,
                "Trajectory",
                "Everything sent over the API up to this request",
              ),
            ],
          ),
        ]
        @ (
          trajectory
            ? [
              button(
                ~cls=l.mode == Window ? ["on"] : [],
                ~title=
                  Printf.sprintf(
                    "The model's window for request %d: exactly the messages its body carried",
                    l.seq,
                  ),
                "Exact window",
                go(LensMode(l.mode == Window ? Trajectory : Window)),
              ),
            ]
            : []
        )
        @ [
          button(
            ~title="The request before",
            ~enabled=l.seq > 1,
            {js|‹|js},
            agent(ViewAsAgent(l.chat, Some(l.seq - 1))),
          ),
          button(
            ~title="The request after",
            ~enabled=l.seq < l.count,
            {js|›|js},
            agent(ViewAsAgent(l.chat, Some(l.seq + 1))),
          ),
          button(
            ~cls=l.live ? ["on"] : [],
            ~title="Follow each new request",
            "Live",
            agent(ViewAsAgent(l.chat, l.live ? Some(l.seq) : None)),
          ),
          button(
            ~cls=["leave"],
            ~title="Back to your own view",
            "Leave",
            agent(LeaveAgentView),
          ),
        ],
      ),
    ],
  );
};

/* Stacked, nothing open: what stood in for cells */
let nothing_open: Node.t =
  div(
    ~attrs=[Attr.classes(["agent-lens-note"])],
    [text("Nothing was open in this request; the agent saw only the map.")],
  );

/* Stacked: the map the request sent, a read-only editor with the
   program's statics and run, its notes at line ends; folds away */
let map_panel =
    (
      ~globals: Globals.t,
      ~go: Workspace.Action.t => Effect.t(unit),
      ~signal: CellEditor.View.event => Effect.t(unit),
      ~selected: option(CellEditor.Selection.t),
      l: AgentLens.t,
    )
    : Node.t => {
  let result = Program.result(l.program);
  let head =
    button(
      ~attrs=[
        Attr.classes(["lens-map-head"]),
        Attr.title(l.map_shown ? "Fold the map" : "Show the map"),
        Attr.on_click(_ => go(LensMapShown(!l.map_shown))),
      ],
      [
        span(
          ~attrs=[Attr.classes(["lens-map-chevron"])],
          [text(l.map_shown ? {js|▾|js} : {js|▸|js})],
        ),
        span(
          ~attrs=[Attr.classes(["lens-map-title"])],
          [text("The map")],
        ),
        span(
          ~attrs=[Attr.classes(["lens-map-hint"])],
          [
            text(
              Printf.sprintf(
                {js|as request %d showed the program: bodies folded to ⋯|js},
                l.seq,
              ),
            ),
          ],
        ),
      ],
    );
  let notes = () => {
    let measured = l.map.editor.editor.editor.syntax.main_splice.measured;
    let fm = globals.font_metrics;
    AgentLensMap.comments(
      ~rows=measured.total_rows,
      id => Haz3lcore.Measured.find_by_id(id, measured),
      AgentLensMap.notes(~value=EvalResult.Model.get_value(result), l.map),
    )
    |> Array.to_list
    |> List.mapi((row, c) => (row, c))
    |> List.filter(((_, c)) => c != "")
    |> List.map(((row, c)) => {
         let col =
           switch (Haz3lcore.Measured.row_shape(row, measured)) {
           | Some(r) => r.content_end
           | None => 0
           };
         span(
           ~attrs=[
             Attr.classes(["lens-map-note"]),
             Attr.create(
               "style",
               Printf.sprintf(
                 "left: %fpx; top: %fpx; line-height: %fpx;",
                 float_of_int(col + 2) *. fm.col_width,
                 float_of_int(row) *. fm.row_height,
                 fm.row_height,
               ),
             ),
           ],
           [text(c)],
         );
       });
  };
  div(
    ~attrs=[Attr.classes(["agent-lens-map"])],
    [head]
    @ (
      l.map_shown
        ? [
          CellEditor.View.view(
            ~globals,
            ~signal,
            ~inject=a => go(LensMap(a)),
            ~selected,
            ~result_kind=`TestSigilsOnly,
            ~lines=true,
            ~master=(result, _ => Effect.Ignore),
            ~cull=false,
            ~overlays=[
              div(~attrs=[Attr.classes(["lens-map-notes"])], notes()),
            ],
            l.map.editor,
          ),
        ]
        : []
    ),
  );
};

let panel_head = (title: list(Node.t), actions: list(Node.t)): Node.t =>
  div(
    ~attrs=[Attr.classes(["lens-panel-head"])],
    [span(~attrs=[Attr.classes(["lens-panel-title"])], title)] @ actions,
  );

let sent = (l: AgentLens.t): Node.t =>
  div(
    ~attrs=[Attr.classes(["agent-lens-panel"])],
    [
      panel_head(
        [
          text(Printf.sprintf({js|The view request %d sent · |js}, l.seq)),
          size(l.sent),
        ],
        [copy_button(~title="Copy the view, exact bytes", l.sent)],
      ),
      create("pre", ~attrs=[Attr.classes(["lens-text"])], [text(l.sent)]),
    ],
  );

let kind_class = (k: Trajectory.kind): string =>
  "traj-k-"
  ++ (
    switch (k) {
    | Request => "request"
    | System => "system"
    | Tools => "tools"
    | Tool => "tool"
    | Developer => "developer"
    | User => "user"
    | Context => "context"
    | Round => "round"
    | Thinking => "thinking"
    | Assistant => "assistant"
    | Call => "call"
    | Result => "result"
    | Usage => "usage"
    | Note => "note"
    | Summary => "summary"
    }
  );

/* a section folds under its summary line: what it is, its size, its
   gist, and its raw toggle and copy, shown while open (copy also on
   hover) but always holding their place, so the line keeps its height.
   What was never sent has no JSON to show raw */
let rec section = (s: Trajectory.section): Node.t => {
  let controls =
    span(
      ~attrs=[Attr.classes(["traj-controls"])],
      s.sent
        ? [
          label(
            ~attrs=[
              Attr.classes(["traj-raw-label"]),
              Attr.title("Show this section exactly as sent"),
            ],
            [
              input(
                ~attrs=[
                  Attr.type_("checkbox"),
                  Attr.classes(["traj-raw-toggle"]),
                ],
                (),
              ),
              text("raw"),
            ],
          ),
          copy_button(
            ~cls=["traj-copy-text"],
            ~title="Copy exact bytes",
            s.text,
          ),
          copy_button(
            ~cls=["traj-copy-raw"],
            ~title="Copy this section's JSON, exact bytes",
            s.raw,
          ),
        ]
        : [
          copy_button(
            ~cls=["traj-copy-text"],
            ~title="Copy, as kept: never sent",
            s.text,
          ),
        ],
    );
  let pre = (cls, t) =>
    create("pre", ~attrs=[Attr.classes([cls])], [text(t)]);
  create(
    "details",
    ~attrs=
      [
        Attr.classes(
          ["traj-section", kind_class(s.kind)]
          @ (s.in_request ? [] : ["traj-elsewhere"])
          @ (s.sent ? [] : ["traj-unsent"]),
        ),
      ]
      @ (s.folded ? [] : [Attr.create("open", "")]),
    [
      create(
        "summary",
        ~attrs=[Attr.classes(["traj-summary"])],
        [
          span(~attrs=[Attr.classes(["traj-label"])], [text(s.label)]),
          size(~cls=["traj-size"], s.raw),
          span(~attrs=[Attr.classes(["traj-first"])], [text(s.brief)]),
          controls,
        ],
      ),
      div(
        ~attrs=[Attr.classes(["traj-body"])],
        (s.shown == "" ? [] : [pre("traj-shown", s.shown)])
        @ (s.sent ? [pre("traj-raw", s.raw)] : [])
        @ List.map(section, s.children),
      ),
    ],
  );
};

let trajectory = (l: AgentLens.t): Node.t =>
  div(
    ~attrs=[Attr.classes(["agent-lens-panel"])],
    switch (l.trajectory) {
    | Some(t) => [
        panel_head(
          [
            text(
              l.mode == Window
                ? Printf.sprintf(
                    {js|The model's window for request %d · |js},
                    t.seq,
                  )
                : Printf.sprintf(
                    {js|Sent over the API up to request %d · this request |js},
                    t.seq,
                  ),
            ),
            size(t.body),
          ],
          [
            copy_button(
              ~title="Copy every section's JSON, exact bytes, one per line",
              Trajectory.to_lines(t),
            ),
            copy_button(
              ~label="body",
              ~title="Copy this request's body, exact bytes",
              t.body,
            ),
          ],
        ),
        div(
          ~attrs=[Attr.classes(["lens-panel-note"])],
          [
            text(
              l.mode == Window
                ? "Exactly this request's body: its messages in order, its own context last."
                : "A request carries every message and only its own context. Thinking and usage come back with a reply and are never sent.",
            ),
          ],
        ),
        div(
          ~attrs=[Attr.classes(["traj-sections"])],
          List.map(section, t.sections),
        ),
      ]
    | None => [
        div(
          ~attrs=[Attr.classes(["agent-lens-note"])],
          [
            text(
              "This request was recorded before trajectories were kept, so what it sent can't be rebuilt.",
            ),
          ],
        ),
      ]
    },
  );
