open Virtual_dom.Vdom;
open Node;
open Util;

/* View as agent's banner and its Sent and Trajectory panels. On screen,
   text reads tidied (JSON indented, sections folded); every copy button
   gives the bytes as sent, and each section can show itself raw. */

let size = (s: string): string => {
  let n = Trajectory.chars(s);
  Printf.sprintf({js|%d chars · ~%d tokens|js}, n, (n + 3) / 4);
};

/* a section's first words, on one line */
let first_words = (s: string): string => {
  let words =
    String.split_on_char(' ', String.map(c => c == '\n' ? ' ' : c, s))
    |> List.filter(w => w != "");
  let line =
    String.concat(" ", ListUtil.take(min(12, List.length(words)), words));
  String.length(line) > 80 ? String.sub(line, 0, 80) ++ {js|…|js} : line;
};

let copy_button = (~cls=[], ~title, s: string): Node.t =>
  button(
    ~attrs=[
      Attr.classes(["lens-button", ...cls]),
      Attr.title(title),
      Attr.on_click(_ => {
        JsUtil.copy_text(s);
        JsUtil.show_copy_toast();
        Effect.Stop_propagation;
      }),
    ],
    [text("Copy")],
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
  let segment = (mode: AgentLens.mode, label, title) =>
    Node.button(
      ~attrs=[
        Attr.classes(["lens-segment", ...l.mode == mode ? ["active"] : []]),
        Attr.title(title),
        Attr.on_click(_ => go(LensMode(mode))),
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
    [
      text(
        "Nothing was open in this request; the agent saw only the map (the outline at left).",
      ),
    ],
  );

let panel_head = (label: string, actions: list(Node.t)): Node.t =>
  div(
    ~attrs=[Attr.classes(["lens-panel-head"])],
    [span(~attrs=[Attr.classes(["lens-panel-title"])], [text(label)])]
    @ actions,
  );

let sent = (l: AgentLens.t): Node.t =>
  div(
    ~attrs=[Attr.classes(["agent-lens-panel"])],
    [
      panel_head(
        Printf.sprintf(
          {js|The view request %d sent · %s|js},
          l.seq,
          size(l.sent),
        ),
        [copy_button(~title="Copy the view, exactly as sent", l.sent)],
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
    | Assistant => "assistant"
    | Call => "call"
    | Result => "result"
    | Summary => "summary"
    }
  );

/* a section folds under its summary line: what it is, its size, its
   first words, and (open) its raw toggle and copy */
let rec section = (s: Trajectory.section): Node.t => {
  let controls =
    span(
      ~attrs=[Attr.classes(["traj-controls"])],
      [
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
          ~title="Copy exactly as sent",
          s.text,
        ),
        copy_button(
          ~cls=["traj-copy-raw"],
          ~title="Copy this section's JSON, exactly as sent",
          s.raw,
        ),
      ],
    );
  let pre = (cls, t) =>
    create("pre", ~attrs=[Attr.classes([cls])], [text(t)]);
  create(
    "details",
    ~attrs=
      [Attr.classes(["traj-section", kind_class(s.kind)])]
      @ (s.folded ? [] : [Attr.create("open", "")]),
    [
      create(
        "summary",
        ~attrs=[Attr.classes(["traj-summary"])],
        [
          span(~attrs=[Attr.classes(["traj-label"])], [text(s.label)]),
          span(
            ~attrs=[Attr.classes(["traj-size"])],
            [text(size(s.raw))],
          ),
          span(
            ~attrs=[Attr.classes(["traj-first"])],
            [text(first_words(s.text))],
          ),
          controls,
        ],
      ),
      div(
        ~attrs=[Attr.classes(["traj-body"])],
        (s.shown == "" ? [] : [pre("traj-shown", s.shown)])
        @ [pre("traj-raw", s.raw)]
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
          Printf.sprintf(
            {js|Sent over the API up to request %d · this request %s|js},
            t.seq,
            size(t.body),
          ),
          [
            copy_button(
              ~title=
                "Copy every section's JSON, exactly as sent, one per line",
              Trajectory.to_lines(t),
            ),
            button(
              ~attrs=[
                Attr.classes(["lens-button"]),
                Attr.title("Copy this request's body, exactly as sent"),
                Attr.on_click(_ => {
                  JsUtil.copy_text(t.body);
                  JsUtil.show_copy_toast();
                  Effect.Ignore;
                }),
              ],
              [text("Copy body")],
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
