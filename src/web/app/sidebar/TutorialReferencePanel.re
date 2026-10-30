open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open ProbeControls;

/* The panel may contain reference prose, probe controls, or both. Its
 * availability is independent of whether the lesson has Markdown. */
type context = {
  reference: option(string),
  probe_config: option(TutorialProbeConfig.t),
};

let of_lesson = (lesson: Tutorial.p('a)): option(context) => {
  let config = TutorialProbeConfig.of_slide(lesson.module_name);
  let probe_config: option(TutorialProbeConfig.t) =
    List.is_empty(config.flags) ? None : Some(config);
  switch (lesson.task_reference, probe_config) {
  | (None, None) => None
  | (reference, probe_config) =>
    Some({
      reference,
      probe_config,
    })
  };
};

/* Progressive controls and console switch in the tutorial reference panel. */
/* The strip body: toggles panel + quick reference + (optional) color legend.
 * Does NOT include the console switch/body (handled separately so the panel
 * can swap its whole content into the print console). */
let strip_view =
    (
      ~globals: Globals.t,
      ~explain_this_inject,
      ~config: TutorialProbeConfig.t,
    )
    : list(Node.t) => {
  let {TutorialProbeConfig.flags, new_flags, initial: _} = config;
  let toggles =
    (
      mem(flags, AutoProbe)
        ? [auto_probe_toggle(~globals, ~is_new=mem(new_flags, AutoProbe))]
        : []
    )
    @ (
      mem(flags, SamplesToggle)
        ? [
          samples_toggle(
            ~explain_this_inject,
            ~is_new=mem(new_flags, SamplesToggle),
          ),
        ]
        : []
    );
  let toggle_panel =
    List.is_empty(toggles)
      ? [] : [div(~attrs=[clss(["toggle-controls", "panel"])], toggles)];
  let legend =
    mem(flags, Legend)
      ? [ProbeSidebar.legend_view(~globals, ~explain_this_inject)] : [];
  toggle_panel
  @ quick_ref_panel(
      /* Tutorial references stay readable regardless of the indicated term. */
      ~context_clss=["can-probe", "has-probe", "has-manual"],
      ~new_flags,
      flags,
    )
  @ legend;
};

/* "Console": a Reference / Console mode switch at the top of the
 * panel. Console mode replaces the whole panel body with the print console.
 * State is a module-level ref (matching ProbeSidebar's own ref-based mode
 * switches); explain_this_inject pokes a re-render. */

let console_mode = ref(false); /* false = Reference, true = Console */

let console_header = (~explain_this_inject): Node.t => {
  let switch_to = (target, _) => {
    console_mode := target;
    explain_this_inject(ExplainThisUpdate.SpecificityOpen(true));
  };
  let is_ref = ! console_mode^;
  div(
    ~attrs=[clss(["main-title"])],
    [
      span(
        ~attrs=
          [clss(["mode-label"] @ (is_ref ? ["active"] : ["inactive"]))]
          @ (is_ref ? [] : [Attr.on_pointerdown(switch_to(false))]),
        [text("Reference")],
      ),
      span(~attrs=[clss(["mode-separator"])], [text(" / ")]),
      span(
        ~attrs=
          [clss(["mode-label"] @ (is_ref ? ["inactive"] : ["active"]))]
          @ (is_ref ? [Attr.on_pointerdown(switch_to(true))] : []),
        [text("Console")],
      ),
    ],
  );
};

let view =
    (
      ~globals: Globals.t,
      ~explain_this_inject,
      ~editor: CodeWithStatics.Model.t,
      context: context,
    ) => {
  let render_md = blocks => {
    let (nodes, _) =
      ExplainThis.mk_translation_doc(~globals, ~inject=_ => (), blocks);
    nodes;
  };
  let sections =
    switch (context.reference) {
    | Some(body) => TaskReferenceSplit.split(Omd.of_string(body))
    | None => []
    };
  let section_nodes =
    List.map(
      ~f=
        ((heading: option(string), content)) =>
          switch (heading) {
          | None =>
            div(
              ~attrs=[clss(["task-reference-preamble"])],
              render_md(content),
            )
          | Some(h) =>
            Node.details(
              ~attrs=[
                clss(["task-reference-section"]),
                Attr.create("open", ""),
              ],
              [
                Node.summary(
                  ~attrs=[clss(["task-reference-section-title"])],
                  [text(h)],
                ),
                div(
                  ~attrs=[clss(["task-reference-section-body"])],
                  render_md(content),
                ),
              ],
            )
          },
      sections,
    );
  let body_div = div(~attrs=[clss(["task-reference-body"])], section_nodes);
  let console_on =
    switch (context.probe_config) {
    | Some(config) => mem(config.flags, Console)
    | None => false
    };
  let strip = () =>
    switch (context.probe_config) {
    | Some(config) => strip_view(~globals, ~explain_this_inject, ~config)
    | None => []
    };
  /* When the print console is introduced, the panel header becomes a
   * Reference / Console switch and Console mode swaps the whole body for the
   * print console. Otherwise the strip (when nonempty) sits between the
   * "Task Reference" header and the markdown body. The strip and console live
   * inside a #probe-sidebar wrapper so they inherit the probe panel's styling;
   * .task-reference-panel remains the ancestor so markdown stays styled. */
  if (console_on) {
    let inner =
      console_mode^
        ? ProbeSidebar.printarium_body(~explain_this_inject, ~editor)
        : strip() @ [body_div];
    div(
      ~attrs=[clss(["task-reference-panel"])],
      [
        div(
          ~attrs=[Attr.id("probe-sidebar"), clss(["tutorial-probe-strip"])],
          [console_header(~explain_this_inject), ...inner],
        ),
      ],
    );
  } else {
    let strip = strip();
    let strip_div =
      List.is_empty(strip)
        ? []
        : [
          div(
            ~attrs=[
              Attr.id("probe-sidebar"),
              clss(["tutorial-probe-strip"]),
            ],
            strip,
          ),
        ];
    div(
      ~attrs=[clss(["task-reference-panel"])],
      [
        div(
          ~attrs=[clss(["task-reference-header"])],
          [
            div(
              ~attrs=[clss(["task-reference-title"])],
              [text("Task Reference")],
            ),
          ],
        ),
        ...strip_div @ [body_div],
      ],
    );
  };
};
