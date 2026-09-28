open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open ProbeControls;

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
    toggles == []
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
