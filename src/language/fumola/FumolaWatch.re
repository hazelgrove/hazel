/* The watch panel for one Fumola instance -- its events, nodes and edges --
   for a livelit to draw beside its code.

   The panel is rendered by the web layer (FumolaSidebar), which this library
   cannot depend on, so the page installs it here on every frame, as it does
   the app bridge; a livelit rendering outside the page (the test runner)
   finds nothing installed and draws nothing. */
/* A livelit's own panes, drawn as tabs beside Events, Nodes and Edges
   rather than stacked above them: the pane's rows are fixed, and stacked
   they pushed the watch panel out of view. */
type panes = {
  program: Virtual_dom.Vdom.Node.t,
  outline: Virtual_dom.Vdom.Node.t,
  printed: Virtual_dom.Vdom.Node.t,
};

let instance_view:
  ref((string, option(panes)) => option(Virtual_dom.Vdom.Node.t)) =
  ref((_, _) => None);
