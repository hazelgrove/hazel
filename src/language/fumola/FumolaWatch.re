/* The watch panel for one Fumola instance -- its events, nodes and edges --
   for a livelit to draw beside its code.

   The panel is rendered by the web layer (FumolaSidebar), which this library
   cannot depend on, so the page installs it here on every frame, as it does
   the app bridge; a livelit rendering outside the page (the test runner)
   finds nothing installed and draws nothing. */
let instance_view: ref(string => option(Virtual_dom.Vdom.Node.t)) =
  ref(_ => None);
