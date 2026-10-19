/* The sidebar panel listing a mode's slides as a tree of their SlidePath
 * folders. Clicking a slide switches to it; clicking a folder opens or closes
 * it, and which folders are closed persists in the sidebar settings. */

type context = {
  /* The shown slide's position in `paths`. */
  current: int,
  paths: list(SlidePath.t),
  /* Switch to the slide at a position in `paths`. */
  select: int => Virtual_dom.Vdom.Effect.t(unit),
};

let view: (~globals: Globals.t, context) => Virtual_dom.Vdom.Node.t;
