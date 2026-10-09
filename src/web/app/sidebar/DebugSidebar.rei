/* The "Debug — Cursor Info" sidebar panel: what statics knows about the term
   at the cursor, its syntax, and the worker messaging metrics (which render
   whether or not there is a cursor). Rendered by Sidebar.

   `'update` is the host's action type, carried by `Cursor.cursor` but never
   read here -- this panel only displays, so it fits any host. */

let view:
  (~globals: Globals.t, ~cursor: Cursor.cursor('update)) =>
  Virtual_dom.Vdom.Node.t;
