/* The debug sidebar's only entry point: Sidebar renders this. Every field
   builder and section driver below it is private. */

let view:
  (~globals: Globals.t, ~cursor: Cursor.cursor('a)) => Virtual_dom.Vdom.Node.t;
