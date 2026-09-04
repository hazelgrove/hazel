/* Interface for a debug sidebar section: a stable `title` (also its collapse
   key) and a `view` producing the section body. DebugSidebar wraps the body in
   its collapsible `section` driver, so `view` returns just the fields. */
module type S = {
  let title: string;
  let view: (~globals: Globals.t) => list(Util_web.WebUtil.Node.t);
};
