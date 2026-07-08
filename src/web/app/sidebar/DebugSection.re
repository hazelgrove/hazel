/* Interface for a debug sidebar section: a stable `title` (also its collapse
   key) and a `view` producing the section body. DebugSidebar wraps the body in
   its collapsible `section` driver, so `view` returns just the fields. Modeled
   on the STEPPER signature in editors/stepper/StepInterface.re. */
module type S = {
  let title: string;
  let view: (~globals: Globals.t) => list(Util.WebUtil.Node.t);
};
