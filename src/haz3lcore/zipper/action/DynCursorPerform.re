open Language;

/* Utility functions for updating dynamic cursor state in the zipper.
 * These are used by both ProbePerform and Refractors. */

let update_dyn_cursor = (z: Zipper.t, f: DynCursor.t => DynCursor.t): Zipper.t =>
  Zipper.update_refractors(z, refractors =>
    {
      ...refractors,
      dyn_cursor: f(refractors.dyn_cursor),
    }
  );

let update_pinned_call =
    (z: Zipper.t, f: option(Probe.call_stack) => option(Probe.call_stack))
    : Zipper.t =>
  update_dyn_cursor(z, dyn_cursor =>
    {
      ...dyn_cursor,
      pinned_stack: f(dyn_cursor.pinned_stack),
    }
  );
