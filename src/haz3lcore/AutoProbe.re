/* AUTO-PROBE MODE
 *
 * Controls how the editor automatically places probes. Threaded from the
 * web Settings down through Editor.calculate into ProbePerform.
 *
 *   Off   — no automatic probes (manual probes still work).
 *   Caret — probe the top-level definition the caret is currently inside,
 *           following it as the user navigates. Implemented as a multi
 *           probe anchored on that def's body (plus the parameter pattern
 *           for function-definition sugar).
 *   All   — probe the whole program at once: one probe per source row,
 *           covering every definition and sequence component. Implemented
 *           as a single multi probe anchored on the program root, letting
 *           MultiProbe's per-row selection do the work (the same machinery
 *           Caret uses, just with a whole-program anchor). See
 *           ProbePerform.update_autoprobe.
 */
[@deriving (show({with_path: false}), yojson, eq)]
type t =
  | Off
  | Caret
  | All;

let sexp_of_t = (mode: t): Sexplib.Sexp.t =>
  Sexplib.Sexp.Atom(
    switch (mode) {
    | Off => "Off"
    | Caret => "Caret"
    | All => "All"
    },
  );

/* Tolerant parser. This setting used to be a `bool`, so persisted settings
 * blobs may still carry `false`/`true`; map those to Off/Caret (the old
 * on-behavior). Anything unrecognized falls back to Off rather than
 * raising — Store.deserialize catches a raise by discarding ALL settings,
 * so a single stale field shouldn't wipe the user's other preferences. */
let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
  switch (sexp) {
  | Atom("All") => All
  | Atom("Caret" | "On" | "true") => Caret
  | Atom("Off" | "false")
  | _ => Off
  };
