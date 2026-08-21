/* Auto-probe mode (threaded from Settings through Editor.calculate into
 * ProbePerform):
 *   Off   — no automatic probes (manual probes still work)
 *   Caret — probe the top-level definition the caret is inside
 *   All   — probe the whole program, one probe per source row */
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

/* Tolerant: this setting was once a bool, so old blobs carry false/true (map
 * to Off/Caret). Unknown -> Off rather than raise (a raise makes Store discard
 * ALL settings). */
let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
  switch (sexp) {
  | Atom("All") => All
  | Atom("Caret" | "On" | "true") => Caret
  | Atom("Off" | "false")
  | _ => Off
  };
