/* Projector kinds shared between Grammar.re and ProjectorCore.re.
 * This module exists to break the dependency cycle between
 * the language and haz3lcore libraries. */

/* The different kinds of projector. New projector
 * types need to be registered here in order to be
 * able to create and update their instances */
[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type t =
  | Fold
  | Probe
  | Statics
  | Checkbox
  | Slider
  | SliderF
  | Card
  | Livelit
  | TextArea
  | Table
  | Csv;

let livelit_projectors: list(t) = [
  Csv, /* Competes with Card for empty list */
  Card, /* Competes with Csv for empty list */
  Checkbox,
  Slider,
  SliderF,
  TextArea,
  Table,
  Card,
  Livelit,
];

/* Refractors are like probes - additive decorations, not syntax-replacing */
let refractors: list(t) = [Probe, Statics];
let is_refractor = (kind: t) => List.mem(kind, refractors);

/* A friendly name for each projector. This is used
 * both for identifying a projector in the CSS and for
 * selecting projectors in the projector panel menu */
let name = (p: t): string =>
  switch (p) {
  | Fold => "fold"
  | Probe => "probe"
  | Statics => "statics"
  | Checkbox => "check"
  | Slider => "slider"
  | SliderF => "sliderf"
  | Card => "card"
  | Livelit => "livelit"
  | TextArea => "text"
  | Table => "table"
  | Csv => "csv"
  };

/* Inverse of `name`, derived from it and the enumerated `all` (built once)
 * so the two cannot drift — a new kind needs only a case in `name` above,
 * which the compiler already requires since that match is exhaustive. */
let by_name: list((string, t)) = List.map(k => (name(k), k), all);

let of_name_opt = (p: string): option(t) => List.assoc_opt(p, by_name);

/* Partial: callers must already know the name is a kind. */
let of_name = (p: string): t =>
  switch (of_name_opt(p)) {
  | Some(k) => k
  | None => failwith("Unknown projector kind")
  };

let is_name = str => Option.is_some(of_name_opt(str));
