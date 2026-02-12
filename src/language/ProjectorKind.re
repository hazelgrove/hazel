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
  Card,
  Livelit,
  Table,
];

/* Note: Probe intentionally excluded - probes use separate action path */
let projectors: list(t) = livelit_projectors @ [Fold];

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
  | Csv => "csv"
  | Table => "table"
  };

module StringMap = Map.Make(String);
let name_to_kind: StringMap.t(t) =
  List.fold_left(
    (map, kind) => StringMap.add(name(kind), kind, map),
    StringMap.empty,
    all,
  );

let of_name = (p: string): t =>
  switch (StringMap.find_opt(p, name_to_kind)) {
  | Some(k) => k
  | None => failwith("Unknown projector kind")
  };

let is_name = StringMap.mem(_, name_to_kind);
