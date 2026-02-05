/* Projector kinds shared between Grammar.re and ProjectorCore.re.
 * This module exists to break the dependency cycle between
 * the language and haz3lcore libraries. */

/* The different kinds of projector. New projector
 * types need to be registered here in order to be
 * able to create and update their instances */
/* The different kinds of projector. New projector
 * types need to be registered here in order to be
 * able to create and update their instances */
[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type t =
  | Fold
  | Probe
  | Statics
  | Player
  | Checkbox
  | Slider
  | SliderF
  | Knob
  | Card
  | Livelit
  | TextArea
  | Csv
  | NotePicker
  | RhythmGrid
  | XYPad
  | SamplePicker
  | ScalePicker;

let livelit_projectors: list(t) = [
  Checkbox,
  Slider,
  SliderF,
  Knob,
  TextArea,
  Csv, /* Competes with Card for empty list */
  Card, /* Competes with Csv for empty list */
  Livelit,
  NotePicker,
  RhythmGrid,
  XYPad,
  SamplePicker,
  ScalePicker,
];

/* Note: Probe intentionally excluded - probes use separate action path */
let projectors: list(t) = livelit_projectors @ [Fold];

/* Refractors are like probes - additive decorations, not syntax-replacing */
let refractors: list(t) = [Probe, Statics, Player];
let is_refractor = (kind: t) => List.mem(kind, refractors);

/* A friendly name for each projector. This is used
 * both for identifying a projector in the CSS and for
 * selecting projectors in the projector panel menu */
let name = (p: t): string =>
  switch (p) {
  | Fold => "fold"
  | Probe => "probe"
  | Statics => "statics"
  | Player => "player"
  | Checkbox => "check"
  | Slider => "slider"
  | SliderF => "sliderf"
  | Knob => "knob"
  | Card => "card"
  | Livelit => "livelit"
  | TextArea => "text"
  | Csv => "csv"
  | NotePicker => "notes"
  | RhythmGrid => "rhythm"
  | XYPad => "xypad"
  | SamplePicker => "samplepicker"
  | ScalePicker => "scalepicker"
  };

/* This must be updated and kept 1-to-1 with the above
 * name function in order to be able to select the
 * projector in the projector panel menu */
let of_name = (p: string): t =>
  switch (p) {
  | "fold" => Fold
  | "probe" => Probe
  | "statics" => Statics
  | "player" => Player
  | "check" => Checkbox
  | "slider" => Slider
  | "sliderf" => SliderF
  | "knob" => Knob
  | "text" => TextArea
  | "livelit" => Livelit
  | "card" => Card
  | "csv" => Csv
  | "notes" => NotePicker
  | "rhythm" => RhythmGrid
  | "xypad" => XYPad
  | "samplepicker" => SamplePicker
  | "scalepicker" => ScalePicker
  | _ => failwith("Unknown projector kind")
  };

let is_name = str => List.mem(str, List.map(name, all));
