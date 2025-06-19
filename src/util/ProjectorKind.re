/* The different kinds of projector. New projector
 * types need to be registered here in order to be
 * able to create and update their instances */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Fold
  | Info
  | Probe
  | Pair
  | Checkbox
  | Slider
  | SliderF
  | Card
  | Livelit
  | TextArea;

/* Projectors that apply to categories of values
 * which are syntactically mutually exclusive */
let livelit_projectors: list(t) = [
  Checkbox,
  Slider,
  Pair,
  SliderF,
  TextArea,
  Card,
];

let projectors: list(t) = livelit_projectors @ [Fold, Info, Livelit, Probe];

/* A friendly name for each projector. This is used
 * both for identifying a projector in the CSS and for
 * selecting projectors in the projector panel menu */
let name = (p: t): string => p |> sexp_of_t |> Sexplib.Sexp.to_string;

/* This must be updated and kept 1-to-1 with the above
 * name function in order to be able to select the
 * projector in the projector panel menu */
let of_name = (p: string): t => p |> Sexplib.Sexp.of_string |> t_of_sexp;

/* Keep this in sync with Keyboard.re */
let shortcut_of = (kind: t): option(string) =>
  switch (kind) {
  | Fold => Some("Option-f")
  | Info => Some("Option-t")
  | Probe => Some("Option-v")
  | Checkbox
  | Slider
  | SliderF
  | Card
  | TextArea => Some("Option-l")
  | Livelit
  | Pair => None
  };
