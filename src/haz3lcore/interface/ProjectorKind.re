/* The different kinds of projector. New projector
 * types need to be registered here in order to be
 * able to create and update their instances */
[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type t =
  | Fold
  | Type
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
  SliderF,
  TextArea,
  Card,
];

let projectors: list(t) =
  livelit_projectors @ [Fold, Type, Livelit, Probe, Pair];

/* A friendly name for each projector. This is used
 * both for identifying a projector in the CSS and for
 * selecting projectors in the projector panel menu */
let name = (p: t): string => p |> sexp_of_t |> Sexplib.Sexp.to_string;

/* This must be updated and kept 1-to-1 with the above
 * name function in order to be able to select the
 * projector in the projector panel menu */
let of_name = (p: string): t => p |> Sexplib.Sexp.of_string |> t_of_sexp;

//TODO(andrew): might have to adjust name casing or adjust names in existing backup_texts
let is_name = str => List.mem(str, List.map(name, all));

/* Keep this in sync with Keyboard.re */
let shortcut_of = (kind: t): option(string) =>
  switch (kind) {
  | Fold => Some("Option-f")
  | Type => Some("Option-t")
  | Probe => Some("Option-v")
  | Checkbox
  | Slider
  | SliderF
  | Card
  | TextArea => Some("Option-l")
  | Livelit
  | Pair => None
  };
