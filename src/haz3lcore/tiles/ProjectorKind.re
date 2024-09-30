/* The different kinds of projector. New projectors
 * types need to be registered here in order to be
 * able to create and update their instances */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Fold
  | Info
  | Checkbox
  | Slider
  | SliderF
  | TextArea;
