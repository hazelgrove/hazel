type t = {
  visible: bool,
  show_expanded: bool,
  option_1: int,
  option_2: int,
  option_3: int,
  option_4: int,
  option_5: int,
  option_6: int,
};

let init: t;

[@deriving sexp]
type update =
  | Toggle_visible
  | Toggle_show_expanded
  | Toggle_option_1
  | Toggle_option_2
  | Toggle_option_3
  | Toggle_option_4
  | Toggle_option_5
  | Toggle_option_6;

let apply_update: (update, t) => t;
