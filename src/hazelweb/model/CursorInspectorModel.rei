type t = {
  visible: bool,
  show_expanded: bool,
  novice_mode: bool,
  strategy_guide: bool,
  strategy_guide_lit: bool,
  strategy_guide_var: bool,
  strategy_guide_fun: bool,
  strategy_guide_branch: bool,
  strategy_guide_new_var: bool,
  strategy_guide_other: bool,
};

let init: t;

[@deriving sexp]
type update =
  | Toggle_visible
  | Toggle_show_expanded
  | Toggle_novice_mode
  | Toggle_strategy_guide
  | Toggle_strategy_guide_lit
  | Toggle_strategy_guide_var
  | Toggle_strategy_guide_fun
  | Toggle_strategy_guide_branch
  | Toggle_strategy_guide_new_var
  | Toggle_strategy_guide_other;

let apply_update: (update, t) => t;
