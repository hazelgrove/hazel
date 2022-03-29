/* TODO: Ardi - Example for storing info of user selections */

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

let init = {
  visible: false,
  show_expanded: false,
  novice_mode: true,
  strategy_guide: false,
  strategy_guide_lit: false,
  strategy_guide_var: false,
  strategy_guide_fun: false,
  strategy_guide_branch: false,
  strategy_guide_new_var: false,
  strategy_guide_other: false,
};

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

let apply_update = (u: update, settings: t) =>
  switch (u) {
  | Toggle_visible => {...settings, visible: !settings.visible}
  | Toggle_show_expanded => {
      ...settings,
      show_expanded: !settings.show_expanded,
    }
  | Toggle_novice_mode => {...settings, novice_mode: !settings.novice_mode}
  | Toggle_strategy_guide => {
      ...settings,
      strategy_guide: !settings.strategy_guide,
    }
  | Toggle_strategy_guide_lit => {
      ...settings,
      strategy_guide_lit: !settings.strategy_guide_lit,
    }
  | Toggle_strategy_guide_var => {
      ...settings,
      strategy_guide_var: !settings.strategy_guide_var,
    }
  | Toggle_strategy_guide_fun => {
      ...settings,
      strategy_guide_fun: !settings.strategy_guide_fun,
    }
  | Toggle_strategy_guide_branch => {
      ...settings,
      strategy_guide_branch: !settings.strategy_guide_branch,
    }
  | Toggle_strategy_guide_new_var => {
      ...settings,
      strategy_guide_new_var: !settings.strategy_guide_new_var,
    }
  | Toggle_strategy_guide_other => {
      ...settings,
      strategy_guide_other: !settings.strategy_guide_other,
    }
  };
