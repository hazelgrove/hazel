open Sexplib.Std;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  strategy_guide: bool,
  strategy_guide_lit: bool,
  strategy_guide_var: bool,
  strategy_guide_fun: bool,
  strategy_guide_branch: bool,
  strategy_guide_new_var: bool,
  strategy_guide_other: bool,
};

let init = {
  strategy_guide: false,
  strategy_guide_lit: false,
  strategy_guide_var: false,
  strategy_guide_fun: false,
  strategy_guide_branch: false,
  strategy_guide_new_var: false,
  strategy_guide_other: false,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type update =
  | Toggle_strategy_guide
  | Toggle_strategy_guide_lit
  | Toggle_strategy_guide_var
  | Toggle_strategy_guide_fun
  | Toggle_strategy_guide_branch
  | Toggle_strategy_guide_new_var
  | Toggle_strategy_guide_other;

let apply_update = (u: update, settings: t) =>
  switch (u) {
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
