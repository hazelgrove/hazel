/* TODO: Ardi - Example for storing info of user selections */

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

let init = {
  visible: false,
  show_expanded: false,
  option_1: 0,
  option_2: 0,
  option_3: 0,
  option_4: 0,
  option_5: 0,
  option_6: 0,
};

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

// TODO - change hard-coded 99s
let apply_update = (u: update, settings: t) =>
  switch (u) {
  | Toggle_visible => {...settings, visible: !settings.visible}
  | Toggle_show_expanded => {
      ...settings,
      show_expanded: !settings.show_expanded,
    }
  | Toggle_option_1 => {...settings, option_1: 99}
  | Toggle_option_2 => {...settings, option_2: 99}
  | Toggle_option_3 => {...settings, option_3: 99}
  | Toggle_option_4 => {...settings, option_4: 99}
  | Toggle_option_5 => {...settings, option_5: 99}
  | Toggle_option_6 => {...settings, option_6: 99}
  };
