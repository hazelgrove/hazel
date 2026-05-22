/* ListRenderer - Visualize a list value as a numbered, vertical list. */
[@deriving (show({with_path: false}), sexp, yojson)]
type m = {
  selected: option(int),
  menu_state: Util.Menu.t,
};
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | Select(option(int))
  | ToggleMenu
  | CloseMenu
  | MenuAction(Util.Menu.action);
type v = list(Language.Exp.t);

include
  RichProbe.RichProbe with
    type model = m and type action = a and type value = v;
