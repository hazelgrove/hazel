open Util;
/* TableRenderer - A reusable module for rendering interactive tables with column operations */
[@deriving (show({with_path: false}), sexp, yojson)]
type menu_state = option((int, list(string)));
[@deriving (show({with_path: false}), sexp, yojson)]
type m = {menu_state};
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | CloseMenu
  | ShowMenu(int)
  | ShowSubmenu(list(string))
  | DropColumn(string)
  | ConversionColumn(string, string)
  | RenameColumn(string, string)
  | AddColumnAfter(string, string);

include RichProbe.RichProbe with type model = m and type action = a;
