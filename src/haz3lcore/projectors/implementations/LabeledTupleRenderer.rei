/* LabeledTupleRenderer - View a labeled tuple as a key/value card with
 * per-field actions (Extract, Drop, Rename). */
[@deriving (show({with_path: false}), sexp, yojson)]
type menu_state = option((string, int));
[@deriving (show({with_path: false}), sexp, yojson)]
type m = {menu_state};
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | CloseMenu
  | ShowMenu(string)
  | MenuSelect(int);
type v = list((option(string), Language.Exp.t));

include
  RichProbe.RichProbe with
    type model = m and type action = a and type value = v;
