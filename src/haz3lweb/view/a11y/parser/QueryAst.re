open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type count = option(int);

[@deriving (show({with_path: false}), sexp, yojson)]
type textobject_key =
  | Term // w
  | Parenthesis; // ()

[@deriving (show({with_path: false}), sexp, yojson)]
type action_key =
  | Move; // m

[@deriving (show({with_path: false}), sexp, yojson)]
type query_decoration =
  | Definition; // d

[@deriving (show({with_path: false}), sexp, yojson)]
type query_key =
  | Type // t
  | Read; // r

[@deriving (show({with_path: false}), sexp, yojson)]
type query_op = (option(query_decoration), query_key);

[@deriving (show({with_path: false}), sexp, yojson)]
type query = (textobject, query_op)
and textobject =
  | Inner(textobject_key) // i
  | Query(query); // q

[@deriving (show({with_path: false}), sexp, yojson)]
type action = (textobject, action_key);

[@deriving (show({with_path: false}), sexp, yojson)]
type command =
  | Action(action)
  | Query(query);
