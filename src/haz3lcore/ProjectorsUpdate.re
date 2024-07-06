open Sexplib.Std;

/* The kind of syntax data to which projection can apply */
[@deriving (show({with_path: false}), sexp, yojson)]
type syntax = Piece.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Remove
  | FocusInternal(string)
  | Default // Defer input to focal DOM element
  | Escape(string, Util.Direction.t)
  | UpdateSyntax(syntax => syntax)
  | UpdateModel(string);
