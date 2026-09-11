open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

/**
  The sort of Blackboard terms.

  Blackboard's core logic has one syntactic class, and for now the editor
  uses one sort for everything Blackboard: terms, the `x : T` entries of a
  signature, and the `assume`/`construct` blocks of a document. Following
  DrvSort, which does the same for judgments, contexts and propositions
  because of a remolding issue, the distinctions are made by form label in
  MakeTerm rather than by sort.
 */

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type t =
  | Term;

let class_of =
  fun
  | Term => "Bb";

let to_string =
  fun
  | Term => "BbTerm";

let to_string_short =
  fun
  | Term => "Blackboard";
