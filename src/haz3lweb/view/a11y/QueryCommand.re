// open Sexplib.Std;

/* QueryCommand.re

      This module defines the query commands and query plan format.

   */

[@deriving (show({with_path: false}), sexp, yojson)]
type selection =
  | Term
  | Match;

[@deriving (show({with_path: false}), sexp, yojson)]
type query =
  | Type
  | Definition
  | DoNothing;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Select(selection)
  | Query(query);
