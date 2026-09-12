/* Blackboard editor terms with ids at every node. */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = BbGrammar.t(IdTagged.IdTag.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type term = BbGrammar.term(IdTagged.IdTag.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type hole = BbGrammar.hole(IdTagged.IdTag.t);
