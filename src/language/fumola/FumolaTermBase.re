/* Fumola editor terms with ids at every node. */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = FumolaGrammar.exp(IdTagged.IdTag.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type exp_term = FumolaGrammar.exp_term(IdTagged.IdTag.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type dec = FumolaGrammar.dec(IdTagged.IdTag.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type pat = FumolaGrammar.pat(IdTagged.IdTag.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type hole = FumolaGrammar.hole(IdTagged.IdTag.t);
