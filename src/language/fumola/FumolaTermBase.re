/* Fumola editor terms with ids at every node, and with Hazel expressions as
   the host terms `hazel … end` can embed. */

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  FumolaGrammar.exp(Grammar.exp_t(IdTagged.IdTag.t), IdTagged.IdTag.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type exp_term =
  FumolaGrammar.exp_term(Grammar.exp_t(IdTagged.IdTag.t), IdTagged.IdTag.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type dec =
  FumolaGrammar.dec(Grammar.exp_t(IdTagged.IdTag.t), IdTagged.IdTag.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type pat =
  FumolaGrammar.pat(Grammar.exp_t(IdTagged.IdTag.t), IdTagged.IdTag.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type hole =
  FumolaGrammar.hole(Grammar.exp_t(IdTagged.IdTag.t), IdTagged.IdTag.t);
