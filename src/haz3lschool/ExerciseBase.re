open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type key = (string, int);

module TermItem = {
  type t = {
    term: TermBase.UExp.t,
    term_ranges: TermRanges.t,
  };
};

module StaticsItem = {
  type t = CachedStatics.statics;
};

module DynamicsItem = {
  type t = {
    term: TermBase.UExp.t,
    info_map: Statics.Map.t,
    result: ModelResult.t,
  };
  let empty: t = {
    term: {
      term: Tuple([]),
      ids: [Id.mk()],
    },
    info_map: Id.Map.empty,
    result: NoElab,
  };
  let statics_only = ({term, info_map, _}: StaticsItem.t): t => {
    {term, info_map, result: NoElab};
  };
};
