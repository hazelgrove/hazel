open Sexplib.Std;

[@deriving sexp]
type splice_map('exp) = IntMap.t((HTyp.t, 'exp));
[@deriving sexp]
type t('exp) = {
  next: SpliceName.t,
  splice_map: splice_map('exp),
  splice_order: list(SpliceName.t),
};
let empty: t('exp) = {next: 0, splice_map: IntMap.empty, splice_order: []};

let splice_map = ({splice_map, _}) => splice_map;
let update_splice_map = ({next, splice_order, _}, splice_map) => {
  next,
  splice_map,
  splice_order,
};

let splice_var_prefix = "__hazel_splice_";
let var_of_splice_name = splice_name =>
  splice_var_prefix ++ string_of_int(splice_name);
