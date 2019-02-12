open SemanticsCommon;
type splice_name = nat;
type splice_map('exp) = Util.NatMap.t((HTyp.t, 'exp));
type t('exp) = {next: splice_name, splice_map: splice_map('exp)};
let empty : t('exp) = {next: 0, splice_map: Util.NatMap.empty};

let splice_map = ({splice_map, _}) => splice_map;
let update_splice_map = ({next, _}, splice_map) => {next, splice_map};

let var_of_splice_name = (splice_name) => 
  "__hazel_splice_" ++ string_of_int(splice_name);
