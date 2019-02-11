open SemanticsCommon;
type splice_name = nat;
type splice_map('exp) = Util.NatMap.t((HTyp.t, 'exp));
type t('exp) = {next: splice_name, splice_map: splice_map('exp)};
let empty : t('exp) = {next: 0, splice_map: Util.NatMap.empty};
