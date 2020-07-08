/**
 * Map keyed by splices of a particular livelit application.
 * TODO: unify with SpliceInfo.splice_map.
 */
module ApMap = Map.Make(SpliceName);

/**
 * A generic map keyed by every splice in a program.
 */
type t('v) = MetaVarMap.t(ApMap.t('v));

let empty = MetaVarMap.empty;
let singleton = MetaVarMap.singleton;
let map = MetaVarMap.map;
let union = MetaVarMap.union;

let get_ap = (u: MetaVar.t, map: t('v)): ApMap.t('v) =>
  switch (MetaVarMap.find_opt(u, map)) {
  | None => raise(Not_found)
  | Some(ap_map) => ap_map
  };
let put_ap = (u: MetaVar.t, ap_map: ApMap.t('v), map: t('v)): t('v) =>
  MetaVarMap.add(u, ap_map, map);

let get_splice = (u: MetaVar.t, splice_name: SpliceName.t, map: t('v)): 'v =>
  MetaVarMap.find_opt(u, map)
  |> OptUtil.get(() => raise(Not_found))
  |> ApMap.find(splice_name);
let put_splice =
    (u: MetaVar.t, splice_name: SpliceName.t, v: 'v, map: t('v)): t('v) => {
  let new_ap_map = map |> get_ap(u) |> ApMap.add(splice_name, v);
  map |> put_ap(u, new_ap_map);
};
