open Util;

// TODO(d) clean up this module, overdetermined wrt ids

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Touch(Id.t)
  | Delete(Id.t)
  | Remold(Id.t, Mold.t, Mold.t); // from, to

// used to record effects over the course of a single action
let s: ref(Id.Map.t(list(t))) = ref(Id.Map.empty);
let s_clear = () => s := Id.Map.empty;
let s_add = (id, effs) =>
  s :=
    s^
    |> Id.Map.update(
         id,
         fun
         | None => Some(effs)
         | Some(effs') => Some(effs @ effs'),
       );
let s_touch = (ids: list(Id.t)) =>
  ids |> List.iter(id => s_add(id, [Touch(id)]));
let s_remold = (id: Id.t, mold, mold') =>
  Mold.eq(mold, mold') ? () : s_add(id, [Remold(id, mold, mold')]);

let s_touched = (id: Id.t): bool =>
  switch (Id.Map.find_opt(id, s^)) {
  | None => false
  | Some(effs) => List.mem(Touch(id), effs)
  };
// returns original mold
let s_remolded = (id: Id.t): option(Mold.t) =>
  switch (Id.Map.find_opt(id, s^)) {
  | None => None
  | Some(effs) =>
    effs
    |> List.filter_map(
         fun
         | Remold(_, m, _) => Some(m)
         | _ => None,
       )
    |> ListUtil.hd_opt
  };
