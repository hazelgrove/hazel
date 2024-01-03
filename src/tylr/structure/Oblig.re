[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Missing_meld // convex grout
  | Missing_tile // ghost tile
  | Incon_meld // pre/postfix grout
  | Extra_meld; // infix grout

// low to high severity
let all = [Missing_meld, Missing_tile, Incon_meld, Extra_meld];
let severity = o => Option.get(List.find_index((==)(o), all));

module Ord = {
  type nonrec t = t;
  let compare = (l, r) => Int.compare(severity(l), severity(r));
};
module Map = Map.Make(Ord);

module Delta = {
  include Map;
  type t = Map.t(int);
  let find = (o, map) => Option.value(find_opt(o, map), ~default=0);

  let compare = (l, r) =>
    List.fold_right(
      (o, c) => c != 0 ? c : Int.compare(find(o, l), find(o, r)),
      all,
      0,
    );

  let add_effect = (R(eff): Effects.recorded) =>
    switch (eff) {
    | Insert(t) =>
      switch (of_token(t)) {
      | None => Fun.id
      | Some(o) => incr(o)
      }
    | Remove(t) =>
      switch (of_token(t)) {
      | None => Fun.id
      | Some(o) => decr(o)
      }
    | _ => Fun.id
    };
  let of_effects = List.fold_left(Fun.flip(add_effect), zero);

  let minimize = (f, xs) => {
    open OptUtil.Syntax;
    let+ (y, effs, _) =
      xs
      |> List.map(Effects.record(f))
      |> List.map(((y, effs)) => (y, effs, of_effects(effs)))
      |> List.sort(((_, _, l), (_, _, r)) => compare(l, r))
      |> ListUtil.hd_opt;
    Effects.commit(effs);
    y;
  };
};
