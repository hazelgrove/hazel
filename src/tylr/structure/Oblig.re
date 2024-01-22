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

  let minimize =
      (~to_zero=false, f: 'x => option('y), xs: list('x)): option('y) => {
    open OptUtil.Syntax;
    let* (y, effs, delta) =
      xs
      |> List.map(Effects.record(f))
      |> List.filter_map(((y_opt, effs)) =>
           y_opt |> Option.map(y => (y, effs, of_effects(effs)))
         )
      |> ListUtil.min(((_, _, l), (_, _, r)) => compare(l, r))
      |> ListUtil.hd_opt;
    if (!to_zero || delta == zero) {
      Effects.commit(effs);
      Some(y);
    } else {
      None;
    };
  };
};
