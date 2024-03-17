open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Missing_meld // convex grout
  | Missing_tile // ghost tile
  | Incon_meld // pre/postfix grout
  | Extra_meld; // infix grout

// low to high severity
let all = [Missing_meld, Missing_tile, Incon_meld, Extra_meld];
let severity = o => Option.get(ListUtil.find_index((==)(o), all));

let of_token = (tok: Token.t) =>
  switch (tok.mtrl) {
  | Space => None
  | Grout =>
    let null_l = Mold.nullable(~side=L, tok.mold);
    let null_r = Mold.nullable(~side=R, tok.mold);
    if (null_l && null_r) {
      Some(Missing_meld);
    } else if (null_l || null_r) {
      Some(Incon_meld);
    } else {
      Some(Extra_meld);
    };
  | Tile(lbl) => Label.is_complete(tok.text, lbl) ? None : Some(Missing_tile)
  };

module Ord = {
  type nonrec t = t;
  let compare = (l, r) => Int.compare(severity(l), severity(r));
};
module Map = Map.Make(Ord);

module Delta = {
  include Map;
  type t = Map.t(int);
  let find = (o, map) => Option.value(find_opt(o, map), ~default=0);

  let zero = empty;
  let decr = (o, map) => add(o, find(o, map) - 1, map);
  let incr = (o, map) => add(o, find(o, map) + 1, map);

  let compare = (l, r) =>
    List.fold_right(
      (o, c) => c != 0 ? c : Int.compare(find(o, l), find(o, r)),
      all,
      0,
    );

  let add_effect = (R(eff): Effects.recorded) =>
    switch (eff) {
    | Effects.Insert(t) =>
      switch (of_token(t)) {
      | None => Fun.id
      | Some(o) => incr(o)
      }
    | Effects.Remove(t) =>
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
      |> ListUtil.min(((_, _, l), (_, _, r)) => compare(l, r));
    if (!to_zero || compare(delta, zero) <= 0) {
      Effects.commit(effs);
      Some(y);
    } else {
      None;
    };
  };
};
