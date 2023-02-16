open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  space: Space.t,
  terrs: list(Terrace.t),
};
[@deriving (show({with_path: false}), sexp, yojson)]
type slope = t;
// type dn = t; // left-to-right: terrs space
// type up = t; // left-to-right: space terrs

let mk = (~s=Space.empty, terrs) => {space: s, terrs};
let empty = mk([]);

let map_space = (f, s) => {...s, space: f(s.space)};

let cat = (_, _) => failwith("todo");

module Dn = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = slope;

  let of_meld = _ => failwith("todo");

  let snoc_space = (terr, s) => map_space(Fun.flip(Space.cat, s), terr);

  let rec snoc =
          (dn: t, ~kid=Meld.empty(), terr: Terrace.L.t): Result.t(t, Meld.t) => {
    let kid = Meld.pad(~l=dn.space, kid);
    switch (dn.terrs) {
    | [] => Error(kid)
    | [hd, ...tl] =>
      // left-to-right: tl hd kid terr
      switch (Terrace.cmp(hd, ~kid, terr)) {
      | {lt: Some(kid_terr), _} =>
        Ok(cat(of_meld(kid_terr), mk(dn.terrs)))
      | {eq: Some(hd_kid_terr), _} =>
        Ok(cat(of_meld(hd_kid_terr), mk(tl)))
      | {gt: Some(hd_kid), _} => snoc(mk(tl), ~kid=hd_kid, terr)
      | {lt: None, eq: None, gt: None} =>
        let s =
          Meld.is_empty(kid)
          |> OptUtil.get_or_raise(Invalid_argument("Slope.Dn.cons_terr"));
        let hd_terr = Terrace.in_(hd, ~s, terr);
        Ok(cat(of_meld(hd_terr), mk(tl)));
      }
    };
  };

  let cons = (terr: Terrace.R.t, dn: t) => {
    ...dn,
    terrs: [terr, ...dn.terrs],
  };
};
module Up = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = slope;

  let of_meld = _ => failwith("todo");

  let cons_space = s => map_space(Space.cat(s));

  let rec cons =
          (terr: Terrace.R.t, ~kid=Meld.empty(), up: t): Result.t(t, Meld.t) => {
    let kid = Meld.pad(kid, ~r=up.space);
    switch (up.terrs) {
    | [] => Error(kid)
    | [hd, ...tl] =>
      // left-to-right: terr kid hd tl
      switch (Terrace.cmp(terr, ~kid, hd)) {
      | {lt: Some(kid_hd), _} => cons(terr, ~kid=kid_hd, mk(tl))
      | {eq: Some(terr_kid_hd), _} =>
        Ok(cat(of_meld(terr_kid_hd), mk(tl)))
      | {gt: Some(terr_kid), _} =>
        Ok(cat(of_meld(terr_kid), mk(up.terrs)))
      | {lt: None, eq: None, gt: None} =>
        let s =
          Meld.is_empty(kid)
          |> OptUtil.get_or_raise(Invalid_argument("Slope.Up.cons_terr"));
        let terr_hd = Terrace.in_(terr, ~s, hd);
        Ok(cat(of_meld(terr_hd), mk(tl)));
      }
    };
  };

  let snoc = (up: t, terr: Terrace.L.t) => {
    ...up,
    terrs: up.terrs @ [terr],
  };
};

let dn_onto_up =
    ({terrs, space}: Dn.t, up: Up.t): Result.t(Up.t, (Dn.t, Meld.t)) =>
  List.fold_left(
    (up, terr) =>
      switch (up) {
      | Error((dn, kid)) => Error((Dn.cons(terr, dn), kid))
      | Ok(up) =>
        Up.cons(terr, up) |> Result.map_error(~f=kid => (mk([terr]), kid))
      },
    Ok(Up.cons_space(space, up)),
    terrs,
  );

let up_onto_dn =
    (dn: Dn.t, {space, terrs}: Up.t): Result.t(t, (Meld.t, Up.t)) =>
  List.fold_left(
    (dn, terr) =>
      switch (dn) {
      | Error((kid, up)) => Error((kid, Up.snoc(up, terr)))
      | Ok(dn) =>
        Dn.snoc(dn, terr) |> Result.map_error(~f=kid => (kid, mk([terr])))
      },
    Ok(Dn.snoc_space(dn, space)),
    terrs,
  );
