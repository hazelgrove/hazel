open Sexplib.Std;
open Util;

module M = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    space: Space.t,
    terrs: list(Terrace.t),
  };

  let mk = (~s=Space.empty, terrs) => {space: s, terrs};
  let empty = mk([]);
  let of_terr = terr => mk([terr]);

  let map_space = (f, s) => {...s, space: f(s.space)};
  // let has_space = s =>
  //   Space.is_empty(s.space)
  //   ? None : Some((s.space, {...s, space: Space.empty}));
};
include M;

module Dn = {
  include M;
  let of_meld = _ => failwith("todo");
  let cat = (_, _) => failwith("todo");

  let snoc_space = (dn, s) => map_space(Fun.flip(Space.cat, s), dn);
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

  let unsnoc_lexeme = (~char=false, dn: t): option((t, Lexeme.t)) =>
    switch (Space.unsnoc(~char, dn.space)) {
    | Some((space, s)) => Some(({...dn, space}, Lexeme.S(s)))
    | None =>
      switch (dn.terrs) {
      | [] => None
      | [hd, ...tl] =>
        let (rest, s, lx) = Terrace.R.unsnoc_lexeme(~char, hd);
        Some((mk(cat(tl, rest), ~s), lx));
      }
    };

  let zip = (dn: t, kid: Meld.t) =>
    List.fold_left(
      (kid, terr) => Terrace.R.unmk(terr, kid),
      Meld.pad(~l=dn.space, kid),
      dn.terrs,
    );
};

module Up = {
  include M;

  let of_meld = _ => failwith("todo");

  let cat = (_, _) => failwith("todo");

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

  let uncons_lexeme = (~char=false, up: t): option((Lexeme.t, t)) =>
    switch (Space.uncons(~char, up.space)) {
    | Some((s, space)) => Some((Lexeme.S(s), {...up, space}))
    | None =>
      switch (up.terrs) {
      | [] => None
      | [hd, ...tl] =>
        let (lx, s, rest) = Terrace.L.uncons_lexeme(~char, hd);
        Some((lx, mk(~s, cat(rest, tl))));
      }
    };

  let zip = (kid: Meld.t, up: t) =>
    List.fold_left(
      (kid, terr) => Terrace.L.unmk(kid, terr),
      Meld.pad(kid, ~r=up.space),
      up.terrs,
    );
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
