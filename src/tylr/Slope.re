open Sexplib.Std;
open Util;

module M = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    space: Space.t,
    // Dn and Up slopes named based on left-to-right order of terraces
    // as they appear in edit state, but terraces are always maintained
    // internally low-to-high
    terrs: list(Terrace.t),
  };

  let mk = (~s=Space.empty, terrs) => {space: s, terrs};
  let empty = mk([]);
  let of_terr = terr => mk([terr]);
  let of_piece = p => of_terr(Terrace.of_piece(p));
  let of_s = s => mk(~s, []);

  let height = slope => List.length(slope.terrs);

  let map_space = (f, s) => {...s, space: f(s.space)};
  // let has_space = s =>
  //   Space.is_empty(s.space)
  //   ? None : Some((s.space, {...s, space: Space.empty}));

  // always folds up the slope
  let fold = (f_s, f_terr, slope) =>
    List.fold_left(f_terr, f_s(slope.space), slope.terrs);
};
include M;

module Dn = {
  include M;
  let of_meld = mel => {
    let rec go = (terrs, mel) =>
      switch (Terrace.R.mk(mel)) {
      | None =>
        // todo: may need to handle singleton kid
        let (l, r) = mel.space;
        mk(~s=Space.cat(l, r), terrs);
      | Some((terr, kid)) => go([terr, ...terrs], kid)
      };
    go([], mel);
  };

  let hsup_space = (s: Space.t, dn: t) =>
    switch (ListUtil.split_last_opt(dn.terrs)) {
    | None => map_space(Space.cat(s), dn)
    | Some((ts, t)) => {...dn, terrs: ts @ [Terrace.R.pad(s, t)]}
    };
  let hsup = (terr: Terrace.R.t, dn: t) => {
    ...dn,
    terrs: [terr, ...dn.terrs],
  };
  let llup = dn =>
    ListUtil.split_last_opt(dn.terrs)
    |> Option.map(((terrs, t)) => (t, {...dn, terrs}));

  let cat = (l: t, r: t) => {
    let r = hsup_space(l.space, r);
    mk(~s=r.space, r.terrs @ l.terrs);
  };

  let push_space = (dn, s) => map_space(Fun.flip(Space.cat, s), dn);

  // let rec push = (dn: t, ~kid=Meld.empty(), w: Wald.t): Result.t(t, Meld.t) => {
  //   let kid = Meld.pad(~l=dn.space, kid);
  //   switch (dn.terrs) {
  //   | [] => Error(kid)
  //   | [hd, ...tl] =>

  //   };
  // };

  let rec push =
          (dn: t, ~kid=Meld.empty(), terr: Terrace.L.t): Result.t(t, Meld.t) => {
    let kid = Meld.pad(~l=dn.space, kid);
    switch (dn.terrs) {
    | [] => Error(kid)
    | [hd, ...tl] =>
      // left-to-right: tl hd kid terr
      switch (Terrace.cmp(hd, ~kid, terr)) {
      | Some(Lt(kid_terr)) => Ok(cat(mk(dn.terrs), of_meld(kid_terr)))
      | Some(Eq(hd_kid_terr)) =>
        // todo: tighten this check to sort and left tip change
        // todo: avoid decomposing left kid if unneeded
        if (Meld.end_piece(~side=L, hd_kid_terr) != Some(Chain.fst(hd.wal))) {
          // if eq merge led to replacing left end of hd with a new piece,
          // then need to continue recursing in case of precedence change
          let (l, hkt) =
            Terrace.L.mk(hd_kid_terr) |> OptUtil.get_or_fail("expected root");
          let dn = cat(mk(tl), of_meld(l));
          snoc(dn, hkt);
        } else {
          Ok(cat(mk(tl), of_meld(hd_kid_terr)));
        }
      | Some(Gt(hd_kid)) => snoc(mk(tl), ~kid=hd_kid, terr)
      | None =>
        let s =
          Meld.is_empty(kid)
          |> OptUtil.get_or_raise(Invalid_argument("Slope.Dn.cons_terr"));
        let hd_terr = Terrace.in_(hd, ~s, terr);
        Ok(cat(mk(tl), of_meld(hd_terr)));
      }
    };
  };
  let unsnoc_lexeme = (~char=false, dn: t): option((t, Lexeme.t(_))) =>
    switch (Space.unsnoc(~char, dn.space)) {
    | Some((space, s)) => Some(({...dn, space}, Lexeme.S(s)))
    | None =>
      switch (dn.terrs) {
      | [] => None
      | [hd, ...tl] =>
        // left-to-right: tl hd
        let (rest, s, lx) = Terrace.R.unsnoc_lexeme(~char, hd);
        Some((mk(tl @ rest, ~s), lx));
      }
    };

  let zip = (dn: t, kid: Meld.t) =>
    List.fold_left(
      (kid, terr) => Terrace.R.unmk(terr, kid),
      Meld.pad(~l=dn.space, kid),
      dn.terrs,
    );

  let rec mold =
          (~match, dn: t, ~kid: option(Sort.o)=?, t: Token.t)
          : Result.t(Mold.t, option(Sort.o)) =>
    switch (dn.terrs) {
    | [] => Error(kid)
    | [terr, ...terrs] =>
      open Result.Syntax;
      let/ kid = Terrace.R.mold(terr, ~kid?, t);
      mold(~match, {...dn, terrs}, ~kid?, t);
    };

  let rec mold_lt = (dn: t, ~kid: option(Sort.o)=?, t: Token.t) =>
    switch (dn.terrs) {
    | [] => Error(kid)
    | [terr, ...terrs] =>
      open Result.Syntax;
      let/ kid = Terrace.R.mold_lt(terr, ~kid?, t);
      mold_lt({...dn, terrs}, ~kid?, t);
    };
  let rec mold_eq = (dn: t, ~kid: option(Sort.o)=?, t: Token.t) =>
    switch (dn.terrs) {
    | [] => Error(kid)
    | [terr, ...terrs] =>
      open Result.Syntax;
      let/ kid = Terrace.R.mold_eq(terr, ~kid?, t);
      mold_eq({...dn, terrs}, ~kid?, t);
    };

  let complement = dn => List.concat_map(Terrace.R.complement, dn.terrs);
};

module Up = {
  include M;

  let of_meld = mel => {
    let rec go = (mel, terrs) =>
      switch (Terrace.L.mk(mel)) {
      | None =>
        // todo: may need to handle singleton kid
        let (l, r) = mel.space;
        mk(~s=Space.cat(l, r), terrs);
      | Some((kid, terr)) => go(kid, [terr, ...terrs])
      };
    go(mel, []);
  };

  let snoc_space = (up: t, s: Space.t) =>
    switch (ListUtil.split_last_opt(up.terrs)) {
    | None => map_space(Fun.flip(Space.cat, s), up)
    | Some((ts, t)) => {...up, terrs: ts @ [Terrace.L.pad(t, s)]}
    };
  let snoc = (up: t, terr: Terrace.L.t) => {
    ...up,
    terrs: up.terrs @ [terr],
  };
  let unsnoc = up =>
    ListUtil.split_first_opt(up.terrs)
    |> Option.map(((t, terrs)) => ({...up, terrs}, t));

  let cat = (l: t, r: t) => {
    let l = snoc_space(l, r.space);
    mk(~s=l.space, l.terrs @ r.terrs);
  };

  let cons_space = s => map_space(Space.cat(s));

  let rec cons =
          (terr: Terrace.R.t, ~kid=Meld.empty(), up: t): Result.t(t, Meld.t) => {
    let kid = Meld.pad(kid, ~r=up.space);
    switch (up.terrs) {
    | [] => Error(kid)
    | [hd, ...tl] =>
      // left-to-right: terr kid hd tl
      switch (Terrace.cmp(terr, ~kid, hd)) {
      | Some(Lt(kid_hd)) => cons(terr, ~kid=kid_hd, mk(tl))
      | Some(Eq(terr_kid_hd)) =>
        if (Meld.end_piece(~side=R, terr_kid_hd) != Some(Chain.lst(hd.wal))) {
          let (tkh, r) =
            Terrace.R.mk(terr_kid_hd) |> OptUtil.get_or_fail("expected root");
          let up = cat(of_meld(r), mk(tl));
          cons(tkh, up);
        } else {
          Ok(cat(of_meld(terr_kid_hd), mk(tl)));
        }
      | Some(Gt(terr_kid)) => Ok(cat(of_meld(terr_kid), mk(up.terrs)))
      | None =>
        let s =
          Meld.is_empty(kid)
          |> OptUtil.get_or_raise(Invalid_argument("Slope.Up.cons_terr"));
        let terr_hd = Terrace.in_(terr, ~s, hd);
        Ok(cat(of_meld(terr_hd), mk(tl)));
      }
    };
  };
  let uncons_lexeme = (~char=false, up: t): option((Lexeme.t(_), t)) =>
    switch (Space.uncons(~char, up.space)) {
    | Some((s, space)) => Some((Lexeme.S(s), {...up, space}))
    | None =>
      switch (up.terrs) {
      | [] => None
      | [hd, ...tl] =>
        let (lx, s, rest) = Terrace.L.uncons_lexeme(~char, hd);
        Some((lx, mk(~s, rest @ tl)));
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
