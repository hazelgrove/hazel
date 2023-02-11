module Dn = {
  // left-to-right in edit state: hierarchically downward
  // (but hd of chain is rightmost)
  type t = Chain.t(Space.t, Meld.Closed.l);

  let of_meld = _ => failwith("todo");

  let cons_meld =
      (mel: Meld.Closed.l, ~kid=Meld.empty(), suf: t): Result.t(t, Meld.t) =>
    switch (Chain.unlink(suf)) {
    | None => Error(Meld.pad(kid, ~r=Chain.lst(suf)))
    | Some((s, hd, tl)) =>
      // left-to-right: mel kid s hd tl
      let kid = Meld.pad(kid, ~l=r);
      switch (
        Meld.lt(mel, ~kid, hd),
        Meld.eq(mel, ~kid, hd),
        Meld.gt(mel, ~kid, hd),
      ) {
      | (Some(kid_hd), _, _) => cons_meld(mel, ~kid=kid_hd, tl)
      | (_, Some(mel_kid_hd), _) => Ok(cat(of_meld(hd_kid_mel), tl))
      | (_, _, Some(mel_kid)) => Ok(cat(of_meld(mel_kid), link(hd, tl)))
      | (None, None, None) =>
        let s =
          Meld.is_empty(kid)
          |> OptUtil.get_or_raise(Invalid_argument("Suffix.cons_meld"));
        let mel_hd = Meld.in_(mel, ~s, hd);
        Ok(cat(of_meld(hd_mel), tl));
      };
    };
};

module Up = {
  // left-to-right, hierarchically upward
  type t = Chain.t(Space.t, Meld.Closed.r);

  let of_meld = _ => failwith("todo");

  let cons_meld =
      (pre: t, ~kid=Meld.empty(), mel: Meld.Closed.l): Result.t(t, Meld.t) =>
    switch (Chain.unlink(pre)) {
    | None => Error(Meld.pad(~l=Chain.fst(pre), kid))
    | Some((s, hd, tl)) =>
      // left-to-right: tl hd s kid mel
      let kid = Meld.pad(~l=s, kid);
      switch (
        Meld.lt(hd, ~kid, mel),
        Meld.eq(hd, ~kid, mel),
        Meld.gt(hd, ~kid, mel),
      ) {
      | (Some(kid_mel), _, _) => Ok(cat(of_meld(kid_mel), link(hd, tl)))
      | (_, Some(hd_kid_mel), _) => Ok(cat(of_meld(hd_kid_mel), tl))
      | (_, _, Some(hd_kid)) => cons_meld(tl, ~kid=hd_kid, mel)
      | (None, None, None) =>
        let s =
          Meld.is_empty(kid)
          |> OptUtil.get_or_raise(Invalid_argument("Prefix.cons_meld"));
        let hd_mel = Meld.in_(hd, ~s, mel);
        Ok(cat(of_meld(hd_mel), tl));
      };
    };
};

module Key = {
  // the "keystone" of the arch
  type t = Meld.Closed.t;

  let mk_l = ((hd, tl): Meld.Closed.l, p: Piece.t) =>
    Chain.untrim(hd, tl, p);
  let mk_r = (p: Piece.t, (tl, hd): Meld.Closed.r) =>
    Chain.untrim(p, tl, hd);

  let of_piece = _ => failwith("todo");
};

// left-to-right order: up key dn
type t = {
  up: Up.t,
  key: option(Key.t),
  dn: Dn.t,
};

let mk = (~up=[], ~key=?, ~dn=[], ()) => {up, key, dn};
let empty = mk();
// invariant: if key == None, then up == [] == dn
// iow "can't have arch without keystone"
let is_empty = a => Option.is_none(a.key);

let map_up = (f, a) => {...a, up: f(a.up)};
let map_dn = (f, a) => {...a, dn: f(a.dn)};

let rec cons_meld = (mel: Meld.Closed.r, a: t) =>
  if (is_empty(a)) {
    let (tl, hd) = mel;
    switch (Meld.Closed.mk_l(tl)) {
    | Error(kid) => cons_meld(kid, mk(~key=Key.of_piece(hd), ()))
    | Ok((kid, closed)) =>
      mk(~up=Up.of_meld(kid), ~key=Key.mk_l(closed, hd), ())
    };
  } else {
    map_up(Up.cons_meld(mel), a)
  };
let rec snoc_meld = (a: t, mel: Meld.Closed.l) =>
  if (is_empty(a)) {
    let (hd, tl) = mel;
    switch (Meld.Closed.mk_r(tl)) {
    | Error(kid) => snoc_meld(mk(~key=Key.of_piece(hd), ()), kid)
    | Ok((closed, kid)) =>
      mk(~key=Key.mk_r(hd, closed), ~dn=Dn.of_meld(kid), ())
    };
  } else {
    map_dn(Dn.cons_meld(mel), a)
  };
