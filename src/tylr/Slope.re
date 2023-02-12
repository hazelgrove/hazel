module Up = {
  // left-to-right, hierarchically upward
  type t = Chain.t(Space.t, Meld.Closed.l);

  let empty = Chain.of_loop(Space.empty);

  let of_meld = _ => failwith("todo");

  let cons_meld =
      (mel: Meld.Closed.r, ~kid=Meld.empty(), suf: t): Result.t(t, Meld.t) =>
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
          |> OptUtil.get_or_raise(Invalid_argument("Slope.Up.cons_meld"));
        let mel_hd = Meld.in_(mel, ~s, hd);
        Ok(cat(of_meld(hd_mel), tl));
      };
    };
};

module Dn = {
  // left-to-right: hierarchically downward
  // (but stored right-to-left in chain for efficiency, since
  // we always start traversals at lowest point of slope)
  type t = Chain.t(Space.t, Meld.Closed.r);

  let empty = Chain.of_loop(Space.empty);
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
          |> OptUtil.get_or_raise(Invalid_argument("Slope.Dn.cons_meld"));
        let hd_mel = Meld.in_(hd, ~s, mel);
        Ok(cat(of_meld(hd_mel), tl));
      };
    };
};
