open Haunch;

module Key = {
  // the "keystone" of the arch
  type t = Meld.Closed.t;

  let mk_l = ((hd, tl): Meld.Closed.l, p: Piece.t) =>
    Chain.untrim(hd, tl, p);
  let mk_r = (p: Piece.t, (tl, hd): Meld.Closed.r) =>
    Chain.untrim(p, tl, hd);

  let of_piece = _ => failwith("todo");
};

// left-to-right-: up key dn
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
    map_up(Up.cons_meld(mel), a);
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
    map_dn(Dn.cons_meld(mel), a);
  };

let onto_up = (a: t, up: Up.t): Up.t =>
  a.dn
  |> Dn.fold(
       s => Up.cons_space(s, up),
       (up, mel, s) => up |> Up.cons_meld(mel) |> Up.cons_space(s),
     )
  |> Up.cons_meld(Meld.Closed.r_of_t(a.key))
  |> Up.cat(a.up);
let onto_dn = (dn: Dn.t, a: t): Dn.t =>
  a.up
  |> Up.fold(
       s => Dn.cons_space(s, dn),
       (dn, mel, s) => dn |> Dn.cons_meld(mel) |> Dn.cons_space(s),
     )
  |> Dn.cons_meld(Meld.Closed.l_of_t(a.key))
  |> Dn.cat(a.dn);
