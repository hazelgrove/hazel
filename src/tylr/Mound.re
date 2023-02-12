open Slope;

module Peak = {
  type t = Meld.Closed.t;

  let mk_l = ((hd, tl): Meld.Closed.l, p: Piece.t) =>
    Chain.untrim(hd, tl, p);
  let mk_r = (p: Piece.t, (tl, hd): Meld.Closed.r) =>
    Chain.untrim(p, tl, hd);

  let of_piece = _ => failwith("todo");
};

// left-to-right: up peak dn
type t' = {
  up: Up.t,
  peak: Peak.t,
  dn: Dn.t,
};
type t = option(t');

let mk = (~up=[], ~peak=?, ~dn=[], ()) =>
  peak |> Option.map(peak => {up, peak, dn});
let empty = mk();
let is_empty: t => _ = Option.is_none;

let map: (_, t) => t = Option.map;
let map_up = f => map(m => {...m, up: f(m.up)});
let map_dn = f => map(m => {...m, dn: f(m.dn)});

let rec cons_meld = (mel: Meld.Closed.r, m: t) =>
  if (is_empty(m)) {
    let (tl, hd) = mel;
    switch (Meld.Closed.mk_l(tl)) {
    | Error(kid) => cons_meld(kid, mk(~key=Peak.of_piece(hd), ()))
    | Ok((kid, closed)) =>
      mk(~up=Up.of_meld(kid), ~key=Peak.mk_l(closed, hd), ())
    };
  } else {
    map_up(Up.cons_meld(mel), m);
  };
let rec snoc_meld = (m: t, mel: Meld.Closed.l) =>
  if (is_empty(m)) {
    let (hd, tl) = mel;
    switch (Meld.Closed.mk_r(tl)) {
    | Error(kid) => snoc_meld(mk(~key=Peak.of_piece(hd), ()), kid)
    | Ok((closed, kid)) =>
      mk(~key=Peak.mk_r(hd, closed), ~dn=Dn.of_meld(kid), ())
    };
  } else {
    map_dn(Dn.cons_meld(mel), m);
  };

let onto_up = (m: t, up: Up.t): Up.t =>
  m.dn
  |> Dn.fold(
       s => Up.cons_space(s, up),
       (up, mel, s) => up |> Up.cons_meld(mel) |> Up.cons_space(s),
     )
  |> Up.cons_meld(Meld.Closed.r_of_t(m.peak))
  |> Up.cat(m.up);
let onto_dn = (dn: Dn.t, m: t): Dn.t =>
  m.up
  |> Up.fold(
       s => Dn.cons_space(s, dn),
       (dn, mel, s) => dn |> Dn.cons_meld(mel) |> Dn.cons_space(s),
     )
  |> Dn.cons_meld(Meld.Closed.l_of_t(m.peak))
  |> Dn.cat(m.dn);
