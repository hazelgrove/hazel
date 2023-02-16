// open Util;
open Slope;

// left-to-right: up top dn
type t = {
  up: Up.t,
  top: Retainer.t,
  dn: Dn.t,
};

let mk = (~up=Slope.empty, ~dn=Slope.empty, top) => {up, top, dn};
// let empty = mk();
// let is_empty: t => _ = Option.is_none;

// let map: (_, t) => t = Option.map;
// let map_up = f => map(m => {...m, up: f(m.up)});
// let map_dn = f => map(m => {...m, dn: f(m.dn)});

// let of_r = ({backfill, retainer}: Terrace.R.t): t =>
//   switch (Terrace.L.mk(backfill)) {
//   | None =>
//     // todo: review that passing terr.backfill here is fine
//     mk(~up=Up.of_meld(backfill), retainer)
//   | Some((kid, l)) => mk(~up=Up.of_meld(kid), Retainer.of_l(l, retainer))
//   };

let cons = (terr: Terrace.R.t, zigg: t) =>
  switch (Up.cons(terr, zigg.up)) {
  | Ok(up) => {...zigg, up}
  | Error(kid) =>
    //                                  ----------zigg
    //                -----------------terr
    // left-to-right: backfill retainer kid top dn
    //                --------new up
    //                         --------new top
    //                                  ----------new dn
    let up = Up.of_meld(terr.backfill);
    let dn = Dn.cons(Terrace.{backfill: kid, retainer: zigg.top}, zigg.dn);
    mk(~up, terr.retainer, ~dn);
  };
let snoc = (zigg: t, terr: Terrace.L.t) =>
  switch (Dn.snoc(zigg.dn, terr)) {
  | Ok(dn) => {...zigg, dn}
  | Error(kid) =>
    //                ----------zigg
    //                           -----------------terr
    // left-to-right: up top kid retainer backfill
    //                                    --------new dn
    //                           --------new top
    //                ----------new up
    let up = Up.snoc(zigg.up, Terrace.{retainer: zigg.top, backfill: kid});
    let dn = Dn.of_meld(terr.backfill);
    mk(~up, terr.retainer, ~dn);
  };

// let onto_up = (zigg: t, up: Up.t): Up.t =>
//   Slope.dn_onto_up(zigg.dn, up)
//   |> Up.cons(Terrace.r_of_t(zigg.top))
//   |> Up.cat(zigg.up);
// let onto_dn = (dn: Dn.t, zigg: t): Dn.t =>
//   Dn.snoc_up(dn, zigg.up)
//   |> Fun.flip(Dn.snoc(Terrace.l_of_t(zigg.top)))
//   |> Fun.flip(Dn.cat(zigg.dn));
