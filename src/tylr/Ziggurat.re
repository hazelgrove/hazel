// open Util;
open Slope;

// left-to-right: up top dn
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  up: Up.t,
  top: Retainer.t,
  dn: Dn.t,
};

let of_dn = _ => failwith("todo of_dn");
let of_up = _ => failwith("todo of_up");

// let x = (1 + [a + b ? c / d : e * f] + 3) + 4 * 5 in x + 1

// stepwell
// .                                                         .
// let x = ------------------------------------------in x + 1
//         .                                [+ 4, * 5]
//         (-------------------------------)
//         [1 +]                       [+ 3]

// selection
//                     ? c / d :
//                 + b           e *
//               a                   f

// selection:
//   focus: L
//   ziggurat: Some({ up: [a, + b], top: "? c / d :", dn: [e *, f] })
// stepwell:
//   slopes: ([1, +], [+, 3])
//   bridge: ( "(" , ")" )

let mk = (~up=Up.empty, ~dn=Dn.empty, top) => {up, top, dn};
// let empty = mk();
// let is_empty: t => _ = Option.is_none;

let map_top = (f, zigg) => {...zigg, top: f(zigg.top)};
let put_top = top => map_top(_ => top);

let map_up = (f, zigg) => {...zigg, up: f(zigg.up)};
let put_up = up => map_up(_ => up);

let map_dn = (f, zigg) => {...zigg, dn: f(zigg.dn)};
let put_dn = dn => map_dn(_ => dn);

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
  // todo: need to handle possibility of terr >= top
  | Error(kid) =>
    switch (Terrace.cmp(terr, ~kid, Terrace.of_retainer(zigg.top))) {
    | {lt: None, eq: None, gt: None} => failwith("expected fit")
    | {gt: Some(terr_kid), _} => put_up(Up.of_meld(terr_kid), zigg)
    | {eq: Some(terr_kid_top), _} =>
      let (l, top, r) = Option.get(Retainer.of_meld(terr_kid_top));
      zigg
      |> put_up(Up.of_meld(l))
      |> put_top(top)
      |> map_dn(Dn.cat(Dn.of_meld(r)));
    | {lt: Some(_), _} =>
      //                                  ----------zigg
      //                -----------------terr
      // left-to-right: backfill retainer kid top dn
      //                --------new up
      //                         --------new top
      //                                  ----------new dn
      let up = Up.of_meld(terr.backfill);
      let dn = Dn.cons(Terrace.{backfill: kid, retainer: zigg.top}, zigg.dn);
      mk(~up, terr.retainer, ~dn);
    }
  };
let snoc = (zigg: t, terr: Terrace.L.t) =>
  switch (Dn.snoc(zigg.dn, terr)) {
  | Ok(dn) => {...zigg, dn}
  | Error(kid) =>
    switch (Terrace.cmp(Terrace.of_retainer(zigg.top), ~kid, terr)) {
    | {lt: None, eq: None, gt: None} => failwith("expected fit")
    | {lt: Some(kid_terr), _} => put_dn(Dn.of_meld(kid_terr), zigg)
    | {eq: Some(top_kid_terr), _} =>
      let (l, top, r) = Option.get(Retainer.of_meld(top_kid_terr));
      zigg
      |> map_up(Fun.flip(Up.cat, Up.of_meld(l)))
      |> put_top(top)
      |> put_dn(Dn.of_meld(r));
    | {gt: Some(_), _} =>
      //                ----------zigg
      //                           -----------------terr
      // left-to-right: up top kid retainer backfill
      //                                    --------new dn
      //                           --------new top
      //                ----------new up
      let up = Up.snoc(zigg.up, Terrace.{retainer: zigg.top, backfill: kid});
      let dn = Dn.of_meld(terr.backfill);
      mk(~up, terr.retainer, ~dn);
    }
  };

// let onto_up = (zigg: t, up: Up.t): Up.t =>
//   Slope.dn_onto_up(zigg.dn, up)
//   |> Up.cons(Terrace.r_of_t(zigg.top))
//   |> Up.cat(zigg.up);
// let onto_dn = (dn: Dn.t, zigg: t): Dn.t =>
//   Dn.snoc_up(dn, zigg.up)
//   |> Fun.flip(Dn.snoc(Terrace.l_of_t(zigg.top)))
//   |> Fun.flip(Dn.cat(zigg.dn));

let cons_space = s => map_up(Up.cons_space(s));
let snoc_space = Fun.flip(s => map_dn(Fun.flip(Dn.snoc_space, s)));
