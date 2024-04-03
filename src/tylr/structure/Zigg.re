// L2R: up top dn
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  up: Slope.Up.t,
  top: Wald.t,
  dn: Slope.Dn.t,
};

let mk = (~up=Slope.empty, ~dn=Slope.empty, top) => {up, top, dn};
let unit = tok => mk(Wald.unit(tok));

let orient = (d: Dir.t, {up, top, dn}: t) => {
  let (s_d, s_b) = Dir.order(d, (up, dn));
  let top = Dir.pick(d, (Fun.id, Wald.rev), top);
  (s_d, top, s_b);
};
let unorient = (d: Dir.t, (s_d, top, s_b)) => {
  let (up, dn) = Dir.order(d, (s_d, s_b));
  let top = Dir.pick(d, (Fun.id, Wald.rev), top);
  mk(~up, top, ~dn);
};

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

let map_top = (f, zigg) => {...zigg, top: f(zigg.top)};
// let put_top = (top, zigg) => {...zigg, top: Some(top)};

let map_up = (f, zigg) => {...zigg, up: f(zigg.up)};
let put_up = up => map_up(_ => up);

let map_dn = (f, zigg) => {...zigg, dn: f(zigg.dn)};
let put_dn = dn => map_dn(_ => dn);
