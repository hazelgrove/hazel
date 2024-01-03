open Sexplib.Std;
open Util;
open Slope;

// left-to-right: up top dn
[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = {
  up: Slope.Up.t('a),
  top: Wald.t('a),
  dn: Slope.Dn.t('a),
};

let mk = (~up=Slope.empty, ~dn=Slope.empty, top) => {up, top, dn};

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
let put_top = (top, zigg) => {...zigg, top: Some(top)};

let map_up = (f, zigg) => {...zigg, up: f(zigg.up)};
let put_up = up => map_up(_ => up);

let map_dn = (f, zigg) => {...zigg, dn: f(zigg.dn)};
let put_dn = dn => map_dn(_ => dn);

let pull_lexeme = (~char=false, zigg: t): option((Lexeme.t(_), t)) => {
  open OptUtil.Syntax;
  // try pulling from up
  let/ () = {
    let+ (lx, up) = Up.uncons_lexeme(~char, zigg.up);
    (lx, put_up(up, zigg));
  };
  // try pulling from top
  let/ () = {
    let+ top = zigg.top;
    switch (Chain.unlink(top)) {
    | Some((p, kid, top)) =>
      switch (Piece.unzip(1, p)) {
      | R((c, rest_p)) when char =>
        let top = Chain.link(rest_p, kid, top);
        (Lexeme.T(c), put_top(top, zigg));
      | _ =>
        let dn = Dn.of_meld(kid);
        let zigg = zigg |> put_top(top) |> put_dn(dn);
        (Lexeme.T(p), zigg);
      }
    | None =>
      let p = Chain.fst(top);
      switch (Piece.unzip(1, p)) {
      | R((c, rest_p)) when char =>
        let top = Wald.of_piece(rest_p);
        (Lexeme.T(c), put_top(top, zigg));
      | _ => (Lexeme.T(p), of_dn(zigg.dn))
      };
    };
  };
  // try pulling from dn.space
  assert(Slope.height(zigg.dn) == 0);
  let+ (pulled, s) = Space.uncons(~char, zigg.dn.space);
  (Lexeme.S(pulled), {...zigg, dn: Dn.mk(~s, [])});
};
let llup_lexeme = (~char=false, zigg: t): option((t, Lexeme.t(_))) => {
  open OptUtil.Syntax;
  // try pulling from dn
  let/ () = {
    let+ (dn, lx) = Dn.unsnoc_lexeme(~char, zigg.dn);
    (put_dn(dn, zigg), lx);
  };
  // try pulling from top
  let/ () = {
    let+ top = zigg.top;
    switch (Chain.unknil(top)) {
    | Some((top, kid, p)) =>
      switch (Piece.unzip(Piece.length(p) - 1, p)) {
      | R((rest_p, c)) when char =>
        let top = Chain.knil(top, kid, rest_p);
        (put_top(top, zigg), Lexeme.T(c));
      | _ =>
        let up = Up.of_meld(kid);
        let zigg = zigg |> put_up(up) |> put_top(top);
        (zigg, Lexeme.T(p));
      }
    | None =>
      let p = Chain.lst(top);
      switch (Piece.unzip(Piece.length(p) - 1, p)) {
      | R((rest_p, c)) when char =>
        let top = Wald.of_piece(rest_p);
        (put_top(top, zigg), Lexeme.T(c));
      | _ => (of_up(zigg.up), Lexeme.T(p))
      };
    };
  };
  // try pulling from up.space
  assert(Slope.height(zigg.up) == 0);
  let+ (s, lluped) = Space.unsnoc(~char, zigg.up.space);
  ({...zigg, up: Up.mk(~s, [])}, Lexeme.S(lluped));
};
// let pull_lexeme = (~from: Dir.t, ~char=false, zigg): option((Lexeme.t, t)) =>
//   switch (from) {
//   | L => pull_lexeme(~char, zigg)
//   | R => llup_lexeme(~char, zigg) |> Option.map(((a, b)) => (b, a))
//   };

let split_lt = (dn: Dn.t, zigg: t): (Dn.t, Dn.t) =>
  dn
  |> Dn.fold(
       s => (Dn.(empty, mk(~s, [])), push_space(s, zigg)),
       (((lt, geq), seg), t) =>
         switch (push(t, seg)) {
         | Ok(seg) =>
           assert(lt == Dn.empty);
           ((lt, Dn.cons(t, geq)), seg);
         | Error(seg) => ((Dn.cons(t, lt), geq), seg)
         },
     )
  |> fst;
let split_gt = (zigg: t, up: Up.t): (Up.t, Up.t) =>
  up
  |> Up.fold(
       s => (hsup_space(zigg, s), Up.(mk(~s, []), empty)),
       ((seg, (leq, gt)), t) =>
         switch (hsup(seg, t)) {
         | Ok(seg) =>
           assert(gt == Up.empty);
           (seg, (Up.snoc(leq, t), gt));
         | Error(seg) => (seg, (leq, Up.snoc(gt, t)))
         },
     )
  |> snd;
