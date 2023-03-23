open Sexplib.Std;
open Util;
open Slope;

// left-to-right: up top dn
// invariant: if top is None, then up and dn have 0 height
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  up: Up.t,
  top: option(Wald.t),
  dn: Dn.t,
};

let mk = (~up=Up.empty, ~top=?, ~dn=Dn.empty, ()) => {
  if (Option.is_none(top) && Slope.(height(up) > 0 || height(dn) > 0)) {
    raise(Invalid_argument("Ziggurat.mk"));
  };
  {up, top, dn};
};
let empty = mk();
let is_empty = ({up, top, dn}: t) =>
  Option.is_none(top) && Space.(is_empty(up.space) && is_empty(dn.space));

let of_up = up =>
  switch (Up.unsnoc(up)) {
  | None => mk(~up, ())
  | Some((up, t)) =>
    let dn = Dn.of_meld(t.mel);
    mk(~up, ~top=t.wal, ~dn, ());
  };
let of_dn = dn =>
  switch (Dn.uncons(dn)) {
  | None => mk(~dn, ())
  | Some((t, dn)) =>
    let up = Up.of_meld(t.mel);
    mk(~up, ~top=t.wal, ~dn, ());
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
let put_top = (top, zigg) => {...zigg, top: Some(top)};

let map_up = (f, zigg) => {...zigg, up: f(zigg.up)};
let put_up = up => map_up(_ => up);

let map_dn = (f, zigg) => {...zigg, dn: f(zigg.dn)};
let put_dn = dn => map_dn(_ => dn);

// let of_r = ({mel, wal}: Terrace.R.t): t =>
//   switch (Terrace.L.mk(mel)) {
//   | None =>
//     // todo: review that passing terr.mel here is fine
//     mk(~up=Up.of_meld(mel), wal)
//   | Some((kid, l)) => mk(~up=Up.of_meld(kid), Wald.of_l(l, wal))
//   };

let push = (terr: Terrace.R.t, zigg: t) =>
  switch (Up.cons(terr, zigg.up)) {
  | Ok(up) => Ok({...zigg, up})
  | Error(kid) =>
    switch (zigg.top) {
    | None =>
      let up = Up.of_meld(terr.mel);
      let dn = Dn.mk(~s=Space.cat(zigg.up.space, zigg.dn.space), []);
      Error(mk(~up, ~top=terr.wal, ~dn, ()));
    | Some(top) =>
      switch (Terrace.cmp(terr, ~kid, Terrace.of_wald(top))) {
      | None => failwith("expected fit")
      | Some(Gt(terr_kid)) => Ok(put_up(Up.of_meld(terr_kid), zigg))
      | Some(Eq(terr_kid_top)) =>
        let (l, top, r) = Option.get(Wald.mk(terr_kid_top));
        Ok(
          zigg
          |> put_up(Up.of_meld(l))
          |> put_top(top)
          |> map_dn(Dn.cat(Dn.of_meld(r))),
        );
      | Some(Lt(_)) =>
        //                                  ----------zigg
        //                -----------------terr
        // left-to-right: mel wal kid top dn
        //                --------new up
        //                         --------new top
        //                                  ----------new dn
        let up = Up.of_meld(terr.mel);
        let dn = Dn.cons(Terrace.{mel: kid, wal: top}, zigg.dn);
        Error(mk(~up, ~top=terr.wal, ~dn, ()));
      }
    }
  };
let hsup = (zigg: t, terr: Terrace.L.t) =>
  switch (Dn.snoc(zigg.dn, terr)) {
  | Ok(dn) => Ok({...zigg, dn})
  | Error(kid) =>
    switch (zigg.top) {
    | None =>
      let up = Up.mk(~s=Space.cat(zigg.up.space, zigg.dn.space), []);
      let dn = Dn.of_meld(terr.mel);
      Error(mk(~up, ~top=terr.wal, ~dn, ()));
    | Some(top) =>
      switch (Terrace.cmp(Terrace.of_wald(top), ~kid, terr)) {
      | None => failwith("expected fit")
      | Some(Lt(kid_terr)) => Ok(put_dn(Dn.of_meld(kid_terr), zigg))
      | Some(Eq(top_kid_terr)) =>
        let (l, top, r) = Option.get(Wald.mk(top_kid_terr));
        Ok(
          zigg
          |> map_up(Fun.flip(Up.cat, Up.of_meld(l)))
          |> put_top(top)
          |> put_dn(Dn.of_meld(r)),
        );
      | Some(Gt(_)) =>
        //                ----------zigg
        //                           -----------------terr
        // left-to-right: up top kid wal mel
        //                                    --------new dn
        //                           --------new top
        //                ----------new up
        let up = Up.snoc(zigg.up, Terrace.{wal: top, mel: kid});
        let dn = Dn.of_meld(terr.mel);
        Error(mk(~up, ~top=terr.wal, ~dn, ()));
      }
    }
  };
// let push = (~onto: Dir.t, t) =>
//   switch (onto) {
//   | L => push(t)
//   | R => Fun.flip(hsup, t)
//   };

let push_space = s => map_up(Up.cons_space(s));
let hsup_space = Fun.flip(s => map_dn(Fun.flip(Dn.snoc_space, s)));

let push_lexeme = (lx, zigg) =>
  switch (Lexeme.to_piece(lx)) {
  | Error(s) => Ok(push_space(s, zigg))
  | Ok(p) => push(Terrace.of_piece(p), zigg)
  };
let hsup_lexeme = (zigg, lx) =>
  switch (Lexeme.to_piece(lx)) {
  | Error(s) => Ok(hsup_space(zigg, s))
  | Ok(p) => hsup(zigg, Terrace.of_piece(p))
  };

let pull_lexeme = (~char=false, zigg: t): option((Lexeme.t, t)) => {
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
        (Lexeme.of_piece(c), put_top(top, zigg));
      | _ =>
        let dn = Dn.of_meld(kid);
        let zigg = zigg |> put_top(top) |> put_dn(dn);
        (Lexeme.of_piece(p), zigg);
      }
    | None =>
      let p = Chain.fst(top);
      switch (Piece.unzip(1, p)) {
      | R((c, rest_p)) when char =>
        let top = Wald.of_piece(rest_p);
        (Lexeme.of_piece(c), put_top(top, zigg));
      | _ => (Lexeme.of_piece(p), of_dn(zigg.dn))
      };
    };
  };
  // try pulling from dn.space
  assert(Slope.height(zigg.dn) == 0);
  let+ (pulled, s) = Space.uncons(~char, zigg.dn.space);
  (Lexeme.S(pulled), {...zigg, dn: Dn.mk(~s, [])});
};
let llup_lexeme = (~char=false, zigg: t): option((t, Lexeme.t)) => {
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
        (put_top(top, zigg), Lexeme.of_piece(c));
      | _ =>
        let up = Up.of_meld(kid);
        let zigg = zigg |> put_up(up) |> put_top(top);
        (zigg, Lexeme.of_piece(p));
      }
    | None =>
      let p = Chain.lst(top);
      switch (Piece.unzip(Piece.length(p) - 1, p)) {
      | R((rest_p, c)) when char =>
        let top = Wald.of_piece(rest_p);
        (put_top(top, zigg), Lexeme.of_piece(c));
      | _ => (of_up(zigg.up), Lexeme.of_piece(p))
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
