open Util;
open Slope;

// todo: unify better with ziggurat
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | S(Space.t)
  | Z(Ziggurat.t);

let s = s => S(s);
let z = z => Z(z);

let empty = S(Space.empty);
let is_empty =
  fun
  | S(s) when Space.is_empty(s) => true
  | _ => false;

let of_meld = _ => failwith("todo Segment.of_meld");

let push_space = s =>
  fun
  | S(s') => S(Space.cat(s, s'))
  | Z(zigg) => Z(Ziggurat.cons_space(s, zigg));
let hsup_space = (seg, s) =>
  switch (seg) {
  | S(s') => S(Space.cat(s', s))
  | Z(zigg) => Z(Ziggurat.snoc_space(zigg, s))
  };

let push = (t: Terrace.R.t, seg: t) =>
  switch (seg) {
  | S(s) => Error(Z(Ziggurat.of_dn(Dn.mk([t], ~s))))
  | Z(zigg) =>
    Ziggurat.push(t, zigg) |> Result.map(~f=z) |> Result.map_error(~f=z)
  };
let hsup = (seg: t, t: Terrace.L.t) =>
  switch (seg) {
  | S(s) => Error(Z(Ziggurat.of_up(Up.mk(~s, [t]))))
  | Z(zigg) =>
    Ziggurat.hsup(zigg, t) |> Result.map(~f=z) |> Result.map_error(~f=z)
  };

let push_lexeme = (lx, seg) =>
  switch (Lexeme.to_piece(lx)) {
  | Error(s) => Ok(push_space(s, seg))
  | Ok(p) => push(Terrace.of_piece(p), seg)
  };
let hsup_lexeme = (seg, lx) =>
  switch (Lexeme.to_piece(lx)) {
  | Error(s) => Ok(hsup_space(seg, s))
  | Ok(p) => hsup(seg, Terrace.of_piece(p))
  };

let pull_lexeme = (~char=false, seg) =>
  switch (seg) {
  | S(s) =>
    Space.uncons(~char, s)
    |> Option.map(((pulled, s)) => (Lexeme.S(pulled), S(s)))
  | Z(zigg) =>
    switch (Up.uncons_lexeme(~char, zigg.up)) {
    | Some((lx, up)) => Some((lx, Z(Ziggurat.put_up(up, zigg))))
    | None =>
      switch (Chain.unlink(zigg.top)) {
      | Some((p, kid, top)) =>
        switch (Piece.unzip(1, p)) {
        | R((c, rest_p)) when char =>
          let top = Chain.link(rest_p, kid, top);
          Some((Lexeme.of_piece(c), Z({...zigg, top})));
        | _ =>
          let dn = Dn.of_meld(kid);
          Some((Lexeme.of_piece(p), Z({...zigg, dn, top})));
        }
      | None =>
        let p = Chain.fst(zigg.top);
        switch (Piece.unzip(1, p)) {
        | R((c, rest_p)) when char =>
          let top = Retainer.of_piece(rest_p);
          Some((Lexeme.of_piece(c), Z({...zigg, top})));
        | _ =>
          switch (Ziggurat.of_dn(zigg.dn)) {
          | Some(zigg) => Some((Lexeme.of_piece(p), Z(zigg)))
          | None => Some((Lexeme.of_piece(p), S(zigg.dn.space)))
          }
        };
      }
    }
  };
let llup_lexeme = (~char=false, seg) =>
  switch (seg) {
  | S(s) =>
    Space.unsnoc(~char, s)
    |> Option.map(((s, lluped)) => (S(s), Lexeme.S(lluped)))
  | Z(zigg) =>
    switch (Dn.unsnoc_lexeme(~char, zigg.dn)) {
    | Some((dn, lx)) => Some((Z(Ziggurat.put_dn(dn, zigg)), lx))
    | None =>
      switch (Chain.unknil(zigg.top)) {
      | Some((top, kid, p)) =>
        switch (Piece.unzip(Piece.length(p) - 1, p)) {
        | R((rest_p, c)) when char =>
          let top = Chain.knil(top, kid, rest_p);
          Some((Z({...zigg, top}), Lexeme.of_piece(c)));
        | _ =>
          let up = Up.of_meld(kid);
          Some((Z({...zigg, up, top}), Lexeme.of_piece(p)));
        }
      | None =>
        let p = Chain.lst(zigg.top);
        switch (Piece.unzip(Piece.length(p) - 1, p)) {
        | R((rest_p, c)) when char =>
          let top = Retainer.of_piece(rest_p);
          Some((Z({...zigg, top}), Lexeme.of_piece(c)));
        | _ =>
          switch (Ziggurat.of_up(zigg.up)) {
          | Some(zigg) => Some((Z(zigg), Lexeme.of_piece(p)))
          | None => Some((S(zigg.up.space), Lexeme.of_piece(p)))
          }
        };
      }
    }
  };

let split_lt = (dn: Dn.t, seg: t): (Dn.t, Dn.t) =>
  dn
  |> Dn.fold(
       s => (Dn.(empty, mk(~s, [])), push_space(s, seg)),
       (((lt, geq), seg), t) =>
         switch (push(t, seg)) {
         | Ok(seg) =>
           assert(lt == Dn.empty);
           ((lt, Dn.cons(t, geq)), seg);
         | Error(seg) => ((Dn.cons(t, lt), geq), seg)
         },
     )
  |> fst;
let split_gt = (seg: t, up: Up.t): (Up.t, Up.t) =>
  up
  |> Up.fold(
       s => (hsup_space(seg, s), Up.(mk(~s, []), empty)),
       ((seg, (leq, gt)), t) =>
         switch (hsup(seg, t)) {
         | Ok(seg) =>
           assert(gt == Up.empty);
           (seg, (Up.snoc(leq, t), gt));
         | Error(seg) => (seg, (leq, Up.snoc(gt, t)))
         },
     )
  |> snd;
