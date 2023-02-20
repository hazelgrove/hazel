open Util;
open Slope;

// todo: unify better with ziggurat
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | S(Space.t)
  | Z(Ziggurat.t);

let empty = S(Space.empty);
let is_empty =
  fun
  | S(s) when Space.is_empty(s) => true
  | _ => false;

let push_space = s =>
  fun
  | S(s') => S(Space.cat(s, s'))
  | Z(zigg) => Z(Ziggurat.cons_space(s, zigg));
let hsup_space = (seg, s) =>
  switch (seg) {
  | S(s') => S(Space.cat(s', s))
  | Z(zigg) => Z(Ziggurat.snoc_space(zigg, s))
  };

let push_lexeme = (lx, seg) =>
  switch (Lexeme.to_piece(lx)) {
  | Error(s) => push_space(s, seg)
  | Ok(p) =>
    Z(
      switch (seg) {
      | S(s) => Ziggurat.mk(Retainer.of_piece(p), ~dn=Slope.Dn.mk(~s, []))
      | Z(zigg) => Ziggurat.cons(Terrace.of_piece(p), zigg)
      },
    )
  };
let hsup_lexeme = (seg, lx) =>
  switch (Lexeme.to_piece(lx)) {
  | Error(s) => hsup_space(seg, s)
  | Ok(p) =>
    Z(
      switch (seg) {
      | S(s) => Ziggurat.mk(~up=Slope.Up.mk(~s, []), Retainer.of_piece(p))
      | Z(zigg) => Ziggurat.snoc(zigg, Terrace.of_piece(p))
      },
    )
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
        | Some((c, rest_p)) when char =>
          let top = Chain.link(rest_p, kid, top);
          Some((Lexeme.of_piece(c), Z({...zigg, top})));
        | _ =>
          let dn = Dn.of_meld(kid);
          Some((Lexeme.of_piece(p), Z({...zigg, dn, top})));
        }
      | None =>
        let p = Chain.fst(zigg.top);
        switch (Piece.unzip(1, p)) {
        | Some((c, rest_p)) when char =>
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
        | Some((rest_p, c)) when char =>
          let top = Chain.knil(top, kid, rest_p);
          Some((Z({...zigg, top}), Lexeme.of_piece(c)));
        | _ =>
          let up = Up.of_meld(kid);
          Some((Z({...zigg, up, top}), Lexeme.of_piece(p)));
        }
      | None =>
        let p = Chain.lst(zigg.top);
        switch (Piece.unzip(Piece.length(p) - 1, p)) {
        | Some((rest_p, c)) when char =>
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
