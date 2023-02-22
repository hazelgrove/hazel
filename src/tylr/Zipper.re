open Sexplib.Std;
open Util;

// todo: document potential same-id token on either side of caret
// l|et x = 1 in x + 1
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  sel: Selection.t,
  rel: Stepwell.t,
};

let mk = (~sel=Selection.empty, rel) => {sel, rel};

let init =
  mk(
    Stepwell.of_slopes(
      Slopes.mk(
        ~r=
          Slope.Up.of_meld(
            Meld.of_grout(Grout.mk(Mold.mk_operand(Some(Sort.root)))),
          ),
        (),
      ),
    ),
  );

// let unselect = (d: Dir.t, {sel, rel}: t) =>
//   rel |> Stepwell.cons_seg(~onto=Dir.toggle(d), sel.seg) |> mk;
let unselect = (d: Dir.t, {sel, rel}: t) =>
  rel |> Stepwell.cons_arch(~onto=Dir.toggle(d), sel.arch) |> mk;

let zip = (~d=Dir.L, z: t): Meld.t => {
  let z = unselect(d, z);
  Stepwell.zip(z.rel);
};

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Move(Dir.t)
    | Select(Dir.t)
    | Delete(Dir.t)
    | Insert(string);
};

let move = (d: Dir.t, z: t): option(t) =>
  if (!Selection.is_empty(z.sel)) {
    Some(unselect(d, z));
  } else {
    Stepwell.shift_char(~from=d, z.rel)
    |> Option.map(rel => {...z, rel: Stepwell.assemble(rel)});
  };
let move_n = (n: int, z: t): option(t) =>
  switch (n) {
  | _ when n < 0 => Option.bind(move(L, z), move_n(n + 1))
  | _ when n > 0 => Option.bind(move(R, z), move_n(n - 1))
  | _zero => Some(z)
  };

let unzip_end = (~unzipped, side: Dir.t, mel: Meld.t) =>
  unzipped
  |> Stepwell.cons_slopes(
       side == L
         ? Slope.(Dn.empty, Up.of_meld(mel))
         : Slope.(Dn.of_meld(mel), Up.empty),
     )
  |> mk;

// todo: standardize a la unzip_piece
let unzip_space = (s: Space.t, unzipped) => {
  let (paths, s) = (s.paths, Space.clear_paths(s));
  let (l, sel, r) =
    switch (paths) {
    | [] => Space.(empty, empty, s)
    | [n] =>
      let (l, r) = Space.split(n, s);
      (l, Space.empty, r);
    | [m, n, ..._] =>
      assert(m <= n);
      let (s, r) = Space.split(n, s);
      let (l, s) = Space.split(m, s);
      (l, s, r);
    };
  unzipped
  |> Stepwell.cons_slopes(Slopes.mk(~l, ~r, ()))
  |> mk(~sel=Segment.s(sel));
};

let unzip_piece = (p: Piece.t) => {
  let (paths, p) = (p.paths, Piece.clear_paths(p));
  let ret = (~l=?, ~sel=?, ~r=?, ()) => (l, sel, r);
  let single_path = n =>
    switch (Piece.unzip(n, p)) {
    | L(L) => ret(~r=p, ())
    | L(R) => ret(~l=p, ())
    | R((l, r)) => ret(~l, ~r, ())
    };
  switch (paths) {
  | [] => ret(~r=p, ())
  | [n] => single_path(n)
  // only handling up to two paths marking selection atm
  | [m, n, ..._] =>
    assert(m <= n);
    switch (Piece.unzip(n, p)) {
    | L(L) => ret(~r=p, ())
    | L(R) => single_path(m)
    | R((p, r)) =>
      switch (Piece.unzip(m, p)) {
      | L(L) => ret(~sel=p, ~r, ())
      | L(R) => ret(~l=p, ~r, ())
      | R((l, p)) => ret(~l, ~sel=p, ~r, ())
      }
    };
  };
};

let unzip_lex = (lex: Path.Lex.t, mel: Meld.t, unzipped) =>
  switch (lex) {
  | Space(L) =>
    let (l, r) = mel.space;
    let mel = {...mel, space: (Space.empty, r)};
    unzipped
    |> Stepwell.cons_slopes(Slopes.mk(~up=Up.of_meld(mel)))
    |> unzip_space(l);
  | Space(R) =>
    let (l, r) = mel.space;
    let mel = {...mel, space: (l, Space.empty)};
    unzipped
    |> Stepwell.cons_slopes(Slopes.mk(~dn=Dn.of_meld(mel)))
    |> unzip_space(r);
  | Piece(n) =>
    let (mel_l, p, mel_r) = Meld.split_piece(m, mel);
    let (p_l, p_sel, p_r) = unzip_piece(p);
    let sel =
      p_sel
      |> Option.map(Segment.of_piece)
      |> Option.value(~default=Segment.empty);
    let l =
      p_l
      |> Option.map(p => Meld.knil(mel_l, p))
      |> Option.value(~default=mel_l)
      |> Slope.Dn.of_meld;
    let r =
      p_r
      |> Option.map(p => Meld.link(p, mel_r))
      |> Option.value(~default=mel_r)
      |> Slope.Up.of_meld;
    unzipped |> Stepwell.cons_slopes(Slopes.mk(~l, ~r, ())) |> mk(~sel);
  };

let rec unzip = (~unzipped=Stepwell.empty, mel: Meld.t): t => {
  let (paths, mel) = (mel.paths, Meld.distribute_paths(mel));
  switch (Paths.hd_kid(paths)) {
  | Some(kid) =>
    let mel = Meld.distribute_space(mel);
    switch (Bridge.unzip(kid, mel)) {
    | Some((kid, b)) =>
      let unzipped = Stepwell.cons_bridge(b, unzipped);
      unzip(~unzipped, kid);
    | None =>
      let (kid, slopes) =
        if (kid == 0) {
          let (kid, t) =
            Terrace.L.mk(mel) |> OptUtil.get_or_raise(Path.Invalid);
          (kid, Slope.(Dn.empty, Up.of_terr(t)));
        } else {
          let (t, kid) =
            Terrace.R.mk(mel) |> OptUtil.get_or_raise(Path.Invalid);
          (kid, Slope.(Dn.of_terr(t), Up.empty));
        };
      let unzipped = Stepwell.cons_slopes(slopes, unzipped);
      unzip(~unzipped, kid);
    };
  | None =>
    switch (Paths.hd_lex(paths)) {
    | Some(lex) => unzip_lex(lex, mel, unzipped)
    | None => failwith("todo: unzipping selections")
    }
  };
};

let select = (d: Dir.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let b = Dir.toggle(d);
  if (d == z.sel.foc || Selection.is_empty(z.sel)) {
    let+ (c, rel) = Stepwell.uncons_char(~from=d, z.rel);
    let bs = Stepwell.bounds(rel);
    let sel = Selection.cons_lexeme(c, {...z.sel, foc: d}, bs);
    mk(~sel, rel);
  } else {
    // checked for selection empty above
    let (c, sel) = Option.get(Selection.uncons_char(z.sel));
    let rel =
      z.rel
      |> Stepwell.cons_lexeme(~onto=b, c)
      |> Stepwell.assemble(~sel=sel.seg);
    return(mk(~sel, rel));
  };
};

let delete = (d: Dir.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let+ z = Selection.is_empty(z.sel) ? select(d, z) : return(z);
  let (lexed, rel) = z.rel |> Stepwell.assemble |> Stepwell.relex;
  mk(Stepwell.insert(lexed, rel));
};

let insert = (s: string, z: t): t => {
  let (lexed, rel) = z.rel |> Stepwell.assemble |> Stepwell.relex(~insert=s);
  mk(Stepwell.insert(lexed, rel));
};

let perform = (a: Action.t, z: t): option(t) =>
  switch (a) {
  | Move(d) => move(d, z)
  | Select(d) => select(d, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
