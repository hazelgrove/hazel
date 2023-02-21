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
    Stepwell.of_sib(
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

let rec unzip = (~unzipped=Stepwell.empty, mel: Meld.t): t => {
  let (paths, mel) = (mel.paths, Meld.clear_paths(mel));
  switch (paths) {
  | [] => unzip_end(~unzipped, L, mel)
  | [{kids: [], here: Space(side, n)}] =>
    unzip_end(~unzipped, side, mel)
    |> move_n(side == L ? n : Space.length(snd(mel.space)) - n)
    |> OptUtil.get_or_raise(Path.Invalid)
  | [{kids: [], here: Piece(m, n)}] =>
    open Slope;
    let (l, p, r) = Meld.split_piece(m, mel);
    let slopes =
      switch (Piece.unzip(n, p)) {
      | Some((p_l, p_r)) => (
          Dn.of_meld(Meld.knil(l, p_l)),
          Up.of_meld(Meld.link(p_r, r)),
        )
      | None =>
        n == 0
          ? (Dn.of_meld(l), Up.of_meld(Meld.link(p, r)))
          : (Dn.of_meld(Meld.knil(l, p)), Up.of_meld(r))
      };
    unzipped |> Stepwell.cons_slopes(slopes) |> mk;
  | [{kids: [hd, ...tl], _} as path] =>
    switch (Bridge.unzip(hd, mel)) {
    | Some((b, kid)) =>
      let unzipped = Stepwell.cons_bridge(b, unzipped);
      let kid = Meld.add_paths([{...path, kids: tl}], kid);
      unzip(~unzipped, kid);
    | None =>
      let (kid, slopes) =
        if (hd == 0) {
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
    }
  | _ => failwith("todo unzip selection")
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
