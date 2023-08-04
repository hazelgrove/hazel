open Sexplib.Std;
open Util;

module Path = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
  let shift = (n, p) => p + n;
};

// todo: rename to something like Material
// module Shape = {
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t =
//     | T(Tile.t)
//     | G(Grout.t);

//   let t = t => T(t);
//   let g = g => G(g);
// };

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  paths: list(Path.t),
  mold: Mold.t,
  token: Token.t,
};

// well-labeled invariant: for piece p
// !Label.is_empty(p.mold.label) ==> is_prefix(p.token, p.mold.label)
exception Ill_labeled;

let label = p => Mold.label(p.mold);
let sort = p => p.mold.sort;
let prec = p => p.mold.prec;

// let tip = (side, p) => Mold.tip(side, mold(p));
// let tips = (side, p) => Mold.tips(side, mold(p));
// let convexable = (side, p) => List.mem(Tip.Convex, tips(side, p));

// let mk = (~id=?, ~paths=[], shape: Shape.t) => {
//   let id = id |> OptUtil.get(() => Id.Gen.next());
//   {id, paths, shape};
// };
// let of_grout = (~id=?, ~paths=[], g) => mk(~id?, ~paths, G(g));
// let of_tile = (~id=?, ~paths=[], t) => mk(~id?, ~paths, T(t));

let add_paths = (ps, p) => {...p, paths: ps @ p.paths};
let clear_paths = p => {...p, paths: []};

let is_grout = p => Label.is_empty(label(p));

let is_constant = p => Label.is_const(label(p));

let token_length = p => Token.length(p.token);
let label_length = p =>
  switch (Label.length(label(p))) {
  | Some(n) => n
  | None => token_length(p)
  };

let is_complete = p =>
  // assumes well-labeled
  switch (Label.length(label(p))) {
  | None => true
  | Some(0) =>
    assert(is_grout(p));
    true;
  | Some(n) => Token.length(p.token) == n
  };

let is_porous = p => Token.is_empty(p.token);

let unzip = (n: int, p: t): Result.t((t, t), Dir.t) => {
  switch (Token.unzip(n, p.token)) {
  | Error(L) => Error(L)
  | Error(R) when n == Token.length(p.token) =>
    switch (Label.unzip(n, label(p))) {
    | Error(_r) => Error(R)
    | Ok((lbl_l, lbl_r)) =>
      let l = Piece.put_label(lbl_l, p);
      let r = Piece.put_label(lbl_r, {...p, token: Token.empty});
      Ok((l, r));
    }
  | Error(R) => Error(R)
  | Ok((tok_l, tok_r)) =>
    switch (Label.unzip(label(p))) {
    | Error(_) => raise(Ill_labeled)
    | Ok((lbl_l, lbl_r)) =>
      let l = Piece.put_label(lbl_l, {...p, token: tok_l});
      let r = Piece.put_label(lbl_r, {...p, token: tok_r});
      Ok((l, r));
    }
  };
};

let zip = (l: t, r: t): option(t) => {
  open OptUtil.Syntax;
  let* () = OptUtil.of_bool(Id.eq(l.id, r.id));
  let+ lbl = Label.zip(label(l), label(r));
  l
  |> put_label(lbl)
  |> put_token(l.token ++ r.token)
  |> put_paths(l.paths @ List.map(Path.shift(token_length(l)), r.paths));
};
let zips = (l, r) => Option.is_some(zip(l, r));

// replace/merge/fuse/matches no longer needed under assumptions:
// - grout removed and re-inserted on each edit
// - mold procedure handles removing and inserting temp tiles

// let replace = (l: t, r: t): option(t) => {
//   let replaced_l = add_paths(List.map(_ => 0, l.paths), r);
//   let replaced_r = add_paths(List.map(_ => length(l), r.paths), l);
//   switch (l.shape, r.shape) {
//   | (G(g), _)
//       when Grout.is_empty(g) && Tips.consistent(tips(L, l), tips(L, r)) =>
//     Some(replaced_l)
//   | (_, G(g))
//       when Grout.is_empty(g) && Tips.consistent(tips(R, l), tips(R, r)) =>
//     Some(replaced_r)
//   | (T(t), _) when !Tile.is_filled(t) && proto(l) == proto(r) =>
//     Some(replaced_l)
//   | (_, T(t)) when !Tile.is_filled(t) && proto(l) == proto(r) =>
//     Some(replaced_r)
//   | _ => None
//   };
// };

// let merge = (l: t, r: t): option(t) =>
//   switch (l.shape, r.shape) {
//   | (G(g_l), G(g_r)) =>
//     Grout.merge(g_l, g_r)
//     |> Option.map(of_grout(~id=l.id, ~paths=l.paths @ r.paths))
//   | _ => None
//   };

// let fuse = (l: t, r: t): option(t) => {
//   open OptUtil.Syntax;
//   let/ () = zip(l, r);
//   let/ () = replace(l, r);
//   merge(l, r);
// };

// let matches = (l: t, r: t): option(Complement.t) => {
//   let mk_mold = frames => {...mold(l), frames};
//   let rec go = (l, r) => {
//     let moved = Gram.Zipper.move_to_tok(~skip_nullable=true, R, l);
//     if (List.mem(r, moved)) {
//       zipper_const(l)
//       |> Option.map(((label, frames)) =>
//            [Proto.{label, mold: mk_mold(frames)}]
//          );
//     } else {
//       moved
//       |> List.map(l => (l, go(l, r)))
//       |> List.filter(((_, found_r)) => Option.is_some(found_r))
//       |> List.filter_map(((l, found_r)) =>
//            zipper_const(l)
//            |> Option.map(((label, frames)) =>
//                 (Proto.{label, mold: mk_mold(frames)}, found_r)
//               )
//          )
//       |> ListUtil.hd_opt
//       |> Option.map(((l, compl)) => [l, ...Option.get(compl)]);
//     };
//   };
//   go(zipper(l), zipper(r)) |> Option.map(List.tl);
// };

// let complement_beyond = (~side: Dir.t, p: t): Complement.t => {
//   let rec go = z =>
//     switch (Gram.Zipper.move_to_tok(~skip_nullable=true, side, z)) {
//     // | [z, ..._] when Option.map(zipper, upto) == Some(z) => []
//     // default to first alternative
//     | [(Tok(Const(label)), frames) as z, ..._] =>
//       let mold = {...mold(p), frames};
//       [Proto.{label, mold}, ...go(z)];
//     | _ => []
//     };
//   go(zipper(p));
// };

// let fst_mold = (cmpl: Complement.t, p: t) =>
//   ListUtil.hd_opt(cmpl)
//   |> Option.map(Proto.mold_)
//   |> Option.value(~default=mold(p));
// let lst_mold = (p: t, cmpl: Complement.t) =>
//   ListUtil.last_opt(cmpl)
//   |> Option.map(Proto.mold_)
//   |> Option.value(~default=mold(p));

// let lt = (l: t, r: t): option(Complement.t) => {
//   open OptUtil.Syntax;
//   let* (s_l, p_l) = Mold.concavable(R, mold(l));
//   let* _ = SortDeps.takes(s_l, sort(r));
//   let cmpl = complement_beyond(~side=L, r);
//   switch (Mold.concavable(L, fst_mold(cmpl, r))) {
//   | Some((s_r, p_r))
//       when Sort.eq(s_l, s_r) && !Prec.lt(~a=LangUtil.assoc(s_l), p_l, p_r) =>
//     None
//   | _ => Some(cmpl)
//   };
// };

// let gt = (l: t, r: t): option(Complement.t) => {
//   open OptUtil.Syntax;
//   let* (s_r, p_r) = Mold.concavable(L, mold(r));
//   let* _ = SortDeps.takes(s_r, sort(l));
//   let cmpl = complement_beyond(~side=R, l);
//   switch (Mold.concavable(R, lst_mold(l, cmpl))) {
//   | Some((s_l, p_l))
//       when Sort.eq(s_l, s_r) && !Prec.gt(~a=LangUtil.assoc(s_l), p_l, p_r) =>
//     None
//   | _ => Some(cmpl)
//   };
// };

// let mold_eq = (p: t, t: Token.t): option((Grammar.Walk.Eq.t, Mold.t)) => {
//   let leq = Grammar.walk(R, proto(p));
//   Prototiles.of_token(t)
//   |> List.filter_map(q => {
//        open OptUtil.Syntax;
//        let* ws = Proto.Map.find_opt(q, leq);
//        let* w = ListUtil.hd_opt(ws);
//        Grammar.Walk.height(w) == 1 ? Some((w, q)) : None;
//      })
//   |> ListUtil.hd_opt;
// };

// todo: factor in kid (may want to include kids in grammar walks?)
// (hmmm... realizing that would also indicate exactly what holes to insert)
// (so then melding doesn't need to be responsible for error correction at all)
// let mold_lt =
//     (p: t, ~kid=?, t: Token.t): Result.t((Grammar.Walk.t, Mold.t), Sort.o) => {
//   let leq = Grammar.walk(R, proto(p));
//   Prototiles.of_token(t)
//   |> List.filter_map(q => {
//        open OptUtil.Syntax;
//        let* ws = Proto.Map.find_opt(q, leq);
//        let* w = ListUtil.hd_opt(ws);
//        Grammar.Walk.height(w) > 1 ? Some((w, q)) : None;
//      })
//   |> ListUtil.hd_opt
//   |> Result.of_option(~error=sort(p));
// };
