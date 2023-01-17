open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Aba.t(option(kid), Piece.t)
[@deriving (show({with_path: false}), sexp, yojson)]
and kid =
  | K(t);

exception Missing_root;

let empty = Aba.singleton(None);
let is_empty: t => bool = (==)(empty);

let of_piece = (~l=?, ~r=?, p) => Aba.mk([l, r], [p]);
let of_grout = _ => failwith("todo of_grout");

let root: t => list(Piece.t) = Aba.get_bs;
let kids: t => list(option(kid)) = Aba.get_as;

let rec to_lexemes = c =>
  c |> Aba.join(kid_to_lexemes, Piece.to_lexemes) |> List.concat
and kid_to_lexemes =
  fun
  | None => []
  | Some(K(c)) => to_lexemes(c);

[@warning "-27"]
let mold = (c: t, ~kid=?, t: Token.t) => failwith("todo Chain.mold");
// precond: root(c) != []
let sort = _ => failwith("todo sort");
// precond: root(c) != []
let prec = _ => failwith("todo prec");

let expected_sort = (_side: Dir.t, _) =>
  failwith("todo Chain.expected_sort");

let match_ = (_, ~kid as _=?, _) => failwith("todo match_");

module Padded = {
  type c = t;
  // chain with padding (ie single-chain segment)
  type t = (c, (Space.t, Space.t));
  let mk = (~l=Space.empty, ~r=Space.empty, c) => (c, (l, r));
  let empty = mk(empty);
  let is_empty: t => bool = (==)(empty);
  let pad = (~l=Space.empty, ~r=Space.empty, (c, (l', r')): t) => (
    c,
    (l @ l', r' @ r),
  );
};

let cmp = (_: t, _: t): Cmp.t => failwith("todo cmp");

// precond: root(par) != []
// precond: kid convexified
// let push_kid_l = (kid: Padded.t, par: Padded.t): Padded.t => {
//   let (c_kid, (l_kid, r_kid)) = kid;
//   let (c_par, (l_par, r_par)) = par;
//   let (k, p, c) = Aba.uncons(c_par) |> OptUtil.get_or_raise(Missing_root);
//   // todo: consider relaxing this and merging with input kid
//   assert(k == None);
//   let p = Piece.pad(~l=l_par @ r_kid, p);
//   let c = Aba.cons(Some(K(c_kid)), p, c);
//   Padded.mk(~l=l_kid, ~r=r_par, c);
// };
// // precond: root(par) != []
// // precond: kid convexified
// let push_kid_r = (par: Padded.t, kid: Padded.t): Padded.t => {
//   let (c_par, (l_par, r_par)) = par;
//   let (c_kid, (l_kid, r_kid)) = kid;
//   let (c, p, k) = Aba.unsnoc(c_par) |> OptUtil.get_or_raise(Missing_root);
//   // todo: consider relaxing this and merging with input kid
//   assert(k == None);
//   let p = Piece.pad(~r=r_par @ l_kid, p);
//   let c = Aba.snoc(c, p, Some(K(c_kid)));
//   Padded.mk(~l=l_par, ~r=r_kid, c);
// };

let is_porous =
  fun
  | None => true
  | Some(K(kid)) => to_lexemes(kid) |> List.for_all(Lexeme.is_porous);

let rec merge = (~expected: Sort.t, l: t, r: t): t => {
  // possibly shadowed below
  let (kid_l, kid_r) = Aba.(first_a(l), first_a(r));
  switch (Aba.unsnoc(l), Aba.uncons(r)) {
  | (None, None) =>
    switch (merge_kids(~expected, kid_l, kid_r)) {
    | None => of_grout(Grout.mk_convex(expected))
    | Some(K(c)) => c // todo: double-check that unwrapping is ok here
    }
  | (None, Some((kid_r, p, r))) =>
    let kid = merge_kids(~expected=?Piece.expected_sort(L, p), kid_l, kid_r);
    Aba.cons(kid, p, r);
  | (Some((l, p, kid_l)), None) =>
    let kid = merge_kids(~expected=?Piece.expected_sort(R, p), kid_l, kid_r);
    Aba.snoc(l, p, kid);
  | (Some((l', p_l, kid_l)), Some((kid_r, p_r, r'))) =>
    // todo: preserve space
    switch (Piece.rel(p_l, p_r)) {
    | Fills(p) when is_porous(kid_l) && is_porous(kid_r) =>
      Aba.append(l', p, r')
    | Passes(L) when is_porous(kid_l) => failwith("todo")
    | Passes(R) when is_porous(kid_r) => failwith("todo")
    | _ =>
      switch (Piece.cmp(p_l, p_r)) {
      | In =>
        assert(kid_l == None);
        assert(kid_r == None);
        let (s_l, pr_l) = failwith("todo piece sort"); // Piece.(sort(p_l), prec(p_l));
        let (s_r, pr_r) = failwith("todo piece sort"); // Piece.(sort(p_r), prec(p_r));
        let g = Grout.mk_concave((s_l, pr_l), (s_r, pr_r));
        of_piece(
          ~l=mk_kid(~expected=s_l, l),
          ~r=mk_kid(~expected=s_r, r),
          Piece.mk(G(g)),
        );
      | Lt =>
        assert(kid_l == None);
        let r = mk_kid(~expected=Piece.expected_sort(R, p_l), r);
        Aba.snoc(l', p_l, Some(r));
      | Gt =>
        assert(kid_r == None);
        let l = mk_kid(~expected=Piece.expected_sort(L, p_r), l);
        Aba.cons(Some(l), p_r, r');
      | Eq =>
        let expected = Piece.expected_sort(R, p_l);
        let kid = merge_kids(~expected?, kid_l, kid_r);
        Aba.append(l', p_l, Aba.cons(kid, p_r, r'));
      }
    }
  };
}
[@warning "-27"]
and merge_kids = (~expected=?, l: option(kid), r: option(kid)): option(kid) =>
  failwith("todo merge_kids")
[@warning "-27"]
and mk_kid = (~expected=?, c: t): kid => failwith("todo mk_kid");

// let merge = (l: Padded.t, r: Padded.t): Padded.t => {
//   let (c_l, (s_ll, s_lr)) = l;
//   let (c_r, (s_rl, s_rr)) = r;
//   switch (cmp(c_l, c_r)) {
//   | In =>
//     assert(root(c_l) != []);
//     assert(root(c_r) != []);
//     let (s_l, s_r) = (sort(c_l), sort(c_r));
//     let g = Grout.mk_concave((s_l, prec(l)), (s_r, prec(r)));
//     Aba.mk(
//       [Some(Kid.mk(~expected=s_l, c_l)), Some(Kid.mk(~expected=s_r, c_r))],
//       // todo: add padding
//       [Piece.mk(~l=s_lr, ~r=s_rl, G(g))],
//     )
//     |> Padded.mk(~l=s_ll, ~r=s_rr);
//   | Lt =>
//     let r = convexify(r);
//     push_kid_r(l, r);
//   | Eq => failwith("todo eq")
//   | Gt =>
//     let l = convexify(l);
//     push_kid_l(l, r);
//   };
// }
// and merge_eq = (l: t, r: t): t =>
//   if (is_empty(r)) {
//     l
//   } else {
//     switch (Aba.uncons(l)) {
//     | None => failwith("todo")
//     | Some((kid_l, p_l, l)) =>
//       // todo: consider singleton kid case if needed
//       assert(root(r) != []);
//       let (kid_r, p_r, r) = Option.get(Aba.uncons(r));
//       switch (Piece.fill(p_l, p_r)) {
//       | None =>
//         Aba.cons(kid, p_l, merge_eq(l, r))
//       | Some(p) =>

//       }
//     };
//   };

module Kid = {
  type c = t;
  type t = kid;

  let to_chain: option(t) => c =
    fun
    | None => empty
    | Some(K(c)) => c;
  // let rec mk = (~expected: Sort.t, c: c): t =>
  //   K(c |> convexify_l(~expected) |> fst |> convexify_r(~expected) |> fst)
  // and convexify_l = (~expected: Sort.t, c: c): Padded.t =>
  //   switch (Aba.uncons(c)) {
  //   | None =>
  //     assert(Aba.first_a(c) == None);
  //     Padded.mk(of_grout(Grout.mk_convex(expected)));
  //   | Some((kid, p, c')) =>
  //     let c =
  //       switch (Piece.expected_sort(L, p)) {
  //       | None =>
  //         assert(kid == None);
  //         c;
  //       | Some(s) =>
  //         let kid = mk(~expected=s, to_chain(kid));
  //         Aba.cons(Some(kid), p, c');
  //       };
  //     switch (Piece.mk_match(L, p)) {
  //     | None => c
  //     | Some(p) =>
  //       // recurse to generate any additional matching pieces
  //       convexify_l(~expected, Aba.cons(None, p, c))
  //     };
  //   }
  // and convexify_r = (~expected: Sort.t, c: c): c =>
  //   switch (Aba.unsnoc(c)) {
  //   | None =>
  //     assert(Aba.first_a(c) == None);
  //     of_grout(Grout.mk_convex(expected));
  //   | Some((c', p, kid)) =>
  //     let c =
  //       switch (Piece.expected_sort(R, p)) {
  //       | None =>
  //         assert(kid == None);
  //         c;
  //       | Some(s) =>
  //         let kid = mk(~expected=s, to_chain(kid));
  //         Aba.snoc(c', p, Some(kid));
  //       };
  //     switch (Piece.mk_match(R, p)) {
  //     | None => c
  //     | Some(p) =>
  //       // recurse to generate any additional matching pieces
  //       convexify_r(~expected, Aba.snoc(c, p, None))
  //     };
  //   };
};

let pop_kid_l = (c: t): (option(kid), Space.t, t) =>
  switch (Aba.uncons(c)) {
  | None => (Aba.last_a(c), Space.empty, empty)
  | Some((k, p, c)) =>
    let (s, p) = Piece.pop_space_l(p);
    (k, s, Aba.cons(None, p, c));
  };
let pop_kid_r = (c: t): (t, Space.t, option(kid)) =>
  switch (Aba.unsnoc(c)) {
  | None => (empty, Space.empty, Aba.last_a(c))
  | Some((c, p, k)) =>
    let (p, s) = Piece.pop_space_r(p);
    (Aba.snoc(c, p, None), s, k);
  };

// raises if parent is missing root
let push_kid_l = (kid: t, s: Space.t, par: t): t =>
  switch (Aba.uncons(par)) {
  | None =>
    raise(Invalid_argument("Chain.push_kid_l: input parent missing root"))
  | Some((k, p, c)) =>
    assert(k == None);
    let p = Piece.pad(~l=s, p);
    let kid = mk_kid(~expected=?Piece.expected_sort(L, p), kid);
    Aba.cons(Some(kid), p, c);
  };

let finish_l = (~kid=Padded.empty, c: t) => {
  let (kid, (l, r)) = kid;
  let c = push_kid_l(kid, r, c);
  Padded.mk(~l, c);
};

let finish_r = (_, ~kid as _=?, ()) => failwith("todo finish_r");

let cmp_mold = (_: t, _: Mold.t): option(Cmp.t) =>
  failwith("todo cmp_mold");
// accepts empty chains if expected arg provided
[@warning "-27"]
let finish = (~expected: option(Sort.t)=?, _) => failwith("todo finish");

let cmp_merge = (l: t, ~kid=?, r: t): Cmp.Result.t(t, Padded.t, t, Padded.t) =>
  switch (cmp(l, r)) {
  | In =>
    assert(kid == None);
    assert(root(l) != []);
    assert(root(r) != []);
    let (s_l, s_r) = (sort(l), sort(r));
    let g = Grout.mk_concave((s_l, prec(l)), (s_r, prec(r)));
    Cmp.Result.In(
      Aba.mk(
        [Some(mk_kid(~expected=s_l, l)), Some(mk_kid(~expected=s_r, r))],
        // todo: add padding
        [Piece.mk(G(g))],
      ),
    );
  | Lt => Lt(finish_l(~kid?, r))
  | Eq => Eq(match_(l, ~kid?, r))
  | Gt => Gt(finish_r(l, ~kid?, ()))
  };

let to_prefix = (c: t): Aba.t(Space.t, t) => {
  let (c, s, k) = pop_kid_r(c);
  let pre =
    switch (k) {
    | None => Aba.singleton(s)
    | Some(K(kid)) => Aba.mk([s, Space.empty], [kid])
    };
  Aba.cons(Space.empty, c, pre);
};

let of_piece = (p: Piece.t) => Aba.mk([None, None], [p]);
let of_grout = (g: Grout.t) => of_piece(Piece.mk(G(g)));
let of_tile = (t: Tile.t) => of_piece(Piece.mk(T(t)));

[@warning "-27"]
let pop_lexeme = (~from: Dir.t, _) => failwith("todo pop_lexeme");

// todo: probably want to replace with lexeme
[@warning "-27"]
let pop_token = (~from: Dir.t, _) => failwith("todo pop_token");

let split_uni_kid = (d: Dir.t, c: t): (option(kid), t) =>
  switch (d) {
  | L =>
    let (k, (ps, ks)) = Aba.split_first_a(c);
    (k, ([None, ...ks], ps));
  | R =>
    let ((ks, ps), k) = Aba.split_last_a(c);
    (k, (ks @ [None], ps));
  };

// let tip_l = (c: t) => {
//   let (kid, p) =
//     Aba.first_ab(c) |> OptUtil.get_or_raise(Missing_root_pieces);
//   switch (kid) {
//   | Some(_) => Convex
//   | None => Piece.tip(L, p)
//   };
// };
// let tip_l = (c: t) => {
//   let (p, kid) = Aba.lab_ba(c) |> OptUtil.get_or_raise(Missing_root_pieces);
//   switch (kid) {
//   | Some(_) => Convex
//   | None => Piece.tip(R, p)
//   };
// };
// let tip = (d: Dir.t): (t => Tip.t) =>
//   switch (d) {
//   | L => tip_l
//   | R => tip_r
//   };

// let merge = (l: t, r: t): t =>
// switch (Chain.tip(R, c), Chain.tip(L, hd)) {
// | (Convex, Convex) => raise(Nonmonotonic)
// | (Convex, Concave(_)) =>
// }
