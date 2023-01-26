// open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type shape =
  | T(Tile.t)
  | G(Grout.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  shape,
  space: (Space.s, Space.s),
};

let mk = (~l=Space.empty, ~r=Space.empty, shape) => {shape, space: (l, r)};

let pad = (~l=Space.empty, ~r=Space.empty, {shape, space: (l', r')}) => {
  shape,
  space: (l @ l', r' @ r),
};

let is_porous = _ => failwith("todo is_porous");

let space = ({space: (l, r), _}) => l @ r;

let pop_space_l = ({shape, space: (l, r)}: t) => (
  l,
  {shape, space: (Space.empty, r)},
);
let pop_space_r = ({shape, space: (l, r)}: t) => (
  {shape, space: (l, Space.empty)},
  r,
);

let mold = p =>
  switch (p.shape) {
  | T(t) => t.mold
  | G(g) => g.mold
  };
let sort = p => Mold.sort_(mold(p));
let prec = p => Mold.prec_(mold(p));
let tip = (side, p) => Mold.tip(side, mold(p));

let length = (~with_space as _=false, p: t) => {
  switch (p.shape) {
  | T(t) => Tile.length(t)
  | G(g) => Grout.length(g)
  };
};

let is_grout = p =>
  switch (p.shape) {
  | G(_) => true
  | T(_) => false
  };

// separate from cmp bc these are only relevant
// based on surrounding meld kids (see Meld.merge/degrout)
type dg =
  | Degrout
  | Fill(Dir.t)
  | Pass(Dir.t);

[@warning "-27"]
let degrout = (l: t, r: t): option(dg) => failwith("todo degrout");
// switch (l.shape, r.shape) {
// | (T(_), T(_)) => None
// | (T(t), G({mold: None, _})) =>
//   switch (t.mold) {
//   | None =>
//     // unmolded grout is infix between unmolded tiles
//     Token.Shape.is_operand(token)
//     ? None : Some(Fill(R))
//   | Some(_) => Some(Fill(R))
//   }
// | (G({mold: None, _}), T(t)) =>
//   switch (t.mold) {
//   | None =>
//     Token.Shape.is_operand(token)
//     ? None : Some(Fill(L))
//   | Some(_) => Some(Fill(L))
//   }
// | (T(t), G({mold: Some(m), _}))
// | _ => failwith("todo")
// };

let is_strict = _ => failwith("todo is_strict");

let zipper = (p: t): Gram.Zipper.t(_) => {
  let t =
    switch (p.shape) {
    | G(_) => ""
    | T(t) => t.token
    };
  (Atom(Tok(Token.shape(t))), mold(p).frames);
};

let eq = (l: t, r: t): option(Sort.Ana.t) => {
  switch (Mold.tip(R, mold(l))) {
  | Convex => None
  | Concave(sort, _) =>
    let (z_l, z_r) = (zipper(l), zipper(r));
    let (moved_l, moved_r) = Gram.Zipper.(move(R, z_l), move(L, z_r));
    let strict = is_strict(l) || is_strict(r);
    List.exists(Gram.Zipper.consistent(z_l), moved_r)
    && List.exists(Gram.Zipper.consistent(z_r), moved_l)
      ? Some(Sort.Ana.mk(~strict, ~sort?, ())) : None;
  };
};

let cmp = (l: t, r: t): (Cmp.leg(Sort.Ana.t) as 'r) => {
  let (m_l, m_r) = (mold(l), mold(r));
  switch (eq(l, r)) {
  | Some(s) => Eq(s)
  | None =>
    let lt = sort => Cmp.Lt(Sort.Ana.mk(~strict=is_strict(l), ~sort?, ()));
    let gt = sort => Cmp.Gt(Sort.Ana.mk(~strict=is_strict(r), ~sort?, ()));
    let eq = sort => {
      let strict = is_strict(l) || is_strict(r);
      Cmp.Eq(Sort.Ana.mk(~strict, ~sort?, ()));
    };
    let try_lt = (~else_=Cmp.In(), sort_l): 'r =>
      Sort.compare_o(sort_l, sort(r)) <= 0 ? lt(sort_l) : else_;
    let try_gt = (~else_=Cmp.In(), sort_r): 'r =>
      Sort.compare_o(sort(l), sort_r) >= 0 ? gt(sort_r) : else_;
    switch (Mold.tip(R, m_l), Mold.tip(L, m_r)) {
    | (Convex, Convex) => In()
    | (Convex, Concave(sort_r, _)) => try_gt(sort_r)
    | (Concave(sort_l, _), Convex) => try_lt(sort_l)
    | (Concave(sort_l, prec_l), Concave(sort_r, prec_r)) =>
      // todo: revise when generalizing to sort partial order
      switch (Sort.compare_o(sort(l), sort(r))) {
      | c when c < 0 => lt(sort_l)
      | c when c > 0 => gt(sort_r)
      | _ =>
        switch (Prec.compare(prec_l, prec_r)) {
        | c when c < 0 => try_lt(~else_=try_gt(sort_r), sort_l)
        | c when c > 0 => try_gt(~else_=try_lt(sort_l), sort_r)
        | _ =>
          switch (LangUtil.assoc(sort(l), prec_l)) {
          | Some(L) => gt(sort_r)
          | Some(R) => lt(sort_l)
          | None => eq(sort_l)
          }
        }
      }
    };
  };
};
