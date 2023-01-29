open Sexplib.Std;
open Util;

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

let space_l = ({space: (l, _), _}) => l;
let space_r = ({space: (_, r), _}) => r;
let space = ({space: (l, r), _}) => l @ r;

let id = p =>
  switch (p.shape) {
  | G(g) => g.id
  | T(t) => t.id
  };

let zip = (l: t, r: t): option(t) =>
  switch (l.shape, r.shape) {
  | (G(_), T(_))
  | (T(_), G(_)) => None
  | (G(g_l), G(g_r)) =>
    Grout.zip(g_l, g_r)
    |> Option.map(g => mk(~l=space_l(l), ~r=space_r(r), G(g)))
  | (T(t_l), T(t_r)) =>
    Tile.zip(t_l, t_r)
    |> Option.map(t => mk(~l=space_l(l), ~r=space_r(r), T(t)))
  };

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

let is_strict = p =>
  switch (p.shape) {
  | T(_) => true
  | G(g) => Grout.suggestion(g) != ""
  };

let zipper = (p: t): Gram.Zipper.a(_) => {
  let t =
    switch (p.shape) {
    | G(_) => ""
    | T(t) => t.token
    };
  (Tok(Token.shape(t)), mold(p).frames);
};

let eq = (l: t, r: t): option(Sort.Ana.t) => {
  switch (Mold.tip(R, mold(l))) {
  | Convex => None
  | Concave(sort, _) =>
    let (z_l, z_r) = (zipper(l), zipper(r));
    let (moved_l, moved_r) =
      Gram.Zipper.(move_to_next_tok(R, z_l), move_to_next_tok(L, z_r));
    let strict = is_strict(l) || is_strict(r);
    List.exists(Gram.Zipper.consistent(z_l), moved_r)
    && List.exists(Gram.Zipper.consistent(z_r), moved_l)
      ? Some(Sort.Ana.mk(~strict, ~sort?, ())) : None;
  };
};

let eq_transitive = (l: t, r: t): bool => {
  let rec go = (z_l, z_r) => {
    let moved_r = Gram.Zipper.move_to_next_tok(L, z_r);
    List.exists(Gram.Zipper.consistent(z_l), moved_r)
      ? true : List.exists(go(z_l), moved_r);
  };
  go(zipper(l), zipper(r));
};

// separate from cmp bc these are only relevant
// based on surrounding meld kids (see Meld.merge/degrout)
type dg =
  | Degrout
  | Fill(Dir.t)
  | Pass(Dir.t);

let degrout = (l: t, r: t): option(dg) =>
  switch (l.shape, r.shape) {
  | (T(_), T(_)) => None
  | (G(_), _) when mold(l) == mold(r) => Some(Fill(L))
  | (_, G(_)) when mold(l) == mold(r) => Some(Fill(R))
  | (G(_), _) when eq_transitive(r, l) => Some(Pass(L))
  | (_, G(_)) when eq_transitive(r, l) => Some(Pass(R))
  | (G(g), _) when Grout.suggestion(g) != "" => None
  | (_, G(g)) when Grout.suggestion(g) != "" => None
  // todo: probably need strengthen this check for degrouting
  | (G(_), G(_))
      when Tip.fits(Mold.tip(L, mold(l)), Mold.tip(R, mold(r))) =>
    Some(Degrout)
  | (G(_), _) =>
    Tip.same_shape(Mold.tip(L, mold(l)), Mold.tip(L, mold(r)))
      ? Some(Fill(L)) : None
  | (_, G(_)) =>
    Tip.same_shape(Mold.tip(R, mold(l)), Mold.tip(R, mold(r)))
      ? Some(Fill(R)) : None
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

module Step = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
};

let unzip = (step: Step.t, p: t): Either.t(Dir.t, (t, t)) => {
  let (l, r) = p.space;
  switch (p.shape) {
  | G(g) =>
    Grout.unzip(step, g)
    |> Either.map_r(((g_l, g_r)) => (mk(~l, G(g_l)), mk(~r, G(g_r))))
  | T(t) =>
    Tile.unzip(step, t)
    |> Either.map_r(((t_l, t_r)) => (mk(~l, T(t_l)), mk(~r, T(t_r))))
  };
};
