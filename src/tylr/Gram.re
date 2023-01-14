open Util;

module Atom = {
  type t('sort) =
    | Tok(Token.Shape.t)
    | Kid('sort);
};

module Extremity = {
  type t('sort) = option(Atom.t('sort));
  type s('sort) = list(t('sort));
};

// maybe want to generalize over atoms
type t('sort) =
  | Atom(Atom.t('sort))
  | Star(t('sort))
  | Seq(list(t('sort)))
  | Alt(list(t('sort)));

// simplifying assumption, not fundamentally necessary
exception No_consecutive_tokens;

let tok_shape = (t: Token.Shape.t) => Atom(Tok(t));
let tok = (s: string) => tok_shape(Const(s));

let kid = s => Atom(Kid(s));

let seq = gs => Seq(gs);
let alt = gs => Alt(gs);

let eps = Seq([]);

let opt = g => Alt([eps, g]);

let rec nullable =
  fun
  // assuming all sorts are non-nullable
  // but could change this in future
  | Atom(_) => false
  | Star(_) => true
  | Seq(gs) => List.for_all(nullable, gs)
  | Alt(gs) => List.exists(nullable, gs);

let rec root_token =
  fun
  | Atom(Kid(_)) => false
  | Atom(Tok(_)) => true
  | Star(_) => false
  | Seq(gs) => List.exists(root_token, gs)
  | Alt(gs) => List.for_all(root_token, gs);

let exterior_seq =
    (exterior: t(_) => Extremity.s(_), gs: list(t(_))): Extremity.s(_) => {
  let (nullable, rest) = ListUtil.take_while(nullable, gs);
  let nullable = List.concat_map(exterior, nullable);
  let rest =
    switch (rest) {
    | [] => [None]
    | [hd, ..._] => exterior(hd)
    };
  nullable @ rest;
};

let edge = _ => failwith("todo edge");

let rec exterior = (d: Dir.t, g: t(_)): Extremity.s(_) =>
  switch (g) {
  | Atom(a) => [Some(a)]
  | Star(g) => exterior(d, g)
  | Alt(gs) => List.concat_map(exterior(d), gs)
  | Seq(gs) => exterior_seq(edge(d), ListUtil.rev_if(d == R, gs))
  };

module Frame = {
  type g('sort) = t('sort);
  type t('sort) =
    | Star_
    | Seq_(list(g('sort)), list(g('sort)))
    | Alt_(list(g('sort)), list(g('sort)));
  type s('sort) = list(t('sort));

  let empty = [];

  let rec interior = (d: Dir.t): (s(_) => Extremity.s(_)) =>
    fun
    | [] => [None]
    | [f, ...fs] =>
      switch (d, f) {
      | (_, Star_ | Alt_(_))
      | (L, Seq_([], _))
      | (R, Seq_(_, [])) => interior(d, fs)
      | (L, Seq_([_, ..._] as gs_l, _)) =>
        exterior_seq(exterior(R), gs_l)
        @ (List.for_all(nullable, gs_l) ? interior(d, fs) : [])
      | (R, Seq_(_, [_, ..._] as gs_r)) =>
        exterior_seq(exterior(L), gs_r)
        @ (List.for_all(nullable, gs_r) ? interior(d, fs) : [])
      };

  let rec must_match = (d: Dir.t, fs: s(_)): bool =>
    switch (fs) {
    | [] => false
    | [f, ...fs] =>
      switch (d, f) {
      | (_, Star_ | Alt_(_))
      | (L, Seq_([], _))
      | (R, Seq_(_, [])) => must_match(d, fs)
      | (L, Seq_([_, ..._] as gs_l, _)) =>
        List.exists(root_token, gs_l) || must_match(d, fs)
      | (R, Seq_(_, [_, ..._] as gs_r)) =>
        List.exists(root_token, gs_r) || must_match(d, fs)
      }
    };
};
