open Util;

module Atom = {
  type t =
    | Tok(Token.shape)
    | Kid(Sort.t);
};

module Extremity = {
  type t = option(Atom.t);
  type s = list(t);
};

// maybe want to generalize over atoms
type t =
  | Atom(Atom.t)
  | Star(t)
  | Seq(list(t))
  | Alt(list(t));

// simplifying assumption, not fundamentally necessary
exception No_consecutive_tokens;

let seq = gs => Seq(gs);
let alt = gs => Alt(gs);

let eps = Seq([]);

let tok = (t: Token.shape) => Atom(Tok(t));
let kid = s => Atom(Kid(s));

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

let exterior_seq = (exterior: t => Extremity.s, gs: list(t)): Extremity.s => {
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

let rec exterior = (d: Dir.t, g: t): Extremity.s =>
  switch (g) {
  | Atom(a) => [Some(a)]
  | Star(g) => exterior(d, g)
  | Alt(gs) => List.concat_map(exterior(d), gs)
  | Seq(gs) => exterior_seq(edge(d), ListUtil.rev_if(d == R, gs))
  };

module Frame = {
  type g = t;
  type t =
    | Star_
    | Seq_(list(g), list(g))
    | Alt_(list(g), list(g));
  type s = list(t);

  let empty = [];

  let rec interior = (d: Dir.t): (s => Extremity.s) =>
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

  let rec must_match = (d: Dir.t, fs: s): bool =>
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
