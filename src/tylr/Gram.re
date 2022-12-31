open Util;

module Atom = {
  type t =
    | Tok(Token.t)
    | Kid(Sort.t);
};

module Extremity = {
  type t = option(Atom.t);
  type s = list(t);
};

type t =
  | Atom(Atom.t)
  | Star(t)
  | Seq(list(t))
  | Alt(list(t));

// simplifying assumption, not fundamentally necessary
exception No_consecutive_tokens;

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

let rec exterior = (d: Dir.t, g: t): Extremity.s =>
  fun
  | Atom(a) => [Some(a)]
  | Star(g) => exterior(d, g)
  | Alt(gs) => List.concat_map(exterior(d), gs)
  | Seq(gs) => exterior_seq(edge(d), ListUtil.rev_if(d == R, gs));

module Frame = {
  type g = t;
  type t =
    | Star_
    | Seq_(list(g), list(g))
    | Alt_(list(g), list(g));
  type s = list(t);

  let rec interior = (d: Dir.t, fs: s): Extremity.s =>
    fun
    | [] => [None]
    | [f, ...fs] =>
      switch (d, f) {
      | (_, Star_ | Alt_(_))
      | (L, Seq([], _))
      | (R, Seq(_, [])) => interior(d, fs)
      | (L, Seq([_, ..._] as gs_l, _)) =>
        exterior_seq(exterior(R), gs_l)
        @ (List.for_all(nullable, gs_l) ? interior(d, fs) : [])
      | (R, Seq(_, [_, ..._] as gs_r)) =>
        exterior_seq(exterior(L), gs_r)
        @ (List.for_all(nullable, gs_r) ? interior(d, fs) : [])
      };

  let must_match = (d: Dir.t, fs: s): bool =>
    fun
    | [] => false
    | [f, ...fs] =>
      switch (d, f) {
      | (_, Star_ | Alt_(_))
      | (L, Seq([], _))
      | (R, Seq(_, [])) => must_match(d, fs)
      | (L, Seq([_, ..._] as gs_l, _)) =>
        List.exists(root_token, gs_l) || must_match(d, fs)
      | (R, Seq(_, [_, ..._] as gs_r)) =>
        List.exists(root_token, gs_r) || must_match(d, fs)
      };
};
