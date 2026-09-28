open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

/* Blackboard core terms: Figure 1 of "Blackboard: A Clean Slate Proof
   Assistant", in the author's corrected form.  One syntactic class:

     t, T ::= x | t : T | type | (x : T1) -> T2 | t1 t2

   Binders are named, as users write them.  The de Bruijn reference
   semantics is the Rocq development in metatheory/Blackboard.v. */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Var(string)
  | Type
  | Mem(t, t) /* t : T, internal type membership */
  | Pi(string, t, t) /* (x : A) -> B */
  | App(t, t);

/* A signature is an ordered list of declarations; later entries may
   mention earlier names. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type entry = {
  name: string,
  ty: t,
};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type signature = list(entry);

/* Documents are sequences of blocks (paper, Section 3).  An assume block
   may omit its tactic, as the paper does after the first example. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type block =
  | Assume(signature, option(string))
  | Construct(signature, string);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type doc = list(block);

module S = Set.Make(String);

let rec free_vars = (t: t): S.t =>
  switch (t) {
  | Var(x) => S.singleton(x)
  | Type => S.empty
  | Mem(a, b)
  | App(a, b) => S.union(free_vars(a), free_vars(b))
  | Pi(x, a, b) => S.union(free_vars(a), S.remove(x, free_vars(b)))
  };

let fresh = (x: string, avoid: S.t): string => {
  let rec go = y => S.mem(y, avoid) ? go(y ++ "'") : y;
  go(x);
};

/* Capture-avoiding substitution t[s/x]. */
let rec subst = (x: string, s: t, t: t): t =>
  switch (t) {
  | Var(z) => z == x ? s : t
  | Type => Type
  | Mem(a, b) => Mem(subst(x, s, a), subst(x, s, b))
  | App(a, b) => App(subst(x, s, a), subst(x, s, b))
  | Pi(y, a, b) =>
    let a' = subst(x, s, a);
    if (y == x) {
      Pi(y, a', b);
    } else if (S.mem(y, free_vars(s))) {
      let avoid = S.union(free_vars(s), S.add(x, free_vars(b)));
      let y' = fresh(y, avoid);
      Pi(y', a', subst(x, s, subst(y, Var(y'), b)));
    } else {
      Pi(y, a', subst(x, s, b));
    };
  };

/* Equality up to renaming of bound variables.  The only notion of "same
   type" the checker has: the logic has no reduction. */
let alpha_eq = (t1: t, t2: t): bool => {
  let rec go = (env: list((string, string)), t1, t2) =>
    switch (t1, t2) {
    | (Var(x), Var(y)) =>
      switch (List.assoc_opt(x, env)) {
      | Some(y') => y == y'
      | None => x == y && !List.exists(((_, b)) => b == y, env)
      }
    | (Type, Type) => true
    | (Mem(a, b), Mem(c, d))
    | (App(a, b), App(c, d)) => go(env, a, c) && go(env, b, d)
    | (Pi(x, a, b), Pi(y, c, d)) =>
      go(env, a, c) && go([(x, y), ...env], b, d)
    | _ => false
    };
  go([], t1, t2);
};

/* Concrete syntax, as the paper writes it: application binds tightest,
   then membership, then the arrow; the arrow is right-associative and a
   dependent domain is written as a binder.  A non-dependent arrow whose
   domain is itself a membership is printed with an explicit `_` binder so
   that it re-parses unambiguously. */
let rec print = (p: int, t: t): string => {
  let paren = (q, s) => q < p ? "(" ++ s ++ ")" : s;
  switch (t) {
  | Var(x) => x
  | Type => "type"
  | App(f, a) => paren(2, print(2, f) ++ " " ++ print(3, a))
  | Mem(a, b) => paren(1, print(2, a) ++ " : " ++ print(2, b))
  | Pi(x, a, b) =>
    let dependent = S.mem(x, free_vars(b));
    let dom =
      switch (a) {
      | _ when dependent => "(" ++ x ++ " : " ++ print(0, a) ++ ")"
      | Mem(_) => "(_ : " ++ print(0, a) ++ ")"
      | _ => print(1, a)
      };
    paren(0, dom ++ " -> " ++ print(0, b));
  };
};

let to_string: t => string = print(0);
