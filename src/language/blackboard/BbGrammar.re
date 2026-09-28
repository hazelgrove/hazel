open Util;

/* The shape of Blackboard terms as they appear in the editor, polymorphic in
   the annotation carried at each node (ids, in the editor).  This mirrors
   the tiles of Form.re one for one; reading these as terms of the core
   logic is the job of Bb.to_kernel.  In particular the dependent arrow is
   not a constructor here: `(x : A) -> B` is an Arrow whose domain is a
   parenthesized membership, and only the kernel reading turns it into a
   binder. */
module M =
       (
         W: {
           [@deriving (show({with_path: false}), sexp, yojson, eq)]
           type t('a, 'b);
         },
       ) => {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type term('a) =
    | Hole(hole('a))
    | Var(Var.t)
    | Type
    | Parens(t('a))
    | Mem(t('a), t('a)) /* t : T; also a signature entry when t is a name */
    | Arrow(t('a), t('a)) /* A -> B, or (x : A) -> B */
    | Ap(t('a), t('a)) /* f(a); f(a, b) is Ap(f, Tuple([a, b])) */
    | Tuple(list(t('a))) /* commas; meaningful only as an argument */
    | Seq(list(t('a))) /* semicolons: signature entries, document blocks */
    | Assume(t('a), t('a)) /* assume entries by tactic end */
    | Construct(t('a), t('a)) /* construct entries by tactic end */
  and t('a) = W.t(term('a), 'a)
  and hole('a) =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(t('a)));
};

module M_Annotated = M(Annotated);
include M_Annotated;


let rec map_annotation: type a b. (a => b, t(a)) => t(b) =
  (f, e) => {
    let go = x => map_annotation(f, x);
    let term: term(b) =
      switch (e.term) {
      | Hole(h) => Hole(map_hole_annotation(f, h))
      | Var(x) => Var(x)
      | Type => Type
      | Parens(t) => Parens(go(t))
      | Mem(a, b) => Mem(go(a), go(b))
      | Arrow(a, b) => Arrow(go(a), go(b))
      | Ap(a, b) => Ap(go(a), go(b))
      | Tuple(ts) => Tuple(List.map(go, ts))
      | Seq(ts) => Seq(List.map(go, ts))
      | Assume(a, b) => Assume(go(a), go(b))
      | Construct(a, b) => Construct(go(a), go(b))
      };
    {
      term,
      annotation: f(e.annotation),
    };
  }

and map_hole_annotation: type a b. (a => b, hole(a)) => hole(b) =
  (f, h) =>
    switch (h) {
    | Invalid(s) => Invalid(s)
    | EmptyHole => EmptyHole
    | MultiHole(ts) => MultiHole(List.map(x => map_annotation(f, x), ts))
    };
