/* The shape of Fumola terms as they appear in the editor, polymorphic in the
   annotation carried at each node (ids, in the editor).

   This is the M1 subset of Fumola, not all of it: the core expression forms
   plus the adapton operations. Fumola's surface is Motoko-derived and its
   grammar (crates/fumola_parser/src/lib/parser.lalrpop, 834 lines) also
   covers objects, classes, actors, generics, candid, async and quoted ASTs.
   Those are deliberately absent; see docs/fumola-tiles-design.md.

   Constructors mirror the productions of that grammar, and the comments name
   the production each one comes from, because the printer's correctness is
   judged against it. Where this departs from the Rust AST it is to keep the
   editor's job simple: numeric literals stay as text (Fumola keeps them that
   way too), and projection does not distinguish `e.0` from `e.x`, since the
   printer only has to put the text back. */

open Util;

module M =
       (
         W: {
           [@deriving (show({with_path: false}), sexp, yojson, eq)]
           type t('a, 'b);
         },
       ) => {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type lit =
    | Nat(string)
    | Float(string)
    | Char(string)
    | Text(string)
    | Bool(bool)
    | Null
    | Unit;

  /* ExpUn: the unary operators, spelled as Fumola spells them. */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type un =
    | Pos /* + */
    | Neg /* - */
    | BitNot; /* ^ */

  /* ExpBin3 through ExpBin9. The grouping here follows the parser's
     precedence chain, not arithmetic intuition: `|` binds *tighter* than
     `+`, which is the opposite of C. See FumolaPrint.prec_of_bin. */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type bin =
    | Add /* + */
    | Sub /* - */
    | Mul /* * */
    | Div /* / */
    | Mod /* % */
    | Pow /* ** */
    | Cat /* # */
    | BitOr /* | */
    | BitAnd /* & */
    | Xor /* ^ */
    | ShL /* << */
    | ShR /* >> */
    | RotL /* <<> */
    | RotR; /* <>> */

  /* ExpBin2. */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type rel =
    | Eq
    | Neq
    | Lt
    | Gt
    | Le
    | Ge;

  /* AdaptonNav: `goto` and `within`, which differ only in the keyword. */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type nav =
    | Goto
    | Within;

  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type exp_term('a) =
    | Hole(hole('a))
    /* --- ExpNullary, ExpPlain: the atoms --- */
    | Var(string)
    | Lit(lit)
    | Paren(exp('a))
    | Tuple(list(exp('a))) /* "(" e, e ")" */
    | Block(list(dec('a))) /* "{" d; d "}" -- see FumolaPrint.exp_nest */
    | Array(bool, list(exp('a))) /* "[" e, e "]"; true for `[var …]` */
    | QuotedId(string) /* `t -- a quoted name, the `#` of AdaptonNav dims */
    | Prim(string) /* prim "adaptonNow" */
    /* --- ExpPost: tighter than the unary forms --- */
    | Ap(exp('a), exp('a)) /* e1 e2; the argument is an atom */
    | Proj(exp('a), string) /* e.x and e.0 alike */
    | Index(exp('a), exp('a)) /* e[i] */
    | Bang(exp('a)) /* e! */
    /* --- ExpUn --- */
    | Variant(string, option(exp('a))) /* #tag and #tag e */
    | Opt(exp('a)) /* ? e */
    | Un(un, exp('a))
    | Not(exp('a))
    | Unquote(exp('a)) /* ~ e */
    /* --- ExpBin0 .. ExpBin9 --- */
    | Bin(exp('a), bin, exp('a))
    | Rel(exp('a), rel, exp('a))
    | And(exp('a), exp('a))
    | Or(exp('a), exp('a))
    /* --- ExpNonDec: looser than every binary operator, which is why each
       of these needs parentheses to sit inside one. `force x + 1` is a
       syntax error in Fumola, not a misparse. --- */
    | If(exp('a), exp('a), option(exp('a)))
    | Switch(exp('a), list(case('a)))
    | Assert(exp('a))
    | Ignore(exp('a))
    | Return(option(exp('a)))
    /* --- the adapton core, at that same level --- */
    | Thunk(list(dec('a))) /* thunk { … } */
    | Force(exp('a)) /* force e */
    | Put(exp('a), exp('a)) /* e := e */
    | Get(exp('a)) /* @ e */
    | DoPutForce(exp('a), exp('a)) /* do @ e1 e2 */
    | DoNav(nav, exp('a), exp('a), list(dec('a))) /* do goto d e { … } */
  and exp('a) = W.t(exp_term('a), 'a)
  and case('a) = {
    pat: pat('a),
    body: exp('a),
  }
  and dec_term('a) =
    | DHole(hole('a))
    | DExp(exp('a))
    | DLet(pat('a), exp('a)) /* let p = e */
    | DVar(pat('a), exp('a)) /* var p = e */
    | DFunc(string, pat('a), list(dec('a))) /* func f p { … } */
  and dec('a) = W.t(dec_term('a), 'a)
  and pat_term('a) =
    | PHole(hole('a))
    | PVar(string)
    | PWild /* _ */
    | PLit(lit)
    | PParen(pat('a))
    | PTuple(list(pat('a)))
    | PVariant(string, option(pat('a)))
    | POpt(pat('a)) /* ? p */
  and pat('a) = W.t(pat_term('a), 'a)
  and hole('a) =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(exp('a)));
};

module M_Annotated = M(Annotated);
include M_Annotated;


let rec map_annotation: type a b. (a => b, exp(a)) => exp(b) =
  (f, e) => {
    let go = x => map_annotation(f, x);
    let go_ds = ds => List.map(map_dec_annotation(f), ds);
    let term: exp_term(b) =
      switch (e.term) {
      | Hole(h) => Hole(map_hole_annotation(f, h))
      | Var(x) => Var(x)
      | Lit(l) => Lit(l)
      | Paren(e) => Paren(go(e))
      | Tuple(es) => Tuple(List.map(go, es))
      | Block(ds) => Block(go_ds(ds))
      | Array(v, es) => Array(v, List.map(go, es))
      | QuotedId(x) => QuotedId(x)
      | Prim(s) => Prim(s)
      | Ap(a, b) => Ap(go(a), go(b))
      | Proj(e, x) => Proj(go(e), x)
      | Index(a, b) => Index(go(a), go(b))
      | Bang(e) => Bang(go(e))
      | Variant(t, e) => Variant(t, Option.map(go, e))
      | Opt(e) => Opt(go(e))
      | Un(u, e) => Un(u, go(e))
      | Not(e) => Not(go(e))
      | Unquote(e) => Unquote(go(e))
      | Bin(a, o, b) => Bin(go(a), o, go(b))
      | Rel(a, o, b) => Rel(go(a), o, go(b))
      | And(a, b) => And(go(a), go(b))
      | Or(a, b) => Or(go(a), go(b))
      | If(c, t, f') => If(go(c), go(t), Option.map(go, f'))
      | Switch(e, cs) =>
        Switch(go(e), List.map(map_case_annotation(f), cs))
      | Assert(e) => Assert(go(e))
      | Ignore(e) => Ignore(go(e))
      | Return(e) => Return(Option.map(go, e))
      | Thunk(ds) => Thunk(go_ds(ds))
      | Force(e) => Force(go(e))
      | Put(a, b) => Put(go(a), go(b))
      | Get(e) => Get(go(e))
      | DoPutForce(a, b) => DoPutForce(go(a), go(b))
      | DoNav(n, d, e, ds) => DoNav(n, go(d), go(e), go_ds(ds))
      };
    {
      term,
      annotation: f(e.annotation),
    };
  }

and map_dec_annotation: type a b. (a => b, dec(a)) => dec(b) =
  (f, d) => {
    let term: dec_term(b) =
      switch (d.term) {
      | DHole(h) => DHole(map_hole_annotation(f, h))
      | DExp(e) => DExp(map_annotation(f, e))
      | DLet(p, e) => DLet(map_pat_annotation(f, p), map_annotation(f, e))
      | DVar(p, e) => DVar(map_pat_annotation(f, p), map_annotation(f, e))
      | DFunc(n, p, ds) =>
        DFunc(
          n,
          map_pat_annotation(f, p),
          List.map(map_dec_annotation(f), ds),
        )
      };
    {
      term,
      annotation: f(d.annotation),
    };
  }

and map_case_annotation: type a b. (a => b, case(a)) => case(b) =
  (f, c) => {
    pat: map_pat_annotation(f, c.pat),
    body: map_annotation(f, c.body),
  }

and map_pat_annotation: type a b. (a => b, pat(a)) => pat(b) =
  (f, p) => {
    let go = x => map_pat_annotation(f, x);
    let term: pat_term(b) =
      switch (p.term) {
      | PHole(h) => PHole(map_hole_annotation(f, h))
      | PVar(x) => PVar(x)
      | PWild => PWild
      | PLit(l) => PLit(l)
      | PParen(p) => PParen(go(p))
      | PTuple(ps) => PTuple(List.map(go, ps))
      | PVariant(t, p) => PVariant(t, Option.map(go, p))
      | POpt(p) => POpt(go(p))
      };
    {
      term,
      annotation: f(p.annotation),
    };
  }

and map_hole_annotation: type a b. (a => b, hole(a)) => hole(b) =
  (f, h) =>
    switch (h) {
    | Invalid(s) => Invalid(s)
    | EmptyHole => EmptyHole
    | MultiHole(es) => MultiHole(List.map(map_annotation(f), es))
    };
