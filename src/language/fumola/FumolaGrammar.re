/* The shape of Fumola terms as they appear in the editor, polymorphic in the
   annotation carried at each node (ids, in the editor).

   This is the M1 subset of Fumola, not all of it: the core expression forms
   plus the adapton operations. Fumola's surface is Motoko-derived and its
   grammar (crates/fumola_parser/src/lib/parser.lalrpop, 834 lines) also
   covers objects, classes, actors, generics, candid, async and quoted ASTs.
   Those are deliberately absent; see docs/fumola-tiles-design.md.

   The type is generic in the host expression it can embed: 'h is whatever
   language is dropping a term into a Fumola program, and Hazel instantiates
   it with its own Grammar.exp_t. That parameter is what lets `hazel … end`
   nest a real Hazel subtree -- with statics, completion and type errors --
   anywhere inside a Fumola program, which is the thing the livelit could not
   do: its model had one `input` slot at the boundary. It is a parameter
   rather than a direct reference because Hazel's Grammar already refers to
   this module, and naming Grammar here would close the cycle.

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
  type exp_term('h, 'a) =
    | Hole(hole('h, 'a))
    /* A host-language term embedded in a Fumola program. See `hazel … end`
       in Form.fumola_get; the printer is handed a function that renders one
       as Fumola source. */
    | Hazel('h)
    /* --- ExpNullary, ExpPlain: the atoms --- */
    | Var(string)
    | Lit(lit)
    | Paren(exp('h, 'a))
    | Tuple(list(exp('h, 'a))) /* "(" e, e ")" */
    | Block(list(dec('h, 'a))) /* "{" d; d "}" -- see FumolaPrint.exp_nest */
    | Array(bool, list(exp('h, 'a))) /* "[" e, e "]"; true for `[var …]` */
    | QuotedId(string) /* `t -- a quoted name, the `#` of AdaptonNav dims */
    | Prim(string) /* prim "adaptonNow" */
    /* --- ExpPost: tighter than the unary forms --- */
    | Ap(exp('h, 'a), exp('h, 'a)) /* e1 e2; the argument is an atom */
    | Proj(exp('h, 'a), string) /* e.x and e.0 alike */
    | Index(exp('h, 'a), exp('h, 'a)) /* e[i] */
    | Bang(exp('h, 'a)) /* e! */
    /* --- ExpUn --- */
    | Variant(string, option(exp('h, 'a))) /* #tag and #tag e */
    | Opt(exp('h, 'a)) /* ? e */
    | Un(un, exp('h, 'a))
    | Not(exp('h, 'a))
    | Unquote(exp('h, 'a)) /* ~ e */
    /* --- ExpBin0 .. ExpBin9 --- */
    | Bin(exp('h, 'a), bin, exp('h, 'a))
    | Rel(exp('h, 'a), rel, exp('h, 'a))
    | And(exp('h, 'a), exp('h, 'a))
    | Or(exp('h, 'a), exp('h, 'a))
    /* --- ExpNonDec: looser than every binary operator, which is why each
       of these needs parentheses to sit inside one. `force x + 1` is a
       syntax error in Fumola, not a misparse. --- */
    | If(exp('h, 'a), exp('h, 'a), option(exp('h, 'a)))
    | Switch(exp('h, 'a), list(case('h, 'a)))
    | Assert(exp('h, 'a))
    | Ignore(exp('h, 'a))
    | Return(option(exp('h, 'a)))
    /* --- the adapton core, at that same level --- */
    | Thunk(list(dec('h, 'a))) /* thunk { … } */
    | Force(exp('h, 'a)) /* force e */
    | Put(exp('h, 'a), exp('h, 'a)) /* e := e */
    | Get(exp('h, 'a)) /* @ e */
    | DoPutForce(exp('h, 'a), exp('h, 'a)) /* do @ e1 e2 */
    | DoNav(nav, exp('h, 'a), exp('h, 'a), list(dec('h, 'a))) /* do goto d e { … } */
  and exp('h, 'a) = W.t(exp_term('h, 'a), 'a)
  and case('h, 'a) = {
    pat: pat('h, 'a),
    body: exp('h, 'a),
  }
  and dec_term('h, 'a) =
    | DHole(hole('h, 'a))
    | DExp(exp('h, 'a))
    | DLet(pat('h, 'a), exp('h, 'a)) /* let p = e */
    | DVar(pat('h, 'a), exp('h, 'a)) /* var p = e */
    | DFunc(string, pat('h, 'a), list(dec('h, 'a))) /* func f p { … } */
    /* Dec::LetImport: `import P "path"`, with the `=` that Fumola's grammar
       marks as sugar and accepts either way.  We always print it, because
       the tile that builds one is shaped like `let`. */
    | DImport(pat('h, 'a), exp('h, 'a))
  and dec('h, 'a) = W.t(dec_term('h, 'a), 'a)
  and pat_term('h, 'a) =
    | PHole(hole('h, 'a))
    | PVar(string)
    | PWild /* _ */
    | PLit(lit)
    | PParen(pat('h, 'a))
    | PTuple(list(pat('h, 'a)))
    | PVariant(string, option(pat('h, 'a)))
    | POpt(pat('h, 'a)) /* ? p */
  and pat('h, 'a) = W.t(pat_term('h, 'a), 'a)
  and hole('h, 'a) =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(exp('h, 'a)));
};

module M_Annotated = M(Annotated);
include M_Annotated;

/* `f_h` maps the embedded host terms; Hazel passes its own annotation map. */


let rec map_annotation:
  type h k a b. ((h => k, a => b), exp(h, a)) => exp(k, b) =
  ((f_h, f) as fs, e) => {
    let go = x => map_annotation(fs, x);
    let go_ds = ds => List.map(map_dec_annotation(fs), ds);
    let term: exp_term(k, b) =
      switch (e.term) {
      | Hole(h) => Hole(map_hole_annotation(fs, h))
      | Hazel(h) => Hazel(f_h(h))
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
        Switch(go(e), List.map(map_case_annotation(fs), cs))
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

and map_dec_annotation:
  type h k a b. ((h => k, a => b), dec(h, a)) => dec(k, b) =
  ((_, f) as fs, d) => {
    let term: dec_term(k, b) =
      switch (d.term) {
      | DHole(h) => DHole(map_hole_annotation(fs, h))
      | DExp(e) => DExp(map_annotation(fs, e))
      | DLet(p, e) =>
        DLet(map_pat_annotation(fs, p), map_annotation(fs, e))
      | DVar(p, e) =>
        DVar(map_pat_annotation(fs, p), map_annotation(fs, e))
      | DFunc(n, p, ds) =>
        DFunc(
          n,
          map_pat_annotation(fs, p),
          List.map(map_dec_annotation(fs), ds),
        )
      | DImport(p, e) =>
        DImport(map_pat_annotation(fs, p), map_annotation(fs, e))
      };
    {
      term,
      annotation: f(d.annotation),
    };
  }

and map_case_annotation:
  type h k a b. ((h => k, a => b), case(h, a)) => case(k, b) =
  (fs, c) => {
    pat: map_pat_annotation(fs, c.pat),
    body: map_annotation(fs, c.body),
  }

and map_pat_annotation:
  type h k a b. ((h => k, a => b), pat(h, a)) => pat(k, b) =
  ((_, f) as fs, p) => {
    let go = x => map_pat_annotation(fs, x);
    let term: pat_term(k, b) =
      switch (p.term) {
      | PHole(h) => PHole(map_hole_annotation(fs, h))
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

and map_hole_annotation:
  type h k a b. ((h => k, a => b), hole(h, a)) => hole(k, b) =
  (fs, h) =>
    switch (h) {
    | Invalid(s) => Invalid(s)
    | EmptyHole => EmptyHole
    | MultiHole(es) => MultiHole(List.map(map_annotation(fs), es))
    };
