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
