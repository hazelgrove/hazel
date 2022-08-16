open Sexplib.Std;

/* TERM

   These data structures define the term structures on which
   the static and dynamic semantics of the language are based.
   Each sort has a corresponding U<Sort> module.

   The contained cls type lists the terms of that sort, and
   should be in 1-1 correspondence with the term type which
   is used to build composite terms.

   This is wrapped in a record type to associate a unique id
   with each term. These unique ids are the same as from the
   tile structure from the syntax module, as there is a 1-1
   correspondence between terms and tiles.

   TODO: add tests to check if there are forms and/or terms
   without correponding syntax classes */

[@deriving (show({with_path: false}), sexp, yojson)]
type parse_flag =
  | Whitespace // Not really an error
  | MalformedGrout // Should never happen
  | UnrecognizedTerm // Reminder to add term to MakeTerm
  | IncompleteTile; // Remove in future

let show_parse_flag: parse_flag => string =
  fun
  | Whitespace => "Whitespace"
  | MalformedGrout => "Malformed Grout"
  | UnrecognizedTerm => "Unrecognized Term"
  | IncompleteTile => "Incomplete Tile";

module UTyp = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | Int
    | Float
    | Bool
    | Arrow
    | Tuple
    | List
    | Parens;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag, Piece.t)
    | EmptyHole
    | MultiHole(list(Id.t), list(t))
    | Int
    | Float
    | Bool
    | List(t)
    | Arrow(t, t)
    | Tuple(list(Id.t), list(t))
    | Parens(t)
  and t = {
    id: Id.t,
    term,
  };

  let cls_of_term: term => cls =
    fun
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | MultiHole(_) => MultiHole
    | Int => Int
    | Float => Float
    | Bool => Bool
    | List(_) => List
    | Arrow(_) => Arrow
    | Tuple(_) => Tuple
    | Parens(_) => Parens;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid Type"
    | EmptyHole => "Empty Type Hole"
    | MultiHole => "Multi Type Hole"
    | Int
    | Float
    | Bool => "Base Type"
    | List => "List Type"
    | Arrow => "Function Type"
    | Tuple => "Product Type"
    | Parens => "Parenthesized Type Term";
};

module UPat = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | Wild
    | Int
    | Float
    | Bool
    | Triv
    | ListNil
    | Var
    | Tuple
    | Parens
    | TypeAnn;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag, Piece.t)
    | EmptyHole
    | MultiHole(list(Id.t), list(t))
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | Triv
    | ListNil
    | Var(Token.t)
    | Tuple(list(Id.t), list(t))
    | Parens(t)
    | TypeAnn(t, UTyp.t)
  and t = {
    id: Id.t,
    term,
  };

  let cls_of_term: term => cls =
    fun
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | MultiHole(_) => MultiHole
    | Wild => Wild
    | Int(_) => Int
    | Float(_) => Float
    | Bool(_) => Bool
    | Triv => Triv
    | ListNil => ListNil
    | Var(_) => Var
    | Tuple(_) => Tuple
    | Parens(_) => Parens
    | TypeAnn(_) => TypeAnn;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid Pattern"
    | EmptyHole => "Empty Pattern Hole"
    | MultiHole => "Multi Pattern Hole"
    | Wild => "Wildcard Pattern"
    | Int => "Integer Literal"
    | Float => "Float Literal"
    | Bool => "Boolean Literal"
    | Triv => "Trivial Literal. Pathetic, really."
    | ListNil => "List Literal"
    | Var => "Pattern Variable"
    | Tuple => "Tuple Pattern"
    | Parens => "Parenthesized Pattern"
    | TypeAnn => "Type Annotation";
};

module UExp = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bool =
    | And
    | Or;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_int =
    | Plus
    | Minus
    | Times
    | Divide
    | LessThan
    | GreaterThan
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_float =
    | Plus
    | Minus
    | Times
    | Divide
    | LessThan
    | GreaterThan
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin =
    | Int(op_int)
    | Float(op_float)
    | Bool(op_bool);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | Triv
    | Bool
    | Int
    | Float
    | ListLit
    | Fun
    | FunAnn
    | Tuple
    | Var
    | Let
    | LetAnn
    | Ap
    | If
    | Seq
    | Test
    | Parens
    | Cons
    | BinOp(op_bin);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag, Piece.t)
    | EmptyHole
    | MultiHole(list(Id.t), list(t))
    | Triv
    | Bool(bool)
    | Int(int)
    | Float(float)
    | ListLit(list(Id.t), list(t))
    | Fun(UPat.t, t)
    | FunAnn(UPat.t, UTyp.t, t) //TODO: deprecate
    | Tuple(list(Id.t), list(t))
    | Var(Token.t)
    | Let(UPat.t, t, t)
    | LetAnn(UPat.t, UTyp.t, t, t) //TODO: deprecate
    | Ap(t, t)
    //| ApBuiltin(Token.t, list(t))
    // Maybe ops with fn semantics should be builtins as well?
    | If(t, t, t)
    | Seq(t, t)
    | Test(t)
    | Parens(t)
    | Cons(t, t)
    | BinOp(op_bin, t, t)
  and t = {
    id: Id.t,
    term,
  };

  let cls_of_term: term => cls =
    fun
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | MultiHole(_) => MultiHole
    | Triv => Triv
    | Bool(_) => Bool
    | Int(_) => Int
    | Float(_) => Float
    | ListLit(_) => ListLit
    | Fun(_) => Fun
    | FunAnn(_) => FunAnn
    | Tuple(_) => Tuple
    | Var(_) => Var
    | Let(_) => Let
    | LetAnn(_) => LetAnn
    | Ap(_) => Ap
    | If(_) => If
    | Seq(_) => Seq
    | Test(_) => Test
    | Parens(_) => Parens
    | Cons(_) => Cons
    | BinOp(op, _, _) => BinOp(op);

  let show_op_bool: op_bool => string =
    fun
    | And => "Boolean Conjunction"
    | Or => "Boolean Disjunction";

  let show_op_int: op_int => string =
    fun
    | Plus => "Integer Addition"
    | Minus => "Integer Subtraction"
    | Times => "Integer Multiplication"
    | Divide => "Integer Division"
    | LessThan => "Integer Less Than"
    | GreaterThan => "Integer Greater Than"
    | Equals => "Integer Equality";

  let show_op_float: op_float => string =
    fun
    | Plus => "Float Addition"
    | Minus => "Float Subtraction"
    | Times => "Float Multiplication"
    | Divide => "Float Division"
    | LessThan => "Float Less Than"
    | GreaterThan => "Float Greater Than"
    | Equals => "Float Equality";

  let show_binop: op_bin => string =
    fun
    | Int(op) => show_op_int(op)
    | Float(op) => show_op_float(op)
    | Bool(op) => show_op_bool(op);

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid Expression"
    | EmptyHole => "Empty Expression Hole"
    | MultiHole => "Multi Expression Hole"
    | Triv => "Trivial Literal. Pathetic, really."
    | Bool => "Boolean Literal"
    | Int => "Integer Literal"
    | Float => "Float Literal"
    | ListLit => "List Literal"
    | Fun => "Function Literal"
    | FunAnn => "Annotated Function Literal"
    | Tuple => "Tuple Literal"
    | Var => "Variable Reference"
    | Let => "Let Expression"
    | LetAnn => "Annotated Let Expression"
    | Ap => "Function Application"
    | If => "If Expression"
    | Seq => "Sequence Expression"
    | Test => "Test (Effectful)"
    | Parens => "Parenthesized Expression"
    | Cons => "Cons"
    | BinOp(op) => show_binop(op);
};

/* Converts a syntactic type into a semantic type */
let rec utyp_to_ty: UTyp.t => Typ.t =
  utyp => {
    let ty =
      switch (utyp.term) {
      | Invalid(_)
      | MultiHole(_) => Typ.Typ_syntax.Unknown(Internal)
      | EmptyHole => Unknown(TypeHole)
      | Bool => Bool
      | Int => Int
      | Float => Float
      | Arrow(u1, u2) =>
        Arrow(
          Typ.to_syntax(utyp_to_ty(u1)),
          Typ.to_syntax(utyp_to_ty(u2)),
        )
      | Tuple(_, us) =>
        Prod(List.map(u => Typ.to_syntax(utyp_to_ty(u)), us))
      | List(u) => List(Typ.to_syntax(utyp_to_ty(u)))
      | Parens(u) => Typ.to_syntax(utyp_to_ty(u))
      };
    Typ.of_syntax(ty);
  };

type any =
  | Exp(UExp.t)
  | Pat(UPat.t)
  | Typ(UTyp.t)
  | Rul(unit) // TODO
  | Nul(unit)
  | Any(unit);
