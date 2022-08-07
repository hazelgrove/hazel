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

module UTyp = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | Int
    | Float
    | Bool
    | Arrow
    | Prod
    | Parens;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(Piece.t)
    | EmptyHole
    | Int
    | Float
    | Bool
    | Arrow(t, t)
    | Prod(t, t)
    | Parens(t)
  and t = {
    id: Id.t,
    term,
  };

  let cls_of_term: term => cls =
    fun
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | Int => Int
    | Float => Float
    | Bool => Bool
    | Arrow(_) => Arrow
    | Prod(_) => Prod
    | Parens(_) => Parens;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid Type"
    | EmptyHole => "Empty Type Hole"
    | Int
    | Float
    | Bool => "Concrete Type"
    | Arrow => "Arrow Type"
    | Prod => "Product Type"
    | Parens => "Parenthesized Type Term";
};

module UPat = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | Wild
    | Int
    | Float
    | Bool
    | Var
    | Pair
    | Parens
    | TypeAnn;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(Piece.t)
    | EmptyHole
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | Var(Token.t)
    | Pair(t, t)
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
    | Wild => Wild
    | Int(_) => Int
    | Float(_) => Float
    | Bool(_) => Bool
    | Var(_) => Var
    | Pair(_) => Pair
    | Parens(_) => Parens
    | TypeAnn(_) => TypeAnn;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid Pattern"
    | EmptyHole => "Empty Pattern Hole"
    | Wild => "Wildcard Pattern"
    | Int => "Integer Literal"
    | Float => "Float Literal"
    | Bool => "Boolean Literal"
    | Var => "Pattern Variable"
    | Pair => "Pair Pattern"
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
    | Bool
    | Int
    | Float
    | Fun
    | FunAnn
    | Pair
    | Var
    | Let
    | LetAnn
    | Ap
    | If
    | Seq
    | Test
    | Parens
    | BinOp(op_bin);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(Piece.t) //everything? text? keyword?
    //| InvalidSegment(Segment.t)
    | EmptyHole
    | Bool(bool)
    | Int(int)
    | Float(float)
    | Fun(UPat.t, t)
    | FunAnn(UPat.t, UTyp.t, t)
    | Pair(t, t)
    | Var(Token.t)
    | Let(UPat.t, t, t)
    | LetAnn(UPat.t, UTyp.t, t, t)
    | Ap(t, t)
    //| ApBuiltin(Token.t, list(t))
    // maybe everything with fn semantics should be a builtin e.g. plus??
    | If(t, t, t)
    | Seq(t, t)
    | Test(t)
    | Parens(t)
    | BinOp(op_bin, t, t)
  and t = {
    id: Id.t,
    term,
  };

  let cls_of_term: term => cls =
    fun
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | Bool(_) => Bool
    | Int(_) => Int
    | Float(_) => Float
    | Fun(_) => Fun
    | FunAnn(_) => FunAnn
    | Pair(_) => Pair
    | Var(_) => Var
    | Let(_) => Let
    | LetAnn(_) => LetAnn
    | Ap(_) => Ap
    | If(_) => If
    | Seq(_) => Seq
    | Test(_) => Test
    | Parens(_) => Parens
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
    | Bool => "Boolean Literal"
    | Int => "Integer Literal"
    | Float => "Float Literal"
    | Fun => "Function Literal"
    | FunAnn => "Annotated Function Literal"
    | Pair => "Pair Literal"
    | Var => "Variable Reference"
    | Let => "Let Expression"
    | LetAnn => "Annotated Let Expression"
    | Ap => "Function Application"
    | If => "If Expression"
    | Seq => "Sequence Expression"
    | Test => "Test (Effectful)"
    | Parens => "Parenthesized Expression"
    | BinOp(op) => show_binop(op);
};

/* Converts a syntactic type into a semantic type */
let rec utyp_to_ty: UTyp.t => Typ.t =
  utyp =>
    switch (utyp.term) {
    | Invalid(_)
    | EmptyHole => Unknown(TypeHole)
    | Int => Int
    | Float => Float
    | Bool => Bool
    | Arrow(u1, u2) => Arrow(utyp_to_ty(u1), utyp_to_ty(u2))
    | Prod(u1, u2) => Prod(utyp_to_ty(u1), utyp_to_ty(u2))
    | Parens(u1) => utyp_to_ty(u1)
    };
