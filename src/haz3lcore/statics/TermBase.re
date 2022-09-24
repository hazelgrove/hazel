open Sexplib.Std;

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

module rec Any: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp(UExp.t)
    | Pat(UPat.t)
    | Typ(UTyp.t)
    | Rul(URul.t)
    | Nul(unit)
    | Any(unit);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp(UExp.t)
    | Pat(UPat.t)
    | Typ(UTyp.t)
    | Rul(URul.t)
    | Nul(unit)
    | Any(unit);
}
and UExp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un_int =
    | Minus;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_bool =
    | And
    | Or;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_int =
    | Plus
    | Minus
    | Times
    | Divide
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_float =
    | Plus
    | Minus
    | Times
    | Divide
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_string =
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un =
    | Int(op_un_int);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin =
    | Int(op_bin_int)
    | Float(op_bin_float)
    | Bool(op_bin_bool)
    | String(op_bin_string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | Triv
    | Bool
    | Int
    | Float
    | String
    | ListLit
    | Tag
    | Fun
    | Tuple
    | Var
    | Let
    | Ap
    | If
    | Seq
    | Test
    | Parens
    | Cons
    | UnOp(op_un)
    | BinOp(op_bin)
    | Match;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Triv
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t))
    | Tag(string, list(t))
    | Fun(UPat.t, t)
    | Tuple(list(t))
    | Var(Token.t)
    | Let(UPat.t, t, t)
    // Let_pat(UPat.t, t)
    | Ap(t, t)
    | If(t, t, t)
    | Seq(t, t)
    | Test(t)
    | Parens(t) // (
    | Cons(t, t)
    | UnOp(op_un, t)
    | BinOp(op_bin, t, t)
    | Match(t, list((UPat.t, t)))
  and t = {
    // invariant: nonempty
    ids: list(Id.t),
    term,
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un_int =
    | Minus;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_bool =
    | And
    | Or;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_int =
    | Plus
    | Minus
    | Times
    | Divide
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_float =
    | Plus
    | Minus
    | Times
    | Divide
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_string =
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un =
    | Int(op_un_int);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin =
    | Int(op_bin_int)
    | Float(op_bin_float)
    | Bool(op_bin_bool)
    | String(op_bin_string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | Triv
    | Bool
    | Int
    | Float
    | String
    | ListLit
    | Tag
    | Fun
    | Tuple
    | Var
    | Let
    | Ap
    | If
    | Seq
    | Test
    | Parens
    | Cons
    | UnOp(op_un)
    | BinOp(op_bin)
    | Match;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Triv
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t))
    | Tag(string, list(t))
    | Fun(UPat.t, t)
    | Tuple(list(t))
    | Var(Token.t)
    | Let(UPat.t, t, t)
    // Let_pat(UPat.t, t)
    | Ap(t, t)
    | If(t, t, t)
    | Seq(t, t)
    | Test(t)
    | Parens(t) // (
    | Cons(t, t)
    | UnOp(op_un, t)
    | BinOp(op_bin, t, t)
    | Match(t, list((UPat.t, t)))
  and t = {
    // invariant: nonempty
    ids: list(Id.t),
    term,
  };
}
and UPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | Triv
    | ListLit(list(t))
    | Tag(string, list(t))
    | Cons(t, t)
    | Var(Token.t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | TypeAnn(t, UTyp.t)
  and t = {
    ids: list(Id.t),
    term,
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | Triv
    | ListLit(list(t))
    | Tag(string, list(t))
    | Cons(t, t)
    | Var(Token.t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | TypeAnn(t, UTyp.t)
  and t = {
    ids: list(Id.t),
    term,
  };
}
and UTyp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Int
    | Float
    | Bool
    | String
    | List(t)
    | Arrow(t, t)
    | Tuple(list(t))
    | Parens(t)
  and t = {
    ids: list(Id.t),
    term,
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Int
    | Float
    | Bool
    | String
    | List(t)
    | Arrow(t, t)
    | Tuple(list(t))
    | Parens(t)
  and t = {
    ids: list(Id.t),
    term,
  };
}
and URul: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
    | Hole(list(Any.t))
    | Rules(UExp.t, list((UPat.t, UExp.t)))
  and t = {
    ids: list(Id.t),
    term,
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
    | Hole(list(Any.t))
    | Rules(UExp.t, list((UPat.t, UExp.t)))
  and t = {
    ids: list(Id.t),
    term,
  };
};
