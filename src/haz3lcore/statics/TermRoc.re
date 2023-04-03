open Sexplib.Std;

module rec UExp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un_int =
    | Minus;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_bool =
    | And
    | Or;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_int_float =
    | Plus
    | Minus
    | Times
    | Power
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
    | Int_Float(op_bin_int_float)
    | Bool(op_bin_bool)
    | String(op_bin_string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Bool
    | Int
    | Float
    | String
    | ListLit
    // | Tag
    | Fun
    | Record
    // | Tuple
    | Var
    | Assign
    | Ap
    | If
    | Seq
    | SeqList
    | SeqIndent
    | SeqMatchIndent
    | SeqNoBreak
    | Expect
    | Parens
    | UnOp(op_un)
    | BinOp(op_bin)
    | Match
    | TypeAnn;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t))
    // | Tag(string)
    | Fun(UPat.t, t)
    | Record(list(t))
    // | Tuple(list(t))
    | Var(Token.t)
    | Assign(UPat.t, t)
    | Ap(t, t)
    | If(t, t, t)
    | Seq(t, t)
    | SeqList(list(t))
    | SeqIndent(t, t)
    | SeqMatchIndent(t, t)
    | SeqNoBreak(list(t))
    | Expect(t)
    | Parens(t)
    | UnOp(op_un, t)
    | BinOp(op_bin, t, t)
    | Match(t, list((UPat.t, t)))
    | TypeAnn(Token.t, UTyp.t);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un_int =
    | Minus;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_bool =
    | And
    | Or;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_int_float =
    | Plus
    | Minus
    | Times
    | Power
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
    | Int_Float(op_bin_int_float)
    | Bool(op_bin_bool)
    | String(op_bin_string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Bool
    | Int
    | Float
    | String
    | ListLit
    // | Tag
    | Fun
    | Record
    // | Tuple
    | Var
    | Assign
    | Ap
    | If
    | Seq
    | SeqList
    | SeqIndent
    | SeqMatchIndent
    | SeqNoBreak
    | Expect
    | Parens
    | UnOp(op_un)
    | BinOp(op_bin)
    | Match
    | TypeAnn;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t))
    // | Tag(string)
    | Fun(UPat.t, t)
    | Record(list(t))
    // | Tuple(list(t))
    | Var(Token.t)
    | Assign(UPat.t, t)
    | Ap(t, t)
    | If(t, t, t)
    | Seq(t, t)
    | SeqList(list(t))
    | SeqIndent(t, t)
    | SeqMatchIndent(t, t)
    | SeqNoBreak(list(t))
    | Expect(t)
    | Parens(t)
    | UnOp(op_un, t)
    | BinOp(op_bin, t, t)
    | Match(t, list((UPat.t, t)))
    | TypeAnn(Token.t, UTyp.t);
}
and UPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Wild
    | Rest
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | ListLit(list(t))
    // | Tag(string)
    | Var(Token.t)
    | Record(list(t))
    // | Tuple(list(t))
    | Parens(t);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Wild
    | Rest
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | ListLit(list(t))
    // | Tag(string)
    | Var(Token.t)
    | Record(list(t))
    // | Tuple(list(t))
    | Parens(t);
}
and UTyp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Int
    | Float
    | Bool
    | String
    | List(t)
    | Var(string)
    | Arrow(t, t)
    | Record(list(t))
    // | Tuple(list(t))
    | Parens(t);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Int
    | Float
    | Bool
    | String
    | List(t)
    | Var(string)
    | Arrow(t, t)
    | Record(list(t))
    // | Tuple(list(t))
    | Parens(t);
};
