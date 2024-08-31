open Util;

module Value = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | NumLit(int);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | NumLit;

  //   let repr: t => string =
  //     (NumLit(n)) => "_" ++ string_of_int(n) ++ "_";
  let eq: (t, t) => bool = (a, b) => a == b;
};

module Expr = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | NumLit(int)
    | UnOp(unop, t)
    | BinOp(binop, t, t)
  and unop =
    | OpNeg
  and binop =
    | OpPlus
    | OpMinus
    | OpTimes;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | NumLit
    | UnOp
    | BinOp;

  let eq: (t, t) => bool = (a, b) => a == b;
};

module Judgement = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Value(Expr.t)
    | Eval(Expr.t, Value.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Value
    | Eval;
};

module Syntax_AL = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Value(Value.t)
    | Expr(Expr.t)
    | Judgement(Judgement.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Value
    | Expr
    | Judgement;
};

module Rule = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | V_NumLit
    | E_NumLit
    | E_Neg
    | E_Plus
    | E_Minus
    | E_Times;

  let prems_num =
    fun
    | V_NumLit => 0
    | E_NumLit => 1
    | E_Neg => 1
    | E_Plus => 2
    | E_Minus => 2
    | E_Times => 2;

  let all = [V_NumLit, E_NumLit, E_Neg, E_Plus, E_Minus, E_Times];
};
