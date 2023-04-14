open Sexplib.Std;

module rec Any: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp(UExp.t)
    | Pat(UPat.t)
    | Typ(UTyp.t)
    | TPat(UTPat.t)
    | Rul(URul.t)
    | Nul(unit)
    | Any(unit);

  let is_exp: t => option(UExp.t);
  let is_pat: t => option(UPat.t);
  let is_typ: t => option(UTyp.t);
  let to_string: t => string;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp(UExp.t)
    | Pat(UPat.t)
    | Typ(UTyp.t)
    | TPat(UTPat.t)
    | Rul(URul.t)
    | Nul(unit)
    | Any(unit);

  let is_exp: t => option(UExp.t) =
    fun
    | Exp(e) => Some(e)
    | _ => None;
  let is_pat: t => option(UPat.t) =
    fun
    | Pat(p) => Some(p)
    | _ => None;
  let is_typ: t => option(UTyp.t) =
    fun
    | Typ(t) => Some(t)
    | _ => None;

  let to_string = (a: t) =>
    switch (a) {
    | Exp(e) => UExp.to_string(e)
    | Pat(p) => UPat.to_string(p)
    | Typ(t) => UTyp.to_string(t)
    | TPat(tp) => UTPat.to_string(tp)
    | Rul(_) => "Rul"
    | Nul(_) => "Nul"
    | Any(_) => "Any"
    };
}
and UExp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un_bool =
    | Not;

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
    | Power
    | Divide
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equals
    | NotEquals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_float =
    | Plus
    | Minus
    | Times
    | Power
    | Divide
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equals
    | NotEquals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_string =
    | Concat
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un =
    | Int(op_un_int)
    | Bool(op_un_bool);

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
    | TyAlias
    | Ap
    | If
    | Seq
    | Test
    | Parens
    | Cons
    | ListConcat
    | UnOp(op_un)
    | BinOp(op_bin)
    | Match;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Triv
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t))
    | Tag(string)
    | Fun(UPat.t, t)
    | Tuple(list(t))
    | Var(Token.t)
    | Let(UPat.t, t, t)
    | TyAlias(UTPat.t, UTyp.t, t)
    | Ap(t, t)
    | If(t, t, t)
    | Seq(t, t)
    | Test(t)
    | Parens(t) // (
    | Cons(t, t)
    | ListConcat(t, t)
    | UnOp(op_un, t)
    | BinOp(op_bin, t, t)
    | Match(t, list((UPat.t, t)))
  and t = {
    // invariant: nonempty
    ids: list(Id.t),
    term,
  };

  let to_string: t => string;
  let bool_op_to_string: op_bin_bool => string;
  let int_op_to_string: op_bin_int => string;
  let float_op_to_string: op_bin_float => string;
  let string_op_to_string: op_bin_string => string;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un_bool =
    | Not;

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
    | Power
    | Divide
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equals
    | NotEquals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_float =
    | Plus
    | Minus
    | Times
    | Power
    | Divide
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equals
    | NotEquals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_string =
    | Concat
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un =
    | Int(op_un_int)
    | Bool(op_un_bool);

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
    | TyAlias
    | Ap
    | If
    | Seq
    | Test
    | Parens
    | Cons
    | ListConcat
    | UnOp(op_un)
    | BinOp(op_bin)
    | Match;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Triv
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t))
    | Tag(string)
    | Fun(UPat.t, t)
    | Tuple(list(t))
    | Var(Token.t)
    | Let(UPat.t, t, t)
    | TyAlias(UTPat.t, UTyp.t, t)
    | Ap(t, t)
    | If(t, t, t)
    | Seq(t, t)
    | Test(t)
    | Parens(t) // (
    | Cons(t, t)
    | ListConcat(t, t)
    | UnOp(op_un, t)
    | BinOp(op_bin, t, t)
    | Match(t, list((UPat.t, t)))
  and t = {
    // invariant: nonempty
    ids: list(Id.t),
    term,
  };

  let bool_op_to_string = (op: op_bin_bool): string => {
    switch (op) {
    | And => "&&"
    | Or => "\\/"
    };
  };

  let int_op_to_string = (op: op_bin_int): string => {
    switch (op) {
    | Plus => "+"
    | Minus => "-"
    | Times => "*"
    | Power => "**"
    | Divide => "/"
    | LessThan => "<"
    | LessThanOrEqual => "<="
    | GreaterThan => ">"
    | GreaterThanOrEqual => ">="
    | Equals => "=="
    | NotEquals => "!="
    };
  };

  let float_op_to_string = (op: op_bin_float): string => {
    switch (op) {
    | Plus => "+."
    | Minus => "-."
    | Times => "*."
    | Power => "**."
    | Divide => "/."
    | LessThan => "<."
    | LessThanOrEqual => "<=."
    | GreaterThan => ">."
    | GreaterThanOrEqual => ">=."
    | Equals => "==."
    | NotEquals => "!=."
    };
  };

  let string_op_to_string = (op: op_bin_string): string => {
    switch (op) {
    | Concat => "++"
    | Equals => "$=="
    };
  };

  let bin_op_to_string = (op: op_bin): string => {
    switch (op) {
    | Int(op) => int_op_to_string(op)
    | Float(op) => float_op_to_string(op)
    | Bool(op) => bool_op_to_string(op)
    | String(op) => string_op_to_string(op)
    };
  };

  let un_op_to_string = (op: op_un): string => {
    switch (op) {
    | Int(Minus) => "-"
    | Bool(Not) => "!"
    };
  };

  let rec to_string = (e: t): string => {
    let s = to_string;
    switch (e.term) {
    | MultiHole(es) => es |> List.map(Any.to_string) |> String.concat(" ")
    | EmptyHole => "?"
    | Triv => "()"
    | Bool(b) => b ? "true" : "false"
    | Invalid(token)
    | String(token)
    | Var(token)
    | Tag(token) => token
    | Int(i) => string_of_int(i)
    | Float(f) => string_of_float(f)
    | ListLit(es) => es |> List.map(s) |> String.concat(", ")
    | Fun(p, e) =>
      Printf.sprintf("fun %s ->\n %s", UPat.to_string(p), s(e))
    | Tuple(es) => es |> List.map(s) |> String.concat(", ")
    | Let(p, e1, e2) =>
      Printf.sprintf(
        "let %s = %s in\n %s",
        UPat.to_string(p),
        s(e1),
        s(e2),
      )
    | TyAlias(p, t, e) =>
      Printf.sprintf(
        "type %s = %s in\n %s",
        UTPat.to_string(p),
        UTyp.to_string(t),
        s(e),
      )
    | Ap(e1, e2) => s(e1) ++ "(" ++ s(e2) ++ ")"
    | If(e1, e2, e3) =>
      //TODO: linebreaks if over threshold
      Printf.sprintf("if %s then %s else %s", s(e1), s(e2), s(e3))
    | Seq(e1, e2) => Printf.sprintf("%s; %s", s(e1), s(e2))
    | Test(e) => Printf.sprintf("test %s end", s(e))
    | Parens(e) => Printf.sprintf("(%s)", s(e))
    | Cons(e1, e2) => Printf.sprintf("%s :: %s", s(e1), s(e2))
    | ListConcat(e1, e2) => Printf.sprintf("%s @ %s", s(e1), s(e2))
    | UnOp(op, e) => un_op_to_string(op) ++ s(e)
    | BinOp(op, e1, e2) =>
      s(e1) ++ " " ++ bin_op_to_string(op) ++ " " ++ s(e2)
    | Match(e, rules) =>
      let rule_to_string =
        List.map(((p, e)) => "| " ++ UPat.to_string(p) ++ " => " ++ s(e));
      Printf.sprintf(
        "case %s\n %s\nend",
        s(e),
        rules |> rule_to_string |> String.concat("\n"),
      );
    };
  };
}
and UPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | Triv
    | ListLit(list(t))
    | Tag(string)
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
  let to_string: t => string;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | Triv
    | ListLit(list(t))
    | Tag(string)
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
  let rec to_string = (p: t): string => {
    let s = to_string;
    switch (p.term) {
    | MultiHole(holes) =>
      holes |> List.map(Any.to_string) |> String.concat(" ")
    | EmptyHole => "?"
    | Wild => "_"
    | Triv => "()"
    | Invalid(token)
    | String(token)
    | Tag(token)
    | Var(token) => token
    | Int(i) => string_of_int(i)
    | Float(f) => string_of_float(f)
    | Bool(b) => string_of_bool(b)
    | Parens(p) => "(" ++ s(p) ++ ")"
    | Ap(p1, p2) => s(p1) ++ "(" ++ s(p2) ++ ")"
    | Cons(p1, p2) => s(p1) ++ " :: " ++ s(p2)
    | TypeAnn(p, t) => s(p) ++ " : " ++ UTyp.to_string(t)
    | Tuple(ps) => "(" ++ (ps |> List.map(s) |> String.concat(", ")) ++ ")"
    | ListLit(ps) => "[" ++ (ps |> List.map(s) |> String.concat(", ")) ++ "]"
    };
  };
}
and UTyp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Int
    | Float
    | Bool
    | String
    | List(t)
    | Var(string)
    | Tag(string)
    | Arrow(t, t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | USum(list(t))
  and t = {
    ids: list(Id.t),
    term,
  };
  let to_string: t => string;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Int
    | Float
    | Bool
    | String
    | List(t)
    | Var(string)
    | Tag(string)
    | Arrow(t, t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | USum(list(t))
  and t = {
    ids: list(Id.t),
    term,
  };
  let rec to_string = (t: t): string => {
    let s = to_string;
    switch (t.term) {
    | MultiHole(tms) => List.map(Any.to_string, tms) |> String.concat(" ")
    | EmptyHole => "?"
    | Int => "Int"
    | Float => "Float"
    | Bool => "Bool"
    | String => "String"
    | Invalid(token)
    | Var(token)
    | Tag(token) => token
    | Parens(t) => "(" ++ s(t) ++ ")"
    | List(t) => "[" ++ s(t) ++ "]"
    | Ap(t1, t2) => s(t1) ++ "(" ++ s(t2) ++ ")"
    | Arrow(t1, t2) => "(" ++ s(t1) ++ " -> " ++ s(t2) ++ ")"
    | Tuple(ts) => "(" ++ (List.map(s, ts) |> String.concat(", ")) ++ ")"
    | USum(ts) => "(" ++ (List.map(s, ts) |> String.concat(" + ")) ++ ")"
    };
  };
}
and UTPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Var(Token.t)
  and t = {
    ids: list(Id.t),
    term,
  };
  let to_string: t => string;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Var(Token.t)
  and t = {
    ids: list(Id.t),
    term,
  };
  let to_string = (t: t): string => {
    switch (t.term) {
    | MultiHole(tms) => List.map(Any.to_string, tms) |> String.concat(" ")
    | EmptyHole => "?"
    | Invalid(token)
    | Var(token) => token
    };
  };
}
and URul: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | Hole(list(Any.t))
    | Rules(UExp.t, list((UPat.t, UExp.t)))
  and t = {
    ids: list(Id.t),
    term,
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | Hole(list(Any.t))
    | Rules(UExp.t, list((UPat.t, UExp.t)))
  and t = {
    ids: list(Id.t),
    term,
  };
};
