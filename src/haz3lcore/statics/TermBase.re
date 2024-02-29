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
}
and UExp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un_bool =
    | Not;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un_meta =
    | Unquote;

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
    | Meta(op_un_meta)
    | Int(op_un_int)
    | Bool(op_un_bool);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin =
    | Int(op_bin_int)
    | Float(op_bin_float)
    | Bool(op_bin_bool)
    | String(op_bin_string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type ap_direction =
    | Forward
    | Reverse;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | StaticErrorHole
    | DynamicErrorHole
    | FailedCast
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
    | Ap(ap_direction)
    | If
    | Seq
    | Test
    | Filter
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
    | StaticErrorHole(Id.t, t)
    | DynamicErrorHole(t, InvalidOperationError.t)
    | FailedCast(t, Typ.t, Typ.t)
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t))
    | Constructor(string)
    | Fun(UPat.t, t)
    | Tuple(list(t))
    | Var(Var.t)
    | Let(UPat.t, t, t)
    | FixF(UPat.t, t)
    | TyAlias(UTPat.t, UTyp.t, t)
    | Ap(ap_direction, t, t)
    | If(t, t, t)
    | Seq(t, t)
    | Test(t)
    | Filter(FilterAction.t, t, t)
    | Parens(t) // (
    | Cons(t, t)
    | ListConcat(t, t)
    | UnOp(op_un, t)
    | BinOp(op_bin, t, t)
    | BuiltinFun(string)
    | Match(t, list((UPat.t, t)))
  and t = {
    // invariant: nonempty
    ids: list(Id.t),
    copied: bool,
    term,
  };

  let bool_op_to_string: op_bin_bool => string;
  let int_op_to_string: op_bin_int => string;
  let float_op_to_string: op_bin_float => string;
  let string_op_to_string: op_bin_string => string;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un_bool =
    | Not;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un_meta =
    | Unquote;

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
    | Meta(op_un_meta)
    | Int(op_un_int)
    | Bool(op_un_bool);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin =
    | Int(op_bin_int)
    | Float(op_bin_float)
    | Bool(op_bin_bool)
    | String(op_bin_string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type ap_direction =
    | Forward
    | Reverse;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | StaticErrorHole
    | DynamicErrorHole
    | FailedCast
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
    | Ap(ap_direction)
    | If
    | Seq
    | Test
    | Filter
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
    | StaticErrorHole(Id.t, t)
    | DynamicErrorHole(t, InvalidOperationError.t)
    | FailedCast(t, Typ.t, Typ.t)
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t))
    | Constructor(string)
    | Fun(UPat.t, t) // TODO: Add option(Var.t) name field to end; Add optional closure to function
    | Tuple(list(t))
    | Var(Var.t)
    | Let(UPat.t, t, t)
    | FixF(UPat.t, t) // DONE [CHECK WITH SOMEONE THAT I GOT THE STATIC SEMANTICS RIGHT]
    | TyAlias(UTPat.t, UTyp.t, t)
    | Ap(ap_direction, t, t) // note: function is always first then argument; even in pipe mode
    | If(t, t, t)
    | Seq(t, t)
    | Test(t)
    | Filter(FilterAction.t, t, t) // TODO: Change to reflect DHExp
    // TODO: Add closures
    | Parens(t)
    | Cons(t, t)
    | ListConcat(t, t)
    | UnOp(op_un, t)
    | BinOp(op_bin, t, t)
    | BuiltinFun(string) /// Doesn't currently have a distinguishable syntax...
    | Match(t, list((UPat.t, t)))
  // TODO: Add Casts
  and t = {
    // invariant: nonempty
    ids: list(Id.t), // > DHEXP // Multiple ids?? // Add source??
    copied: bool,
    term,
  };

  let bool_op_to_string = (op: op_bin_bool): string => {
    switch (op) {
    | And => "&&"
    | Or => "||"
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
    | ListLit(list(t))
    | Constructor(string)
    | Cons(t, t)
    | Var(Var.t)
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
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | ListLit(list(t))
    | Constructor(string)
    | Cons(t, t)
    | Var(Var.t)
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
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Int
    | Float
    | Bool
    | String
    | List(t)
    | Var(string)
    | Constructor(string)
    | Arrow(t, t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | Sum(list(variant))
  and variant =
    | Variant(Constructor.t, list(Id.t), option(t))
    | BadEntry(t)
  and t = {
    ids: list(Id.t),
    term,
  };
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
    | Constructor(string)
    | Arrow(t, t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | Sum(list(variant))
  and variant =
    | Variant(Constructor.t, list(Id.t), option(t))
    | BadEntry(t)
  and t = {
    ids: list(Id.t),
    term,
  };
}
and UTPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Var(TypVar.t)
  and t = {
    ids: list(Id.t),
    term,
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Var(TypVar.t)
  and t = {
    ids: list(Id.t),
    term,
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
