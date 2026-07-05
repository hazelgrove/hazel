open Language;
open Haz3lcore;

let parens = value => "(" ++ value ++ ")";

let is_integer_float = value => value == Float.round(value);

let is_float_pi = value => abs_float(value -. Float.pi) < 0.000001;

let string_of_float_literal = value =>
  is_integer_float(value)
    ? string_of_int(int_of_float(value)) : string_of_float(value);

let op_to_algebrite =
  fun
  | Operators.Int(Operators.Plus)
  | Nat(Plus)
  | SInt(Plus)
  | Float(Plus) => Some("+")
  | Operators.Int(Operators.Minus)
  | SInt(Minus)
  | Float(Minus) => Some("-")
  | Operators.Int(Operators.Times)
  | Nat(Times)
  | SInt(Times)
  | Float(Times) => Some("*")
  | Operators.Int(Operators.Divide)
  | Nat(Divide)
  | SInt(Divide)
  | Float(Divide) => Some("/")
  | Operators.Int(Operators.Power)
  | Nat(Power)
  | SInt(Power)
  | Float(Power) => Some("^")
  | _ => None;

let function_name = exp => {
  let exp = exp |> DHExp.strip_ascriptions;
  switch (exp.term) {
  | Var(("sin" | "cos" | "tan") as name)
  | BuiltinFun(("sin" | "cos" | "tan") as name) => Some(name)
  | _ => None
  };
};

let rec serialize_for_algebrite = (exp: Exp.t): option(string) => {
  let exp = exp |> DHExp.strip_ascriptions;
  switch (exp.term) {
  | Parens(inner)
  | Asc(inner, _) => serialize_for_algebrite(inner)
  | Atom(Int(value))
  | Atom(Nat(value)) => Some(Bigint.to_string(value))
  | Atom(SInt(value)) => Some(string_of_int(value))
  | Atom(Float(value)) when is_float_pi(value) => Some("pi")
  | Atom(Float(value)) => Some(string_of_float_literal(value))
  | Var(name) => Some(name)
  | UnOp(
      Operators.Int(Operators.Minus) | SInt(Minus) | Float(Minus),
      inner,
    ) =>
    serialize_for_algebrite(inner)
    |> Option.map(value => "(-" ++ value ++ ")")
  | BinOp(op, left, right) =>
    switch (
      op_to_algebrite(op),
      serialize_for_algebrite(left),
      serialize_for_algebrite(right),
    ) {
    | (Some(op), Some(left), Some(right)) =>
      Some(parens(left ++ " " ++ op ++ " " ++ right))
    | _ => None
    }
  | Ap(Operators.Forward, fn, arg) =>
    switch (function_name(fn), serialize_for_algebrite(arg)) {
    | (Some(name), Some(arg)) => Some(name ++ parens(arg))
    | _ => None
    }
  | _ => None
  };
};

let replace_all = (needle, replacement, input) =>
  Str.global_replace(Str.regexp_string(needle), replacement, input);

let hazel_syntax_of_algebrite = value =>
  value |> String.trim |> replace_all("^", "**");

let editor_of_hazel_text = (~settings as _, text) =>
  text
  |> Parser.to_zipper(~root=Exp)
  |> Option.map(zipper =>
       CodeEditable.Model.mk(Editor.Model.mk(zipper, ~root=Exp))
     );
