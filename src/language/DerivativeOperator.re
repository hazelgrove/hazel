/* Stable semantic identities for Hazel's two derivative operators.

   Internal names are deliberately not valid Hazel identifiers. Surface
   spellings are presentation metadata so renaming them does not affect
   statics, proof rules, saved profiles, or Rocq certificates. */

let expression_internal_name = "$hazel.derivative.expression";
let function_internal_name = "$hazel.derivative.function";

let expression_surface_prefix = "deriv";
let expression_surface_separator = "by";
let function_surface = "D";

let is_surface_token = token =>
  token == expression_surface_prefix || token == function_surface;

let legacy_name = "diff";

let rec strip = (exp: Exp.t) =>
  switch (exp.term) {
  | Parens(inner)
  | Asc(inner, _) => strip(inner)
  | _ => exp
  };

let var_exp = name => Exp.fresh(Var(name));
let app_exp = (name, arg) =>
  Exp.fresh(Ap(Operators.Forward, var_exp(name), arg));

let expression = (~body, ~variable) =>
  /* The semantic tuple remains [variable, body] so changing the surface syntax
     does not affect statics, proof rules, saved profiles, or certificates. */
  app_exp(expression_internal_name, Exp.fresh(Tuple([variable, body])));

let function_ = function_exp => app_exp(function_internal_name, function_exp);

let function_name = (exp: Exp.t) => {
  switch (strip(exp).term) {
  | Var(name)
  | BuiltinFun(name) => Some(name)
  | _ => None
  };
};

let expression_parts = (~legacy=true, exp: Exp.t) => {
  switch (strip(exp).term) {
  | Ap(Operators.Forward, fn, arg) =>
    switch (function_name(fn), strip(arg).term) {
    | (Some(name), Tuple([variable, body]))
        when name == expression_internal_name =>
      Some((strip(body), strip(variable)))
    | (Some(name), Tuple([body, variable]))
        when legacy && name == legacy_name =>
      Some((strip(body), strip(variable)))
    | _ => None
    }
  | _ => None
  };
};

let function_argument = (~legacy=true, exp: Exp.t) => {
  switch (strip(exp).term) {
  | Ap(Operators.Forward, fn, arg) =>
    switch (function_name(fn)) {
    | Some(name)
        when name == function_internal_name || legacy && name == legacy_name =>
      switch (strip(arg).term) {
      | Tuple([_, _]) when legacy && name == legacy_name => None
      | _ => Some(strip(arg))
      }
    | _ => None
    }
  | _ => None
  };
};

let is_expression = exp => Option.is_some(expression_parts(exp));
let is_function = exp => Option.is_some(function_argument(exp));
