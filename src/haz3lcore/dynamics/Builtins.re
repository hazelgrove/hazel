open DHExp;

/*
   Built-in functions for Hazel.

   To add a built-in function or constant, write the implementation in the
   `Pervasives.Impls` module below and add it to `builtins`.

   See the existing ones for reference.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type builtin =
  | Const(Typ.t, DHExp.t)
  | Fn(Typ.t, Typ.t, DHExp.t => DHExp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = VarMap.t_(builtin);

[@deriving (show({with_path: false}), sexp, yojson)]
type forms = VarMap.t_(DHExp.t => DHExp.t);

type result = Result.t(DHExp.t, EvaluatorError.t);

let const = (name: Var.t, typ: Typ.t, v: DHExp.t, builtins: t): t =>
  VarMap.extend(builtins, (name, Const(typ, v)));
let fn =
    (name: Var.t, t1: Typ.t, t2: Typ.t, impl: DHExp.t => DHExp.t, builtins: t)
    : t =>
  VarMap.extend(builtins, (name, Fn(t1, t2, impl)));

module Pervasives = {
  module Impls = {
    /* constants */
    let infinity = DHExp.FloatLit(Float.infinity);
    let neg_infinity = DHExp.FloatLit(Float.neg_infinity);
    let nan = DHExp.FloatLit(Float.nan);
    let epsilon_float = DHExp.FloatLit(epsilon_float);
    let pi = DHExp.FloatLit(Float.pi);
    let max_int = DHExp.IntLit(Int.max_int);
    let min_int = DHExp.IntLit(Int.min_int);

    let unary = (f: DHExp.t => result, r: DHExp.t) =>
      switch (f(r)) {
      | Ok(r') => r'
      | Error(e) => EvaluatorError.Exception(e) |> raise
      };

    let is_finite =
      unary(
        fun
        | FloatLit(f) => Ok(BoolLit(Float.is_finite(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let is_infinite =
      unary(
        fun
        | FloatLit(f) => Ok(BoolLit(Float.is_infinite(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let is_nan =
      unary(
        fun
        | FloatLit(f) => Ok(BoolLit(Float.is_nan(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let string_of_int =
      unary(
        fun
        | IntLit(n) => Ok(StringLit(string_of_int(n)))
        | d => Error(InvalidBoxedIntLit(d)),
      );

    let string_of_float =
      unary(
        fun
        | FloatLit(f) => Ok(StringLit(string_of_float(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let string_of_bool =
      unary(
        fun
        | BoolLit(b) => Ok(StringLit(string_of_bool(b)))
        | d => Error(InvalidBoxedBoolLit(d)),
      );

    let int_of_float =
      unary(
        fun
        | FloatLit(f) => Ok(IntLit(int_of_float(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let float_of_int =
      unary(
        fun
        | IntLit(n) => Ok(FloatLit(float_of_int(n)))
        | d => Error(InvalidBoxedIntLit(d)),
      );

    let abs =
      unary(
        fun
        | IntLit(n) => Ok(IntLit(abs(n)))
        | d => Error(InvalidBoxedIntLit(d)),
      );

    let float_op = fn =>
      unary(
        fun
        | FloatLit(f) => Ok(FloatLit(fn(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let abs_float = float_op(abs_float);
    let ceil = float_op(ceil);
    let floor = float_op(floor);
    let sqrt = float_op(sqrt);
    let exp = float_op(exp);
    let log = float_op(log);
    let log10 = float_op(log10);
    let sin = float_op(sin);
    let cos = float_op(cos);
    let tan = float_op(tan);
    let asin = float_op(asin);
    let acos = float_op(acos);
    let atan = float_op(atan);

    let of_string =
        (convert: string => option('a), wrap: 'a => DHExp.t, name: string) =>
      unary(
        fun
        | StringLit(s) as d =>
          switch (convert(s)) {
          | Some(n) => Ok(wrap(n))
          | None =>
            let d' = DHExp.Ap(DHExp.BuiltinFun(name), d);
            Ok(InvalidOperation(d', InvalidOfString));
          }
        | d => Error(InvalidBoxedStringLit(d)),
      );

    let int_of_string = of_string(int_of_string_opt, n => IntLit(n));
    let float_of_string = of_string(float_of_string_opt, f => FloatLit(f));
    let bool_of_string = of_string(bool_of_string_opt, b => BoolLit(b));

    let int_mod = (name, d1) =>
      switch (d1) {
      | Tuple([IntLit(n), IntLit(m)]) =>
        switch (m) {
        | 0 =>
          InvalidOperation(
            DHExp.Ap(DHExp.BuiltinFun(name), d1),
            DivideByZero,
          )
        | _ => IntLit(n mod m)
        }
      | d1 => raise(EvaluatorError.Exception(InvalidBoxedTuple(d1)))
      };

    let string_length =
      unary(
        fun
        | StringLit(s) => Ok(IntLit(String.length(s)))
        | d => Error(InvalidBoxedStringLit(d)),
      );

    let string_compare =
      unary(
        fun
        | Tuple([StringLit(s1), StringLit(s2)]) =>
          Ok(IntLit(String.compare(s1, s2)))
        | d => Error(InvalidBoxedTuple(d)),
      );

    let string_trim =
      unary(
        fun
        | StringLit(s) => Ok(StringLit(String.trim(s)))
        | d => Error(InvalidBoxedStringLit(d)),
      );

    let string_of: DHExp.t => option(string) =
      fun
      | StringLit(s) => Some(s)
      | _ => None;

    let string_concat =
      unary(
        fun
        | Tuple([StringLit(s1), ListLit(_, _, _, xs)]) =>
          switch (xs |> List.map(string_of) |> Util.OptUtil.sequence) {
          | None => Error(InvalidBoxedStringLit(List.hd(xs)))
          | Some(xs) => Ok(StringLit(String.concat(s1, xs)))
          }
        | d => Error(InvalidBoxedTuple(d)),
      );

    let string_sub = name =>
      unary(
        fun
        | Tuple([StringLit(s), IntLit(idx), IntLit(len)]) as d =>
          try(Ok(StringLit(String.sub(s, idx, len)))) {
          | _ =>
            let d' = DHExp.Ap(DHExp.BuiltinFun(name), d);
            Ok(InvalidOperation(d', IndexOutOfBounds));
          }
        | d => Error(InvalidBoxedTuple(d)),
      );

    let atom =
      unary(
        fun
        | StringLit(s) => Ok(PropLit(Atom(s)))
        | d => Error(InvalidBoxedStringLit(d)),
      );
    let and_ =
      unary(
        fun
        | Tuple([PropLit(p1), PropLit(p2)]) => Ok(PropLit(And(p1, p2)))
        | d => Error(InvalidBoxedTuple(d)),
      );
    let or_ =
      unary(
        fun
        | Tuple([PropLit(p1), PropLit(p2)]) => Ok(PropLit(Or(p1, p2)))
        | d => Error(InvalidBoxedTuple(d)),
      );
    let implies =
      unary(
        fun
        | Tuple([PropLit(p1), PropLit(p2)]) => Ok(PropLit(Implies(p1, p2)))
        | d => Error(InvalidBoxedTuple(d)),
      );
    let prop_of: DHExp.t => option(Derivation.Prop.t) =
      fun
      | PropLit(p) => Some(p)
      | _ => None;
    let entail =
      unary(
        fun
        | Tuple([ListLit(_, _, Prop, ctx), PropLit(p)]) =>
          switch (ctx |> List.map(prop_of) |> Util.OptUtil.sequence) {
          | Some(ctx) => Ok(JudgementLit(Wrong(Entail(ctx, p))))
          | None => Error(InvalidBoxedListLit(List.hd(ctx)))
          }
        | d => Error(InvalidBoxedTuple(d)),
      );
    let judgement_of: DHExp.t => option(Derivation.Judgement.t) =
      fun
      | JudgementLit(j) => Some(j)
      | _ => None;
    let rule = (_name, rule) =>
      unary(
        fun
        | Tuple([
            JudgementLit(conclusion),
            ListLit(_, _, Judgement, premises),
          ]) =>
          switch (premises |> List.map(judgement_of) |> Util.OptUtil.sequence) {
          | Some(premises) =>
            Ok(
              switch (
                DerivationError.RuleVer.verify(rule, conclusion, premises)
              ) {
              | Ok(_) =>
                if (premises
                    |> List.exists(
                         fun
                         | Derivation.Judgement.Verified(_) => false
                         | _ => true,
                       )) {
                  JudgementLit(
                    Partial(Derivation.Judgement.just(conclusion)),
                  );
                } else {
                  JudgementLit(
                    Verified(Derivation.Judgement.just(conclusion)),
                  );
                }
              | Error(_e) =>
                JudgementLit(Wrong(Derivation.Judgement.just(conclusion)))
              },
            )
          | None => Error(InvalidBoxedListLit(List.hd(premises)))
          }
        | d => Error(InvalidBoxedTuple(d)),
      );
    let rule_Assumption = rule("rule_Assumption", Assumption);
    let rule_And_I = rule("rule_And_I", And_I);
    let rule_And_E_L = rule("rule_And_E_L", And_E_L);
    let rule_And_E_R = rule("rule_And_E_R", And_E_R);
    let rule_Or_I_L = rule("rule_Or_I_L", Or_I_L);
    let rule_Or_I_R = rule("rule_Or_I_R", Or_I_R);
    let rule_Or_E = rule("rule_Or_E", Or_E);
    let rule_Implies_I = rule("rule_Implies_I", Implies_I);
    let rule_Implies_E = rule("rule_Implies_E", Implies_E);
    let rule_Truth_I = rule("rule_Truth_I", Truth_I);
    let rule_Falsity_E = rule("rule_Falsity_E", Falsity_E);
  };

  open Impls;

  let rule = (name, func) =>
    fn(name, Prod([Judgement, List(Judgement)]), Judgement, func);
  let builtins =
    VarMap.empty
    |> const("infinity", Float, infinity)
    |> const("neg_infinity", Float, neg_infinity)
    |> const("nan", Float, nan)
    |> const("epsilon_float", Float, epsilon_float)
    |> const("pi", Float, pi)
    |> const("max_int", Int, max_int)
    |> const("min_int", Int, min_int)
    |> fn("is_finite", Float, Bool, is_finite)
    |> fn("is_infinite", Float, Bool, is_infinite)
    |> fn("is_nan", Float, Bool, is_nan)
    |> fn("int_of_float", Float, Int, int_of_float)
    |> fn("float_of_int", Int, Float, float_of_int)
    |> fn("string_of_int", Int, String, string_of_int)
    |> fn("string_of_float", Float, String, string_of_float)
    |> fn("string_of_bool", Bool, String, string_of_bool)
    |> fn("int_of_string", String, Int, int_of_string("int_of_string"))
    |> fn(
         "float_of_string",
         String,
         Float,
         float_of_string("float_of_string"),
       )
    |> fn("bool_of_string", String, Bool, bool_of_string("bool_of_string"))
    |> fn("abs", Int, Int, abs)
    |> fn("abs_float", Float, Float, abs_float)
    |> fn("ceil", Float, Float, ceil)
    |> fn("floor", Float, Float, floor)
    |> fn("exp", Float, Float, exp)
    |> fn("log", Float, Float, log)
    |> fn("log10", Float, Float, log10)
    |> fn("sqrt", Float, Float, sqrt)
    |> fn("sin", Float, Float, sin)
    |> fn("cos", Float, Float, cos)
    |> fn("tan", Float, Float, tan)
    |> fn("asin", Float, Float, asin)
    |> fn("acos", Float, Float, acos)
    |> fn("atan", Float, Float, atan)
    |> fn("mod", Prod([Int, Int]), Int, int_mod("mod"))
    |> fn("string_length", String, Int, string_length)
    |> fn("string_compare", Prod([String, String]), Int, string_compare)
    |> fn("string_trim", String, String, string_trim)
    |> fn(
         "string_concat",
         Prod([String, List(String)]),
         String,
         string_concat,
       )
    |> fn(
         "string_sub",
         Prod([String, Int, Int]),
         String,
         string_sub("string_sub"),
       )
    |> fn("atom", String, Prop, atom)
    |> fn("and", Prod([Prop, Prop]), Prop, and_)
    |> fn("or", Prod([Prop, Prop]), Prop, or_)
    |> fn("implies", Prod([Prop, Prop]), Prop, implies)
    |> const("truth", Prop, PropLit(Truth))
    |> const("falsity", Prop, PropLit(Falsity))
    |> fn("entail", Prod([List(Prop), Prop]), Judgement, entail)
    |> rule("rule_Assumption", rule_Assumption)
    |> rule("rule_And_I", rule_And_I)
    |> rule("rule_And_E_L", rule_And_E_L)
    |> rule("rule_And_E_R", rule_And_E_R)
    |> rule("rule_Or_I_L", rule_Or_I_L)
    |> rule("rule_Or_I_R", rule_Or_I_R)
    |> rule("rule_Or_E", rule_Or_E)
    |> rule("rule_Implies_I", rule_Implies_I)
    |> rule("rule_Implies_E", rule_Implies_E)
    |> rule("rule_Truth_I", rule_Truth_I)
    |> rule("rule_Falsity_E", rule_Falsity_E);
};

let ctx_init: Ctx.t = {
  let meta_cons_map = ConstructorMap.of_list([("$e", None), ("$v", None)]);
  let meta =
    Ctx.TVarEntry({
      name: "$Meta",
      id: Id.invalid,
      kind: Kind.Singleton(Sum(meta_cons_map)),
    });
  List.map(
    fun
    | (name, Const(typ, _)) => Ctx.VarEntry({name, typ, id: Id.invalid})
    | (name, Fn(t1, t2, _)) =>
      Ctx.VarEntry({name, typ: Arrow(t1, t2), id: Id.invalid}),
    Pervasives.builtins,
  )
  |> Ctx.extend(_, meta)
  |> Ctx.add_ctrs(_, "$Meta", Id.invalid, meta_cons_map);
};

let forms_init: forms =
  List.filter_map(
    fun
    | (_, Const(_)) => None
    | (name, Fn(_, _, f)) => Some((name, f)),
    Pervasives.builtins,
  );

let env_init: Environment.t =
  List.fold_left(
    env =>
      fun
      | (name, Const(_, d)) => Environment.extend(env, (name, d))
      | (name, Fn(_)) => Environment.extend(env, (name, BuiltinFun(name))),
    Environment.empty,
    Pervasives.builtins,
  );
