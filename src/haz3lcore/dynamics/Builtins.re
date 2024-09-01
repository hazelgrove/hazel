open DHExp;

/*
   Built-in functions for Hazel.

   To add a built-in function or constant, write the implementation in the
   `Pervasives.Impls` module below and add it to `builtins`.

   See the existing ones for reference.
 */

[@deriving (show({with_path: false}), sexp)]
type builtin =
  | Const(Typ.t, DHExp.t)
  | Fn(Typ.t, Typ.t, DHExp.t => DHExp.t);

[@deriving (show({with_path: false}), sexp)]
type t = VarMap.t_(builtin);

[@deriving (show({with_path: false}), sexp)]
type forms = VarMap.t_(DHExp.t => DHExp.t);

type result = Result.t(DHExp.t, EvaluatorError.t);

let const = (name: Var.t, typ: Typ.term, v: DHExp.t, builtins: t): t =>
  VarMap.extend(builtins, (name, Const(typ |> Typ.fresh, v)));
let fn =
    (
      name: Var.t,
      t1: Typ.term,
      t2: Typ.term,
      impl: DHExp.t => DHExp.t,
      builtins: t,
    )
    : t =>
  VarMap.extend(
    builtins,
    (name, Fn(t1 |> Typ.fresh, t2 |> Typ.fresh, impl)),
  );

module Pervasives = {
  module Impls = {
    /* constants */
    let infinity = DHExp.Float(Float.infinity) |> fresh;
    let neg_infinity = DHExp.Float(Float.neg_infinity) |> fresh;
    let nan = DHExp.Float(Float.nan) |> fresh;
    let epsilon_float = DHExp.Float(epsilon_float) |> fresh;
    let pi = DHExp.Float(Float.pi) |> fresh;
    let max_int = DHExp.Int(Int.max_int) |> fresh;
    let min_int = DHExp.Int(Int.min_int) |> fresh;

    let unary = (f: DHExp.t => result, d: DHExp.t) => {
      switch (f(d)) {
      | Ok(r') => r'
      | Error(e) => EvaluatorError.Exception(e) |> raise
      };
    };

    let binary = (f: (DHExp.t, DHExp.t) => result, d: DHExp.t) => {
      switch (term_of(d)) {
      | Tuple([d1, d2]) =>
        switch (f(d1, d2)) {
        | Ok(r) => r
        | Error(e) => EvaluatorError.Exception(e) |> raise
        }
      | _ => raise(EvaluatorError.Exception(InvalidBoxedTuple(d)))
      };
    };

    let ternary = (f: (DHExp.t, DHExp.t, DHExp.t) => result, d: DHExp.t) => {
      switch (term_of(d)) {
      | Tuple([d1, d2, d3]) =>
        switch (f(d1, d2, d3)) {
        | Ok(r) => r
        | Error(e) => EvaluatorError.Exception(e) |> raise
        }
      | _ => raise(EvaluatorError.Exception(InvalidBoxedTuple(d)))
      };
    };

    let is_finite =
      unary(d =>
        switch (term_of(d)) {
        | Float(f) => Ok(fresh(Bool(Float.is_finite(f))))
        | _ => Error(InvalidBoxedFloatLit(d))
        }
      );

    let is_infinite =
      unary(d =>
        switch (term_of(d)) {
        | Float(f) => Ok(fresh(Bool(Float.is_infinite(f))))
        | _ => Error(InvalidBoxedFloatLit(d))
        }
      );

    let is_nan =
      unary(d =>
        switch (term_of(d)) {
        | Float(f) => Ok(fresh(Bool(Float.is_nan(f))))
        | _ => Error(InvalidBoxedFloatLit(d))
        }
      );

    let string_of_int =
      unary(d =>
        switch (term_of(d)) {
        | Int(n) => Ok(fresh(String(string_of_int(n))))
        | _ => Error(InvalidBoxedIntLit(d))
        }
      );

    let string_of_float =
      unary(d =>
        switch (term_of(d)) {
        | Float(f) => Ok(fresh(String(string_of_float(f))))
        | _ => Error(InvalidBoxedFloatLit(d))
        }
      );

    let string_of_bool =
      unary(d =>
        switch (term_of(d)) {
        | Bool(b) => Ok(fresh(String(string_of_bool(b))))
        | _ => Error(InvalidBoxedBoolLit(d))
        }
      );

    let int_of_float =
      unary(d =>
        switch (term_of(d)) {
        | Float(f) => Ok(fresh(Int(int_of_float(f))))
        | _ => Error(InvalidBoxedFloatLit(d))
        }
      );

    let float_of_int =
      unary(d =>
        switch (term_of(d)) {
        | Int(n) => Ok(fresh(Float(float_of_int(n))))
        | _ => Error(InvalidBoxedIntLit(d))
        }
      );

    let abs =
      unary(d =>
        switch (term_of(d)) {
        | Int(n) => Ok(fresh(Int(abs(n))))
        | _ => Error(InvalidBoxedIntLit(d))
        }
      );

    let float_op = fn =>
      unary(d =>
        switch (term_of(d)) {
        | Float(f) => Ok(fresh(Float(fn(f))))
        | _ => Error(InvalidBoxedFloatLit(d))
        }
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
      unary(d =>
        switch (term_of(d)) {
        | String(s) =>
          switch (convert(s)) {
          | Some(n) => Ok(wrap(n))
          | None =>
            let d' = DHExp.BuiltinFun(name) |> DHExp.fresh;
            let d' = DHExp.Ap(Forward, d', d) |> DHExp.fresh;
            let d' = DynamicErrorHole(d', InvalidOfString) |> DHExp.fresh;
            Ok(d');
          }
        | _ => Error(InvalidBoxedStringLit(d))
        }
      );

    let int_of_string =
      of_string(int_of_string_opt, n => Int(n) |> DHExp.fresh);
    let float_of_string =
      of_string(float_of_string_opt, f => Float(f) |> DHExp.fresh);
    let bool_of_string =
      of_string(bool_of_string_opt, b => Bool(b) |> DHExp.fresh);

    let int_mod = (name, d1) =>
      binary(
        (d1, d2) =>
          switch (term_of(d1), term_of(d2)) {
          | (Int(_), Int(0)) =>
            Ok(
              fresh(
                DynamicErrorHole(
                  DHExp.Ap(Forward, DHExp.BuiltinFun(name) |> fresh, d1)
                  |> fresh,
                  DivideByZero,
                ),
              ),
            )
          | (Int(n), Int(m)) => Ok(Int(n mod m) |> fresh)
          | (Int(_), _) =>
            raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2)))
          | (_, _) =>
            raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1)))
          },
        d1,
      );

    let string_length =
      unary(d =>
        switch (term_of(d)) {
        | String(s) => Ok(Int(String.length(s)) |> fresh)
        | _ => Error(InvalidBoxedStringLit(d))
        }
      );

    let string_compare =
      binary((d1, d2) =>
        switch (term_of(d1), term_of(d2)) {
        | (String(s1), String(s2)) =>
          Ok(Int(String.compare(s1, s2)) |> fresh)
        | (String(_), _) => Error(InvalidBoxedStringLit(d2))
        | (_, _) => Error(InvalidBoxedStringLit(d1))
        }
      );

    let string_trim =
      unary(d =>
        switch (term_of(d)) {
        | String(s) => Ok(String(String.trim(s)) |> fresh)
        | _ => Error(InvalidBoxedStringLit(d))
        }
      );

    let string_of: DHExp.t => option(string) =
      d =>
        switch (term_of(d)) {
        | String(s) => Some(s)
        | _ => None
        };

    let string_concat =
      binary((d1, d2) =>
        switch (term_of(d1), term_of(d2)) {
        | (String(s1), ListLit(xs)) =>
          switch (xs |> List.map(string_of) |> Util.OptUtil.sequence) {
          | None => Error(InvalidBoxedStringLit(List.hd(xs)))
          | Some(xs) => Ok(String(String.concat(s1, xs)) |> fresh)
          }
        | (String(_), _) => Error(InvalidBoxedListLit(d2))
        | (_, _) => Error(InvalidBoxedStringLit(d1))
        }
      );

    let string_sub = _ =>
      ternary((d1, d2, d3) =>
        switch (term_of(d1), term_of(d2), term_of(d3)) {
        | (String(s), Int(idx), Int(len)) =>
          try(Ok(String(String.sub(s, idx, len)) |> fresh)) {
          | _ =>
            // TODO: make it clear that the problem could be with d3 too
            Ok(DynamicErrorHole(d2, IndexOutOfBounds) |> fresh)
          }
        | (String(_), Int(_), _) => Error(InvalidBoxedIntLit(d3))
        | (String(_), _, _) => Error(InvalidBoxedIntLit(d2))
        | (_, _, _) => Error(InvalidBoxedIntLit(d1))
        }
      );
    // let atom =
    //   unary(d =>
    //     switch (term_of(d)) {
    //     | String(s) => Ok(Prop(Atom(s)) |> fresh)
    //     | _ => Error(InvalidBoxedStringLit(d))
    //     }
    //   );
    // let and_ =
    //   unary(d =>
    //     switch (term_of(d)) {
    //     | Tuple([{term: Prop(p1), _}, {term: Prop(p2), _}]) =>
    //       Ok(Prop(And(p1, p2)) |> fresh)
    //     | _ => Error(InvalidBoxedTuple(d))
    //     }
    //   );
    // let or_ =
    //   unary(d =>
    //     switch (term_of(d)) {
    //     | Tuple([{term: Prop(p1), _}, {term: Prop(p2), _}]) =>
    //       Ok(Prop(Or(p1, p2)) |> fresh)
    //     | _ => Error(InvalidBoxedTuple(d))
    //     }
    //   );
    // let implies =
    //   unary(d =>
    //     switch (term_of(d)) {
    //     | Tuple([{term: Prop(p1), _}, {term: Prop(p2), _}]) =>
    //       Ok(Prop(Implies(p1, p2)) |> fresh)
    //     | _ => Error(InvalidBoxedTuple(d))
    //     }
    //   );
    // let prop_of: DHExp.t => option(Derivation.Syntax.t) =
    //   d =>
    //     switch (term_of(d)) {
    //     | Prop(p) => Some(p)
    //     | _ => None
    //     };
    // let entail =
    //   unary(d =>
    //     switch (term_of(d)) {
    //     | Tuple([{term: ListLit(ctx), _}, {term: Prop(p), _}]) =>
    //       switch (ctx |> List.map(prop_of) |> Util.OptUtil.sequence) {
    //       | Some(ctx) => Ok(Judgement(Entail(ctx, p)) |> fresh)
    //       | None => Error(InvalidBoxedListLit(List.hd(ctx)))
    //       }
    //     | _ => Error(InvalidBoxedTuple(d))
    //     }
    //   );
    // let judgement_of: DHExp.t => option(Derivation.Judgement.t) =
    //   d =>
    //     switch (term_of(d)) {
    //     | Judgement(j) => Some(j)
    //     | _ => None
    //     };
    // let rule = (_name, rule) =>
    //   unary(d =>
    //     switch (term_of(d)) {
    //     | Tuple([
    //         {term: Judgement(conclusion), _},
    //         {term: ListLit(premises), _},
    //       ]) =>
    //       switch (premises |> List.map(judgement_of) |> Util.OptUtil.sequence) {
    //       | Some(premises) =>
    //         Ok(
    //           switch (
    //             DerivationError.RuleVer.verify(rule, conclusion, premises)
    //           ) {
    //           | Ok(_) =>
    //             if (premises
    //                 |> List.exists(
    //                      fun
    //                      | Derivation.Judgement.Verified(_) => false
    //                      | _ => true,
    //                    )) {
    //               Judgement(Partial(Derivation.Judgement.just(conclusion)))
    //               |> fresh;
    //             } else {
    //               Judgement(Verified(Derivation.Judgement.just(conclusion)))
    //               |> fresh;
    //             }
    //           | Error(_e) =>
    //             Judgement(Wrong(Derivation.Judgement.just(conclusion)))
    //             |> fresh
    //           },
    //         )
    //       | None => Error(InvalidBoxedListLit(List.hd(premises)))
    //       }
    //     | _ => Error(InvalidBoxedTuple(d))
    //     }
    //   );
    // let rule_Assumption = rule("rule_Assumption", Assumption);
    // let rule_And_I = rule("rule_And_I", And_I);
    // let rule_And_E_L = rule("rule_And_E_L", And_E_L);
    // let rule_And_E_R = rule("rule_And_E_R", And_E_R);
    // let rule_Or_I_L = rule("rule_Or_I_L", Or_I_L);
    // let rule_Or_I_R = rule("rule_Or_I_R", Or_I_R);
    // let rule_Or_E = rule("rule_Or_E", Or_E);
    // let rule_Implies_I = rule("rule_Implies_I", Implies_I);
    // let rule_Implies_E = rule("rule_Implies_E", Implies_E);
    // let rule_Truth_I = rule("rule_Truth_I", Truth_I);
    // let rule_Falsity_E = rule("rule_Falsity_E", Falsity_E);
  };

  open Impls;

  // let rule = (name, func) =>
  //   fn(
  //     name,
  //     Prod([
  //       Judgement |> Typ.fresh,
  //       List(Judgement |> Typ.fresh) |> Typ.fresh,
  //     ]),
  //     Judgement,
  //     func,
  //   );
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
    |> fn(
         "mod",
         Prod([Int |> Typ.fresh, Int |> Typ.fresh]),
         Int,
         int_mod("mod"),
       )
    |> fn("string_length", String, Int, string_length)
    |> fn(
         "string_compare",
         Prod([String |> Typ.fresh, String |> Typ.fresh]),
         Int,
         string_compare,
       )
    |> fn("string_trim", String, String, string_trim)
    |> fn(
         "string_concat",
         Prod([String |> Typ.fresh, List(String |> Typ.fresh) |> Typ.fresh]),
         String,
         string_concat,
       )
    |> fn(
         "string_sub",
         Prod([String |> Typ.fresh, Int |> Typ.fresh, Int |> Typ.fresh]),
         String,
         string_sub("string_sub"),
       );
  // |> fn("atom", String, Prop, atom)
  // |> fn("and", Prod([Prop |> Typ.fresh, Prop |> Typ.fresh]), Prop, and_)
  // |> fn("or", Prod([Prop |> Typ.fresh, Prop |> Typ.fresh]), Prop, or_)
  // |> fn(
  //      "implies",
  //      Prod([Prop |> Typ.fresh, Prop |> Typ.fresh]),
  //      Prop,
  //      implies,
  //    )
  // |> const("truth", Prop, Prop(Truth) |> fresh)
  // |> const("falsity", Prop, Prop(Falsity) |> fresh)
  // |> fn(
  //      "entail",
  //      Prod([List(Prop |> Typ.fresh) |> Typ.fresh, Prop |> Typ.fresh]),
  //      Judgement,
  //      entail,
  //    );
  // |> rule("rule_Assumption", rule_Assumption)
  // |> rule("rule_And_I", rule_And_I)
  // |> rule("rule_And_E_L", rule_And_E_L)
  // |> rule("rule_And_E_R", rule_And_E_R)
  // |> rule("rule_Or_I_L", rule_Or_I_L)
  // |> rule("rule_Or_I_R", rule_Or_I_R)
  // |> rule("rule_Or_E", rule_Or_E)
  // |> rule("rule_Implies_I", rule_Implies_I)
  // |> rule("rule_Implies_E", rule_Implies_E)
  // |> rule("rule_Truth_I", rule_Truth_I)
  // |> rule("rule_Falsity_E", rule_Falsity_E);
};

let ctx_init: Ctx.t = {
  let meta_cons_map: ConstructorMap.t(Typ.t) = [
    Variant("$e", [Id.mk()], None),
    Variant("$v", [Id.mk()], None),
  ];
  let meta =
    Ctx.TVarEntry({
      name: "$Meta",
      id: Id.invalid,
      kind: Ctx.Singleton(Sum(meta_cons_map) |> Typ.fresh),
    });
  List.map(
    fun
    | (name, Const(typ, _)) => Ctx.VarEntry({name, typ, id: Id.invalid})
    | (name, Fn(t1, t2, _)) =>
      Ctx.VarEntry({name, typ: Arrow(t1, t2) |> Typ.fresh, id: Id.invalid}),
    Pervasives.builtins,
  )
  |> Ctx.extend(_, meta)
  // |> Ctx.extend(_, BuiltinsDerivation.tvar_entries)
  // |> List.fold_left(Ctx.extend, _, BuiltinsDerivation.tvar_entries)
  |> BuiltinsDerivation.add_tvar_entries
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
      | (name, Fn(_)) =>
        Environment.extend(env, (name, BuiltinFun(name) |> fresh)),
    Environment.empty,
    Pervasives.builtins,
  );
