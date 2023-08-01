/*
   Built-in functions for Hazel.

   To add a built-in function or constant, write the implementation in the
   `Pervasives.Impls` module below and add it to `Pervasives`.

   See the existing ones for reference.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = VarMap.t_(Builtin.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type forms = VarMap.t_((DHExp.t, Builtin.builtin_evaluate));

let ctx = (builtins: t): Ctx.t =>
  List.map(
    ((name, Builtin.{typ, _})) =>
      Ctx.VarEntry({name, typ, id: Id.invalid}),
    builtins,
  );

let forms = (builtins: t): forms =>
  List.map(
    ((name, Builtin.{typ: _, eval, elab})) => (name, (elab, eval)),
    builtins,
  );

let using = (name: Var.t, impl: Var.t => Builtin.t, builtins: t): t =>
  VarMap.extend(builtins, (name, impl(name)));

module Pervasives = {
  module Impls = {
    open EvaluatorMonad;
    open EvaluatorResult;

    /* is_finite implementation. */
    let is_finite = (name, r1) =>
      switch (r1) {
      | BoxedValue(FloatLit(f)) =>
        let b = Float.is_finite(f);
        BoxedValue(BoolLit(b)) |> return;
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1)))
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };

    /* is_infinite implementation. */
    let is_infinite = (name, r1) =>
      switch (r1) {
      | BoxedValue(FloatLit(f)) =>
        let b = Float.is_infinite(f);
        BoxedValue(BoolLit(b)) |> return;
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1)))
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };

    /* is_NaN implementation. */
    let is_nan = (name, r1) =>
      switch (r1) {
      | BoxedValue(FloatLit(f)) =>
        let b = Float.is_nan(f);
        BoxedValue(BoolLit(b)) |> return;
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1)))
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };

    /* int_of_string implementation. */
    let int_of_string = (name, r1) =>
      switch (r1) {
      | BoxedValue(StringLit(f) as d1) =>
        let i = int_of_string_opt(f);
        switch (i) {
        | Some(x) => BoxedValue(IntLit(x)) |> return
        | None =>
          Indet(
            InvalidOperation(ApBuiltin(name, [d1]), InvalidIntOfString),
          )
          |> return
        };
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedStringLit(d1)))
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };

    /* int_of_float implementation. */
    let int_of_float = (name, r1) =>
      switch (r1) {
      | BoxedValue(FloatLit(f)) =>
        let i = int_of_float(f);
        BoxedValue(IntLit(i)) |> return;
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1)))
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };

    /* float_of_int implementation. */
    let float_of_int = (name, r1) =>
      switch (r1) {
      | BoxedValue(IntLit(i)) =>
        let f = float_of_int(i);
        BoxedValue(FloatLit(f)) |> return;
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1)))
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };

    /* float_of_string implementation. */
    let float_of_string = (name, r1) =>
      switch (r1) {
      | BoxedValue(StringLit(s) as d1) =>
        let f = float_of_string_opt(s);
        switch (f) {
        | Some(x) => BoxedValue(FloatLit(x)) |> return
        | None =>
          Indet(
            InvalidOperation(ApBuiltin(name, [d1]), InvalidFloatOfString),
          )
          |> return
        };
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedStringLit(d1)))
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };

    /* string_of_int implementation. */
    let string_of_int = (name, r1) =>
      switch (r1) {
      | BoxedValue(IntLit(i)) =>
        let s = string_of_int(i);
        BoxedValue(StringLit(s)) |> return;
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1)))
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };

    /* string_of_float implementation. */
    let string_of_float = (name, r1) =>
      switch (r1) {
      | BoxedValue(FloatLit(f)) =>
        let s = string_of_float(f);
        BoxedValue(StringLit(s)) |> return;
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1)))
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };

    /* mod implementation */
    let int_mod = (name, r1) =>
      switch (r1) {
      | BoxedValue(Tuple([IntLit(n), IntLit(m)]) as d1) =>
        switch (m) {
        | 0 =>
          Indet(InvalidOperation(ApBuiltin(name, [d1]), DivideByZero))
          |> return
        | _ => return(BoxedValue(IntLit(n mod m)))
        }
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedTuple(d1)))
      | Indet(d1) => return(Indet(ApBuiltin(name, [d1])))
      };

    /* PI implementation. */
    let pi = DHExp.FloatLit(Float.pi);

    let source = (name, r1) => {
      let rec generate_pat = (dp: DHPat.t): DHExp.t => {
        switch (dp) {
        | EmptyHole(_) => Constructor("IP_EmptyHole")
        | NonEmptyHole(_, _, _, dp) => generate_pat(dp)
        | Wild => Constructor("IP_Wild")
        | ExpandingKeyword(_) => Constructor("IP_ExpandingKeyword")
        | InvalidText(_) => Constructor("IP_InvalidText")
        | BadConstructor(_) => Constructor("IP_BadConstructor")
        | Var(x) => Ap(Constructor("IP_Var"), StringLit(x))
        | IntLit(v) => Ap(Constructor("IP_Int"), IntLit(v))
        | FloatLit(v) => Ap(Constructor("IP_Float"), FloatLit(v))
        | BoolLit(v) => Ap(Constructor("IP_Bool"), BoolLit(v))
        | StringLit(v) => Ap(Constructor("IP_String"), StringLit(v))
        | ListLit(_, l) =>
          Ap(
            Constructor("IP_ListLit"),
            ListLit(
              -1,
              -1,
              Var("IPat"),
              List.map(dp1 => generate_pat(dp1), l),
            ),
          )
        | Cons(dp1, dp2) =>
          Ap(
            Constructor("IP_Cons"),
            Tuple([generate_pat(dp1), generate_pat(dp2)]),
          )
        | Tuple(l) =>
          Ap(
            Constructor("IP_Tuple"),
            Tuple(List.map(dp1 => generate_pat(dp1), l)),
          )
        | Constructor(name) =>
          Ap(Constructor("IP_Constructor"), StringLit(name))
        | Ap(dp1, dp2) =>
          Ap(
            Constructor("IP_Ap"),
            Tuple([generate_pat(dp1), generate_pat(dp2)]),
          )
        };
      }
      and generate_rules = (l: list(DHExp.rule)): list(DHExp.t) => {
        List.map(
          (rule: DHExp.rule) => {
            switch (rule) {
            | Rule(dp, d) => DHExp.Tuple([generate_pat(dp), generate(d)])
            }
          },
          l,
        );
      }
      and generate_case = (case: DHExp.case): DHExp.t => {
        switch (case) {
        | Case(d1, l, _) =>
          Tuple([
            generate(d1),
            Ap(
              Constructor("IE_ListLit"),
              ListLit(-1, -1, Var("IExp"), generate_rules(l)),
            ),
          ])
        };
      }
      and generate = (d: DHExp.t): DHExp.t => {
        switch (d) {
        | EmptyHole(_) => Constructor("IE_EmptyHole")
        | NonEmptyHole(_, _, _, d) => generate(d)
        | ExpandingKeyword(_) => Constructor("IE_ExpandingKeyword")
        | FreeVar(_, _, name) => Ap(Constructor("IE_Var"), StringLit(name))
        | InvalidText(_) => Constructor("IE_InvalidText")
        | InconsistentBranches(_, _, case) =>
          Ap(Constructor("IE_Match"), generate_case(case))
        | Closure(_, d) => generate(d)
        | BoundVar(name) => Ap(Constructor("IE_Var"), StringLit(name))
        | Sequence(d1, d2) =>
          Ap(
            Constructor("IE_Sequence"),
            Tuple([generate(d1), generate(d2)]),
          )
        | Let(dp, d1, d2) =>
          Ap(
            Constructor("IE_Let"),
            Tuple([generate_pat(dp), generate(d1), generate(d2)]),
          )
        | FixF(_, _, d) => generate(d)
        | Fun(dp, _, d, v) =>
          let dh_name =
            switch (v) {
            | Some(name) => DHExp.Ap(Constructor("Some"), StringLit(name))
            | None => DHExp.Constructor("None")
            };
          Ap(
            Constructor("IE_Fun"),
            Tuple([generate_pat(dp), generate(d), dh_name]),
          );
        | Ap(d1, d2) =>
          Ap(Constructor("IE_Ap"), Tuple([generate(d1), generate(d2)]))
        | ApBuiltin(_) => Constructor("IE_ApBuiltin")
        | TestLit(_) => Constructor("TestLit")
        | BoolLit(_) => Ap(Constructor("IE_Bool"), d)
        | IntLit(_) => Ap(Constructor("IE_Int"), d)
        | FloatLit(_) => Ap(Constructor("IE_Float"), d)
        | StringLit(_) => Ap(Constructor("IE_String"), d)
        | BinBoolOp(_, d1, d2) =>
          Ap(Constructor("IE_BinOp"), Tuple([generate(d1), generate(d2)]))
        | BinIntOp(_, d1, d2) =>
          Ap(Constructor("IE_BinOp"), Tuple([generate(d1), generate(d2)]))
        | BinFloatOp(_, d1, d2) =>
          Ap(Constructor("IE_BinOp"), Tuple([generate(d1), generate(d2)]))
        | BinStringOp(_, d1, d2) =>
          Ap(Constructor("IE_BinOp"), Tuple([generate(d1), generate(d2)]))
        | ListLit(v1, v2, _, l) =>
          Ap(
            Constructor("IE_ListLit"),
            ListLit(v1, v2, Var("IExp"), List.map(d1 => generate(d1), l)),
          )
        | Cons(d1, d2) =>
          Ap(Constructor("IE_Cons"), Tuple([generate(d1), generate(d2)]))
        | Tuple(l) =>
          Ap(
            Constructor("IE_Tuple"),
            Tuple(List.map(d1 => generate(d1), l)),
          )
        | Prj(d1, _) => Ap(Constructor("IE_Prj"), generate(d1))
        | Constructor(name) =>
          Ap(Constructor("IE_Constructor"), StringLit(name))
        | ConsistentCase(case) =>
          Ap(Constructor("IE_Match"), generate_case(case))
        | Cast(d1, _, _) => generate(d1)
        | FailedCast(d1, _, _) => generate(d1)
        | InvalidOperation(d1, _) => generate(d1)
        };
      };
      switch (r1) {
      | BoxedValue(d1) =>
        let res = generate(d1);
        print_endline("YABO");
        print_endline(DHExp.show(res));
        BoxedValue(res) |> return;
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };
    };
    /* Infinity-float implementation. */
    let infinity = DHExp.FloatLit(Float.infinity);
    let neg_infinity = DHExp.FloatLit(Float.neg_infinity);

    /* NaN float implementation. */
    let nan = DHExp.FloatLit(Float.nan);
  };

  let pi = name => Builtin.mk_zero(name, Float, Impls.pi);
  let infinity = name => Builtin.mk_zero(name, Float, Impls.infinity);
  let neg_infinity = name => Builtin.mk_zero(name, Float, Impls.neg_infinity);
  let nan = name => Builtin.mk_zero(name, Float, Impls.nan);
  let is_finite = name =>
    Builtin.mk_one(name, Arrow(Float, Bool), Impls.is_finite);
  let is_infinite = name =>
    Builtin.mk_one(name, Arrow(Float, Bool), Impls.is_infinite);
  let is_nan = name =>
    Builtin.mk_one(name, Arrow(Float, Bool), Impls.is_nan);
  let int_of_float = name =>
    Builtin.mk_one(name, Arrow(Float, Int), Impls.int_of_float);
  let int_of_string = name =>
    Builtin.mk_one(name, Arrow(String, Int), Impls.int_of_string);
  let float_of_int = name =>
    Builtin.mk_one(name, Arrow(Int, Float), Impls.float_of_int);
  let float_of_string = name =>
    Builtin.mk_one(name, Arrow(String, Float), Impls.float_of_string);
  let string_of_float = name =>
    Builtin.mk_one(name, Arrow(Float, String), Impls.string_of_float);
  let string_of_int = name =>
    Builtin.mk_one(name, Arrow(Int, String), Impls.string_of_int);
  let modulo = name =>
    Builtin.mk_one(name, Arrow(Prod([Int, Int]), Int), Impls.int_mod);
  let source = name =>
    Builtin.mk_one(
      name,
      Arrow(Unknown(Internal), Var("IExp")),
      Impls.source,
    );

  let builtins =
    VarMap.empty
    |> using("pi", pi)
    |> using("infinity", infinity)
    |> using("neg_infinity", neg_infinity)
    |> using("nan", nan)
    |> using("is_finite", is_finite)
    |> using("is_infinite", is_infinite)
    |> using("is_nan", is_nan)
    |> using("int_of_float", int_of_float)
    |> using("int_of_string", int_of_string)
    |> using("float_of_int", float_of_int)
    |> using("float_of_string", float_of_string)
    |> using("string_of_int", string_of_int)
    |> using("string_of_float", string_of_float)
    |> using("mod", modulo)
    |> using("source", source);
};
