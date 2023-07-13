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

    /* int_of_float implementation. */
    let int_of_float = (name, r1) =>
      switch (r1) {
      | BoxedValue(FloatLit(f)) =>
        let i = int_of_float(f);
        BoxedValue(IntLit(i)) |> return;
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1)))
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };

    /* float_of_int implementation. */
    let float_of_int = (name, r1) =>
      switch (r1) {
      | BoxedValue(IntLit(i)) =>
        let f = float_of_int(i);
        BoxedValue(FloatLit(f)) |> return;
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1)))
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
        | EmptyHole(_) => Tag("IP_EmptyHole")
        | NonEmptyHole(_, _, _, dp) => generate_pat(dp)
        | Wild => Tag("IP_Wild")
        | ExpandingKeyword(_) => Tag("IP_ExpandingKeyword")
        | InvalidText(_) => Tag("IP_InvalidText")
        | BadTag(_) => Tag("IP_BadTag")
        | Var(x) => Ap(Tag("IP_Var"), StringLit('\"' ++ x ++ '\"'))
        | IntLit(v) => Ap(Tag("IP_Int"), IntLit(v))
        | FloatLit(v) => Ap(Tag("IP_Float"), FloatLit(v))
        | BoolLit(v) => Ap(Tag("IP_Bool"), BoolLit(v))
        | StringLit(v) => Ap(Tag("IP_String"), StringLit(v))
        | ListLit(_, l) =>
          Ap(
            Tag("IP_ListLit"),
            ListLit(
              -1,
              -1,
              Var("IPat"),
              List.map(dp1 => generate_pat(dp1), l),
            ),
          )
        | Cons(dp1, dp2) =>
          Ap(
            Tag("IP_Cons"),
            Tuple([generate_pat(dp1), generate_pat(dp2)]),
          )
        | Tuple(l) =>
          Ap(
            Tag("IP_Tuple"),
            Tuple(List.map(dp1 => generate_pat(dp1), l)),
          )
        | Tag(name) => Ap(Tag("IP_Tag"), StringLit(name))
        | Ap(dp1, dp2) =>
          Ap(Tag("IP_Ap"), Tuple([generate_pat(dp1), generate_pat(dp2)]))
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
              Tag("IE_ListLit"),
              ListLit(-1, -1, Var("IExp"), generate_rules(l)),
            ),
          ])
        };
      }
      and generate = (d: DHExp.t): DHExp.t => {
        switch (d) {
        | EmptyHole(_) => Tag("IE_EmptyHole")
        | NonEmptyHole(_, _, _, d) => generate(d)
        | ExpandingKeyword(_) => Tag("IE_ExpandingKeyword")
        | FreeVar(_, _, name) => Ap(Tag("IE_Var"), StringLit('\"' ++ name ++ '\"'))
        | InvalidText(_) => Tag("IE_InvalidText")
        | InconsistentBranches(_, _, case) =>
          Ap(Tag("IE_Match"), generate_case(case))
        | Closure(_, d) => generate(d)
        | BoundVar(name) => Ap(Tag("IE_Var"), StringLit('\"' ++ name ++ '\"'))
        | Sequence(d1, d2) =>
          Ap(Tag("IE_Sequence"), Tuple([generate(d1), generate(d2)]))
        | Let(dp, d1, d2) =>
          Ap(
            Tag("IE_Let"),
            Tuple([generate_pat(dp), generate(d1), generate(d2)]),
          )
        | FixF(_, _, d) => generate(d)
        | Fun(dp, _, d, v) =>
          let dh_name =
            switch (v) {
            | Some(name) => DHExp.Ap(Tag("Some"), StringLit(name))
            | None => DHExp.Tag("None")
            };
          Ap(
            Tag("IE_Fun"),
            Tuple([generate_pat(dp), generate(d), dh_name]),
          );
        | Ap(d1, d2) =>
          Ap(Tag("IE_Ap"), Tuple([generate(d1), generate(d2)]))
        | ApBuiltin(_) => Tag("IE_ApBuiltin")
        | TestLit(_) => Tag("TestLit")
        | BoolLit(_) => Ap(Tag("IE_Bool"), d)
        | IntLit(_) => Ap(Tag("IE_Int"), d)
        | FloatLit(_) => Ap(Tag("IE_Float"), d)
        | StringLit(_) => Ap(Tag("IE_String"), d)
        | BinBoolOp(_, d1, d2) =>
          Ap(Tag("IE_BinOp"), Tuple([generate(d1), generate(d2)]))
        | BinIntOp(_, d1, d2) =>
          Ap(Tag("IE_BinOp"), Tuple([generate(d1), generate(d2)]))
        | BinFloatOp(_, d1, d2) =>
          Ap(Tag("IE_BinOp"), Tuple([generate(d1), generate(d2)]))
        | BinStringOp(_, d1, d2) =>
          Ap(Tag("IE_BinOp"), Tuple([generate(d1), generate(d2)]))
        | ListLit(v1, v2, _, l) =>
          Ap(
            Tag("IE_ListLit"),
            ListLit(v1, v2, Var("IExp"), List.map(d1 => generate(d1), l)),
          )
        | Cons(d1, d2) =>
          Ap(Tag("IE_Cons"), Tuple([generate(d1), generate(d2)]))
        | Tuple(l) =>
          Ap(Tag("IE_Tuple"), Tuple(List.map(d1 => generate(d1), l)))
        | Prj(d1, _) => Ap(Tag("IE_Prj"), generate(d1))
        | Tag(name) => Ap(Tag("IE_Tag"), StringLit(name))
        | ConsistentCase(case) => Ap(Tag("IE_Match"), generate_case(case))
        | Cast(d1, _, _) => generate(d1)
        | FailedCast(d1, _, _) => generate(d1)
        | InvalidOperation(d1, _) => generate(d1)
        };
      };
      switch (r1) {
      | BoxedValue(d1) => BoxedValue(generate(d1)) |> return
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };
    };
  };

  let pi = name => Builtin.mk_zero(name, Float, Impls.pi);
  let int_of_float = name =>
    Builtin.mk_one(name, Arrow(Float, Int), Impls.int_of_float);
  let float_of_int = name =>
    Builtin.mk_one(name, Arrow(Int, Float), Impls.float_of_int);
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
    |> using("int_of_float", int_of_float)
    |> using("float_of_int", float_of_int)
    |> using("mod", modulo)
    |> using("source", source);
};
