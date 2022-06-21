/*
   Built-in functions for Hazel.

   To add a built-in function or constant, write the implementation in the
   `Impls` module below and add it to the `builtins` list.

   See the existing ones for reference.
 */

module Impls = {
  open EvaluatorResult;

  /* int_of_float implementation. */
  let int_of_float = (ident, r1) =>
    switch (r1) {
    | BoxedValue(FloatLit(f)) =>
      let i = int_of_float(f);
      BoxedValue(IntLit(i));
    | BoxedValue(d1) =>
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1)))
    | Indet(d1) => Indet(ApBuiltin(ident, [d1]))
    };

  /* float_of_int implementation. */
  let float_of_int = (ident, r1) =>
    switch (r1) {
    | BoxedValue(IntLit(i)) =>
      let f = float_of_int(i);
      BoxedValue(FloatLit(f));
    | BoxedValue(d1) =>
      raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1)))
    | Indet(d1) => Indet(ApBuiltin(ident, [d1]))
    };

  /* mod implementation */
  let int_mod = (ident, r1, r2) =>
    switch (r1) {
    | BoxedValue(IntLit(n) as d1) =>
      switch (r2) {
      | BoxedValue(IntLit(m) as d2) =>
        switch (n, m) {
        | (_, 0) =>
          Indet(InvalidOperation(ApBuiltin(ident, [d1, d2]), DivideByZero))
        | (n, m) => BoxedValue(IntLit(n mod m))
        }
      | BoxedValue(d2) =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2)))
      | Indet(d2) => Indet(ApBuiltin(ident, [d1, d2]))
      }
    | BoxedValue(d1) =>
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1)))
    | Indet(d1) =>
      switch (r2) {
      | BoxedValue(d2)
      | Indet(d2) => Indet(ApBuiltin(ident, [d1, d2]))
      }
    };

  let string_of_int = (ident, r1) =>
    switch (r1) {
    | BoxedValue(IntLit(i)) =>
      let s = string_of_int(i) |> UnescapedString.from_string_unchecked;
      BoxedValue(StringLit({str: s, vseqs: [], iseqs: []}));
    | BoxedValue(d1) =>
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1)))
    | Indet(d1) => Indet(ApBuiltin(ident, [d1]))
    };

  let string_of_float = (ident, r1) =>
    switch (r1) {
    | BoxedValue(FloatLit(f)) =>
      let s = string_of_float(f) |> UnescapedString.from_string_unchecked;
      BoxedValue(StringLit({str: s, vseqs: [], iseqs: []}));
    | BoxedValue(d1) =>
      raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1)))
    | Indet(d1) => Indet(ApBuiltin(ident, [d1]))
    };

  let string_of_bool = (ident, r1) =>
    switch (r1) {
    | BoxedValue(BoolLit(b)) =>
      let s = string_of_bool(b) |> UnescapedString.from_string_unchecked;
      BoxedValue(StringLit({str: s, vseqs: [], iseqs: []}));
    | BoxedValue(d1) =>
      raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1)))
    | Indet(d1) => Indet(ApBuiltin(ident, [d1]))
    };

  let int_of_string = (ident, r1) =>
    switch (r1) {
    | BoxedValue(StringLit({str: s, vseqs: _, iseqs}) as d1) =>
      switch (iseqs) {
      | [] =>
        let s = s |> UnescapedString.to_string;
        switch (int_of_string_opt(s)) {
        | Some(i) => BoxedValue(IntLit(i))
        | None => Indet(InvalidOperation(d1, InvalidIntOfString))
        };
      | _ => Indet(InvalidOperation(d1, InvalidIntOfString))
      }
    | BoxedValue(d1) =>
      raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1)))
    | Indet(d1) => Indet(ApBuiltin(ident, [d1]))
    };

  let float_of_string = (ident, r1) =>
    switch (r1) {
    | BoxedValue(StringLit({str: s, vseqs: _, iseqs}) as d1) =>
      switch (iseqs) {
      | [] =>
        let s = s |> UnescapedString.to_string;
        switch (float_of_string_opt(s)) {
        | Some(f) => BoxedValue(FloatLit(f))
        | None => Indet(InvalidOperation(d1, InvalidFloatOfString))
        };
      | _ => Indet(InvalidOperation(d1, InvalidFloatOfString))
      }
    | BoxedValue(d1) =>
      raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1)))
    | Indet(d1) => Indet(ApBuiltin(ident, [d1]))
    };

  let bool_of_string = (ident, r1) =>
    switch (r1) {
    | BoxedValue(StringLit({str: s, vseqs: _, iseqs}) as d1) =>
      switch (iseqs) {
      | [] =>
        let s = s |> UnescapedString.to_string;
        switch (bool_of_string_opt(s)) {
        | Some(b) => BoxedValue(BoolLit(b))
        | None => Indet(InvalidOperation(d1, InvalidBoolOfString))
        };
      | _ => Indet(InvalidOperation(d1, InvalidBoolOfString))
      }
    | BoxedValue(d1) =>
      raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1)))
    | Indet(d1) => Indet(ApBuiltin(ident, [d1]))
    };

  let string_length = (ident, r1) =>
    switch (r1) {
    | BoxedValue(StringLit({str: s, vseqs: _, iseqs}) as d1) =>
      switch (iseqs) {
      | [] => BoxedValue(IntLit(UnescapedString.length(s)))
      | _ => Indet(d1)
      }
    | BoxedValue(d1) =>
      raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1)))
    | Indet(d1) => Indet(ApBuiltin(ident, [d1]))
    };

  /* PI implementation. */
  let pi = DHExp.FloatLit(Float.pi);
};

let builtins: list(Builtin.t) = [
  Builtin.mk_zero("PI", Float, Impls.pi),
  Builtin.mk_one("int_of_float", Arrow(Float, Int), Impls.int_of_float),
  Builtin.mk_one("float_of_int", Arrow(Int, Float), Impls.float_of_int),
  Builtin.mk_two("mod", Arrow(Int, Arrow(Int, Int)), Impls.int_mod),
  Builtin.mk_one("string_of_int", Arrow(Int, String), Impls.string_of_int),
  Builtin.mk_one(
    "string_of_float",
    Arrow(Float, String),
    Impls.string_of_float,
  ),
  Builtin.mk_one(
    "string_of_bool",
    Arrow(Bool, String),
    Impls.string_of_bool,
  ),
  Builtin.mk_one("int_of_string", Arrow(String, Int), Impls.int_of_string),
  Builtin.mk_one(
    "float_of_string",
    Arrow(String, Float),
    Impls.float_of_string,
  ),
  Builtin.mk_one(
    "bool_of_string",
    Arrow(String, Bool),
    Impls.bool_of_string,
  ),
  Builtin.mk_one("length", Arrow(String, Int), Impls.string_length),
];

let ctx: VarCtx.t =
  List.map(({ident, ty, _}: Builtin.t) => (ident, ty), builtins);
let forms =
  List.map(
    ({ident, ty: _ty, eval, elab}: Builtin.t) => (ident, (eval, elab)),
    builtins,
  );

let lookup_type = VarMap.lookup(ctx);
let lookup_form = VarMap.lookup(forms);
