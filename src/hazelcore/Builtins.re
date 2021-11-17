open EvaluatorResult;

module Impl = {
  [@deriving sexp]
  type t = (list(DHExp.t), DHExp.t => EvaluatorResult.t) => EvaluatorResult.t;

  let int_of_float = (ident, args, evaluate) =>
    switch (args) {
    | [] => Indet(ApBuiltin(ident, args))
    | [d1, ..._] =>
      switch (evaluate(d1)) {
      | BoxedValue(FloatLit(f)) =>
        let i = int_of_float(f);
        BoxedValue(IntLit(i));
      | _ => Indet(ApBuiltin(ident, args))
      }
    };

  let float_of_int = (ident, args, evaluate) =>
    switch (args) {
    | [] => Indet(ApBuiltin(ident, args))
    | [d1, ..._] =>
      switch (evaluate(d1)) {
      | BoxedValue(IntLit(i)) =>
        let f = float_of_int(i);
        BoxedValue(FloatLit(f));
      | _ => Indet(ApBuiltin(ident, args))
      }
    };

  let string_of_int = (ident, args, evaluate) =>
    switch (args) {
    | [] => Indet(ApBuiltin(ident, args))
    | [d1, ..._] =>
      switch (evaluate(d1)) {
      | BoxedValue(IntLit(i)) =>
        let s = string_of_int(i) |> UnescapedString.from_string_unchecked;
        BoxedValue(StringLit(s));
      | _ => Indet(ApBuiltin(ident, args))
      }
    };

  let string_of_float = (ident, args, evaluate) =>
    switch (args) {
    | [] => Indet(ApBuiltin(ident, args))
    | [d1, ..._] =>
      switch (evaluate(d1)) {
      | BoxedValue(FloatLit(f)) =>
        let s = string_of_float(f) |> UnescapedString.from_string_unchecked;
        BoxedValue(StringLit(s));
      | _ => Indet(ApBuiltin(ident, args))
      }
    };

  let string_of_bool = (ident, args, evaluate) =>
    switch (args) {
    | [] => Indet(ApBuiltin(ident, args))
    | [d1, ..._] =>
      switch (evaluate(d1)) {
      | BoxedValue(BoolLit(b)) =>
        let s = string_of_bool(b) |> UnescapedString.from_string_unchecked;
        BoxedValue(StringLit(s));
      | _ => Indet(ApBuiltin(ident, args))
      }
    };

  let int_of_string = (ident, args, evaluate) =>
    switch (args) {
    | [] => Indet(ApBuiltin(ident, args))
    | [d1, ..._] =>
      switch (evaluate(d1)) {
      | BoxedValue(StringLit(s)) =>
        let s = s |> UnescapedString.to_string;
        switch (int_of_string_opt(s)) {
        | Some(i) => BoxedValue(IntLit(i))
        | None =>
          Indet(
            InvalidOperation(ApBuiltin(ident, args), InvalidIntOfString),
          )
        };
      | _ => Indet(ApBuiltin(ident, args))
      }
    };
};

let builtins: VarMap.t_((HTyp.t, string => Impl.t)) = [
  ("int_of_float", (Arrow(Float, Int), Impl.int_of_float)),
  ("float_of_int", (Arrow(Int, Float), Impl.float_of_int)),
  ("string_of_int", (Arrow(Int, String), Impl.string_of_int)),
  ("int_of_string", (Arrow(String, Int), Impl.int_of_string)),
  ("string_of_float", (Arrow(Float, String), Impl.string_of_float)),
  ("string_of_bool", (Arrow(Bool, String), Impl.string_of_bool)),
];

let ctx: VarCtx.t = List.map(((x, (ty, _))) => (x, ty), builtins);

let impls: VarMap.t_(Impl.t) =
  List.map(((x, (_, impl))) => (x, impl(x)), builtins);

let lookup_type = x => VarMap.lookup(ctx, x);

let lookup_impl = x =>
  VarMap.lookup(builtins, x) |> Option.map(((_, impl)) => impl(x));
