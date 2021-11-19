open EvaluatorResult;

module Impl = {
  [@deriving sexp]
  type t = (list(DHExp.t), DHExp.t => EvaluatorResult.t) => EvaluatorResult.t;

  let mk_one_arg = (f, ident, args, evaluate) => {
    let e = DHExp.ApBuiltin(ident, args);
    switch (args) {
    | [] => Indet(e)
    | [d1, ..._] =>
      let d1' = evaluate(d1);
      f(d1', e);
    };
  };

  let int_of_float =
    (
      (d1', e) =>
        switch (d1') {
        | BoxedValue(FloatLit(f)) =>
          let i = int_of_float(f);
          BoxedValue(IntLit(i));
        | _ => Indet(e)
        }
    )
    |> mk_one_arg;

  let float_of_int =
    (
      (d1', e) =>
        switch (d1') {
        | BoxedValue(IntLit(i)) =>
          let f = float_of_int(i);
          BoxedValue(FloatLit(f));
        | _ => Indet(e)
        }
    )
    |> mk_one_arg;

  let string_of_int =
    (
      (d1', e) =>
        switch (d1') {
        | BoxedValue(IntLit(i)) =>
          let s = string_of_int(i) |> UnescapedString.from_string_unchecked;
          BoxedValue(StringLit(s, []));
        | _ => Indet(e)
        }
    )
    |> mk_one_arg;

  let string_of_float =
    (
      (d1', e) =>
        switch (d1') {
        | BoxedValue(FloatLit(f)) =>
          let s = string_of_float(f) |> UnescapedString.from_string_unchecked;
          BoxedValue(StringLit(s, []));
        | _ => Indet(e)
        }
    )
    |> mk_one_arg;

  let string_of_bool =
    (
      (d1', e) =>
        switch (d1') {
        | BoxedValue(BoolLit(b)) =>
          let s = string_of_bool(b) |> UnescapedString.from_string_unchecked;
          BoxedValue(StringLit(s, []));
        | _ => Indet(e)
        }
    )
    |> mk_one_arg;

  let int_of_string =
    (
      (d1', e) =>
        switch (d1') {
        | BoxedValue(StringLit(s, errors)) =>
          switch (errors) {
          | [] =>
            let s = s |> UnescapedString.to_string;
            switch (int_of_string_opt(s)) {
            | Some(i) => BoxedValue(IntLit(i))
            | None => Indet(InvalidOperation(e, InvalidIntOfString))
            };
          | _ => Indet(InvalidOperation(e, InvalidIntOfString))
          }
        | _ => Indet(e)
        }
    )
    |> mk_one_arg;

  let float_of_string =
    (
      (d1', e) =>
        switch (d1') {
        | BoxedValue(StringLit(s, errors)) =>
          switch (errors) {
          | [] =>
            let s = s |> UnescapedString.to_string;
            switch (float_of_string_opt(s)) {
            | Some(f) => BoxedValue(FloatLit(f))
            | None => Indet(InvalidOperation(e, InvalidFloatOfString))
            };
          | _ => Indet(InvalidOperation(e, InvalidFloatOfString))
          }
        | _ => Indet(e)
        }
    )
    |> mk_one_arg;

  let bool_of_string =
    (
      (d1', e) =>
        switch (d1') {
        | BoxedValue(StringLit(s, errors)) =>
          switch (errors) {
          | [] =>
            let s = s |> UnescapedString.to_string;
            switch (bool_of_string_opt(s)) {
            | Some(b) => BoxedValue(BoolLit(b))
            | None => Indet(InvalidOperation(e, InvalidBoolOfString))
            };
          | _ => Indet(InvalidOperation(e, InvalidBoolOfString))
          }
        | _ => Indet(e)
        }
    )
    |> mk_one_arg;

  let string_length =
    (
      (d1', e) =>
        switch (d1') {
        | BoxedValue(StringLit(s, errors) as d1') =>
          switch (errors) {
          | [] => BoxedValue(IntLit(UnescapedString.length(s)))
          | _ => Indet(d1')
          }
        | _ => Indet(e)
        }
    )
    |> mk_one_arg;
};

let builtins: VarMap.t_((HTyp.t, string => Impl.t)) = [
  ("int_of_float", (Arrow(Float, Int), Impl.int_of_float)),
  ("float_of_int", (Arrow(Int, Float), Impl.float_of_int)),
  ("string_of_int", (Arrow(Int, String), Impl.string_of_int)),
  ("int_of_string", (Arrow(String, Int), Impl.int_of_string)),
  ("string_of_float", (Arrow(Float, String), Impl.string_of_float)),
  ("float_of_string", (Arrow(String, Float), Impl.float_of_string)),
  ("string_of_bool", (Arrow(Bool, String), Impl.string_of_bool)),
  ("bool_of_string", (Arrow(String, Bool), Impl.bool_of_string)),
  ("length", (Arrow(String, Int), Impl.string_length)),
];

let ctx: VarCtx.t = List.map(((x, (ty, _))) => (x, ty), builtins);

let impls: VarMap.t_(Impl.t) =
  List.map(((x, (_, impl))) => (x, impl(x)), builtins);

let lookup_type = x => VarMap.lookup(ctx, x);

let lookup_impl = x =>
  VarMap.lookup(builtins, x) |> Option.map(((_, impl)) => impl(x));
