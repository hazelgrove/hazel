open Util;
open OptUtil.Syntax;
open BuiltinsUtil;

module Fresh = IdTagged.FreshGrammar;

open Fresh.Typ;
open Fresh;

let numeric_constants =
  Fresh.Exp.[
    {
      name: "infinity",
      typ: Atom(Float),
      imp: float(Float.infinity),
    },
    {
      name: "neg_infinity",
      typ: Atom(Float),
      imp: float(Float.neg_infinity),
    },
    {
      name: "nan",
      typ: Atom(Float),
      imp: float(Float.nan),
    },
    {
      name: "epsilon_float",
      typ: Atom(Float),
      imp: float(epsilon_float),
    },
    {
      name: "pi",
      typ: Atom(Float),
      imp: float(Float.pi),
    },
    {
      name: "max_sint",
      typ: Atom(SInt),
      imp: sint(Int.max_int),
    },
    {
      name: "min_sint",
      typ: Atom(SInt),
      imp: sint(Int.min_int),
    },
  ];

let numeric_fns: list(BuiltinsUtil.fn) = [
  {
    name: "is_finite",
    arg: Atom(Float),
    ret: Atom(Bool),
    imp: d => {
      let-unbox f = (Atom(Float), d);
      Some(Exp.bool(Float.is_finite(f)));
    },
  },
  {
    name: "is_infinite",
    arg: Atom(Float),
    ret: Atom(Bool),
    imp: d => {
      let-unbox f = (Atom(Float), d);
      Some(Exp.bool(Float.is_infinite(f)));
    },
  },
  {
    name: "is_nan",
    arg: Atom(Float),
    ret: Atom(Bool),
    imp: d => {
      let-unbox f = (Atom(Float), d);
      Some(Exp.bool(Float.is_nan(f)));
    },
  },
  {
    name: "abs",
    arg: Atom(Int),
    ret: Atom(Int),
    imp: d => {
      let-unbox n = (Atom(Int), d);
      Some(Exp.big_int(Bigint.abs(n)));
    },
  },
  {
    name: "abs_float",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(abs_float),
  },
  {
    name: "ceil",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(ceil),
  },
  {
    name: "floor",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(floor),
  },
  {
    name: "exp",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(exp),
  },
  {
    name: "log",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(log),
  },
  {
    name: "log10",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(log10),
  },
  {
    name: "sqrt",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(sqrt),
  },
  {
    name: "sin",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(sin),
  },
  {
    name: "cos",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(cos),
  },
  {
    name: "tan",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(tan),
  },
  {
    name: "asin",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(asin),
  },
  {
    name: "acos",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(acos),
  },
  {
    name: "atan",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(atan),
  },
  {
    name: "monus",
    arg: Prod([nat(), nat()]),
    ret: Atom(Nat),
    imp:
      binary((d1, d2) => {
        let-unbox m = (Atom(Nat), d1);
        let-unbox n = (Atom(Nat), d2);
        if (Bigint.(<=)(m, n)) {
          Some(Exp.nat(Bigint.zero));
        } else {
          Some(Exp.nat(Bigint.(m - n)));
        };
      }),
  },
  {
    name: "int_mod",
    arg: Prod([int(), int()]),
    ret: Atom(Int),
    imp:
      binary((d1, d2) => {
        open Exp;
        let-unbox m = (Atom(Int), d1);
        let-unbox n = (Atom(Int), d2);
        if (n == Bigint.zero) {
          Some(
            dynamic_error_hole(
              ap(Forward, builtin_fun("mod"), d1),
              DivideByZero,
            ),
          );
        } else {
          Some(big_int(Bigint.(%)(m, n)));
        };
      }),
  },
  {
    name: "sint_mod",
    arg: Prod([sint(), sint()]),
    ret: Atom(SInt),
    imp:
      binary((d1, d2) => {
        open Exp;
        let-unbox m = (Atom(SInt), d1);
        let-unbox n = (Atom(SInt), d2);
        if (n == 0) {
          Some(
            dynamic_error_hole(
              ap(Forward, builtin_fun("mod"), d1),
              DivideByZero,
            ),
          );
        } else {
          Some(sint(m mod n));
        };
      }),
  },
  {
    name: "nat_mod",
    arg: Prod([nat(), nat()]),
    ret: Atom(Nat),
    imp:
      binary((d1, d2) => {
        open Exp;
        let-unbox m = (Atom(Nat), d1);
        let-unbox n = (Atom(Nat), d2);
        if (n == Bigint.zero) {
          Some(
            dynamic_error_hole(
              ap(Forward, builtin_fun("mod"), d1),
              DivideByZero,
            ),
          );
        } else {
          Some(nat(Bigint.(%)(m, n)));
        };
      }),
  },
  {
    name: "float_mod",
    arg: Prod([float(), float()]),
    ret: Atom(Float),
    imp:
      binary((d1, d2) => {
        open Exp;
        let-unbox m = (Atom(Float), d1);
        let-unbox n = (Atom(Float), d2);
        if (n == 0.0) {
          Some(
            dynamic_error_hole(
              ap(Forward, builtin_fun("mod"), d1),
              DivideByZero,
            ),
          );
        } else {
          Some(float((Float.modf(m /. n) |> fst) *. n));
        };
      }),
  },
];

let string_fns: list(BuiltinsUtil.fn) = [
  {
    name: "string_length",
    arg: Atom(String),
    ret: Atom(Int),
    imp: d => {
      let-unbox s = (Atom(String), d);
      Some(Exp.int(String.length(s)));
    },
  },
  {
    name: "string_compare",
    arg: Prod([string(), string()]),
    ret: Atom(Int),
    imp:
      binary((d1, d2) => {
        let-unbox s1 = (Atom(String), d1);
        let-unbox s2 = (Atom(String), d2);
        Some(Exp.int(String.compare(s1, s2)));
      }),
  },
  {
    name: "string_trim",
    arg: Atom(String),
    ret: Atom(String),
    imp: d => {
      let-unbox s = (Atom(String), d);
      Some(Exp.string(String.trim(s)));
    },
  },
  {
    name: "string_escaped",
    arg: Atom(String),
    ret: Atom(String),
    imp: d => {
      let-unbox s = (Atom(String), d);
      Some(Exp.string(String.escaped(s)));
    },
  },
  {
    name: "string_unescaped",
    arg: Atom(String),
    ret: Atom(String),
    imp: d => {
      let-unbox s = (Atom(String), d);
      Some(Exp.string(Scanf.unescaped(s)));
    },
  },
  {
    name: "string_uppercase",
    arg: Atom(String),
    ret: Atom(String),
    imp: d => {
      let-unbox s = (Atom(String), d);
      Some(Exp.string(String.uppercase_ascii(s)));
    },
  },
  {
    name: "string_lowercase",
    arg: Atom(String),
    ret: Atom(String),
    imp: d => {
      let-unbox s = (Atom(String), d);
      Some(Exp.string(String.lowercase_ascii(s)));
    },
  },
  {
    name: "string_capitalize",
    arg: Atom(String),
    ret: Atom(String),
    imp: d => {
      let-unbox s = (Atom(String), d);
      Some(Exp.string(String.capitalize_ascii(s)));
    },
  },
  {
    name: "string_uncapitalize",
    arg: Atom(String),
    ret: Atom(String),
    imp: d => {
      let-unbox s = (Atom(String), d);
      Some(Exp.string(String.uncapitalize_ascii(s)));
    },
  },
  {
    name: "string_join",
    arg: Prod([string(), list(string())]),
    ret: Atom(String),
    imp:
      binary((d1, d2) => {
        let string_of: DHExp.t => option(string) =
          d => {
            let-unbox s = (Atom(String), d);
            Some(s);
          };
        let-unbox s1 = (Atom(String), d1);
        let-unbox xs = (ListLit, d2);
        let* xs' = List.map(string_of, xs) |> Util.OptUtil.sequence;
        Some(Exp.string(String.concat(s1, xs')));
      }),
  },
  {
    name: "string_sub",
    arg: Prod([string(), int(), int()]),
    ret: Atom(String),
    imp:
      ternary((d1, d2, d3) => {
        open Exp;
        let-unbox s = (Atom(String), d1);
        let-unbox idx = (Atom(Int), d2);
        let-unbox len = (Atom(Int), d3);
        try(
          Some(
            string(
              String.sub(
                s,
                idx |> Bigint.to_int |> Option.get,
                len |> Bigint.to_int |> Option.get,
              ),
            ),
          )
        ) {
        | Invalid_argument(_) =>
          let d' = BuiltinFun("string_sub") |> DHExp.fresh;
          let d' = Ap(Forward, d', d1) |> DHExp.fresh;
          let d' = DynamicErrorHole(d', IndexOutOfBounds) |> DHExp.fresh;
          Some(d');
        };
      }),
  },
  {
    name: "string_split",
    arg: Prod([string(), string()]),
    ret: List(string()),
    imp:
      binary((d1, d2) => {
        open Exp;
        let-unbox s = (Atom(String), d1);
        let-unbox sep = (Atom(String), d2);
        let split_str = StringUtil.plain_split(sep, s);
        let split_str' = List.map(s => string(s), split_str);
        Some(list_lit(split_str'));
      }),
  },
  {
    name: "string_match",
    arg: Prod([string(), string()]),
    ret: Atom(Bool),
    imp:
      binary((d1, d2) => {
        let-unbox regexp = (Atom(String), d1);
        let-unbox str = (Atom(String), d2);
        Some(Exp.bool(StringUtil.plain_match(regexp, str)));
      }),
  },
  {
    name: "string_replace",
    arg: Prod([string(), string(), string()]),
    ret: Atom(String),
    imp:
      ternary((d1, d2, d3) => {
        let-unbox regexp = (Atom(String), d1);
        let-unbox str = (Atom(String), d2);
        let-unbox repl = (Atom(String), d3);
        Some(Exp.string(StringUtil.plain_replace(regexp, str, repl)));
      }),
  },
  {
    name: "string_search",
    arg: Prod([string(), string(), int()]),
    ret: Atom(Int),
    imp:
      ternary((d1, d2, d3) => {
        /* Returns index; -1 if not found */
        let-unbox regexp = (Atom(String), d1);
        let-unbox str = (Atom(String), d2);
        let-unbox idx = (Atom(Int), d3);
        Some(
          Exp.int(
            switch (Bigint.to_int(idx)) {
            | None => (-1)
            | Some(idx) => StringUtil.plain_search(regexp, str, idx)
            },
          ),
        );
      }),
  },
];

// Note: With asymmetric unknown types this form of polymorphism is disallowed, these funs shoulder
let pair_fns: list(BuiltinsUtil.fn) = [
  {
    name: "fst",
    arg: Prod([unknown(Ana), unknown(Ana)]),
    ret: Unknown(Ana),
    imp: d => {
      let-unbox t = (Tuple(2), d);
      switch (t) {
      | [x, _] => Some(x)
      | _ => None
      };
    },
  },
  {
    name: "snd",
    arg: Prod([unknown(Ana), unknown(Ana)]),
    ret: Unknown(Ana),
    imp: d => {
      let-unbox t = (Tuple(2), d);
      switch (t) {
      | [_, y] => Some(y)
      | _ => None
      };
    },
  },
];
