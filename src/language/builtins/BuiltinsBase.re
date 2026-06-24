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

let misc_fns: list(BuiltinsUtil.fn) = [
  {
    /* Println for probes study */

    name: "print",
    arg: Unknown(Internal),
    ret: Prod([]),
    imp: _ => Some(Fresh.Exp.tuple([])),
    custom_statics: None,
  },
];

/* numpy-compatible MT19937 (legacy RandomState), implemented natively for speed.
   Reproduces np.random.RandomState(seed) exactly. 32-bit words held in OCaml's
   63-bit ints, masked with `land 0xFFFFFFFF`. */
let mk_mt19937 = (seed: int): (unit => int) => {
  let mt = Array.make(624, 0);
  mt[0] = seed land 0xFFFFFFFF;
  for (i in 1 to 623) {
    mt[i] =
      (1812433253 * (mt[i - 1] lxor mt[i - 1] lsr 30) + i) land 0xFFFFFFFF;
  };
  let mti = ref(624);
  let mag = y => y land 1 == 1 ? 0x9908B0DF : 0;
  () => {
    if (mti^ >= 624) {
      for (kk in 0 to 226) {
        let y = mt[kk] land 0x80000000 lor (mt[kk + 1] land 0x7FFFFFFF);
        mt[kk] = mt[kk + 397] lxor y lsr 1 lxor mag(y);
      };
      for (kk in 227 to 622) {
        let y = mt[kk] land 0x80000000 lor (mt[kk + 1] land 0x7FFFFFFF);
        mt[kk] = mt[kk - 227] lxor y lsr 1 lxor mag(y);
      };
      let y = mt[623] land 0x80000000 lor (mt[0] land 0x7FFFFFFF);
      mt[623] = mt[396] lxor y lsr 1 lxor mag(y);
      mti := 0;
    };
    let y = ref(mt[mti^]);
    mti := mti^ + 1;
    y := y^ lxor y^ lsr 11;
    y := y^ lxor (y^ lsl 7 land 0x9D2C5680);
    y := y^ lxor (y^ lsl 15 land 0xEFC60000);
    y := y^ lxor y^ lsr 18;
    y^ land 0xFFFFFFFF;
  };
};

/* numpy RandomState(seed).permutation(n): Fisher-Yates with masked-rejection
   bounded draws (rk_interval), 32-bit path. */
let mt19937_permutation = (seed: int, n: int): list(int) => {
  let gen = mk_mt19937(seed);
  let rk_interval = maxv =>
    if (maxv == 0) {
      0;
    } else {
      let mask = ref(maxv);
      mask := mask^ lor mask^ lsr 1;
      mask := mask^ lor mask^ lsr 2;
      mask := mask^ lor mask^ lsr 4;
      mask := mask^ lor mask^ lsr 8;
      mask := mask^ lor mask^ lsr 16;
      let v = ref(gen() land mask^);
      while (v^ > maxv) {
        v := gen() land mask^;
      };
      v^;
    };
  let arr = Array.init(n, i => i);
  for (i in n - 1 downto 1) {
    let j = rk_interval(i);
    let t = arr[i];
    arr[i] = arr[j];
    arr[j] = t;
  };
  Array.to_list(arr);
};

let numeric_fns: list(BuiltinsUtil.fn) = [
  {
    /* np_permutation((seed, n)) == np.random.RandomState(seed).permutation(n).
       Native MT19937 + Fisher-Yates for performance (the pure-Hazel version is
       correct but slow). Used for reproducible train_test_split in da-bench. */
    name: "np_permutation",
    arg: Prod([int(), int()]),
    ret: List(int()),
    imp:
      [@warning "-8"]
      (
        d => {
          let-unbox [ds, dn] = (Tuple(2), d);
          let-unbox sb = (Atom(Int), ds);
          let-unbox nb = (Atom(Int), dn);
          switch (Bigint.to_int(sb), Bigint.to_int(nb)) {
          | (Some(seed), Some(n)) =>
            Some(
              Fresh.Exp.list_lit(
                List.map(
                  i => Fresh.Exp.int(i),
                  mt19937_permutation(seed, n),
                ),
              ),
            )
          | _ => None
          };
        }
      ),
    custom_statics: None,
  },
  {
    name: "is_finite",
    arg: Atom(Float),
    ret: Atom(Bool),
    imp: d => {
      let-unbox f = (Atom(Float), d);
      Some(Exp.bool(Float.is_finite(f)));
    },
    custom_statics: None,
  },
  {
    name: "is_infinite",
    arg: Atom(Float),
    ret: Atom(Bool),
    imp: d => {
      let-unbox f = (Atom(Float), d);
      Some(Exp.bool(Float.is_infinite(f)));
    },
    custom_statics: None,
  },
  {
    name: "is_nan",
    arg: Atom(Float),
    ret: Atom(Bool),
    imp: d => {
      let-unbox f = (Atom(Float), d);
      Some(Exp.bool(Float.is_nan(f)));
    },
    custom_statics: None,
  },
  {
    name: "abs",
    arg: Atom(Int),
    ret: Atom(Int),
    imp: d => {
      let-unbox n = (Atom(Int), d);
      Some(Exp.big_int(Bigint.abs(n)));
    },
    custom_statics: None,
  },
  {
    name: "abs_float",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(abs_float),
    custom_statics: None,
  },
  {
    name: "ceil",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(ceil),
    custom_statics: None,
  },
  {
    name: "floor",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(floor),
    custom_statics: None,
  },
  {
    /* Nearest integer, ties away from zero (matches OCaml's Float.round).
       Round to N decimals by composing: round(x *. 100.0) /. 100.0 */
    name: "round",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(Float.round),
    custom_statics: None,
  },
  {
    name: "exp",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(exp),
    custom_statics: None,
  },
  {
    name: "log",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(log),
    custom_statics: None,
  },
  {
    name: "log10",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(log10),
    custom_statics: None,
  },
  {
    name: "sqrt",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(sqrt),
    custom_statics: None,
  },
  {
    name: "sin",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(sin),
    custom_statics: None,
  },
  {
    name: "cos",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(cos),
    custom_statics: None,
  },
  {
    name: "tan",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(tan),
    custom_statics: None,
  },
  {
    name: "asin",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(asin),
    custom_statics: None,
  },
  {
    name: "acos",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(acos),
    custom_statics: None,
  },
  {
    name: "atan",
    arg: Atom(Float),
    ret: Atom(Float),
    imp: float_op(atan),
    custom_statics: None,
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
    custom_statics: None,
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
    custom_statics: None,
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
    custom_statics: None,
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
    custom_statics: None,
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
    custom_statics: None,
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
    custom_statics: None,
  },
  {
    name: "string_trim",
    arg: Atom(String),
    ret: Atom(String),
    imp: d => {
      let-unbox s = (Atom(String), d);
      Some(Exp.string(String.trim(s)));
    },
    custom_statics: None,
  },
  {
    name: "string_escaped",
    arg: Atom(String),
    ret: Atom(String),
    imp: d => {
      let-unbox s = (Atom(String), d);
      Some(Exp.string(String.escaped(s)));
    },
    custom_statics: None,
  },
  {
    name: "string_unescaped",
    arg: Atom(String),
    ret: Atom(String),
    imp: d => {
      let-unbox s = (Atom(String), d);
      Some(Exp.string(Scanf.unescaped(s)));
    },
    custom_statics: None,
  },
  {
    name: "string_uppercase",
    arg: Atom(String),
    ret: Atom(String),
    imp: d => {
      let-unbox s = (Atom(String), d);
      Some(Exp.string(String.uppercase_ascii(s)));
    },
    custom_statics: None,
  },
  {
    name: "string_lowercase",
    arg: Atom(String),
    ret: Atom(String),
    imp: d => {
      let-unbox s = (Atom(String), d);
      Some(Exp.string(String.lowercase_ascii(s)));
    },
    custom_statics: None,
  },
  {
    name: "string_capitalize",
    arg: Atom(String),
    ret: Atom(String),
    imp: d => {
      let-unbox s = (Atom(String), d);
      Some(Exp.string(String.capitalize_ascii(s)));
    },
    custom_statics: None,
  },
  {
    name: "string_uncapitalize",
    arg: Atom(String),
    ret: Atom(String),
    imp: d => {
      let-unbox s = (Atom(String), d);
      Some(Exp.string(String.uncapitalize_ascii(s)));
    },
    custom_statics: None,
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
    custom_statics: None,
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
    custom_statics: None,
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
    custom_statics: None,
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
    custom_statics: None,
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
    custom_statics: None,
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
    custom_statics: None,
  },
];

let pair_fns: list(BuiltinsUtil.fn) = [
  {
    name: "fst",
    arg: Prod([unknown(Internal), unknown(Internal)]),
    ret: Unknown(Internal),
    imp: d => {
      let-unbox t = (Tuple(2), d);
      switch (t) {
      | [x, _] => Some(x)
      | _ => None
      };
    },
    custom_statics: None,
  },
  {
    name: "snd",
    arg: Prod([unknown(Internal), unknown(Internal)]),
    ret: Unknown(Internal),
    imp: d => {
      let-unbox t = (Tuple(2), d);
      switch (t) {
      | [_, y] => Some(y)
      | _ => None
      };
    },
    custom_statics: None,
  },
];
