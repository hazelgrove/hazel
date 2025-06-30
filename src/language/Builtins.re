open Util;
open OptUtil.Syntax;

/*
   Built-in functions for Hazel.

   To add a built-in function or constant, write the implementation in the
   `Pervasives.Impls` module below and add it to `builtins`.

   See the existing ones for reference.
 */
module Fresh = IdTagged.FreshGrammar;

[@deriving (show({with_path: false}), sexp)]
type builtin =
  | Const(Typ.t, DHExp.t)
  | Fn(Typ.t, Typ.t, DHExp.t => option(DHExp.t))
  | HazelFn(Typ.t, Typ.t, Exp.t);

[@deriving (show({with_path: false}), sexp)]
type t = VarMap.t_(builtin);

[@deriving (show({with_path: false}), sexp)]
type forms = VarMap.t_(DHExp.t => option(DHExp.t));

exception BuiltinAlreadyDefined(Var.t);

// Like VarMap.extend but it fails if the name is already bound
let extend = (builtins: t, (name: Var.t, v: builtin)): t =>
  if (VarMap.contains(builtins, name)) {
    raise(BuiltinAlreadyDefined(name));
  } else {
    VarMap.extend(builtins, (name, v));
  };

// Like VarMap.concat but it fails if the name is already bound
let concat = (builtins: t, new_builtins: t): t => {
  List.iter(
    ((new_builtin, _)) =>
      if (VarMap.contains(builtins, new_builtin)) {
        raise(BuiltinAlreadyDefined(new_builtin));
      },
    new_builtins,
  );
  VarMap.concat(builtins, new_builtins);
};

let const = (name: Var.t, typ: Typ.term, v: DHExp.t, builtins: t): t =>
  extend(builtins, (name, Const(typ |> Typ.fresh, v)));
let fn =
    (
      name: Var.t,
      t1: Typ.term,
      t2: Typ.term,
      impl: DHExp.t => option(DHExp.t), // None if indet
      builtins: t,
    )
    : t =>
  extend(builtins, (name, Fn(t1 |> Typ.fresh, t2 |> Typ.fresh, impl)));

let hazel_fn =
    (name: Var.t, t1: Typ.term, t2: Typ.term, expr: Exp.t, builtins: t): t =>
  extend(builtins, (name, HazelFn(t1 |> Typ.fresh, t2 |> Typ.fresh, expr)));

let (let-unbox) = ((request, v), f) =>
  switch (Unboxing.unbox(request, v)) {
  | IndetMatch
  | DoesNotMatch => None
  | Matches(n) => f(n)
  };

module Pervasives = {
  module Impls = {
    open Fresh.Exp;
    /* constants */
    let infinity = float(Float.infinity);
    let neg_infinity = float(Float.neg_infinity);
    let nan = float(Float.nan);
    let epsilon_float = float(epsilon_float);
    let pi = float(Float.pi);
    let max_int = sint(Int.max_int);
    let min_int = sint(Int.min_int);

    [@warning "-8"]
    // let-unbox guarantees that the tuple will have length 2
    let binary = (f: (DHExp.t, DHExp.t) => option(DHExp.t), d: DHExp.t) => {
      let-unbox [d1, d2] = (Tuple(2), d);
      f(d1, d2);
    };

    [@warning "-8"]
    // let-unbox guarantees that the tuple will have length 3int
    let ternary =
        (f: (DHExp.t, DHExp.t, DHExp.t) => option(DHExp.t), d: DHExp.t) => {
      let-unbox [d1, d2, d3] = (Tuple(3), d);
      f(d1, d2, d3);
    };

    let is_finite = d => {
      let-unbox f = (Atom(Float), d);
      Some(bool(Float.is_finite(f)));
    };

    let is_infinite = d => {
      let-unbox f = (Atom(Float), d);
      Some(bool(Float.is_infinite(f)));
    };

    let is_nan = d => {
      let-unbox f = (Atom(Float), d);
      Some(bool(Float.is_nan(f)));
    };

    let abs = d => {
      let-unbox n = (Atom(Int), d);
      Some(big_int(Bigint.abs(n)));
    };

    let float_op = (fn, d) => {
      let-unbox f = (Atom(Float), d);
      Some(float(fn(f)));
    };

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

    let int_mod = name =>
      binary((d1, d2) => {
        let-unbox m = (Atom(Int), d1);
        let-unbox n = (Atom(Int), d2);
        if (n == Bigint.zero) {
          Some(
            dynamic_error_hole(
              ap(Forward, builtin_fun(name), d1),
              DivideByZero,
            ),
          );
        } else {
          Some(big_int(Bigint.(%)(m, n)));
        };
      });

    let sint_mod = name =>
      binary((d1, d2) => {
        let-unbox m = (Atom(SInt), d1);
        let-unbox n = (Atom(SInt), d2);
        if (n == 0) {
          Some(
            dynamic_error_hole(
              ap(Forward, builtin_fun(name), d1),
              DivideByZero,
            ),
          );
        } else {
          Some(sint(m mod n));
        };
      });

    let nat_mod = name =>
      binary((d1, d2) => {
        let-unbox m = (Atom(Nat), d1);
        let-unbox n = (Atom(Nat), d2);
        if (n == Bigint.zero) {
          Some(
            dynamic_error_hole(
              ap(Forward, builtin_fun(name), d1),
              DivideByZero,
            ),
          );
        } else {
          Some(nat(Bigint.(%)(m, n)));
        };
      });

    let float_mod = name =>
      binary((d1, d2) => {
        let-unbox m = (Atom(Float), d1);
        let-unbox n = (Atom(Float), d2);
        if (n == 0.0) {
          Some(
            dynamic_error_hole(
              ap(Forward, builtin_fun(name), d1),
              DivideByZero,
            ),
          );
        } else {
          Some(float((Float.modf(m /. n) |> fst) *. n));
        };
      });

    let monus =
      binary((d1, d2) => {
        let-unbox m = (Atom(Nat), d1);
        let-unbox n = (Atom(Nat), d2);
        if (Bigint.(<=)(m, n)) {
          Some(nat(Bigint.zero));
        } else {
          Some(nat(Bigint.(m - n)));
        };
      });

    let string_length = d => {
      let-unbox s = (Atom(String), d);
      Some(int(String.length(s)));
    };

    let string_compare =
      binary((d1, d2) => {
        let-unbox s1 = (Atom(String), d1);
        let-unbox s2 = (Atom(String), d2);
        Some(int(String.compare(s1, s2)));
      });

    let string_trim = d => {
      let-unbox s = (Atom(String), d);
      Some(string(String.trim(s)));
    };

    let string_of: DHExp.t => option(string) =
      d => {
        let-unbox s = (Atom(String), d);
        Some(s);
      };

    let string_join =
      binary((d1, d2) => {
        let-unbox s1 = (Atom(String), d1);
        let-unbox xs = (ListLit, d2);
        let* xs' = List.map(string_of, xs) |> Util.OptUtil.sequence;
        Some(string(String.concat(s1, xs')));
      });

    let string_sub = name =>
      ternary((d1, d2, d3) => {
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
          let d' = BuiltinFun(name) |> DHExp.fresh;
          let d' = Ap(Forward, d', d1) |> DHExp.fresh;
          let d' = DynamicErrorHole(d', IndexOutOfBounds) |> DHExp.fresh;
          Some(d');
        };
      });

    let string_split = _ =>
      binary((d1, d2) => {
        let-unbox s = (Atom(String), d1);
        let-unbox sep = (Atom(String), d2);
        let split_str = Util.StringUtil.plain_split(sep, s);
        let split_str' = List.map(s => string(s), split_str);
        Some(list_lit(split_str'));
      });
  };

  open Impls;

  // Update src/menhirParser/Lexer.mll when any new builtin is added

  // Convert ListBuiltins.fn records to Builtins.builtin values
  let of_list_builtin = (list_fn: ListBuiltins.fn): builtin => {
    HazelFn(list_fn.arg |> Typ.fresh, list_fn.ret |> Typ.fresh, list_fn.imp);
  };

  // Add list builtins to the main builtins map
  let add_list_builtins = (builtins: t): t => {
    ListBuiltins.builtins
    |> List.map((list_fn: ListBuiltins.fn) =>
         (list_fn.name, of_list_builtin(list_fn))
       )
    |> List.fold_left(
         (acc, (name, builtin)) => extend(acc, (name, builtin)),
         builtins,
       );
  };

  let of_atom_builtin = (b: Atom.builtin): builtin => {
    switch (b) {
    | OneFun(k1, k2, f) =>
      Fn(
        Atom(k1 |> Atom.cls_of_kind) |> Typ.fresh,
        Atom(k2 |> Atom.cls_of_kind) |> Typ.fresh,
        (d: DHExp.t) => {
          let-unbox x = (Atom(k1), d);
          switch (f(x)) {
          | L(x) => Some(Atom(Atom.repack(k2, x)) |> Exp.fresh)
          | R(_) => None
          };
        },
      )
    | TwoFun(k1, k2, k3, f) =>
      Fn(
        Prod([
          Atom(k1 |> Atom.cls_of_kind) |> Typ.fresh,
          Atom(k2 |> Atom.cls_of_kind) |> Typ.fresh,
        ])
        |> Typ.fresh,
        Atom(k3 |> Atom.cls_of_kind) |> Typ.fresh,
        [@warning "-8"] (d: DHExp.t) => {
          let-unbox [x, y] = (Tuple(2), d);
          let-unbox x = (Atom(k1), x);
          let-unbox y = (Atom(k2), y);
          switch (f(x, y)) {
          | L(x) => Some(Atom(Atom.repack(k3, x)) |> Exp.fresh)
          | R(_) => None
          };
        },
      )
    };
  };

  let builtins =
    Fresh.Typ.(
      VarMap.empty
      |> const("infinity", Atom(Float), infinity)
      |> const("neg_infinity", Atom(Float), neg_infinity)
      |> const("nan", Atom(Float), nan)
      |> const("epsilon_float", Atom(Float), epsilon_float)
      |> const("pi", Atom(Float), pi)
      |> const("max_sint", Atom(SInt), max_int)
      |> const("min_sint", Atom(SInt), min_int)
      |> fn("is_finite", Atom(Float), Atom(Bool), is_finite)
      |> fn("is_infinite", Atom(Float), Atom(Bool), is_infinite)
      |> fn("is_nan", Atom(Float), Atom(Bool), is_nan)
      |> fn("abs", Atom(Int), Atom(Int), abs)
      |> fn("abs_float", Atom(Float), Atom(Float), abs_float)
      |> fn("ceil", Atom(Float), Atom(Float), ceil)
      |> fn("floor", Atom(Float), Atom(Float), floor)
      |> fn("exp", Atom(Float), Atom(Float), exp)
      |> fn("log", Atom(Float), Atom(Float), log)
      |> fn("log10", Atom(Float), Atom(Float), log10)
      |> fn("sqrt", Atom(Float), Atom(Float), sqrt)
      |> fn("sin", Atom(Float), Atom(Float), sin)
      |> fn("cos", Atom(Float), Atom(Float), cos)
      |> fn("tan", Atom(Float), Atom(Float), tan)
      |> fn("asin", Atom(Float), Atom(Float), asin)
      |> fn("acos", Atom(Float), Atom(Float), acos)
      |> fn("atan", Atom(Float), Atom(Float), atan)
      |> fn("monus", Prod([nat(), nat()]), Atom(Nat), monus)
      |> fn("int_mod", Prod([int(), int()]), Atom(Int), int_mod("mod"))
      |> fn(
           "sint_mod",
           Prod([sint(), sint()]),
           Atom(SInt),
           sint_mod("mod"),
         )
      |> fn("nat_mod", Prod([nat(), nat()]), Atom(Nat), nat_mod("mod"))
      |> fn(
           "float_mod",
           Prod([float(), float()]),
           Atom(Float),
           float_mod("mod"),
         )
      |> fn("string_length", Atom(String), Atom(Int), string_length)
      |> fn(
           "string_compare",
           Prod([string(), string()]),
           Atom(Int),
           string_compare,
         )
      |> fn("string_trim", Atom(String), Atom(String), string_trim)
      |> fn(
           "string_join",
           Prod([string(), list(string())]),
           Atom(String),
           string_join,
         )
      |> fn(
           "string_sub",
           Prod([string(), int(), int()]),
           Atom(String),
           string_sub("string_sub"),
         )
      |> fn(
           "string_split",
           Prod([string(), string()]),
           List(string()),
           string_split("string_split"),
         )
    )
    |> concat(
         _,
         List.map(
           ((n, b)) => (n, of_atom_builtin(b)),
           Atom.converter_builtins,
         ),
       )
    |> concat(
         _,
         List.map(
           ((n, b)) => (n, of_atom_builtin(b)),
           Operators.builtins,
         ),
       )
    |> add_list_builtins;
};

module TypeAliases = {
  // Helper function to create a type alias entry
  let create_type_alias = (name: string, typ: Typ.t): Ctx.entry => {
    Ctx.TVarEntry({
      name,
      id: Id.invalid,
      kind: Ctx.Singleton(typ),
    });
  };

  // Helper function to create constructor map for sum types
  let create_constructor_map =
      (variants: list((string, option(Typ.t)))): ConstructorMap.t(Typ.t) => {
    List.map(
      ((name, typ_opt)) =>
        ConstructorMap.Variant(name, [Id.mk()], typ_opt),
      variants,
    );
  };

  // Option type: None + Some(?)
  let option_type: Typ.t = {
    let option_cons_map =
      create_constructor_map([
        ("None", None),
        ("Some", Some(Unknown(Internal) |> Typ.fresh)),
      ]);
    Fresh.Typ.sum(option_cons_map);
  };

  // List of type aliases to add to the context
  let type_aliases: list((string, Typ.t)) = [("Option", option_type)];

  // Convert type aliases to context entries
  let entries: list(Ctx.entry) =
    List.map(((name, typ)) => create_type_alias(name, typ), type_aliases);

  // Add constructors for type aliases to the context
  let add_constructors = (ctx: Ctx.t): Ctx.t => {
    List.fold_left(
      (ctx, (name, typ)) => {
        let cons_map =
          switch (Typ.term_of(typ)) {
          | Sum(cons_map) => cons_map
          | _ => failwith("Type alias must be a sum type")
          };
        Ctx.add_ctrs(ctx, name, Id.invalid, cons_map);
      },
      ctx,
      type_aliases,
    );
  };
};

let livelits_init =
  Livelit.livelits |> List.map(entry => Ctx.LivelitEntry(entry));

let entries =
  TypeAliases.entries
  |> List.append(
       List.map(
         fun
         | (name, Const(typ, _)) =>
           Ctx.VarEntry({
             name,
             typ,
             id: Id.invalid,
           })
         | (name, Fn(t1, t2, _)) =>
           Ctx.VarEntry({
             name,
             typ: Fresh.Typ.arrow(t1, t2),
             id: Id.invalid,
           })
         | (name, HazelFn(t1, t2, _)) =>
           Ctx.VarEntry({
             name,
             typ: Fresh.Typ.arrow(t1, t2),
             id: Id.invalid,
           }),
         Pervasives.builtins,
       ),
     )
  |> List.append(
       Livelit.livelits |> List.map(entry => Ctx.LivelitEntry(entry)),
     );

let ctx_init: option(Operators.mode) => Ctx.t =
  use_mode => {
    let meta_cons_map: ConstructorMap.t(Typ.t) = [
      Variant("$e", [Id.mk()], None),
      Variant("$v", [Id.mk()], None),
    ];
    let meta =
      Ctx.TVarEntry({
        name: "$Meta",
        id: Id.invalid,
        kind: Ctx.Singleton(Fresh.Typ.sum(meta_cons_map)),
      });
    Ctx.{
      use_mode,
      entries:
        TypeAliases.add_constructors({
          use_mode,
          entries: [],
        }).
          entries
        @ entries,
    }
    |> Ctx.extend(_, meta)
    |> Ctx.add_ctrs(_, "$Meta", Id.invalid, meta_cons_map);
  };

let forms_init: forms =
  List.filter_map(
    fun
    | (_, Const(_)) => None
    | (name, Fn(_, _, f)) => Some((name, f))
    | (name, HazelFn(_, _, f)) =>
      Some((
        name,
        (
          (d: DHExp.t) => {
            Some(Fresh.Exp.ap(Forward, f, d));
          }
        ),
      )),
    Pervasives.builtins,
  );

let env_init: Environment.t =
  List.fold_left(
    env =>
      fun
      | (name, Const(_, d)) => Environment.extend(env, (name, d))
      | (name, Fn(_)) =>
        Environment.extend(env, (name, Fresh.Exp.builtin_fun(name)))
      | (name, HazelFn(_, _, _)) =>
        Environment.extend(env, (name, Fresh.Exp.builtin_fun(name))),
    Environment.empty,
    Pervasives.builtins,
  );
