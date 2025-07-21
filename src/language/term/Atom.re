open Util;

/* ========== DEFINITIONS ========== */

[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Int
  | SInt
  | Nat
  | Float
  | Bool
  | String
  | RegExp;

/* This type is like cls, but each variant is associated with a type, allowing
   us to use the ocaml type checker to check we're using payloads correctly */
type kind('a) =
  | Int: kind(Bigint.t)
  | SInt: kind(int)
  | Nat: kind(Bigint.t)
  | Float: kind(float)
  | Bool: kind(bool)
  | String: kind(string)
  | RegExp: kind(StringUtil.regexp);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Int(Bigint.t)
  | SInt(int)
  | Nat(Bigint.t)
  | Float
      // This equality condition is used to say that two floats are equal if they are equal in the ExpToSegment serialization
      (
        [@equal (a, b) => Printf.(sprintf("%f", a) == sprintf("%f", b))] float,
      )
  | Bool(bool)
  | String(string)
  | RegExp(StringUtil.regexp);

let cls_of_kind = (type a, kind: kind(a)): cls =>
  switch (kind) {
  | Int => Int
  | SInt => SInt
  | Nat => Nat
  | Float => Float
  | Bool => Bool
  | String => String
  | RegExp => RegExp
  };

let cls_of_t: t => cls =
  fun
  | Int(_) => Int
  | SInt(_) => SInt
  | Nat(_) => Nat
  | Float(_) => Float
  | Bool(_) => Bool
  | String(_) => String
  | RegExp(_) => RegExp;

let cls_string_lower: cls => string =
  fun
  | Int => "int"
  | SInt => "sint"
  | Nat => "nat"
  | Float => "float"
  | Bool => "bool"
  | String => "string"
  | RegExp => "regexp";

/* ========== MATCHING ========== */

let unbox = (type a, request: kind(a), e: t): option(a) =>
  switch (request, e) {
  | (Int, Int(i)) => Some(i)
  | (Int, _) => None
  | (SInt, SInt(i)) => Some(i)
  | (SInt, _) => None
  | (Nat, Nat(i)) => Some(i)
  | (Nat, _) => None
  | (Float, Float(f)) => Some(f)
  | (Float, _) => None
  | (Bool, Bool(b)) => Some(b)
  | (Bool, _) => None
  | (String, String(s)) => Some(s)
  | (String, _) => None
  | (RegExp, RegExp(r)) => Some(r)
  | (RegExp, _) => None
  };

// Note[Matt]: return wrapper needed for polymorphic types
type wrapper =
  | V('a, kind('a)): wrapper;

let unpack = (e: t): wrapper =>
  switch (e) {
  | Int(i) => V(i, Int)
  | SInt(i) => V(i, SInt)
  | Nat(i) => V(i, Nat)
  | Float(f) => V(f, Float)
  | Bool(b) => V(b, Bool)
  | String(s) => V(s, String)
  | RegExp(r) => V(r, RegExp)
  };

let repack = (type a, kind: kind(a), x: a): t =>
  switch (kind) {
  | Int => Int(x)
  | SInt => SInt(x)
  | Nat => Nat(x)
  | Float => Float(x)
  | Bool => Bool(x)
  | String => String(x)
  | RegExp => RegExp(x)
  };

// Mpte[Matt]: return wrapper needed for polymorphic types
type cls_wrapper =
  | W(kind('a)): cls_wrapper;

let (let.cls) = (type b, cls: cls, f: cls_wrapper => b): b =>
  switch (cls) {
  | Int => f(W(Int))
  | SInt => f(W(SInt))
  | Nat => f(W(Nat))
  | Float => f(W(Float))
  | Bool => f(W(Bool))
  | String => f(W(String))
  };

/* ========== CONVERSION ========== */

let convert =
    (type a, type b, from: kind(a), to_: kind(b), v: a)
    : Either.t(b, InvalidOperationError.t) => {
  switch (from, to_) {
  | (Int, Int) => L(v)
  | (Int, SInt) =>
    switch (Bigint.to_int(v)) {
    | Some(i) => L(i)
    | None => R(InvalidOperationError.IntegerTooBig)
    }
  | (Int, Nat) =>
    v < Bigint.zero ? R(InvalidOperationError.NegativeNat) : L(v)
  | (Int, Float) => L(Bigint.to_float(v))
  | (Int, Bool) => L(v != Bigint.zero)
  | (Int, String) => L(Bigint.to_string(v))

  | (SInt, SInt) => L(v)
  | (SInt, Int) => L(Bigint.of_int(v))
  | (SInt, Nat) =>
    v < 0 ? R(InvalidOperationError.NegativeNat) : L(Bigint.of_int(v))
  | (SInt, Bool) => L(v != 0)
  | (SInt, Float) => L(float_of_int(v))
  | (SInt, String) => L(string_of_int(v))

  | (Nat, Nat) => L(v)
  | (Nat, SInt) =>
    switch (Bigint.to_int(v)) {
    | Some(i) => L(i)
    | None => R(InvalidOperationError.IntegerTooBig)
    }
  | (Nat, Int) => L(v)
  | (Nat, Bool) => L(v != Bigint.zero)
  | (Nat, Float) => L(Bigint.to_float(v))
  | (Nat, String) => L(Bigint.to_string(v))

  | (Float, Float) => L(v)
  | (Float, SInt) => L(int_of_float(v))
  | (Float, Int) => L(Bigint.of_float(v))
  | (Float, Nat) =>
    v < 0.0 ? R(InvalidOperationError.NegativeNat) : L(Bigint.of_float(v))
  | (Float, Bool) => L(v != 0.0)
  | (Float, String) => L(string_of_float(v))

  | (Bool, Bool) => L(v)
  | (Bool, SInt) => L(v ? 1 : 0)
  | (Bool, Nat) => L(v ? Bigint.one : Bigint.zero)
  | (Bool, Int) => L(v ? Bigint.one : Bigint.zero)
  | (Bool, Float) => L(v ? 1.0 : 0.0)
  | (Bool, String) => L(string_of_bool(v))

  | (String, String) => L(v)
  | (String, SInt) =>
    switch (int_of_string_opt(v)) {
    | Some(i) => L(i)
    | None => R(InvalidOperationError.InvalidOfString)
    }
  | (String, Int) =>
    switch (Bigint.of_string_opt(v)) {
    | Some(i) => L(i)
    | None => R(InvalidOperationError.InvalidOfString)
    }
  | (String, Nat) =>
    switch (Bigint.of_string_opt(v)) {
    | Some(i) =>
      i < Bigint.zero ? R(InvalidOperationError.NegativeNat) : L(i)
    | None => R(InvalidOperationError.InvalidOfString)
    }
  | (String, Float) =>
    switch (float_of_string_opt(v)) {
    | Some(f) => L(f)
    | None => R(InvalidOperationError.InvalidOfString)
    }
  | (String, Bool) =>
    switch (bool_of_string_opt(v)) {
    | Some(b) => L(b)
    | None => R(InvalidOperationError.InvalidOfString)
    }
  | (String, RegExp) => L(StringUtil.of_string(v))
  | (RegExp, String) => L(StringUtil.to_string(v))
  };
};

let to_literal = (e: t): string =>
  switch (e) {
  | Int(i) => i |> Bigint.to_string
  | Nat(i) => i |> Bigint.to_string
  | SInt(i) => i |> string_of_int
  | Float(f) => Printf.sprintf("%f", f)
  | Bool(b) => b |> string_of_bool
  | String(s) => "\"" ++ s ++ "\""
  | RegExp(r) => StringUtil.to_string(r);
  };

/* ========== BUILTINS ========== */

type builtin =
  | OneFun(kind('a), kind('b), 'a => Either.t('b, InvalidOperationError.t))
    : builtin
  | TwoFun(
      kind('a),
      kind('b),
      kind('c),
      ('a, 'b) => Either.t('c, InvalidOperationError.t),
    )
    : builtin;

let converter_builtins =
  ListUtil.cross(all_of_cls, all_of_cls)
  |> List.filter_map(((cls1, cls2)) =>
       if (cls1 == cls2) {
         None;
       } else {
         Some((
           cls_string_lower(cls2) ++ "_of_" ++ cls_string_lower(cls1),
           {
             let.cls W(cls1) = cls1;
             let.cls W(cls2) = cls2;
             OneFun(cls1, cls2, convert(cls1, cls2));
           },
         ));
       }
     );
