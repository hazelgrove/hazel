open Util;

/* ========== DEFINITIONS ========== */

[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Int
  | Nat
  | Float
  | Bool
  | String;

/* This type is like cls, but each variant is associated with a type, allowing
   us to use the ocaml type checker to check we're using payloads correctly */
type kind('a) =
  | Int: kind(int)
  | Nat: kind(int)
  | Float: kind(float)
  | Bool: kind(bool)
  | String: kind(string);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Int(int)
  | Nat(int)
  | Float
      // This equality condition is used to say that two floats are equal if they are equal in the ExpToSegment serialization
      (
        [@equal (a, b) => Printf.(sprintf("%f", a) == sprintf("%f", b))] float,
      )
  | Bool(bool)
  | String(string);

let cls_of_kind = (type a, kind: kind(a)): cls =>
  switch (kind) {
  | Int => Int
  | Nat => Nat
  | Float => Float
  | Bool => Bool
  | String => String
  };

let cls_of_t: t => cls =
  fun
  | Int(_) => Int
  | Nat(_) => Nat
  | Float(_) => Float
  | Bool(_) => Bool
  | String(_) => String;

let cls_string_lower: cls => string =
  fun
  | Int => "int"
  | Nat => "nat"
  | Float => "float"
  | Bool => "bool"
  | String => "string";

/* ========== MATCHING ========== */

let unbox = (type a, request: kind(a), e: t): option(a) =>
  switch (request, e) {
  | (Int, Int(i)) => Some(i)
  | (Int, _) => None
  | (Nat, Nat(i)) => Some(i)
  | (Nat, _) => None
  | (Float, Float(f)) => Some(f)
  | (Float, _) => None
  | (Bool, Bool(b)) => Some(b)
  | (Bool, _) => None
  | (String, String(s)) => Some(s)
  | (String, _) => None
  };

// Note[Matt]: return wrapper needed for polymorphic types
type wrapper =
  | V('a, kind('a)): wrapper;

let unpack = (type a, e: t): wrapper =>
  switch (e) {
  | Int(i) => V(i, Int)
  | Nat(i) => V(i, Nat)
  | Float(f) => V(f, Float)
  | Bool(b) => V(b, Bool)
  | String(s) => V(s, String)
  };

let repack = (type a, kind: kind(a), x: a): t =>
  switch (kind) {
  | Int => Int(x)
  | Nat => Nat(x)
  | Float => Float(x)
  | Bool => Bool(x)
  | String => String(x)
  };

// Mpte[Matt]: return wrapper needed for polymorphic types
type cls_wrapper =
  | W(kind('a)): cls_wrapper;

let (let.cls) = (type b, cls: cls, f: cls_wrapper => b): b =>
  switch (cls) {
  | Int => f(W(Int))
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
  | (Int, Nat) => v < 0 ? R(InvalidOperationError.NegativeNat) : L(v)
  | (Int, Bool) => L(v != 0)
  | (Int, Float) => L(float_of_int(v))
  | (Int, String) => L(string_of_int(v))

  | (Nat, Nat) => L(v)
  | (Nat, Int) => L(v)
  | (Nat, Bool) => L(v != 0)
  | (Nat, Float) => L(float_of_int(v))
  | (Nat, String) => L(string_of_int(v))

  | (Float, Float) => L(v)
  | (Float, Int) => L(int_of_float(v))
  | (Float, Nat) => L(int_of_float(v))
  | (Float, Bool) => L(v != 0.0)
  | (Float, String) => L(string_of_float(v))

  | (Bool, Bool) => L(v)
  | (Bool, Int) => L(v ? 1 : 0)
  | (Bool, Nat) => L(v ? 1 : 0)
  | (Bool, Float) => L(v ? 1.0 : 0.0)
  | (Bool, String) => L(string_of_bool(v))

  | (String, String) => L(v)
  | (String, Int) =>
    switch (int_of_string_opt(v)) {
    | Some(i) => L(i)
    | None => R(InvalidOperationError.InvalidOfString)
    }
  | (String, Nat) =>
    switch (int_of_string_opt(v)) {
    | Some(i) => i < 0 ? R(InvalidOperationError.NegativeNat) : L(i)
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
  };
};

let to_literal = (e: t): string =>
  switch (e) {
  | Int(i) => i |> string_of_int
  | Nat(i) => i |> string_of_int
  // TODO: do floats print right?
  | Float(f) => f |> string_of_float
  | Bool(b) => b |> string_of_bool
  | String(s) => "\"" ++ s ++ "\""
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
  |> List.map(((cls1, cls2)) =>
       (
         cls_string_lower(cls2) ++ "_of_" ++ cls_string_lower(cls1),
         {
           let.cls W(cls1) = cls1;
           let.cls W(cls2) = cls2;
           OneFun(cls1, cls2, convert(cls1, cls2));
         },
       )
     );
