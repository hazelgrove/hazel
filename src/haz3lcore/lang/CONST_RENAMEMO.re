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

/* ========== CONVERSION ========== */

let to_literal = (e: t): string =>
  switch (e) {
  | Int(i) => i |> string_of_int
  | Nat(i) => i |> string_of_int
  // TODO: do floats print right?
  | Float(f) => f |> string_of_float
  | Bool(b) => b |> string_of_bool
  | String(s) => "\"" ++ s ++ "\""
  };

// let convert = (type a, type b, from: kind(a), to_: kind(b), x: a): b =>
//   switch (from, to_) {
//   | _ => failwith("TODO: Conversion implemented yet")
//   };

// let to_string = (e: t): string => {
//   let V(v, k) = unpack(e);
//   convert(k, String, v);
// };
