open Util;

/* ========== DEFINITIONS ========== */

[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Int
  | SInt
  | Nat
  | Float
  | Bool
  | String;

/* This type is like cls, but each variant is associated with a type, allowing
   us to use the ocaml type checker to check we're using payloads correctly */
type kind('a) =
  | Int: kind(Bigint.t)
  | SInt: kind(Bigint.t)
  | Nat: kind(Bigint.t)
  | Float: kind(float)
  | Bool: kind(bool)
  | String: kind(string);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Int(Bigint.t)
  | SInt(Bigint.t)
  | Nat(Bigint.t)
  | Float
      // This equality condition is used to say that two floats are equal if they are equal in the ExpToSegment serialization
      (
        [@equal
          (a, b) =>
            Printf.(String.equal(sprintf("%f", a), sprintf("%f", b)))
        ] float,
      )
  | Bool(bool)
  | String(string);

let cls_of_kind = (type a, kind: kind(a)): cls =>
  switch (kind) {
  | Int => Int
  | SInt => SInt
  | Nat => Nat
  | Float => Float
  | Bool => Bool
  | String => String
  };

let cls_of_t: t => cls =
  fun
  | Int(_) => Int
  | SInt(_) => SInt
  | Nat(_) => Nat
  | Float(_) => Float
  | Bool(_) => Bool
  | String(_) => String;

let cls_string_lower: cls => string =
  fun
  | Int => "int"
  | SInt => "sint"
  | Nat => "nat"
  | Float => "float"
  | Bool => "bool"
  | String => "string";

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
  };

let repack = (type a, kind: kind(a), x: a): t =>
  switch (kind) {
  | Int => Int(x)
  | SInt => SInt(x)
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
  | SInt => f(W(SInt))
  | Nat => f(W(Nat))
  | Float => f(W(Float))
  | Bool => f(W(Bool))
  | String => f(W(String))
  };

/* ========== CONVERSION ========== */

/* An SInt has 64 bits everywhere: its values are [-2^63, 2^63 - 1]. It is held in a Bigint, as an Int is, because an
   OCaml int cannot hold 64 bits: it has 32 in the browser (js_of_ocaml) and 63 natively, and before, SInt wrapped
   silently at whichever width ran it (2147483647 + 1 was -2147483648 in the browser). A value made into an SInt, and
   each SInt operation's result (Operators), is checked against that range; outside it is an error, never a wrap. */
let sint_max = Bigint.(pow(of_int(2), of_int(63)) - one);
let sint_min = Bigint.(neg(pow(of_int(2), of_int(63))));
let sint_fits = (n: Bigint.t): bool =>
  Bigint.(n >= sint_min) && Bigint.(n <= sint_max);

let convert =
    (type a, type b, from: kind(a), to_: kind(b), v: a)
    : result(b, InvalidOperationError.t) => {
  switch (from, to_) {
  | (Int, Int) => Ok(v)
  | (Int, SInt) =>
    sint_fits(v) ? Ok(v) : Error(InvalidOperationError.IntegerTooBig)
  | (Int, Nat) =>
    Bigint.(v < zero) ? Error(InvalidOperationError.NegativeNat) : Ok(v)
  | (Int, Float) => Ok(Bigint.to_float(v))
  | (Int, Bool) => Ok(!Bigint.equal(v, Bigint.zero))
  | (Int, String) => Ok(Bigint.to_string(v))

  | (SInt, SInt) => Ok(v)
  | (SInt, Int) => Ok(v)
  | (SInt, Nat) =>
    Bigint.(v < zero) ? Error(InvalidOperationError.NegativeNat) : Ok(v)
  | (SInt, Bool) => Ok(!Bigint.equal(v, Bigint.zero))
  | (SInt, Float) => Ok(Bigint.to_float(v))
  | (SInt, String) => Ok(Bigint.to_string(v))

  | (Nat, Nat) => Ok(v)
  | (Nat, SInt) =>
    sint_fits(v) ? Ok(v) : Error(InvalidOperationError.IntegerTooBig)
  | (Nat, Int) => Ok(v)
  | (Nat, Bool) => Ok(!Bigint.equal(v, Bigint.zero))
  | (Nat, Float) => Ok(Bigint.to_float(v))
  | (Nat, String) => Ok(Bigint.to_string(v))

  | (Float, Float) => Ok(v)
  | (Float, SInt) =>
    /* truncated toward zero; NaN and the infinities do not fit */
    if (Float.is_finite(v)) {
      let n = Bigint.of_float(Float.round_towards_zero(v));
      sint_fits(n) ? Ok(n) : Error(InvalidOperationError.IntegerTooBig);
    } else {
      Error(InvalidOperationError.IntegerTooBig);
    }
  | (Float, Int) => Ok(Bigint.of_float(v))
  | (Float, Nat) =>
    Float.(v < 0.0)
      ? Error(InvalidOperationError.NegativeNat) : Ok(Bigint.of_float(v))
  | (Float, Bool) => Ok(!Float.equal(v, 0.0))
  | (Float, String) => Ok(string_of_float(v))

  | (Bool, Bool) => Ok(v)
  | (Bool, SInt) => Ok(v ? Bigint.one : Bigint.zero)
  | (Bool, Nat) => Ok(v ? Bigint.one : Bigint.zero)
  | (Bool, Int) => Ok(v ? Bigint.one : Bigint.zero)
  | (Bool, Float) => Ok(v ? 1.0 : 0.0)
  | (Bool, String) => Ok(string_of_bool(v))

  | (String, String) => Ok(v)
  | (String, SInt) =>
    switch (Bigint.of_string_opt(v)) {
    | Some(i) =>
      sint_fits(i) ? Ok(i) : Error(InvalidOperationError.IntegerTooBig)
    | None => Error(InvalidOperationError.InvalidOfString)
    }
  | (String, Int) =>
    switch (Bigint.of_string_opt(v)) {
    | Some(i) => Ok(i)
    | None => Error(InvalidOperationError.InvalidOfString)
    }
  | (String, Nat) =>
    switch (Bigint.of_string_opt(v)) {
    | Some(i) =>
      Bigint.(i < zero) ? Error(InvalidOperationError.NegativeNat) : Ok(i)
    | None => Error(InvalidOperationError.InvalidOfString)
    }
  | (String, Float) =>
    switch (float_of_string_opt(v)) {
    | Some(f) => Ok(f)
    | None => Error(InvalidOperationError.InvalidOfString)
    }
  | (String, Bool) =>
    switch (bool_of_string_opt(v)) {
    | Some(b) => Ok(b)
    | None => Error(InvalidOperationError.InvalidOfString)
    }
  };
};

let to_literal = (e: t): string =>
  switch (e) {
  | Int(i) => i |> Bigint.to_string
  | Nat(i) => i |> Bigint.to_string
  | SInt(i) => i |> Bigint.to_string
  | Float(f) => sprintf("%f", f)
  | Bool(b) => b |> string_of_bool
  | String(s) => "\"" ++ s ++ "\""
  };

/* ========== BUILTINS ========== */

type builtin =
  | OneFun(kind('a), kind('b), 'a => result('b, InvalidOperationError.t))
    : builtin
  | TwoFun(
      kind('a),
      kind('b),
      kind('c),
      ('a, 'b) => result('c, InvalidOperationError.t),
    )
    : builtin;

/* Comparator data per class: kind (carries type info) plus the underlying
 * OCaml compare function. BuiltinsADT.of_atom_compare consumes this to
 * derive the Ord builtin. Bool has no builtin compare. */
type compare_entry =
  | Cmp(kind('a), ('a, 'a) => int): compare_entry;

/* Single source of truth for compare builtins. Exhaustive on cls so adding
 * a new class forces a decision here (compile error). */
let compare_of_cls: cls => option(compare_entry) =
  fun
  | Int => Some(Cmp(Int, Bigint.compare))
  | SInt => Some(Cmp(SInt, Bigint.compare))
  | Nat => Some(Cmp(Nat, Bigint.compare))
  | Float => Some(Cmp(Float, Float.compare))
  | String => Some(Cmp(String, String.compare))
  | Bool => None;

let compare_builtin = (cls: cls): option(string) =>
  compare_of_cls(cls)
  |> Option.map(~f=_ => cls_string_lower(cls) ++ "_compare");

let compare_builtins: list((string, compare_entry)) =
  all_of_cls
  |> List.filter_map(~f=cls =>
       compare_of_cls(cls)
       |> Option.map(~f=entry =>
            (cls_string_lower(cls) ++ "_compare", entry)
          )
     );

let conversions_from = (from_: cls): list((string, cls)) =>
  all_of_cls
  |> List.filter_map(~f=to_ =>
       if (equal_cls(from_, to_)) {
         None;
       } else {
         Some((
           cls_string_lower(to_) ++ "_of_" ++ cls_string_lower(from_),
           to_,
         ));
       }
     );

let converter_builtins =
  all_of_cls
  |> List.concat_map(~f=cls1 =>
       conversions_from(cls1)
       |> List.map(~f=((name, cls2)) =>
            (
              name,
              {
                let.cls W(cls1) = cls1;
                let.cls W(cls2) = cls2;
                OneFun(cls1, cls2, convert(cls1, cls2));
              },
            )
          )
     );
