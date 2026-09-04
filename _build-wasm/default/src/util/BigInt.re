/* SPIKE (wasm-eval-bench): a Bigint that does NOT depend on the bignum
   package, swapped in over BigInt.re by bench/wasm/run.sh.

   Why: bignum pulls zarith and the Jane Street Core C-stub surface, none of
   which wasm_of_ocaml can satisfy -- 53 unimplemented primitives, 27 of
   them ml_z_*. Dropping bignum from [util] drops all of it.

   Policy, per the decision to leave bignum-dependent parts unimplemented
   for now: construction, conversion and comparison are total, because
   Builtins.ctx_init needs them just to boot the interpreter. ARITHMETIC
   RAISES. That is deliberate -- a workload that reaches Int or Nat
   arithmetic fails loudly rather than silently benchmarking 63-bit math and
   being reported as an arbitrary-precision result.

   Fixed-precision workloads (SInt, Float) never reach these paths. See
   bench/fixed-*.hz. */

open Sexplib.Std;
include Ppx_yojson_conv_lib.Yojson_conv.Primitives;

[@deriving (show({with_path: false}), sexp)]
type t = int;

let unimplemented = op =>
  failwith(
    "Bigint." ++ op ++ ": arbitrary precision is unimplemented in the "
    ++ "wasm-eval-bench spike. Use SInt or Float (see bench/fixed-*.hz).",
  );

/* -- total: needed to boot the interpreter -- */
let zero: t = 0;
let one: t = 1;
let of_int = (i: int): t => i;
let to_int = (i: t): option(int) => Some(i);
let to_string = (i: t): string => string_of_int(i);
let of_string = (s: string): t => int_of_string(s);
let of_string_opt = (s: string): option(t) => int_of_string_opt(s);
let compare = (a: t, b: t): int => Int.compare(a, b);
let equal = (a: t, b: t): bool => a == b;
let (<) = (a: t, b: t): bool => a < b;
let (<=) = (a: t, b: t): bool => a <= b;

/* -- unimplemented: arbitrary-precision arithmetic -- */
let (+) = (_: t, _: t): t => unimplemented("+");
let (-) = (_: t, _: t): t => unimplemented("-");
let ( * ) = (_: t, _: t): t => unimplemented("*");
let (/) = (_: t, _: t): t => unimplemented("/");
let (%) = (_: t, _: t): t => unimplemented("%");
let pow = (_: t, _: t): t => unimplemented("pow");
let neg = (_: t): t => unimplemented("neg");
let abs = (_: t): t => unimplemented("abs");
let to_float = (_: t): float => unimplemented("to_float");
let of_float = (_: float): t => unimplemented("of_float");

let t_of_yojson = (json): t => int_of_string(string_of_yojson(json));
let yojson_of_t = (i: t): Yojson.Safe.t => `String(to_string(i));
