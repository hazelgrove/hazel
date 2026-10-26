/* Exact rational numbers: an opaque, finite numerator/denominator pair.

   Invariant held by every value of [t], however it was produced (arithmetic or
   decoding): den > 0, gcd(|num|, den) = 1, and zero is 0/1. There are no
   infinities or undefined values. Operations that would divide by zero return
   [None]; callers map that to their own error (for Hazel, DivideByZero).

   Nothing here routes through float except [to_float], which is lossy and for
   display only. */

type t;

/* None iff [den] is zero. Otherwise reduces to lowest terms with a positive
   denominator. */
let make: (Bigint.t, Bigint.t) => option(t);
let of_bigint: Bigint.t => t;
let zero: t;
let one: t;

let num: t => Bigint.t;
/* Always positive. */
let den: t => Bigint.t;

let add: (t, t) => t;
let sub: (t, t) => t;
let mul: (t, t) => t;
let neg: t => t;
/* None iff the argument is zero. */
let inv: t => option(t);
/* None iff the divisor is zero. */
let div: (t, t) => option(t);

let equal: (t, t) => bool;
/* Numeric order, as [Stdlib.compare]: negative, zero or positive. */
let compare: (t, t) => int;

let to_float: t => float;

/* For [@deriving show]: a debugging form such as "-1/3". This is not the
   user-facing display, which belongs to Hazel's printer. */
let pp: (Format.formatter, t) => unit;
let show: t => string;

/* Serialization for [@deriving sexp, yojson]. Provisional encoding: the decimal
   strings of num and den, as a JSON array ["-1","3"] or a sexp (-1 3).
   Decoders accept only what the encoders produce: a zero denominator,
   non-canonical pairs (2/4, 1/-3, 0/5) and non-canonical digit strings ("+1",
   "007") are rejected with the usual of_sexp / of_yojson error. */
let sexp_of_t: t => Sexplib.Sexp.t;
let t_of_sexp: Sexplib.Sexp.t => t;
let yojson_of_t: t => Yojson.Safe.t;
let t_of_yojson: Yojson.Safe.t => t;
