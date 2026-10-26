/* See Rational.rei. Every value is built by [normalize] (or is already
   canonical, like [neg] of a canonical value), so the record is never exposed
   raw. */

type t = {
  num: Bigint.t,
  den: Bigint.t,
};

let to_z = Bigint.to_zarith_bigint;

let gcd = (a, b) => Bigint.of_zarith_bigint(Z.gcd(to_z(a), to_z(b)));

/* Requires d != 0, so g > 0. */
let normalize = (n: Bigint.t, d: Bigint.t): t => {
  let g = gcd(n, d);
  let n = Bigint.(n / g)
  and d = Bigint.(d / g);
  Bigint.(d < zero)
    ? {
      num: Bigint.neg(n),
      den: Bigint.neg(d),
    }
    : {
      num: n,
      den: d,
    };
};

let make = (n, d) =>
  Bigint.equal(d, Bigint.zero) ? None : Some(normalize(n, d));
let of_bigint = n => {
  num: n,
  den: Bigint.one,
};
let zero = of_bigint(Bigint.zero);
let one = of_bigint(Bigint.one);

let num = r => r.num;
let den = r => r.den;

let add = (a, b) =>
  normalize(Bigint.(a.num * b.den + b.num * a.den), Bigint.(a.den * b.den));
let sub = (a, b) =>
  normalize(Bigint.(a.num * b.den - b.num * a.den), Bigint.(a.den * b.den));
let mul = (a, b) =>
  normalize(Bigint.(a.num * b.num), Bigint.(a.den * b.den));
let neg = a => {
  ...a,
  num: Bigint.neg(a.num),
};
let inv = a =>
  Bigint.equal(a.num, Bigint.zero) ? None : Some(normalize(a.den, a.num));
let div = (a, b) => Option.map(mul(a), inv(b));

/* Square-and-multiply over the Bigint exponent itself (e >= 0): Bigint.pow
   narrows its exponent to a machine int. */
let bigint_pow = (base: Bigint.t, e: Bigint.t): Bigint.t => {
  let two = Bigint.of_int(2);
  let rec go = (base, e, acc) => {
    let acc = Bigint.(equal(rem(e, two), zero)) ? acc : Bigint.(acc * base);
    let e = Bigint.(e / two);
    Bigint.(equal(e, zero)) ? acc : go(Bigint.(base * base), e, acc);
  };
  Bigint.(equal(e, zero)) ? Bigint.one : go(base, e, Bigint.one);
};

/* Powers of coprime parts stay coprime, so only the sign needs fixing. */
let pow = (a, e) =>
  if (Bigint.(e >= zero)) {
    Some({
      num: bigint_pow(a.num, e),
      den: bigint_pow(a.den, e),
    });
  } else {
    Option.map(
      i => {
        let e = Bigint.neg(e);
        {
          num: bigint_pow(i.num, e),
          den: bigint_pow(i.den, e),
        };
      },
      inv(a),
    );
  };

/* Canonical form makes equality structural. */
let equal = (a, b) =>
  Bigint.equal(a.num, b.num) && Bigint.equal(a.den, b.den);
/* Denominators are positive, so cross-multiplying preserves order. */
let compare = (a, b) =>
  Bigint.compare(Bigint.(a.num * b.den), Bigint.(b.num * a.den));

/* Divide in integers first, keeping about 64 significant bits, so that values
   whose parts overflow a float (e.g. (10^400+1)/10^400) still convert. */
let to_float = ({num, den}) => {
  let n = to_z(num)
  and d = to_z(den);
  let shift = 64 - (Z.numbits(n) - Z.numbits(d));
  let q =
    shift >= 0
      ? Z.div(Z.shift_left(n, shift), d)
      : Z.div(n, Z.shift_left(d, - shift));
  ldexp(Z.to_float(q), - shift);
};

let show = ({num, den}) =>
  Bigint.to_string(num) ++ "/" ++ Bigint.to_string(den);
let pp = (fmt, r) => Format.pp_print_string(fmt, show(r));

/* Decoding: accept exactly what the encoders produce. */
let canonical_bigint = (s: string): option(Bigint.t) =>
  switch (Bigint.of_string_opt(s)) {
  | Some(n) when Bigint.to_string(n) == s => Some(n)
  | _ => None
  };

let of_strings = (n: string, d: string): option(t) =>
  switch (canonical_bigint(n), canonical_bigint(d)) {
  | (Some(n), Some(d)) =>
    switch (make(n, d)) {
    | Some(r) when Bigint.equal(r.num, n) && Bigint.equal(r.den, d) =>
      Some(r)
    | _ => None
    }
  | _ => None
  };

let decode_error = "Rational: expected a canonical [num, den] pair of decimal strings with den > 0";

let sexp_of_t = ({num, den}): Sexplib.Sexp.t =>
  Sexplib.Sexp.List([
    Sexplib.Sexp.Atom(Bigint.to_string(num)),
    Sexplib.Sexp.Atom(Bigint.to_string(den)),
  ]);

let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
  switch (sexp) {
  | Sexplib.Sexp.List([Sexplib.Sexp.Atom(n), Sexplib.Sexp.Atom(d)]) =>
    switch (of_strings(n, d)) {
    | Some(r) => r
    | None => Sexplib0.Sexp_conv.of_sexp_error(decode_error, sexp)
    }
  | _ => Sexplib0.Sexp_conv.of_sexp_error(decode_error, sexp)
  };

let yojson_of_t = ({num, den}): Yojson.Safe.t =>
  `List([`String(Bigint.to_string(num)), `String(Bigint.to_string(den))]);

let t_of_yojson = (json: Yojson.Safe.t): t =>
  switch (json) {
  | `List([`String(n), `String(d)]) =>
    switch (of_strings(n, d)) {
    | Some(r) => r
    | None =>
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error(decode_error, json)
    }
  | _ => Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error(decode_error, json)
  };
