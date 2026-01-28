/* eslint-disable no-unused-vars */

// We represent a [Z.t] as a javascript 32bit integers or as a BigInt.
// Like in ZArith, we guarantee that:
//  - If the number fits in a 32bit integer, it is stored in a 32bit integer,
//    not a BigInt.
//  - Conversely, if the number does not fit in a 32bit integer, it is stored in
//    a BigInt. We almost could get away with using primitive integers up to 53
//    bits, but this would require giving up marshaling (because the js_of_ocaml
//    runtime assumes that numbers are either 32bit integers or floats, and
//    refuses to marshal floats that are not exactly 32bit integers), and so we
//    don't. I am not sure this would bring much performance benefits due to the
//    additional checks required anyways (the checks for 32bit integers happen
//    in ZArith's OCaml code, so we can't replace them with checks for 53 bit
//    integers).
//
// Note that some functions in this module call other functions as utility in
// a way that doesn't respect these invariants, and so some functions can see
// non-normalized numbers as input.

//Provides: ml_z_normalize
function ml_z_normalize(x) {
  if (typeof x === "number") {
    if (x === (x | 0)) return x;

    return BigInt(x);
  }

  if (-2147483648 <= x && x <= 2147483647) return Number(x) | 0;
  return x;
}

//external mul_overflows: int -> int -> bool
//Provides: ml_z_mul_overflows
function ml_z_mul_overflows(x, y) {
  let z = x * y;
  return +(z !== (z | 0));
}

//external init: unit -> unit
//Provides: ml_z_init
//Requires: caml_zarith_marshal, caml_zarith_unmarshal, caml_custom_ops
//Requires: ml_z_hash, ml_z_compare
function ml_z_init(unit) {
  caml_custom_ops['_z'] =
  {
    serialize: caml_zarith_marshal,
    deserialize: caml_zarith_unmarshal,
    hash: ml_z_hash,
    compare: ml_z_compare,
  };
  Object.defineProperty(BigInt.prototype, 'caml_custom', { value: '_z' });
  return 0
}

//external neg: t -> t
//Provides: ml_z_neg const
//Requires: ml_z_normalize
function ml_z_neg(z1) {
  return ml_z_normalize(-z1);
}

//external add: t -> t -> t
//Provides: ml_z_add const
//Requires: ml_z_normalize
function ml_z_add(z1, z2) {
  return ml_z_normalize(BigInt(z1) + BigInt(z2));
}

//external sub: t -> t -> t
//Provides: ml_z_sub const
//Requires: ml_z_normalize
function ml_z_sub(z1, z2) {
  return ml_z_normalize(BigInt(z1) - BigInt(z2));
}

//external mul: t -> t -> t
//Provides: ml_z_mul const
//Requires: ml_z_normalize
function ml_z_mul(z1, z2) {
  return ml_z_normalize(BigInt(z1) * BigInt(z2));
}

//external div: t -> t -> t
//Provides: ml_z_div
//Requires: caml_raise_zero_divide, ml_z_normalize
function ml_z_div(z1, z2) {
  if (z2 == 0) caml_raise_zero_divide();
  return ml_z_normalize(BigInt(z1) / BigInt(z2));
}

//external cdiv: t -> t -> t
//Provides: ml_z_cdiv
//Requires: ml_z_div, ml_z_sign, ml_z_normalize
function ml_z_cdiv(z1, z2) {
  let z1_pos = ml_z_sign(z1);
  let z2_pos = ml_z_sign(z2);
  if (z1_pos * z2_pos > 0) /* Multiplication is like a signwise xor */ {
    if (BigInt(z1) % BigInt(z2) !== 0n) {
      return ml_z_normalize(BigInt(z1) / BigInt(z2) + 1n);
    }
  }
  return ml_z_div(z1, z2);
}

//external fdiv: t -> t -> t
//Provides: ml_z_fdiv
//Requires: ml_z_div, ml_z_sign, ml_z_normalize
function ml_z_fdiv(z1, z2) {
  let z1_pos = ml_z_sign(z1);
  let z2_pos = ml_z_sign(z2);
  if (z1_pos * z2_pos < 0) /* Multiplication is like a signwise xor */ {
    if (BigInt(z1) % BigInt(z2) !== 0n) {
      return ml_z_normalize(BigInt(z1) / BigInt(z2) - 1n);
    }
  }
  return ml_z_div(z1, z2);
}

//external rem: t -> t -> t
//Provides: ml_z_rem
//Requires: caml_raise_zero_divide, ml_z_normalize
function ml_z_rem(z1, z2) {
  if (z2 == 0) caml_raise_zero_divide();
  return ml_z_normalize(BigInt(z1) % BigInt(z2));
}

//external div_rem: t -> t -> (t * t)
//Provides: ml_z_div_rem
//Requires: ml_z_div, ml_z_rem
function ml_z_div_rem(z1, z2) {
  return [0, ml_z_div(z1, z2), ml_z_rem(z1, z2)]
}
//external succ: t -> t
//Provides: ml_z_succ const
//Requires: ml_z_normalize
function ml_z_succ(z1) {
  return ml_z_normalize(BigInt(z1) + 1n);
}

//external pred: t -> t
//Provides: ml_z_pred const
//Requires: ml_z_normalize
function ml_z_pred(z1) {
  return ml_z_normalize(BigInt(z1) - 1n);
}

//external abs: t -> t
//Provides: ml_z_abs const
//Requires: ml_z_normalize
function ml_z_abs(z1) {
  if (z1 < 0) return ml_z_normalize(-z1);
  return z1;
}

//external logand: t -> t -> t
//Provides: ml_z_logand const
//Requires: ml_z_normalize
function ml_z_logand(z1, z2) {
  return ml_z_normalize(BigInt(z1) & BigInt(z2));
}

//external logor: t -> t -> t
//Provides: ml_z_logor const
//Requires: ml_z_normalize
function ml_z_logor(z1, z2) {
  return ml_z_normalize(BigInt(z1) | BigInt(z2));
}

//external logxor: t -> t -> t
//Provides: ml_z_logxor const
//Requires: ml_z_normalize
function ml_z_logxor(z1, z2) {
  return ml_z_normalize(BigInt(z1) ^ BigInt(z2));
}

//external lognot: t -> t
//Provides: ml_z_lognot const
//Requires: ml_z_normalize
function ml_z_lognot(z1) {
  return ml_z_normalize(~z1);
}

//external shift_left: t -> int -> t
//Provides: ml_z_shift_left const
//Requires: ml_z_normalize
function ml_z_shift_left(z1, amt) {
  return ml_z_normalize(BigInt(z1) << BigInt(amt));
}

//external shift_right: t -> int -> t
//Provides: ml_z_shift_right const
//Requires: ml_z_normalize
function ml_z_shift_right(z1, amt) {
  return ml_z_normalize(BigInt(z1) >> BigInt(amt));
}

//external shift_right_trunc: t -> int -> t
//Provides: ml_z_shift_right_trunc const
//Requires: ml_z_normalize
function ml_z_shift_right_trunc(z1, z2) {
  return ml_z_normalize(BigInt(z1) / (1n << BigInt(z2)));
}

//external of_int32: int32 -> t
//Provides: ml_z_of_int32 const
function ml_z_of_int32(i) {
  return i | 0;
}

//external of_nativeint: nativeint -> t
//Provides: ml_z_of_nativeint const
function ml_z_of_nativeint(i) {
  return i | 0;
}

//external of_int64: int64 -> t
//Provides: ml_z_of_int64 const
//Requires: caml_int64_compare, caml_int64_neg, ml_z_normalize
//Requires: caml_int64_create_lo_hi,caml_int64_hi32,caml_int64_lo32
function ml_z_of_int64(i64) {
  let neg = false;
  if (caml_int64_compare(i64, caml_int64_create_lo_hi(0, 0)) < 0) {
    neg = true;
    i64 = caml_int64_neg(i64)
  }
  let lo = caml_int64_lo32(i64) >>> 0;
  let hi = caml_int64_hi32(i64) >>> 0;
  let x = BigInt(lo) + (BigInt(hi) << 32n);
  if (neg) { x = -x }
  return ml_z_normalize(x)
}

//external of_float: float -> t
//Provides: ml_z_of_float
//Requires: caml_raise_constant, caml_named_value, ml_z_normalize
function ml_z_of_float(f1) {
  if (f1 == Infinity || f1 == -Infinity || f1 != f1)
    caml_raise_constant(caml_named_value("ml_z_overflow"));
  return ml_z_normalize(f1 < 0 ? Math.ceil(f1) : Math.floor(f1));
}

//external to_int: t -> int
//Provides: ml_z_to_int
//Requires: caml_raise_constant, caml_named_value
function ml_z_to_int(z1) {
  if (typeof z1 === "number" && z1 === (z1 | 0)) return z1;
  caml_raise_constant(caml_named_value("ml_z_overflow"));
}

//external to_int32: t -> int32
//Provides: ml_z_to_int32
//Requires: ml_z_to_int
function ml_z_to_int32(z1) { return ml_z_to_int(z1) }

//external to_int32_unsigned: t -> int32
//Provides: ml_z_to_int32_unsigned
//Requires: ml_z_fits_int32_unsigned, ml_z_normalize, caml_raise_constant
//Requires: caml_named_value
function ml_z_to_int32_unsigned(z1) {
  if (ml_z_fits_int32_unsigned(z1)) {
    return Number(BigInt.asIntN(32, BigInt(z1))) | 0;
  }
  caml_raise_constant(caml_named_value("ml_z_overflow"));
}

//external to_nativeint_unsigned: t -> nativeint
//Provides: ml_z_to_nativeint_unsigned
//Requires: ml_z_to_int32_unsigned
function ml_z_to_nativeint_unsigned(z1) {
  return ml_z_to_int32_unsigned(z1);
}

//external to_int64: t -> int64
//Provides: ml_z_to_int64
//Requires: ml_z_fits_int64, caml_raise_constant, caml_named_value
//Requires: caml_int64_create_lo_hi
function ml_z_to_int64(z1) {
  if (!ml_z_fits_int64(z1)) {
    caml_raise_constant(caml_named_value("ml_z_overflow"));
  }
  let z1n = BigInt(z1)
  let mask = 0xffffffffn
  let lo = Number(z1n & mask);
  let hi = Number((z1n >> 32n) & mask);
  let x = caml_int64_create_lo_hi(lo, hi);
  return x;
}

//external to_int64_unsigned: t -> int64
//Provides: ml_z_to_int64_unsigned
//Requires: ml_z_fits_int64_unsigned, caml_raise_constant, caml_named_value
//Requires: caml_int64_create_lo_hi
function ml_z_to_int64_unsigned(z1) {
  if (!ml_z_fits_int64_unsigned(z1)) {
    caml_raise_constant(caml_named_value("ml_z_overflow"));
  }
  let z1n = BigInt.asIntN(64, BigInt(z1))
  let mask = 0xffffffffn
  let lo = Number(z1n & mask);
  let hi = Number((z1n >> 32n) & mask);
  let x = caml_int64_create_lo_hi(lo, hi);
  return x;
}

//external testbit: t -> int -> bool
//Provides: ml_z_testbit const
function ml_z_testbit(z, pos) {
  return Number((BigInt(z) >> BigInt(pos)) & 1n);
}

//external to_nativeint: t -> nativeint
//Provides: ml_z_to_nativeint
//Requires: ml_z_to_int
function ml_z_to_nativeint(z1) { return ml_z_to_int(z1) }

//external format: string -> t -> string
//Provides: ml_z_format
//Requires: caml_jsbytes_of_string, caml_failwith, caml_string_of_jsbytes
//Requires: ml_z_normalize
function ml_z_format(fmt, z1) {
  let z1n = BigInt(z1);
  fmt = caml_jsbytes_of_string(fmt);
  // https://github.com/ocaml/Zarith/blob/d0555d451ce295c4497f24a8d9993f8dd23097df/z.mlip#L297
  let base = 10;
  let cas = 0;
  let width = 0;
  let alt = 0;
  let dir = 0;
  let sign = '';
  let pad = ' ';
  let idx = 0;
  let prefix = "";
  while (fmt[idx] == '%') idx++;
  for (; ; idx++) {
    if (fmt[idx] == '#') alt = 1;
    else if (fmt[idx] == '0') pad = '0';
    else if (fmt[idx] == '-') dir = 1;
    else if (fmt[idx] == ' ' || fmt[idx] == '+') sign = fmt[idx];
    else break;
  }
  if (z1n < 0) { sign = '-'; z1n = -z1n }
  for (; fmt[idx] >= '0' && fmt[idx] <= '9'; idx++)
    width = 10 * width + (+fmt[idx]);
  switch (fmt[idx]) {
    case 'i': case 'd': case 'u': break;
    case 'b': base = 2; if (alt) prefix = "0b"; break;
    case 'o': base = 8; if (alt) prefix = "0o"; break;
    case 'x': base = 16; if (alt) prefix = "0x"; break;
    case 'X': base = 16; if (alt) prefix = "0X"; cas = 1; break;
    default:
      caml_failwith("Unsupported format '" + fmt + "'");
  }
  if (dir) pad = ' ';
  let res = z1n.toString(base);
  if (cas === 1) {
    res = res.toUpperCase();
  }
  let size = res.length;
  if (pad == ' ') {
    if (dir) {
      res = sign + prefix + res;
      for (; res.length < width;) res = res + pad;
    } else {
      res = sign + prefix + res;
      for (; res.length < width;) res = pad + res;
    }
  } else {
    let pre = sign + prefix;
    for (; res.length + pre.length < width;) res = pad + res;
    res = pre + res;
  }
  return caml_string_of_jsbytes(res);
}

//Provides: jsoo_z_of_js_string_base
//Requires: caml_invalid_argument, ml_z_normalize
function jsoo_z_of_js_string_base(base, s) {
  if (base == 0) { // https://github.com/ocaml/Zarith/blob/b8dbaf48a7927061df699ad7ce642bb4f1fe5308/caml_z.c#L598
    base = 10;
    let p = 0;
    let sign = 1;
    if (s[p] == '-') { sign = -1; p++ }
    else if (s[p] == '+') { p++ }
    if (s[p] == '0') {
      p++;
      if (s.length == p) {
        return 0;
      } else {
        let bc = s[p];
        if (bc == 'o' || bc == 'O') {
          base = 8;
        } else if (bc == 'x' || bc == 'X') {
          base = 16;
        } else if (bc == 'b' || bc == 'B') {
          base = 2;
        }
        if (base != 10) {
          s = s.substring(p + 1);
          if (sign == -1) s = "-" + s;
        }
      }
    }
  }

  function digit(code) {
    if (code >= 48 && code <= 57) return code - 48;
    if (code >= 97 && code <= 102) return code - 97 + 10;
    if (code >= 65 && code <= 70) return code - 65 + 10;
  }
  let i = 0;
  if (s[i] == '+') {
    //remove leading '+'
    s = s.substring(1);
  }
  else if (s[i] == '-') i++;
  if (s[i] == '_') caml_invalid_argument("Z.of_substring_base: invalid digit");
  s = s.replace(/_/g, '');
  //normalize "empty" numbers
  if (s == '-' || s == '') s = '0';
  for (; i < s.length; i++) {
    let c = digit(s.charCodeAt(i));
    if (c == undefined || c >= base)
      caml_invalid_argument("Z.of_substring_base: invalid digit");
  }

  if (base === 10) return ml_z_normalize(BigInt(s));

  let neg = false;
  i = 0;
  if (s[i] == '-') { neg = true; i++; }
  let n = 0n;
  for (; i < s.length; i++) {
    n *= BigInt(base);
    n += BigInt(digit(s.charCodeAt(i)));
  }
  if (neg) n = -n;
  return ml_z_normalize(n);
}

//external of_substring_base: int -> string -> pos:int -> len:int -> t
//Provides: ml_z_of_substring_base
//Requires: jsoo_z_of_js_string_base, caml_jsbytes_of_string
//Requires: caml_invalid_argument, caml_ml_string_length
function ml_z_of_substring_base(base, s, pos, len) {
  s = caml_jsbytes_of_string(s);
  if (pos != 0 || len != s.length) {
    if (s.length - pos < len) {
      caml_invalid_argument("Z.of_substring_base: invalid offset or length");
    }
    s = s.slice(pos, pos + len);
  }
  return jsoo_z_of_js_string_base(base, s);
}

//external compare: t -> t -> int
//Provides: ml_z_compare const
function ml_z_compare(z1, z2) {
  return z1 == z2 ? 0 : z1 > z2 ? 1 : -1;
}

//external equal: t -> t -> bool
//Provides: ml_z_equal const
function ml_z_equal(z1, z2) {
  return z1 == z2 ? 1 : 0;
}

//external sign: t -> int
//Provides: ml_z_sign const
function ml_z_sign(z1) {
  return z1 == 0 ? 0 : z1 > 0 ? 1 : -1;
}

//external gcd: t -> t -> t
//Provides: ml_z_gcd
//Requires: ml_z_normalize
function ml_z_gcd(z1, z2) {
  let a = BigInt(z1);
  if (a < 0) a = -a;
  let b = BigInt(z2);
  if (b < 0) b = -b;
  if (a === b) return ml_z_normalize(a);
  if (a === 0n) return ml_z_normalize(b);
  if (b === 0n) return ml_z_normalize(a);
  let c = 1n, d, t;
  function min(a, b) { return a < b ? a : b }
  while ((a & 1n) === 0n && (b & 1n) === 0n) {
    d = min(a & -a, b & -b);
    a /= d;
    b /= d;
    c *= d;
  }
  while ((a & 1n) === 0n) {
    a /= a & -a;
  }
  do {
    while ((b & 1n) === 0n) {
      b /= b & -b;
    }
    if (a > b) {
      t = b; b = a; a = t;
    }
    b -= a;
  } while (b !== 0n);
  return ml_z_normalize(c === 1n ? a : a * c);
}

//external numbits: t -> int
//Provides: ml_z_numbits const
function ml_z_numbits(z1) {
  if (z1 < 0) z1 = -z1;
  let n = 0;
  let upperBound = 1n;
  while (upperBound <= z1) {
    n += 1;
    upperBound = upperBound << 1n;
  }
  return n; // 2^{n-1} <= |x| < 2^n
}

//external fits_int: t -> bool
//Provides: ml_z_fits_int const
function ml_z_fits_int(z1) {
  return typeof z1 === "number" ? +(z1 === (z1 | 0)) : 0;
}

//external fits_int32: t -> bool
//Provides: ml_z_fits_int32
//Requires: ml_z_fits_int
function ml_z_fits_int32(z1) {
  return ml_z_fits_int(z1);
}

//external fits_int32_unsigned: t -> bool
//Provides: ml_z_fits_int32_unsigned
function ml_z_fits_int32_unsigned(z1) {
  return 0 <= z1 && z1 <= 4294967295;
}

//external fits_int64_unsigned: t -> bool
//Provides: ml_z_fits_int64_unsigned
function ml_z_fits_int64_unsigned(z1) {
  return 0 <= z1 && z1 <= 18446744073709551615n;
}

//external fits_nativeint_unsigned: t -> bool
//Provides: ml_z_fits_nativeint_unsigned
//Requires: ml_z_fits_int32_unsigned
function ml_z_fits_nativeint_unsigned(z1) {
  return ml_z_fits_int32_unsigned(z1);
}


//external fits_int64: t -> bool
//Provides: ml_z_fits_int64
function ml_z_fits_int64(z1) {
  if (z1 <= 9223372036854775807n && z1 >= -9223372036854775808n)
    return 1
  else
    return 0
}

//external fits_nativeint: t -> bool
//Provides: ml_z_fits_nativeint
//Requires: ml_z_fits_int
function ml_z_fits_nativeint(z1) {
  return ml_z_fits_int(z1);
}

//external powm: t -> t -> t -> t
//Provides: ml_z_powm
//Requires: ml_z_normalize, ml_z_invert, caml_raise_zero_divide
//Requires: jsoo_bigint_mod_pow
function ml_z_powm(z1, z2, z3) {
  if (z3 == 0) caml_raise_zero_divide();
  if (z3 == 1 || z3 == -1) return 0;
  if (z2 == 0) return 1;
  let z3n = BigInt(z3);
  if (z2 < 0) {
    let inv = BigInt(ml_z_invert(z1, z3));
    let r = jsoo_bigint_mod_pow(inv, BigInt(-z2), z3n);
    if (r < 0) r = r + (z3n < 0 ? -z3n : z3n);
    return ml_z_normalize(r);
  } else {
    let r = jsoo_bigint_mod_pow(BigInt(z1), BigInt(z2), z3n);
    if (r < 0) r = r + (z3n < 0 ? -z3n : z3n);
    return ml_z_normalize(r);
  }
}

//external pown: t -> t -> t
//Provides: ml_z_pow
//Requires: caml_failwith, ml_z_normalize, caml_invalid_argument
function ml_z_pow(z1, i1) {
  if (i1 < 0) {
    caml_invalid_argument("Z.pow: exponent must be nonnegative");
  }
  return ml_z_normalize(BigInt(z1) ** BigInt(i1));
}

//external hash: t -> int
//Provides: ml_z_hash const
//Requires: caml_hash_mix_int
function ml_z_hash(z1) {
  let z1n = BigInt(z1);
  let neg = z1n < 0;
  if (neg) z1n = -z1n;

  let acc = 0, len = 1;
  let left = z1n;
  while (left >= 2 ** 32) {
    acc = caml_hash_mix_int(acc, Number(left & 0xffffffffn));
    left >>= 32n;
    len += 1;
  }
  acc = caml_hash_mix_int(acc, Number(left) | 0);
  if (len % 2 !== 0) {
    acc = caml_hash_mix_int(acc, 0);
  }
  if (neg) {
    acc = acc + 1
  }
  return acc | 0
}

//external to_bits: t -> string
//Provides: ml_z_to_bits const
//Requires: caml_string_of_jsbytes, caml_str_repeat
function ml_z_to_bits(z1) {
  let z1n = BigInt(z1);
  if (z1n < 0) z1n = -z1n;
  let res = "";
  while (z1n !== 0n) {
    res += String.fromCharCode(Number(z1n % 256n) | 0);
    z1n /= 256n;
  }
  while (res.length % 4 != 0) {
    res += String.fromCharCode(0);
  }
  return caml_string_of_jsbytes(res);
}

//external of_bits: string -> t
//Provides: ml_z_of_bits const
//Requires: caml_string_unsafe_get, caml_ml_string_length, ml_z_normalize
function ml_z_of_bits(z1) {
  let r = 0n;
  let base = 1n;
  for (let i = 0; i < caml_ml_string_length(z1); i++) {
    let d = caml_string_unsafe_get(z1, i);
    r += base * BigInt(d);
    base *= 256n;
  }
  return ml_z_normalize(r);
}

//external powm_sec: t -> t -> t -> t
//Provides: ml_z_powm_sec
//Requires: caml_failwith, ml_z_powm, caml_invalid_argument
function ml_z_powm_sec(z1, z2, z3) {
  if (z1 < 0) z3 = -z3;
  // powm_sec requires that the exponent be positive
  if (z2 < 1) {
    caml_invalid_argument("Z.powm_sec: exponent must be positive");
  }
  if ((BigInt(z3) & 1n) !== 1n) {
    caml_invalid_argument("Z.powm_sec: modulus must be odd");
  }
  return ml_z_powm(z1, z2, z3)
}

//external root: t -> int -> t
//Provides: ml_z_root
//Requires: ml_z_pow, ml_z_normalize, caml_invalid_argument
function ml_z_root(z, i) {
  if (i % 2 === 0 && z < 0) {
    caml_invalid_argument("Z.root: even root of a negative number");
  }

  if (z == 0 || z == 1) {
    return Number(z) | 0;
  }

  let start = 0n;
  let end = BigInt(z);
  let ans = 0n;

  let bigi = BigInt(i);
  while (start <= end) {
    let mid = (start + end) / 2n;
    let po = mid ** bigi;
    if (po == z) {
      return ml_z_normalize(mid);
    } else if (po < z) {
      start = mid + 1n;
      ans = mid;
    } else {
      end = mid - 1n;
    }
  }
  return ml_z_normalize(ans);
}

//external rootrem: t -> int -> t * t
//Provides: ml_z_rootrem
//Requires: ml_z_pow, ml_z_normalize, caml_invalid_argument
function ml_z_rootrem(z, i) {
  if (i % 2 === 0 && z < 0) {
    caml_invalid_argument("Z.rootrem: even root of a negative number");
  }

  if (z == 0 || z === 1) {
    return [0, Number(z) | 0, 0];
  }

  let start = 0n;
  let end = BigInt(z);
  let ans = 0n;

  let bigi = BigInt(i);
  while (start <= end) {
    let mid = (start + end) / 2n;
    let po = mid ** bigi;
    if (po == z) {
      return [0, ml_z_normalize(mid), 0];
    } else if (po < z) {
      start = mid + 1n;
      ans = mid;
    } else {
      end = mid - 1n;
    }
  }
  return [0, ml_z_normalize(ans), ml_z_normalize(BigInt(z) - (ans ** bigi))];
}

//external invert: t -> t -> t
//Provides: ml_z_invert
//Requires: caml_raise_zero_divide, ml_z_gcdext_intern, ml_z_normalize
function ml_z_invert(a, n) {
  // Because [a.modInv(n)] produces different results for edge cases,
  // we wrote our own implementation based on gcdext_intern.
  if (n == 1 || n == -1)
    return 0;
  if (n == 0 && (a == 1 || a == -1)) {
    return a;
  }
  if (n == 0 || a == 0) {
    caml_raise_zero_divide();
  }
  let x = ml_z_gcdext_intern(a, n);
  let r = BigInt(x[2]);
  a = BigInt(a);
  n = BigInt(n);
  if (n < 0) n = -n;
  let tmp = (a * r) % n;
  if (tmp < 0) tmp += n;
  if (r < 0) r += n;
  if (tmp == 1) {
    return ml_z_normalize(r);
  }
  caml_raise_zero_divide();
}

//external perfect_power: t -> bool
//Provides: ml_z_perfect_power
//Requires: caml_failwith, ml_z_numbits, ml_z_root, ml_z_pow
function ml_z_perfect_power(z) {
  // Return true if op is a perfect power, i.e., if there exist integers a and
  // b, with b > 1, such that op = a^b.
  // Otherwise false.
  if (z == 0 || z == 1 || z == -1) return 1;

  let zp = z < 0 ? -z : z;
  let log2z = ml_z_numbits(zp);
  for (let b = 2; b <= log2z; b++) {
    if (z < 0 && b % 2 == 0) continue;
    let p = ml_z_root(zp, b);
    if (z < 0) p = -p;
    let r = ml_z_pow(p, b);
    if (z == r) {
      return 1;
    }
  }
  return 0;
}

//external perfect_square: t -> bool
//Provides: ml_z_perfect_square
//Requires: ml_z_root
function ml_z_perfect_square(z) {
  if (z < 0) return 0;
  let root = BigInt(ml_z_root(z, 2));
  if (root * root == z) {
    return 1;
  } else {
    return 0
  }
}

//Must have 0 <= low <= high
//Provides: jsoo_bigint_rand_between
function jsoo_bigint_rand_between(low, high) {
  let range = high - low + 1n;
  let base = 1e7, bigbase = BigInt(base);
  let result = 0n, restricted = true;

  let digits = [];
  while (range >= bigbase) {
    digits.push(Number(range % bigbase));
    range /= bigbase;
  }
  digits.push(Number(range));
  digits.reverse();

  for (let i = 0; i < digits.length; i++) {
    let top = restricted ? digits[i] | 0 : base;
    range /= bigbase;
    let digit = Math.floor(Math.random() * top);
    result *= bigbase;
    result += BigInt(digit);
    if (digit < top) restricted = false;
  }
  return low + result;
}

//Provides: jsoo_bigint_mod_pow
function jsoo_bigint_mod_pow(self, exp, mod) {
  if (mod === 0n) throw new Error("Cannot take modPow with modulo 0");
  if (exp < 0) throw new Error("Cannot take modPow with negative exponent");
  let r = 1n, base = self % mod;
  while (exp > 0) {
    if (base === 0n) return 0n;
    if ((exp & 1n) === 1n) r = (r * base) % mod;
    exp >>= 1n;
    base = (base * base) % mod;
  }
  return r;
}

//external probab_prime: t -> int -> int
//Provides: ml_z_probab_prime const
//Requires: jsoo_bigint_mod_pow, jsoo_bigint_rand_between
//Note: called with [1n] from [ml_z_primorial]
function ml_z_probab_prime(z, i) {
  if (z < 0) z = -z;

  // Test for basic primes (multiples of 2, 3, 5)
  if (z == 1) return 0;
  if (z == 2 || z == 3 || z == 5) return 1;
  let n = BigInt(z);
  if (n % 2n === 0n || n % 3n === 0n || n % 5n === 0n) return 0;
  if (z < 49) return 1;

  // Miller-Rabin test
  let nPrev = n - 1n,
    b = nPrev,
    r = 0,
    a, d, j, x;
  while (b % 2n === 0n) b /= 2n, r++;
  next: for (j = 0; j < i; j++) {
    a = jsoo_bigint_rand_between(2n, n - 2n);
    x = jsoo_bigint_mod_pow(a, b, n);
    if (x === 1n || x === nPrev) continue;
    for (d = r - 1; d != 0; d--) {
      x = (x * x) % n;
      if (x === 1n) return 0;
      if (x === nPrev) continue next;
    }
    return 0;
  }
  return 1;
}

//external nextprime: t -> t
//Provides: ml_z_nextprime const
//Requires: ml_z_normalize, ml_z_probab_prime
function ml_z_nextprime(z1) {
  // Interestingly, the zarith next_prime only returns
  // probabalistic primes.  We do the same, with the
  // same probablistic parameter of 25.
  // https://fossies.org/dox/gmp-6.1.2/mpz_2nextprime_8c_source.html

  if (z1 < 1 || z1 == 1) {
    return 2;
  }

  let z1n = BigInt(z1)
  if ((z1n & 1n) === 1n) {
    z1n += 2n;
  } else {
    z1n += 1n;
  }

  for (; ;) {
    if (ml_z_probab_prime(z1n, 25)) {
      return ml_z_normalize(z1n);
    } else {
      z1n += 2n;
    }
  }
}

//external c_extract: t -> int -> int -> t
//Provides: ml_z_extract
//Requires: caml_failwith, ml_z_normalize
function ml_z_extract(z1, pos, len) {
  return ml_z_normalize((BigInt(z1) >> BigInt(pos)) & ((1n << BigInt(len)) - 1n));
}

//external c_extract_small: t -> int -> int -> t
//Provides: ml_z_extract_small
function ml_z_extract_small(z1, pos, len) {
  return Number(BigInt.asIntN(32, z1 >> BigInt(pos))) & ((1 << len) - 1);
}

//external gcdext_intern: t -> t -> (t * t * bool)
//Provides: ml_z_gcdext_intern
//Requires: caml_raise_zero_divide, ml_z_normalize
function ml_z_gcdext_intern(z1, z2) {
  if (z1 == 0) caml_raise_zero_divide();
  let a = BigInt(z1);
  let b = BigInt(z2);
  let x = 0n;
  let lastx = 1n;
  let y = 1n;
  let lasty = 1n;
  let q, t, r;
  while (b !== 0n) {
    q = a / b;
    r = a - q * b;
    t = x;
    x = lastx - q * x;
    lastx = t;
    t = y;
    y = lasty - q * y;
    lasty = t;
    a = b;
    b = r;
  }
  if (a < 0)
    return [0, ml_z_normalize(-a), ml_z_normalize(-lastx), 1]
  else
    return [0, ml_z_normalize(a), ml_z_normalize(lastx), 1]
}

//external sqrt: t -> t
//Provides: ml_z_sqrt
//Requires: ml_z_root, caml_invalid_argument
function ml_z_sqrt(z1) {
  if (z1 < 0) {
    caml_invalid_argument("Z.sqrt: square root of a negative number");
  }
  return ml_z_root(z1, 2);
}

//external sqrt_rem: t -> (t * t)
//Provides: ml_z_sqrt_rem
//Requires: ml_z_root, caml_invalid_argument, ml_z_normalize
function ml_z_sqrt_rem(z) {
  if (z < 0) {
    caml_invalid_argument("Z.sqrt_rem: square root of a negative number");
  }
  let root = ml_z_root(z, 2);
  let rootn = BigInt(root);
  let diff = BigInt(z) - rootn * rootn;
  return [0, root, ml_z_normalize(diff)]
}

//external trailing_zeros: t -> int
//Provides: ml_z_trailing_zeros const
function ml_z_trailing_zeros(z) {
  if (z == 0) {
    // max_int in 32bit
    return 0x7fffffff;
  }
  if (z < 0) z = -z;
  let zn = BigInt(z);
  let i = 0;
  zn = (zn ^ (zn - 1n)) >> 1n;
  for (i = 0; zn !== 0n; i++) zn >>= 1n;
  return i;
}

//external popcount: t -> int
//Provides: ml_z_popcount
//Requires: caml_raise_constant, caml_named_value
function ml_z_popcount(z) {
  if (z < 0) {
    caml_raise_constant(caml_named_value("ml_z_overflow"));
  }
  z = BigInt(z);
  let i;
  for (i = 0; z !== 0n; i++) {
    z &= z - 1n;
  }
  if (i !== (i | 0)) caml_raise_constant(caml_named_value("ml_z_overflow"));
  return i | 0;
}

//external hamdist: t -> t -> int
//Provides: ml_z_hamdist
//Requires: ml_z_popcount, caml_invalid_argument, caml_raise_constant
//Requires: caml_named_value, ml_z_normalize
function ml_z_hamdist(z1, z2) {
  if ((z1 < 0) !== (z2 < 0)) {
    caml_raise_constant(caml_named_value("ml_z_overflow"));
  }
  if ((typeof z1 == 'bignum' || typeof z2 == 'bignum') && (z1 < 0 || z2 < 0)) {
    caml_invalid_argument("Z.hamdist: negative arguments");
  }
  return ml_z_popcount(BigInt(z1) ^ BigInt(z2));
}

//external size: t -> int
//Provides: ml_z_size const
function ml_z_size(z1) {
  // Claim to be a 32-bit architecture.
  if (z1 < 0) z1 = -z1;
  let z1n = BigInt(z1);
  let len = 1;
  while (z1n >= 2 ** 32) { len += 1; z1n >>= 32n; }
  return len | 0;
}

//external divexact: t -> t -> t
//Provides: ml_z_divexact
//Requires: ml_z_div
function ml_z_divexact(z1, z2) {
  return ml_z_div(z1, z2);
}


//Provides: caml_zarith_marshal
function caml_zarith_marshal(writer, v, sz) {
  let vn = BigInt(v);
  let neg = vn < 0;
  if (neg) vn = -vn;
  let bits = [];
  while (vn >= 2 ** 32) {
    bits.push(Number(vn & 0xffffffffn));
    vn >>= 32n;
  }
  bits.push(Number(vn) | 0);
  writer.write(8, neg ? 1 : 0);
  let block = bits.length;
  let len = block * 4;
  writer.write(32, len);
  for (let i = 0; i < block; i++) {
    writer.write(8, (bits[i] >>> 0) & 0xff);
    writer.write(8, (bits[i] >>> 8) & 0xff);
    writer.write(8, (bits[i] >>> 16) & 0xff);
    writer.write(8, (bits[i] >>> 24) & 0xff);
  }
  sz[0] = 4 * (1 + (((len + 3) / 4) | 0));
  sz[1] = 8 * (1 + (((len + 7) / 8) | 0));
}

//Provides: caml_zarith_unmarshal
//Requires: caml_failwith, ml_z_normalize
function caml_zarith_unmarshal(reader, sz) {
  let negate;
  switch (reader.read8u()) {
    case 1: negate = true; break;
    case 0: negate = false; break;
    default: caml_failwith("input_value: z (malformed input)");
  }
  let len = reader.read32u();
  let x = 0n;
  for (let i = 0; i < len / 4; i++) {
    let y = reader.read8u();
    y |= reader.read8u() << 8
    y |= reader.read8u() << 16
    y |= reader.read8u() << 24
    x |= BigInt.asUintN(32, BigInt(y)) << (BigInt(i) * 32n);
  }
  if (negate) x = -x;
  sz[0] = len + 4;
  return ml_z_normalize(x)
}

//Provides: ml_z_divisible
function ml_z_divisible(a, b) {
  if (a == 0 && b == 0) return 1;
  if (b == 0) return 0;
  if (b == 1) return 1;
  if (b == 2 || b == -2) return +((BigInt(a) & 1n) === 0n);
  return +((BigInt(a) % BigInt(b)) === 0n);
}

//Provides: ml_z_congruent
//Requires: ml_z_divisible
function ml_z_congruent(a, b, c) {
  return ml_z_divisible(BigInt(a) - BigInt(b), c);
}

//external remove : t -> t -> t * int
//Provides: ml_z_remove
//Requires: ml_z_normalize, caml_raise_zero_divide
function ml_z_remove(a, b) {
  if (b == 0) caml_raise_zero_divide();
  if (a == 0 || b == 1 || b == -1) return [0, a, 0];
  let i = 0;
  a = BigInt(a);
  b = BigInt(b);
  while ((a % b) === 0n) {
    a /= b;
    i++;
  }
  return [0, ml_z_normalize(a), i];
}

//external fac : int -> t
//Provides: ml_z_fac
//Requires: ml_z_facM, caml_invalid_argument
function ml_z_fac(i) {
  if (i < 0) caml_invalid_argument("Z.fac: non-positive argument");
  return ml_z_facM(i, 1);
}

//external fac2 : int -> t
//Provides: ml_z_fac2
//Requires: ml_z_facM, caml_invalid_argument
function ml_z_fac2(i) {
  if (i < 0) caml_invalid_argument("Z.fac2: non-positive argument");
  return ml_z_facM(i, 2);
}

//external facM : int -> int -> t
//Provides: ml_z_facM
//Requires: caml_invalid_argument, ml_z_normalize
function ml_z_facM(i, m) {
  if (i < 0 || m < 0) caml_invalid_argument("Z.facM: non-positive argument");
  if (m == 0) return i;
  let mn = BigInt(m);
  let current = BigInt(i);
  let res = 1n;
  while (current > 0) {
    res *= current;
    current -= mn;
  }
  return ml_z_normalize(res);
}

//external fib : int -> t
//Provides: ml_z_fib
//Requires: caml_invalid_argument, ml_z_normalize
function ml_z_fib(i) {
  if (i < 0) caml_invalid_argument("Z.fib: negative arguments");
  if (i == 0 || i == 1) return i;
  let a = 0n, b = 1n;
  for (let k = 1; k < i; k++) {
    let b2 = b;
    b += a;
    a = b2;
  }
  return ml_z_normalize(b);
}

//external lucnum : int -> t
//Provides: ml_z_lucnum
//Requires: caml_invalid_argument, ml_z_normalize
function ml_z_lucnum(i) {
  if (i < 0) caml_invalid_argument("Z.lucnum: negative arguments");
  if (i == 0) return 2;
  if (i == 1) return 1;
  let a = 2n, b = 1n;
  for (let k = 1; k < i; k++) {
    let b2 = b;
    b += a;
    a = b2;
  }
  return ml_z_normalize(b);
}

//Provides: ml_z_jacobi
//Requires: caml_invalid_argument
function ml_z_jacobi(n, k) {
  let nn = BigInt(n);
  let kn = BigInt(k);
  //assert(k > 0 and k % 2 == 1)
  if (kn <= 0 || kn % 2n !== 1n)
    caml_invalid_argument("Z.jacobi: second argument is negative or even");
  nn = nn % kn;
  if (nn < 0) nn += kn;
  let t = 1;
  while (nn !== 0n) {
    while (nn % 2n === 0n) {
      nn /= 2n;
      let r = kn % 8n
      if (r === 3n || r === 5n) {
        t = -t
      }
    }
    let n1 = nn, k1 = kn;
    nn = k1;
    kn = n1;
    if (nn % 4n === 3n && kn % 4n === 3n) {
      t = -t
    }
    nn = nn % kn
  }
  if (kn === 1n)
    return t
  else
    return 0
}

//Provides: ml_z_legendre
//Requires: ml_z_jacobi
function ml_z_legendre(a, b) {
  return ml_z_jacobi(a, b);
}

//Provides: ml_z_kronecker
//Requires: caml_failwith
function ml_z_kronecker(n, k) {
  caml_failwith("ml_z_kronecker is not implemented");
}

//Provides: ml_z_primorial
//Requires: ml_z_probab_prime, ml_z_normalize, caml_invalid_argument
function ml_z_primorial(a) {
  if (a < 0) caml_invalid_argument("Z.primorial: non-positive argument");
  let z1 = 1n;
  let res = 1n;
  while (z1 <= a) {
    if (ml_z_probab_prime(z1, 25)) {
      res *= z1;
    }
    if (z1 === 1n || z1 === 2n) z1 += 1n;
    else z1 += 2n;
  }
  return ml_z_normalize(res);
}

//Provides: ml_z_bin
//Requires: ml_z_normalize, caml_invalid_argument
function ml_z_bin(n, k) {
  if (k < 0) caml_invalid_argument("Z.bin: non-positive argument");
  n = BigInt(n);
  k = BigInt(k);
  let coeff = 1n;
  for (let x = n - k + 1n; x <= n; x++) coeff *= x;
  for (let x = 1n; x <= k; x++) coeff /= x;
  return ml_z_normalize(coeff);
}
