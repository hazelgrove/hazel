/* global bigInt */
/* eslint-disable no-unused-vars */

// We represent a [Z.t] as a javascript 32bit integers if it fits or as a bigInt.

//Provides: ml_z_normalize
//Requires: bigInt
function ml_z_normalize(x){
  var y = x.toJSNumber () | 0;
  if(x.equals(bigInt(y))) return y;
  return x;
}

//Provides: ml_z_mul_overflows
function ml_z_mul_overflows(x,y){
  var z = x*y;
  return z != (z|0);
}

//external init: unit -> unit
//Provides: ml_z_init
//Requires: caml_zarith_marshal, caml_zarith_unmarshal, caml_custom_ops, ml_z_hash, ml_z_compare
function ml_z_init(unit) {
  caml_custom_ops['_z'] =
    { serialize : caml_zarith_marshal,
      deserialize : caml_zarith_unmarshal,
      hash : ml_z_hash,
      compare : ml_z_compare,
    };
  return 0 }

//external neg: t -> t
//Provides: ml_z_neg const
//Requires: bigInt, ml_z_normalize
function ml_z_neg(z1) {
  return ml_z_normalize(bigInt(z1).negate());
}

//external add: t -> t -> t
//Provides: ml_z_add const
//Requires: bigInt, ml_z_normalize
function ml_z_add(z1, z2) {
  return ml_z_normalize(bigInt(z1).add(bigInt(z2)));
}

//external sub: t -> t -> t
//Provides: ml_z_sub const
//Requires: bigInt, ml_z_normalize
function ml_z_sub(z1, z2) {
  return ml_z_normalize(bigInt(z1).subtract(bigInt(z2)));
}

//external mul: t -> t -> t
//Provides: ml_z_mul const
//Requires: bigInt, ml_z_normalize
function ml_z_mul(z1, z2) {
  return ml_z_normalize(bigInt(z1).multiply(bigInt(z2)));
}

//external div: t -> t -> t
//Provides: ml_z_div
//Requires: bigInt, caml_raise_zero_divide, ml_z_normalize
function ml_z_div(z1, z2) {
  z2 = bigInt(z2)
  if(z2.equals(bigInt(0))) caml_raise_zero_divide();
  return ml_z_normalize(bigInt(z1).divide(bigInt(z2)))
}

//external cdiv: t -> t -> t
//Provides: ml_z_cdiv
//Requires: bigInt, ml_z_div, ml_z_sign, ml_z_add
function ml_z_cdiv(z1, z2) {
  var z1_pos = ml_z_sign(z1);
  var z2_pos = ml_z_sign(z2);
  if (z1_pos * z2_pos > 0) /* Multiplication is like a signwise xor */ {
    if (!bigInt(z1).mod(bigInt(z2)).equals(bigInt(0))) {
      return ml_z_add(ml_z_div(z1, z2), bigInt(1)) ;
    }
  }
  return ml_z_div(z1, z2);
}

//external fdiv: t -> t -> t
//Provides: ml_z_fdiv
//Requires: bigInt, ml_z_div, ml_z_sign, ml_z_sub
function ml_z_fdiv(z1, z2) {
  var z1_pos = ml_z_sign(z1);
  var z2_pos = ml_z_sign(z2);
  if (z1_pos * z2_pos < 0) /* Multiplication is like a signwise xor */ {
    if (!bigInt(z1).mod(bigInt(z2)).equals(bigInt(0))) {
      return ml_z_sub(ml_z_div(z1, z2), bigInt(1)) ;
    }
  }
  return ml_z_div(z1, z2);
}

//external rem: t -> t -> t
//Provides: ml_z_rem
//Requires: bigInt, caml_raise_zero_divide, ml_z_normalize
function ml_z_rem(z1, z2) {
  z2 = bigInt(z2);
  if (z2.equals(bigInt(0))) {
    caml_raise_zero_divide();
  }
  return ml_z_normalize(bigInt(z1).mod(z2));
}

//external div_rem: t -> t -> (t * t)
//Provides: ml_z_div_rem
//Requires: ml_z_div, ml_z_rem
function ml_z_div_rem(z1, z2) {
  return [0, ml_z_div(z1,z2), ml_z_rem(z1, z2)]
}
//external succ: t -> t
//Provides: ml_z_succ const
//Requires: bigInt, ml_z_normalize
function ml_z_succ(z1) {
  return ml_z_normalize(bigInt(z1).next());
}

//external pred: t -> t
//Provides: ml_z_pred const
//Requires: bigInt, ml_z_normalize
function ml_z_pred(z1) {
  return ml_z_normalize(bigInt(z1).prev());
}

//external abs: t -> t
//Provides: ml_z_abs const
//Requires: bigInt, ml_z_normalize
function ml_z_abs(z1) {
  return ml_z_normalize(bigInt(z1).abs());
}

//external logand: t -> t -> t
//Provides: ml_z_logand const
//Requires: bigInt, ml_z_normalize
function ml_z_logand(z1, z2) {
  return ml_z_normalize(bigInt(z1).and(bigInt(z2)));
}

//external logor: t -> t -> t
//Provides: ml_z_logor const
//Requires: bigInt, ml_z_normalize
function ml_z_logor(z1, z2) {
  return ml_z_normalize(bigInt(z1).or(bigInt(z2)));
}

//external logxor: t -> t -> t
//Provides: ml_z_logxor const
//Requires: bigInt, ml_z_normalize
function ml_z_logxor(z1, z2) {
  return ml_z_normalize(bigInt(z1).xor(bigInt(z2)));
}

//external lognot: t -> t
//Provides: ml_z_lognot const
//Requires: bigInt,ml_z_normalize
function ml_z_lognot(z1) {
  return ml_z_normalize(bigInt(z1).not());
}

//external shift_left: t -> int -> t
//Provides: ml_z_shift_left const
//Requires: bigInt, ml_z_normalize
function ml_z_shift_left(z1, amt) {
  return ml_z_normalize(bigInt(z1).shiftLeft(amt));
}

//external shift_right: t -> int -> t
//Provides: ml_z_shift_right const
//Requires: bigInt, ml_z_normalize
function ml_z_shift_right(z1, amt) {
  return ml_z_normalize(bigInt(z1).shiftRight(amt));
}

//external shift_right_trunc: t -> int -> t
//Provides: ml_z_shift_right_trunc const
//Requires: bigInt, ml_z_div
function ml_z_shift_right_trunc(z1, z2) {
  return ml_z_div(bigInt(z1), bigInt(2).pow(z2))
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
//Requires: bigInt, caml_int64_compare, caml_int64_neg, ml_z_normalize
//Requires: caml_int64_create_lo_hi,caml_int64_hi32,caml_int64_lo32
function ml_z_of_int64(i64) {
  var neg = false;
  if(caml_int64_compare(i64, caml_int64_create_lo_hi(0,0)) < 0) {
    neg = true;
    i64 = caml_int64_neg(i64)
  }
  var lo = caml_int64_lo32(i64) >>> 0;
  var hi = caml_int64_hi32(i64) >>> 0;
  var x = bigInt(lo).add(bigInt(hi).shiftLeft(32));
  if(neg) { x = x.negate() };
  return ml_z_normalize(x)
}

//external of_float: float -> t
//Provides: ml_z_of_float
//Requires: bigInt, caml_raise_constant, caml_named_value, ml_z_normalize
function ml_z_of_float(f1) {
  if(f1 == Infinity || f1 == -Infinity || f1 != f1)
    caml_raise_constant(caml_named_value("ml_z_overflow"));
  return ml_z_normalize(bigInt(f1<0?Math.ceil(f1):Math.floor(f1)));
}

//external to_int: t -> int
//Provides: ml_z_to_int
//Requires: bigInt, caml_raise_constant, caml_named_value
function ml_z_to_int(z1) {
  if (z1 == (z1 | 0)) return z1 | 0;
  caml_raise_constant(caml_named_value("ml_z_overflow"));
}

//external to_int32: t -> int32
//Provides: ml_z_to_int32
//Requires: ml_z_to_int
function ml_z_to_int32(z1) { return ml_z_to_int(z1) }

//external to_int64: t -> int64
//Provides: ml_z_to_int64
//Requires: bigInt, ml_z_fits_int64, caml_raise_constant, caml_named_value
//Requires: caml_int64_create_lo_hi
function ml_z_to_int64(z1) {
  z1 = bigInt(z1)
  if(!ml_z_fits_int64(z1)) {
    caml_raise_constant(caml_named_value("ml_z_overflow"));
  }
  var mask = bigInt(0xffffffff)
  var lo = z1.and(mask).toJSNumber();
  var hi = z1.shiftRight(32).and(mask).toJSNumber();
  var x = caml_int64_create_lo_hi(lo, hi);
  return x;
}

//external testbit: t -> bool
//Provides: ml_z_testbit
//Requires: bigInt
function ml_z_testbit(z,pos){
  z = bigInt(z);
  return (z.shiftRight(pos).and(bigInt(1)).toJSNumber())|0;
}

//external to_nativeint: t -> nativeint
//Provides: ml_z_to_nativeint
//Requires: ml_z_to_int
function ml_z_to_nativeint(z1) { return ml_z_to_int(z1) }

//external format: string -> t -> string
//Provides: ml_z_format 
//Requires: bigInt
//Requires: caml_jsbytes_of_string, caml_failwith, caml_string_of_jsbytes, ml_z_normalize
function ml_z_format(fmt, z1) {
  z1 = bigInt(z1);
  var fmt = caml_jsbytes_of_string(fmt);
  // https://github.com/ocaml/Zarith/blob/d0555d451ce295c4497f24a8d9993f8dd23097df/z.mlip#L297
  var base = 10;
  var cas = 0;
  var width = 0;
  var alt = 0;
  var dir = 0;
  var sign = '';
  var pad = ' ';
  var idx = 0;
  var prefix="";
  while(fmt[idx] == '%') idx++;
  for(;; idx++) {
    if(fmt[idx] == '#') alt = 1;
    else if (fmt[idx] == '0') pad = '0';
    else if (fmt[idx] == '-') dir = 1;
    else if (fmt[idx] == ' ' || fmt[idx] == '+') sign = fmt[idx];
    else break;
  }
  if(z1.lt(bigInt(0))){sign = '-';z1 = z1.negate()};
  for(;fmt[idx]>='0' && fmt[idx] <='9';idx++)
    width=10*width + (+fmt[idx]);
  switch(fmt[idx]){
  case 'i': case 'd': case 'u': break;
  case 'b': base = 2; if(alt) prefix = "0b"; break;
  case 'o': base = 8; if(alt) prefix = "0o"; break;
  case 'x': base = 16; if(alt) prefix = "0x"; break;
  case 'X': base = 16; if(alt) prefix = "0X"; cas = 1; break;
  default:
    caml_failwith("Unsupported format '" + fmt + "'");
  }
  if (dir) pad = ' ';
  var res = z1.toString(base);
  if (cas === 1) {
    res = res.toUpperCase();
  }
  var size = res.length;
  if (pad == ' ') {
    if(dir) {
      res = sign + prefix + res;
      for(;res.length<width;) res = res + pad;
    } else {
      res = sign + prefix + res;
      for(;res.length<width;) res = pad + res;
    }
  } else {
    var pre = sign + prefix;
    for(;res.length+pre.length<width;) res = pad + res;
    res = pre + res;
  }
  return caml_string_of_jsbytes(res);
}

//Provides: jsoo_z_of_js_string_base
//Requires: bigInt, caml_invalid_argument, ml_z_normalize
function jsoo_z_of_js_string_base(base, s) {
  if (base == 0) { // https://github.com/ocaml/Zarith/blob/b8dbaf48a7927061df699ad7ce642bb4f1fe5308/caml_z.c#L598
    base = 10;
    var p = 0;
    var sign = 1;
    if(s[p] == '-') { sign = -1; p++ }
    else if (s[p] == '+') { p++ }
    if (s[p] == '0') {
      p ++;
      if (s.length == p) {
        return 0;
      } else {
        var bc = s[p];
        if (bc == 'o' || bc == 'O') {
          base = 8;
        } else if (bc == 'x' || bc == 'X') {
          base = 16;
        } else if (bc == 'b' || bc == 'B') {
          base = 2;
        } 
        if(base != 10) {
          s = s.substring(p+1);
          if(sign == -1) s = "-" + s;
        }
      }
    }
  }
  
  function digit(code){
    if(code >= 48 && code <= 57) return code - 48;
    if(code >= 97 && code <= 102) return code - 97 + 10;
    if(code >= 65 && code <= 70) return code - 65 + 10;
  }
  var i = 0;
  if (s[i] == '+') {
    //remove leading '+'
    s = s.substring(1);
  }
  else if(s[i] == '-') i++;
  if(s[i] == '_') caml_invalid_argument("Z.of_substring_base: invalid digit");
  s = s.replace(/_/g,'');
  //normalize "empty" numbers
  if(s == '-' || s == '') s = '0';
  for( ; i < s.length ; i++){
    var c = digit(s.charCodeAt(i));
    if(c == undefined || c >= base)
      caml_invalid_argument("Z.of_substring_base: invalid digit");
  }
  return ml_z_normalize(bigInt(s, base));
  
}

//external of_substring_base: int -> string -> pos:int -> len:int -> t
//Provides: ml_z_of_substring_base
//Requires: jsoo_z_of_js_string_base, caml_jsbytes_of_string, caml_invalid_argument, caml_ml_string_length
function ml_z_of_substring_base(base, s, pos, len) {
  s = caml_jsbytes_of_string(s);
  if(pos != 0 || len != s.length) {
    if (s.length - pos < len) {
      caml_invalid_argument("Z.of_substring_base: invalid offset or length");
    }
    s = s.slice(pos,pos+len);
  }
  return jsoo_z_of_js_string_base(base, s);
}

//external compare: t -> t -> int
//Provides: ml_z_compare const
//Requires: bigInt
function ml_z_compare(z1, z2) {
  return bigInt(z1).compare(bigInt(z2));
}

//external equal: t -> t -> bool
//Provides: ml_z_equal const
//Requires: bigInt
function ml_z_equal(z1, z2) {
  return bigInt(z1).equals(bigInt(z2)) ? 1 : 0;
}

//external sign: t -> int
//Provides: ml_z_sign const
//Requires: bigInt
function ml_z_sign(z1) {
  return bigInt(z1).compare(bigInt.zero);
}

//external gcd: t -> t -> t
//Provides: ml_z_gcd
//Requires: bigInt, ml_z_normalize
function ml_z_gcd(z1, z2) {
  return ml_z_normalize(bigInt.gcd(bigInt(z1), bigInt(z2)).abs());
}

//external numbits: t -> int
//Provides: ml_z_numbits const
//Requires: bigInt
function ml_z_numbits(z1) {
  z1 = bigInt(z1).abs();
  var n = 0;
  var upperBound = bigInt.one;
  while (upperBound.leq(z1)) {
    n += 1;
    upperBound = upperBound.multiply(2);
  }
  return n; // 2^{n-1} <= |x| < 2^n
}

//external fits_int: t -> bool
//Provides: ml_z_fits_int const
//Requires: bigInt
function ml_z_fits_int(z1) {
  if(z1 == (z1 | 0)) return 1;
  else return 0;
}

//external fits_int32: t -> bool
//Provides: ml_z_fits_int32
//Requires: ml_z_fits_int
function ml_z_fits_int32(z1) {
  return ml_z_fits_int(z1);
}

//external fits_int64: t -> bool
//Provides: ml_z_fits_int64
//Requires: bigInt
function ml_z_fits_int64(z1) {
  z1 = bigInt(z1)
  if (z1.compare(bigInt("9223372036854775807")) <= 0 && z1.compare(bigInt("-9223372036854775808")) >= 0)
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
//Requires: bigInt, ml_z_normalize, ml_z_invert, caml_raise_zero_divide
function ml_z_powm(z1, z2, z3) {
  var zero = bigInt(0);
  var one = bigInt(1);
  z1 = bigInt(z1);
  z2 = bigInt(z2);
  z3 = bigInt(z3);
  if(z3.equals(zero)) caml_raise_zero_divide();
  if(z3.abs().equals(one)) return 0;
  if(z2.equals(zero)) return 1;
  if(z2.lt(0)) {
    var inv = bigInt(ml_z_invert(z1, z3));
    var r = inv.modPow(z2.negate(), z3);
    if(r.lt(zero)) r = r.add(z3.abs());
    return ml_z_normalize(r);
  } else {
    var r = bigInt(z1).modPow(z2, z3);
    if(r.lt(zero)) r = r.add(z3.abs());
    return ml_z_normalize(r);
  }
}

//external pown: t -> t -> t
//Provides: ml_z_pow
//Requires: bigInt, caml_failwith, ml_z_normalize, caml_invalid_argument
function ml_z_pow(z1, i1) {
  i1 = bigInt(i1);
  if (i1.lt(bigInt(0))) {
    caml_invalid_argument("Z.pow: exponent must be nonnegative");
  }
  return ml_z_normalize(bigInt(z1).pow(i1));
}

//external hash: t -> int
//Provides: ml_z_hash const
//Requires: bigInt, caml_hash_mix_int
function ml_z_hash(z1) {
  var a = bigInt(z1).toArray(Math.pow(2, 32));
  var acc = 0;
  for (var i = 0; i < a.value.length; i++) {
    acc = caml_hash_mix_int(acc, a.value[i]);
  }
  if(a.value.length % 2 != 0) {
    acc = caml_hash_mix_int(acc, 0);
  }
  if(a.isNegative){
    acc = acc + 1
  }
  return acc | 0
}

//external to_bits: t -> string
//Provides: ml_z_to_bits const
//Requires: caml_string_of_jsbytes, caml_str_repeat, bigInt
function ml_z_to_bits(z1) {
  z1 = bigInt(z1).abs();
  var res = "";
  while(!z1.equals(bigInt(0))){
    res += String.fromCharCode(z1.mod(bigInt(256)));
    z1 = z1.divide(bigInt(256));
  }
  while(res.length % 4 != 0){
    res += String.fromCharCode(0);
  }
  return caml_string_of_jsbytes(res);
}

//external of_bits: string -> t
//Provides: ml_z_of_bits const
//Requires: caml_string_unsafe_get, caml_ml_string_length, bigInt, ml_z_normalize
function ml_z_of_bits(z1) {
  var r = bigInt.zero
  var base1 = bigInt(256);
  var base = bigInt.one;
  for(var i = 0; i < caml_ml_string_length(z1); i++){
    var d = caml_string_unsafe_get(z1,i);
    r = bigInt(base).multiply(d).add(r);
    base = bigInt(base).multiply(base1);
  }
  return ml_z_normalize(r);
}

//external powm_sec: t -> t -> t -> t
//Provides: ml_z_powm_sec
//Requires: bigInt, caml_failwith, ml_z_powm, caml_invalid_argument
function ml_z_powm_sec(z1, z2, z3) {
  z3 = bigInt(z3).abs();
  // powm_sec requires that the exponent be positive
  var one = bigInt(1);
  if (bigInt(z2).lt(one)) {
    caml_invalid_argument("Z.powm_sec: exponent must be positive");
  }
  if (!bigInt(z3).and(one).equals(one)) {
    caml_invalid_argument("Z.powm_sec: modulus must be odd");
  }
  return ml_z_powm(z1, z2, z3)
}

//external root: t -> int -> t
//Provides: ml_z_root
//Requires: ml_z_pow,  bigInt, ml_z_normalize, caml_invalid_argument
function ml_z_root(z, i) {
  var zero = bigInt(0);
  var one = bigInt(1);
  z = bigInt(z);

  if (i % 2 === 0 && z.lt(zero)) {
    caml_invalid_argument("Z.root: even root of a negative number");
  }

  if (z.equals(zero) || z.equals(one)) {
    return ml_z_normalize(z);
  }

  var start = zero;
  var end = z;
  var ans = null;

  var two = bigInt(2);

  while (start.leq(end))
  {
    var mid = start.add(end).divide(two);
    var po = mid.pow(i);
    if (po.equals(z)) {
      return ml_z_normalize(mid);
    } else if (po.lt(z)) {
      start = mid.next();
      ans = mid;
    } else {
      end = mid.prev();
    }
  }
  return ml_z_normalize(ans);
}

//external rootrem: t -> int -> t * t
//Provides: ml_z_rootrem
//Requires: ml_z_pow,  bigInt, ml_z_normalize, caml_invalid_argument
function ml_z_rootrem(z, i) {
  var zero = bigInt(0);
  var one = bigInt(1);
  z = bigInt(z);

  if (i % 2 === 0 && z.lt(zero)) {
    caml_invalid_argument("Z.rootrem: even root of a negative number");
  }

  if (z.equals(zero) || z.equals(one)) {
    return [0, ml_z_normalize(z), zero];
  }

  var start = zero;
  var end = z;
  var ans = null;

  var two = bigInt(2);

  while (start.leq(end))
  {
    var mid = start.add(end).divide(two);
    var po = mid.pow(i);
    if (po.equals(z)) {
      return [0, ml_z_normalize(mid), zero];
    } else if (po.lt(z)) {
      start = mid.next();
      ans = mid;
    } else {
      end = mid.prev();
    }
  }
  return [0, ml_z_normalize(ans), ml_z_normalize(z.minus(ans.pow(i)))];
}

//external invert: t -> t -> t
//Provides: ml_z_invert
//Requires: bigInt, caml_raise_zero_divide, ml_z_gcdext_intern, ml_z_normalize
function ml_z_invert(a, n) {
  // Because [a.modInv(n)] produces different results for edge cases,
  // we wrote our own implementation based on gcdext_intern.
  a = bigInt(a);
  n = bigInt(n);
  var zero = bigInt(0);
  var one = bigInt(1);
  if(n.abs().equals(one))
    return 0;
  if (n.equals(zero) && a.abs().equals(one)) {
    return a;
  }
  if (n.equals(zero) || a.equals(zero)) {
    caml_raise_zero_divide();
  }
  var x = ml_z_gcdext_intern(a, n);
  var r = bigInt(x[2]);
  var tmp = bigInt(a).multiply(r).mod(n);
  if(tmp.lt(zero)) tmp = tmp.add(n.abs());
  if(r.lt(zero)) r = r.add(n.abs());
  if(tmp.equals(one)) {
    return ml_z_normalize(r);
  }
  caml_raise_zero_divide();
}

//external perfect_power: t -> bool
//Provides: ml_z_perfect_power
//Requires: bigInt, caml_failwith, ml_z_numbits, ml_z_root, ml_z_pow
function ml_z_perfect_power(z) {
  // Return true if op is a perfect power, i.e., if there exist integers a and
  // b, with b > 1, such that op = a^b.
  // Otherwise false.
  z = bigInt(z);
  var zero = bigInt(0);
  var one = bigInt(1);

  if (z.equals(zero) || z.equals(one) || z.equals(one.negate())) {
    return 1;
  }
  var log2z = ml_z_numbits(z.abs());
  for (var b = 2; b <= log2z; b++) {
    if(z.lt(zero) && b % 2 == 0) continue;
    var zp = z.abs();
    var p = bigInt(ml_z_root(zp, b));
    if(z.lt(zero)) p = p.negate();
    var r = bigInt(ml_z_pow(p, b));
    if (z.equals(r)) {
      return 1;
    }
  }
  return 0;
}

//external perfect_square: t -> bool
//Provides: ml_z_perfect_square
//Requires: bigInt, ml_z_root
function ml_z_perfect_square(z) {
  z = bigInt(z);
  if (z.lt(bigInt(0))) {
    return 0;
  }
  var root = bigInt(ml_z_root(z, 2));
  if (root.multiply(root).eq(z)) {
    return 1;
  }
  else {
    return 0
  };
}

//external probab_prime: t -> int -> int
//Provides: ml_z_probab_prime const
//Requires: bigInt
function ml_z_probab_prime(z, i) {
  if (bigInt(z).isProbablePrime(i)) {
    return 1;
  } else {
    return 0;
  }
}

//external nextprime: t -> t
//Provides: ml_z_nextprime const
//Requires: bigInt, ml_z_normalize
function ml_z_nextprime(z1) {
  // Interestingly, the zarith next_prime only returns 
  // probabalistic primes.  We do the same, with the 
  // same probablistic parameter of 25.
  // https://fossies.org/dox/gmp-6.1.2/mpz_2nextprime_8c_source.html
  
  z1 = bigInt(z1)
  var one = bigInt(1);
  var two = bigInt(2);

  if (z1.lt(one) || z1.equals(one)) {
    return 2;
  }

  if (z1.and(one).equals(one)) {
    z1 = z1.add(two);
  } else {
    z1 = z1.add(one);
  }

  while (true) {
    if (z1.isProbablePrime(25)) {
      return ml_z_normalize(z1);
    } else {
      z1 = z1.add(two)
    }
  }
}

//external extract: t -> int -> int -> t
//Provides: ml_z_extract
//Requires: caml_failwith, bigInt, ml_z_normalize
function ml_z_extract(z1, pos, len) {
  z1 = bigInt(z1);
  return ml_z_normalize(z1.shiftRight(pos).and(bigInt(2).pow(len).subtract(1)));
}

//external extract_small: t -> int -> int -> t
//Provides: ml_z_extract_small
//Requires: ml_z_extract
function ml_z_extract_small(z1, pos, len) {
  return ml_z_extract(z1, pos, len);
}

//external gcdext_intern: t -> t -> (t * t * bool)
//Provides: ml_z_gcdext_intern
//Requires: bigInt, caml_raise_zero_divide, ml_z_normalize
function ml_z_gcdext_intern(z1, z2) {
  z1 = bigInt(z1);
  z2 = bigInt(z2);
  var gcd = bigInt.gcd(z1,z2);
  var a = z1;
  var b = z2;
  var x = bigInt(0);
  var lastx = bigInt(1);
  var y = bigInt(1);
  var lasty = bigInt(1);
  var q, t, r;
  if(z1.equals(bigInt(0))) {
    caml_raise_zero_divide();
  }
  while(!b.equals(bigInt(0))) {
    q = a.divide(b);
    r = a.subtract(q.multiply(b));
    t = x;
    x = lastx.subtract(q.multiply(x));
    lastx = t;
    t = y;
    y = lasty.subtract(q.multiply(y));
    lasty = t;
    a = b;
    b = r;
  }
  if(a.lt(bigInt(0)))
    return [0,ml_z_normalize(a.negate()),ml_z_normalize(lastx.negate()),1]
  else
    return [0,ml_z_normalize(a),ml_z_normalize(lastx),1]
}

//external sqrt: t -> t
//Provides: ml_z_sqrt
//Requires: bigInt, ml_z_root, caml_invalid_argument
function ml_z_sqrt(z1) {
  var z = bigInt(z1);
  var zero = bigInt(0);
  if (z.lt(zero)) {
    caml_invalid_argument("Z.sqrt: square root of a negative number");
  }
  return ml_z_root(z, 2);
}

//external sqrt_rem: t -> (t * t)
//Provides: ml_z_sqrt_rem
//Requires: bigInt, ml_z_root, caml_invalid_argument, ml_z_normalize
function ml_z_sqrt_rem(z) {
  z = bigInt(z);
  var zero = bigInt(0);
  if (z.lt(zero)) {
    caml_invalid_argument("Z.sqrt_rem: square root of a negative number");
  }
  var root = bigInt(ml_z_root(z, 2));
  var mul = root.multiply(root);
  var diff = z.subtract(mul);
  return [0, ml_z_normalize(root), ml_z_normalize(diff)]
}

//external trailing_zeros: t -> int
//Provides: ml_z_trailing_zeros const
//Requires: bigInt
function ml_z_trailing_zeros(z) {
  z = bigInt(z).abs();
  var zero = bigInt(0);
  var one = bigInt(1);
  if (z.equals(zero)) {
    // max_int in 32bit
    return 0x7fffffff;
  }
  var i = 0
  z = z.xor(z.prev()).shiftRight(1);
  for (i = 0; !z.equals(bigInt.zero); i++) {
    z = z.shiftRight(1);
  }
  return i;
}

//external popcount: t -> int
//Provides: ml_z_popcount
//Requires: bigInt, caml_raise_constant, caml_named_value
function ml_z_popcount(z) {
  z = bigInt(z);
  var zero = bigInt(0);
  var one = bigInt(1);
  if (z.lt(zero)) {
    caml_raise_constant(caml_named_value("ml_z_overflow"));
  }
  var i;
  for (i = 0; !z.equals(zero); i++) {
    z = z.and(z.prev());
  }
  if(i != (i|0)) caml_raise_constant(caml_named_value("ml_z_overflow"));
  return i|0;
}

//external hamdist: t -> t -> int
//Provides: ml_z_hamdist
//Requires: bigInt, ml_z_popcount, caml_invalid_argument, caml_raise_constant, caml_named_value
function ml_z_hamdist(z1, z2) {
  if(bigInt(z1).isNegative() != bigInt(z2).isNegative ()){
    caml_raise_constant(caml_named_value("ml_z_overflow"));
  }
  if((z1 != (z1 | 0) || z2 != (z2 | 0)) && (bigInt(z1).isNegative() || bigInt(z2).isNegative ())){
    caml_invalid_argument("Z.hamdist: negative arguments");
  }
  return ml_z_popcount(bigInt(z1).xor(bigInt(z2)));
}

//external size: t -> int
//Provides: ml_z_size const
//Requires: bigInt
function ml_z_size(z1) {
  // Claim to be a 32-bit architecture.
  return bigInt(z1).toArray(Math.pow(2, 32)).value.length;
}

//external divexact: t -> t -> t
//Provides: ml_z_divexact
//Requires: bigInt, ml_z_div
function ml_z_divexact(z1, z2) {
  return ml_z_div(z1, z2);
}


//Provides: caml_zarith_marshal
//Requires: bigInt
function caml_zarith_marshal(writer, v, sz) {
  v = bigInt(v);
  var bits = v.toArray(Math.pow(2, 32));
  writer.write(8, bits.isNegative ?1 : 0);
  var block = bits.value.length;
  var len = block * 4;
  writer.write(32, len);
  for(var i = block - 1; i >= 0; i --){
    writer.write(8, (bits.value[i] >>> 0 ) & 0xff);
    writer.write(8, (bits.value[i] >>> 8 ) & 0xff);
    writer.write(8, (bits.value[i] >>> 16) & 0xff);
    writer.write(8, (bits.value[i] >>> 24) & 0xff);
  }
  sz[0] = 4 * (1 + (((len + 3) / 4) | 0));
  sz[1] = 8 * (1 + (((len + 7) / 8) | 0));
}

//Provides: caml_zarith_unmarshal
//Requires: bigInt, caml_failwith, ml_z_normalize
function caml_zarith_unmarshal(reader, sz) {
  var negate;
  switch(reader.read8u ()) {
  case 1: negate = true; break;
  case 0: negate = false; break;
  default: caml_failwith("input_value: z (malformed input)");
  }
  var len = reader.read32u();
  var x = bigInt(0);
  for(var i = 0; i < len / 4; i++){
    var y = bigInt(reader.read8u());
    y = y.add((reader.read8u()) << 8);
    y = y.add((reader.read8u()) << 16);
    y = y.add(((reader.read8u()) << 24) >>> 0);
    x = y.shiftLeft(i * 32).add(x);
  }
  if(negate) x = x.negate();
  sz[0] = len + 4;
  return ml_z_normalize(x)
}

//Provides: ml_z_divisible
//Requires: bigInt
function ml_z_divisible(a, b){
  var zero = bigInt(0);
  a = bigInt(a);
  b = bigInt(b);
  if(a.equals(zero) && b.equals(zero)) return 1;
  return a.isDivisibleBy(b)?1:0;
}

//Provides: ml_z_congruent
//Requires: bigInt
function ml_z_congruent(a,b,c){
  var zero = bigInt(0);
  a = bigInt(a);
  b = bigInt(b);
  c = bigInt(c);
  if(c.equals(zero) && a.equals(b)) return 1;
  return a.minus(b).isDivisibleBy(c) ? 1 : 0;
}

//Provides: ml_z_remove
//Requires: bigInt, ml_z_normalize, caml_raise_zero_divide
function ml_z_remove(a,b){
  var zero = bigInt(0);
  var one = bigInt(1);
  a = bigInt(a);
  b = bigInt(b);
  if(b.equals(zero)) caml_raise_zero_divide();
  if(a.equals(zero) || b.abs().equals(one)) return [0, a, 0];
  var i = 0;
  while(a.isDivisibleBy(b)){
    a = a.divide(b);
    i++;
  }
  return [0, ml_z_normalize(a), i];
}

//Provides: ml_z_fac
//Requires: ml_z_facM, caml_invalid_argument
function ml_z_fac(i){
  if(i<=0) caml_invalid_argument("Z.fact: negative arguments");
  return ml_z_facM(i,1);
}

//Provides: ml_z_fac2
//Requires: ml_z_facM, caml_invalid_argument
function ml_z_fac2(i){
  if(i<=0) caml_invalid_argument("Z.fact2: negative arguments");
  return ml_z_facM(i,2);
}

//Provides: ml_z_facM
//Requires: caml_invalid_argument, bigInt, ml_z_normalize
function ml_z_facM(i, m){
  if(i<=0||m<=0) caml_invalid_argument("Z.factM: negative arguments");
  m = bigInt(m);
  var current = bigInt(i);
  var res = bigInt(1);
  while(current.isPositive()){
    res = res.multiply(current);
    current = current.minus(m);
  }
  return ml_z_normalize(res);
}

//Provides: ml_z_fib
//Requires: caml_invalid_argument, ml_z_normalize, bigInt
function ml_z_fib(i){
  if(i < 0) caml_invalid_argument("Z.fib: negative arguments");
  if(i == 0 || i == 1) return i;
  var a = bigInt(0), b = bigInt(1);
  for(var k = 1; k < i; k++){
    var b2 = b;
    b = a.add(b);
    a = b2;
  }
  return ml_z_normalize(b);
}

//Provides: ml_z_lucnum
//Requires: caml_invalid_argument, ml_z_normalize, bigInt
function ml_z_lucnum(i){
  if(i < 0) caml_invalid_argument("Z.lucnum: negative arguments");
  if(i == 0) return 2;
  if(i == 1) return 1;
  var a = bigInt(2), b = bigInt(1);
  for(var k = 1; k < i; k++){
    var b2 = b;
    b = a.add(b);
    a = b2;
  }
  return ml_z_normalize(b);
}

//Provides: ml_z_jacobi
//Requires: bigInt, caml_invalid_argument
function ml_z_jacobi(n, k){
  n = bigInt(n);
  k = bigInt(k);
  //assert(k > 0 and k % 2 == 1)
  if(k.leq(bigInt(0)) || k.mod(bigInt(2)).neq(bigInt(1)))
    caml_invalid_argument("Z.jacobi: second argument is negative or even");
  n = n.mod(k);
  if(n.lt(bigInt(0))) n = n.add(k);
  var t = 1;
  while (! n.equals(bigInt(0))){
    while (n.isDivisibleBy(bigInt(2))) {
      n = n.divide(bigInt(2))
      var r = k.mod(bigInt(8))
      if (r.equals(bigInt(3)) || r.equals(bigInt(5))){
        t = -t
      }
    }
    var n1 = n, k1 = k;
    n = k1;
    k = n1;
    if (n.mod(bigInt(4)).equals(bigInt(3)) &&  k.mod(bigInt(4)).equals(bigInt(3))) {
      t = -t
    }
    n = n.mod(k)
  }
  if(k.equals(bigInt(1)))
    return t
  else
    return 0
}

//Provides: ml_z_legendre
//Requires: ml_z_jacobi
function ml_z_legendre(a,b){
  return ml_z_jacobi(a,b);
}

//Provides: ml_z_kronecker
//Requires: caml_failwith
function ml_z_kronecker(n,k){
  caml_failwith("ml_z_kronecker is not implemented");
}

//Provides: ml_z_primorial
//Requires: bigInt, ml_z_normalize
function ml_z_primorial(a){
  var one = bigInt(1);
  var two = bigInt(2);
  var z1 = one;
  var res = one;
  a = bigInt(a);
  while (z1.leq(a)) {
    if (z1.isProbablePrime(25)) {
      res = res.multiply(z1);
    }
    if(z1.equals(one) || z1.equals(two)) z1 = z1.add(one);
    else z1 = z1.add(two)
  }
  return ml_z_normalize(res);
}

//Provides: ml_z_bin
//Requires: ml_z_normalize, bigInt, caml_invalid_argument
function ml_z_bin(n, k){
  var n = bigInt(n);
  var k = bigInt(k);
  var coeff = bigInt(1);
  for (var x = n.minus(k).add(bigInt(1)); x.leq(n); x = x.add(bigInt(1))) coeff = coeff.multiply(x);
  for (x = bigInt(1); x.leq(k); x = x.add(bigInt(1))) coeff = coeff.divide(x);
  return ml_z_normalize(coeff);

}
