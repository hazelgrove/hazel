/* A Merkle hash of an immutable OCaml value, by its runtime structure,
   remembered per object in a JS WeakMap: two values hash alike exactly
   when they are built the same way, ids and all (modulo two 32-bit hashes
   colliding together). Hashing a value costs a walk of the parts not seen
   before; asking again about an object already hashed is one lookup.

   What it is for: recognizing that two terms are identical without
   walking them, where they come from different places -- the eval
   worker's cached elaboration and the one in a new request, unmarshaled
   into fresh objects every time. Identical terms are equal under any of
   Equality's settings, so `same` can short-circuit a structural
   comparison; see Equality.equality's ~hash_shortcut.

   It assumes the value is not mutated after it is hashed (terms are
   immutable). A value it cannot hash soundly -- a function, a lazy value
   not yet forced, an object with no printed form, a cycle -- has no hash,
   and `same` answers false, so callers compare as before. */
open Js_of_ocaml;

let impl: Lazy.t(Js.Unsafe.any) =
  lazy(
    Js.Unsafe.js_expr(
      {js|(function () {
  const memo = new WeakMap();
  const BUSY = {}, NONE = {};
  const fnv = (s, a, b) => {
    for (let i = 0; i < s.length; i++) {
      const c = s.charCodeAt(i);
      a = Math.imul(a ^ c, 16777619);
      b = Math.imul(b + c, 2654435761) ^ (b >>> 13);
    }
    return [a >>> 0, b >>> 0];
  };
  const mix = (h, x, y) => {
    h[0] = Math.imul(h[0] ^ x, 16777619) >>> 0;
    h[1] = (Math.imul(h[1] ^ y, 2246822519) + 0x9e3779b9) >>> 0;
  };
  const hash = v => {
    switch (typeof v) {
      case "number":
        return Number.isInteger(v) && Math.abs(v) < 2147483648
          ? [(v ^ 0x5bd1e995) >>> 0, Math.imul(v, 0x27d4eb2d) >>> 0]
          : fnv("f" + v, 0x811c9dc5, 7);
      case "string": return fnv(v, 0x811c9dc5 ^ 0x51, 0x2545f491);
      case "boolean": return v ? [1, 3] : [2, 5];
      case "bigint": return fnv("n" + v, 3, 11);
      case "undefined": return [7, 13];
      case "object": {
        if (v === null) return [17, 19];
        const m = memo.get(v);
        if (m === BUSY || m === NONE) return null; /* a cycle, or no hash */
        if (m !== undefined) return m;
        let h;
        if (Array.isArray(v)) {
          /* an OCaml block: [tag, field, ...]; an unforced lazy holds a
             function, which has no hash */
          memo.set(v, BUSY);
          h = [0x9747b28c ^ v.length, 0x85ebca6b ^ v.length];
          for (let i = 0; i < v.length; i++) {
            const c = hash(v[i]);
            if (c === null) { memo.set(v, NONE); return null; }
            mix(h, c[0], c[1]);
          }
        } else {
          /* bytes, big integers and other custom values: by their printed
             form and kind, or not at all */
          if (typeof v.toString !== "function" || v.toString === Object.prototype.toString) { memo.set(v, NONE); return null; }
          const kind = v.constructor && v.constructor.name ? v.constructor.name : "?";
          h = fnv(kind + ":" + String(v), 0x165667b1, 0xc2b2ae35);
        }
        memo.set(v, h);
        return h;
      }
      default: return null; /* functions, symbols */
    }
  };
  return (a, b) => {
    try {
      const x = hash(a), y = hash(b);
      return x !== null && y !== null && x[0] === y[0] && x[1] === y[1];
    } catch (_) {
      return false; /* too deep to walk: compare as before */
    }
  };
})()|js},
    )
  );

/* Whether two values are built the same way, by their hashes. False when
   either has no hash: the caller then compares them itself. */
let same = (a: 'a, b: 'a): bool =>
  a === b
  || Js.to_bool(
       Js.Unsafe.fun_call(
         Lazy.force(impl),
         [|Js.Unsafe.inject(a), Js.Unsafe.inject(b)|],
       ),
     );

/* Answers remembered for pairs of objects, by the identity of both: for a
   comparison of immutable values that would otherwise be repeated on the
   same pair. One answer per left object (the last pair asked about),
   held weakly. Values that are not objects (numbers, strings) are not
   remembered. */
module Pairs = {
  type t = Js.Unsafe.any;
  let create = (): t =>
    Js.Unsafe.new_obj(Js.Unsafe.get(Js.Unsafe.global, "WeakMap"), [||]);
  let find_impl: Lazy.t(Js.Unsafe.any) =
    lazy(
      Js.Unsafe.js_expr(
        {js|(function (m, a, b) {
  if (typeof a !== "object" || a === null) return -1;
  const e = m.get(a);
  return e !== undefined && e[0] === b ? e[1] : -1;
})|js},
      )
    );
  let add_impl: Lazy.t(Js.Unsafe.any) =
    lazy(
      Js.Unsafe.js_expr(
        {js|(function (m, a, b, r) {
  if (typeof a === "object" && a !== null) m.set(a, [b, r]);
})|js},
      )
    );
  let find = (m: t, a: 'a, b: 'a): option(bool) =>
    switch (
      Js.Unsafe.fun_call(
        Lazy.force(find_impl),
        [|m, Js.Unsafe.inject(a), Js.Unsafe.inject(b)|],
      )
    ) {
    | (-1) => None
    | r => Some(r == 1)
    };
  let add = (m: t, a: 'a, b: 'a, r: bool): unit =>
    Js.Unsafe.fun_call(
      Lazy.force(add_impl),
      [|
        m,
        Js.Unsafe.inject(a),
        Js.Unsafe.inject(b),
        Js.Unsafe.inject(r ? 1 : 0),
      |],
    );
};
