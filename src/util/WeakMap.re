/* Pointer-based memoization using JavaScript's WeakMap and Map.
 *
 * WeakMap uses reference identity (===) for key lookup, which in js_of_ocaml
 * corresponds to OCaml's physical equality (==). This gives O(1) lookup
 * and automatic GC of entries when keys are no longer referenced.
 *
 * For primitive keys (numbers, strings, booleans), which can't be WeakMap
 * keys, we fall back to a regular JS Map.
 *
 * Satisfies the MemoTbl.S interface from src/pretty/MemoTbl.re. */

module Js = Js_of_ocaml.Js;

module JS_MAP = {
  class type t ('k, 'v) = {
    pub get: 'k => Js.meth(Js.optdef('v));
    pub has: 'k => Js.meth(bool);
    pub set: ('k, 'v) => Js.meth(t('k, 'v));
  };
};

module JsMap = {
  type t('k, 'v) = Js.t(JS_MAP.t('k, 'v));
  let mk: 'k 'v. unit => t('k, 'v) =
    () => {
      let c = Js.Unsafe.global##._Map;
      [%js new c];
    };
};

module JsWeakMap = {
  type t('k, 'v) = Js.t(JS_MAP.t('k, 'v));
  let mk: 'k 'v. unit => t('k, 'v) =
    () => {
      let c = Js.Unsafe.global##._WeakMap;
      [%js new c];
    };
};

let is_primitive_representation_impl: Js.Unsafe.top => Js.t(bool) =
  Js.Unsafe.pure_js_expr(
    "function (val) {
      return (val === null) || (typeof val !== 'function') && (typeof val !== 'object');
    }",
  );

let is_primitive_representation: 'a. 'a => bool =
  x => Js.to_bool(is_primitive_representation_impl(Obj.magic(x)));

type t('k, 'v) = {
  primitive_keys: JsMap.t('k, 'v),
  non_primitive_keys: JsWeakMap.t('k, 'v),
};

let mk = (): t('k, 'v) => {
  {primitive_keys: JsMap.mk(), non_primitive_keys: JsWeakMap.mk()};
};

let get = (t: t('k, 'v), k: 'k): option('v) => {
  let map =
    if (is_primitive_representation(k)) {
      t.primitive_keys;
    } else {
      t.non_primitive_keys;
    };
  Js.Optdef.to_option(map##get(k));
};

let set = (t: t('k, 'v), k: 'k, v: 'v): unit => {
  let map =
    if (is_primitive_representation(k)) {
      t.primitive_keys;
    } else {
      t.non_primitive_keys;
    };
  ignore(map##set(k, v));
};
