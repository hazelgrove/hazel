module Js = Js_of_ocaml.Js;

module type MAP = {
  type t('k, 'v);
  type key;

  let mk: unit => t('k, 'v);
  let get: 'k => option('v);
  let set: ('k, 'v) => t('k, 'v);
};

module JS_MAP = {
  class type t ('k, 'v) = {
    pub get: 'k => Js.meth(Js.optdef('v));
    pub has: 'k => Js.meth(bool);
    pub set: ('k, 'v) => Js.meth(t('k, 'v));
  };
};

module type AnyType = {type t;};

module JsMap = (T: AnyType) => {
  type t('k, 'v) = Js.t(JS_MAP.t('k, 'v));
  type key = T.t;

  let mk: unit => t('k, 'v) =
    () => {
      let c = Js.Unsafe.global##._Map;
      %js
      new c;
    };
};

module JsWeakMap = (T: AnyType) => {
  type t('k, 'v) = Js.t(JS_MAP.t('k, 'v));
  type key = T.t;

  let mk: 'k 'v. unit => t('k, 'v) =
    () => {
      let c = Js.Unsafe.global##._WeakMap;
      %js
      new c;
    };
};

module OcamlMap = (T: Map.OrderedType) => {
  module M = Map.Make(T);
  type t('k, 'v) = M.t('v);
  type key = T.t;

  let mk: unit => t('k, 'v) =
    () => {
      M.empty;
    };

  let get: 'k => option('v) = {
    k => {
      switch (M.find(k)) {
      | v => Some(v)
      | exception Not_found => None
      };
    };
  };

  let set: ('k, 'v) => t('k, 'v) = {
    (k, v) => {
      M.add(k, v, M.empty);
    };
  };
};

/*

 let is_primitive_representation_impl: Js.Unsafe.top => Js.t(bool) =
   // Based on https://stackoverflow.com/questions/8511281/check-if-a-value-is-an-object-in-javascript
   Js.Unsafe.pure_js_expr(
     "
   function (val) {
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
   */
