module Js = Js_of_ocaml.Js;

module type MAP = {
  type t('k, 'v);
  type key;

  let mk: unit => t('k, 'v);
  let get: (t('k, 'v), 'k) => option('v);
  let set: (t('k, 'v), 'k, 'v) => t('k, 'v);
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

  let get: (t('k, 'v), 'k) => option('v) = {
    (m, k) => {
      Js.Optdef.to_option(m##get(k));
    };
  };

  let set: (t('k, 'v), 'k, 'v) => t('k, 'v) = {
    (m, k, v) => {
      ignore(m##set(k, v));
      m;
    };
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

  let get: (t('k, 'v), 'k) => option('v) = {
    (m, k) => {
      Js.Optdef.to_option(m##get(k));
    };
  };

  let set: (t('k, 'v), 'k, 'v) => t('k, 'v) = {
    (m, k, v) => {
      ignore(m##set(k, v));
      m;
    };
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

  let get: (t('k, 'v), 'k) => option('v) = {
    (m, k) => {
      switch (M.find(k, m)) {
      | v => Some(v)
      | exception Not_found => None
      };
    };
  };

  let set: (t('k, 'v), 'k, 'v) => t('k, 'v) = {
    (m, k, v) => {
      M.add(k, v, m);
    };
  };
};
