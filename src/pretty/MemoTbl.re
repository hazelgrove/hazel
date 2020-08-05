module type S = {
  type t('k, 'v);
  let mk: unit => t('k, 'v);
  let get: (t('k, 'v), 'k) => option('v);
  let set: (t('k, 'v), 'k, 'v) => unit;
};
