let memoize = (f: 'k => 'v): ('k => 'v) => {
  let table: WeakMap.t('k, 'v) = WeakMap.mk();
  k =>
    switch (WeakMap.get(table, k)) {
    | None =>
      let v = f(k);
      let _ = WeakMap.set(table, k, v);
      v;
    | Some(v) => v
    };
};
