type memoization_value('a) = {
  mutable inline_true: option('a),
  mutable inline_false: option('a),
};

let memoize =
    (f: (~memoize: bool, ~enforce_inline: bool, 'k) => 'v)
    : ((~memoize: bool, ~enforce_inline: bool, 'k) => 'v) => {
  let table: WeakMap.t('k, memoization_value('v)) = WeakMap.mk();
  (~memoize: bool, ~enforce_inline: bool, k: 'k) => (
    if (!memoize) {
      f(~memoize, ~enforce_inline, k);
    } else {
      switch (WeakMap.get(table, k)) {
      | None =>
        let v = f(~memoize, ~enforce_inline, k);
        let m =
          if (enforce_inline) {
            {inline_true: Some(v), inline_false: None};
          } else {
            {inline_false: Some(v), inline_true: None};
          };
        let _ = WeakMap.set(table, k, m);
        v;
      | Some(m: memoization_value('v)) =>
        if (enforce_inline) {
          switch (m.inline_true) {
          | Some(v) => v
          | None =>
            let v = f(~memoize, ~enforce_inline, k);
            m.inline_true = Some(v);
            v;
          };
        } else {
          switch (m.inline_false) {
          | Some(v) => v
          | None =>
            let v = f(~memoize, ~enforce_inline, k);
            m.inline_false = Some(v);
            v;
          };
        }
      };
    }: 'v
  );
};
