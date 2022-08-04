module type S = {
  let memoize:
    (
      (~memoize: bool, ~enforce_inline: bool, 'k) => 'v,
      ~memoize: bool,
      ~enforce_inline: bool,
      'k
    ) =>
    'v;
};

module DummyMemo: S = {
  let memoize = f => f;
};
