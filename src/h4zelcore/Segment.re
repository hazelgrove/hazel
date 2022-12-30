type t = list(Chain.t);

// when input chain structure (specifically parent-kid relations)
// must be broken to give proper assembly
exception Nonmonotonic;

// assume push onto head for now
let rec push = (c0: Chain.t, cs: t): t =>
  switch (cs) {
  | [] => [c0]
  // | [c1] =>
  //   switch (Chain.comp(c0, c1)) {
  //   | None => []
  //   | Some(Lt) => [Chain.merge(c0, c1)]
  //   }
  | [c1, ...tl] =>
    switch (Chain.comp(c0, c1)) {
    | Some(Gt) =>
      // may need to complete right edge of c0 first
      [c0, ...cs]
    | Some(Eq) => [Chain.merge(c0, c1), ...tl]
    | Some(Lt) =>
      switch (tl) {
      | [] => [c0, ...cs]
      | [c2, ...tl] =>
        switch (Chain.comp(c0, c2)) {
        | None => failwith("breaks connected invariant")
        | Some(Gt) => push(Chain.merge(c0, c1), [c2, ...tl])
        | Some(Lt) => push(c0, [Chain.merge(c1, c2), ...tl])
        | Some(Eq) => [Chain.(merge(merge(c0, c1), c2)), ...tl]
        }
      }
    | None =>
      switch (tl) {
      | [] => [Chain.merge(c0, c1)]
      | [c2, ...tl] =>
        let c01 = Chain.merge(c0, c1);
        switch (Chain.comp(c01, c2)) {
        | None => failwith("breaks connected invariant")
        | Some(Gt) => push(c01, [c2, ...tl])
        | Some(Lt) => push(c0, [Chain.merge(c1, c2), ...tl])
        | Some(Eq) => [Chain.merge(c01, c2), ...tl]
        };
      }
    }
  };
