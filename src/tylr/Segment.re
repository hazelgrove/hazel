type t = list(Chain.t);

// when input chain structure (specifically parent-kid relations)
// must be broken to give proper assembly
exception Nonmonotonic;

// assume push onto head of chains in left-to-right order
let push = (c: Chain.t, cs: t): t => {
  let rec go = (c0: Chain.t, ~mid=?, cs: t): t =>
    switch (cs) {
    | [] => [c0, ...Option.to_list(mid)]
    | [c1, ...tl] =>
      switch (Chain.comp(c0, c1)) {
      | Some(Eq) =>
        let c =
          switch (mid) {
          | None => Chain.merge(c0, c1)
          | Some(c_mid) => Chain.(merge(c0, merge(c_mid, c1)))
          };
        [c, ...tl];
      | Some(Lt) =>
        let mid =
          switch (mid) {
          | None => c1
          | Some(c_mid) => Chain.merge(c_mid, c1)
          };
        go(c0, ~mid, tl);
      | Some(Gt) =>
        switch (mid) {
        | None =>
          // hull_r on c0?
          [c0, ...cs]
        | Some(c_mid) => [Chain.merge(c0, c_mid), ...tl]
        }
      | None =>
        assert(mid == None);

        // ignore matching molds atm

        let g = failwith("todo grout");
        [c0, ...go(g, tl)];
      }
    };
  go(c, cs);
};
