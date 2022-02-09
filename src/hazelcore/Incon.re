open Sets;

let is_inconsistent_nums = (xis: list(Constraints.t)): bool => {
  let (int_set, not_int_list) =
    List.fold_left(
      ((int_set, not_int_list), xi: Constraints.t) =>
        switch (xi) {
        | Int(n) => (IntSet.add(n, int_set), not_int_list)
        | NotInt(n) => (int_set, [n, ...not_int_list])
        | _ => failwith("input can only be Int | NotInt")
        },
      (IntSet.empty, []),
      xis,
    );
  if (IntSet.cardinal(int_set) > 1) {
    true;
  } else {
    List.fold_left(
      (incon, n) =>
        if (incon) {
          incon;
        } else {
          IntSet.mem(n, int_set);
        },
      false,
      not_int_list,
    );
  };
};

let is_inconsistent_float = (xis: list(Constraints.t)): bool => {
  let (float_set, not_float_list) =
    List.fold_left(
      ((float_set, not_float_list), xi: Constraints.t) =>
        switch (xi) {
        | Float(n) => (FloatSet.add(n, float_set), not_float_list)
        | NotFloat(n) => (float_set, [n, ...not_float_list])
        | _ => failwith("input can only be Float | NotFloat")
        },
      (FloatSet.empty, []),
      xis,
    );
  if (FloatSet.cardinal(float_set) > 1) {
    true;
  } else {
    List.fold_left(
      (incon, n) =>
        if (incon) {
          incon;
        } else {
          FloatSet.mem(n, float_set);
        },
      false,
      not_float_list,
    );
  };
};

let rec is_inconsistent = (~may=false, xis: list(Constraints.t)): bool =>
  switch (xis) {
  | [] => false
  | [xi, ...xis'] =>
    switch (xi) {
    | Truth => is_inconsistent(~may, xis')
    | Falsity => true
    | Hole => may ? true : is_inconsistent(~may, xis')
    | And(xi1, xi2) => is_inconsistent(~may, [xi1, xi2, ...xis'])
    | Or(xi1, xi2) =>
      is_inconsistent(~may, [xi1, ...xis'])
      && is_inconsistent(~may, [xi2, ...xis'])
    | InjL(_) =>
      if (List.exists(
            fun
            | Constraints.InjR(_) => true
            | _ => false,
            xis,
          )) {
        true;
      } else {
        switch (
          List.partition(
            fun
            | Constraints.InjL(_) => true
            | _ => false,
            xis,
          )
        ) {
        | (injLs, []) =>
          let unwrap = List.map(Constraints.unwrapL, injLs);
          is_inconsistent(~may, unwrap);
        | (injLs, other) => is_inconsistent(~may, other @ injLs)
        };
      }
    | InjR(_) =>
      if (List.exists(
            fun
            | Constraints.InjL(_) => true
            | _ => false,
            xis,
          )) {
        true;
      } else {
        switch (
          List.partition(
            fun
            | Constraints.InjR(_) => true
            | _ => false,
            xis,
          )
        ) {
        | (injRs, []) =>
          let unwrap = List.map(Constraints.unwrapR, injRs);
          is_inconsistent(~may, unwrap);
        | (injRs, other) => is_inconsistent(~may, other @ injRs)
        };
      }
    | Int(_)
    | NotInt(_) =>
      switch (
        List.partition(
          fun
          | Constraints.Int(_)
          | NotInt(_) => true
          | _ => false,
          xis,
        )
      ) {
      | (ns, []) => is_inconsistent_nums(ns)
      | (ns, other) => is_inconsistent(~may, other @ ns)
      }
    | Float(_)
    | NotFloat(_) =>
      switch (
        List.partition(
          fun
          | Constraints.Float(_)
          | NotFloat(_) => true
          | _ => false,
          xis,
        )
      ) {
      | (fs, []) => is_inconsistent_float(fs)
      | (fs, other) => is_inconsistent(~may, other @ fs)
      }
    | Pair(_, _) =>
      switch (
        List.partition(
          fun
          | Constraints.Pair(_) => true
          | _ => false,
          xis,
        )
      ) {
      | (pairs, []) =>
        let (xisL, xisR) =
          List.fold_left(
            ((xisL, xisR), pair) => {
              let (xiL, xiR) = Constraints.unwrap_pair(pair);
              ([xiL, ...xisL], [xiR, ...xisR]);
            },
            ([], []),
            pairs,
          );
        is_inconsistent(~may, xisL) || is_inconsistent(~may, xisR);
      | (pairs, other) => is_inconsistent(~may, other @ pairs)
      }
    }
  };

let is_redundant = (xi_cur: Constraints.t, xi_pre: Constraints.t): bool =>
  is_inconsistent(
    ~may=false,
    Constraints.[And(truify(xi_cur), falsify(xi_pre))],
  );

let is_exhaustive = (xi: Constraints.t): bool =>
  is_inconsistent(~may=true, Constraints.[dual(truify(xi))]);
