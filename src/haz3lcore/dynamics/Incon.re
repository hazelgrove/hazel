open Sets;

let is_inconsistent_nums = (xis: list(Constraint.t)): bool => {
  let (int_set, not_int_list) =
    List.fold_left(
      ((int_set, not_int_list), xi: Constraint.t) =>
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
      (incon, n) => incon || IntSet.mem(n, int_set),
      false,
      not_int_list,
    );
  };
};

let is_inconsistent_float = (xis: list(Constraint.t)): bool => {
  let (float_set, not_float_list) =
    List.fold_left(
      ((float_set, not_float_list), xi: Constraint.t) =>
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
      (incon, n) => incon || FloatSet.mem(n, float_set),
      false,
      not_float_list,
    );
  };
};

let is_inconsistent_string = (xis: list(Constraint.t)): bool => {
  let (string_set, not_string_list) =
    List.fold_left(
      ((string_set, not_string_list), xi: Constraint.t) =>
        switch (xi) {
        | String(n) => (StringSet.add(n, string_set), not_string_list)
        | NotString(n) => (string_set, [n, ...not_string_list])
        | _ => failwith("input can only be String | NotString")
        },
      (StringSet.empty, []),
      xis,
    );
  if (StringSet.cardinal(string_set) > 1) {
    true;
  } else {
    List.fold_left(
      (incon, n) => incon || StringSet.mem(n, string_set),
      false,
      not_string_list,
    );
  };
};

let rec is_inconsistent = (~may=false, xis: list(Constraint.t)): bool =>
  switch (xis) {
  | [] => false
  | _
      when
        List.exists(Constraint.is_inl, xis)
        && List.exists(Constraint.is_inr, xis) =>
    true
  | [xi, ...xis'] =>
    switch (xi) {
    | Truth => is_inconsistent(~may, xis')
    | Falsity => true
    | Hole => assert(false) // Impossible
    | And(xi1, xi2) => is_inconsistent(~may, [xi1, xi2, ...xis'])
    | Or(xi1, xi2) =>
      is_inconsistent(~may, [xi1, ...xis'])
      && is_inconsistent(~may, [xi2, ...xis'])
    | InjL(_) =>
      switch (List.partition(Constraint.is_inl, xis)) {
      | (injLs, []) =>
        let unwrap = List.map(Constraint.unwrapL, injLs);
        is_inconsistent(~may, unwrap);
      | (injLs, other) => is_inconsistent(~may, other @ injLs)
      }
    | InjR(_) =>
      switch (List.partition(Constraint.is_inr, xis)) {
      | (injRs, []) =>
        let unwrap = List.map(Constraint.unwrapR, injRs);
        is_inconsistent(~may, unwrap);
      | (injRs, other) => is_inconsistent(~may, other @ injRs)
      }
    | Int(_)
    | NotInt(_) =>
      switch (
        List.partition(
          fun
          | Constraint.Int(_)
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
          | Constraint.Float(_)
          | NotFloat(_) => true
          | _ => false,
          xis,
        )
      ) {
      | (fs, []) => is_inconsistent_float(fs)
      | (fs, other) => is_inconsistent(~may, other @ fs)
      }
    | Bool(_)
    | NotBool(_) => assert(false) // Unused
    | String(_)
    | NotString(_) =>
      switch (
        List.partition(
          fun
          | Constraint.String(_)
          | NotString(_) => true
          | _ => false,
          xis,
        )
      ) {
      | (fs, []) => is_inconsistent_string(fs)
      | (fs, other) => is_inconsistent(~may, other @ fs)
      }
    | Pair(_, _) =>
      switch (
        List.partition(
          fun
          | Constraint.Pair(_) => true
          | _ => false,
          xis,
        )
      ) {
      | (pairs, []) =>
        let (xisL, xisR) =
          List.fold_left(
            ((xisL, xisR), pair) => {
              let (xiL, xiR) = Constraint.unwrap_pair(pair);
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

let is_redundant = (xi_cur: Constraint.t, xi_pre: Constraint.t): bool =>
  is_inconsistent(
    ~may=false,
    Constraint.[And(truify(xi_cur), dual(falsify(xi_pre)))],
  );

let is_exhaustive = (xi: Constraint.t): bool =>
  is_inconsistent(~may=true, Constraint.[dual(truify(xi))]);
