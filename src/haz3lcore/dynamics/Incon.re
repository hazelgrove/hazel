open Sets;

let is_inconsistent_int = (xis: list(Constraint.t)): bool => {
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
  IntSet.cardinal(int_set) > 1
  || List.exists(IntSet.mem(_, int_set), not_int_list);
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
  FloatSet.cardinal(float_set) > 1
  || List.exists(FloatSet.mem(_, float_set), not_float_list);
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
  StringSet.cardinal(string_set) > 1
  || List.exists(StringSet.mem(_, string_set), not_string_list);
};

let rec is_inconsistent = (xis: list(Constraint.t)): bool =>
  switch (xis) {
  | [] => false
  | _
      when
        List.exists(Constraint.is_inl, xis)
        && List.exists(Constraint.is_inr, xis) =>
    true
  | [xi, ...xis'] =>
    switch (xi) {
    | Truth => is_inconsistent(xis')
    | Falsity => true
    | Hole => assert(false) // Impossible
    | And(xi1, xi2) => is_inconsistent([xi1, xi2, ...xis'])
    | Or(xi1, xi2) =>
      is_inconsistent([xi1, ...xis']) && is_inconsistent([xi2, ...xis'])
    | InjL(_) =>
      switch (List.partition(Constraint.is_inl, xis)) {
      | (injLs, []) =>
        injLs |> List.map(Constraint.unwrapL) |> is_inconsistent
      | (injLs, others) => is_inconsistent(others @ injLs)
      }
    | InjR(_) =>
      switch (List.partition(Constraint.is_inr, xis)) {
      | (injRs, []) =>
        injRs |> List.map(Constraint.unwrapR) |> is_inconsistent
      | (injRs, others) => is_inconsistent(others @ injRs)
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
      | (ns, []) => is_inconsistent_int(ns)
      | (ns, others) => is_inconsistent(others @ ns)
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
      | (fs, others) => is_inconsistent(others @ fs)
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
      | (ss, []) => is_inconsistent_string(ss)
      | (ss, others) => is_inconsistent(others @ ss)
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
          pairs |> List.map(Constraint.unwrap_pair) |> List.split;
        is_inconsistent(xisL) || is_inconsistent(xisR);
      | (pairs, others) => is_inconsistent(others @ pairs)
      }
    }
  };

let is_redundant = (xi_cur: Constraint.t, xi_pre: Constraint.t): bool =>
  is_inconsistent(
    Constraint.[And(truify(xi_cur), dual(falsify(xi_pre)))],
  );

let is_exhaustive = (xi: Constraint.t): bool =>
  is_inconsistent(Constraint.[dual(truify(xi))]);
