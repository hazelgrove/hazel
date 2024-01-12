open Sets;

type b =
  | True
  | False(Constraint.t);

let is_inconsistent_int = (xis: list(Constraint.t)): b => {
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
  if (IntSet.cardinal(int_set) > 1
      || List.exists(IntSet.mem(_, int_set), not_int_list)) {
    True;
  } else if (IntSet.cardinal(int_set) == 1) {
    False(Int(IntSet.choose(int_set)));
  } else {
    // IntSet.cardinal(int_set) == 0
    False(
      Int(List.fold_left(max, List.hd(not_int_list), not_int_list) + 1),
    );
  };
};

let is_inconsistent_float = (xis: list(Constraint.t)): b => {
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
  if (FloatSet.cardinal(float_set) > 1
      || List.exists(FloatSet.mem(_, float_set), not_float_list)) {
    True;
  } else if (FloatSet.cardinal(float_set) == 1) {
    False(Float(FloatSet.choose(float_set)));
  } else {
    // FloatSet.cardinal(float_set) == 0
    False(
      Float(
        List.fold_left(max, List.hd(not_float_list), not_float_list) +. 1.0,
      ),
    );
  };
};

let is_inconsistent_string = (xis: list(Constraint.t)): b => {
  let (string_set, not_string_list) =
    List.fold_left(
      ((string_set, not_string_list), xi: Constraint.t) =>
        switch (xi) {
        | String(s) => (StringSet.add(s, string_set), not_string_list)
        | NotString(s) => (string_set, [s, ...not_string_list])
        | _ => failwith("input can only be String | NotString")
        },
      (StringSet.empty, []),
      xis,
    );
  if (StringSet.cardinal(string_set) > 1
      || List.exists(StringSet.mem(_, string_set), not_string_list)) {
    True;
  } else if (StringSet.cardinal(string_set) == 1) {
    False(String(StringSet.choose(string_set)));
  } else {
    // StringSet.cardinal(string_set) == 0
    False(
      String(
        List.fold_left(max, List.hd(not_string_list), not_string_list) ++ "1",
      ),
    );
  };
};

let rec is_inconsistent = (xis: list(Constraint.t)): b =>
  switch (xis) {
  | [] => False(Constraint.Truth)
  | _
      when
        List.exists(Constraint.is_injL, xis)
        && List.exists(Constraint.is_injR, xis) =>
    True
  | [xi, ...xis'] =>
    switch (xi) {
    | Truth => is_inconsistent(xis')
    | Falsity => True
    | Hole => assert(false) // Impossible
    | And(xi1, xi2) => is_inconsistent([xi1, xi2, ...xis'])
    | Or(xi1, xi2) =>
      switch (
        is_inconsistent([xi1, ...xis']),
        is_inconsistent([xi2, ...xis']),
      ) {
      | (True, True) => True
      | (False(_) as b, _)
      | (_, False(_) as b) => b
      }
    | InjL(_) =>
      switch (List.partition(Constraint.is_injL, xis)) {
      | (injLs, []) =>
        switch (injLs |> List.map(Constraint.unwrapL) |> is_inconsistent) {
        | True => True
        | False(xi) => False(InjL(xi))
        }
      | (injLs, others) => is_inconsistent(others @ injLs)
      }
    | InjR(_) =>
      switch (List.partition(Constraint.is_injR, xis)) {
      | (injRs, []) =>
        switch (injRs |> List.map(Constraint.unwrapR) |> is_inconsistent) {
        | True => True
        | False(xi) => False(InjR(xi))
        }
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
  )
  == True;

let is_exhaustive = (xi: Constraint.t): bool =>
  is_inconsistent(Constraint.[dual(truify(xi))]) == True;
