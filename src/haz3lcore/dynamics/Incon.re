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

let is_inconsistent_string = (xis: list(Constraint.t)): bool => {
  let (string_set, not_string_list) =
    List.fold_left(
      ((string_set, not_string_list), xi: Constraint.t) =>
        switch (xi) {
        | Float(n) => (StringSet.add(n, string_set), not_string_list)
        | NotFloat(n) => (string_set, [n, ...not_string_list])
        | _ => failwith("input can only be Float | NotFloat")
        },
      (StringSet.empty, []),
      xis,
    );
  if (StringSet.cardinal(string_set) > 1) {
    true;
  } else {
    List.fold_left(
      (incon, n) =>
        if (incon) {
          incon;
        } else {
          StringSet.mem(n, string_set);
        },
      false,
      not_string_list,
    );
  };
};

let rec is_inconsistent = (~may=false, xis: list(Constraint.t)): bool =>
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
            | Constraint.InjR(_) => true
            | _ => false,
            xis,
          )) {
        true;
      } else {
        switch (
          List.partition(
            fun
            | Constraint.InjL(_) => true
            | _ => false,
            xis,
          )
        ) {
        | (injLs, []) =>
          let unwrap = List.map(Constraint.unwrapL, injLs);
          is_inconsistent(~may, unwrap);
        | (injLs, other) => is_inconsistent(~may, other @ injLs)
        };
      }
    | InjR(_) =>
      if (List.exists(
            fun
            | Constraint.InjL(_) => true
            | _ => false,
            xis,
          )) {
        true;
      } else {
        switch (
          List.partition(
            fun
            | Constraint.InjR(_) => true
            | _ => false,
            xis,
          )
        ) {
        | (injRs, []) =>
          let unwrap = List.map(Constraint.unwrapR, injRs);
          is_inconsistent(~may, unwrap);
        | (injRs, other) => is_inconsistent(~may, other @ injRs)
        };
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
    /*
     * Explanation for the check of inconsistency for list:
     * List can be thought as the type (Nil(Unit) + Cons(Item, List)), or alternatively (Unit + (Item × List)).
     * Thus by analogy with InjL and InjR, an empty list is inconsistent with a non-empty one, which means that two
     * lists of different lengths are automatically inconsistent.  If all lists are having the same length, then I
     * will compare them from left to right (aka in the order of unfolding them) to see if any item in the same
     * position are inconsistent.  This version may be inefficient, but I am running out of ideas to optimize it.
     */
    | List(_) =>
      switch (
        List.partition(
          fun
          | Constraint.List(_) => true
          | _ => false,
          xis,
        )
      ) {
      | (lists, []) =>
        let lengths =
          List.map(x => List.length(Constraint.unwrap_list(x)), lists);
        // check if all lengths are equal
        // This could be done with exceptions, but I found nothing related on ReasonML website.
        let all_lengths_are_equal =
          List.for_all(x => x == List.hd(lengths), List.tl(lengths));
        if (all_lengths_are_equal) {
          let order_by_index =
            List.fold_left(
              // ordered_by_index: list(list(t)); item: packed version of list(t)
              (ordered_by_index, lst) =>
                List.map2(
                  // We need a function that maps `(list(t), t)` to list(list(t)
                  (old_list, item) => [item, ...old_list],
                  ordered_by_index,
                  Constraint.unwrap_list(lst),
                ),
              // Initial version of empty list, in list(list(t))
              List.map(x => [x], Constraint.unwrap_list(List.hd(lists))),
              // Rest of items
              List.tl(lists),
            );
          // Check if there are inconsistency in each element
          List.fold_left(
            (previous, item) => previous || is_inconsistent(~may, item),
            may,
            order_by_index,
          );
        } else {
          true; // Automatically inconsistent
        };
      | (lists, other) => is_inconsistent(~may, other @ lists)
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
