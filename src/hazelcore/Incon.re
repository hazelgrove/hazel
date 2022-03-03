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

let rec inj_inconsistent: Constraints.t => bool =
  fun
  | ConstInj(_)
  | ArgInj(_)
  | Truth => false
  | And(xi1, xi2)
  | Or(xi1, xi2) => inj_inconsistent(xi1) || inj_inconsistent(xi2)
  | Falsity
  | Hole
  | Int(_)
  | NotInt(_)
  | Float(_)
  | NotFloat(_)
  | Pair(_) => true;

let split_inj_constraints = xis =>
  List.fold_left(
    ((const_injs, arg_injs, other_xis), xi': Constraints.t) =>
      switch (xi') {
      | ConstInj(_) => ([xi', ...const_injs], arg_injs, other_xis)
      | ArgInj(_) => (const_injs, [xi', ...arg_injs], other_xis)
      | _ => (const_injs, arg_injs, [xi', ...other_xis])
      },
    ([], [], []),
    xis,
  );

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
    | ConstInj(_, tag0) =>
      let (const_injs, arg_injs, other_xis) = split_inj_constraints(xis');
      List.exists(inj_inconsistent, other_xis)
      || (
        switch (other_xis) {
        | [] =>
          // CINCInjTag
          List.exists(
            fun
              | Constraints.ConstInj(_, tag) => !UHTag.consistent(tag, tag0)
            | _ => false,
            const_injs,
          )
          // CINCInjArg
            || arg_injs != [];
        | _ =>
          arg_injs != []
          || is_inconsistent(~may, other_xis @ [xi, ...const_injs])
        }
      );
    | ArgInj(_, tag0, xi_arg0) =>
      let (const_injs, arg_injs, other_xis) = split_inj_constraints(xis');
      List.exists(inj_inconsistent, other_xis)
      || (
        switch (other_xis) {
        | [] =>
          let xi_args =
            List.filter_map(
              fun
              | Constraints.ArgInj(_, _, xi_arg) => Some(xi_arg)
              | _ => None,
              arg_injs,
            );
          // CINCInjTag
          List.exists(
            fun
              | Constraints.ArgInj(_, tag, _) => !UHTag.consistent(tag, tag0)
            | _ => false,
            arg_injs,
          )
          // CINCInjArg
          || const_injs != []
          || is_inconsistent(~may, [xi_arg0, ...xi_args]);
        | _ =>
          const_injs != []
          || is_inconsistent(~may, other_xis @ [xi, ...arg_injs])
        }
      );
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
  }
and is_inconsistent_inj_tags = (tag0: UHTag.t, cs: list(Constraints.t)): bool =>
  switch (tag0) {
  | EmptyTagHole(_)
  | Tag(InTagHole(_), _) => false
  | Tag(NotInTagHole, t0) =>
    List.exists(
      fun
      | Constraints.ConstInj(_, Tag(NotInTagHole, t))
      | ArgInj(_, Tag(NotInTagHole, t), _) => !String.equal(t, t0)
      | _ => false,
      cs,
    )
  };

let is_redundant = (xi_cur: Constraints.t, xi_pre: Constraints.t): bool =>
  is_inconsistent(
    ~may=false,
    Constraints.[And(truify(xi_cur), dual(falsify(xi_pre)))],
  );

let is_exhaustive = (xi: Constraints.t): bool =>
  is_inconsistent(~may=true, Constraints.[dual(truify(xi))]);

let generate_redundancy_list = (xi_list: list(Constraints.t)): list(int) =>
  switch (xi_list) {
  | [] => failwith("not possible to have 0 xi")
  | [xi, ...xis] =>
    let (rev_list, _) =
      List.fold_left(
        ((flags, xi_pre), xi_cur) =>
          if (is_redundant(xi_cur, xi_pre)) {
            ([1, ...flags], Constraints.Or(xi_pre, xi_cur));
          } else {
            ([0, ...flags], Constraints.Or(xi_pre, xi_cur));
          },
        ([0], xi),
        xis,
      );
    List.rev(rev_list);
  };
