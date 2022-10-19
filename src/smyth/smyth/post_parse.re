open Lang;

let salvage_constructor: exp => option((string, list(typ))) = (
  {
    let rec helper = (type_args, exp) =>
      switch (exp) {
      | EVar(name) =>
        if (Char2.uppercase_char(name.[0])) {
          Some((name, type_args));
        } else {
          None;
        }

      | EApp(_, head, EAType(type_arg)) =>
        helper([type_arg, ...type_args], head)

      | _ => None
      };

    helper([]);
  }:
    exp => option((string, list(typ)))
);

let exp: exp => exp = (
  root => {
    Fresh.set_largest_hole(Exp.largest_hole(root));
    let rec helper = exp =>
      switch (exp) {
      | EApp(special, e1, EAExp(e2)) =>
        switch (salvage_constructor(e1)) {
        | Some((ctor_name, type_args)) =>
          ECtor(ctor_name, type_args, helper(e2))

        | None => EApp(special, helper(e1), EAExp(helper(e2)))
        }

      | EVar(name) =>
        if (Char2.uppercase_char(name.[0])) {
          ECtor(name, [], ETuple([]));
        } else {
          EVar(name);
        }

      | EApp(special, head, EAType(type_arg)) =>
        switch (salvage_constructor(exp)) {
        | Some((ctor_name, type_args)) =>
          ECtor(ctor_name, type_args, ETuple([]))

        | None => EApp(special, helper(head), EAType(type_arg))
        }

      | EHole(hole_name) =>
        if (Int.equal(hole_name, Fresh.unused)) {
          EHole(Fresh.gen_hole());
        } else {
          EHole(hole_name);
        }

      | EFix(f, x, body) => EFix(f, x, helper(body))

      | ETuple(components) => ETuple(List.map(helper, components))

      | EProj(n, i, arg) => EProj(n, i, helper(arg))

      | ECtor(ctor_name, type_args, arg) =>
        ECtor(ctor_name, type_args, helper(arg))

      | ECase(scrutinee, branches) =>
        ECase(
          helper(scrutinee),
          List.map(Pair2.map_snd(Pair2.map_snd(helper)), branches),
        )

      | EAssert(e1, e2) => EAssert(helper(e1), helper(e2))

      | ETypeAnnotation(e, tau) => ETypeAnnotation(helper(e), tau)
      };

    helper(root);
  }:
    exp => exp
  /* Main cases */
  /* Handle constructor applications */
  /* Handle syntactic sugar for unapplied constructors */
  /* Set proper hole names */
  /* Other cases */
);
