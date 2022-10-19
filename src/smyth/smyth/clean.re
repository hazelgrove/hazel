open Lang;

let propagate = (hf: hole_filling): hole_filling => {
  let rec propagate_exp = exp =>
    switch (exp) {
    /* Main case */

    | EHole(hole_name) =>
      switch (Hole_map.find_opt(hole_name, hf)) {
      | Some(hole_exp) =>
        if (Exp.syntactically_equal(exp, hole_exp)) {
          EHole(hole_name);
        } else {
          propagate_exp(hole_exp);
        }

      | None => EHole(hole_name)
      }

    /* Other cases */

    | EFix(f, x, body) => EFix(f, x, propagate_exp(body))

    | EApp(special, e1, EAExp(e2)) =>
      EApp(special, propagate_exp(e1), EAExp(propagate_exp(e2)))

    | EApp(special, e1, EAType(type_arg)) =>
      EApp(special, propagate_exp(e1), EAType(type_arg))

    | EVar(x) => EVar(x)

    | ETuple(components) => ETuple(List.map(propagate_exp, components))

    | EProj(n, i, arg) => EProj(n, i, propagate_exp(arg))

    | ECtor(ctor_name, type_args, arg) =>
      ECtor(ctor_name, type_args, propagate_exp(arg))

    | ECase(scrutinee, branches) =>
      ECase(
        propagate_exp(scrutinee),
        List.map(Pair2.map_snd(Pair2.map_snd(propagate_exp)), branches),
      )

    | EAssert(e1, e2) => EAssert(propagate_exp(e1), propagate_exp(e2))

    | ETypeAnnotation(e, tau) => ETypeAnnotation(propagate_exp(e), tau)
    };

  Hole_map.map(propagate_exp, hf);
};

let restrict =
    (delta: hole_ctx, hf: hole_filling): option(list((hole_name, exp))) =>
  delta
  |> List.map(((hole_name, _)) =>
       Option2.sequence_snd((hole_name, Hole_map.find_opt(hole_name, hf)))
     )
  |> Option2.sequence;

let clean = delta => propagate >> restrict(delta);
