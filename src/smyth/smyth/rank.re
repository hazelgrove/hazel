open Lang;

let rec exp_size_rank: exp => int = (
  fun
  | EFix(_, _, body) => 1 + exp_size_rank(body)

  | EApp(_, e1, EAExp(e2)) => 1 + exp_size_rank(e1) + exp_size_rank(e2)

  | EApp(_, e1, EAType(_)) => 1 + exp_size_rank(e1)

  | EVar(_) => 1

  | ETuple(components) =>
    if (components == []) {
      0;
    } else {
      1 + List2.sum(List.map(exp_size_rank, components));
    }

  | EProj(_, _, arg) => exp_size_rank(arg)

  | ECtor(_, _, arg) => 1 + exp_size_rank(arg)

  | ECase(scrutinee, branches) =>
    1
    + exp_size_rank(scrutinee)
    + List2.sum(List.map(((_, (_, e))) => exp_size_rank(e), branches))

  | EHole(_) => 1

  | EAssert(e1, e2) => 1 + exp_size_rank(e1) + exp_size_rank(e2)

  | ETypeAnnotation(e, _) => exp_size_rank(e):
    exp => int
  /* Don't penalize units */
  /* "Focusing": projections don't add to the size rank */
  /* Do not penalize for type annotations */
);

let exp_rank: exp => int = (
  switch (Params.ranking_method^) {
  | Params.Size => exp_size_rank
  }:
    exp => int
);

let rank: list((hole_name, exp)) => int = (
  List.map(((_, e)) => exp_rank(e)) >> List2.sum:
    list((hole_name, exp)) => int
);

let sort: list(list((hole_name, exp))) => list(list((hole_name, exp))) = (
  List.sort((hf1, hf2) => Int.compare(rank(hf1), rank(hf2))):
    list(list((hole_name, exp))) => list(list((hole_name, exp)))
);

let first_recursive:
  list(list((hole_name, exp))) => option(list((hole_name, exp))) = (
  List.find_opt(List.map(snd) >> List.exists(Exp.has_special_recursion)):
    list(list((hole_name, exp))) => option(list((hole_name, exp)))
);
