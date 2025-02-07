// Find exp with id using ugly exception route

exception Found(Exp.t);

let find_exp_id = (id: Id.t, exp: Exp.t) =>
  switch (
    Exp.map_term(
      ~f_exp=
        (cont, exp) =>
          if (Exp.rep_id(exp) == id) {
            raise(Found(exp));
          } else {
            cont(exp);
          },
      exp,
    )
  ) {
  | exception (Found(x)) => Some(x)
  | _ => None
  };
