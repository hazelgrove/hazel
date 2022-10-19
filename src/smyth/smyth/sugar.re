open Lang;

let rec nat: exp => option(int) = (
  fun
  | ECtor("S", [], arg) => Option.map((+)(1), nat(arg))

  | ECtor("Z", [], ETuple([])) => Some(0)

  | _ => None:
    exp => option(int)
);

let listt: exp => option((list(exp), list(typ))) = (
  {
    let rec helper = expected_opt =>
      fun
      | ECtor("Cons", type_args, ETuple([head, tail])) => {
          let good = () =>
            Option.map(
              ((es, taus)) => ([head, ...es], taus),
              helper(expected_opt, tail),
            );

          switch (expected_opt) {
          | Some(expected) =>
            if (Type.equal(TTuple(expected), TTuple(type_args))) {
              good();
            } else {
              None;
            }

          | None => good()
          };
        }

      | ECtor("Nil", type_args, ETuple([])) => {
          let good = () => Some(([], type_args));

          switch (expected_opt) {
          | Some(expected) =>
            if (Type.equal(TTuple(expected), TTuple(type_args))) {
              good();
            } else {
              None;
            }

          | None => good()
          };
        }

      | _ => None;

    helper(None);
  }:
    exp => option((list(exp), list(typ)))
);
