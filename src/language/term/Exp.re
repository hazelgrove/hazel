include Term.Exp;

let temp: term => t =
  term => {
    term,
    annotation: {
      ids: [Id.invalid],
    },
  };

// TODO
let to_tuple = (es: list(t)): t =>
  switch (es) {
  | [] => Tuple([]) |> temp
  | [e] => e
  | _ =>
    Tuple(es |> List.map((x: t): tuple_entry => Unlabeled(x), _)) |> temp
  };
