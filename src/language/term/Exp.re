include Term.Exp;

let temp: term => t =
  term => {
    term,
    annotation: {
      ids: [Id.invalid],
    },
  };

let to_tuple = (es: list(t)): t =>
  switch (es) {
  | []
  | [{term: TupLabel(_), _}] => Tuple(es) |> temp
  | [e] => e
  | _ => Tuple(es) |> temp
  };
