include Term.Exp;

let temp: term => t =
  term => {
    term,
    annotation: {
      ids: [Id.invalid],
    },
  };

let to_tuple = (es: list(t)): t => TempGrammar.Exp.(tuple(es));
