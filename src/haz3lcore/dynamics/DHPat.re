include Pat;

/* A Dynamic Pattern (DHPat) is a pattern that is part of an expression
   that has been type-checked. Hence why these functions take both a
   pattern, dp, and an info map, m, with type information. */

let rec get_label: t => option((LabeledTuple.label, t)) =
  dp =>
    switch (dp |> term_of) {
    | Parens(dp) => get_label(dp)
    | TupLabel({term: Label(name), _}, t') => Some((name, t'))
    | _ => None
    };
