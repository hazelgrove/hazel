let labeled_tuple_to_labels = (es: list((option('a), 'b))) => {
  es |> List.map(((p, _)) => p);
};

let labeled_tuple_to_unlabeled_tuple = (es: list((option('a), 'b))) => {
  es |> List.map(((_, e)) => e);
};
