/* From Elm 0.19 */
let rec get_by_weight: ((float, 'a), list((float, 'a)), float) => 'a = (
  ((weight, value), others, countdown) =>
    switch (others) {
    | [] => value

    | [second, ...other_others] =>
      if (countdown <= abs_float(weight)) {
        value;
      } else {
        get_by_weight(second, other_others, countdown -. abs_float(weight));
      }
    }:
    ((float, 'a), list((float, 'a)), float) => 'a
);

/* From Elm 0.19 */
let weighted: ((float, 'a), list((float, 'a))) => 'a = (
  (first, others) => {
    let normalize = ((weight, _)) => abs_float(weight);

    let total = normalize(first) +. List2.fsum(List.map(normalize, others));

    get_by_weight(first, others, Random.float(total));
  }:
    ((float, 'a), list((float, 'a))) => 'a
);

let rec sample_unique_helper:
  (list('a), list((int, unit => 'a))) => list('a) = (
  (acc, info) =>
    switch (info) {
    | [] => acc

    | [(size, gen), ...rest_info] =>
      if (List.length(acc) >= size) {
        sample_unique_helper(acc, rest_info);
      } else {
        let x = gen();

        if (Option.is_some(List.find_opt((==)(x), acc))) {
          sample_unique_helper(acc, info);
        } else {
          sample_unique_helper([x, ...acc], info);
        };
      }
    }:
    (list('a), list((int, unit => 'a))) => list('a)
);

let sample_unique: list((int, unit => 'a)) => list('a) = (
  info => sample_unique_helper([], info) |> List.rev:
    list((int, unit => 'a)) => list('a)
);
