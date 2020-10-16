let split_first = (xs: list('a)): ('a, list('a)) =>
  switch (xs) {
  | [] => failwith("empty list")
  | [first, ...trailing] => (first, trailing)
  };

let split_last_opt = (xs: list('a)): option((list('a), 'a)) =>
  switch (List.rev(xs)) {
  | [] => None
  | [y, ...ys] => Some((List.rev(ys), y))
  };
let split_last = (xs: list('a)): (list('a), 'a) =>
  switch (split_last_opt(xs)) {
  | None => failwith("empty list")
  | Some(r) => r
  };

let rec split_first_n = (n: int, xs: list('a)): (list('a), list('a)) =>
  switch (n, xs) {
  | (0, xs) => ([], xs)
  | (_, [x, ...xs]) =>
    let (xs1, xs2) = split_first_n(n - 1, xs);
    ([x, ...xs1], xs2);
  | (_, _) => failwith("this shouldn't be hit")
  };
