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

let unzip3: list(('a, 'b, 'c)) => (list('a), list('b), list('c)) = xs => {
  let rec go: ((list('a), list('b), list('c)), list(('a, 'b, 'c))) => (list('a), list('b), list('c)) = ((as, bs, cs) as res) => fun
    | [] => res
    | [(a, b, c), ...xs] => go (([a, ...as], [b, ...bs], [c, ...cs]), xs)
  go([], xs)
};
