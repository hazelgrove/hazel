open Pervasives2;

let pure_bind = (xs, f) => List.map(f, xs);

let pure = x => [x];

let bind = (xs, f) => List.map(f, xs) |> List.concat;

let concat_map = (f, xs) => bind(xs, f);

let maximum =
  fun
  | [] => None

  | [head, ...tail] => Some(List.fold_left(max, head, tail));

let repeat = (n, x) => {
  let rec helper = (k, acc) =>
    if (k <= 0) {
      acc;
    } else {
      helper(k - 1, [x, ...acc]);
    };

  helper(n, []);
};

let sequence = mxs =>
  List.fold_right(
    (xs, acc) => bind(xs) @@ (x => pure_bind(acc) @@ (ys => [x, ...ys])),
    mxs,
    [[]],
  );

let filter_somes = xs => List.filter_map(Fun.id, xs);

let intersperse = (sep, xs) => {
  let rec helper = acc =>
    fun
    | [] => List.rev(acc)
    | [x] => List.rev([x, ...acc])
    | [head, ...tail] => helper([sep, head, ...acc], tail);

  helper([], xs);
};

let range = (~low, ~high) =>
  ListLabels.init(~len=high - low + 1, ~f=(+)(low));

let remove_first = (y, xs) => {
  let rec helper = acc =>
    fun
    | [] => List.rev(acc)

    | [head, ...tail] =>
      if (head == y) {
        List.rev_append(acc, tail);
      } else {
        helper([head, ...acc], tail);
      };

  helper([], xs);
};

let permutations = ys => {
  /* Source: https://stackoverflow.com/a/40099411 */
  let rec permutations' = xs =>
    if (xs == []) {
      [[]];
    } else {
      bind(xs) @@
      (
        x =>
          bind(permutations'(remove_first(x, xs))) @@
          (permutation => [[x, ...permutation]])
      );
    };

  List.sort_uniq(compare, permutations'(ys));
};

let map3 = (f, xs1, xs2, xs3) =>
  List.map2(((x1, x2), x3) => f(x1, x2, x3), List.combine(xs1, xs2), xs3);

let hd_opt = xs =>
  switch (xs) {
  | [] => None

  | [head, ..._] => Some(head)
  };

let tl_opt = xs =>
  switch (xs) {
  | [] => None

  | [_, ...tail] => Some(tail)
  };

let uncons = xs =>
  switch (xs) {
  | [] => None

  | [head, ...tail] => Some((head, tail))
  };

let is_empty = xs =>
  switch (xs) {
  | [] => true

  | [_, ..._] => false
  };

let rec transpose = xss =>
  if (List.for_all(is_empty, xss)) {
    [];
  } else {
    [
      List.filter_map(hd_opt, xss),
      ...transpose(List.map(tl_opt >> Option2.with_default([]), xss)),
    ];
  };

let collapse_equal = xs =>
  switch (xs) {
  | [] => None

  | [head, ...tail] =>
    if (List.for_all(x => x == head, tail)) {
      Some(head);
    } else {
      None;
    }
  };

let index_left = xs => List.mapi((i, x) => (i, x), xs);

let index_right = xs => List.mapi((i, x) => (x, i), xs);

let rec find_map = (f, xs) =>
  switch (xs) {
  | [] => None

  | [head, ...tail] =>
    switch (f(head)) {
    | Some(x) => Some(x)

    | None => find_map(f, tail)
    }
  };

let sum = xs => List.fold_left((+), 0, xs);

let fsum = xs => List.fold_left((+.), 0.0, xs);

let average = xs => {
  let len = List.length(xs);

  if (Int.equal(len, 0)) {
    None;
  } else {
    Some(fsum(xs) /. float_of_int(len));
  };
};

let take = (n, xs) => {
  let rec helper = (acc, n, xs) =>
    if (n <= 0) {
      List.rev(acc);
    } else {
      switch (xs) {
      | [] => List.rev(acc)

      | [head, ...tail] => helper([head, ...acc], n - 1, tail)
      };
    };

  helper([], n, xs);
};

let rec drop = (n, xs) =>
  if (n <= 0) {
    xs;
  } else {
    switch (xs) {
    | [] => []

    | [_, ...tail] => drop(n - 1, tail)
    };
  };

let cartesian_product = (xs, ys) =>
  concat_map(x => List.map(y => (x, y), ys), xs);

let count = (pred, xs) => {
  let rec helper = acc =>
    fun
    | [] => acc

    | [head, ...tail] =>
      helper(
        if (pred(head)) {
          acc + 1;
        } else {
          acc;
        },
        tail,
      );

  helper(0, xs);
};
