/* Source: https://crypto.stanford.edu/~blynn/haskell/count.html */
let rec partition = (~n, ~k) =>
  if (n == 0 && k == 0) {
    [[]];
  } else if (n <= 0 || k <= 0) {
    [];
  } else {
    let option1 =
      partition(~n=n - 1, ~k=k - 1) |> List.map(parts => parts @ [1]);

    let option2 = partition(~n=n - k, ~k) |> List.map(List.map((+)(1)));

    option1 @ option2;
  };

let partition_permutations = (~n, ~k) =>
  partition(~n, ~k) |> List2.concat_map(List2.permutations);

/* Source: https://stackoverflow.com/a/16950740/ */
let rec pow = a =>
  fun
  | 0 => 1

  | 1 => a

  | n => {
      let b = pow(a, n / 2);

      b
      * b
      * (
        if (n mod 2 == 0) {
          1;
        } else {
          a;
        }
      );
    };
