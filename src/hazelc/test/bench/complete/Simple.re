open Hazelc_test_util.Bench;
module Seq = Stdlib.Seq;

let rec seq_nat = (i, ()) => {
  Seq.Cons(i, seq_nat(i + 1));
};

let rec seq_take = (n, xs) =>
  if (n == 0) {
    Seq.empty;
  } else {
    () =>
      switch (xs()) {
      | Seq.Nil => Nil
      | Seq.Cons(x, xs) => Cons(x, seq_take(n - 1, xs))
      };
  };

let bench = () => {
  let add2000 =
    seq_take(2000, seq_nat(1))
    |> Seq.fold_left((acc, i) => acc ++ " + " ++ string_of_int(i), "0");
  bench("add2000", 1000L, add2000, ~grain_source=Some(add2000));

  ();
};
