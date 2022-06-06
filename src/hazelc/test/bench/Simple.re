open Hazelc_test_util.Bench;

let bench = () => {
  let rec random_seq = (state, ()) => {
    let state' = Random.State.copy(state);
    Stdlib.Seq.Cons(Random.State.int(state', 100), random_seq(state'));
  };

  let rec take = (n, xs) => {
    module Seq = Stdlib.Seq;
    if (n == 0) {
      Stdlib.Seq.empty;
    } else {
      () =>
        switch (xs()) {
        | Seq.Nil => Nil
        | Seq.Cons(x, xs) => Cons(x, take(n - 1, xs))
        };
    };
  };

  let adds =
    take(1000, random_seq(Random.get_state()))
    |> Stdlib.Seq.fold_left(
         (acc, d) =>
           Printf.sprintf("%s + %d * %d - %d", acc, d, d + 1, d + 2),
         "0",
       );
  bench("adds", adds);
};
