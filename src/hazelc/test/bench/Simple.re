open Hazelc_test_util.Bench;

let int5 = "5";
let%bench_fun "comp 5" = bench(int5);
let%bench_fun "eval 5" = bench_eval(int5);

let lets = "
let a : Int = 5 in
let b : Int = a + a in
let c : Int = b + a in
let d : Int = c + b + a in
let e : Int = d + c + b + a in
let f : Int = e + d + c + b + a in
let g : Int = f + e + d + c + b + a in
let h : Int = g + f + e + d + c + b + a in
let i : Int = h + g + f + e + d + c + b + a in
let j : Int = i + h + g + f + e + d + c + b + a in
j";
let%bench_fun "comp lets" = bench(lets);
let%bench_fun "eval lets" = bench_eval(lets);

let rec random_seq = (state, ()) => {
  let state' = Random.State.copy(state);
  Stdlib.Seq.Cons(Random.State.int(state', 10), random_seq(state'));
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
  take(2000, random_seq(Random.get_state()))
  |> Stdlib.Seq.fold_left(
       (acc, d) => Printf.sprintf("%s + %d", acc, d),
       "0",
     );
let%bench_fun "comp adds" = bench(adds);
let%bench_fun "eval adds" = bench_eval(adds);
