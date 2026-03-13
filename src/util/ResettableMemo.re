/* Resettable memoization wrapper around Core.Memo.general.
 *
 * Drop-in replacement that tracks all memoized functions and allows
 * clearing all caches at once via clear_all(). Used by benchmarks
 * to control cold vs warm cache state.
 *
 * Also supports registering arbitrary reset callbacks (e.g., for
 * WeakMap-based caches via register_resetter). */

let resetters: ref(list(unit => unit)) = ref([]);

/* Create a memoized function with a clearable cache.
 * API matches Core.Memo.general. */
let general = (~cache_size_bound=?, f) => {
  let make = () =>
    switch (cache_size_bound) {
    | Some(n) => Core.Memo.general(~cache_size_bound=n, f)
    | None => Core.Memo.general(f)
    };
  let current = ref(make());
  resetters :=
    [
      () => {
        current := make();
      },
      ...resetters^,
    ];
  x => current^(x);
};

/* Register an additional reset callback (e.g., for WeakMap caches). */
let register_resetter = (f: unit => unit): unit => {
  resetters := [f, ...resetters^];
};

/* Clear all memo caches and run all registered resetters. */
let clear_all = (): unit => List.iter(f => f(), resetters^);
