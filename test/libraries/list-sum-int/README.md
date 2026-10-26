# list-sum-int: Int sum and product over lists

`ListSumIntLibrarySource.definitions` holds two ordinary Hazel definitions:

```
let sum_int  = fun (xs : [Int]) -> case xs | [] => 0 | x :: t => x + sum_int(t)  end in
let prod_int = fun (xs : [Int]) -> case xs | [] => 1 | x :: t => x * prod_int(t) end in
```

Both use structural recursion. Each recursive call is on the tail, so the IH of a list induction sits directly at `sum_int(t)` / `prod_int(t)`.

**No builtin, no generated converter and no trusted or stated identity is added.** Int `+`/`*` are the exact (Bigint) Int operators. Float is deliberately excluded, because it is not associative.

## Relation to `fold_left`

On closed lists, `sum_int(xs) == fold_left(xs, fun (a, x) -> a + x, 0)` and the `*`/`1` analogue evaluate to `true`. Test_ListSumIntLibrary checks this at runtime. The universal relation is **not** stated or proved here. On the current kernel the generalised-accumulator pattern of `Test_Equiv_Inductions` (`acc + len(xs) == fold_left(xs, add1, acc)`) is the template a future proof would follow, but it is not attempted.

## What is proved (existing proof steps only)

- **Defining equations:** `sum_nil`, `prod_nil`, `sum_cons` and `prod_cons`, by one `eval` plus `refl_eq`.
- **Singletons:** `sum_single` and `prod_single`, by `eval` plus the existing `rewrite` of `x + 0` / `x * 1`.
- **Closed instances:** discharged by the checker's closed evaluation (`Evaluated`).

## Open frontier (left honest)

`sum_int(xs @ ys) == sum_int(xs) + sum_int(ys)` is pinned with a proof hole as **Incomplete**. Linearity, distributivity and the product analogues need Int ring identities inside an induction. How those are established is an open trust/design choice (contract U2), so nothing is manufactured here.
