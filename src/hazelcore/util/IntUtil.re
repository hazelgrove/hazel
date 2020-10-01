let min = (ns: list(int)): int =>
  ns
  |> List.fold_left(
       (min_so_far, n) =>
         switch (min_so_far) {
         | None => Some(n)
         | Some(min_so_far) => Some(min(min_so_far, n))
         },
       None,
     )
  |> OptUtil.get(() =>
       raise(Invalid_argument("IntUtil.min: expected a nonempty list"))
     );

let max = (ns: list(int)): int =>
  ns
  |> List.fold_left(
       (max_so_far, n) =>
         switch (max_so_far) {
         | None => Some(n)
         | Some(max_so_far) => Some(max(max_so_far, n))
         },
       None,
     )
  |> OptUtil.get(() =>
       raise(Invalid_argument("IntUtil.max: expected a nonempty list"))
     );
