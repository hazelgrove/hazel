/* Represents a multimap with a total order, used in ctx and env */

type t('k, 'v, 'cmp) = {
  map: Core.Map.t('k, NEList.t('v), 'cmp),
  rev_order: list(('k, 'v)),
};

let empty = c => {
  map: Core.Map.empty(c),
  rev_order: [],
};

let singleton = ((k, v), c) => {
  map: Core.Map.singleton(c, k, NEList.singleton(v)),
  rev_order: [(k, v)],
};

let extend = (m, (k, v)) => {
  map:
    Core.Map.update(
      m.map,
      k,
      ~f=
        fun
        | None => NEList.singleton(v)
        | Some(x) => NEList.cons(v, x),
    ),
  rev_order: [(k, v), ...List.remove_assoc(k, m.rev_order)],
};

let concat = (x1, x2) => {
  map:
    Core.Map.merge(
      ~f=
        (~key as _) =>
          fun
          | `Left(v) => Some(v)
          | `Right(v) => Some(v)
          | `Both(v1, v2) => Some(NEList.append(v1, v2)),
      x1.map,
      x2.map,
    ),
  rev_order: x1.rev_order @ x2.rev_order,
};

let lookup = (type k, type v, type cmp, k: k, m: t(k, v, cmp)) =>
  switch (Core.Map.find(m.map, k)) {
  | None => None
  | Some(x) => Some(NEList.head(x))
  };

let lookup_n = (k, n, m) =>
  switch (Core.Map.find(k, m.map)) {
  | None => None
  | Some(x) => NEList.nth(n, x)
  };

let contains = (k, m) => Core.Map.mem(k, m.map);

let num_bindings = (k, m) =>
  switch (Core.Map.find(k, m.map)) {
  | None => 0
  | Some(x) => NEList.length(x)
  };

let to_assoc_list = m => m.rev_order;

let of_assoc_list = (type k, type v, rev_order: list((k, v)), c) => {
  let order = rev_order |> List.rev;
  let map =
    order
    |> List.fold_left(
         (map, (k, v)) =>
           Core.Map.update(
             map,
             k,
             ~f=
               fun
               | None => NEList.singleton(v)
               | Some(x) => NEList.cons(v, x),
           ),
         Core.Map.empty(c),
       );
  {
    map,
    rev_order,
  };
};

let mapo = (c, f, m) =>
  m
  |> to_assoc_list
  |> List.map(((k, v)) => (k, f((k, v))))
  |> of_assoc_list(_, c);

let filter = (c, f, m) =>
  m
  |> to_assoc_list
  |> List.filter(((k, v)) => f((k, v)))
  |> of_assoc_list(_, c);

let filter_map = (c, f, m) =>
  m
  |> to_assoc_list
  |> List.filter_map(((k, v)) => f((k, v)))
  |> of_assoc_list(_, c);

let filter_find_map =
    (type k, type v, type w, type c, f: v => option(w), k: k, m: t(k, v, c))
    : list(w) =>
  Core.Map.find(m.map, k) |> NEList.list_of_option_t |> List.filter_map(f);

let map2 =
    (
      type k,
      type v,
      type w,
      type x,
      type cmp,
      c: Core.Comparator.Module.t(k, cmp),
      f: (k, option(v), option(w)) => x,
      m1: t(k, v, cmp),
      m2: t(k, w, cmp),
    )
    : t(k, x, cmp) => {
  let remove_one = (key: k, m) => {
    Core.Map.change(m, key, ~f=x
      =>
        x
        |> NEList.list_of_option_t
        |> ListUtil.tl_opt
        |> Option.map(NEList.option_t_of_list)
        |> Option.join
      ); /*     */
  };
  let lookup = (k: k, m) =>
    switch (Core.Map.find(m, k)) {
    | None => None
    | Some(x) => Some(NEList.head(x))
    };
  // Add all the items in m1 first
  m1
  |> to_assoc_list
  |> List.fold_left(
       ((m2', xs), (k, v)) =>
         switch (lookup(k, m2')) {
         | None => (m2', [(k, f(k, Some(v), None)), ...xs])
         | Some(x) => (
             remove_one(k, m2'),
             [(k, f(k, Some(v), Some(x))), ...xs],
           )
         },
       (m2.map, []),
     )
  |> snd
  |> List.rev
  |> of_assoc_list(_, c);
  // Then add all the items in m2
};
