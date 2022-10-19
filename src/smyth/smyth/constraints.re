open Lang;

/* Hole set functions */

module Hole_set =
  Set.Make({
    type t = hole_name;
    let compare = compare;
  });

let domain = (m: hole_map('a)): Hole_set.t =>
  Hole_set.of_list(List.map(fst) @@ Hole_map.bindings(m));

/* Hole map functions */

let delete_min = map => {
  open Option2.Syntax;
  let+ (k, v) = Hole_map.min_binding_opt(map);
  ((k, v), Hole_map.remove(k, map));
};

let empty = (Hole_map.empty, Hole_map.empty);

let from_hole_filling = hf => (hf, Hole_map.empty);

let from_unsolved_constraints = us => (Hole_map.empty, us);

let solved_singleton = (h, e) => (Hole_map.singleton(h, e), Hole_map.empty);

let unsolved_singleton = (h, w) => (
  Hole_map.empty,
  Hole_map.singleton(h, w),
);

let merge_solved = fs => {
  exception Merge_failure;
  let merge_map =
    Hole_map.union((_, e1, e2) =>
      if (Exp.syntactically_equal(e1, e2)) {
        Some(e1);
      } else {
        raise_notrace(Merge_failure);
      }
    );

  try(Some(List.fold_left(merge_map, Hole_map.empty, fs))) {
  | Merge_failure => None
  };
};

let merge_unsolved = us => {
  let merge_map = Hole_map.union((_, v1, v2) => Some(v1 @ v2));

  List.fold_left(merge_map, Hole_map.empty, us);
};

let merge = ks => {
  open Option2.Syntax;
  let (fs, us) = List.split(ks);

  let+ f = merge_solved(fs);
  let u = merge_unsolved(us);

  (f, u);
};

let satisfies = (hf, (f0, us)) =>
  Option2.Syntax.(
    Option2.with_default(false) @@
    {
      let* _ = Option2.guard @@ Hole_set.subset(domain(f0), domain(hf));
      /* When successful, hf_merged = hf because dom(f0) `subset` dom(hf) */
      let+ hf_merged = merge_solved([f0, hf]);
      Hole_map.for_all(
        (hole_name, worlds) =>
          Example.exp_satisfies(hf_merged, EHole(hole_name), worlds),
        us,
      );
    }
  );
