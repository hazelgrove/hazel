type t =
  | Unfilled_slot // convex grout
  | Missing_tile // unfinished tile
  | Inconsistent_sort // prefix/postfix grout
  | Incomparability; // infix grout

// low to high severity
let all = [Unfilled_slot, Missing_tile, Inconsistent_sort, Incomparability];
let severity = o => Option.get(List.find_index((==)(o), all));

let compare = (l, r) => Int.compare(severity(l), severity(r));

module Map = {
  include Map.Make({
    type nonrec t = t;
    let compare = compare;
  });
};

module Count = {
  include Map;
  type t = Map.t(int);

  // higher to lower severity
  let bindings = count =>
    ranked
    |> List.rev_map(o => (o, Option.value(find_opt(o, count), ~default=0)));

  let compare = (l: t, r: t) =>
    List.compare(
      ((_, l), (_, r)) => Int.compare(l, r),
      bindings(l),
      bindings(r),
    );
};
