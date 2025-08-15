// Tracks the branch used in a type join
[@deriving show]
type branch =
  | None
  | Left
  | Right
  | Both;

let left =
  fun
  | Left
  | Both => true
  | _ => false;
let right =
  fun
  | Right
  | Both => true
  | _ => false;

let both =
  fun
  | Both => true
  | _ => false;

let flip =
  fun
  | Left => Right
  | Right => Left
  | x => x;

let combine_branches_used = (branch_used1, branch_used2) =>
  switch (branch_used1, branch_used2) {
  | (Both, _)
  | (_, Both)
  | (Left, Right)
  | (Right, Left) => Both
  | (Left, _)
  | (_, Left) => Left
  | (Right, _)
  | (_, Right) => Right
  | (None, None) => None
  };

// Select l/r/~both/~none depending on value of branch. If ~both left empty then select right (r)
let choose_branch = (l, r, ~both=?, ~none=?) =>
  fun
  | Both => Option.value(both, ~default=r)
  | None => Option.value(none, ~default=r)
  | Right => r
  | Left => l;

// Either a successful join, returning slice parts only from the left branch where atomic types meet, i.e. Int, Int
// Or an unsuccessful join, returning the list of inconsistent atoms
type join('a, 'b) =
  | Join('a, branch)
  | NoJoin(list(('b, 'b)));
let (let.) = (x, f) =>
  switch (x) {
  | Join(t, b) => f((t, b))
  | NoJoin(ts) => NoJoin(ts)
  }; // Bind, named let. to not shadow option bind let*
let (let+) = (x, f) =>
  switch (x) {
  | Join(t, b) => f((t, b)) |> (((t, b)) => Join(t, b))
  | NoJoin(ts) => NoJoin(ts)
  }; // Map
let (and+) = (x, y) =>
  // Parallel binding: combines branches and concatenates inconsistency joins automatically
  switch (x, y) {
  | (Join(t1, b1), Join(t2, b2)) =>
    let combined_branch = combine_branches_used(b1, b2);
    Join((t1, t2), combined_branch);
  | (Join(_), NoJoin(ts))
  | (NoJoin(ts), Join(_)) => NoJoin(ts)
  | (NoJoin(ts1), NoJoin(ts2)) => NoJoin(ts1 @ ts2)
  };
let (and!) = (x, (): unit) =>
  // Terminates the parallel binds, binding the last variable to the combined branch
  switch (x) {
  | Join(t, b) => Join(t, b)
  | NoJoin(ts) => NoJoin(ts)
  };
// These let., and+, and! bindings are somewhat complex. The usage is as follows:
// 1) Sequential bindings: using let+ on it's own.
// let+ (s_join, branch_used) = join(...) in (f(s_join), g(branch_used))
//    - maps f and g to the type and branch components of successfull Joins
//    - while keeping
// When using multiple let+ only the first NoJoin is returned, and branches are accessed individually.
// 2) Parllel bindings: Branches and NoJoin lists are combined automatically (so errors from both parts are accumulated)
// let+ s_join1 = join(...) in               Maps as normal, but defers binding the branch used until it is combined by the and+
// and+ s_join2 = join(...) in               Combines branches used and NoJoins while also binding the next join
// and! combined_branches_used = () in ...   Binds the combined branch used

// Map_join allows mapping independently over successfull and unsuccessul joins
let map_join = (f, g) =>
  fun
  | Join(t, b) => f(t, b) |> (((t, b)) => Join(t, b))
  | NoJoin(ts) => NoJoin(List.map(g, ts));
