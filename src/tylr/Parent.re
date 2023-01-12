type t = (Chain.t, Chain.t);

let root = failwith("todo parent root");

let cmp_merge_l =
    (par: t, ~kid=?, c: Chain.t)
    : option(Cmp.Result.t(Chain.t, t, t)) =>
    failwith("todo");

let cmp_merge_r =
    (c: Chain.t, ~kid=?, par: t)
    : option(Cmp.Result.t(t, t, Chain.t)) =>
    failwith("todo");
