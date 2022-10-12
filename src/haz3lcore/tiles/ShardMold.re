[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  sort: Sort.t,
  nibs: Nibs.t,
};

let null = {
  sort: Exp,
  nibs: Nib.({sort: Exp, shape: Convex}, {sort: Exp, shape: Convex}),
};

let pre = (~prec, ~pad=true, ~sort_r=?, sort) => {
  let sort_r =
    switch (sort_r) {
    | None => sort
    | Some(s) => s
    };
  {sort, nibs: Nib.(convex(sort), concave(~prec, ~pad, sort_r))};
};
