[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  mold: Mold.t,
};

let mk = (~id=?, mold: Mold.t) => {
  let id =
    switch (id) {
    | None => Id.Gen.next()
    | Some(id) => id
    };
  {id, mold};
};

let mk_convex = (~id=?, sort: Sort.t) => mk(~id?, Mold.mk(sort, Prec.max));

// inputs are sort and prec of kids-to-be
let mk_concave = (~id=?, (s_l: Sort.t, p_l: Prec.t), (s_r, p_r)) => {
  let sort = Sort.lca(s_l, s_r);
  let prec =
    if (sort == s_l) {
      p_l;
    } else if (sort == s_r) {
      p_r;
    } else {
      Prec.max_op;
    };
  mk(~id?, Mold.mk(sort, prec));
};

// todo: incorporate unique filling
let length = _ => 1;
