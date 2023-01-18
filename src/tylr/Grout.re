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
let mk_concave = (~id=?, sort, prec) => {
  // todo: somehow need to fix this to enable any greater-prec sort as kid
  // probably need grout-specific mold datatype
  mk(
    ~id?,
    Mold.mk_infix(~l=sort, ~r=sort, sort, prec),
  );
};

// todo: incorporate unique filling
let length = _ => 1;
