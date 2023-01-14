type t = {
  id: Id.t,
  mold: option(Mold.t),
  token: Token.t,
};

let mk = (~id=?, ~mold=?, token: Token.t) => {
  let id =
    switch (id) {
    | None => Id.Gen.next()
    | Some(id) => id
    };
  {id, mold, token};
};

let split_cursor = (_: t) => failwith("todo split_cursor");
