type t = {
  id: Id.t,
  mold: Mold.t,
};

let mk = (~id=?, mold) => {
  let id =
    switch (id) {
    | None => Id.Gen.next()
    | Some(id) => id
    };
  {id, mold};
};
