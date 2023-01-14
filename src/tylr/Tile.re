type t = {
  id: Id.t,
  mold: option(Mold.t),
  token: Token.t,
};

let mk = (~mold=?, token: Token.t): t => {
  let id = Id.Gen.next();
  {id, mold, token};
};
