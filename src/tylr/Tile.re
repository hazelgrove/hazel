type t = {
  mold: option(Mold.t),
  token: Token.t,
};

let unmolded = token => {token, mold: None};
