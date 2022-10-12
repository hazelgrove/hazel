[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  mold: ShardMold.t,
  token: Token.t,
};

let map_token = (f: Token.t => Token.t, {mold, token}: t): t => {
  mold,
  token: f(token),
};
