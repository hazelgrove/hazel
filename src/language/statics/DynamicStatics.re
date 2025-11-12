open Util;

module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry = {
    exps: list(Exp.t),
    ty_envs: list(Environment.t(Typ.t)),
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(entry); // Probably put these in one list

  let empty = Id.Map.empty;
  let lookup = Id.Map.find_opt;
};
