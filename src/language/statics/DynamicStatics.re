module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(Typ.t);

  let empty = Id.Map.empty;
  let lookup = Id.Map.find_opt;
};
