module Sort: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;

  let root: t;
  let compare: (t, t) => int;
  let lca: (t, t) => t;

  let to_string: t => string;
  // hack to do sort-specific stuff in web
  // todo: unhack
  let of_string: string => t;
};

let t: list((Sort.t, list((Gram.t(Sort.t), Assoc.t))));
