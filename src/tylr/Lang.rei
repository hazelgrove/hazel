module Sort: {
  type t;
  let compare: (t, t) => int;
  let root: t;
  let to_string: t => string;
  // hack to do sort-specific stuff in web
  // todo: unhack
  let of_string: string => t;
};

let t: list((Sort.t, list((Gram.t(Sort.t), Assoc.t))));
