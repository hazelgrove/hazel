type t = list(Typ.t);
let union: (t, t) => t;
let empty: t;
let singleton: Typ.t => t;
let to_list: t => t;
let add: (Typ.t, t) => t;
