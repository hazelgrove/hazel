type t;

let of_relatives: Relatives.t => option(t);
let touches_ancestor: (t, Ancestor.t) => bool;
let is_satisfied: t => bool;
let cover_by_label: (t, Label.t) => t;
let crack_siblings: (~demand: t, Siblings.t) => Siblings.t;
