type t;

let of_relatives: Relatives.t => option(t);
let touches_generation: (t, Ancestors.generation) => bool;
let is_satisfied: t => bool;
let cover_by_generation: (t, Ancestors.generation) => t;
let crack_siblings: (~demand: t, Siblings.t) => Siblings.t;
