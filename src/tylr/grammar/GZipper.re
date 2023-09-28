[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('focus) = {
  sort: Sort.t,
  prec: Prec.t,
  zipper: Regex.Zipper.t('focus),
};

let mk = (~sort, ~prec, zipper) => {sort, prec, zipper};

let map = failwith("todo");
let map_opt = failwith("todo");

// module Bound = {
//   type t = option((Sort.t, Prec.t));

//   let lt = (b: t, (p, a)) =>
//     switch (b) {
//     |
//     }
// }
