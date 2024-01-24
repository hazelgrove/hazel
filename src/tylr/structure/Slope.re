type t = list(Terr.t);

let empty = [];
let singleton = t => [t];
// let of_piece = p => of_terr(Terr.of_piece(p));
let height = List.length;

let fold = List.fold_left;

// Dn and Up slopes named based on left-to-right order of terraces
// as displayed on screen, but terraces are always maintained
// in list order low-to-high
module Dn = {
  type t = list(Terr.R.t);
};
module Up = {
  type t = list(Terr.L.t);
};
