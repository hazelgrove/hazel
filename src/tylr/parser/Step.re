include Molded.Sym;
type t = Molded.Sym.t;

module Ord = {
  type nonrec t = t;
  let compare = compare;
};
module Map = Map.Make(Ord);

// module T = {
//   type t = Molded.Label.t;
// };

// module NT = {
//   type t =
//     | Eq(Molded.Sort.t)
//     | Neq(Molded.Sort.t, option(Molded.Sort.t));
// };

// type t = Sym.t(T.t, NT.t);

// let mk = ((msym, mold): Molded.Sym.t) =>
//   switch (msym) {
//   | T(mlbl) => T((mlbl, mold))
//   | NT(msrt) => NT(Eq(msrt, mold))
//   };

