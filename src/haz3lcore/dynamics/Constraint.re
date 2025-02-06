open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Truth
  | Falsity /* unmatchable pattern */
  | Hole
  | Int(int)
  | Float(float)
  | String(string)
  | Ap(Constructor.t, option(t))
  | Tuple(list(t));

// let of_ap = (ctx, mode, ctr: option(Constructor.t), arg: t, syn_ty): t =>
//   switch (ctr) {
//   | Some(name) =>
//     let ty =
//       switch (mode) {
//       | Mode.Ana(ty) => Some(ty)
//       | Syn => syn_ty
//       | _ => None
//       };
//     switch (ty) {
//     | Some(ty) =>
//       switch (Typ.weak_head_normalize(ctx, ty) |> Typ.term_of) {
//       | Rec(_, {term: Sum(map), _})
//       | Sum(map) =>
//         let num_variants =
//           ConstructorMap.get_constructors(map) |> List.length;
//         switch (ConstructorMap.nth(map, name)) {
//         | Some(nth) => arg |> ctr_of_nth_variant(num_variants, nth)
//         | None => Falsity
//         };
//       | _ => Falsity
//       }
//     | None => Falsity
//     };
//   | None => Falsity
//   };

// let of_ctr = (ctx, mode, name, self) => {
//   let syn_ty =
//     switch (self) {
//     | Self.IsConstructor({syn_ty, _}) => syn_ty
//     | _ => failwith("Called Constraint.of_ctr on non-constructor.")
//     };
//   of_ap(ctx, mode, Some(name), Truth, syn_ty);
// };
