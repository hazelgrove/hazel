open Sexplib.Std;
open Util;

// todo: document potential same-id token on either side of caret
// l|et x = 1 in x + 1
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  foc: Dir.t,
  sel: Ziggurat.t,
  ctx: Stepwell.t,
};

let mk = (~foc=Dir.L, ~sel=Ziggurat.empty, ctx) => {foc, sel, ctx};

// let init =
//   mk(
//     Stepwell.of_slopes(
//       Slopes.mk(
//         ~r=
//           Slope.Up.of_meld(
//             Meld.of_grout(Grout.mk(Mold.mk_operand(Some(Sort.root)))),
//           ),
//         (),
//       ),
//     ),
//   );

let unselect = (d: Dir.t, {foc: _, sel, ctx}: Zipper.t) =>
  ctx |> Stepwell.push_zigg(~onto=Dir.toggle(d), sel) |> mk;

let zip = (~d=Dir.L, z: t): Meld.t => {
  let z = unselect(d, z);
  Stepwell.zip(z.ctx);
};
// todo: cleanup
let push_sel = (lx: Lexeme.t, foc: Dir.t, sel) =>
  switch (foc) {
  | L => Result.unwrap(Ziggurat.push_lexeme(lx, sel))
  | R => Result.unwrap(Ziggurat.hsup_lexeme(sel, lx))
  };
let pull_sel = (~char=false, foc: Dir.t, sel) =>
  switch (foc) {
  | L => Ziggurat.pull_lexeme(~char, sel)
  | R => Ziggurat.llup_lexeme(~char, sel) |> Option.map(((a, b)) => (b, a))
  };
