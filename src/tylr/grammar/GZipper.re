open Util;

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

let step_to_tok = (d: Dir.t, z: t(Atom.t)): list(t(Label.t)) =>
  Regex.step(d, z.zipper)
  |>

// shallow precedence-bounded entry into given sort, stepping to
// all possible atoms at the entered edge across disjunctions
let enter = (~from: Dir.t, ~l=?, ~r=?, s: Sort.t): list(t(Atom.t)) =>
  Grammar.v
  |> Sort.Map.find(s)
  |> Prec.Table.map((p, a, rgx) => {
      let has_kid = List.exists(((atom, _)) => Atom.is_kid(atom));
      let l_bounded =
        switch (l) {
        | Some((s_l, p_l)) when Sort.eq(s_l, s) => Prec.lt(~a, p_l, p)
        | _ => true
        };
      let r_bounded =
        switch (r) {
        | Some((s_r, p_r)) when Sort.eq(s, s_r) => Prec.gt(~a, p, p_r)
        | _ => true
        };

      let (ls, rs) = Regex.Zipper.(enter(~from=L, rgx), enter(~from=R, rgx));
      has_kid(ls) && !l_bounded || has_kid(rs) && !r_bounded
      ? [] : Dir.choose(from, (ls, rs))
     })
  |> List.concat;
