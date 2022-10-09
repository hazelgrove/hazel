module Mold = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    sort: Sort.t,
    nibs: Nibs.t,
  };
};

module Form = {
  type t = {
    mold: Mold.t,
    token: Token.t,
  };

  let of_term = ({mold, label}: F0rm.t): list(t) => {
    // let lbl = List.mapi((i, t) => (i, t), label);
    let (l_kid, m_kids, r_kid) = mold.kids;
    let outer_nib =
      fun
      | None => Nib.{sort: mold.sort, shape: Convex}
      | Some(kid) => Nib.{
        sort: kid.sort,
        shape: Concave({prec: mold.prec, pad: kid.pad}),
      };
    let (l, r) = (outer_nib(l_kid), outer_nib(r_kid));
    let m = i => {
      let kid = List.nth(m_kids, i);
      let sort = kid.sort;
      let shape = Nib.Shape.Concave({
          pad: kid.pad,
          prec: failwith("sort-specific min"),
        });
      Nib.{sort, shape};
    };
    label
    |> List.mapi((i, token) => {
      let l = i == 0 ? l : m(i - 1);
      let r = i == List.length(label) - 1 ? r : m(i);
      let mold = Mold.{sort: mold.sort, nibs: (l, r)};
      {mold, token};
    });
  };

  let all: list(list(Shard.t)) =
    List.map(of_term, F0rm.all);

  let
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  mold: Mold.t,
  token: Token.t,
};

let id_ = s => s.id;