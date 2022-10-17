module Mold = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    sort: Sort.t,
    nibs: Nibs.t,
  };

  let null = {
    sort: Exp,
    nibs: Nib.({sort: Exp, shape: Convex}, {sort: Exp, shape: Convex}),
  };

  let pre = (~prec, ~pad=true, ~sort_r=?, sort) => {
    let sort_r =
      switch (sort_r) {
      | None => sort
      | Some(s) => s
      };
    {sort, nibs: Nib.(convex(sort), concave(~prec, ~pad, sort_r))};
  };
};

module Form = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    mold: Mold.t,
    token: Token.t,
  };

  let map_token = (f: Token.t => Token.t, {mold, token}: t): t => {
    mold,
    token: f(token),
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  form: Form.t,
};

let id_ = s => s.id;
let form_ = s => s.form;

let nibs = s => s.form.mold.nibs;

let map_form = (f, {id, form}) => {id, form: f(form)};
