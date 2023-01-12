type t = Lineage.t;

let empty = Aba.singleton(Siblings.empty);

let cons = (par, ~sib=Siblings.empty, des: t) => Aba.cons(par, sib, des);

let uncons = (des: t): (Siblings.t, option(Parent.t), t) =>
  switch (Aba.uncons(des)) {
  | None => (Aba.hd(des), None, empty)
  | Some((sib, par, des)) => (sib, Some(par), des)
  };

let unsnoc: t => Either.t(Siblings.t, (Siblings.t, Parent.t)) =
  failwith("todo");

// todo: consider just returning parent
// precond: l is nonempty
let cmp_merge = (l: Chain.t, kid: t, r: Chain.t): option((Cmp.t, t)) => {
  open OptUtil.Syntax;
  let+ cmp = Chain.cmp(l, r);
  let merged =
    // todo: finish before merging
    switch (cmp) {
    | Lt => cons(Parent.mk(~r, ()), kid)
    | Gt => cons(Parent.mk(~l, ()), kid)
    | Eq => cons(Parent.mk(~l, ~r, ()), kid)
    };
  (cmp, merged);
};

let push_pre =
    (pre: Segment.t, kid: t, c: Chain.t)
    : option(
      Cmp.Result.t((Segment.t), (Segment.t, t), )
    )




let mold = (des: t, t: Token.t): option(Mold.t) => {




  let (pre, suf) = Aba.hd(des);
  switch (Segment.mold(suf, t)) {
  | Some(_) as r => r
  | None =>
    switch (Aba.uncons(des)) {
    | None =>
    }
  }
}


let remold = (des: t, c: Chain.t): (t, Segment.t) =>
  failwith("todo");
