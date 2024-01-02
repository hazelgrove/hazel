module Step = {
  type t = Molded.t(MSym.t);

  module Ord = {
    type nonrec t = t;
    let compare = compare;
  };
  module Map = Map.Make(Ord);
};

module Base = {
  type t = Chain.t(list(Step.t), unit);
};
include Base;

module Set = {
  type nonrec t = Step.Map.t(list(t));
  let empty = Step.Map.empty;
  let mk = (elems: list(Base.t)) => failwith("todo");
  let single = elem => mk([elem]);
  let union_all = _ => failwith("todo");

  let bind = (walked: t, f: Base.t => t) =>
    {
      open ListUtil.Syntax;
      let* (_, ws) = Step.Map.bindings(walked);
      let* w = ws;
      f(w);
    }
    |> union_all;

  module Syntax = {
    let return = single;
    let ( let* ) = bind;
  };
};

let step = (d, (msym, mold): Step.t): list(Step.t) =>
  RZipper.step(d, (msym, mold.rctx))
  |> List.map(((msym, rctx)) => (msym, {...mold, rctx}));

let step_enter = (~from: Dir.t, ~l=?, ~r=?, s: Sort.t): list(Step.t) => {
  MGrammar.v
  |> Sort.Map.find(s)
  |> Prec.Table.mapi(((p, a), rgx) => {
       // need to check for legal entry from both sides
       let entered_l =
         RZipper.enter(~from=L, rgx)
         |> List.filter(((sym, _)) =>
              MSym.is_t(sym) || Prec.lt(~a, l, p)
            )
         |> List.map(((sym, rctx)) => Molded.(sym, {sort, prec, ctx}));
       let entered_r =
         RZipper.enter(~from=R, rgx)
         |> List.filter(((sym, _)) =>
              MSym.is_t(sym) || Prec.gt(~a, p, r)
            )
         |> List.map(((sym, rctx)) => Molded.(sym, {sort, prec, ctx}));
       List.(is_empty(entered_l) || is_empty(entered_r))
         ? [] : Dir.choose(from, entered_l, entered_r);
     })
  |> List.concat
};

let enter = (~from: Dir.t, ~l=?, ~r=?, sort: Sort.t) => {
  let seen = Hashtbl.create(10);
  let rec go = (s: Sort.t) =>
    switch (Hashtbl.find_opt(seen, s)) {
    | Some() => Step.Set.empty
    | None =>
      Hashtbl.add(seen, s, ());
      open ListUtil.Syntax;
      let (l, r) = Sort.eq(s, sort) ? (l, r) : (None, None);
      let* (sym, _) = step_enter(~from, ~l?, ~r?, s);
      switch (sym) {
      | T(_) => return(step)
      | NT(s) => [step, ...go(s)]
      };
    };
  go(sort);
};