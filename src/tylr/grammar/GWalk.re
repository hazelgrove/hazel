open Util;

include Walk;

module Step = {
  type t = GAtom.t;
};

type t = Walk.t(Step.t);

module Set = {
  type nonrec t = Step.Map.t(list(t));
  let empty = Step.Map.empty;
  let mk = (elems: list(Base.t)) => failwith("todo");
  let single = elem => mk([elem]);
  let union_all = _ => failwith("todo");

  let bind = (walked: t, f: Base.t => t) =>
    {
      open ListUtil.Syntax;
      let* (_, ws) = GMtrl.Map.bindings(walked);
      let* w = ws;
      f(w);
    }
    |> union_all;

  module Syntax = {
    let return = single;
    let ( let* ) = bind;
  };
};

let step = d => GZipper.map_z(RZipper.step(d));

let step_enter = (~from: Dir.t, ~l=?, ~r=?, s: Sort.t): Step.Set.t => {
  Grammar.v
  |> Sort.Map.find(s)
  |> Prec.Table.map((p, a, rgx) => {
    let entered_l =
      RZipper.enter(~from=L, rgx)
      |> List.filter(((atom, _)) =>
        Grammar.Atom.is_mold(atom) || Prec.lt(~a, l, p)
      )
      |> List.map(GAtom.mk(~sort=s, ~prec=p));
    let entered_r =
      RZipper.enter(~from=R, rgx)
      |> List.filter(((atom, _)) =>
        Grammar.Atom.is_mold(atom) || Prec.gt(~a, p, r)
      )
      |> List.map(GAtom.mk(~sort=s, ~prec=p));
    // need to check for legal entry from both sides
    List.(is_empty(entered_l) || is_empty(entered_r))
    ? [] : Dir.choose(from, entered_l, entered_r);
  })
  |> List.concat;
};

let enter = (~from: Dir.t, ~l=?, ~r=?, s: Sort.t) => {
  let seen = Hashtbl.create(100);
  let rec go = (~l=?, ~r=?, s: Sort.t) =>
    switch (Hashtbl.find_opt(seen, (l, s, r))) {
    | Some(_) => Step.Set.empty
    | None =>
      Hashtbl.add(seen, (l, s, r), ());
      open Step.Set.Syntax;
      let* gatom = step_enter(~from, ~l?, ~r?, s);
      switch (gatom) {
      | Mold(_) => return(gatom)
      | Meld(s) =>
        let (l, r) = GSort.bounds(s);
        go(~l?, ~r?, GSort.sort(s));
      };
    };
  go(~l?, ~r?, s);
};

let enter_g = (~from: Dir.t, s: GSort.t) => {
  let (l, r) = GSort.bounds(s);
  enter(~from, ~l?, ~r?, GSort.sort(s));
};