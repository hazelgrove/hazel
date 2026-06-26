open Util;

type gamma = VarMap.t_(Typ.t);

type result = {
  omitted: Id.Set.t,
  gamma,
  psi: Typ.t,
  context: Ctx.t,
  ana: Typ.t,
};

type direction = [
  | `Syn
  | `Ana
];

type exp_result = (Info.exp, Exp.t, Id.Map.t(Info.t));

exception Focus_not_found(Id.t);
exception Wrong_focus_sort;
exception Incompatible_query(Typ.t);

let gap: Typ.t = Typ.temp(Unknown(Hole(EmptyHole)));
let unknown: Typ.t = Typ.temp(Unknown(Internal));

let is_gap = (ty: Typ.t): bool =>
  switch (Typ.term_of(ty)) {
  | Unknown(Hole(EmptyHole))
  | Unknown(SynSwitch) => true
  | _ => false
  };

let empty_result = {
  omitted: Id.Set.empty,
  gamma: VarMap.empty,
  psi: gap,
  context: Ctx.empty,
  ana: gap,
};

let keep = (~parent as _: Exp.t, child: exp_result, k: exp_result => 'a): 'a =>
  k(child);

let omit = (~parent as _: Exp.t, child: exp_result, k: exp_result => 'a): 'a =>
  k(child);

let source_child =
    (~parent as _: Exp.t, child: exp_result, k: exp_result => 'a): 'a =>
  k(child);

let with_run = (f: unit => 'a): 'a => f();

let slice =
    (
      ~focus as _: option(Id.t)=None,
      ~direction as _: direction=`Syn,
      _root: exp_result,
      _query: Typ.t,
    )
    : result => empty_result;
