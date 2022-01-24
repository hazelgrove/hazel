module ElaborationResult: {
  module Syn: {type t = option((DHTyp.t, Kind.t(DHTyp.t), Delta.t));};

  module Ana: {type t = option((DHTyp.t, Delta.t));};

  include Monads.MONAD with type t('a) := option('a);
};

let syn: (Contexts.t, Delta.t, UHTyp.t) => ElaborationResult.Syn.t;

let syn_kind: (Contexts.t, UHTyp.t) => option(Kind.t(DHTyp.t));
let syn_kind_skel:
  (Contexts.t, UHTyp.skel, UHTyp.seq) => option(Kind.t(DHTyp.t));
let syn_kind_operand: (Contexts.t, UHTyp.operand) => option(Kind.t(DHTyp.t));

let ana:
  (Contexts.t, Delta.t, UHTyp.t, Kind.t(DHTyp.t)) => ElaborationResult.Ana.t;

let syn_fix_holes:
  (Contexts.t, MetaVarGen.t, UHTyp.t) =>
  (UHTyp.t, Kind.t(DHTyp.t), MetaVarGen.t);
let syn_fix_holes_z:
  (Contexts.t, MetaVarGen.t, ZTyp.t) =>
  (ZTyp.t, Kind.t(DHTyp.t), MetaVarGen.t);

let ana_fix_holes:
  (Contexts.t, MetaVarGen.t, UHTyp.t, Kind.t(DHTyp.t)) =>
  (UHTyp.t, MetaVarGen.t);
