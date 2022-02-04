module ElaborationResult: {
  module Syn: {
    type t = option((HTyp.t, Kind.t, Delta.t, Contexts.t, MetaVarGen.t));
  };

  module Ana: {type t = option((HTyp.t, Delta.t, Contexts.t, MetaVarGen.t));
  };

  include Monads.MONAD with type t('a) := option('a);
};

let syn:
  (Contexts.t, MetaVarGen.t, Delta.t, UHTyp.t) => ElaborationResult.Syn.t;

let syn_kind: (Contexts.t, MetaVarGen.t, UHTyp.t) => option(Kind.t);
let syn_kind_skel:
  (Contexts.t, MetaVarGen.t, UHTyp.skel, UHTyp.seq) => option(Kind.t);
let syn_kind_operand:
  (Contexts.t, MetaVarGen.t, UHTyp.operand) => option(Kind.t);

let ana:
  (Contexts.t, MetaVarGen.t, Delta.t, UHTyp.t, Kind.t) =>
  ElaborationResult.Ana.t;

let syn_fix_holes:
  (Contexts.t, MetaVarGen.t, UHTyp.t) => (UHTyp.t, Kind.t, MetaVarGen.t);
let syn_fix_holes_z:
  (Contexts.t, MetaVarGen.t, ZTyp.t) => (ZTyp.t, Kind.t, MetaVarGen.t);

let ana_fix_holes:
  (Contexts.t, MetaVarGen.t, UHTyp.t, Kind.t) => (UHTyp.t, MetaVarGen.t);
