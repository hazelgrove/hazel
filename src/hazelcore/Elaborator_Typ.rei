module ElaborationResult: {
  type t = option((HTyp.t, Kind.t, Delta.t));

  include Monads.MONAD with type t('a) := option('a);
};

let syn: (Contexts.t, Delta.t, UHTyp.t) => ElaborationResult.t;

let syn_kind: (Contexts.t, UHTyp.t) => option(Kind.t);

let ana: (Contexts.t, Delta.t, UHTyp.t, Kind.t) => ElaborationResult.t;

let ana_kind: (Contexts.t, UHTyp.t, Kind.t) => option(Kind.t);
