open Util;

/* Projector dependencies are currently somewhat convoluted.
 * This is the lowermost projectors module; Base depends on
 * this (specifically, it parameterizes the type t below over piece).
 *
 * ProjectorBase then depends on this and on Base.piece,
 * and also on Vdom, necessitating its inclusion in Core.
 * The individual projector implementations depend on ProjectorBase.
 * ProjectorInit then depends on the projector implementations.
 *
 * ProjectorInfo depends on ProjectorBase but not on ProjectorInit
 * (to avoid cyclical dependencies due to MakeTerm and ExpToSegment) */

/* Kind is now defined in src/language/ProjectorKind.re to allow
 * sharing with Grammar.re (which is in the language library) */
module Kind = Language.ProjectorKind;

/* Projectors in syntax */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('syntax) = {
  id: Id.t,
  kind: Kind.t,
  syntax: 'syntax,
  model: string,
};

let mk = (~id=Id.mk(), kind, syntax, model) => {
  id,
  kind,
  syntax,
  model,
};

module Shape = Util.ProjectorShape;
/* Projectors currently are all convex */
let shapes = (_: t('a)): Nibs.shapes => Nib.Shape.(Convex, Convex);

/* Serialization bypass: projectors store their Exp.t here instead of
   round-tripping through segment/term serialization. Keyed by projector
   piece ID. Used by the Automerge projectors to avoid serializing loaded
   JSON docs through editor segment text. */
let bypass_table: ref(Id.Map.t(Language.Exp.t)) = ref(Id.Map.empty);

let set_bypass = (id: Id.t, exp: Language.Exp.t): unit =>
  bypass_table := Id.Map.add(id, exp, bypass_table^);

let get_bypass = (id: Id.t): option(Language.Exp.t) =>
  Id.Map.find_opt(id, bypass_table^);

let remove_bypass = (id: Id.t): unit =>
  bypass_table := Id.Map.remove(id, bypass_table^);
