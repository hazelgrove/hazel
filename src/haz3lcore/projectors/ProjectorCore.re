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

/* Where a projector instance draws its primary UI. Inline means
 * in-place in the code; Sidebar means docked in the projector panel,
 * leaving a compact chip at the code site. */
module Placement = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t =
    | Inline
    | Sidebar;

  let toggle: t => t =
    fun
    | Inline => Sidebar
    | Sidebar => Inline;

  let is_sidebar: t => bool =
    fun
    | Inline => false
    | Sidebar => true;
};

/* Projectors in syntax.
 * `placement` is defaulted on deserialization so documents persisted
 * before placement existed (init slides, localStorage) still load. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('syntax) = {
  id: Id.t,
  kind: Kind.t,
  syntax: 'syntax,
  model: string,
  [@sexp.default Placement.Inline] [@yojson.default Placement.Inline]
  placement: Placement.t,
};

let mk = (~id=Id.mk(), ~placement=Placement.Inline, kind, syntax, model) => {
  id,
  kind,
  syntax,
  model,
  placement,
};

let toggle_placement = (p: t('syntax)): t('syntax) => {
  ...p,
  placement: Placement.toggle(p.placement),
};

module Shape = Util.ProjectorShape;
/* Projectors currently are all convex */
let shapes = (_: t('a)): Nibs.shapes => Nib.Shape.(Convex, Convex);
