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
/* Placement is defined in src/language/ProjectorPlacement.re, beside
 * Kind, so that Token can parse an invoke token's placement suffix. */
module Placement = Language.ProjectorPlacement;

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
  /* Whether the projector also shows its own syntax, editable, below its
     GUI (a livelit's toggle). Kept here and in the invoke token, as
     placement is, so it survives a round trip through text. Defaulted for
     documents saved before it existed. */
  [@sexp.default false] [@yojson.default false]
  show_syntax: bool,
};

let mk =
    (
      ~id=Id.mk(),
      ~placement=Placement.Inline,
      ~show_syntax=false,
      kind,
      syntax,
      model,
    ) => {
  id,
  kind,
  syntax,
  model,
  placement,
  show_syntax,
};

let toggle_show_syntax = (p: t('syntax)): t('syntax) => {
  ...p,
  show_syntax: !p.show_syntax,
};

let toggle_placement = (p: t('syntax)): t('syntax) => {
  ...p,
  placement: Placement.toggle(p.placement),
};

module Shape = Util.ProjectorShape;
/* Projectors currently are all convex */
let shapes = (_: t('a)): Nibs.shapes => Nib.Shape.(Convex, Convex);
