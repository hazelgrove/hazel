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

module Kind = {
  /* The different kinds of projector. New projector
   * types need to be registered here in order to be
   * able to create and update their instances */
  [@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
  type t =
    | Fold
    | Probe
    | Statics
    | Checkbox
    | Slider
    | SliderF
    | Card
    | Livelit
    | TextArea
    | Csv
    | NotePicker;

  let livelit_projectors: list(t) = [
    Checkbox,
    Slider,
    SliderF,
    TextArea,
    Card,
    Livelit,
    Csv,
    NotePicker,
  ];

  /* Note: Probe intentionally excluded - probes use separate action path */
  let projectors: list(t) = livelit_projectors @ [Fold];

  /* Refractors are like probes - additive decorations, not syntax-replacing */
  let refractors: list(t) = [Probe, Statics];
  let is_refractor = (kind: t) => List.mem(kind, refractors);

  /* A friendly name for each projector. This is used
   * both for identifying a projector in the CSS and for
   * selecting projectors in the projector panel menu */
  let name = (p: t): string =>
    switch (p) {
    | Fold => "fold"
    | Probe => "probe"
    | Statics => "statics"
    | Checkbox => "check"
    | Slider => "slider"
    | SliderF => "sliderf"
    | Card => "card"
    | Livelit => "livelit"
    | TextArea => "text"
    | Csv => "csv"
    | NotePicker => "notes"
    };

  /* This must be updated and kept 1-to-1 with the above
   * name function in order to be able to select the
   * projector in the projector panel menu */
  let of_name = (p: string): t =>
    switch (p) {
    | "fold" => Fold
    | "probe" => Probe
    | "statics" => Statics
    | "check" => Checkbox
    | "slider" => Slider
    | "sliderf" => SliderF
    | "text" => TextArea
    | "livelit" => Livelit
    | "card" => Card
    | "csv" => Csv
    | "notes" => NotePicker
    | _ => failwith("Unknown projector kind")
    };

  let is_name = str => List.mem(str, List.map(name, all));
};

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
