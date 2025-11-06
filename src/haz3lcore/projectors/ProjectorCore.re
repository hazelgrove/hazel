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
    | Info
    | Probe
    | Checkbox
    | Slider
    | SliderF
    | Card
    | Livelit
    | TextArea
    | Exo(Exo.kind)
    | Graph
    | ObservablePlot;

  let livelit_projectors: list(t) =
    [Checkbox, Slider, SliderF, TextArea, Card, Livelit]
    @ List.map(x => Exo(x), Exo.all_of_kind);

  let projectors: list(t) =
    livelit_projectors @ [Fold, Info, Probe, Graph, ObservablePlot];

  /* A friendly name for each projector. This is used
   * both for identifying a projector in the CSS and for
   * selecting projectors in the projector panel menu */

  let name = (p: t): string =>
    switch (p) {
    | Fold => "fold"
    | Info => "type"
    | Probe => "probe"
    | Checkbox => "check"
    | Slider => "slider"
    | SliderF => "sliderf"
    | Card => "card"
    | Livelit => "livelit"
    | TextArea => "text"
    | Exo(exo_kind) => Exo.name(exo_kind)
    | Graph => "graph"
    | ObservablePlot => "ObservablePlot"
    };

  /* This must be updated and kept 1-to-1 with the above
   * name function in order to be able to select the
   * projector in the projector panel menu */
  let of_name = (p: string): t =>
    switch (p) {
    | "fold" => Fold
    | "type" => Info
    | "probe" => Probe
    | "check" => Checkbox
    | "slider" => Slider
    | "sliderf" => SliderF
    | "text" => TextArea
    | "livelit" => Livelit
    | "card" => Card
    | "graph" => Graph
    | "ObservablePlot" => ObservablePlot
    | _ => Exo(Exo.of_name(p))
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

let mk = (kind, syntax, model) => {
  id: Id.mk(),
  kind,
  syntax,
  model,
};

module Shape = Util.ProjectorShape;
/* Projectors currently are all convex */
let shapes = (_: t('a)): Nibs.shapes => Nib.Shape.(Convex, Convex);
