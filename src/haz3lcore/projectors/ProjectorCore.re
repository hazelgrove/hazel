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
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
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
    | DataFrame;

  let livelit_projectors: list(t) = [
    Checkbox,
    Slider,
    SliderF,
    TextArea,
    Card,
    Livelit,
    DataFrame,
  ];

  let projectors: list(t) = livelit_projectors @ [Fold, Info, Probe];

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
    | DataFrame => "dataframe"
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
    | "dataframe" => DataFrame
    | _ => failwith("Unknown projector kind")
    };
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

module Shape = {
  /* A projector shape determines the space left for
   * that projector, and how text flows around a projector
   * in a text editor. All projectors have a horizontal
   * extend (in characters), and the vertical extent may
   * be either 1 character (Inline), or it may insert
   * an additional number of linebreaks, either immediately
   * after the projector (Block style) or defer them to
   * the next linebreak (Tab style). In the latter case,
   * if there are multiple Tab projectors on a line, the
   * total extra linebreaks inserted is the maxium required
   * to accomodate them */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type vertical =
    | Inline
    | Tab(int)
    | Block(int);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    horizontal: int,
    vertical,
  };
  let inline = (width: int): t => {
    horizontal: width,
    vertical: Inline,
  };
  let default: t = inline(0);

  let token = (shape: t): string =>
    switch (shape.vertical) {
    | Inline
    | Tab(_) => String.make(shape.horizontal, ' ')
    | Block(num_lb) =>
      String.make(num_lb, '\n') ++ String.make(shape.horizontal, ' ')
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type shape = t;

  module Map = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Id.Map.t(shape);

    let empty: t = Id.Map.empty;

    let lookup = (id: Id.t, shape_map: t): shape =>
      switch (Id.Map.find_opt(id, shape_map)) {
      | None => inline(0) //TODO: error reporting
      | Some(shape) => shape
      };
  };
};
/* Projectors currently are all convex */
let shapes = (_: t('a)): Nibs.shapes => Nib.Shape.(Convex, Convex);
