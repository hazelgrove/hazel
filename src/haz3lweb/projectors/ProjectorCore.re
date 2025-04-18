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
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Fold
    | Info
    | Probe
    | Checkbox
    | Slider
    | SliderF
    | Card
    | TextArea;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_model =
    | Expected
    | Self;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type card_mode =
    | Show
    | Choose(int)
    | Flipped;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type fold_model = {
    [@default "⋱"]
    text: string,
  };

  type gadt('a) =
    | Fold: gadt(fold_model)
    | Info: gadt(type_model)
    | Probe: gadt(unit)
    | Checkbox: gadt(unit)
    | Slider: gadt(unit)
    | SliderF: gadt(unit)
    | Card: gadt(card_mode)
    | TextArea: gadt(unit);

  let of_gadt = (type a, kind: gadt(a)): t =>
    switch (kind) {
    | Fold => Fold
    | Info => Info
    | Probe => Probe
    | Checkbox => Checkbox
    | Slider => Slider
    | SliderF => SliderF
    | Card => Card
    | TextArea => TextArea
    };

  type w =
    | W(gadt('a)): w;

  let (let.gadt) = (type b, kind: t, f: w => b) =>
    switch (kind) {
    | Fold => f(W(Fold))
    | Info => f(W(Info))
    | Probe => f(W(Probe))
    | Checkbox => f(W(Checkbox))
    | Slider => f(W(Slider))
    | SliderF => f(W(SliderF))
    | Card => f(W(Card))
    | TextArea => f(W(TextArea))
    };

  let livelit_projectors: list(t) = [
    Checkbox,
    Slider,
    SliderF,
    TextArea,
    Card,
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
    | TextArea => "text"
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
    | "card" => Card
    | _ => failwith("Unknown projector kind")
    };
};

type model =
  | V(Kind.gadt('a), 'a): model;

let pp_model = (f, model) =>
  Format.fprintf(
    f,
    switch (model) {
    | V(Fold, _) => "Fold"
    | V(Info, _) => "Info"
    | V(Probe, _) => "Probe"
    | V(Checkbox, _) => "Checkbox"
    | V(Slider, _) => "Slider"
    | V(SliderF, _) => "SliderF"
    | V(Card, _) => "Card"
    | V(TextArea, _) => "TextArea"
    },
  );

let model_of_sexp = (sexp: Sexplib.Sexp.t): model =>
  // take s-expressions of the form ("fold", m), deserialize m, and turn it into V(Fold, m)
  switch (sexp) {
  | List([Atom("fold"), m]) => V(Fold, m |> Kind.fold_model_of_sexp)
  | List([Atom("info"), m]) => V(Info, m |> Kind.type_model_of_sexp)
  | List([Atom("probe"), _]) => V(Probe, ())
  | List([Atom("checkbox"), _]) => V(Checkbox, ())
  | List([Atom("slider"), _]) => V(Slider, ())
  | List([Atom("sliderf"), _]) => V(SliderF, ())
  | List([Atom("card"), m]) => V(Card, m |> Kind.card_mode_of_sexp)
  | List([Atom("text"), _]) => V(TextArea, ())
  | _ => failwith("Unknown projector kind")
  };

let sexp_of_model = (model: model): Sexplib.Sexp.t =>
  switch (model) {
  | V(Fold, m) => List([Atom("fold"), m |> Kind.sexp_of_fold_model])
  | V(Info, m) => List([Atom("info"), m |> Kind.sexp_of_type_model])
  | V(Probe, _) => List([Atom("probe"), () |> sexp_of_unit])
  | V(Checkbox, _) => List([Atom("checkbox"), () |> sexp_of_unit])
  | V(Slider, _) => List([Atom("slider"), () |> sexp_of_unit])
  | V(SliderF, _) => List([Atom("sliderf"), () |> sexp_of_unit])
  | V(Card, m) => List([Atom("card"), m |> Kind.sexp_of_card_mode])
  | V(TextArea, _) => List([Atom("text"), () |> sexp_of_unit])
  };

let model_of_yojson = (yojson: Yojson.Safe.t): model =>
  switch (yojson) {
  | `List([`String("fold"), m]) => V(Fold, m |> Kind.fold_model_of_yojson)
  | `List([`String("info"), m]) => V(Info, m |> Kind.type_model_of_yojson)
  | `List([`String("probe"), _]) => V(Probe, ())
  | `List([`String("checkbox"), _]) => V(Checkbox, ())
  | `List([`String("slider"), _]) => V(Slider, ())
  | `List([`String("sliderf"), _]) => V(SliderF, ())
  | `List([`String("card"), m]) => V(Card, m |> Kind.card_mode_of_yojson)
  | `List([`String("text"), _]) => V(TextArea, ())
  | _ => failwith("Unknown projector kind")
  };

let yojson_of_model = (model: model): Yojson.Safe.t =>
  switch (model) {
  | V(Fold, m) => `List([`String("fold"), m |> Kind.yojson_of_fold_model])
  | V(Info, m) => `List([`String("info"), m |> Kind.yojson_of_type_model])
  | V(Probe, _) => `List([`String("probe"), () |> yojson_of_unit])
  | V(Checkbox, _) => `List([`String("checkbox"), () |> yojson_of_unit])
  | V(Slider, _) => `List([`String("slider"), () |> yojson_of_unit])
  | V(SliderF, _) => `List([`String("sliderf"), () |> yojson_of_unit])
  | V(Card, m) => `List([`String("card"), m |> Kind.yojson_of_card_mode])
  | V(TextArea, _) => `List([`String("text"), () |> yojson_of_unit])
  };

/* Projectors in syntax */
[@deriving (show({with_path: false}), sexp, yojson)]
type t('syntax) = {
  id: Id.t,
  kind: Kind.t,
  syntax: 'syntax,
  model,
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
  [@deriving (show({with_path: false}), sexp, yojson)]
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
