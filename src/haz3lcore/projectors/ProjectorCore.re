open Util;

/* Projector dependencies are currently somewhat convoluted.
 * This is the lowermost projectors module; Base depends on
 * this (specifically, it parameterizes the type t below over piece).
 *
 * ProjectorBase then depends on this and on Base.piece('p),
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

let kind_of_model = (V(x, _)) => Kind.of_gadt(x);

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
