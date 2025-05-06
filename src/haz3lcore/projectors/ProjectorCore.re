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
    // | Fold
    | Info
    | Pair
    // | Probe
    // | Checkbox
    | Slider;
  // | SliderF
  // | Card
  // | TextArea;

  type gadt('model, 'action, 'ed) =
    // | Fold: gadt(FoldProj.model('ed), FoldProj.action, 'ed)
    | Info: gadt(TypeProj.model('ed), TypeProj.action, 'ed)
    | Pair: gadt(PairProj.model('ed), PairProj.action, 'ed)
    // | Probe: gadt(ProbeProj.model('ed), ProbeProj.action, 'ed)
    // | Checkbox: gadt(CheckboxProj.model('ed), CheckboxProj.action, 'ed)
    | Slider: gadt(SliderProj.model('ed), SliderProj.action, 'ed);
  // | SliderF: gadt(SliderFProj.model('ed), SliderFProj.action, 'ed)
  // | Card: gadt(CardProj.model('ed), CardProj.action, 'ed)
  // | TextArea: gadt(TextAreaProj.model('ed), TextAreaProj.action, 'ed);

  let of_gadt = (type m, type a, type ed, kind: gadt(m, a, ed)): t =>
    switch (kind) {
    // | Fold => Fold
    | Info => Info
    | Pair => Pair
    // | Probe => Probe
    // | Checkbox => Checkbox
    | Slider => Slider
    // | SliderF => SliderF
    // | Card => Card
    // | TextArea => TextArea
    };

  type w('ed) =
    | W(gadt('a, 'b, 'ed)): w('ed);

  let (let.gadt) = (type b, kind: t, f: w('ed) => b) =>
    switch (kind) {
    // | Fold => f(W(Fold))
    | Info => f(W(Info))
    | Pair => f(W(Pair))
    // | Probe => f(W(Probe))
    // | Checkbox => f(W(Checkbox))
    | Slider => f(W(Slider))
    // | SliderF => f(W(SliderF))
    // | Card => f(W(Card))
    // | TextArea => f(W(TextArea))
    };

  let livelit_projectors: list(t) = [
    // Checkbox,
    Slider,
    Pair,
    // SliderF,
    // TextArea,
    // Card,
  ];

  let projectors: list(t) =
    livelit_projectors
    @ [
      // Fold,
      Info,
      // Probe
    ];

  /* A friendly name for each projector. This is used
   * both for identifying a projector in the CSS and for
   * selecting projectors in the projector panel menu */
  let name = (p: t): string =>
    switch (p) {
    // | Fold => "fold"
    | Info => "type"
    | Pair => "pair"
    // | Probe => "probe"
    // | Checkbox => "check"
    | Slider => "slider"
    // | SliderF => "sliderf"
    // | Card => "card"
    // | TextArea => "text"
    };

  /* This must be updated and kept 1-to-1 with the above
   * name function in order to be able to select the
   * projector in the projector panel menu */
  let of_name = (p: string): t =>
    switch (p) {
    // | "fold" => Fold
    | "type" => Info
    | "pair" => Pair
    // | "probe" => Probe
    // | "check" => Checkbox
    | "slider" => Slider
    // | "sliderf" => SliderF
    // | "text" => TextArea
    // | "card" => Card
    | _ => failwith("Unknown projector kind")
    };
};

type model('ed) =
  | V(Kind.gadt('a, 'b, 'ed), 'a): model('ed);

let kind_of_model = (V(x, _)) => Kind.of_gadt(x);

let pp_model = (type ed, _pp_ed, f, model: model(ed)) =>
  Format.fprintf(
    f,
    switch (model) {
    // | V(Fold, _) => "Fold"
    | V(Info, _) => "Info"
    | V(Pair, _) => "Pair"
    // | V(Probe, _) => "Probe"
    // | V(Checkbox, _) => "Checkbox"
    | V(Slider, _) => "Slider"
    // | V(SliderF, _) => "SliderF"
    // | V(Card, _) => "Card"
    // | V(TextArea, _) => "TextArea"
    },
  );

let model_of_sexp = (ed_of_sexp, sexp: Sexplib.Sexp.t): model('ed) =>
  // take s-expressions of the form ("fold", m), deserialize m, and turn it into V(Fold, m)
  switch (sexp) {
  // | List([Atom("fold"), m]) =>
  //   V(Fold, m |> FoldProj.model_of_sexp(ed_of_sexp))
  | List([Atom("info"), m]) =>
    V(Info, m |> TypeProj.model_of_sexp(ed_of_sexp))
  // | List([Atom("probe"), _]) => V(Probe, ())
  // | List([Atom("checkbox"), _]) => V(Checkbox, ())
  | List([Atom("slider"), m]) =>
    V(Slider, SliderProj.model_of_sexp(ed_of_sexp, m))
  // | List([Atom("sliderf"), _]) => V(SliderF, ())
  // | List([Atom("card"), m]) =>
  // V(Card, m |> CardProj.model_of_sexp(ed_of_sexp))
  // | List([Atom("text"), _]) => V(TextArea, ())
  | _ => failwith("Unknown projector kind")
  };

let sexp_of_model =
    (type ed, sexp_of_ed: ed => Sexplib0.Sexp.t, model: model(ed))
    : Sexplib.Sexp.t =>
  switch (model) {
  // | V(Fold, m) =>
  // List([Atom("fold"), m |> FoldProj.sexp_of_model(sexp_of_ed)])
  | V(Info, m) =>
    List([Atom("info"), m |> TypeProj.sexp_of_model(sexp_of_ed)])
  | V(Pair, m) =>
    List([Atom("pair"), m |> PairProj.sexp_of_model(sexp_of_ed)])
  // | V(Probe, _) => List([Atom("probe"), () |> sexp_of_unit])
  // | V(Checkbox, _) => List([Atom("checkbox"), () |> sexp_of_unit])
  | V(Slider, m) =>
    List([Atom("slider"), m |> SliderProj.sexp_of_model(sexp_of_ed)])
  // | V(SliderF, _) => List([Atom("sliderf"), () |> sexp_of_unit])
  // | V(Card, m) =>
  // List([Atom("card"), m |> CardProj.sexp_of_model(sexp_of_ed)])
  // | V(TextArea, _) => List([Atom("text"), () |> sexp_of_unit])
  };

let model_of_yojson = (ed_of_yojson, yojson: Yojson.Safe.t): model('ed) =>
  switch (yojson) {
  // | `List([`String("fold"), m]) =>
  // V(Fold, m |> FoldProj.model_of_yojson(ed_of_yojson))
  | `List([`String("info"), m]) =>
    V(Info, m |> TypeProj.model_of_yojson(ed_of_yojson))
  // | `List([`String("probe"), _]) => V(Probe, ())
  // | `List([`String("checkbox"), _]) => V(Checkbox, ())
  | `List([`String("slider"), m]) =>
    V(Slider, m |> SliderProj.model_of_yojson(ed_of_yojson))
  // | `List([`String("sliderf"), _]) => V(SliderF, ())
  // | `List([`String("card"), m]) =>
  // V(Card, m |> CardProj.model_of_yojson(ed_of_yojson))
  // | `List([`String("text"), _]) => V(TextArea, ())
  | _ => failwith("Unknown projector kind")
  };

let yojson_of_model =
    (type ed, yojson_of_ed: ed => Yojson.Safe.t, model: model(ed))
    : Yojson.Safe.t =>
  switch (model) {
  // | V(Fold, m) =>
  // `List([`String("fold"), m |> FoldProj.yojson_of_model(yojson_of_ed)])
  | V(Info, m) =>
    `List([`String("info"), m |> TypeProj.yojson_of_model(yojson_of_ed)])
  | V(Pair, m) =>
    `List([`String("pair"), m |> PairProj.yojson_of_model(yojson_of_ed)])
  // | V(Probe, _) => `List([`String("probe"), () |> yojson_of_unit])
  // | V(Checkbox, _) => `List([`String("checkbox"), () |> yojson_of_unit])
  | V(Slider, m) =>
    `List([
      `String("slider"),
      m |> SliderProj.yojson_of_model(yojson_of_ed),
    ])
  // | V(SliderF, _) => `List([`String("sliderf"), () |> yojson_of_unit])
  // | V(Card, m) =>
  // `List([`String("card"), m |> CardProj.yojson_of_model(yojson_of_ed)])
  // | V(TextArea, _) => `List([`String("text"), () |> yojson_of_unit])
  };
