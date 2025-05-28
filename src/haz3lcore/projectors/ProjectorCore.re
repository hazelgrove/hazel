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

open Util;

module Kind = {
  type gadt('model, 'action, 'focus, 'ed, 'ed_a, 'ed_f) =
    // | Fold: gadt(FoldProj.model('ed), FoldProj.action, 'ed)
    | Info: gadt(
              TypeProj.model('ed),
              TypeProj.action('ed_a),
              TypeProj.focus('ed_f),
              'ed,
              'ed_a,
              'ed_f,
            )
    | Pair: gadt(
              PairProj.model('ed),
              PairProj.action('ed_a),
              PairProj.focus('ed_f),
              'ed,
              'ed_a,
              'ed_f,
            )
    // | Probe: gadt(ProbeProj.model('ed), ProbeProj.action, 'ed)
    // | Checkbox: gadt(CheckboxProj.model('ed), CheckboxProj.action, 'ed)
    | Slider: gadt(
                SliderProj.model('ed),
                SliderProj.action('ed_a),
                SliderProj.focus('ed_f),
                'ed,
                'ed_a,
                'ed_f,
              )
    // | SliderF: gadt(SliderFProj.model('ed), SliderFProj.action, 'ed)
    // | Card: gadt(CardProj.model('ed), CardProj.action, 'ed)
    | Livelit
        : gadt(
            LivelitProj.model('ed),
            LivelitProj.action('ed_a),
            LivelitProj.focus('ed_f),
            'ed,
            'ed_a,
            'ed_f,
          );
  // | TextArea: gadt(TextAreaProj.model('ed), TextAreaProj.action, 'ed);

  /* The different kinds of projector. New projector
   * types need to be registered here in order to be
   * able to create and update their instances */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t =
    // | Fold
    | Info
    | Pair
    // | Probe
    // | Checkbox
    | Slider
    // | SliderF
    // | Card
    | Livelit;
  // | TextArea;

  let gadt_eq =
      (
        type a,
        type b,
        type c,
        type d,
        type e,
        type f,
        type g,
        type h,
        type i,
        type j,
        type k,
        type l,
        gadt1: gadt(a, b, c, d, e, f),
        gadt2: gadt(g, h, i, j, k, l),
      ) => {
    switch (gadt1, gadt2) {
    | (Info, Info) => true
    | (Info, _) => false
    | (Pair, Pair) => true
    | (Pair, _) => false
    | (Slider, Slider) => true
    | (Slider, _) => false
    | (Livelit, Livelit) => true
    | (Livelit, _) => false
    };
  };

  let of_gadt =
      (
        type m,
        type a,
        type f,
        type ed_m,
        type ed_a,
        type ed_f,
        kind: gadt(m, a, f, ed_m, ed_a, ed_f),
      )
      : t =>
    switch (kind) {
    // | Fold => Fold
    | Info => Info
    | Pair => Pair
    // | Probe => Probe
    // | Checkbox => Checkbox
    | Slider => Slider
    | Livelit => Livelit
    // | SliderF => SliderF
    // | Card => Card
    // | TextArea => TextArea
    };

  type w('ed_m, 'ed_a, 'ed_f) =
    | W(gadt('a, 'b, 'c, 'ed_m, 'ed_a, 'ed_f)): w('ed_m, 'ed_a, 'ed_f);

  let (let.gadt) = (type b, kind: t, f: w('ed, 'ed_a, 'ed_f) => b) =>
    switch (kind) {
    // | Fold => f(W(Fold))
    | Info => f(W(Info))
    | Pair => f(W(Pair))
    // | Probe => f(W(Probe))
    // | Checkbox => f(W(Checkbox))
    | Slider => f(W(Slider))
    | Livelit => f(W(Livelit))
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
    // Livelit,
  ];

  let projectors: list(t) =
    livelit_projectors
    @ [
      // Fold,
      Info,
      Livelit,
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
    | Livelit => "livelit"
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
    | "livelit" => Livelit
    // | "sliderf" => SliderF
    // | "text" => TextArea
    // | "card" => Card
    | _ => failwith("Unknown projector kind")
    };
};

type model('ed_m, 'ed_a, 'ed_f) =
  | V(Kind.gadt('a, 'b, 'c, 'ed_m, 'ed_a, 'ed_f), 'a, Calc.saved(Any.t))
    : model('ed_m, 'ed_a, 'ed_f);

let kind_of_model = (V(x, _, _)) => Kind.of_gadt(x);

/* After adding a new projector module, add it here so that
 * it can be instantiated. The first-class module created by
 * this function must be reified whenever projector methods
 * are to be called; see `shape` below for an example */
let to_module =
    (
      type ed_m,
      type ed_a,
      type ed_f,
      type model,
      type action,
      type focus,
      kind: Kind.gadt(model, action, focus, ed_m, ed_a, ed_f),
    )
    : ProjectorBase.methods(model, action, focus, ed_m, ed_a, ed_f) =>
  switch (kind) {
  // | Fold => FoldProj.methods
  | Info => TypeProj.methods
  | Pair => PairProj.methods
  // | Probe => ProbeProj.methods
  // | Checkbox => CheckboxProj.methods
  | Slider => SliderProj.methods
  | Livelit => LivelitProj.methods
  // | SliderF => SliderFProj.methods
  // | Card => CardProj.methods
  // | TextArea => TextAreaProj.methods
  };

let pp_model =
    (
      type ed,
      type ed_a,
      type ed_f,
      _pp_ed,
      _pp_ed_a,
      _pp_ed_f,
      _f,
      _model: model(ed, ed_a, ed_f),
    ) => {
  // Format.printf(f, model |> kind_of_model |> Kind.name);  // Note(matt): I tried to make this but it gnarly type errors
  failwith(
    "cannot print",
  );
};

let model_of_sexp =
    (ed_of_sexp, _ed_a_of_sexp, _ed_f_of_sexp, sexp: Sexplib.Sexp.t)
    : model('ed, 'ed_a, 'ed_f) =>
  switch (sexp) {
  | List([Atom(kind_string), m]) =>
    open Kind;
    let.gadt W(kind_gadt) = kind_string |> Kind.of_name;
    let methods = to_module(kind_gadt);
    V(kind_gadt, m |> methods.model_of_sexp(ed_of_sexp), Calc.Pending);
  | _ => failwith("Projector desearialization failed")
  };

let sexp_of_model =
    (
      type ed,
      type ed_a,
      type ed_f,
      sexp_of_ed: ed => Sexplib.Sexp.t,
      _sexp_of_ed_a: ed_a => Sexplib.Sexp.t,
      _sexp_of_ed_f: ed_f => Sexplib.Sexp.t,
      model: model(ed, ed_a, ed_f),
    )
    : Sexplib.Sexp.t =>
  switch (model) {
  | V(kind_gadt, m, _) =>
    open Kind;
    let methods = to_module(kind_gadt);
    List([
      Atom(name(kind_of_model(model))),
      m |> methods.sexp_of_model(sexp_of_ed),
    ]);
  };

let model_of_yojson =
    (ed_of_yojson, _ed_a_of_yojson, _ed_f_of_yojson, yojson: Yojson.Safe.t)
    : model('ed, 'ed_a, 'ed_f) =>
  switch (yojson) {
  | `List([`String(kind_string), m]) =>
    open Kind;
    let.gadt W(kind_gadt) = kind_string |> Kind.of_name;
    let methods = to_module(kind_gadt);
    V(kind_gadt, m |> methods.model_of_yojson(ed_of_yojson), Calc.Pending);
  | _ => failwith("Projector desearialization failed")
  };

let yojson_of_model =
    (
      type ed,
      type ed_a,
      type ed_f,
      yojson_of_ed: ed => Yojson.Safe.t,
      _yojson_of_ed_a: ed_a => Yojson.Safe.t,
      _yojson_of_ed_f: ed_f => Yojson.Safe.t,
      model: model(ed, ed_a, ed_f),
    ) =>
  switch (model) {
  | V(kind_gadt, m, _) =>
    open Kind;
    let methods = to_module(kind_gadt);
    `List([
      `String(name(kind_of_model(model))),
      m |> methods.yojson_of_model(yojson_of_ed),
    ]);
  };

let mk_info =
    (~id: Id.t, ~statics: Statics.Map.t, ~dynamics: Dynamics.Map.t)
    : ProjectorBase.info => {
  id,
  statics: Statics.Map.lookup(id, statics),
  dynamics: Dynamics.Map.lookup(id, dynamics),
};

module Update = {
  type t('ed_f) =
    | A(Kind.gadt('a, 'b, 'c, 'ed_m, 'ed_a, 'ed_f), 'b): t('ed_a);

  let kind_of_focus = (A(x, _)) => Kind.of_gadt(x);

  let pp = (type ed_f, _pp_ed_f, _f, _focus: t(ed_f)) => {
    failwith("cannot print");
  };

  let sexp_of_t =
      (type ed_f, sexp_of_ed_f: ed_f => Sexplib.Sexp.t, t: t(ed_f))
      : Sexplib.Sexp.t =>
    switch (t) {
    | A(kind_gadt, m) =>
      open Kind;
      let methods = to_module(kind_gadt);
      List([
        Atom(name(of_gadt(kind_gadt))),
        m |> methods.sexp_of_action(sexp_of_ed_f),
      ]);
    };

  let t_of_sexp = (ed_f_of_sexp, sexp: Sexplib.Sexp.t): t('ed_f) =>
    switch (sexp) {
    | List([Atom(kind_string), m]) =>
      open Kind;
      let.gadt W(kind_gadt) = kind_string |> Kind.of_name;
      let methods = to_module(kind_gadt);
      A(kind_gadt, m |> methods.action_of_sexp(ed_f_of_sexp));
    | _ => failwith("Projector focus deserialization failed")
    };

  let yojson_of_t =
      (type ed_f, yojson_of_ed_f: ed_f => Yojson.Safe.t, t: t(ed_f)) =>
    switch (t) {
    | A(kind_gadt, m) =>
      open Kind;
      let methods = to_module(kind_gadt);
      `List([
        `String(name(of_gadt(kind_gadt))),
        m |> methods.yojson_of_action(yojson_of_ed_f),
      ]);
    };

  let t_of_yojson = (ed_f_of_yojson, yojson: Yojson.Safe.t): t('ed_f) =>
    switch (yojson) {
    | `List([`String(kind_string), m]) =>
      open Kind;
      let.gadt W(kind_gadt) = kind_string |> Kind.of_name;
      let methods = to_module(kind_gadt);
      A(kind_gadt, m |> methods.action_of_yojson(ed_f_of_yojson));
    | _ => failwith("Projector focus deserialization failed")
    };

  let update =
      (
        ~common: ProjectorInterface.common,
        ~update_ed,
        ~sort: Sort.t,
        ~id: Id.t,
        A(gadt1, action),
        V(gadt2, model, exp_cache),
      )
      : model('ed_m, 'ed_a, 'ed_f) =>
    if (Kind.gadt_eq(gadt1, gadt2)) {
      let methods = to_module(gadt2);
      V(
        gadt2,
        methods.update(
          ~update_ed,
          ~common,
          ~sort,
          mk_info(
            ~id,
            ~statics=common.statics.info_map,
            ~dynamics=common.dynamics,
          ),
          model,
          action |> Obj.magic // Note(Matt): Using Obj.magic here because we know the types are the same if gadt_eq(gadt1, gadt2) is true
        ),
        exp_cache,
      );
    } else {
      raise(Failure.Exception(Wrong_projector));
    };

  let calculate =
      (
        ~calculate_ed,
        ~common: ProjectorInterface.common,
        V(gadt, model, exp_cache): model('ed_m, 'ed_a, 'ed_f),
      )
      : model('ed_m, 'ed_a, 'ed_f) => {
    let methods = to_module(gadt);
    V(gadt, methods.calculate(~calculate_ed, ~common, model), exp_cache);
  };
};

module Focus = {
  type t('ed_f) =
    | F(Kind.gadt('a, 'b, 'c, 'ed_m, 'ed_a, 'ed_f), 'c): t('ed_f);

  let kind_of_focus = (F(x, _)) => Kind.of_gadt(x);

  let pp = (type ed_f, _pp_ed_f, _f, _focus: t(ed_f)) => {
    failwith("cannot print");
  };

  let sexp_of_t =
      (type ed_f, sexp_of_ed_f: ed_f => Sexplib.Sexp.t, t: t(ed_f))
      : Sexplib.Sexp.t =>
    switch (t) {
    | F(kind_gadt, m) =>
      open Kind;
      let methods = to_module(kind_gadt);
      List([
        Atom(name(of_gadt(kind_gadt))),
        m |> methods.sexp_of_focus(sexp_of_ed_f),
      ]);
    };

  let t_of_sexp = (ed_f_of_sexp, sexp: Sexplib.Sexp.t): t('ed_f) =>
    switch (sexp) {
    | List([Atom(kind_string), m]) =>
      open Kind;
      let.gadt W(kind_gadt) = kind_string |> Kind.of_name;
      let methods = to_module(kind_gadt);
      F(kind_gadt, m |> methods.focus_of_sexp(ed_f_of_sexp));
    | _ => failwith("Projector focus deserialization failed")
    };

  let yojson_of_t =
      (type ed_f, yojson_of_ed_f: ed_f => Yojson.Safe.t, t: t(ed_f)) =>
    switch (t) {
    | F(kind_gadt, m) =>
      open Kind;
      let methods = to_module(kind_gadt);
      `List([
        `String(name(of_gadt(kind_gadt))),
        m |> methods.yojson_of_focus(yojson_of_ed_f),
      ]);
    };

  let t_of_yojson = (ed_f_of_yojson, yojson: Yojson.Safe.t): t('ed_f) =>
    switch (yojson) {
    | `List([`String(kind_string), m]) =>
      open Kind;
      let.gadt W(kind_gadt) = kind_string |> Kind.of_name;
      let methods = to_module(kind_gadt);
      F(kind_gadt, m |> methods.focus_of_yojson(ed_f_of_yojson));
    | _ => failwith("Projector focus deserialization failed")
    };
};
