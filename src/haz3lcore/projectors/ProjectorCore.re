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
    | Fold: gadt(
              FoldProj.model('ed),
              FoldProj.action('ed_a),
              FoldProj.focus('ed_f),
              'ed,
              'ed_a,
              'ed_f,
            )
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
    | Probe: gadt(
               ProbeProj.model('ed),
               ProbeProj.action('ed_a),
               ProbeProj.focus('ed_f),
               'ed,
               'ed_a,
               'ed_f,
             )
    | Checkbox: gadt(
                  CheckboxProj.model('ed),
                  CheckboxProj.action('ed_a),
                  CheckboxProj.focus('ed_f),
                  'ed,
                  'ed_a,
                  'ed_f,
                )
    | Slider: gadt(
                SliderProj.model('ed),
                SliderProj.action('ed_a),
                SliderProj.focus('ed_f),
                'ed,
                'ed_a,
                'ed_f,
              )
    | SliderF: gadt(
                 SliderFProj.model('ed),
                 SliderFProj.action('ed_a),
                 SliderFProj.focus('ed_f),
                 'ed,
                 'ed_a,
                 'ed_f,
               )
    | Card: gadt(
              CardProj.model('ed),
              CardProj.action('ed_a),
              CardProj.focus('ed_f),
              'ed,
              'ed_a,
              'ed_f,
            )
    | Livelit: gadt(
                 LivelitProj.model('ed),
                 LivelitProj.action('ed_a),
                 LivelitProj.focus('ed_f),
                 'ed,
                 'ed_a,
                 'ed_f,
               )
    | TextArea
        : gadt(
            TextAreaProj.model('ed),
            TextAreaProj.action('ed_a),
            TextAreaProj.focus('ed_f),
            'ed,
            'ed_a,
            'ed_f,
          );

  /* The different kinds of projector. New projector
   * types need to be registered here in order to be
   * able to create and update their instances */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t =
    | Fold
    | Info
    | Probe
    | Pair
    | Checkbox
    | Slider
    | SliderF
    | Card
    | Livelit
    | TextArea;

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
    | (Fold, Fold) => true
    | (Fold, _) => false
    | (Probe, Probe) => true
    | (Probe, _) => false
    | (Checkbox, Checkbox) => true
    | (Checkbox, _) => false
    | (Slider, Slider) => true
    | (Slider, _) => false
    | (SliderF, SliderF) => true
    | (SliderF, _) => false
    | (TextArea, TextArea) => true
    | (TextArea, _) => false
    | (Card, Card) => true
    | (Card, _) => false
    | (Livelit, Livelit) => true
    | (Livelit, _) => false
    | (Pair, Pair) => true
    | (Pair, _) => false
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
    | Fold => Fold
    | Info => Info
    | Probe => Probe
    | Pair => Pair
    | Checkbox => Checkbox
    | Slider => Slider
    | SliderF => SliderF
    | Livelit => Livelit
    | Card => Card
    | TextArea => TextArea
    };

  type w('ed_m, 'ed_a, 'ed_f) =
    | W(gadt('a, 'b, 'c, 'ed_m, 'ed_a, 'ed_f)): w('ed_m, 'ed_a, 'ed_f);

  let (let.gadt) = (type b, kind: t, f: w('ed, 'ed_a, 'ed_f) => b) =>
    switch (kind) {
    | Fold => f(W(Fold))
    | Info => f(W(Info))
    | Probe => f(W(Probe))
    | Pair => f(W(Pair))
    | Checkbox => f(W(Checkbox))
    | Slider => f(W(Slider))
    | Livelit => f(W(Livelit))
    | SliderF => f(W(SliderF))
    | Card => f(W(Card))
    | TextArea => f(W(TextArea))
    };

  let livelit_projectors: list(t) = [
    Checkbox,
    Slider,
    Pair,
    SliderF,
    TextArea,
    Card,
  ];

  let projectors: list(t) =
    livelit_projectors @ [Fold, Info, Livelit, Probe];

  /* A friendly name for each projector. This is used
   * both for identifying a projector in the CSS and for
   * selecting projectors in the projector panel menu */
  let name = (p: t): string =>
    switch (p) {
    | Fold => "fold"
    | Info => "type"
    | Pair => "pair"
    | Probe => "probe"
    | Checkbox => "check"
    | Slider => "slider"
    | Livelit => "livelit"
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
    | "pair" => Pair
    | "probe" => Probe
    | "check" => Checkbox
    | "slider" => Slider
    | "livelit" => Livelit
    | "sliderf" => SliderF
    | "text" => TextArea
    | "card" => Card
    | _ => failwith("Unknown projector kind")
    };
};

type model('ed_m, 'ed_a, 'ed_f) =
  | V(
      Kind.gadt('a, 'b, 'c, 'ed_m, 'ed_a, 'ed_f),
      'a,
      Calc.saved(Language.Any.t),
    )
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
      module Editor:
        ProjectorInterface.EDITOR with
          type model = ed_m and type action = ed_a and type focus = ed_f,
      kind: Kind.gadt(model, action, focus, ed_m, ed_a, ed_f),
    )
    : (module ProjectorInterface.PROJECTOR with
         type model' = model and
         type action' = action and
         type focus' = focus and
         type editor_model = ed_m) =>
  switch (kind) {
  | Fold => (module FoldProj.M(Editor))
  | Info => (module TypeProj.M(Editor))
  | Probe => (module ProbeProj.M(Editor))
  | Pair => (module PairProj.M(Editor))
  | Checkbox => (module CheckboxProj.M(Editor))
  | Slider => (module SliderProj.M(Editor))
  | Livelit => (module LivelitProj.M(Editor))
  | SliderF => (module SliderFProj.M(Editor))
  | Card => (module CardProj.M(Editor))
  | TextArea => (module TextAreaProj.M(Editor))
  };

let pp_model =
    (type ed, type ed_a, type ed_f, _f, _model: model(ed, ed_a, ed_f)) => {
  // Format.printf(f, model |> kind_of_model |> Kind.name);  // Note(matt): I tried to make this but it gnarly type errors
  failwith(
    "cannot print",
  );
};

let model_of_sexp =
    (type ed_m, ~editor_module, sexp: Sexplib.Sexp.t)
    : model(ed_m, 'ed_a, 'ed_f) =>
  switch (sexp) {
  | List([Atom(kind_string), m]) =>
    open Kind;
    let.gadt W(kind_gadt) = kind_string |> Kind.of_name;
    let methods = to_module(editor_module, kind_gadt);
    let of_sexp =
      methods
      |> (
        (
          type p_m,
          type p_a,
          type p_f,
          module Methods:
            ProjectorInterface.PROJECTOR with
              type model' = p_m and
              type action' = p_a and
              type focus' = p_f and
              type editor_model = ed_m,
          sexp,
        ) => (
          {
            Methods.model'_of_sexp(sexp);
          }: p_m
        )
      );
    V(kind_gadt, m |> of_sexp, Calc.Pending);
  | _ => failwith("Projector desearialization failed")
  };

let sexp_of_model =
    (
      type ed,
      type ed_a,
      type ed_f,
      ~editor_module,
      model: model(ed, ed_a, ed_f),
    )
    : Sexplib.Sexp.t =>
  switch (model) {
  | V(kind_gadt, m, _) =>
    open Kind;
    let methods = to_module(editor_module, kind_gadt);
    let sexp_of_model =
      methods
      |> (
        (
          type p_m,
          type p_a,
          type p_f,
          module Methods:
            ProjectorInterface.PROJECTOR with
              type model' = p_m and
              type action' = p_a and
              type focus' = p_f and
              type editor_model = ed,
          m,
        ) => (
          {
            Methods.sexp_of_model'(m);
          }: Sexplib.Sexp.t
        )
      );
    List([Atom(name(kind_of_model(model))), m |> sexp_of_model]);
  };

let model_of_yojson =
    (type ed_m, ~editor_module, yojson: Yojson.Safe.t)
    : model(ed_m, 'ed_a, 'ed_f) =>
  switch (yojson) {
  | `List([`String(kind_string), m]) =>
    open Kind;
    let.gadt W(kind_gadt) = kind_string |> Kind.of_name;
    let methods = to_module(editor_module, kind_gadt);
    let of_yojson =
      methods
      |> (
        (
          type p_m,
          type p_a,
          type p_f,
          module Methods:
            ProjectorInterface.PROJECTOR with
              type model' = p_m and
              type action' = p_a and
              type focus' = p_f and
              type editor_model = ed_m,
          json,
        ) => (
          {
            Methods.model'_of_yojson(json);
          }: p_m
        )
      );
    V(kind_gadt, m |> of_yojson, Calc.Pending);
  | _ => failwith("Projector desearialization failed")
  };

let yojson_of_model =
    (
      type ed,
      type ed_a,
      type ed_f,
      ~editor_module,
      model: model(ed, ed_a, ed_f),
    ) =>
  switch (model) {
  | V(kind_gadt, m, _) =>
    open Kind;
    let methods = to_module(editor_module, kind_gadt);
    let yojson_of_model =
      methods
      |> (
        (
          type p_m,
          type p_a,
          type p_f,
          module Methods:
            ProjectorInterface.PROJECTOR with
              type model' = p_m and
              type action' = p_a and
              type focus' = p_f and
              type editor_model = ed,
          m,
        ) => (
          {
            Methods.yojson_of_model'(m);
          }: Yojson.Safe.t
        )
      );
    `List([`String(name(kind_of_model(model))), m |> yojson_of_model]);
  };

let mk_info =
    (
      ~id: Id.t,
      ~statics: Language.Statics.Map.t,
      ~dynamics: Language.Dynamics.Map.t,
    )
    : ProjectorBase.info => {
  id,
  statics: Language.Statics.Map.lookup(id, statics),
  dynamics: Language.Dynamics.Map.lookup(id, dynamics),
};

module Update = {
  type t('ed_m, 'ed_a, 'ed_f) =
    | A(Kind.gadt('a, 'b, 'c, 'ed_m, 'ed_a, 'ed_f), 'b)
      : t('ed_m, 'ed_a, 'ed_f);

  let kind_of_focus = (A(x, _)) => Kind.of_gadt(x);

  let pp =
      (type ed_m, type ed_a, type ed_f, _f, _focus: t(ed_m, ed_a, ed_f))
      : unit => {
    failwith("cannot print");
  };

  let sexp_of_t =
      (
        type ed_m,
        type ed_a,
        type ed_f,
        ~editor_module,
        t: t(ed_m, ed_a, ed_f),
      )
      : Sexplib.Sexp.t =>
    switch (t) {
    | A(kind_gadt, m) =>
      open Kind;
      let methods = to_module(editor_module, kind_gadt);
      let sexp_of_action =
        methods
        |> (
          (
            type p_m,
            type p_a,
            type p_f,
            type ed_m,
            module Methods:
              ProjectorInterface.PROJECTOR with
                type model' = p_m and
                type action' = p_a and
                type focus' = p_f and
                type editor_model = ed_m,
            a,
          ) => (
            {
              Methods.sexp_of_action'(a);
            }: Sexplib.Sexp.t
          )
        );
      List([Atom(name(of_gadt(kind_gadt))), m |> sexp_of_action]);
    };

  let t_of_sexp =
      (type ed_m, type ed_a, type ed_f, ~editor_module, sexp: Sexplib.Sexp.t)
      : t(ed_m, ed_a, ed_f) =>
    switch (sexp) {
    | List([Atom(kind_string), m]) =>
      open Kind;
      let.gadt W(kind_gadt) = kind_string |> Kind.of_name;
      let methods = to_module(editor_module, kind_gadt);
      let action_of_sexp =
        methods
        |> (
          (
            type p_m,
            type p_a,
            type p_f,
            module Methods:
              ProjectorInterface.PROJECTOR with
                type model' = p_m and
                type action' = p_a and
                type focus' = p_f and
                type editor_model = ed_m,
            sexp,
          ) => (
            {
              Methods.action'_of_sexp(sexp);
            }: p_a
          )
        );
      A(kind_gadt, m |> action_of_sexp);
    | _ => failwith("Projector focus deserialization failed")
    };

  let yojson_of_t =
      (
        type ed_m,
        type ed_a,
        type ed_f,
        ~editor_module,
        t: t(ed_m, ed_a, ed_f),
      ) =>
    switch (t) {
    | A(kind_gadt, m) =>
      open Kind;
      let methods = to_module(editor_module, kind_gadt);
      let yojson_of_action =
        methods
        |> (
          (
            type p_m,
            type p_a,
            type p_f,
            module Methods:
              ProjectorInterface.PROJECTOR with
                type model' = p_m and
                type action' = p_a and
                type focus' = p_f and
                type editor_model = ed_m,
            a,
          ) => (
            {
              Methods.yojson_of_action'(a);
            }: Yojson.Safe.t
          )
        );
      `List([`String(name(of_gadt(kind_gadt))), m |> yojson_of_action]);
    };

  let t_of_yojson =
      (type ed_m, type ed_a, type ed_f, ~editor_module, yojson: Yojson.Safe.t)
      : t(ed_m, ed_a, ed_f) =>
    switch (yojson) {
    | `List([`String(kind_string), m]) =>
      open Kind;
      let.gadt W(kind_gadt) = kind_string |> Kind.of_name;
      let methods = to_module(editor_module, kind_gadt);
      let action_of_yojson =
        methods
        |> (
          (
            type p_m,
            type p_a,
            type p_f,
            module Methods:
              ProjectorInterface.PROJECTOR with
                type model' = p_m and
                type action' = p_a and
                type focus' = p_f and
                type editor_model = ed_m,
            json,
          ) => (
            {
              Methods.action'_of_yojson(json);
            }: p_a
          )
        );
      A(kind_gadt, m |> action_of_yojson);
    | _ => failwith("Projector focus deserialization failed")
    };

  let update =
      (
        type ed_m,
        type ed_a,
        type ed_f,
        ~editor_module,
        ~common: ProjectorInterface.common,
        ~sort: Sort.t,
        ~id: Id.t,
        A(gadt1, action),
        V(gadt2, model, exp_cache),
      )
      : model(ed_m, ed_a, ed_f) =>
    if (Kind.gadt_eq(gadt1, gadt2)) {
      let methods = to_module(editor_module, gadt2);
      let update =
        methods
        |> (
          (
            type p_m,
            type p_a,
            type p_f,
            module Methods:
              ProjectorInterface.PROJECTOR with
                type model' = p_m and
                type action' = p_a and
                type focus' = p_f and
                type editor_model = ed_m,
            model,
            action,
          ) => (
            {
              Methods.update(
                ~common,
                ~sort,
                mk_info(
                  ~id,
                  ~statics=common.statics.info_map,
                  ~dynamics=common.dynamics,
                ),
                model,
                action,
              );
            }: p_m
          )
        );
      V(gadt2, update(model, Obj.magic(action)), exp_cache);
    } else {
      raise(Failure.Exception(Wrong_projector));
    };

  let calculate =
      (
        type ed_m,
        type ed_a,
        type ed_f,
        ~editor_module,
        ~common: ProjectorInterface.common,
        V(gadt, model, exp_cache): model(ed_m, ed_a, ed_f),
      )
      : model(ed_m, ed_a, ed_f) => {
    let methods = to_module(editor_module, gadt);
    let calculate =
      methods
      |> (
        (
          type p_m,
          type p_a,
          type p_f,
          module Methods:
            ProjectorInterface.PROJECTOR with
              type model' = p_m and
              type action' = p_a and
              type focus' = p_f and
              type editor_model = ed_m,
          m,
        ) => (
          {
            Methods.calculate(~common, m);
          }: p_m
        )
      );
    V(gadt, calculate(model), exp_cache);
  };
};

module Focus = {
  type t('ed_m, 'ed_a, 'ed_f) =
    | F(Kind.gadt('a, 'b, 'c, 'ed_m, 'ed_a, 'ed_f), 'c)
      : t('ed_m, 'ed_a, 'ed_f);

  let get_cursor_info =
      (
        type ed_m,
        type ed_a,
        type ed_f,
        ~editor_module,
        ~common: ProjectorInterface.common,
        ~inject: Update.t(ed_m, ed_a, ed_f) => Ui_effect.t(unit),
        ~read_only: bool,
        V(gadt1, model, _exp_cache): model(ed_m, ed_a, ed_f),
        F(gadt2, focus): t(ed_m, ed_a, ed_f),
      ) =>
    if (Kind.gadt_eq(gadt1, gadt2)) {
      let methods = to_module(editor_module, gadt1);
      let get_cursor_info =
        methods
        |> (
          (
            type p_m,
            type p_a,
            type p_f,
            module Methods:
              ProjectorInterface.PROJECTOR with
                type model' = p_m and
                type action' = p_a and
                type focus' = p_f and
                type editor_model = ed_m,
            model,
            focus,
          ) => (
            {
              Methods.get_cursor_info(
                ~common,
                ~inject=a => inject(Update.A(gadt1, Obj.magic(a))),
                ~read_only,
                model,
                focus,
              );
            }: Cursor.t
          )
        );
      get_cursor_info(model, Obj.magic(focus));
    } else {
      Cursor.empty;
    };

  let kind_of_focus = (F(x, _)) => Kind.of_gadt(x);

  let pp = (type ed_m, type ed_a, type ed_f, _f, _focus: t(ed_m, ed_a, ed_f)) => {
    failwith("cannot print");
  };

  let sexp_of_t =
      (
        type ed_m,
        type ed_a,
        type ed_f,
        ~editor_module,
        t: t(ed_m, ed_a, ed_f),
      )
      : Sexplib.Sexp.t =>
    switch (t) {
    | F(kind_gadt, m) =>
      open Kind;
      let methods = to_module(editor_module, kind_gadt);
      let sexp_of_focus =
        methods
        |> (
          (
            type p_m,
            type p_a,
            type p_f,
            module Methods:
              ProjectorInterface.PROJECTOR with
                type model' = p_m and
                type action' = p_a and
                type focus' = p_f and
                type editor_model = ed_m,
            f,
          ) => (
            {
              Methods.sexp_of_focus'(f);
            }: Sexplib.Sexp.t
          )
        );
      List([Atom(name(of_gadt(kind_gadt))), m |> sexp_of_focus]);
    };

  let t_of_sexp =
      (type ed_m, type ed_a, type ed_f, ~editor_module, sexp: Sexplib.Sexp.t)
      : t(ed_m, ed_a, ed_f) =>
    switch (sexp) {
    | List([Atom(kind_string), m]) =>
      open Kind;
      let.gadt W(kind_gadt) = kind_string |> Kind.of_name;
      let methods = to_module(editor_module, kind_gadt);
      let focus_of_sexp =
        methods
        |> (
          (
            type p_m,
            type p_a,
            type p_f,
            module Methods:
              ProjectorInterface.PROJECTOR with
                type model' = p_m and
                type action' = p_a and
                type focus' = p_f and
                type editor_model = ed_m,
            sexp,
          ) => (
            {
              Methods.focus'_of_sexp(sexp);
            }: p_f
          )
        );
      F(kind_gadt, m |> focus_of_sexp);
    | _ => failwith("Projector focus deserialization failed")
    };

  let yojson_of_t =
      (
        type ed_m,
        type ed_a,
        type ed_f,
        ~editor_module,
        t: t(ed_m, ed_a, ed_f),
      ) =>
    switch (t) {
    | F(kind_gadt, m) =>
      open Kind;
      let methods = to_module(editor_module, kind_gadt);
      let yojson_of_focus =
        methods
        |> (
          (
            type p_m,
            type p_a,
            type p_f,
            module Methods:
              ProjectorInterface.PROJECTOR with
                type model' = p_m and
                type action' = p_a and
                type focus' = p_f and
                type editor_model = ed_m,
            f,
          ) => (
            {
              Methods.yojson_of_focus'(f);
            }: Yojson.Safe.t
          )
        );
      `List([`String(name(of_gadt(kind_gadt))), m |> yojson_of_focus]);
    };

  let t_of_yojson =
      (type ed_m, type ed_a, type ed_f, ~editor_module, yojson: Yojson.Safe.t)
      : t(ed_m, ed_a, ed_f) =>
    switch (yojson) {
    | `List([`String(kind_string), m]) =>
      open Kind;
      let.gadt W(kind_gadt) = kind_string |> Kind.of_name;
      let methods = to_module(editor_module, kind_gadt);
      let focus_of_yojson =
        methods
        |> (
          (
            type p_m,
            type p_a,
            type p_f,
            module Methods:
              ProjectorInterface.PROJECTOR with
                type model' = p_m and
                type action' = p_a and
                type focus' = p_f and
                type editor_model = ed_m,
            json,
          ) => (
            {
              Methods.focus'_of_yojson(json);
            }: p_f
          )
        );
      F(kind_gadt, m |> focus_of_yojson);
    | _ => failwith("Projector focus deserialization failed")
    };
};
