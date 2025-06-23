open Util;
module Kind = ProjectorKind;

// ============================================================================
//     |  |  ___  __   ___     __   ___     __   __        __   __        __
//     |__| |__  |__) |__     |__) |__     |  \ |__)  /\  / _` /  \ |\ | /__`
//     |  | |___ |  \ |___    |__) |___    |__/ |  \ /~~\ \__> \__/ | \| .__/
// ============================================================================
//
//  This file handles dispatch to the various projectors. It does this using GADTs to
//  store different-typed models for different projectors. GADTs don't interact nicely with the
//  functors used in projectors, so these dispatch functions all look a lot more complicated than
//  they should be.
//
//  If you are here to add a new projector kind:
//
//       - Stay in the "Projector Kinds" section of this file.
//       - Add a case to each type / method, copying the others.
//
//  If you are here to add a new projector method:
//
//      - Stay in the "Projector Method Dispatch" section of this file.
//      - Copy a pre-existing method, I have annotated `calculate` with explanations
//        as it is the simplest example.

// ============================================================
//                       Projector Kinds
// ============================================================

/* This module is called Private because it should only be used in this file */
module GADTPrivate = {
  /* The gadt type is on the surface, exactly the same as the ProjectorCore.Kind.t type,
   * but it allows us to associate the projector's model, action, and focus types
   * with the projector kind, so that we can type check them usefully.
   *
   * It should have one constructor for each projector in ProjectorCore.Kind.t.*/
  type gadt('model, 'action, 'focus, 'ed, 'ed_a, 'ed_f) =
    | Fold: gadt(
              FoldProj.model('ed),
              FoldProj.action('ed_a),
              FoldProj.focus('ed_f),
              'ed,
              'ed_a,
              'ed_f,
            )
    | Type: gadt(
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

  /* w is a wrapper around the gadt type that lets us return
   * an arbitrary gadt without the type checker complaining. */
  type w('ed_m, 'ed_a, 'ed_f) =
    | W(gadt('a, 'b, 'c, 'ed_m, 'ed_a, 'ed_f)): w('ed_m, 'ed_a, 'ed_f);

  /*  let.gadt is the closest thing we can make to a `kind_to_gadt` function.
   *  It lets you take a kind and use the corresponding gadt inside a let block */
  let (let.gadt) =
      (type b, kind: ProjectorKind.t, f: w('ed, 'ed_a, 'ed_f) => b) => {
    /* Welcome! If you were brought here by the type checker, you are
     * probably trying to update the projector kind data structure.
     * We're glad you're here. The gadt type above needs to be kept
     * in sync with the ProjectorKind.t type. Go ahead and update
     * `type gadt` and then update the cases on all the other functions
     * in this section. You should be able to copy the pre-existing cases
     * without understanding how GADTs work. */
    switch (kind) {
    | Fold => f(W(Fold))
    | Type => f(W(Type))
    | Probe => f(W(Probe))
    | Pair => f(W(Pair))
    | Checkbox => f(W(Checkbox))
    | Slider => f(W(Slider))
    | Livelit => f(W(Livelit))
    | SliderF => f(W(SliderF))
    | Card => f(W(Card))
    | TextArea => f(W(TextArea))
    };
  };

  let gadt_to_kind =
      (
        type m,
        type a,
        type f,
        type ed_m,
        type ed_a,
        type ed_f,
        gadt: gadt(m, a, f, ed_m, ed_a, ed_f),
      )
      : ProjectorKind.t =>
    switch (gadt) {
    | Fold => Fold
    | Type => Type
    | Probe => Probe
    | Pair => Pair
    | Checkbox => Checkbox
    | Slider => Slider
    | SliderF => SliderF
    | Livelit => Livelit
    | Card => Card
    | TextArea => TextArea
    };

  /* Please ignore the type parameters, this is a normal equality function
   * for the GADT type, it just can't be autogenerated because it's a GADT. */
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
    | (Type, Type) => true
    | (Type, _) => false
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

  /* Takes a gadt and returns the module with methods associated with that
   * projector kind. */
  let to_module =
      (
        type ed_m,
        type ed_a,
        type ed_f,
        type model,
        type action,
        type focus,
        module Editor:
          EditorInterface.EDITOR with
            type model = ed_m and type action = ed_a and type focus = ed_f,
        kind: gadt(model, action, focus, ed_m, ed_a, ed_f),
      )
      : (module ProjectorInterface.PROJECTOR with
           type model' = model and
           type action' = action and
           type focus' = focus and
           type editor_model = ed_m) =>
    switch (kind) {
    | Fold => (module FoldProj.M(Editor))
    | Type => (module TypeProj.M(Editor))
    | Probe => (module ProbeProj.M(Editor))
    | Pair => (module PairProj.M(Editor))
    | Checkbox => (module CheckboxProj.M(Editor))
    | Slider => (module SliderProj.M(Editor))
    | Livelit => (module LivelitProj.M(Editor))
    | SliderF => (module SliderFProj.M(Editor))
    | Card => (module CardProj.M(Editor))
    | TextArea => (module TextAreaProj.M(Editor))
    };
};

open GADTPrivate;

// ============================================================
//                      Projector Types
// ============================================================

type model('ed_m, 'ed_a, 'ed_f) =
  | V(
      gadt('model, 'b, 'c, 'ed_m, 'ed_a, 'ed_f),
      'model,
      Calc.saved(Language.Any.t),
    )
    : model('ed_m, 'ed_a, 'ed_f);

type action('ed_m, 'ed_a, 'ed_f) =
  | A(gadt('a, 'action, 'c, 'ed_m, 'ed_a, 'ed_f), 'action)
    : action('ed_m, 'ed_a, 'ed_f);

type focus('ed_m, 'ed_a, 'ed_f) =
  | F(gadt('a, 'b, 'focus, 'ed_m, 'ed_a, 'ed_f), 'focus)
    : focus('ed_m, 'ed_a, 'ed_f);

let kind_of_model = (V(x, _, _)) => gadt_to_kind(x);

let get_cached_term = (V(_, _, exp_cache)) => Calc.get_saved_exc(exp_cache);

// ============================================================
//                Projector Method Dispatch
// ============================================================

let init =
    (
      type ed,
      type ed_a,
      type ed_f,
      ~editor_module,
      kind: Kind.t,
      any: Language.Any.t,
      ed: unit => option(ed),
    )
    : option(model(ed, ed_a, ed_f)) => {
  let.gadt W(kind_gadt) = kind;
  let methods = to_module(editor_module, kind_gadt);
  let init =
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
        any,
        ed,
      ) => {
        Methods.mk(any, ed);
      }
    );
  switch (init(any, ed)) {
  | None => None
  | Some(model) => Some(V(kind_gadt, model, Calc.Pending))
  };
};

let update =
    (
      type ed_m,
      type ed_a,
      type ed_f,
      ~editor_module,
      ~common: Common.t,
      ~sort: Sort.t,
      ~id: Id.t,
      A(gadt1, action),
      V(gadt2, model, exp_cache),
    )
    : model(ed_m, ed_a, ed_f) =>
  if (gadt_eq(gadt1, gadt2)) {
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
            Methods.update(~common, ~sort, ~id, model, action);
          }: p_m
        )
      );
    V(gadt2, update(model, Obj.magic(action)), exp_cache);
  } else {
    raise(Failure.Exception(Wrong_projector));
  };

let make_term =
    (
      type ed,
      type ed_a,
      type ed_f,
      ~editor_module,
      ~sort: Sort.t,
      V(k, m, exp_cache): model(ed, ed_a, ed_f),
    )
    : (model(ed, ed_a, ed_f), Calc.t(Language.Any.t)) => {
  let methods = to_module(editor_module, k);
  let mk_term =
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
        ~sort,
        ~prev,
        m,
      ) => {
        Methods.mk_term(~sort, ~prev, m);
      }
    );
  let (ed', term) = mk_term(~sort, ~prev=exp_cache, m);
  (V(k, ed', term |> Calc.save), term);
};

let calculate =
    (
      type ed_m,
      type ed_a,
      type ed_f,
      ~editor_module,
      ~common: Common.t,
      projector_model: model(ed_m, ed_a, ed_f),
    )
    : model(ed_m, ed_a, ed_f) => {
  /* 1. Unpack the projector model to get the GADT, model, and exp_cache.
     Note: this introduces 'existential' type variables for the model,
     action, and focus types. The type checker usually gives existential
     types $ names. */
  let V(gadt, model, exp_cache) = projector_model;
  /* 2. Use the GADT to get the methods for the specific projector kind.
     This is where the type checking happens, ensuring that the model,
     action, and focus types match the GADT's expectations. */
  let methods = to_module(editor_module, gadt);
  /* 3. We wrap the call to methods.calculate so that we can name the
     existential types. It feels like you should be able to reduce this
     application of "methods" but I can't find a way to please the type
     checker without this indirection. */
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
  /* 4. Finally we call the calculate function and then package it
     back up into a model with the same GADT type. */
  V(gadt, calculate(model), exp_cache);
};

let get_cursor_info =
    (
      type ed_m,
      type ed_a,
      type ed_f,
      ~editor_module,
      ~common: Common.t,
      ~inject: action(ed_m, ed_a, ed_f) => Ui_effect.t(unit),
      ~read_only: bool,
      V(gadt1, model, _exp_cache): model(ed_m, ed_a, ed_f),
      F(gadt2, focus): focus(ed_m, ed_a, ed_f),
    ) =>
  if (gadt_eq(gadt1, gadt2)) {
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
              ~inject=a => inject(A(gadt1, Obj.magic(a))),
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

let placeholder =
    (
      type ed_m,
      type ed_a,
      type ed_f,
      ~editor_module,
      ~common,
      p: Base.projector(model(ed_m, ed_a, ed_f)),
    )
    : Util.ProjectorShape.t => {
  let V(kind, model, _) = p.model;
  let methods = to_module(editor_module, kind);
  /* Projector data which is dependent on semantics,
   * separated out for dependency reasons */
  let placeholder =
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
      ) => {
        Methods.placeholder(~common, ~id=p.id, m);
      }
    );
  placeholder(model);
};

/* Route top-level metadata to the projector view function. */
let view =
    (
      type ed_m,
      type ed_a,
      type ed_f,
      ~editor_module,
      ~common,
      ~inject: action(ed_m, ed_a, ed_f) => Ui_effect.t(unit),
      ~escape: ProjectorInterface.external_action => Ui_effect.t(unit),
      ~take_focus: focus(ed_m, ed_a, ed_f) => Ui_effect.t(unit),
      ~focus: option(focus(ed_m, ed_a, ed_f)),
      ~info: ProjectorInterface.info,
      model,
    )
    : ProjectorInterface.View.t => {
  let V(kind_gadt, model, _) = model;
  let methods = to_module(editor_module, kind_gadt);
  let view =
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
      ) => {
        Methods.view;
      }
    );
  view(
    ~common,
    ~inject=a => inject(A(kind_gadt, a)),
    ~escape,
    ~take_focus=f => take_focus(F(kind_gadt, f)),
    ~focus=
      switch (focus) {
      | Some(F(k, f)) when gadt_eq(k, kind_gadt) => Some(Obj.magic(f)) // Note(Matt): Using Obj.magic here because we know the types are the same if gadt_eq(k, kind_gadt) is true
      | _ => None
      },
    ~info,
    model,
  );
};

// ============================================================
// Pure Garbage (because the ppx deriver doesn't work on gadts)
// ============================================================

// Fold this module away and never worry about it again.

module PPXMethods = {
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
      List([Atom(Kind.name(kind_of_model(model))), m |> sexp_of_model]);
    };

  let model_of_yojson =
      (type ed_m, ~editor_module, yojson: Yojson.Safe.t)
      : model(ed_m, 'ed_a, 'ed_f) =>
    switch (yojson) {
    | `List([`String(kind_string), m]) =>
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
      `List([
        `String(Kind.name(kind_of_model(model))),
        m |> yojson_of_model,
      ]);
    };

  let pp_action =
      (
        type ed_m,
        type ed_a,
        type ed_f,
        _f,
        _action: action(ed_m, ed_a, ed_f),
      )
      : unit => {
    failwith("cannot print");
  };

  let sexp_of_action =
      (
        type ed_m,
        type ed_a,
        type ed_f,
        ~editor_module,
        t: action(ed_m, ed_a, ed_f),
      )
      : Sexplib.Sexp.t =>
    switch (t) {
    | A(kind_gadt, m) =>
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
      List([
        Atom(Kind.name(gadt_to_kind(kind_gadt))),
        m |> sexp_of_action,
      ]);
    };

  let action_of_sexp =
      (type ed_m, type ed_a, type ed_f, ~editor_module, sexp: Sexplib.Sexp.t)
      : action(ed_m, ed_a, ed_f) =>
    switch (sexp) {
    | List([Atom(kind_string), m]) =>
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

  let yojson_of_action =
      (
        type ed_m,
        type ed_a,
        type ed_f,
        ~editor_module,
        t: action(ed_m, ed_a, ed_f),
      ) =>
    switch (t) {
    | A(kind_gadt, m) =>
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
      `List([
        `String(Kind.name(gadt_to_kind(kind_gadt))),
        m |> yojson_of_action,
      ]);
    };

  let action_of_yojson =
      (type ed_m, type ed_a, type ed_f, ~editor_module, yojson: Yojson.Safe.t)
      : action(ed_m, ed_a, ed_f) =>
    switch (yojson) {
    | `List([`String(kind_string), m]) =>
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

  let pp_focus =
      (type ed_m, type ed_a, type ed_f, _f, _focus: focus(ed_m, ed_a, ed_f)) => {
    failwith("cannot print");
  };

  let sexp_of_focus =
      (
        type ed_m,
        type ed_a,
        type ed_f,
        ~editor_module,
        t: focus(ed_m, ed_a, ed_f),
      )
      : Sexplib.Sexp.t =>
    switch (t) {
    | F(kind_gadt, m) =>
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
      List([Atom(Kind.name(gadt_to_kind(kind_gadt))), m |> sexp_of_focus]);
    };

  let focus_of_sexp =
      (type ed_m, type ed_a, type ed_f, ~editor_module, sexp: Sexplib.Sexp.t)
      : focus(ed_m, ed_a, ed_f) =>
    switch (sexp) {
    | List([Atom(kind_string), m]) =>
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

  let yojson_of_focus =
      (
        type ed_m,
        type ed_a,
        type ed_f,
        ~editor_module,
        t: focus(ed_m, ed_a, ed_f),
      ) =>
    switch (t) {
    | F(kind_gadt, m) =>
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
      `List([
        `String(Kind.name(gadt_to_kind(kind_gadt))),
        m |> yojson_of_focus,
      ]);
    };

  let focus_of_yojson =
      (type ed_m, type ed_a, type ed_f, ~editor_module, yojson: Yojson.Safe.t)
      : focus(ed_m, ed_a, ed_f) =>
    switch (yojson) {
    | `List([`String(kind_string), m]) =>
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

include PPXMethods;
