open Semantics.Core;
open Tyxml_js;
open Printf;
open Scanf;

/* Hole Refs in HTML */
let x =
  Html5.(
    span(
      ~a=[a_id("hole_ref_" ++ hr.hole_ref_label), a_class(["holeRef"])],
      [],
    )
  );
let x' = Tyxml_js.To_dom.of_span(x);
x'##.classList##contains(Js.string("holeRef"));
x'##.id;

type view_type =
  | Inline(Tyxml_js.Html5.elt([ Html_types.span]))
  | MultiLine(Tyxml_js.Html5.elt([ Html_types.div_content_fun]));

module type PALETTE = {
  let name: string;
  let expansion_ty: HTyp.t;

  type model;
  let init_model: model;

  type model_updater = model => unit;
  let view: (model, model_updater) => view_type;

  let expand: model => UHExp.t;

  let serialize: model => PaletteSerializedModel.t;
  let deserialize: PaletteSerializedModel.t => model;
};

/* hack to persist around Model.t objects for each cell across view updates */
let modelts = Hashtbl.create(10);

module PairPalette: PALETTE = {
  let name = "$pair";
  let expansion_ty = HTyp.(Arrow(Sum(Num, Num), Hole));

  type model = (int, int);
  /* TODO actually, not sure about anything i said below. it's hard to reason
     about the interface between coq and reason */
  /* TODO i think maybe init_model, and possibly view, need to be passed in an
     update_hole_data function, that when called on the hole data returned by
     the monad, will add that data to the current hole map (which is empty in the
     initial case).
     the update_hole_data function is a also effectively an rf, so when invoked,
     it will perform some sort of `do_action` on the AST that goes and modifies
     the actual hole_data residing in the actual AST.
     update_hole_data will essentially wrap NatMap.union
     */
  let init_model = update_hole_data => {
    let m_hole_ref =
      HoleRefs.bind(HoleRefs.new_hole_ref(HTyp.Hole), leftID =>
        HoleRefs.bind(HoleRefs.new_hole_ref(HTyp.Hole), rightID =>
          HoleRefs.ret((leftID, rightID))
        )
      );
    let (model, palette_hole_data, _) = HoleRefs.exec(m_hole_ref);

    /* create a Model.t for each cell here, then make sure it's accessible in view */
    let (n1, n2) = model;
    let leftUIModel = Model.new_model();
    let rightUIModel = Model.new_model();
    Hashtbl.add(modelts, n1, leftUIModel);
    Hashtbl.add(modelts, n2, rightUIModel);

    update_hole_data(palette_hole_data);
    /* TODO delete - i think this way of doing things is not necessary
         let update_hole_map = ((m, hole_map), left, right) =>
           switch (NatMap.drop(hole_map, n1)) {
           | Some(hole_map) =>
             switch (NatMap.drop(hole_map, n2)) {
             | Some(hole_map) =>
               let hole_map_1 = NatMap.extend(hole_map, (n1, left));
               let hole_map_2 = NatMap.extend(hole_map_1, (n2, right));
               (m, hole_map_2);
             | None =>
               /* degenerate */
               (m, hole_map)
             }
           | None =>
             /* degenerate */
             (m, hole_map)
           };
         /* TODO now we need to set it up so whenever phd_rs changes, doAction is invoked
            with an action that can edit the appropriate hole_map inside the AST */
         /* see erratique.ch/software/react/doc/React ,section "Mutual and self reference" */
         let fixed = phd => {
           let phd' =
             React.S.l3(update_hole_map, phd, leftUIModel.e_rs, rightUIModel.e_rs);
           (phd', phd');
         };
         let phd_rs = React.S.fix(palette_hole_data, fixed);
       */

    model;
  };
  type model_updater = model => unit;

  let view = (model, model_updater) => {
    /*
       if we create a Model.t during init_model
       then we have to pass the model.t outside of init_model
       cuz init_model won't get called again, the next time view gets called
       when view gets called, it's passed the model.t
       and passes that to chrome.view
     */

    /* We shouldn't need a listener in the view function, since the view should just
       output HTML for the shape, and an HTMLHole for each cell, and when the HTMLHole
       is generated, it maps the e_rs of the corresponding Model.t to a signal that
       will update the corresponding cell in hole_data appropriately. But the view
       code doesn't actually need to be aware of any of this. */
    let _ =
      JSUtil.listen_to(
        Dom_html.Event.input, /* ? maybe Chrome.re has defined a special event for edits*/
        cell_input,
        _ =>
        {}
      );
    (); /* ? maybe Chrome.re has defined a special event for edits*/
  };

  let expand = ((leftID, rightID)) => {
    let to_uhvar = id =>
      UHExp.Tm(NotInHole, UHExp.Var(NotInVHole, HoleRef.to_var(id)));
    let selectorName = "selector";
    UHExp.Tm(
      NotInHole,
      UHExp.Lam(
        selectorName,
        UHExp.Tm(
          NotInHole,
          UHExp.Case(
            UHExp.Tm(NotInHole, UHExp.Var(NotInVHole, selectorName)),
            ("_", to_uhvar(leftID)),
            ("_", to_uhvar(rightID)),
          ),
        ),
      ),
    );
  };

  /* sprintf/sscanf are magical and treat string literals specially -
     attempt to factor out the format string at your own peril */
  let serialize = ((leftID, rightID)) =>
    sprintf("(%d,%d)", leftID, rightID);
  let deserialize = serialized =>
    sscanf(serialized, "(%d,%d)", (leftID, rightID) => (leftID, rightID));
};

module CheckboxPalette: PALETTE = {
  let name = "$checkbox";
  let expansion_ty = HTyp.Sum(HTyp.Num, HTyp.Num);

  type model = bool;
  let init_model = false;
  type model_updater = model => unit;

  let view = (model, model_updater) => {
    let checked_state = model ? [Html5.a_checked()] : [];
    let input_elt =
      Html5.(input(~a=[a_input_type(`Checkbox), ...checked_state], ()));
    let input_dom = Tyxml_js.To_dom.of_input(input_elt);
    let view_span = Html5.(span([input_elt]));
    let _ =
      JSUtil.listen_to(
        Dom_html.Event.input,
        input_dom,
        _ => {
          let is_checked = Js.to_bool(input_dom##.checked);
          model_updater(is_checked);
          Js._true;
        },
      );
    Inline(view_span);
  };

  let dummy_num = UHExp.Tm(NotInHole, UHExp.NumLit(0));
  let true_exp = UHExp.Tm(NotInHole, UHExp.Inj(UHExp.L, dummy_num));
  let false_exp = UHExp.Tm(NotInHole, UHExp.Inj(UHExp.R, dummy_num));
  let expand = model => model ? true_exp : false_exp;

  let serialize = model => model ? "T" : "F";
  let deserialize = serialized =>
    String.equal(serialized, "T") ? true : false;
};

/* overflow paranoia */
let maxSliderValue = 1000 * 1000 * 1000;
let cropSliderValue = value => max(0, min(maxSliderValue, value));

module SliderPalette: PALETTE = {
  let name = "$slider";
  let expansion_ty = HTyp.Num;

  type model = (int, int);
  type model_updater = model => unit;
  let init_model = (5, 10);

  let view = ((value, sliderMax), model_updater) => {
    let curValString = curVal => Printf.sprintf("%d/%d", curVal, sliderMax);
    let changeMaxButton = (desc, f) =>
      Html5.(
        button(
          ~a=[
            a_onclick(_ => {
              let newSliderMax = f(sliderMax);
              let newValue = min(value, newSliderMax);
              model_updater((newValue, newSliderMax));
              true;
            }),
          ],
          [pcdata(desc)],
        )
      );
    let input_elt =
      Html5.(
        input(
          ~a=[
            a_input_type(`Range),
            a_input_min(`Number(0)),
            a_input_max(`Number(cropSliderValue(sliderMax))),
            a_value(string_of_int(cropSliderValue(value))),
          ],
          (),
        )
      );
    let input_dom = Tyxml_js.To_dom.of_input(input_elt);
    let label_elt = Html5.(label([pcdata(curValString(value))]));
    let label_dom = Tyxml_js.To_dom.of_label(label_elt);
    let decrease_range_button_elt =
      changeMaxButton("Max/10", m => max(10, m / 10));
    let increase_range_button_elt =
      changeMaxButton("Max*10", m => cropSliderValue(m * 10));
    let view_span =
      Html5.(
        span([
          decrease_range_button_elt,
          input_elt,
          label_elt,
          increase_range_button_elt,
        ])
      );
    let _ =
      JSUtil.listen_to(
        Dom_html.Event.input,
        input_dom,
        _ => {
          let _ =
            label_dom##.innerHTML :=
              Js.string(
                curValString(
                  int_of_string(Js.to_string(input_dom##.value)),
                ),
              );
          Js._true;
        },
      );
    let _ =
      JSUtil.listen_to(
        Dom_html.Event.change,
        input_dom,
        _ => {
          let newValue =
            cropSliderValue(int_of_string(Js.to_string(input_dom##.value)));
          model_updater((newValue, sliderMax));
          Js._true;
        },
      );
    Inline(view_span);
  };

  let expand = ((value, _)) =>
    UHExp.Tm(NotInHole, UHExp.NumLit(cropSliderValue(value)));

  /* sprintf/sscanf are magical and treat string literals specially -
     attempt to factor out the format string at your own peril */
  let serialize = ((value, sliderMax)) =>
    sprintf("(%d,%d)", value, sliderMax);
  let deserialize = serialized =>
    sscanf(serialized, "(%d,%d)", (value, sliderMax) => (value, sliderMax));
};

/* ----------
   stuff below is infrastructure
   ---------- */

module PaletteDefinition = UHExp.PaletteDefinition;

type model_updater = PaletteSerializedModel.t => unit;
type serialized_view_fn_t =
  (PaletteSerializedModel.t, model_updater) => view_type;

module PaletteViewCtx = {
  type t = VarMap.t_(serialized_view_fn_t);
  include VarMap;
};

module PaletteContexts = {
  type t = (PaletteCtx.t, PaletteViewCtx.t);
  let empty = (PaletteViewCtx.empty, PaletteCtx.empty);
  let extend:
    (t, (PaletteName.t, PaletteDefinition.t, serialized_view_fn_t)) => t =
    ((palette_ctx, palette_view_ctx), (name, defn, view_fn)) => {
      let palette_view_ctx' =
        PaletteViewCtx.extend(palette_view_ctx, (name, view_fn));
      let palette_ctx' = PaletteCtx.extend(palette_ctx, (name, defn));
      (palette_ctx', palette_view_ctx');
    };
};

module PaletteAdapter = (P: PALETTE) => {
  /* generate palette definition for Semantics */
  let palette_defn =
    PaletteDefinition.{
      expansion_ty: P.expansion_ty,
      initial_model: P.serialize(P.init_model),
      to_exp: serialized_model => P.expand(P.deserialize(serialized_model)),
    };

  let serialized_view_fn = (serialized_model, update_fn) =>
    P.view(P.deserialize(serialized_model), model =>
      update_fn(P.serialize(model))
    );

  let contexts_entry = (P.name, palette_defn, serialized_view_fn);
};

module CheckboxPaletteAdapter = PaletteAdapter(CheckboxPalette);
module SliderPaletteAdapter = PaletteAdapter(SliderPalette);

let empty_palette_contexts = PaletteContexts.empty;
let (initial_palette_ctx, initial_palette_view_ctx) =
  PaletteContexts.extend(
    PaletteContexts.extend(
      empty_palette_contexts,
      CheckboxPaletteAdapter.contexts_entry,
    ),
    SliderPaletteAdapter.contexts_entry,
  );
