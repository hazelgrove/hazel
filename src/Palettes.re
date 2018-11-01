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

/*
 let model = Model.new_model();
 let parent = JSUtil.forceGetElementById("container");
 let (chrome, set_cursor) = Chrome.view(model, true);
 Dom.appendChild(parent, chrome);
 set_cursor();
 */

module PairPalette: PALETTE = {
  let name = "$pair";
  let expansion_ty = HTyp.(Arrow(Sum(Num, Num), Hole));

  /* I don't hink we need to pass around the hole_data after all - see how
       expand is defined And if we did have it in teh model, we'd have to
       serrialize it. But to do that, the serialize function would have to invoke
       the serialize functions of any palettes inside of the cell, and also be able
       to serialize generic HExps. This is definitely beyond the scope of what
       serialize is meant to do. I think it should only serialize shape, since
       the AST stores all the hole_data directly.
       That said, we do need the left and right IDs so that we know how to refer
       to the correct cells - see the definition of expand.
     */
  type model = (int, int);
  let init_model = {
    let m_hole_ref =
      HoleRefs.bind(HoleRefs.new_hole_ref(HTyp.Hole), leftID =>
        HoleRefs.bind(HoleRefs.new_hole_ref(HTyp.Hole), rightID =>
          HoleRefs.ret((leftID, rightID))
        )
      );
    let (model, palette_hole_data, _) = HoleRefs.exec(m_hole_ref);
    /* TODO */
    /* somehow hook up signals from leftModel and rightModel to the hole refs */
    model;
  };
  type model_updater = model => unit;

  let view = (model, model_updater) => {
    /* I think instead of explicitly calling Model.newModel and Chrome.view,
       we should have a HTMLHoles generator, which takes care of all this stuff
       for us. There is also a cyclic dependency we'll eventually have to deal
       eith: Palttes depend on Chrome and Model but those depend on the
       initial palette context which in turn depends on the particular paleettes.
       */
    let leftUIModel = Model.new_model();
    let rightUIModel = Model.new_model();
    let (leftCell, leftSetCursor) = Chrome.view(leftUIModel, false);
    let (rightCell, _) = Chrome.view(rightUIModel, false);
    /* I think there is only one cursor so we can only set it once? */
    leftSetCursor();
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
