open Semantics.Core;
open Tyxml_js;
open Printf;
open Scanf;

/* Hole Ref Generator */
module HoleRefGenerator

/* Hole Refs in HTML */
let x = Html5.(span(~a=[a_id("hole_ref_" ++ hr.hole_ref_label), a_class(["holeRef"])], []));
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

let init_model = {
  let leftModel = Model.new_model();
  let rightModel = Model.new_model();
  let (leftCell, leftSet) = Chrome.view(leftModel, false);
  let (rightCell, rightSet) = Chrome.view(rightModel, false);
  leftSet();
  rightSet();
  /* monad stuff to generate actual hole refs */
  /* somehow hook up signals from leftModel and rightModel to the hole refs */
  /* return the monad return value */
}

let view = (model, model_updater) => {
  let (cell_input, set_cursor) = Chrome.view(model, false);
  let _ =
    JSUtil.listen_to(
      Dom_html.Event.input, /* ? maybe Chrome.re has defined a special event for edits*/
      cell_input,
      _ => {

      }

    )
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
