open Semantics.Core;
open Tyxml_js;

type view_type = Tyxml_js.Html5.elt([ Html_types.span]);

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
    view_span;
  };

  let dummy_num = UHExp.Tm(NotInHole, UHExp.NumLit(0));
  let true_exp = UHExp.Tm(NotInHole, UHExp.Inj(UHExp.L, dummy_num));
  let false_exp = UHExp.Tm(NotInHole, UHExp.Inj(UHExp.R, dummy_num));
  let expand = model => model ? true_exp : false_exp;

  let serialize = model => model ? "T" : "F";
  let deserialize = serialized =>
    String.equal(serialized, "T") ? true : false;
};

/* ----------
   stuff below is infrastructure
   ---------- */

module PaletteDefinition = UHExp.PaletteDefinition;

type model_updater = PaletteSerializedModel.t => unit;
type serialized_view_fn =
  (PaletteSerializedModel.t, model_updater) => view_type;

module PaletteViewCtx = {
  type t = VarMap.t_(serialized_view_fn);
  include VarMap;
};

module PaletteContexts = {
  type t = (PaletteCtx.t, PaletteViewCtx.t);
  let empty = (PaletteViewCtx.empty, PaletteCtx.empty);
  let extend:
    (t, (PaletteName.t, PaletteDefinition.t, serialized_view_fn)) => t =
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

let empty_palette_contexts = PaletteContexts.empty;
let (initial_palette_ctx, initial_palette_view_ctx) =
  PaletteContexts.extend(
    empty_palette_contexts,
    CheckboxPaletteAdapter.contexts_entry,
  );
