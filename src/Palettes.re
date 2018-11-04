open Semantics.Core;
open Tyxml_js;
open Printf;
open Scanf;

type div_type = Html5.elt(Html_types.div);

module HTMLWithCells = {
  type m_html_with_cells =
    | NewCellFor(int)
    | Bind(m_html_with_cells, div_type => m_html_with_cells)
    | Ret(div_type);

  let rec resolve = (view_monad, hole_map, mk_html_cell) =>
    switch (view_monad) {
    | NewCellFor(id') =>
      switch (NatMap.lookup(hole_map, id')) {
      | Some((_, hexp)) => mk_html_cell(hexp)
      | None => Html5.(div(~a=[a_class(["inline-div"])], []))
      }
    | Bind(in_monad, f) =>
      let in_html = resolve(in_monad, hole_map, mk_html_cell);
      resolve(f(in_html), hole_map, mk_html_cell);
    | Ret(v) => v
    };
};

type view_type =
  | Inline(Html5.elt([ Html_types.span]))
  | MultiLine(HTMLWithCells.m_html_with_cells);

module type PALETTE = {
  let name: string;
  let expansion_ty: HTyp.t;

  type model;
  let init_model: UHExp.HoleRefs.m_hole_ref(model);

  type model_updater = model => unit;
  /* model_updater must _not_ be invoked until well after view has completed */
  let view: (model, model_updater) => view_type;

  let expand: model => UHExp.t;

  let serialize: model => PaletteSerializedModel.t;
  let deserialize: PaletteSerializedModel.t => model;
};

module NumPairPalette: PALETTE = {
  let name = "$numpair";
  /* let expansion_ty = HTyp.(Arrow(Arrow(Num, Arrow(Num, Num)), Num)); */
  let expansion_ty = HTyp.Num;

  type model = (int, int);
  let init_model =
    UHExp.HoleRefs.bind(UHExp.HoleRefs.new_hole_ref(HTyp.Num), left_hole_ref =>
      UHExp.HoleRefs.bind(
        UHExp.HoleRefs.new_hole_ref(HTyp.Num),
        right_hole_ref => {
          let leftID = UHExp.HoleRefs.lbl_of(left_hole_ref);
          let rightID = UHExp.HoleRefs.lbl_of(right_hole_ref);
          UHExp.HoleRefs.ret((leftID, rightID));
        },
      )
    );
  type model_updater = model => unit;

  let view = (model, model_updater) =>
    MultiLine(
      HTMLWithCells.Ret(Html5.(div(~a=[a_class(["inline-div"])], []))),
    );

  let expand = ((leftID, rightID)) => UHExp.(Tm(NotInHole, NumLit(leftID))) /*
  {
    let to_uhvar = (id) => UHExp.(Tm(NotInHole, Var(NotInVHole, Helper.Helper.to_var(id))));
    let fVarName = "f";
    let apOpSeq =
      UHExp.(
        OperatorSeq.(
          exp_op_seq(
            Tm(NotInHole, Var(NotInVHole, fVarName)),
            Space,
            ExpOpExp(
              Tm(NotInHole, to_uhvar(leftID)),
              Space,
              Tm(NotInHole, to_uhvar(rightID)),
            ),
          )
        )
      );
    UHExp.(
      Tm(
        NotInHole,
        Lam(
          fVarName,
          Tm(
            NotInHole,
            UHExp.OpSeq(Associator.associate_exp(apOpSeq), apOpSeq),
          ),
        ),
      )
    );
  }*/;

  /* sprintf/sscanf are magical and treat string literals specially -
     attempt to factor out the format string at your own peril */
  let serialize = ((leftID, rightID)) =>
    sprintf("(%d,%d)", leftID, rightID);
  let deserialize = serialized =>
    sscanf(serialized, "(%d,%d)", (leftID, rightID) => (leftID, rightID));
};

module ColorPalette: PALETTE = {
  let name = "$color";
  let expansion_ty = HTyp.Num;

  type model = (int, int, int); /* RGB */
  let init_model = UHExp.HoleRefs.ret((7, 63, 36));

  type model_updater = model => unit;

  let hex_to_model = hex => {
    let (r, g, b) = sscanf(hex, "#%.2s%.2s%.2s", (r, g, b) => (r, g, b));
    let to_decimal = hex => int_of_string("0x" ++ hex);
    (to_decimal(r), to_decimal(g), to_decimal(b));
  };

  let model_to_hex = ((r, g, b)) => {
    let to_hex = dec => sprintf("%.2x", dec);
    "#" ++ to_hex(r) ++ to_hex(g) ++ to_hex(b);
  };

  let view = (model, model_updater) => {
    let value = model_to_hex(model);
    JSUtil.log("value = " ++ value);
    let input_elt =
      Html5.(input(~a=[a_input_type(`Color), a_value(value)], ()));
    let input_dom = Tyxml_js.To_dom.of_input(input_elt);
    let view_div = Html5.(div(~a=[a_class(["inline-div"])], [input_elt]));
    let _ =
      JSUtil.listen_to(
        Dom_html.Event.change,
        input_dom,
        _ => {
          let hex = Js.to_string(input_dom##.value);
          JSUtil.log("hex = " ++ hex);
          let new_model = hex_to_model(hex);
          model_updater(new_model);
          Js._true;
        },
      );
    MultiLine(HTMLWithCells.Ret(view_div));
  };

  let expand = ((r, g, b)) =>
    UHExp.Tm(NotInHole, UHExp.NumLit(1000000 * r + 1000 * g + b));

  let serialize = ((r, g, b)) => sprintf("(%d,%d,%d)", r, g, b);
  let deserialize = serialized =>
    sscanf(serialized, "(%d,%d,%d)", (r, g, b) => (r, g, b));
};

module CheckboxPalette: PALETTE = {
  let name = "$checkbox";
  let expansion_ty = HTyp.Sum(HTyp.Num, HTyp.Num);

  type model = bool;
  let init_model = UHExp.HoleRefs.ret(false);
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
  let init_model = UHExp.HoleRefs.ret((5, 10));

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
      initial_model:
        UHExp.HoleRefs.bind(P.init_model, model =>
          UHExp.HoleRefs.ret(P.serialize(model))
        ),
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
module ColorPaletteAdapter = PaletteAdapter(ColorPalette);
module NumPairPaletteAdapter = PaletteAdapter(NumPairPalette);

let empty_palette_contexts = PaletteContexts.empty;
let (initial_palette_ctx, initial_palette_view_ctx) =
  PaletteContexts.extend(
    PaletteContexts.extend(
      PaletteContexts.extend(
        PaletteContexts.extend(
          empty_palette_contexts,
          CheckboxPaletteAdapter.contexts_entry,
        ),
        SliderPaletteAdapter.contexts_entry,
      ),
      ColorPaletteAdapter.contexts_entry,
    ),
    NumPairPaletteAdapter.contexts_entry,
  );
