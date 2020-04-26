open Sexplib.Std;

module Dom_html = Js_of_ocaml.Dom_html;
module Dom = Js_of_ocaml.Dom;
module Js = Js_of_ocaml.Js;
module Vdom = Virtual_dom.Vdom;

type div_type = Vdom.Node.t;

type splice_getters_to_vdom =
  (SpliceName.t => div_type, SpliceName.t => option(DHExp.t)) => div_type;

module LivelitView = {
  type t =
    | Inline(div_type, int)
    | MultiLine(splice_getters_to_vdom);
};

module type LIVELIT = {
  let name: LivelitName.t;
  let expansion_ty: HTyp.t;

  [@deriving sexp]
  type model;
  [@deriving sexp]
  type action;
  type trigger = action => Vdom.Event.t;

  let init_model: SpliceGenCmd.t(model);
  let update: (model, action) => SpliceGenCmd.t(model);
  let view: (model, trigger) => LivelitView.t;
  let expand: model => UHExp.t;
};

module PairLivelit: LIVELIT = {
  let name = "$pair";
  let expansion_ty = HTyp.(Prod(Hole, Hole));

  [@deriving sexp]
  type model = (int, int);
  [@deriving sexp]
  type action = unit;
  type trigger = action => Vdom.Event.t;

  let init_model =
    SpliceGenCmd.bind(SpliceGenCmd.new_splice, leftID =>
      SpliceGenCmd.bind(SpliceGenCmd.new_splice, rightID =>
        SpliceGenCmd.return((leftID, rightID))
      )
    );
  let update = (m, _) => SpliceGenCmd.return(m);

  let view = ((leftID, rightID), _) =>
    LivelitView.MultiLine(
      (get_splice_div, _) =>
        Vdom.(
          Node.div(
            [Attr.classes(["pair-livelit"])],
            [get_splice_div(leftID), get_splice_div(rightID)],
          )
        ),
    );

  let expand = ((leftID, rightID)) => {
    let to_uhvar = id =>
      UHExp.(
        Var(NotInHole, NotInVarHole, SpliceInfo.var_of_splice_name(id))
      );
    let pair_seq =
      Seq.mk(to_uhvar(leftID), [(UHExp.Comma, to_uhvar(rightID))]);
    UHExp.Block.wrap'(
      OpSeq.mk(~associate=Associator.Exp.associate, pair_seq),
    );
  };
};

/*
  module ColorPalette: PALETTE = {
    let name = "$color";
    let expansion_ty =
      HTyp.(Arrow(Arrow(Num, Arrow(Num, Arrow(Num, Hole))), Hole));

    type model = string;
    let init_model = UHExp.HoleRefs.Ret("#c94d4d");

    type model_updater = model => unit;

    let colors = [
      "#c94d4d",
      "#d8832b",
      "#dab71f",
      "#446648",
      "#165f99",
      "#242551",
    ];

    let view = (model, model_updater) => {
      let mk_color_elt = (color, selected_color) => {
        let selected = color == selected_color ? ["selected"] : [];
        Html5.(
          div(
            ~a=[
              a_class(["color", ...selected]),
              a_style("background-color:" ++ color),
            ],
            [],
          )
        );
      };

      let color_elts = List.map(c => mk_color_elt(c, model), colors);
      let _ =
        List.map2(
          (c, elt) => {
            let elt_dom = Tyxml_js.To_dom.of_div(elt);
            JSUtil.listen_to(
              Dom_html.Event.click,
              elt_dom,
              _ => {
                model_updater(c);
                Js._true;
              },
            );
          },
          colors,
          color_elts,
        );

      let picker = Html5.(div(~a=[a_class(["color-picker"])], color_elts));
      MultiLine(HTMLWithCells.Ret(picker));
    };

    let expand = rgb_hex => {
      let to_decimal = hex => int_of_string("0x" ++ hex);
      let (r, g, b) =
        sscanf(rgb_hex, "#%.2s%.2s%.2s", (r, g, b) =>
          (to_decimal(r), to_decimal(g), to_decimal(b))
        );
      let fVarName = "f";
      let fPat = UHPat.(Pat(NotInHole, Var(fVarName)));
      let r_num = UHExp.(Tm(NotInHole, NumLit(r)));
      let g_num = UHExp.(Tm(NotInHole, NumLit(g)));
      let b_num = UHExp.(Tm(NotInHole, NumLit(b)));
      let body =
        UHExp.(
          Seq.(
            operand_op_seq(
              Tm(NotInHole, Var(NotInVarHole, fVarName)),
              Space,
              operand_op_seq(r_num, Space, ExpOpExp(g_num, Space, b_num)),
            )
          )
        );
      UHExp.(
        Tm(
          NotInHole,
          Lam(
            fPat,
            None,
            Tm(NotInHole, OpSeq(Associator.Exp.associate(body), body)),
          ),
        )
      );
    };

    let serialize = model => model;
    let deserialize = serialized => serialized;
  };
 */

module CheckboxLivelit: LIVELIT = {
  let name = "$checkbox";
  let expansion_ty = HTyp.Bool;

  [@deriving sexp]
  type model = bool;
  [@deriving sexp]
  type action =
    | Toggle;
  type trigger = action => Vdom.Event.t;

  let init_model = SpliceGenCmd.return(false);
  let update = (m, Toggle) => SpliceGenCmd.return(!m);

  let view = (m, trig) => {
    let checked_state = m ? [Vdom.Attr.checked] : [];
    let input_elt =
      Vdom.(
        Node.input(
          [
            Attr.type_("checkbox"),
            Attr.on_input((_, _) => trig(Toggle)),
            ...checked_state,
          ],
          [],
        )
      );
    let view_span = Vdom.Node.span([], [input_elt]);
    LivelitView.Inline(view_span, /* TODO! */ 1);
  };

  let expand = m => UHExp.Block.wrap(UHExp.BoolLit(NotInHole, m));
};

module SliderLivelit: LIVELIT = {
  let name = "$slider";
  let expansion_ty = HTyp.Num;

  [@deriving sexp]
  type model = (int, int);
  [@deriving sexp]
  type action =
    | Slide(int)
    | UpdateMax(int);
  type trigger = action => Vdom.Event.t;

  let init_model = SpliceGenCmd.return((5, 10));
  let update = ((n, max), a) =>
    switch (a) {
    | Slide(n') => SpliceGenCmd.return((n', max))
    | UpdateMax(max') => SpliceGenCmd.return((n, max'))
    };

  /* overflow paranoia */
  let max_slider_value = 1000 * 1000 * 1000;
  let crop_slider_value = value => max(0, min(max_slider_value, value));

  let view = ((value, slider_max), trigger: trigger) => {
    let view_span =
      Vdom.(
        Node.span(
          [],
          [
            Node.input(
              [
                Attr.type_("range"),
                Attr.create("min", "0"),
                Attr.create("max", string_of_int(slider_max)),
                Attr.value(string_of_int(value)),
                Attr.on_input((_, value_str) => {
                  let new_value = int_of_string(value_str);
                  trigger(Slide(new_value));
                }),
              ],
              [],
            ),
            Node.label(
              [],
              [Node.text(Printf.sprintf("%d/%d", value, slider_max))],
            ),
            Node.button(
              [
                Attr.on_click(_ => {
                  let new_slider_max = max(10, slider_max / 10);
                  let new_value = min(value, new_slider_max);
                  Event.Many([
                    trigger(UpdateMax(new_slider_max)),
                    trigger(Slide(new_value)),
                  ]);
                }),
              ],
              [Node.text("/ 10")],
            ),
            Node.button(
              [
                Attr.on_click(_ => {
                  let new_slider_max = crop_slider_value(slider_max * 10);
                  let new_value = min(value, new_slider_max);
                  Event.Many([
                    trigger(UpdateMax(new_slider_max)),
                    trigger(Slide(new_value)),
                  ]);
                }),
              ],
              [Node.text("* 10")],
            ),
          ],
        )
      );
    LivelitView.Inline(view_span, 10);
  };

  let expand = ((value, _)) => UHExp.Block.wrap(UHExp.numlit(value));
};

/* ----------
   stuff below is infrastructure
   ---------- */

type trigger_serialized = SerializedAction.t => Vdom.Event.t;
type serialized_view_fn_t =
  (SerializedModel.t, trigger_serialized) => LivelitView.t;

module LivelitViewCtx = {
  type t = VarMap.t_(serialized_view_fn_t);
  include VarMap;
};

module LivelitContexts = {
  type t = (LivelitCtx.t, LivelitViewCtx.t);
  let empty = (LivelitCtx.empty, LivelitViewCtx.empty);
  let extend =
      ((livelit_ctx, livelit_view_ctx), (name, def, serialized_view_fn)) => {
    if (!LivelitName.is_valid(name)) {
      failwith("Invalid livelit name " ++ name);
    };
    (
      VarMap.extend(livelit_ctx, (name, def)),
      VarMap.extend(livelit_view_ctx, (name, serialized_view_fn)),
    );
  };
};

module LivelitAdapter = (L: LIVELIT) => {
  let serialize_monad = model => SpliceGenCmd.return(L.sexp_of_model(model));

  /* generate palette definition for Semantics */
  let livelit_defn =
    LivelitDefinition.{
      expansion_ty: L.expansion_ty,
      init_model: SpliceGenCmd.bind(L.init_model, serialize_monad),
      update: (serialized_model, serialized_action) =>
        SpliceGenCmd.bind(
          L.update(
            L.model_of_sexp(serialized_model),
            L.action_of_sexp(serialized_action),
          ),
          serialize_monad,
        ),
      expand: serialized_model =>
        L.expand(L.model_of_sexp(serialized_model)),
    };

  let serialized_view_fn = (serialized_model, update_fn) =>
    L.view(L.model_of_sexp(serialized_model), action =>
      update_fn(L.sexp_of_action(action))
    );

  let contexts_entry = (L.name, livelit_defn, serialized_view_fn);
};

module CheckboxLivelitAdapter = LivelitAdapter(CheckboxLivelit);
module PairLivelitAdapter = LivelitAdapter(PairLivelit);
module SliderLivelitAdapter = LivelitAdapter(SliderLivelit);
let empty_livelit_contexts = LivelitContexts.empty;
let (initial_livelit_ctx, initial_livelit_view_ctx) =
  LivelitContexts.extend(
    LivelitContexts.extend(
      LivelitContexts.extend(
        empty_livelit_contexts,
        PairLivelitAdapter.contexts_entry,
      ),
      CheckboxLivelitAdapter.contexts_entry,
    ),
    SliderLivelitAdapter.contexts_entry,
  );
