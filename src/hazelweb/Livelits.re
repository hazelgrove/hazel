open Sexplib.Std;

module Dom_html = Js_of_ocaml.Dom_html;
module Dom = Js_of_ocaml.Dom;
module Js = Js_of_ocaml.Js;
module Vdom = Virtual_dom.Vdom;

type div_type = Vdom.Node.t;

module type VdomWithSplices = {
  include Monads.MONAD;

  type splice_getters = {
    get_splice_div: SpliceName.t => div_type,
    get_splice_value: SpliceName.t => option(DHExp.t),
  };

  let get_splice_div: SpliceName.t => t(div_type);
  let get_splice_value: SpliceName.t => t(option(DHExp.t));
  let exec: (t('a), splice_getters) => 'a;
};

module VdomWithSplices: VdomWithSplices = {
  type splice_getters = {
    get_splice_div: SpliceName.t => div_type,
    get_splice_value: SpliceName.t => option(DHExp.t),
  };

  [@deriving sexp]
  type t('a) = splice_getters => ('a, splice_getters);

  let get_splice_div = (name, {get_splice_div, _} as sg) => (
    get_splice_div(name),
    sg,
  );
  let get_splice_value = (name, {get_splice_value, _} as sg) => (
    get_splice_value(name),
    sg,
  );

  let bind = (m, f, sg) => {
    let (x, sg) = m(sg);
    f(x, sg);
  };
  let return = (m, sg) => (m, sg);

  let exec = (m, sg) => {
    let (x, _) = m(sg);
    x;
  };
};

module LivelitView = {
  type t =
    | Inline(div_type, int)
    | MultiLine(VdomWithSplices.t(div_type));
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
      VdomWithSplices.(
        bind(get_splice_div(leftID), left_splice_view =>
          bind(get_splice_div(rightID), right_splice_view =>
            return(
              Vdom.(
                Node.div(
                  [Attr.classes(["pair-livelit"])],
                  [left_splice_view, right_splice_view],
                )
              ),
            )
          )
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
  let update = (m, _) => SpliceGenCmd.return(!m);

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

/*

 /* overflow paranoia */
 let maxSliderValue = 1000 * 1000 * 1000;
 let cropSliderValue = value => max(0, min(maxSliderValue, value));

 module SliderPalette: PALETTE = {
   let name = "$slider";
   let expansion_ty = HTyp.Num;

   type model = (int, int);
   type model_updater = model => unit;
   let init_model = UHExp.HoleRefs.Ret((5, 10));

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
           [txt(desc)],
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
     let label_elt = Html5.(label([txt(curValString(value))]));
     let label_dom = Tyxml_js.To_dom.of_label(label_elt);
     let decrease_range_button_elt =
       changeMaxButton("/ 10", m => max(10, m / 10));
     let increase_range_button_elt =
       changeMaxButton("* 10", m => cropSliderValue(m * 10));
     let view_span =
       Html5.(
         span([
           input_elt,
           label_elt,
           decrease_range_button_elt,
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
 */
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
      update: (serialized_action, serialized_model) =>
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
let empty_livelit_contexts = LivelitContexts.empty;
let (initial_livelit_ctx, initial_livelit_view_ctx) =
  LivelitContexts.extend(
    LivelitContexts.extend(
      empty_livelit_contexts,
      PairLivelitAdapter.contexts_entry,
    ),
    CheckboxLivelitAdapter.contexts_entry,
  );
