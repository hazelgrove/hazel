open Semantics.Core;
open Tyxml_js;

type view_type = Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.divElement);

module BooleanPalette = {
  /* name */
  let name = "$checkbox";

  /* expansion type */
  let bool_ty: HTyp.t = HTyp.Sum(HTyp.Num, HTyp.Num); /* TODO change to unit */

  /* model */
  type model = bool;
  let init_model = false;
  let (model_rs, model_rf) = React.S.create(init_model);

  /* view */
  let listen_to = (ev, elem, f) =>
    Dom_html.addEventListener(elem, ev, Dom_html.handler(f), Js._false);
  let view: React.signal(view_type) =
    React.S.l1(
      model => {
        let input_elt =
          Html5.(
            input(~a=[a_input_type(`Checkbox), a_value("checked")], ())
          );
        let input_dom = Tyxml_js.To_dom.of_input(input_elt);
        let view_div = Html5.(div([input_elt]));
        let view_dom = Tyxml_js.To_dom.of_div(view_div);
        let _ =
          listen_to(
            Dom_html.Event.input,
            view_dom,
            _ => {
              let value = input_dom##.value;
              let value_str = Js.to_string(value);
              let new_model = String.equal(value_str, "checked");
              model_rf(new_model);
              Js._true;
            },
          );
        view_dom;
      },
      model_rs,
    );

  /* to_exp */
  let dummy_num: UHExp.t = UHExp.Tm(NotInHole, UHExp.NumLit(0));
  let true_exp: UHExp.t = UHExp.Tm(NotInHole, UHExp.Inj(UHExp.L, dummy_num));
  let false_exp: UHExp.t =
    UHExp.Tm(NotInHole, UHExp.Inj(UHExp.R, dummy_num));
  let to_hexp: model => UHExp.t =
    model => if (model) {true_exp} else {false_exp};

  /* serialization and deserialization */
  let serialize: model => PaletteSerializedModel.t =
    model => if (model) {"T"} else {"F"};
  let deserialize: PaletteSerializedModel.t => model =
    serialized =>
      if (String.equal(serialized, "T")) {
        true;
      } else {
        false;
      };

  /* generate palette definition for Semantics */
  let palette_defn =
    UHExp.PaletteDefinition.{
      expansion_ty: bool_ty,
      initial_model: serialize(init_model),
      to_exp: serialized_model => to_hexp(deserialize(serialized_model)),
    };
};

let initial_palette_ctx: PaletteCtx.t =
  PaletteCtx.extend(
    PaletteCtx.empty,
    (BooleanPalette.name, BooleanPalette.palette_defn),
  );
