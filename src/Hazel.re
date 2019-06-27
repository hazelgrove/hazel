module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
open Incr_dom;

// https://github.com/janestreet/incr_dom/blob/master/src/app_intf.ml

module Model = Model;
module Action = Update.Action;
module State = {
  type t = unit;
};

[@warning "-27"]
let on_startup = (~schedule_action, _) => Async_kernel.return();

[@warning "-27"]
let create = (model, ~old_model, ~inject) => {
  open Incr.Let_syntax;
  let%map model = model;

  Component.create(
    ~apply_action=Update.apply_action(model),
    ~on_display=
      (_, ~schedule_action) => Code.set_caret(Model.get_path(model)),
    model,
    Page.view(~inject, model),
  );
};
