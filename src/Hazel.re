open Incr_dom;

// https://github.com/janestreet/incr_dom/blob/master/src/app_intf.ml
module Model = MyModel;
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
    model,
    MyView.mk(~inject, model),
  );
};
