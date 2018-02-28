open Semantics.Core;

open Tyxml_js;

let type_width = 80000;

let environment_entry (x, ty) => {
  let ty_html = View.html_of_ty type_width ("environment-" ^ x) ty;
  Html5.(
    div
      a::[a_class ["environment-entry"]]
      [div a::[a_class ["code"]] [span a::[a_class ["var"]] [pcdata x], pcdata " : ", ty_html]]
  )
};

let of_cursor_info {ZExp.mode: mode, ZExp.form: form, ZExp.ctx: ctx} => {
  let ctx_list = Ctx.to_list ctx;
  let the_environment =
    Html5.(div a::[a_class ["the-environment"]] (List.map environment_entry ctx_list));
  Html5.(div [PanelUtils.titlebar "Environment", the_environment])
};

let mk (cursor_info_rs: Model.cursor_info_rs) => {
  let context_inspector_rs =
    React.S.map (fun cursor_info => [of_cursor_info cursor_info]) cursor_info_rs;
  R.Html5.(
    div
      a::[Html5.a_class ["panel", "context-inspector-panel"]]
      (ReactiveData.RList.from_signal context_inspector_rs)
  )
};
