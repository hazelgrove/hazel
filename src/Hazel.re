open Tyxml_js;
JSUtil.listen_to_t(
  Dom_html.Event.domContentLoaded,
  Dom_html.document,
  _ => {
    let model = Model.new_model();
    let parent = JSUtil.forceGetElementById("container");
    let (chrome, set_cursor) = Chrome.view(model);
    Dom.appendChild(parent, chrome);
    set_cursor();
  },
);
