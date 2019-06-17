module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
JSUtil.listen_to_t(
  Dom_html.Event.domContentLoaded,
  Dom_html.document,
  _ => {
    let model = MyModel.new_init();
    let parent = JSUtil.forceGetElementById("container");
    let (chrome, set_cursor) = MyView.of(model);
    Dom.appendChild(parent, chrome);
    set_cursor();
  },
);
