open Virtual_dom.Vdom;
open Node;
open Editors;
let img = create("img");

let get_content =
  fun
  | Scratch(0, _) =>
    Some(
      img(
        ~key="slide",
        ~attr=
          Attr.many([
            Attr.src("img/slides/forest_1.jpeg"),
            Attr.class_("slide-img"),
          ]),
        [],
      ),
    )
  | Examples("Introduction", _) => Some(div([h1([text("Title Test")])]))
  | _ => None;
