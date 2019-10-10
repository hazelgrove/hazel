module Vdom = Virtual_dom.Vdom;

type t = {
  header: Vdom.Node.t,
  caption: Vdom.Node.t,
  init_zblock: ZExp.zblock,
};

let hazel_logo =
  Vdom.(
    Node.a(
      [Attr.classes(["logo-text"]), Attr.href("https://hazel.org")],
      [Node.text("Hazel")],
    )
  );

let mk =
    (
      ~header=Vdom.Node.span([], [hazel_logo]),
      ~caption=Vdom.Node.div([], []),
      init_zblock: ZExp.zblock,
    )
    : t => {
  header,
  caption,
  init_zblock,
};
