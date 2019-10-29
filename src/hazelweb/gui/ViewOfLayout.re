module Vdom = Virtual_dom.Vdom;

let contenteditable_of_layout: Layout.t('tag) => Vdom.Node.t =
  layout => {
    let record: Layout.text('tag, list(Vdom.Node.t), Vdom.Node.t) = {
      imp_of_string: string => [Vdom.Node.text(string)],
      imp_of_tag: (_, string) => [Vdom.Node.span([], string)], // TODO: add span data
      imp_append: (s1, s2) => s1 @ s2,
      imp_newline: [Vdom.Node.br([])],
      t_of_imp: s => Vdom.Node.span([], s) // TODO: use something other than `span`?
    };
    Layout.make_of_layout(record, layout);
  };
