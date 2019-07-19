module Vdom = Virtual_dom.Vdom;

let cardstack: CardStack.t =
  Vdom.[
    {caption: Node.span([], [Node.text("Test 1")])},
    {caption: Node.span([], [Node.text("Test 2")])},
  ];
