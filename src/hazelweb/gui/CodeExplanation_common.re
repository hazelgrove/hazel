open Virtual_dom.Vdom;

// just draws the ranking
let rank_list = x => {
  List.init(x, i =>
    Node.option(
      [Attr.value(string_of_int(i))],
      [Node.text(string_of_int(i))],
    )
  );
};
