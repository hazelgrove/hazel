open Virtual_dom.Vdom;

// just draws the ranking

let rank_list1 = x => {
  List.init(x, i =>
    Node.option(
      [Attr.value(string_of_int(i))],
      [Node.text(string_of_int(i))],
    )
  );
};

let rank_list = x => {
  List.flatten([
    [
      Node.option(
        [Attr.value("not_selected"), Attr.selected],
        [Node.text("Please rank this option")],
      ),
    ],
    [
      Node.option(
        [Attr.value("None_are_useful")],
        [Node.text("This is not a useful option")],
      ),
    ],
    rank_list1(x),
  ]);
};
