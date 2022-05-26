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
        [Node.text("- Please rank this option")],
      ),
    ],
    [
      Node.option(
        [Attr.value("None_are_useful")],
        [Node.text("X - This is not a useful option")],
      ),
    ],
    rank_list1(x),
  ]);
};

/* TODO - Hannah - this is used (or something pretty similar) other places and should probably be refactored to somewhere
   centeralized like AssistantView_common - or maybe the different uses are different enough... */
let code_node = text =>
  Node.span([Attr.classes(["code"])], [Node.text(text)]);

let highlight =
    (msg: list(Node.t), steps: CursorPath.steps, mapping: ColorSteps.t)
    : (Node.t, ColorSteps.t) => {
  let (c, mapping) = ColorSteps.get_color(steps, mapping);
  /*print_endline(
      "Color chosen at highlight: ("
      ++ Sexp.to_string(CursorPath.sexp_of_steps(steps))
      ++ ", "
      ++ c
      ++ ")",
    );*/
  (
    Node.span(
      [Attr.style(Css_gen.(create(~field="background-color", ~value=c)))],
      msg,
    ),
    mapping,
  );
};

let _max_elems = 7;
let int_to_word_number = (n: int): string => {
  switch (n) {
  | 1 => "first"
  | 2 => "second"
  | 3 => "third"
  | 4 => "fourth"
  | 5 => "fifth"
  | 6 => "sixth"
  | 7 => "seventh"
  | _ => ""
  };
};
let comma_separated_list = (items: list(string)): string => {
  /*let _ = List.map(item => print_endline(item), items);*/
  let length = List.length(items);
  let items =
    List.mapi(
      (index, item) => {
        let separator =
          if (index == length - 1) {
            length > 2 ? ", and" : " and";
          } else if (index == 0) {
            "";
          } else {
            ",";
          };
        separator ++ " " ++ item;
      },
      items,
    );
  List.fold_left((acc, item) => acc ++ item, "", items);
};

let print_markdown = doc => {
  print_endline("-----------------BEGIN PRINTING------------------");
  let rec print_markdown' = doc => {
    let _ =
      List.mapi(
        (index, element) => {
          print_endline(string_of_int(index));
          switch (element) {
          | Omd.Paragraph(d) =>
            print_endline("------Paragraph---------");
            print_markdown'(d);
          | Ul(_items) => print_endline("Ul")
          | Ulp(_items) => print_endline("Ul  PPPPPP")
          | Text(_) => print_endline("Text")
          | Url(_, d, _) =>
            print_endline("URL");
            print_markdown'(d);
          | Code(_) => print_endline("Code")
          | _ => print_endline("Something else")
          };
        },
        doc,
      );
    ();
  };
  print_markdown'(doc);
  print_endline("---------------------END PRINTING-----------------");
};

/*
 Markdown like thing:
 highlighty thing : [thing to highlight](int indices)
 bulleted list: - list item
                - list item
 code: `code`
 */
let build_msg =
    (text: string, show_highlight: bool): (list(Node.t), ColorSteps.t) => {
  let omd = Omd.of_string(text);
  //print_markdown(omd);
  let rec translate =
          (doc: Omd.t, mapping: ColorSteps.t): (list(Node.t), ColorSteps.t) =>
    List.fold_left(
      ((msg, mapping), elem) => {
        switch (elem) {
        | Omd.Paragraph(d) => translate(d, mapping)
        | Text(t) => (List.append(msg, [Node.text(t)]), mapping)
        | Ul(items) =>
          //print_endline("IN THE LIST THINGY");
          let (bullets, mapping) =
            List.fold_left(
              ((nodes, mapping), d) => {
                let (n, mapping) = translate(d, mapping);
                (List.append(nodes, [Node.li([], n)]), mapping);
              },
              ([], mapping),
              items,
            );
          (List.append(msg, [Node.ul([], bullets)]), mapping); /* TODO Hannah - Should this be an ordered list instead of an unordered list? */
        | Code(_name, t) => (List.append(msg, [code_node(t)]), mapping)
        | Url(path, d, _title) =>
          let (d, mapping) = translate(d, mapping);
          let (inner_msg, mapping) =
            if (show_highlight) {
              let path =
                List.map(
                  int_of_string,
                  Re.Str.split(Re.Str.regexp({| |}), path),
                );
              highlight(d, path, mapping);
            } else {
              (Node.span([], d), mapping);
            };
          (List.append(msg, [inner_msg]), mapping);
        | _ =>
          print_endline("OTHER");
          (msg, mapping);
        }
      },
      ([], mapping),
      doc,
    );
  translate(omd, ColorSteps.empty);
};
