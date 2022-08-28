open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Core;

/* TODO - Hannah - this is used (or something pretty similar) other places and should probably be refactored to somewhere
   centeralized like AssistantView_common - or maybe the different uses are different enough... */
let code_node = text =>
  Node.span([Attr.classes(["code"])], [Node.text(text)]);

let highlight =
    (msg: list(Node.t), id: Core.Id.t, mapping: ColorSteps.t)
    : (Node.t, ColorSteps.t) => {
  let (c, mapping) = ColorSteps.get_color(id, mapping);
  /*print_endline(
      "Color chosen at highlight: ("
      ++ Sexp.to_string(CursorPath.sexp_of_steps(steps))
      ++ ", "
      ++ c
      ++ ")",
    );*/
  (Node.span([Attr.classes(["highlight-" ++ c])], msg), mapping);
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
 highlighty thing : [thing to highlight](id)
 bulleted list: - list item
                - list item
 code: `code`
 italics: *word*
 */
let mk_explanation =
    (text: string, show_highlight: bool): (Node.t, ColorSteps.t) => {
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
        | Url(id, d, _title) =>
          let (d, mapping) = translate(d, mapping);
          let (inner_msg, mapping) =
            if (show_highlight) {
              let id = int_of_string(id);
              highlight(d, id, mapping);
            } else {
              (Node.span([], d), mapping);
            };
          (List.append(msg, [inner_msg]), mapping);
        | Emph(d) =>
          let (d, mapping) = translate(d, mapping);
          (
            List.append(
              msg,
              [
                Node.span(
                  [
                    Attr.style(
                      Css_gen.create(~field="font-style", ~value="italic"),
                    ),
                  ],
                  d,
                ),
              ],
            ),
            mapping,
          );
        | _ =>
          print_endline("OTHER");
          (msg, mapping);
        }
      },
      ([], mapping),
      doc,
    );
  let (msg, color_map) = translate(omd, ColorSteps.empty);
  (div([clss(["explanation-contents"])], msg), color_map);
};

let deco = (~colorings, ~expandable, ~zipper, ~map, ~inject, ~font_metrics) => {
  module Deco =
    Deco.Deco({
      let font_metrics = font_metrics;
      let map = map;
      let show_backpack_targets = false;
    });
  let color_highlight =
    List.map(
      ((id, color)) =>
        Deco.term_highlight(
          ~ids=[id],
          ~clss=["highlight-code-" ++ color],
          zipper,
        ),
      colorings,
    )
    |> List.flatten;
  let expandable_highlight =
    Deco.term_highlight(~ids=expandable, ~clss=["expandable"], zipper);
  let _ = inject;
  color_highlight @ expandable_highlight;
};

let syntactic_form_view =
    (
      ~colorings,
      ~expandable,
      ~inject,
      ~font_metrics,
      ~unselected,
      ~settings,
      ~id,
      zipper,
    ) => {
  let map = Measured.of_segment(unselected);
  let code_view = Code.simple_view(~unselected, ~map, ~settings);
  let deco_view =
    deco(~colorings, ~expandable, ~zipper, ~map, ~inject, ~font_metrics);
  div(
    [
      Attr.id(id),
      Attr.class_("code-container"),
      Attr.on_click(_ => {
        print_endline("CLICKED!");
        Event.Ignore;
      }),
    ],
    [code_view] @ deco_view,
  );
};

let get_doc =
    (
      ~inject,
      ~font_metrics,
      ~settings: Model.settings,
      info: option(Core.Statics.t),
    ) => {
  let default = (
    text("No syntactic form available"),
    (text("No explanation available"), ColorSteps.empty),
    text("No examples available"),
  );
  switch (info) {
  | Some(InfoExp({term, _})) =>
    switch (term.term) {
    | Invalid(_) => default
    | EmptyHole => default
    | MultiHole(_ids, _uexps) => default
    | Triv => default
    | Bool(_bool_lit) => default
    | Int(_int_lit) => default
    | Float(_float_lit) => default
    | ListLit(_ids, _uexps) => default
    | Fun(_pat, _body) => default
    | Tuple(_ids, _uexps) => default
    | Var(_var) => default
    | Let(_pat, _def, _body) => default
    | Ap(_fun, _arg) => default
    | If(_cond, _then, _else) => default
    | Seq(_seq1, _seq2) => default
    | Test(_uexp) => default
    | Parens(_uexp) => default
    | Cons(hd, tl) =>
      /* PAUSE POINT - I think what I want to do is more this way...
         Then add decorations to indicate the non-terminals in the
         syntactic form and the decorations can have actions? */
      /*let (zipper, _) = Option.get(Printer.zipper_of_string(0, "hd::tl"));
        let syntactic_form = Zipper.unselect_and_zip(zipper);
        print_endline(
          Sexplib.Sexp.to_string(Segment.sexp_of_t(syntactic_form)),
        );*/
      let cons = Example.mk_monotile(Form.get("cons_exp"));
      let exp = v =>
        Example.mk_monotile(Form.mk(Form.ss, [v], Mold.(mk_op(Exp, []))));
      let left = exp("<hd>");
      let right = exp("<tl>");
      let syntactic_form = [left, cons, right];
      let zipper: Zipper.t = {
        selection: {
          focus: Left,
          content: syntactic_form,
        },
        backpack: [],
        relatives: {
          siblings: ([], []),
          ancestors: [],
        },
        caret: Outer,
        caret_col_target: 0,
      };
      let (explanation, color_map) =
        mk_explanation(
          "Cons operator to make list with [*head*]("
          ++ string_of_int(hd.id)
          ++ ") and [*tail*]("
          ++ string_of_int(tl.id)
          ++ ")",
          true,
        );
      let (left_color, _) = ColorSteps.get_color(hd.id, color_map);
      let (right_color, _) = ColorSteps.get_color(tl.id, color_map);
      let syntactic_form_view =
        syntactic_form_view(
          ~colorings=[
            (Piece.id(left), left_color),
            (Piece.id(right), right_color),
          ],
          ~expandable=[Piece.id(right)],
          ~inject,
          ~font_metrics,
          ~unselected=syntactic_form,
          ~settings,
          ~id="syntactic-form-code",
          zipper,
        );
      (
        syntactic_form_view,
        (explanation, color_map),
        text("No examples available"),
      );
    | UnOp(_op, _uexp) => default
    | BinOp(_op, _left, _right) => default
    | Match(_rule_ids, _scrut, _rule_exps) => default
    }
  | Some(InfoPat(_))
  | Some(InfoTyp(_))
  | Some(InfoRul(_))
  | None
  | Some(Invalid(_)) => default
  };
};

let section = (~section_clss: string, ~title: string, contents: Node.t) =>
  div(
    [clss(["section", section_clss])],
    [div([clss(["section-title"])], [text(title)]), contents],
  );

let get_color_map =
    (
      ~inject,
      ~font_metrics,
      ~settings: Model.settings,
      index': option(int),
      info_map: Core.Statics.map,
    ) => {
  let info: option(Core.Statics.t) =
    switch (index') {
    | Some(index) =>
      switch (Core.Id.Map.find_opt(index, info_map)) {
      | Some(ci) => Some(ci)
      | None => None
      }
    | None => None
    };
  let (_, (_, color_map), _) =
    get_doc(~inject, ~font_metrics, ~settings, info);
  color_map;
};

let view =
    (
      ~inject,
      ~font_metrics,
      ~settings: Model.settings,
      index': option(int),
      info_map: Core.Statics.map,
    ) => {
  let info: option(Core.Statics.t) =
    switch (index') {
    | Some(index) =>
      switch (Core.Id.Map.find_opt(index, info_map)) {
      | Some(ci) => Some(ci)
      | None => None
      }
    | None => None
    };
  let (syn_form, (explanation, _), example) =
    get_doc(~inject, ~font_metrics, ~settings, info);
  div(
    [clss(["lang-doc"])],
    [
      div(
        [Attr.on_click(_ => Event.Stop_propagation)],
        [
          section(
            ~section_clss="syntactic-form",
            ~title="Syntactic Form",
            syn_form,
          ),
          section(
            ~section_clss="explanation",
            ~title="Explanation",
            explanation,
          ),
          section(~section_clss="examples", ~title="Examples", example),
        ],
      ),
    ],
  );
};
