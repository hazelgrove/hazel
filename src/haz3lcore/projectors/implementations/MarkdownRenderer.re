open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* MarkdownRenderer - View a probed string value as rendered Markdown.
 * Uses the omd library; supports a "Show raw" toggle for cases where the
 * rendered output isn't what the user expected. */

[@deriving (show({with_path: false}), sexp, yojson)]
type v = string;
[@deriving (show({with_path: false}), sexp, yojson)]
type m = {raw: bool};
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | ToggleRaw;

[@deriving (show({with_path: false}), sexp, yojson)]
type model = m;
[@deriving (show({with_path: false}), sexp, yojson)]
type action = a;
[@deriving (show({with_path: false}), sexp, yojson)]
type value = v;

let parse = (_sort: Sort.t, exp: Exp.t): option(value) =>
  switch (exp.term) {
  | Atom(String(s)) => Some(s)
  | _ => None
  };

let init = (_: value): model => {raw: false};

let update = (_model: model, action: action): model =>
  switch (action) {
  | ToggleRaw => {raw: !_model.raw}
  };

/* --- Markdown → Vdom --- */

let rec inline_text = (inline: Omd.inline(_)): string =>
  switch (inline) {
  | Omd.Concat(_, items) => String.concat("", List.map(inline_text, items))
  | Omd.Text(_, s)
  | Omd.Code(_, s)
  | Omd.Html(_, s) => s
  | Omd.Emph(_, d)
  | Omd.Strong(_, d) => inline_text(d)
  | Omd.Link(_, {label, _})
  | Omd.Image(_, {label, _}) => inline_text(label)
  | Omd.Hard_break(_)
  | Omd.Soft_break(_) => " "
  };

let rec render_inline = (inline: Omd.inline(_)): list(Node.t) =>
  switch (inline) {
  | Omd.Concat(_, items) => List.concat_map(render_inline, items)
  | Omd.Text(_, s) => [Node.text(s)]
  | Omd.Code(_, s) => [
      Node.code(~attrs=[Attr.classes(["md-code"])], [Node.text(s)]),
    ]
  | Omd.Emph(_, d) => [Node.em(render_inline(d))]
  | Omd.Strong(_, d) => [Node.strong(render_inline(d))]
  | Omd.Link(_, {label, destination, _}) => [
      Node.a(
        ~attrs=[
          Attr.href(destination),
          Attr.create("target", "_blank"),
          Attr.create("rel", "noopener noreferrer"),
        ],
        render_inline(label),
      ),
    ]
  | Omd.Image(_, {label, destination, _}) => [
      Node.create(
        "img",
        ~attrs=[
          Attr.src(destination),
          Attr.create("alt", inline_text(label)),
        ],
        [],
      ),
    ]
  | Omd.Hard_break(_) => [Node.br()]
  | Omd.Soft_break(_) => [Node.text(" ")]
  | Omd.Html(_, s) => [Node.text(s)]
  };

let rec render_block = (block: Omd.block(_)): list(Node.t) =>
  switch (block) {
  | Omd.Paragraph(_, d) => [Node.p(render_inline(d))]
  | Omd.Heading(_, level, d) =>
    let tag = "h" ++ string_of_int(max(1, min(6, level)));
    [Node.create(tag, render_inline(d))];
  | Omd.List(_, list_type, _, items) =>
    let item_nodes =
      List.map(
        blocks => Node.li(List.concat_map(render_block, blocks)),
        items,
      );
    switch (list_type) {
    | Omd.Ordered(_, _) => [Node.ol(item_nodes)]
    | Omd.Bullet(_) => [Node.ul(item_nodes)]
    };
  | Omd.Blockquote(_, blocks) => [
      Node.blockquote(List.concat_map(render_block, blocks)),
    ]
  | Omd.Code_block(_, _info, code) => [
      Node.pre([
        Node.code(
          ~attrs=[Attr.classes(["md-code-block"])],
          [Node.text(code)],
        ),
      ]),
    ]
  | Omd.Thematic_break(_) => [Node.hr()]
  | Omd.Html_block(_, s) => [Node.text(s)]
  | _ => []
  };

let render_markdown = (s: string): list(Node.t) => {
  let doc =
    try(Omd.of_string(s)) {
    | _ => []
    };
  List.concat_map(render_block, doc);
};

/* --- View --- */

let render =
    (
      ~info as _: info,
      ~exp as _: Exp.t,
      ~value: value,
      ~view_seg as _: (Sort.t, Segment.t) => Node.t,
      ~model: model,
      ~local: action => Ui_effect.t(unit),
      ~parent as _: external_action => Ui_effect.t(unit),
      ~sort as _: Sort.t,
      _: unit,
    )
    : Node.t => {
  let toggle =
    Node.button(
      ~attrs=[
        Attr.classes(["md-toggle"]),
        Attr.title(model.raw ? "Show rendered Markdown" : "Show raw source"),
        Attr.on_click(_ => local(ToggleRaw)),
      ],
      [Node.text(model.raw ? "Rendered" : "Raw")],
    );
  let body =
    if (model.raw) {
      Node.pre(~attrs=[Attr.classes(["md-raw"])], [Node.text(value)]);
    } else {
      Node.div(
        ~attrs=[Attr.classes(["md-rendered"])],
        render_markdown(value),
      );
    };
  Node.div(
    ~attrs=[Attr.classes(["md-renderer"])],
    [Node.div(~attrs=[Attr.classes(["md-toolbar"])], [toggle]), body],
  );
};

/* --- Badge --- */

let icon_size = 20.;
let markdown_icon =
  Node.create_svg(
    "svg",
    ~attrs=
      Attr.[
        create("viewBox", "0 0 16 10"),
        create("width", Printf.sprintf("%fpx", icon_size)),
        create("height", Printf.sprintf("%fpx", icon_size)),
        create("preserveAspectRatio", "xMidYMid meet"),
      ],
    [
      Node.create_svg(
        "rect",
        ~attrs=
          Attr.[
            create("x", "0.4"),
            create("y", "0.4"),
            create("width", "15.2"),
            create("height", "9.2"),
            create("rx", "1.2"),
            create("fill", "none"),
            create("stroke", "currentColor"),
            create("stroke-width", "0.6"),
          ],
        [],
      ),
      Node.create_svg(
        "path",
        ~attrs=
          Attr.[
            create(
              "d",
              "M2.4 7.6 V 3 L 4.2 5.4 L 6 3 V 7.6 M 8.2 4.2 V 7.6 M 7.2 6.4 L 8.2 7.6 L 9.2 6.4 M 11.6 4 V 6.6 M 10.6 5.6 L 11.6 6.6 L 12.6 5.6",
            ),
            create("fill", "none"),
            create("stroke", "currentColor"),
            create("stroke-width", "0.6"),
            create("stroke-linecap", "round"),
            create("stroke-linejoin", "round"),
          ],
        [],
      ),
    ],
  );
let badge =
  Node.span(
    ~attrs=[
      Attr.classes(["markdown-badge"]),
      Attr.title("Click to view as Markdown"),
    ],
    [markdown_icon],
  );
