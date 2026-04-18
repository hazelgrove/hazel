open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/** Only allow navigable links we expect from assistant output. */
let href_if_safe = (destination: string): option(string) => {
  let d = String.trim(destination);
  if (String.length(d) == 0) {
    None;
  } else if (String.starts_with(~prefix="#", d)) {
    Some(d);
  } else if (String.starts_with(~prefix="http://", d)
             || String.starts_with(~prefix="https://", d)) {
    Some(d);
  } else if (String.starts_with(~prefix="mailto:", d)) {
    Some(d);
  } else {
    None;
  };
};

let img_src_if_safe = (destination: string): option(string) => {
  switch (href_if_safe(destination)) {
  | Some(s) as ok
      when
        String.starts_with(~prefix="http://", s)
        || String.starts_with(~prefix="https://", s) => ok
  | _ => None
  };
};

let rec translate_inline = (inline: Omd.inline(_)): list(Node.t) => {
  switch (inline) {
  | Omd.Concat(_, items) => List.concat(List.map(translate_inline, items))
  | Omd.Text(_, s) => [text(s)]
  | Omd.Code(_, s) => [
      code(~attrs=[clss(["agent-md-code"])], [text(s)]),
    ]
  | Omd.Emph(_, d) => [em(~attrs=[], translate_inline(d))]
  | Omd.Strong(_, d) => [strong(~attrs=[], translate_inline(d))]
  | Omd.Hard_break(_) => [br(~attrs=[], ())]
  | Omd.Soft_break(_) => [text(" ")]
  | Omd.Link(_, {label, destination, title: _}) =>
    let inner = translate_inline(label);
    switch (href_if_safe(destination)) {
    | Some(href) => [
        a(
          ~attrs=[
            Attr.href(href),
            Attr.target("_blank"),
            Attr.create("rel", "noopener noreferrer"),
            clss(["agent-md-a"]),
          ],
          inner,
        ),
      ]
    | None => inner
    };
  | Omd.Image(_, {label, destination, title}) =>
    switch (img_src_if_safe(destination)) {
    | Some(src) =>
      let attrs =
        [Attr.src(src), clss(["agent-md-img"])]
        @ (
          switch (title) {
          | None => []
          | Some(t) => [Attr.title(t)]
          }
        );
      [img(~attrs, ())];
    | None => translate_inline(label)
    }
  | Omd.Html(_, _) => []
  };
};

let heading = (level: int, children: list(Node.t)): Node.t => {
  let attrs = [
    clss(["agent-md-heading", "agent-md-h" ++ string_of_int(level)]),
  ];
  switch (level) {
  | 1 => h1(~attrs, children)
  | 2 => h2(~attrs, children)
  | 3 => h3(~attrs, children)
  | 4 => h4(~attrs, children)
  | 5 => h5(~attrs, children)
  | _ => h6(~attrs, children)
  };
};

let rec translate_blocks = (blocks: Omd.doc): list(Node.t) => {
  List.concat(
    List.map(
      (elem: Omd.block(_)) => {
        switch (elem) {
        | Omd.Paragraph(_, d) => [
            p(~attrs=[clss(["agent-md-p"])], translate_inline(d)),
          ]
        | Omd.Heading(_, level, d) => [heading(level, translate_inline(d))]
        | Omd.List(_, typ, _, items) =>
          let lis =
            List.map(
              (item_blocks: list(Omd.block(_))) => {
                li(
                  ~attrs=[clss(["agent-md-li"])],
                  translate_blocks(item_blocks),
                )
              },
              items,
            );
          [
            switch (typ) {
            | Omd.Bullet(_) => ul(~attrs=[clss(["agent-md-ul"])], lis)
            | Omd.Ordered(_, _) => ol(~attrs=[clss(["agent-md-ol"])], lis)
            },
          ];
        | Omd.Blockquote(_, bs) => [
            blockquote(
              ~attrs=[clss(["agent-md-bq"])],
              translate_blocks(bs),
            ),
          ]
        | Omd.Thematic_break(_) => [hr(~attrs=[clss(["agent-md-hr"])], ())]
        | Omd.Code_block(_, _label, code_text) => [
            pre(
              ~attrs=[clss(["agent-md-pre"])],
              [
                code(
                  ~attrs=[clss(["agent-md-code-block"])],
                  [text(code_text)],
                ),
              ],
            ),
          ]
        | Omd.Table(_, headers, rows) =>
          let th_cells =
            List.map(
              ((cell, _align)) =>
                th(~attrs=[clss(["agent-md-th"])], translate_inline(cell)),
              headers,
            );
          let body_rows =
            List.map(
              (row: list(Omd.inline(_))) => {
                let tds =
                  List.map(
                    cell =>
                      td(
                        ~attrs=[clss(["agent-md-td"])],
                        translate_inline(cell),
                      ),
                    row,
                  );
                tr(~attrs=[clss(["agent-md-tr"])], tds);
              },
              rows,
            );
          [
            table(
              ~attrs=[clss(["agent-md-table"])],
              [
                thead(
                  ~attrs=[clss(["agent-md-thead"])],
                  [tr(~attrs=[], th_cells)],
                ),
                tbody(~attrs=[clss(["agent-md-tbody"])], body_rows),
              ],
            ),
          ];
        | Omd.Html_block(_, _) => []
        | Omd.Definition_list(_, _) => []
        }
      },
      blocks,
    ),
  );
};

/** Renders assistant markdown to vdom. Empty or whitespace-only input yields an empty root. */
let view = (markdown: string): Node.t => {
  let trimmed = String.trim(markdown);
  let children =
    if (trimmed == "") {
      [];
    } else {
      translate_blocks(Omd.of_string(markdown));
    };
  div(~attrs=[clss(["agent-message-markdown"])], children);
};
