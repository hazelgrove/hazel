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

/* Rough proxy: one row per source line. Close enough for raw-mode and
   typical prose; tables / lists may expand past this and scroll. */
let placeholder = (value: value, _: model): ProjectorCore.Shape.t => {
  let lines = String.fold_left((n, c) => c == '\n' ? n + 1 : n, 1, value);
  ProjectorCore.Shape.{
    vertical: Block(lines),
    horizontal: 0,
  };
};

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

/* Hazel stores string literals with escape sequences intact (e.g. `\n` is
 * two characters in the runtime value, not a newline). Decode the common
 * ones so multi-line markdown actually renders as multiple lines. */
let decode_escapes = (s: string): string => {
  /* Stash escaped backslashes first so subsequent passes don't see `\\n`
   * and decode it as a real newline. */
  let placeholder = "\000BS\000";
  let replace = (pat, repl, str) =>
    Js_of_ocaml.Regexp.global_replace(
      Js_of_ocaml.Regexp.regexp(pat),
      str,
      repl,
    );
  s
  |> replace("\\\\\\\\", placeholder)
  |> replace("\\\\n", "\n")
  |> replace("\\\\t", "\t")
  |> replace("\\\\r", "\r")
  |> replace("\\\\\"", "\"")
  |> replace(placeholder, "\\");
};

/* --- GFM tables (omd doesn't support these natively) ---
 *
 * Detect runs of lines that form a pipe table:
 *   | h1 | h2 |
 *   | -- | :- |
 *   | a  | b  |
 * Render them directly to Vdom; everything else goes through omd. */

type alignment =
  | AlignNone
  | AlignLeft
  | AlignRight
  | AlignCenter;

let split_pipe_row = (line: string): list(string) => {
  let trimmed = String.trim(line);
  let body =
    if (String.length(trimmed) >= 1 && trimmed.[0] == '|') {
      String.sub(trimmed, 1, String.length(trimmed) - 1);
    } else {
      trimmed;
    };
  let body =
    if (String.length(body) >= 1 && body.[String.length(body) - 1] == '|') {
      String.sub(body, 0, String.length(body) - 1);
    } else {
      body;
    };
  String.split_on_char('|', body) |> List.map(String.trim);
};

let is_separator_cell = (s: string): option(alignment) => {
  let n = String.length(s);
  if (n == 0) {
    None;
  } else {
    let left = s.[0] == ':';
    let right = s.[n - 1] == ':';
    let inner_start = left ? 1 : 0;
    let inner_end = right ? n - 1 : n;
    let dashes = ref(0);
    let ok = ref(true);
    for (i in inner_start to inner_end - 1) {
      if (s.[i] == '-') {
        incr(dashes);
      } else {
        ok := false;
      };
    };
    if (ok^ && dashes^ >= 1) {
      Some(
        switch (left, right) {
        | (true, true) => AlignCenter
        | (true, false) => AlignLeft
        | (false, true) => AlignRight
        | (false, false) => AlignNone
        },
      );
    } else {
      None;
    };
  };
};

let parse_separator = (line: string): option(list(alignment)) => {
  let cells = split_pipe_row(line);
  if (List.length(cells) == 0) {
    None;
  } else {
    let aligns = List.map(is_separator_cell, cells);
    if (List.for_all(Option.is_some, aligns)) {
      Some(List.map(Option.get, aligns));
    } else {
      None;
    };
  };
};

let looks_like_table_line = (line: string): bool => {
  let t = String.trim(line);
  String.length(t) > 0 && String.contains(t, '|');
};

let align_attr = (a: alignment): list(Attr.t) =>
  switch (a) {
  | AlignNone => []
  | AlignLeft => [Attr.style(Css_gen.text_align(`Left))]
  | AlignRight => [Attr.style(Css_gen.text_align(`Right))]
  | AlignCenter => [Attr.style(Css_gen.text_align(`Center))]
  };

let render_cell_inline = (s: string): list(Node.t) =>
  /* Render the cell text through omd as inline markdown by wrapping it in
   * a paragraph and pulling its inlines out. Falls back to plain text. */
  switch (Omd.of_string(s)) {
  | [Omd.Paragraph(_, inlines)] => render_inline(inlines)
  | _ => [Node.text(s)]
  };

let render_table =
    (
      aligns: list(alignment),
      header: list(string),
      rows: list(list(string)),
    )
    : Node.t => {
  let zip_with_aligns = cells => {
    let rec aux = (cs, als) =>
      switch (cs, als) {
      | ([], _) => []
      | ([c, ...crest], [a, ...arest]) => [(c, a), ...aux(crest, arest)]
      | ([c, ...crest], []) => [(c, AlignNone), ...aux(crest, [])]
      };
    aux(cells, aligns);
  };
  let header_cells =
    List.map(
      ((c, a)) => Node.th(~attrs=align_attr(a), render_cell_inline(c)),
      zip_with_aligns(header),
    );
  let body_rows =
    List.map(
      row =>
        Node.tr(
          List.map(
            ((c, a)) =>
              Node.td(~attrs=align_attr(a), render_cell_inline(c)),
            zip_with_aligns(row),
          ),
        ),
      rows,
    );
  Node.table(
    ~attrs=[Attr.classes(["md-table"])],
    [Node.thead([Node.tr(header_cells)]), Node.tbody(body_rows)],
  );
};

/* Walk lines, splitting into table blocks and other-text blocks.
 * Tables: [header line; separator line; zero or more row lines]. */
let extract_tables =
    (s: string)
    : list(
        [
          | `Md(string)
          | `Table(Node.t)
        ],
      ) => {
  let lines = String.split_on_char('\n', s);
  let arr = Array.of_list(lines);
  let n = Array.length(arr);
  let out = ref([]);
  let buf = Stdlib.Buffer.create(64);
  let flush_buf = () =>
    if (Stdlib.Buffer.length(buf) > 0) {
      out := [`Md(Stdlib.Buffer.contents(buf)), ...out^];
      Stdlib.Buffer.clear(buf);
    };
  let i = ref(0);
  while (i^ < n) {
    let header = arr[i^];
    let has_sep =
      i^
      + 1 < n
      && looks_like_table_line(header)
      && Option.is_some(parse_separator(arr[i^ + 1]));
    if (has_sep) {
      let aligns = Option.get(parse_separator(arr[i^ + 1]));
      let header_cells = split_pipe_row(header);
      let row_start = i^ + 2;
      let j = ref(row_start);
      while (j^ < n && looks_like_table_line(arr[j^])) {
        incr(j);
      };
      let rows =
        Array.sub(arr, row_start, j^ - row_start)
        |> Array.to_list
        |> List.map(split_pipe_row);
      flush_buf();
      out := [`Table(render_table(aligns, header_cells, rows)), ...out^];
      i := j^;
    } else {
      Stdlib.Buffer.add_string(buf, arr[i^]);
      Stdlib.Buffer.add_char(buf, '\n');
      incr(i);
    };
  };
  flush_buf();
  List.rev(out^);
};

let render_markdown = (s: string): list(Node.t) => {
  let decoded = decode_escapes(s);
  let chunks = extract_tables(decoded);
  List.concat_map(
    fun
    | `Md(text) =>
      switch (Omd.of_string(text)) {
      | doc => List.concat_map(render_block, doc)
      | exception _ => [Node.text(text)]
      }
    | `Table(node) => [node],
    chunks,
  );
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
      Node.pre(
        ~attrs=[Attr.classes(["md-raw"])],
        [Node.text(decode_escapes(value))],
      );
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
