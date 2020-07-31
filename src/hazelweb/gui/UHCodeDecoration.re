module Vdom = Virtual_dom.Vdom;
module MeasuredPosition = Pretty.MeasuredPosition;
module MeasuredLayout = Pretty.MeasuredLayout;

let inline_open_child_border_height = 0.1; // y units
let multiline_open_child_border_width = 0.25; // x units
let tessera_margin = 0.03; // y units

type rects = list(RectilinearPolygon.rect);

type current_term_rects = {
  highlighted: rects,
  closed_children: list((TermSort.t, rects)),
};

let rects =
    (~vtrim=0.0, ~indent=0, start: MeasuredPosition.t, m: UHMeasuredLayout.t)
    : (MeasuredPosition.t, rects) => {
  let mk_rect =
      (
        ~is_first=false,
        ~is_last=false,
        start: MeasuredPosition.t,
        box: MeasuredLayout.box,
      ) =>
    RectilinearPolygon.{
      min: {
        x: Float.of_int(start.col),
        y: Float.of_int(start.row) +. (is_first ? vtrim : 0.0),
      },
      width: Float.of_int(box.width),
      height:
        Float.of_int(box.height)
        -. (is_first ? vtrim : 0.0)
        -. (is_last ? vtrim : 0.0),
    };
  let (leading, last) = ListUtil.split_last(m.metrics);
  let (last_start, leading_rects) =
    leading
    |> List.mapi((i, box) => (i, box))
    |> ListUtil.map_with_accumulator(
         (start: MeasuredPosition.t, (i, box: MeasuredLayout.box)) =>
           (
             {row: start.row + box.height, col: 0},
             mk_rect(~is_first=i == 0, start, box),
           ),
         start,
       );
  let end_: MeasuredPosition.t = {
    row: last_start.row + last.height - 1,
    col:
      last.width
      + (
        switch (leading) {
        | [] => start.col
        | [_, ..._] => indent
        }
      ),
  };
  let last_rect =
    mk_rect(~is_first=leading == [], ~is_last=true, last_start, last);
  (end_, leading_rects @ [last_rect]);
};

let err_hole_view =
    (
      ~corner_radii: (float, float),
      ~offset: int,
      subject: UHMeasuredLayout.t,
    )
    : Vdom.Node.t =>
  subject
  |> rects({row: 0, col: offset})
  |> snd
  |> RectilinearPolygon.mk_svg(
       ~corner_radii,
       ~attrs=
         Vdom.Attr.[
           classes(["err-hole"]),
           create("vector-effect", "non-scaling-stroke"),
         ],
     );

let var_err_hole_view =
    (
      ~corner_radii: (float, float),
      ~offset: int,
      subject: UHMeasuredLayout.t,
    )
    : Vdom.Node.t =>
  subject
  |> rects({row: 0, col: offset})
  |> snd
  |> RectilinearPolygon.mk_svg(
       ~corner_radii,
       ~attrs=
         Vdom.Attr.[
           classes(["var-err-hole"]),
           create("vector-effect", "non-scaling-stroke"),
         ],
     );

let var_use_view =
    (
      ~corner_radii: (float, float),
      ~offset: int,
      subject: UHMeasuredLayout.t,
    )
    : Vdom.Node.t =>
  subject
  |> rects({row: 0, col: offset})
  |> snd
  |> RectilinearPolygon.mk_svg(
       ~corner_radii,
       ~attrs=
         Vdom.Attr.[
           classes(["var-use"]),
           create("vector-effect", "non-scaling-stroke"),
         ],
     );

let inline_open_child_rects =
    (start: MeasuredPosition.t, m: UHMeasuredLayout.t): rects => {
  // TODO relax assumption
  assert(MeasuredLayout.height(m) == 1);
  // make singleton skinny rect along bottom
  [
    RectilinearPolygon.{
      min: {
        x: Float.of_int(start.col),
        y:
          Float.of_int(start.row)
          +. 1.
          -. inline_open_child_border_height
          -. tessera_margin,
      },
      height: inline_open_child_border_height,
      width: Float.of_int(MeasuredLayout.width(m)),
    },
  ];
};

let multiline_open_child_rects =
    (~overflow_left, start: MeasuredPosition.t, m: UHMeasuredLayout.t): rects => {
  let overflow_left = overflow_left ? multiline_open_child_border_width : 0.0;
  [
    // make singleton skinny rect
    RectilinearPolygon.{
      min: {
        x: Float.of_int(start.col) -. overflow_left,
        y: Float.of_int(start.row),
      },
      height: Float.of_int(MeasuredLayout.height(m)),
      width: multiline_open_child_border_width,
    },
  ];
};

let sort_cls: TermSort.t => string =
  fun
  | Typ => "Typ"
  | Pat => "Pat"
  | Exp => "Exp";

let closed_child_filter = (sort: TermSort.t) => {
  let sort_cls = sort_cls(sort);
  Vdom.(
    Node.create_svg(
      "filter",
      [
        Attr.id(
          String.lowercase_ascii(sort_cls) ++ "-closed-child-drop-shadow",
        ),
      ],
      [
        Node.create_svg(
          "feOffset",
          [
            Attr.create("in", "SourceAlpha"),
            Attr.create("dx", "0.1"),
            Attr.create("dy", "0.04"),
            Attr.create("result", "offset-alpha"),
          ],
          [],
        ),
        Node.create_svg(
          "feFlood",
          [
            Attr.classes(["closed-child-inset-shadow", sort_cls]),
            Attr.create("flood-opacity", "1"),
            Attr.create("result", "color"),
          ],
          [],
        ),
        Node.create_svg(
          "feComposite",
          [
            // Attr.classes(["closed-child-drop-shadow"]),
            Attr.create("operator", "out"),
            Attr.create("in", "SourceAlpha"),
            Attr.create("in2", "offset-alpha"),
            Attr.create("result", "shadow-shape"),
          ],
          [],
        ),
        Node.create_svg(
          "feComposite",
          [
            Attr.create("operator", "in"),
            Attr.create("in", "color"),
            Attr.create("in2", "shadow-shape"),
            Attr.create("result", "drop-shadow"),
          ],
          [],
        ),
        Node.create_svg(
          "feMerge",
          [],
          [
            Node.create_svg(
              "feMergeNode",
              [Attr.create("in", "SourceGraphic")],
              [],
            ),
            Node.create_svg(
              "feMergeNode",
              [Attr.create("in", "drop-shadow")],
              [],
            ),
          ],
        ),
      ],
    )
  );
};

let overflow_left: TermShape.t => bool =
  fun
  | SubBlock(_)
  | NTuple(_)
  | BinOp(_) => true
  | Case
  | Rule
  | Operand => false;

// highlighted tesserae (ignoring closed children)
let current_term_tessera_rects =
    (~shape: TermShape.t, ~offset: int, subject: UHMeasuredLayout.t): rects =>
  subject
  |> MeasuredLayout.pos_fold(
       ~start={row: 0, col: offset},
       ~linebreak=_ => [],
       ~text=(_, _) => [],
       ~align=(_, _) => [],
       ~cat=(_, rs1, rs2) => rs1 @ rs2,
       ~annot=
         (go, start, annot: UHAnnot.t, m) =>
           switch (shape, annot) {
           | (Case | SubBlock(_), Step(_))
           | (Case, Term({shape: Rule, _})) => go(m)
           | (_, Tessera) => snd(rects(~vtrim=tessera_margin, start, m))
           | _ => []
           },
     );

// closed child "cutouts" from highlighted tesserae
let current_term_closed_child_rects =
    (~shape: TermShape.t, ~offset: int, subject: UHMeasuredLayout.t)
    : list((TermSort.t, rects)) =>
  subject
  |> MeasuredLayout.pos_fold(
       ~start={row: 0, col: offset},
       ~linebreak=_ => [],
       ~text=(_, _) => [],
       ~align=(_, _) => [],
       ~cat=(_, rss1, rss2) => rss1 @ rss2,
       ~annot=
         (go, start, annot: UHAnnot.t, m) =>
           switch (shape, annot) {
           // hack for case and subblocks
           // TODO remove when we have tiles
           | (Case | SubBlock(_), Step(_))
           | (Case, Term({shape: Rule, _})) => go(m)
           | (_, Tessera) => go(m)
           | (_, ClosedChild({sort, _})) => [
               (sort, snd(rects(~vtrim=0.1, start, m))),
             ]
           | _ => []
           },
     );

// highlighted borders of open children
let current_term_open_child_rects =
    (~shape: TermShape.t, ~offset: int, subject: UHMeasuredLayout.t): rects => {
  let has_multiline_open_child =
    subject
    |> MeasuredLayout.fold(
         ~linebreak=false,
         ~text=_ => false,
         ~align=b => b,
         ~cat=(||),
         ~annot=
           (go, annot: UHAnnot.t, m) =>
             switch (shape, annot) {
             | (Case | SubBlock(_), Step(_))
             | (Case, Term({shape: Rule, _})) => go(m)
             | (_, OpenChild(Multiline)) => true
             | (_, _) => false
             },
       );
  let subject_height = MeasuredLayout.height(subject);
  subject
  |> MeasuredLayout.pos_fold(
       ~start={row: 0, col: offset},
       ~linebreak=_ => [],
       ~text=(_, _) => [],
       ~align=(_, _) => [],
       ~cat=(_, rs1, rs2) => rs1 @ rs2,
       ~annot=
         (go, start, annot: UHAnnot.t, m) => {
           // some tesserae need to be padded on left side to form
           // a straight edge with borders of neighboring multiline
           // open children
           let tessera_padding = overflow_left => {
             let min_x =
               Float.of_int(start.col)
               -. (overflow_left ? multiline_open_child_border_width : 0.0);
             let min_y =
               Float.of_int(start.row)
               +. (start.row == 0 ? tessera_margin : 0.);
             let height =
               Float.of_int(MeasuredLayout.height(m))
               -. (start.row == 0 ? tessera_margin : 0.)
               -. (start.row == subject_height - 1 ? tessera_margin : 0.);
             RectilinearPolygon.[
               {
                 min: {
                   x: min_x,
                   y: min_y,
                 },
                 height,
                 width: multiline_open_child_border_width,
               },
             ];
           };

           switch (shape, annot) {
           | (Case | SubBlock(_), Step(_))
           | (Case, Term({shape: Rule, _})) => go(m)
           | (_, OpenChild(InlineWithBorder)) =>
             inline_open_child_rects(start, m)
           | (_, OpenChild(Multiline)) =>
             multiline_open_child_rects(
               ~overflow_left=overflow_left(shape),
               start,
               m,
             )
           | (Case, Tessera) => tessera_padding(false)
           | (_, Tessera) when has_multiline_open_child && start.col == 0 =>
             // TODO may need to revisit above when guard
             // to support layouts like
             // let _ = \x.{
             //   _
             // } in ...
             // where lambda has offset head
             tessera_padding(overflow_left(shape))
           | _ => []
           };
         },
     );
};

let current_term_view =
    (
      ~corner_radii: (float, float),
      ~offset: int,
      ~sort: TermSort.t,
      ~shape: TermShape.t,
      subject: UHMeasuredLayout.t,
    )
    : Vdom.Node.t => {
  let highlighted = {
    let tesserae = subject |> current_term_tessera_rects(~shape, ~offset);
    let open_child_borders =
      subject |> current_term_open_child_rects(~shape, ~offset);
    RectilinearPolygon.mk_svg(
      ~corner_radii,
      ~attrs=Vdom.[Attr.classes(["code-current-term", sort_cls(sort)])],
      tesserae @ open_child_borders,
    );
  };
  let closed_children =
    subject
    |> current_term_closed_child_rects(~shape, ~offset)
    |> List.map(((sort, rs)) =>
         RectilinearPolygon.mk_svg(
           ~corner_radii,
           ~attrs=[
             Vdom.Attr.classes(["code-closed-child", sort_cls(sort)]),
           ],
           rs,
         )
       );
  let outer_filter =
    Vdom.(
      Node.create_svg(
        "filter",
        [Attr.id("outer-drop-shadow")],
        [
          Node.create_svg(
            "feDropShadow",
            [
              Attr.classes(["current-term-drop-shadow", sort_cls(sort)]),
              Attr.create("dx", "0.1"),
              Attr.create("dy", "0.04"),
              Attr.create("stdDeviation", "0"),
            ],
            [],
          ),
        ],
      )
    );
  // <feOffset in="SourceGraphic" dx="60" dy="60" />
  // <feFlood flood-color="black" flood-opacity="1" result="color"/>
  Vdom.(
    Node.create_svg(
      "g",
      [],
      [
        // TODO cache filters at document root
        outer_filter,
        closed_child_filter(Typ),
        closed_child_filter(Pat),
        closed_child_filter(Exp),
        highlighted,
        ...closed_children,
      ],
    )
  );
};

let caret_view =
    (~font_metrics: FontMetrics.t, {row, col}: MeasuredPosition.t)
    : Vdom.Node.t => {
  Vdom.(
    Node.span(
      [
        Attr.id("caret"),
        Attr.create(
          "style",
          Printf.sprintf(
            // TODO make more robust
            "top: calc(%fpx - 1px); left: calc(%fpx - 1px);",
            Float.of_int(row) *. font_metrics.row_height,
            Float.of_int(col) *. font_metrics.col_width,
          ),
        ),
        Attr.classes(["blink"]),
      ],
      [],
    )
  );
};

let view =
    (
      ~corner_radius=2.5, // px
      ~font_metrics: FontMetrics.t,
      // TODO document
      ~origin: MeasuredPosition.t,
      ~offset: int,
      // ~shape: TermShape.t,
      ~subject: UHMeasuredLayout.t,
      d: Decoration.t,
    ) => {
  let num_rows =
    subject.metrics
    |> List.map((box: MeasuredLayout.box) => box.height)
    |> List.fold_left((+), 0);
  let buffered_height = Float.of_int(num_rows + 1) *. font_metrics.row_height;

  let num_cols =
    List.tl(subject.metrics)
    |> List.map((box: MeasuredLayout.box) => box.width)
    |> List.fold_left(max, offset + List.hd(subject.metrics).width);
  let buffered_width = Float.of_int(num_cols + 1) *. font_metrics.col_width;

  let corner_radii = (
    corner_radius /. font_metrics.col_width,
    corner_radius /. font_metrics.row_height,
  );

  let v =
    switch (d) {
    | ErrHole => err_hole_view(~corner_radii, ~offset, subject)
    | VarErrHole => var_err_hole_view(~corner_radii, ~offset, subject)
    | VarUse => var_use_view(~corner_radii, ~offset, subject)
    | CurrentTerm(sort, shape) =>
      current_term_view(~corner_radii, ~offset, ~sort, ~shape, subject)
    };

  let cls =
    switch (d) {
    | ErrHole => "err-hole"
    | VarErrHole => "var-err-hole"
    | VarUse => "var-use"
    | CurrentTerm(_) => "current-term"
    };

  Vdom.(
    Node.div(
      [
        Attr.classes([
          "decoration-container",
          Printf.sprintf("%s-container", cls),
        ]),
        Attr.create(
          "style",
          Printf.sprintf(
            "top: calc(%fpx - 1px); left: %fpx;",
            (Float.of_int(origin.row) -. 0.5) *. font_metrics.row_height,
            (Float.of_int(origin.col) -. 0.5) *. font_metrics.col_width,
          ),
        ),
      ],
      [
        Node.create_svg(
          "svg",
          [
            Attr.classes([cls]),
            Attr.create(
              "viewBox",
              Printf.sprintf("-0.5 -0.5 %d %d", num_cols + 1, num_rows + 1),
            ),
            Attr.create("width", Printf.sprintf("%fpx", buffered_width)),
            Attr.create("height", Printf.sprintf("%fpx", buffered_height)),
            Attr.create("preserveAspectRatio", "none"),
          ],
          [v],
        ),
      ],
    )
  );
};
