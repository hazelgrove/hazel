open Sexplib.Std;
open Virtual_dom.Vdom;

module MeasuredPosition = Pretty.MeasuredPosition;
module MeasuredLayout = Pretty.MeasuredLayout;

type rects = list(SvgUtil.Rect.t);

let rects =
    (~vtrim=0.0, start: MeasuredPosition.t, m: UHMeasuredLayout.t): rects => {
  let mk_rect =
      (
        ~is_first=false,
        ~is_last=false,
        start: MeasuredPosition.t,
        box: MeasuredLayout.box,
      ) =>
    SvgUtil.Rect.{
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
  let n = List.length(m.metrics);
  m.metrics
  |> List.mapi((i, box) => (i, box))
  |> ListUtil.map_with_accumulator(
       (start: MeasuredPosition.t, (i, box: MeasuredLayout.box)) =>
         (
           {row: start.row + box.height, col: 0},
           mk_rect(~is_first=i == 0, ~is_last=i == n - 1, start, box),
         ),
       start,
     )
  |> snd;
};

module VarUse = {
  let view =
      (
        ~corner_radii: (float, float),
        (offset, subject): UHMeasuredLayout.with_offset,
      )
      : Node.t =>
    subject
    |> rects({row: 0, col: offset})
    |> SvgUtil.OrthogonalPolygon.mk(~corner_radii)
    |> SvgUtil.Path.view(
         ~attrs=
           Attr.[
             classes(["var-use"]),
             create("vector-effect", "non-scaling-stroke"),
           ],
       );
};

module CurrentTerm = {
  let inline_open_child_border_height = 0.1; // y units
  let multiline_open_child_border_width = 0.25; // x units
  let tessera_margin = 0.03; // y units

  let inline_open_child_rects =
      (start: MeasuredPosition.t, m: UHMeasuredLayout.t): rects => {
    // TODO relax assumption
    assert(MeasuredLayout.height(m) == 1);
    // make singleton skinny rect along bottom
    [
      SvgUtil.Rect.{
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
      (
        ~overflow_left,
        ~vtrim_bot=false,
        start: MeasuredPosition.t,
        m: UHMeasuredLayout.t,
      )
      : rects => {
    let overflow_left =
      overflow_left ? multiline_open_child_border_width : 0.0;
    [
      // make singleton skinny rect
      SvgUtil.Rect.{
        min: {
          x: Float.of_int(start.col) -. overflow_left,
          y: Float.of_int(start.row),
        },
        height:
          Float.of_int(MeasuredLayout.height(m))
          -. (vtrim_bot ? tessera_margin : 0.),
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
      (~shape: TermShape.t, (offset, subject): UHMeasuredLayout.with_offset)
      : rects =>
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
             | (_, Tessera) => rects(~vtrim=tessera_margin, start, m)
             | _ => []
             },
       );

  // closed child "cutouts" from highlighted tesserae
  let current_term_closed_child_rects =
      (~shape: TermShape.t, (offset, subject): UHMeasuredLayout.with_offset)
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
                 (sort, rects(~vtrim=0.1, start, m)),
               ]
             | _ => []
             },
       );

  // highlighted borders of open children
  let current_term_open_child_rects =
      (~shape: TermShape.t, (offset, subject): UHMeasuredLayout.with_offset)
      : rects => {
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
             let tessera_padding =
                 (~vtrim_top: bool, ~vtrim_bot: bool, ~overflow_left: bool) => {
               let min_x =
                 Float.of_int(start.col)
                 -. (overflow_left ? multiline_open_child_border_width : 0.0);
               let min_y =
                 Float.of_int(start.row) +. (vtrim_top ? tessera_margin : 0.);
               let height =
                 Float.of_int(MeasuredLayout.height(m))
                 -. (vtrim_top ? tessera_margin : 0.)
                 -. (vtrim_bot ? tessera_margin : 0.);
               SvgUtil.Rect.[
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
                 ~vtrim_bot=start.row == subject_height - 1,
                 start,
                 m,
               )
             | (Case, Tessera) =>
               tessera_padding(
                 ~vtrim_top=start.row == 0,
                 ~vtrim_bot=start.row == subject_height - 1,
                 ~overflow_left=false,
               )
             | (BinOp(_), Tessera) when has_multiline_open_child =>
               tessera_padding(
                 ~vtrim_top=false,
                 ~vtrim_bot=true,
                 ~overflow_left=true,
               )
             | (NTuple({comma_indices}), Tessera)
                 when has_multiline_open_child =>
               tessera_padding(
                 ~vtrim_top=
                   switch (m.layout) {
                   | Annot(Step(step), _)
                       when step == IntUtil.min(comma_indices) =>
                     false
                   | _ => true
                   },
                 ~vtrim_bot=true,
                 ~overflow_left=true,
               )
             | (_, Tessera) when has_multiline_open_child && start.col == 0 =>
               // may need to revisit above `when` guard
               // to support layouts like
               // let _ = \x.{
               //   _
               // } in ...
               // where lambda has offset head
               tessera_padding(
                 ~vtrim_top=start.row == 0,
                 ~vtrim_bot=start.row == subject_height - 1,
                 ~overflow_left=overflow_left(shape),
               )
             | _ => []
             };
           },
       );
  };

  let view =
      (
        ~corner_radii: (float, float),
        ~sort: TermSort.t,
        ~shape: TermShape.t,
        (offset, subject): UHMeasuredLayout.with_offset,
      )
      : Node.t => {
    let highlighted = {
      let tesserae = current_term_tessera_rects(~shape, (offset, subject));
      let open_child_borders =
        current_term_open_child_rects(~shape, (offset, subject));
      tesserae
      @ open_child_borders
      |> SvgUtil.OrthogonalPolygon.mk(~corner_radii)
      |> SvgUtil.Path.view(
           ~attrs=[Attr.classes(["code-current-term", sort_cls(sort)])],
         );
    };
    let closed_children =
      (offset, subject)
      |> current_term_closed_child_rects(~shape)
      |> List.map(((sort, rs)) =>
           rs
           |> SvgUtil.OrthogonalPolygon.mk(~corner_radii)
           |> SvgUtil.Path.view(
                ~attrs=
                  Attr.[classes(["code-closed-child", sort_cls(sort)])],
              )
         );
    let outer_filter =
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
      );
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
    );
  };
};

module ErrHole = {
  let view =
      (
        ~contains_current_term: bool,
        ~corner_radii: (float, float),
        (offset, subject): UHMeasuredLayout.with_offset,
      )
      : Node.t =>
    subject
    |> rects(
         ~vtrim=
           contains_current_term
             ? 0.0 : CurrentTerm.inline_open_child_border_height,
         {row: 0, col: offset},
       )
    |> SvgUtil.OrthogonalPolygon.mk(~corner_radii)
    |> SvgUtil.Path.view(
         ~attrs=
           Attr.[
             classes(["err-hole"]),
             create("vector-effect", "non-scaling-stroke"),
           ],
       );
};

module VarErrHole = {
  let view =
      (
        ~contains_current_term: bool,
        ~corner_radii: (float, float),
        (offset, subject): UHMeasuredLayout.with_offset,
      )
      : Node.t =>
    subject
    |> rects(
         ~vtrim=
           contains_current_term
             ? 0.0 : CurrentTerm.inline_open_child_border_height,
         {row: 0, col: offset},
       )
    |> SvgUtil.OrthogonalPolygon.mk(~corner_radii)
    |> SvgUtil.Path.view(
         ~attrs=
           Attr.[
             classes(["var-err-hole"]),
             create("vector-effect", "non-scaling-stroke"),
           ],
       );
};

module Caret = {
  let view =
      (~font_metrics: FontMetrics.t, {row, col}: MeasuredPosition.t): Node.t => {
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
    );
  };
};

let synth_tab = _width =>
  Node.div(
    Attr.[classes(["synthesizing-tab"])],
    [
      Node.span(
        [Attr.classes(["synthesizing-hole"])],
        [Node.text(UnicodeConstants.nbsp ++ "○")],
      ),
      Node.text(
        StringUtil.cat(UnicodeConstants.[nbsp, double_angle_right, nbsp]),
      ),
    ],
  );

module FilledHole = {
  let view =
      (
        ~font_metrics: FontMetrics.t,
        ~text: list(Node.t),
        ~decorations: list(Node.t),
        (offset, _subject): UHMeasuredLayout.with_offset,
      ) => {
    Node.div(
      [
        Attr.classes(["filled-hole-box"]),
        Attr.create(
          "style",
          Printf.sprintf(
            "top: -1px; left: %fpx;",
            Float.of_int(offset) *. font_metrics.col_width,
          ),
        ),
      ],
      text @ decorations,
    );
  };
};

module FilledHoleZ = {
  let view =
      (
        ~font_metrics: FontMetrics.t,
        ~text: list(Node.t),
        ~decorations: list(Node.t),
        (offset, subject): UHMeasuredLayout.with_offset,
      ) => {
    let width = MeasuredLayout.width(subject);
    let tab = synth_tab(width);
    Node.div(
      [
        Attr.classes(["synthesized-menu"]),
        Attr.create(
          "style",
          Printf.sprintf(
            "top: -1px; left: %fpx;",
            Float.of_int(offset) *. font_metrics.col_width,
          ),
        ),
      ],
      [
        tab,
        Node.div(
          [Attr.classes(["synthesized-option"])],
          text @ decorations,
        ),
      ],
    );
  };
};

module FillingHole = {
  open Shmyth;

  let rec natlist_dhexp_to_string_list = (dhexp: DHExp.t): list(string) => {
    switch (dhexp) {
    | ListNil(_) => []
    | Cons(IntLit(n), cdr) =>
      [string_of_int(n)] @ natlist_dhexp_to_string_list(cdr)
    | _ => failwith("ERROR: natlist_dhexp_to_string: malformed natlist lit")
    };
  };

  let rec constraint_dhexp_to_string = (constraint_dhexp: DHExp.t): string => {
    switch (constraint_dhexp) {
    | IntLit(n) => string_of_int(n)
    | BoolLit(n) => string_of_bool(n)
    | BoundVar(str) => str
    | ListNil(_) => "[]"
    | Cons(_) =>
      "["
      ++ String.concat(", ", natlist_dhexp_to_string_list(constraint_dhexp))
      ++ "]"
    | _ =>
      print_endline("ERROR: constraint_dhexp_to_string:");
      print_endline(
        Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(constraint_dhexp)),
      );
      "?";
    };
  }
  and constraint_ex_to_string = (constraint_ex: Shmyth.hexample): string => {
    switch (constraint_ex) {
    | Ex(dhexp) => constraint_dhexp_to_string(dhexp)
    | ExIO(xs) =>
      let strs = List.map(constraint_dhexp_to_string, xs);
      "λ." ++ String.concat("→", strs);
    };
  };

  let constraints_to_text_table = (constraints: constraint_data) => {
    let headers =
      switch (constraints) {
      | [] => []
      | [(_ex, env), ..._] =>
        let env_names = List.map(((id_str, _v)) => id_str, env);
        ["=", ...env_names];
      };
    let rows =
      constraints
      |> List.map(((ex, env)) => {
           let ex_str = constraint_ex_to_string(ex);
           let env_values =
             List.map(
               ((_id_str, v)) => constraint_dhexp_to_string(v),
               env,
             );
           [ex_str, ...env_values];
         });
    switch (headers) {
    | [] => []
    | h => [h] @ rows
    };
  };

  [@deriving sexp]
  type constraint_str_table = list(list(string));

  let make_row = (row_fn, row_data) => {
    Node.tr(
      [Attr.classes([])],
      List.map(entry => row_fn([], [Node.text(entry)]), row_data),
    );
  };

  let constraints_table = constraints => {
    let contents =
      switch (constraints_to_text_table(constraints)) {
      | [] => []
      | [header, ...rows] => [
          make_row(Node.th, header),
          ...List.map(make_row(Node.td), rows),
        ]
      };
    Node.table([Attr.classes(["synth-constraints"])], contents);
  };

  let view =
      (
        ~font_metrics: FontMetrics.t,
        ~options: ZList.t(Node.t, Node.t),
        ~constraints: Shmyth.constraint_data,
        (offset, subject): UHMeasuredLayout.with_offset,
      ) => {
    let width = MeasuredLayout.width(subject);
    let tab = synth_tab(width);
    let options =
      options
      |> ZList.map(
           o => Node.div([Attr.classes(["selected-filling"])], [o]),
           o => Node.div([], [o]),
         )
      |> (wrapped => ZList.erase(wrapped, x => x));
    Node.div(
      [
        Attr.classes(["synthesizing-menu"]),
        Attr.create(
          "style",
          Printf.sprintf(
            "top: -1px; left: %fpx;",
            Float.of_int(offset) *. font_metrics.col_width,
          ),
        ),
      ],
      [
        tab,
        Node.div([Attr.classes(["synthesizing-options"])], options),
        Node.div([], []),
        constraints_table(constraints),
      ],
    );
  };
};
