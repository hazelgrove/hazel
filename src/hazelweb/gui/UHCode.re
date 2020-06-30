module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open ViewUtil;
// open Sexplib.Std;

module MeasuredLayout = {
  type box = {
    height: int,
    width: int,
  };

  type t = {
    layout: t',
    metrics: list(box),
  }
  and t' =
    | Linebreak
    | Text(string)
    | Align(t)
    | Cat(t, t)
    | Annot(UHAnnot.t, t);

  let update_position = ((row: int, col: int), m: t): (int, int) => {
    let (leading, last) = ListUtil.split_last(m.metrics);
    let total_height =
      leading
      |> List.map(box => box.height)
      |> List.fold_left((+), last.height);
    let updated_row = row + total_height - 1;
    let updated_col =
      switch (leading) {
      | [] => col + last.width
      | [_, ..._] => last.width
      };
    (updated_row, updated_col);
  };

  // flattens away Linebreak and Cat nodes
  let flatten = (m: t): list(list(t)) => {
    let rec go = (~tail: list(list(t)), m: t): list(list(t)) =>
      switch (m.layout) {
      | Text(_)
      | Align(_)
      | Annot(_) =>
        switch (tail) {
        | [] => [[m]]
        | [row, ...rows] => [[m, ...row], ...rows]
        }
      | Linebreak => [[], ...tail]
      | Cat(m1, m2) => go(~tail=go(~tail, m2), m1)
      };
    go(~tail=[], m);
  };

  let table: WeakMap.t(UHLayout.t, t) = WeakMap.mk();
  let rec mk = (l: UHLayout.t): t => {
    switch (WeakMap.get(table, l)) {
    | Some(m) => m
    | None =>
      let m =
        switch (l) {
        | Linebreak =>
          let box = {height: 1, width: 0};
          {metrics: [box, box], layout: Linebreak};
        | Text(s) => {
            metrics: [{height: 1, width: StringUtil.utf8_length(s)}],
            layout: Text(s),
          }
        | Align(l) =>
          let m = mk(l);
          let bounding_box =
            m.metrics
            |> List.fold_left(
                 ({height: bh, width: bw}, {height, width}) =>
                   {height: bh + height, width: max(bw, width)},
                 {height: 0, width: 0},
               );
          {metrics: [bounding_box], layout: Align(m)};
        | Cat(l1, l2) =>
          let m1 = mk(l1);
          let m2 = mk(l2);
          let (leading, last) = ListUtil.split_last(m1.metrics);
          let (first, trailing) = ListUtil.split_first(m2.metrics);
          let mid_box = {
            height: max(last.height, first.height),
            width: last.width + first.width,
          };
          {metrics: leading @ [mid_box, ...trailing], layout: Cat(m1, m2)};
        | Annot(annot, l) =>
          let m = mk(l);
          {...m, layout: Annot(annot, m)};
        };
      ignore(WeakMap.set(table, l, m));
      m;
    };
  };
};

module Decoration = {
  let rects =
      (~row: int, ~col: int, m: MeasuredLayout.t)
      : ((int, int), list(RectilinearPolygon.rect)) =>
    m.metrics
    |> ListUtil.map_with_accumulator(
         ((row, col), box: MeasuredLayout.box) =>
           (
             (row + box.height - 1, 0),
             RectilinearPolygon.{
               min: {
                 x: Float.of_int(col),
                 y: Float.of_int(row),
               },
               width: Float.of_int(box.width),
               height: Float.of_int(box.height),
             },
           ),
         (row, col),
       );

  let err_hole_view =
      (
        ~corner_radii: (float, float),
        ~offset: int,
        subject: MeasuredLayout.t,
      )
      : Vdom.Node.t =>
    subject
    |> rects(~row=0, ~col=offset)
    |> snd
    |> RectilinearPolygon.mk_svg(
         ~corner_radii,
         ~attrs=[Vdom.Attr.classes(["code-err-hole"])],
       );

  let closed_child_view =
      (
        ~corner_radii: (float, float),
        ~offset: int,
        subject: MeasuredLayout.t,
      )
      : Vdom.Node.t =>
    subject
    |> rects(~row=0, ~col=offset)
    |> snd
    |> RectilinearPolygon.mk_svg(
         ~corner_radii,
         ~attrs=[Vdom.Attr.classes(["code-closed-child"])],
       );

  let current_term_view =
      (
        ~corner_radii: (float, float),
        ~offset: int,
        // ~shape: TermShape.t,
        subject: MeasuredLayout.t,
      )
      : Vdom.Node.t => {
    let flattened = MeasuredLayout.flatten(subject);
    let highlighted =
      flattened
      |> ListUtil.map_with_accumulator(
           ((row, col) as rc, line: list(MeasuredLayout.t)) =>
             switch (line) {
             | [{layout: Annot(OpenChild(_), m), _}] =>
               // make singleton skinny rect
               let height =
                 m.metrics
                 |> List.map((box: MeasuredLayout.box) => box.height)
                 |> List.fold_left((+), 0);
               let rects = [
                 RectilinearPolygon.{
                   min: {
                     x: Float.of_int(col),
                     y: Float.of_int(row),
                   },
                   height: Float.of_int(height),
                   width: 0.1,
                 },
               ];
               let next_rc = MeasuredLayout.update_position(rc, m);
               (next_rc, rects);
             | _ =>
               let (next_rc, rects) =
                 line
                 |> ListUtil.map_with_accumulator(
                      ((row, col) as rc, word: MeasuredLayout.t) => {
                        switch (word) {
                        | {layout: Annot(DelimGroup, m), _} =>
                          // get outline rects
                          rects(~row, ~col, m)
                        | {layout: Annot(OpenChild(_), m), _} =>
                          // make singleton skinny rect
                          let height =
                            m.metrics
                            |> List.map((box: MeasuredLayout.box) =>
                                 box.height
                               )
                            |> List.fold_left((+), 0);
                          let rects = [
                            RectilinearPolygon.{
                              min: {
                                x: Float.of_int(col),
                                y: Float.of_int(row),
                              },
                              height: Float.of_int(height),
                              width: 0.1,
                            },
                          ];
                          let next_rc = MeasuredLayout.update_position(rc, m);
                          (next_rc, rects);
                        | _ => failwith("unexpected flattened child of term")
                        }
                      },
                      (row, col),
                    );
               (next_rc, List.flatten(rects));
             },
           (0, offset),
         )
      |> snd
      |> List.flatten
      |> RectilinearPolygon.mk_svg(
           ~corner_radii,
           ~attrs=Vdom.[Attr.classes(["code-current-term"])],
         );

    let closed_children =
      flattened
      |> ListUtil.map_with_accumulator(
           ((row, col), line: list(MeasuredLayout.t)) => {
             let (next_rc, vs) =
               line
               |> ListUtil.map_with_accumulator(
                    ((row, col) as rc, word: MeasuredLayout.t) => {
                      let vs =
                        switch (word) {
                        | {layout: Annot(DelimGroup, m), _} =>
                          MeasuredLayout.flatten(m)
                          |> ListUtil.map_with_accumulator(
                               ((row, col), line: list(MeasuredLayout.t)) => {
                                 let (next_rc, vs) =
                                   line
                                   |> ListUtil.map_with_accumulator(
                                        (
                                          (_, col) as rc,
                                          word: MeasuredLayout.t,
                                        ) => {
                                          let vs =
                                            switch (word) {
                                            | {
                                                layout:
                                                  Annot(ClosedChild(_), m),
                                                _,
                                              } => [
                                                closed_child_view(
                                                  ~corner_radii,
                                                  ~offset=col,
                                                  m,
                                                ),
                                              ]
                                            | _ => []
                                            };
                                          let next_rc =
                                            MeasuredLayout.update_position(
                                              rc,
                                              m,
                                            );
                                          (next_rc, vs);
                                        },
                                        (row, col),
                                      );
                                 (next_rc, List.flatten(vs));
                               },
                               (row, col),
                             )
                          |> snd
                          |> List.flatten
                        | _ => []
                        };
                      let next_rc = MeasuredLayout.update_position(rc, word);
                      (next_rc, vs);
                    },
                    (row, col),
                  );
             (next_rc, List.flatten(vs));
           },
           (0, offset),
         )
      |> snd
      |> List.flatten;

    Vdom.(Node.create_svg("g", [], [highlighted, ...closed_children]));
  };

  let view =
      (
        ~corner_radius=3.0, // px
        ~font_metrics: FontMetrics.t,
        ~row: int,
        ~col: int,
        ~offset: int,
        // ~shape: TermShape.t,
        ~subject: MeasuredLayout.t,
        d: Decoration.t,
      ) => {
    let num_rows =
      subject.metrics
      |> List.map((box: MeasuredLayout.box) => box.height)
      |> List.fold_left((+), 0);
    let buffered_height =
      Float.of_int(num_rows + 1) *. font_metrics.row_height;

    let num_cols =
      subject.metrics
      |> List.map((box: MeasuredLayout.box) => box.width)
      |> List.fold_left(max, 0);
    let buffered_width = Float.of_int(num_cols + 1) *. font_metrics.col_width;

    let corner_radii = (
      corner_radius /. font_metrics.col_width,
      corner_radius /. font_metrics.row_height,
    );

    let v =
      switch (d) {
      | ErrHole => err_hole_view(~corner_radii, ~offset, subject)
      | CurrentTerm => current_term_view(~corner_radii, ~offset, subject)
      };

    let cls =
      switch (d) {
      | ErrHole => "err-hole"
      | CurrentTerm => "current-term"
      };

    Vdom.(
      Node.div(
        [
          Attr.classes([Printf.sprintf("%s-container", cls)]),
          Attr.create(
            "style",
            Printf.sprintf(
              "top: %flh;left: %fch;",
              Float.of_int(row) -. 0.5,
              Float.of_int(col) -. 0.5,
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
};

let clss_of_err: ErrStatus.t => list(cls) =
  fun
  | NotInHole => []
  | InHole(_) => ["InHole"];

let clss_of_verr: VarErrStatus.t => list(cls) =
  fun
  | NotInVarHole => []
  | InVarHole(_) => ["InVarHole"];

let clss_of_case_err: CaseErrStatus.t => list(cls) =
  fun
  | StandardErrStatus(err) => clss_of_err(err)
  | InconsistentBranches(_) => ["InconsistentBranches"];

let cursor_clss = (has_cursor: bool): list(cls) =>
  has_cursor ? ["Cursor"] : [];

let sort_clss: TermSort.t => list(cls) =
  fun
  | Typ => ["Typ"]
  | Pat => ["Pat"]
  | Exp => ["Exp"];

let shape_clss: TermShape.t => list(cls) =
  fun
  | Rule => ["Rule"]
  | Case({err}) => ["Case", ...clss_of_case_err(err)]
  | Var({err, verr, show_use}) =>
    ["Operand", "Var", ...clss_of_err(err)]
    @ clss_of_verr(verr)
    @ (show_use ? ["show-use"] : [])
  | Operand({err}) => ["Operand", ...clss_of_err(err)]
  | BinOp({err, op_index: _}) => ["BinOp", ...clss_of_err(err)]
  | NTuple({err, comma_indices: _}) => ["NTuple", ...clss_of_err(err)]
  | SubBlock(_) => ["SubBlock"];

let open_child_clss = (has_inline_OpenChild: bool, has_para_OpenChild: bool) =>
  List.concat([
    has_inline_OpenChild ? ["has-Inline-OpenChild"] : [],
    has_para_OpenChild ? ["has-Para-OpenChild"] : [],
  ]);

let has_child_clss = (has_child: bool) =>
  has_child ? ["has-child"] : ["no-children"];

let caret_from_pos = (x: float, y: float): Vdom.Node.t => {
  let pos_attr =
    Vdom.Attr.style(
      Css_gen.combine(
        Css_gen.left(`Px(int_of_float(Float.round(x)))),
        Css_gen.top(`Px(int_of_float(Float.round(y)))),
      ),
    );
  Vdom.Node.span(
    [Vdom.Attr.id("caret"), pos_attr, Vdom.Attr.classes(["blink"])],
    [],
  );
};

let decoration_views =
    (~font_metrics: FontMetrics.t, ds: Decorations.t, l: UHLayout.t)
    : list(Vdom.Node.t) => {
  let rec go =
          (
            ~tl: list(Vdom.Node.t),
            ~indent: int,
            ~row: int,
            ~col: int,
            ds: Decorations.t,
            m: MeasuredLayout.t,
          )
          : list(Vdom.Node.t) => {
    let go' = go(~indent, ~row, ~col);
    switch (m.layout) {
    | Linebreak
    | Text(_) => tl
    | Cat(m1, m2) =>
      let mid_row = {
        let height1 =
          m1.metrics
          |> List.map((box: MeasuredLayout.box) => box.height)
          |> List.fold_left((+), 0);
        row + height1 - 1;
      };
      let mid_col = {
        let (leading, MeasuredLayout.{width, _}) =
          ListUtil.split_last(m1.metrics);
        let offset =
          switch (leading) {
          | [] => col
          | [_, ..._] => indent
          };
        offset + width;
      };
      let mid_tl = go(~tl, ~indent, ~row=mid_row, ~col=mid_col, ds, m2);
      go'(~tl=mid_tl, ds, m1);
    | Align(m) => go(~tl, ~indent=col, ~row, ~col, ds, m)
    | Annot(annot, m) =>
      switch (annot) {
      | Step(step) =>
        let stepped = Decorations.take_step(step, ds);
        Decorations.is_empty(stepped) ? tl : go'(~tl, stepped, m);
      | Term(_) =>
        let current_vs =
          Decorations.current(ds)
          |> List.map(
               Decoration.view(
                 ~font_metrics,
                 ~row,
                 ~col=indent,
                 ~offset=col - indent,
                 // ~shape,
                 ~subject=m,
               ),
             );
        go'(~tl=current_vs @ tl, ds, m);
      | _ => go'(~tl, ds, m)
      }
    };
  };
  go(~tl=[], ~indent=0, ~row=0, ~col=0, ds, MeasuredLayout.mk(l));
};

let view =
    (
      ~model: Model.t,
      ~inject: Update.Action.t => Vdom.Event.t,
      ~font_metrics: FontMetrics.t,
      // ~caret_pos: option((int, int)),
      ~decorations: Decorations.t,
      l: UHLayout.t,
    )
    : Vdom.Node.t => {
  TimeUtil.measure_time(
    "UHCode.view",
    model.measurements.measurements && model.measurements.uhcode_view,
    () => {
      open Vdom;

      let rec go: UHBox.t => list(Node.t) =
        fun
        | Text(s) => StringUtil.is_empty(s) ? [] : [Node.text(s)]
        | HBox(boxes) => List.concat(List.map(go, boxes))
        | VBox(boxes) => {
            let vs =
              boxes
              |> List.map(go)
              |> List.flatten
              |> ListUtil.join(Node.br([]));
            [Node.div([Attr.style(Css_gen.display(`Inline_block))], vs)];
          }
        | Annot(annot, box) => {
            let vs = go(box);
            switch (annot) {
            | Token({shape, _}) =>
              let clss =
                switch (shape) {
                | Text => ["code-text"]
                | Op => ["code-op"]
                | Delim(_) => ["code-delim"]
                };
              [Node.span([Attr.classes(clss)], vs)];
            | HoleLabel({len}) =>
              let width = Css_gen.width(`Ch(float_of_int(len)));
              [
                Node.span(
                  [Vdom.Attr.style(width), Attr.classes(["HoleLabel"])],
                  [Node.span([Attr.classes(["HoleNumber"])], vs)],
                ),
              ];
            | UserNewline => [
                Node.span([Attr.classes(["UserNewline"])], vs),
              ]
            | _ => vs
            };
          };

      let code_text = go(Box.mk(l));
      let decorations = decoration_views(~font_metrics, decorations, l);

      /*
       let children =
         switch (caret_pos) {
         | None => vs
         | Some((row, col)) =>
           let x = float_of_int(col) *. model.font_metrics.col_width;
           let y = float_of_int(row) *. model.font_metrics.row_height;
           let caret = caret_from_pos(x, y);
           [caret, ...vs];
         };
       */

      let id = "code-root";
      Node.div(
        [
          Attr.id(id),
          Attr.classes(["code", "presentation"]),
          // need to use mousedown instead of click to fire
          // (and move caret) before cell focus event handler
          Attr.on_mousedown(evt => {
            let container_rect =
              JSUtil.force_get_elem_by_id(id)##getBoundingClientRect;
            let (target_x, target_y) = (
              float_of_int(evt##.clientX),
              float_of_int(evt##.clientY),
            );
            let row_col = (
              Float.to_int(
                (target_y -. container_rect##.top) /. font_metrics.row_height,
              ),
              Float.to_int(
                Float.round(
                  (target_x -. container_rect##.left) /. font_metrics.col_width,
                ),
              ),
            );
            inject(Update.Action.MoveAction(Click(row_col)));
          }),
        ],
        [Node.span([Attr.classes(["code"])], code_text), ...decorations],
      );
    },
  );
};
