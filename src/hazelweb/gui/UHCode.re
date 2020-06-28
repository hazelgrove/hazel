module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open ViewUtil;
open Sexplib.Std;

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
      : list(RectilinearPolygon.rect) =>
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
       )
    |> snd;

  let err_hole_view =
      (
        ~corner_radii: (float, float),
        ~offset: int,
        subject: MeasuredLayout.t,
      )
      : Vdom.Node.t =>
    subject
    |> rects(~row=0, ~col=offset)
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
    |> RectilinearPolygon.mk_svg(
         ~corner_radii,
         ~attrs=[Vdom.Attr.classes(["code-closed-child"])],
       );

  let current_term_view =
      (
        ~corner_radii: (float, float),
        ~offset: int,
        ~shape: TermShape.t,
        subject: MeasuredLayout.t,
      )
      : Vdom.Node.t => {
    let flattened = MeasuredLayout.flatten(subject);
    let highlighted =
      flattened
      |> ListUtil.map_with_accumulator(
           ((row, col), line: list(MeasuredLayout.t)) =>
             switch (line) {
             | [{layout: Annot(OpenChild(_), m), _}] =>
               // make singleton skinny rect
               failwith("unimplemented")
             | _ =>
               line
               |> ListUtil.map_with_accumulator(
                    ((row, col) as rc, word: MeasuredLayout.t) =>
                      switch (word) {
                      | {layout: Annot(DelimGroup, m), _} =>
                        // get outline rects
                        (
                          MeasuredLayout.update_position(rc, word),
                          rects(~row, ~col, m),
                        )
                      | {layout: Annot(OpenChild(_), m), _} =>
                        // make singleton skinny rect
                        failwith("unimplemented")
                      | _ => failwith("unexpected flattened child of term")
                      },
                    (row, col),
                  )
               |> snd
               |> List.flatten
             },
           (0, offset),
         )
      |> snd
      |> List.flatten
      |> RectilinearPolygon.mk_svg(
           ~corner_radii,
           ~attrs=[Attr.classes(["code-current-term"])],
         );

    let closed_children =
      flattened
      |> ListUtil.map_with_accumulator(
           ((row, col), line: list(MeasuredLayout.t)) => {
             line
             |> ListUtil.map_with_accumulator(
                  ((row, col), word: MeasuredLayout.t) =>
                    switch (word) {
                    | {layout: Annot(DelimGroup, m), _} =>
                      MeasuredLayout.flatten(m)
                      |> ListUtil.map_with_accumulator(
                           ((row, col), line: list(MeasuredLayout.t)) => {
                             line
                             |> ListUtil.map_with_accumulator(
                                  ((row, col), word: list(MeasuredLayout.t)) =>
                                    switch (word) {
                                    | {layout: Annot(ClosedChild, m), _} => [
                                        closed_child_view(
                                          ~corner_radii,
                                          ~offset=col,
                                          m,
                                        ),
                                      ]
                                    | _ => []
                                    },
                                  (row, col),
                                )
                             |> snd
                             |> List.flatten
                           },
                           (row, col),
                         )
                      |> snd
                      |> List.flatten
                    | _ => []
                    },
                  (row, col),
                )
             |> snd
             |> List.flatten
           },
           (0, offset),
         )
      |> snd
      |> List.flatten;

    Vdom.(Node.create_svg("g", [], [current_term_view, ...closed_children]));
  };
  /*
     let view = (
       ~corner_radius=3, // px
       ~font_metrics: FontMetrics.t,
       ~row: int,
       ~col: int,
       ~offset: int,
       ~shape: TermShape.t,
       ~subject: MeasuredLayout.t,
       d: t,
     ) => {
       let num_rows =
         subject.metrics
         |> List.map(box => box.height)
         |> List.fold_left((+), 0);
       let buffered_height =
         Float.of_int(num_rows + 1) *. font_metrics.row_height;

       let num_cols =
         subject.metrics
         |> List.map(box => box.width)
         |> List.fold_left(max);
       let buffered_width =
         Float.of_int(num_cols + 1) *. font_metrics.col_width;

       let corner_radii = (
         corner_radius /. font_metrics.col_width,
         corner_radius /. font_metrics.row_height,
       );

       let vs =
         switch (d) {
         | ErrHole => err_hole_view(~corner_radii, ~offset, subject)
         | CurrentTerm => current_term_view(~corner_radii, ~offset, ~shape, subject)
         };

       Vdom.(
         Node.div(
           [
             Attr.create(
               "style",
               Printf.sprintf(
                 "top: %flh;left: %fch;",
                 Float.of_int(row) -. 0.5,
                 Float.of_int(col) -. 0.5,
               )
             )
           ],
           [Node.create_svg(
             "svg",
             [
               Attr.create(
                 "viewBox",
                 Printf.sprintf("-0.5 -0.5 %d %d", num_cols + 1, num_rows + 1),
               ),
               Attr.create("width", Printf.sprintf("%fpx", buffered_width)),
               Attr.create("height", Printf.sprintf("%fpx", buffered_height)),
               Attr.create("preserveAspectRatio", "none"),
             ],
             vs,
           )],
         )
       );
     };
   */
  /*
   let current_term_view =
       (
         ~font_metrics: FontMetrics.t,
         ~hd_offset: int,
         shape: TermShape.t,
         m: MeasuredLayout.t,
       ): Vdom.Node.t => {
     open SvgUtil;

     let FontMetrics.{row_height, col_width} = font_metrics;

     let corner_radius = 4.0;
     let (rx, ry) as corner_radii = (
       corner_radius /. col_width,
       corner_radius /. row_height,
     );

     let row_paths = (
       ~is_first: bool,
       ~is_last: bool,
       ms: list(MeasuredLayout.t)
     ): (Path.t, Path.t) => {
       ms
       |> ListUtil.map_sep(
         ({layout, _}) =>
           switch (layout) {
           | Annot(DelimGroup, m) => delim_group_paths(m)
           | Annot(OpenChild, m) => open_child_paths(m)
           | _ => failwith("unexpected layout")
           },
         _ => ([GoStraight(line_height -. open_child_border_thickness)]),
       )

       ListUtil.pairs(ls)
       |> List.map(
         fun
         | (Annot(DelimGroup, _), Annot(OpenChild, _)) =>

       )
     };

     let height = l.boxes |> List.map(fst) |> List.fold_left((+), 0);
     let width =
       l.boxes |> List.map(snd) |> List.fold_left(max, 0);

     let rows = UHLayout.flatten(l);
     let num_rows = List.length(rows);

     let row_sep = 1.0;
     let open_child_border_width = 2.0;

     let outline_path = {
       let ragged_right_path =
         rows
         |> List.mapi((i, row) =>
           row_path(
             ~is_first=i == 0,
             ~is_last=i == num_rows - 1,
             row,
           )
         )
         |> SvgUtil.BoxPath.concat;
       let total_height =
         rows
         |> List.map(
           List.fold_left(
             (max_height, l) => max(max_height, l),
             0,
           )
         )
         |> List.fold_left((+), 0);
       SvgUtil.BoxPath.view(
         ~radii=corner_radii,
         ~initial_position=(0.0, row_sep /. 2.0),
         ~initial_orientation=(Y, Pos),
         [Turn90(CW), ...ragged_right_path] @ [Turn90(CW), GoStraight(total_height)]
       );
     };

     svg_container(
       ~font_metrics,
       ~html_attrs=[Attr.classes(["current-term"])],
       ~svg_attrs=[
         Attr.create("stroke", "red"),
         Attr.create("stroke-dasharray", "3 2"),
         Attr.create("preserveAspectRatio", "none"),
       ],
       ~height,
       ~width,
       [outline_path]
     );
   };
   */
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

let decorations =
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
          m1.metrics |> List.map(box => box.height) |> List.fold_left((+), 0);
        row + height1 - 1;
      };
      let mid_col = {
        let (leading, {width, _}) = ListUtil.split_last(m1.metrics);
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
      | Term({shape, _}) =>
        let current_vs =
          Decorations.current(ds)
          |> List.map(
               Decoration.view(
                 ~font_metrics,
                 ~row,
                 ~col=indent,
                 ~offset=col - indent,
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
      ~caret_pos: option((int, int)),
      l: UHLayout.t,
    )
    : Vdom.Node.t => {
  TimeUtil.measure_time(
    "UHCode.view",
    model.measurements.measurements && model.measurements.uhcode_view,
    () => {
      open Vdom;

      let (_, vs) = mem_view(~offset=0, l);
      let children =
        switch (caret_pos) {
        | None => vs
        | Some((row, col)) =>
          let x = float_of_int(col) *. model.font_metrics.col_width;
          let y = float_of_int(row) *. model.font_metrics.row_height;
          let caret = caret_from_pos(x, y);
          [caret, ...vs];
        };
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
        children,
      );
    },
  );
};
