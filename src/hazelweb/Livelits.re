open Sexplib.Std;

module Dom_html = Js_of_ocaml.Dom_html;
module Dom = Js_of_ocaml.Dom;
module Js = Js_of_ocaml.Js;
open Virtual_dom.Vdom;

module LivelitView = {
  type div_type = Node.t;

  type splice_and_param_getters = {
    uhcode: SpliceName.t => div_type,
    dhcode: SpliceName.t => option((DHExp.t, div_type)),
    dargs: option(list((Var.t, option((DHExp.t, div_type))))),
  };

  type t = splice_and_param_getters => div_type;
};

module type LIVELIT_VIEW = {
  [@deriving sexp]
  type model;
  [@deriving sexp]
  type action;

  type trigger = action => Event.t;
  type sync = action => unit;
  let view: (model, trigger, sync) => LivelitView.t;
  let view_shape: model => LivelitShape.t;
};

module type LIVELIT = {
  let name: LivelitName.t;
  let expansion_ty: HTyp.t;
  let param_tys: list((Var.t, HTyp.t));

  [@deriving sexp]
  type model;
  [@deriving sexp]
  type action;
  type trigger = action => Event.t;
  type sync = action => unit;

  let view: (model, trigger, sync) => LivelitView.t;
  let view_shape: model => LivelitShape.t;
  let init_model: SpliceGenCmd.t(model);
  let update: (model, action) => SpliceGenCmd.t(model);
  let expand: model => UHExp.t;
};
module MkLivelit =
       (
         LC: BuiltinLivelits.LIVELIT_CORE,
         LV:
           LIVELIT_VIEW with
             type model := LC.model and type action := LC.action,
       )
       : LIVELIT => {
  include LC;
  include LV;
};

module LivelitViewAdapter = (L: LIVELIT) => {
  let serialize_monad = model => SpliceGenCmd.return(L.sexp_of_model(model));

  let serialized_view_fn = (serialized_model, trigger, sync) =>
    L.view(
      L.model_of_sexp(serialized_model),
      action => trigger(L.sexp_of_action(action)),
      action => sync(L.sexp_of_action(action)),
    );

  let serialized_view_shape_fn = serialized_model =>
    L.view_shape(L.model_of_sexp(serialized_model));

  let contexts_entry = (L.name, serialized_view_fn, serialized_view_shape_fn);
};

type trigger_serialized = SerializedAction.t => Event.t;
type sync_serialized = SerializedAction.t => unit;
type serialized_view_fn_t =
  (SerializedModel.t, trigger_serialized, sync_serialized) => LivelitView.t;
type serialized_view_shape_fn_t = SerializedModel.t => LivelitShape.t;

module LivelitViewCtx = {
  type t = VarMap.t_((serialized_view_fn_t, serialized_view_shape_fn_t));
  include VarMap;
};

module LivelitViewContexts = {
  type t = LivelitViewCtx.t;
  let empty = LivelitViewCtx.empty;
  let extend =
      (
        livelit_view_ctx,
        (name, serialized_view_fn, serialized_view_shape_fn),
      ) => {
    if (!LivelitName.is_valid(name)) {
      failwith("Invalid livelit name " ++ name);
    };
    VarMap.extend(
      livelit_view_ctx,
      (name, (serialized_view_fn, serialized_view_shape_fn)),
    );
  };
};

let _to_uhvar = id => UHExp.var(SpliceInfo.var_of_splice_name(id));

let attr_style = Attr.create("style");

let prop_val = (prop: string, value: string) =>
  StringUtil.cat([prop, ": ", value, ";"]);

module PairLivelitView = {
  [@deriving sexp]
  type model = (int, int);
  [@deriving sexp]
  type action = unit;
  type trigger = action => Event.t;
  type sync = action => unit;

  let view =
      (
        (leftID, rightID),
        _,
        _,
        {uhcode, _}: LivelitView.splice_and_param_getters,
      ) =>
    Node.div(
      [Attr.classes(["pair-livelit"])],
      [uhcode(leftID), uhcode(rightID)],
    );
  let view_shape = _ =>
    LivelitShape.Inline(
      // TODO fix brittle magic constant
      20,
    );
};

module MatrixLivelitView = {
  // assume nonzero height and width
  [@deriving sexp]
  type model = BuiltinLivelits.MatrixLivelitCore.model;
  [@deriving sexp]
  type dim = BuiltinLivelits.MatrixLivelitCore.dim;
  [@deriving sexp]
  type action = BuiltinLivelits.MatrixLivelitCore.action;
  type trigger = action => Event.t;
  type sync = action => unit;

  let init_height = 2;
  let init_width = 2;

  let get_height = (m: list(list(SpliceName.t))): int => List.length(m);
  let get_width = (m: list(list(SpliceName.t))): int =>
    List.length(List.hd(m));

  let grid_area = (row_start, col_start, row_end, col_end) =>
    prop_val(
      "grid-area",
      Printf.sprintf(
        "%d / %d / %d / %d",
        row_start,
        col_start,
        row_end,
        col_end,
      ),
    );

  let view =
      (
        (selected, m),
        trig,
        _,
        {uhcode, dhcode, _}: LivelitView.splice_and_param_getters,
      ) => {
    let width = get_width(m);
    let height = get_height(m);
    let row_header =
      ListUtil.range(height)
      |> List.map(i =>
           Node.div(
             [
               attr_style(grid_area(i + 3, 1, i + 4, 2)),
               Attr.classes(["row-header", "pure-button"]),
               Attr.on_click(_ => trig(Del(Row, i): action)),
             ],
             [
               Node.span(
                 [Attr.classes(["index"])],
                 [Node.text(string_of_int(i + 1))],
               ),
               Node.span([Attr.classes(["delete"])], [Node.text("x")]),
             ],
           )
         );
    let col_header =
      ListUtil.range(width)
      |> List.map(j =>
           Node.div(
             [
               attr_style(grid_area(1, j + 3, 2, j + 4)),
               Attr.classes(["col-header", "pure-button"]),
               Attr.on_click(_ => trig(Del(Col, j))),
             ],
             [
               Node.span(
                 [Attr.classes(["index"])],
                 [Node.text(string_of_int(j + 1))],
               ),
               Node.span([Attr.classes(["delete"])], [Node.text("x")]),
             ],
           )
         );
    let add_row_button =
      Node.button(
        [
          attr_style(grid_area(-1, 3, -2, -3)),
          Attr.classes(["add-row", "pure-button"]),
          Attr.on_click(_ => trig(Add(Row))),
        ],
        [Node.text("+")],
      );
    let add_col_button =
      Node.button(
        [
          attr_style(grid_area(3, -2, -3, -1)),
          Attr.classes(["add-col", "pure-button"]),
          Attr.on_click(_ => trig(Add(Col))),
        ],
        [Node.text("+")],
      );
    let maybe_add_formula_bar = rest =>
      if (!true) {
        // TODO: refactor to remove this check
        rest;
      } else {
        Node.div(
          [Attr.classes(["matrix-livelit"])],
          [
            Node.div(
              [Attr.classes(["matrix-formula-bar"])],
              [
                Node.span(
                  [Attr.classes(["matrix-formula-bar-text"])],
                  [Node.text("selected cell's formula: ")],
                ),
                Node.div(
                  [Attr.classes(["matrix-formula-bar-splice"])],
                  [uhcode(selected)],
                ),
              ],
            ),
            rest,
          ],
        );
      };
    let cells =
      m
      |> List.mapi((i, row) =>
           row
           |> List.mapi((j, splice) => {
                let contents =
                  if (!true) {
                    // TODO: refactor to remove this check
                    Node.div(
                      [Attr.classes(["matrix-splice"])],
                      [uhcode(splice)],
                    );
                  } else {
                    let cls =
                      splice == selected
                        ? "matrix-selected" : "matrix-unselected";
                    let child =
                      switch (dhcode(splice)) {
                      | None => Node.text("Uneval'd")
                      | Some((_, view)) => view
                      };
                    Node.div(
                      [
                        Attr.classes([cls]),
                        Attr.on_mousedown(_ => trig(Select(splice))),
                      ],
                      [child],
                    );
                  };
                Node.div(
                  [
                    attr_style(grid_area(i + 3, j + 3, i + 4, j + 4)),
                    Attr.classes(["matrix-cell"]),
                  ],
                  [contents],
                );
              })
         )
      |> List.flatten;

    let cells_border =
      Node.div(
        [
          attr_style(grid_area(3, 3, -3, -3)),
          Attr.classes(["cells-border"]),
        ],
        [],
      );

    let gutter_width = "10px";
    let dim_template = dim =>
      StringUtil.sep(
        List.concat([
          ["auto", gutter_width],
          ListUtil.replicate(dim, "auto"),
          [gutter_width, "auto"],
        ]),
      );
    maybe_add_formula_bar(
      Node.div(
        [
          Attr.classes(["matrix-livelit"]),
          attr_style(
            StringUtil.cat([
              prop_val("grid-template-columns", dim_template(width)),
              prop_val("grid-template-rows", dim_template(height)),
            ]),
          ),
        ],
        List.concat([
          row_header,
          col_header,
          [cells_border, ...cells],
          [add_row_button, add_col_button],
        ]),
      ),
    );
  };

  let view_shape = ((_, matrix)) => {
    let num_rows = List.length(matrix);
    LivelitShape.MultiLine(3 * num_rows + 2 + (true ? 1 : 0)); // TODO: cleanup
  };
};

module GradeCutoffLivelitView = {
  [@deriving sexp]
  type letter_grade = BuiltinLivelits.GradeCutoffLivelitCore.letter_grade;

  [@deriving sexp]
  type model = BuiltinLivelits.GradeCutoffLivelitCore.model;

  [@deriving sexp]
  type action = BuiltinLivelits.GradeCutoffLivelitCore.action;

  type trigger = action => Event.t;
  type sync = action => unit;

  let is_valid_percentage = (p: float) => 0. <= p && p <= 100.;

  let grades_invalids_to_svgs = ((grades, invalid_count)) => {
    let valid_grades =
      grades
      |> List.filter_map(grade =>
           if (0. <= grade && grade <= 100.) {
             Some(
               Node.create_svg(
                 "circle",
                 [
                   Attr.create("cx", Printf.sprintf("%f", grade)),
                   Attr.create("cy", "0"),
                   Attr.create("r", "0.75"),
                   Attr.create("fill", "orange"),
                   Attr.create("stroke-width", "0"),
                 ],
                 [],
               ),
             );
           } else {
             None;
           }
         );
    (
      valid_grades,
      invalid_count + List.length(grades) - List.length(valid_grades),
    );
  };

  let rec dhexp_to_grades_invalids = (rslt, invalid_count) =>
    fun
    | DHExp.Cons(DHExp.FloatLit(g), d) =>
      dhexp_to_grades_invalids([g, ...rslt], invalid_count, d)
    | DHExp.Cons(_, d) =>
      dhexp_to_grades_invalids(rslt, invalid_count + 1, d)
    | DHExp.ListNil(_) => (List.rev(rslt), invalid_count)
    | _ => (List.rev(rslt), invalid_count + 1);

  let view =
      (
        {a, b, c, d, selecting}: BuiltinLivelits.GradeCutoffLivelitCore.model,
        trigger,
        _,
        {dargs, _}: LivelitView.splice_and_param_getters,
      ) => {
    let data_opt =
      switch (dargs) {
      | None
      | Some([(_, None)]) => None
      | Some([(_, Some((d, _)))]) => Some(d)
      | Some(l) =>
        failwith(
          "Invalid grade_cutoffs params: "
          ++ (l |> List.map(((s, _)) => s) |> String.concat(", ")),
        )
      };
    /*
     let grades = [93, 88, 75, 86, 78, 82, 67, 54, 45, 71, 69, 62, 97, 83, 85];
     let data_opt =
       Some(
         grades
         |> List.fold_left(
              (acc, g) => DHExp.Cons(IntLit(g), acc),
              DHExp.ListNil(Int),
            ),
       );
     */
    let grades_invalids_opt =
      Option.map(dhexp_to_grades_invalids([], 0), data_opt);
    let grades_svgs_invalids_opt =
      Option.map(grades_invalids_to_svgs, grades_invalids_opt);
    let (grades_svgs, data_err_msg) =
      switch (grades_svgs_invalids_opt) {
      | None => ([], [Node.text("Grades data was never evaluated")])
      | Some((grades_svgs, invalid_count)) => (
          grades_svgs,
          if (invalid_count == 0) {
            [];
          } else if (invalid_count == 1) {
            [Node.text("1 grade was indeterminate or out of bounds")];
          } else {
            [
              Node.text(
                Printf.sprintf(
                  "%d grades were indeterminate or out of bounds",
                  invalid_count,
                ),
              ),
            ];
          },
        )
      };

    let px_scalar = 5.;
    let scale_len = 100.;
    let thumb_radius = 3.;
    let thumb_tip = 5.;

    let cutoff_thumb = (letter: letter_grade, v: float) => {
      open SvgUtil;
      let chord_distance = thumb_radius /. Float.sqrt(2.);
      let arc_start = Point.{x: v -. chord_distance, y: -. thumb_tip};
      let arc_end = Point.{x: v +. chord_distance, y: -. thumb_tip};
      let control = Point.{x: v, y: -. (thumb_tip -. chord_distance)};
      let origin = Point.{x: v, y: (-0.5)};
      let thumb =
        Path.[
          Q({control, target: arc_start}),
          A({
            rx: thumb_radius,
            ry: thumb_radius,
            x_axis_rotation: 0.,
            large_arc_flag: true,
            sweep_flag: true,
            target: arc_end,
          }),
          Q({control, target: origin}),
        ];
      Node.create_svg(
        "g",
        [],
        [
          Path.view(
            ~attrs=
              Attr.[
                classes(["grade-cutoff-thumb"]),
                create("vector-effect", "non-scaling-stroke"),
                on_mousedown(_ => trigger(StartSelecting(letter): action)),
              ],
            [M(origin), ...thumb],
          ),
          Node.create_svg(
            "text",
            Attr.[
              classes(["grade-label"]),
              create("vector-effect", "non-scaling-stroke"),
              create("dominant-baseline", "middle"),
              create("text-anchor", "middle"),
              create("x", Printf.sprintf("%f", v)),
              create(
                "y",
                Printf.sprintf("%f", -. (thumb_tip +. chord_distance -. 0.4)),
              ),
              on_mousedown(_ => trigger(StartSelecting(letter))),
            ],
            [
              Node.text(
                switch (letter) {
                | A => "A"
                | B => "B"
                | C => "C"
                | D => "D"
                },
              ),
            ],
          ),
        ],
      );
    };

    let percentage_line = grade_points => {
      let percentage_label = (p: int) =>
        Node.create_svg(
          "text",
          [
            Attr.classes(["grade-cutoff-scale-label"]),
            Attr.create("x", string_of_int(p)),
            Attr.create("y", "1"),
            Attr.create("dominant-baseline", "hanging"),
            Attr.create("text-anchor", "middle"),
            Attr.create("vector-effect", "non-scaling-stroke"),
          ],
          [Node.text(string_of_int(p))],
        );
      let percentage_labels =
        ListUtil.range(~lo=0, 11)
        |> List.map(( * )(10))
        |> List.map(percentage_label);
      Node.create_svg(
        "g",
        [],
        [
          Node.create_svg(
            "line",
            [
              Attr.classes(["grade-cutoff-scale"]),
              Attr.create("x1", "0"),
              Attr.create("y1", "0"),
              Attr.create("x2", Printf.sprintf("%f", scale_len)),
              Attr.create("y2", "0"),
              Attr.create("vector-effect", "non-scaling-stroke"),
            ],
            [],
          ),
          ...percentage_labels,
        ]
        @ grade_points,
      );
    };

    let distribution_line =
      switch (grades_invalids_opt) {
      | None => []
      | Some((grades, _)) =>
        let (fs, ds) = List.partition(g => g < d, grades);
        let (ds, cs) = List.partition(g => g < c, ds);
        let (cs, bs) = List.partition(g => g < b, cs);
        let (bs, as_) = List.partition(g => g < a, bs);
        let line_y = (-12.);
        // let line =
        //   Node.create_svg(
        //     "line",
        //     [
        //       Attr.classes(["distribution-line"]),
        //       Attr.create("x1", "0"),
        //       Attr.create("y1", string_of_float(line_y)),
        //       Attr.create("x2", Printf.sprintf("%f", scale_len)),
        //       Attr.create("y2", string_of_float(line_y)),
        //       Attr.create("vector-effect", "non-scaling-stroke"),
        //     ],
        //     [],
        //   );
        let labeled_bucket = (x1, x2, num) => {
          let len = (-0.75);
          let bucket =
            SvgUtil.Path.[
              M({x: x1, y: line_y -. len}),
              V({y: line_y}),
              H({x: x2}),
              V({y: line_y -. len}),
            ]
            |> SvgUtil.Path.view(
                 ~attrs=[
                   Attr.classes(["bucket-path"]),
                   Attr.create("vector-effect", "non-scaling-stroke"),
                 ],
               );
          let label =
            Node.create_svg(
              "text",
              [
                Attr.classes(["grade-cutoff-bucket-label"]),
                Attr.create("x", string_of_float((x1 +. x2) *. 0.5)),
                Attr.create("y", string_of_float(line_y -. 1.)),
                Attr.create("dominant-baseline", "auto"),
                Attr.create("text-anchor", "middle"),
                Attr.create("vector-effect", "non-scaling-stroke"),
              ],
              [Node.text(string_of_int(num))],
            );
          [bucket, label];
        };
        let buffer = 0.4;
        let fs_bucket = labeled_bucket(0., d -. buffer, List.length(fs));
        let ds_bucket =
          labeled_bucket(d +. buffer, c -. buffer, List.length(ds));
        let cs_bucket =
          labeled_bucket(c +. buffer, b -. buffer, List.length(cs));
        let bs_bucket =
          labeled_bucket(b +. buffer, a -. buffer, List.length(bs));
        let as_bucket = labeled_bucket(a +. buffer, 100., List.length(as_));
        fs_bucket @ ds_bucket @ cs_bucket @ bs_bucket @ as_bucket;
      };

    let thumbs = {
      let (a, b, c, d) = (
        cutoff_thumb(A, a),
        cutoff_thumb(B, b),
        cutoff_thumb(C, c),
        cutoff_thumb(D, d),
      );
      // change paint order depending on
      // which thumb is being dragged
      switch (selecting) {
      | None
      | Some(A) => [d, c, b, a]
      | Some(B) => [d, c, a, b]
      | Some(C) => [a, d, b, c]
      | Some(D) => [a, b, c, d]
      };
    };

    let overlay =
      switch (selecting) {
      | None => []
      | Some(letter) => [
          Node.div(
            Attr.[
              classes(["dragging-overlay"]),
              on_mouseup(_ => trigger(StopSelecting)),
              on_mousemove(evt => {
                let scale =
                  JSUtil.force_get_elem_by_cls("grade-cutoff-scale")##getBoundingClientRect;
                let offset_x = Float.of_int(Js.Unsafe.get(evt, "offsetX"));
                let cutoff =
                  100.
                  *. (offset_x -. scale##.left)
                  /. (px_scalar *. scale_len);
                trigger(UpdateCutoff(letter, cutoff));
              }),
            ],
            [],
          ),
        ]
      };

    let width = scale_len +. 2. *. thumb_radius +. 2.;
    let height = 2. *. thumb_radius +. thumb_tip +. 10.;
    Node.div(
      [Attr.classes(["grade-cutoffs-livelit"])],
      [
        Node.create_svg(
          "svg",
          [
            Attr.classes(["grade-display"]),
            Attr.create(
              "viewBox",
              Printf.sprintf(
                "%f %f %f %f",
                -. (thumb_radius +. 1.),
                -. (2. *. thumb_radius +. thumb_tip +. 6.),
                width,
                height,
              ),
            ),
            Attr.create("width", Printf.sprintf("%fpx", px_scalar *. width)),
            Attr.create(
              "height",
              Printf.sprintf("%fpx", px_scalar *. height),
            ),
            Attr.create("stroke", "black"),
          ],
          [percentage_line(grades_svgs), ...thumbs] @ distribution_line,
        ),
        Node.div([Attr.classes(["data-err-msg"])], data_err_msg),
        ...overlay,
      ],
    );
  };

  let view_shape = _ => {
    LivelitShape.MultiLine(
      // TODO
      6,
    );
  };
};

module GrayscaleLivelitView = {
  [@deriving sexp]
  type model = BuiltinLivelits.GrayscaleLivelitCore.model;
  [@deriving sexp]
  type action = BuiltinLivelits.GrayscaleLivelitCore.action;
  type trigger = action => Event.t;
  type sync = action => unit;

  let height = 17;
  let view_shape = _ => LivelitShape.MultiLine(height);

  let view =
      (
        {brightness, grayscale}: model,
        _,
        _,
        {dargs, dhcode, uhcode}: LivelitView.splice_and_param_getters,
      ) => {
    let subject = {
      switch (dargs, dhcode(brightness), dhcode(grayscale)) {
      | (
          Some([("url", Some((DHExp.StringLit(url), _)))]),
          Some((DHExp.IntLit(b), _)),
          Some((DHExp.IntLit(g), _)),
        )
          when 0 <= b && b <= 100 && 0 <= g && g <= 100 =>
        Node.create(
          "img",
          [
            Attr.classes(["subject"]),
            Attr.create("src", url),
            Attr.create(
              "style",
              Printf.sprintf(
                "filter: brightness(%d%%) grayscale(%d%%);",
                b,
                g,
              ),
            ),
          ],
          [],
        )
      | _ =>
        Node.div(
          [
            Attr.classes(["missing-subject"]),
            Attr.create(
              "style",
              Printf.sprintf("background-color: gray; width: 550px;"),
            ),
          ],
          [],
        )
      };
    };
    Node.div(
      [Attr.classes(["grayscale-livelit"])],
      [
        Node.div(
          [Attr.classes(["slider-bar"])],
          [
            Node.create(
              "img",
              [
                Attr.id("grayscale-icon"),
                Attr.create("src", "imgs/grayscale-icon.png"),
              ],
              [],
            ),
            Node.div(
              [Attr.classes(["splice-content"])],
              [uhcode(grayscale)],
            ),
            Node.div([], []),
            Node.create(
              "img",
              [
                Attr.id("brightness-icon"),
                Attr.create(
                  "src",
                  "https://cdn2.iconfinder.com/data/icons/ecommerce-1-4/65/18-512.png",
                ),
              ],
              [],
            ),
            Node.div(
              [Attr.classes(["splice-content"])],
              [uhcode(brightness)],
            ),
          ],
        ),
        subject,
      ],
    );
  };
};

module ColorLivelitView = {
  let name = "$color";
  let expansion_ty = HTyp.Prod([Int, Int, Int, Int]);
  let param_tys = [];

  // https://github.com/microsoft/vscode/blob/6d4f8310a96ae2dfb7eaa8d1807774fb14b3b263/src/vs/base/common/color.ts#L197
  let hsv_of_rgb = ((r, g, b)) => {
    let r = Float.of_int(r) /. 255.0;
    let g = Float.of_int(g) /. 255.0;
    let b = Float.of_int(b) /. 255.0;
    let cmax = max(r, max(g, b));
    let cmin = min(r, min(g, b));
    let delta = cmax -. cmin;
    let s = cmax == 0.0 ? 0.0 : delta /. cmax;
    let m =
      if (delta == 0.0) {
        0.0;
      } else if (cmax == r) {
        Float.rem(Float.rem((g -. b) /. delta, 6.0) +. 6.0, 6.0);
      } else if (cmax == g) {
        (b -. r) /. delta +. 2.0;
      } else {
        (r -. g) /. delta +. 4.0;
      };

    let h = max(0.0, min(Float.round(m *. 60.0), 360.0));
    let s = max(0.0, min(s, 1.0));
    let v = max(0.0, min(cmax, 1.0));
    (h, s, v);
  };

  // https://github.com/microsoft/vscode/blob/6d4f8310a96ae2dfb7eaa8d1807774fb14b3b263/src/vs/base/common/color.ts#L221
  let rgb_of_hsv = ((h, s, v)) => {
    let c = v *. s;
    let x = c *. (1.0 -. Float.abs(Float.rem(h /. 60.0, 2.0) -. 1.0));
    let m = v -. c;

    let (r, g, b) =
      if (h < 60.0) {
        (c, x, 0.0);
      } else if (h < 120.0) {
        (x, c, 0.0);
      } else if (h < 180.0) {
        (0.0, c, x);
      } else if (h < 240.0) {
        (0.0, x, c);
      } else if (h < 300.0) {
        (x, 0.0, c);
      } else if (h < 360.0) {
        (c, 0.0, x);
      } else {
        (0.0, 0.0, 0.0);
      };
    (
      Float.to_int(Float.round((r +. m) *. 255.0)),
      Float.to_int(Float.round((g +. m) *. 255.0)),
      Float.to_int(Float.round((b +. m) *. 255.0)),
    );
  };

  [@deriving sexp]
  type model = BuiltinLivelits.ColorLivelitCore.model;

  [@deriving sexp]
  type action = BuiltinLivelits.ColorLivelitCore.action;
  type trigger = action => Event.t;
  type sync = action => unit;

  let view =
      (
        {rgb: (r, g, b), a, selecting_sat_val}: model,
        trigger,
        _,
        {uhcode, dhcode, _}: LivelitView.splice_and_param_getters,
      ) => {
    let is_valid = color_value => 0 <= color_value && color_value < 256;
    let is_valid_alpha = a => 0 <= a && a <= 100;
    let rgba_values =
      switch (dhcode(r), dhcode(g), dhcode(b), dhcode(a)) {
      | (
          Some((IntLit(r), _)),
          Some((IntLit(g), _)),
          Some((IntLit(b), _)),
          Some((IntLit(a), _)),
        )
          when
            is_valid(r) && is_valid(g) && is_valid(b) && is_valid_alpha(a) =>
        Some((r, g, b, a))
      | _ => None
      };

    let color_picker = {
      let height = 135.0;
      let width = 135.0;
      let px = Printf.sprintf("%fpx");
      let sat_val_box = {
        let box = (bg_color, attrs, children) => {
          Node.div(
            [
              Attr.classes(["sat-val-box"]),
              attr_style(
                StringUtil.cat([
                  prop_val("height", height |> px),
                  prop_val("width", width |> px),
                  prop_val("background-color", bg_color),
                ]),
              ),
              ...attrs,
            ],
            children,
          );
        };
        switch (rgba_values) {
        | None => box("gray", [], [])
        | Some((rval, gval, bval, _)) =>
          let (h, s, v) = hsv_of_rgb((rval, gval, bval));
          let (sat_r, sat_g, sat_b) = rgb_of_hsv((h, 1.0, 1.0));
          let bounded = f => max(0., min(1., f));
          let offset_x_y = evt =>
            Js.Unsafe.(get(evt, "offsetX"), get(evt, "offsetY"))
            |> TupleUtil.map2(Float.of_int);
          let on_first_click = evt => {
            let (offset_x, offset_y) = offset_x_y(evt);
            let s = offset_x /. width;
            let v = (height -. offset_y) /. height;
            let rgb = rgb_of_hsv((h, bounded(s), bounded(v)));
            Event.Many([
              trigger(StartSelectingSatVal: action),
              trigger(SelectRGB(rgb)),
            ]);
          };
          let on_drag = evt => {
            let sat_val_rect =
              JSUtil.force_get_elem_by_cls("sat-val-box")##getBoundingClientRect;
            let (offset_x, offset_y) = offset_x_y(evt);
            let s = (offset_x -. sat_val_rect##.left) /. width;
            let v = (height -. (offset_y -. sat_val_rect##.top)) /. height;
            let rgb = rgb_of_hsv((h, bounded(s), bounded(v)));
            trigger(SelectRGB(rgb));
          };
          let overlay =
            Node.div(
              Attr.[
                classes(["dragging-overlay"]),
                on_mouseup(_ => trigger(StopSelectingSatVal)),
                on_mousemove(on_drag),
              ],
              [],
            );
          box(
            Printf.sprintf("rgb(%d, %d, %d)", sat_r, sat_g, sat_b),
            [Attr.on_mousedown(on_first_click)],
            [
              Node.div(
                [
                  Attr.classes(["sat-val-selector"]),
                  attr_style(
                    StringUtil.cat([
                      prop_val("left", s *. width |> px),
                      prop_val("top", height -. v *. height |> px),
                    ]),
                  ),
                  Attr.on_mousedown(_ => trigger(StartSelectingSatVal)),
                ],
                [],
              ),
              ...selecting_sat_val ? [overlay] : [],
            ],
          );
        };
      };

      let hue_slider = {
        let slider = attrs =>
          Node.div(
            [Attr.classes(["hue-slider-wrapper"])],
            [
              Node.input(
                [
                  Attr.classes(["hue-slider"]),
                  Attr.type_("range"),
                  Attr.create("min", "0"),
                  Attr.create("max", "360"),
                  Attr.create(
                    "style",
                    // slider is rotated 90 degrees
                    Printf.sprintf("width: %fpx;", height),
                  ),
                  ...attrs,
                ]
                |> Attrs.merge_classes_and_styles,
                [],
              ),
            ],
          );
        switch (rgba_values) {
        | None => slider([Attr.classes(["no-thumb"])])
        | Some((r, g, b, _)) =>
          let (h, s, v) = hsv_of_rgb((r, g, b));
          slider([
            Attr.value(string_of_int(Float.to_int(h))),
            Attr.on_input((_, value_str) => {
              let h = float_of_string(value_str);
              trigger(SelectRGB(rgb_of_hsv((h, s, v))));
            }),
          ]);
        };
      };

      let color_swatch = {
        let swatch = (attrs, children) =>
          Node.div([Attr.classes(["color-swatch"]), ...attrs], children);
        switch (rgba_values) {
        | None =>
          swatch(
            [],
            [
              Node.create_svg(
                "svg",
                [Attr.create("viewBox", "0 0 100 100")],
                [
                  Node.create_svg(
                    "line",
                    [
                      Attr.create("x1", "20"),
                      Attr.create("y1", "20"),
                      Attr.create("x2", "80"),
                      Attr.create("y2", "80"),
                      Attr.create("vector-effect", "non-scaling-stroke"),
                    ],
                    [],
                  ),
                  Node.create_svg(
                    "line",
                    [
                      Attr.create("x1", "20"),
                      Attr.create("y1", "80"),
                      Attr.create("x2", "80"),
                      Attr.create("y2", "20"),
                      Attr.create("vector-effect", "non-scaling-stroke"),
                    ],
                    [],
                  ),
                ],
              ),
            ],
          )
        | Some((r, g, b, a)) =>
          swatch(
            [
              Attr.create(
                "style",
                Printf.sprintf(
                  "background-color: rgba(%d, %d, %d, %f);",
                  r,
                  g,
                  b,
                  Float.of_int(a) /. 100.0,
                ),
              ),
            ],
            [
              Node.div(
                [Attr.classes(["color-swatch-label"])],
                [
                  Node.span(
                    [
                      // https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color
                      attr_style(
                        prop_val(
                          "color",
                          Float.of_int(r)
                          *. 0.299
                          +. Float.of_int(g)
                          *. 0.587
                          +. Float.of_int(b)
                          *. 0.114 > 186.
                          *. Float.of_int(a)
                          /. 100.
                            ? "#000000" : "#ffffff",
                        ),
                      ),
                    ],
                    [
                      Node.text(
                        Printf.sprintf(
                          "rgba(%d, %d, %d, %.2f)",
                          r,
                          g,
                          b,
                          Float.of_int(a) /. 100.,
                        ),
                      ),
                    ],
                  ),
                ],
              ),
            ],
          )
        };
      };

      let splice_label = lbl =>
        Node.label([Attr.classes(["splice-label"])], [Node.text(lbl)]);
      let splice = splice_name =>
        Node.div(
          [Attr.classes(["splice-content"])],
          [uhcode(splice_name)],
        );
      Node.div(
        [Attr.classes(["color-picker"])],
        [
          color_swatch,
          Node.div(
            [Attr.classes(["rgb-picker"])],
            [
              splice_label("R"),
              splice(r),
              splice_label("G"),
              splice(g),
              splice_label("B"),
              splice(b),
              splice_label("A"),
              splice(a),
            ],
          ),
          Node.div(
            [Attr.classes(["hsv-picker"])],
            [sat_val_box, hue_slider],
          ),
        ],
      );
    };
    Node.div([Attr.classes(["color-livelit"])], [color_picker]);
  };

  let view_shape = _ => LivelitShape.MultiLine(9);
};

/*
 module GradientLivelitView = {
   let name = "$gradient";
   let expansion_ty = ColorLivelitView.expansion_ty;
   let param_tys = [];

   [@deriving sexp]
   type model = BuiltinLivelits.GradientLivelitCore.model;
   [@deriving sexp]
   type action = BuiltinLivelits.GradientLivelitCore.action;
   type trigger = action => Event.t;
   type sync = action => unit;

   let slider_min = 0;
   let slider_max = 100;
   let init_slider_value = 50;

   let view =
       (
         model: model,
         trigger,
         _,
         {uhcode, _}: LivelitView.splice_and_param_getters,
       ) => {
     Node.span(
       [Attr.classes(["gradient-livelit"])],
       [
         uhcode(model.lcolor),
         Node.input(
           [
             Attr.classes(["slider"]),
             Attr.type_("range"),
             Attr.create("min", string_of_int(slider_min)),
             Attr.create("max", string_of_int(slider_max)),
             Attr.value(string_of_int(model.slider_value)),
             Attr.on_change((_, value_str) => {
               let new_value = int_of_string(value_str);
               trigger(Slide(new_value): action);
             }),
           ],
           [],
         ),
         uhcode(model.rcolor),
       ],
     );
   };

   let view_shape = _ => LivelitShape.Inline(10);
 };
 */

module CheckboxLivelitView = {
  [@deriving sexp]
  type model = BuiltinLivelits.CheckboxLivelitCore.model;
  [@deriving sexp]
  type action = BuiltinLivelits.CheckboxLivelitCore.action;
  type trigger = action => Event.t;
  type sync = action => unit;

  let view = (m, trig, _) => {
    let checked_state = m ? [Attr.checked] : [];
    let input_elt =
      Node.input(
        [
          Attr.type_("checkbox"),
          Attr.on_input((_, _) => trig(Toggle: action)),
          ...checked_state,
        ],
        [],
      );
    _ => Node.span([], [input_elt]);
  };

  let view_shape = _ => LivelitShape.Inline(/* TODO! */ 1);
};

module SliderLivelitMinSpliceView = {
  [@deriving sexp]
  type model = BuiltinLivelits.SliderLivelitMinSpliceCore.model;
  [@deriving sexp]
  type action = BuiltinLivelits.SliderLivelitMinSpliceCore.action;
  type trigger = action => Event.t;
  type sync = action => unit;

  let view_shape = _ => LivelitShape.Inline(14);
  let view =
      (
        (endpoint_splice_number, current_value),
        trigger: trigger,
        _sync,
        {uhcode, dhcode, _}: LivelitView.splice_and_param_getters,
      ) => {
    let endpoint_value =
      switch (dhcode(endpoint_splice_number)) {
      | Some((IntLit(endpoint_val), _)) => string_of_int(endpoint_val)
      | _ => "100" // default
      };
    let splice_div = uhcode(endpoint_splice_number);
    let value = string_of_int(current_value);
    let on_input = (_, value_str) => trigger(int_of_string(value_str));
    Node.span(
      [Attr.classes(["slider-livelit"])],
      [
        Node.input(
          [
            Attr.classes(["slider"]),
            Attr.type_("range"),
            Attr.create("min", "0"),
            Attr.create("max", endpoint_value),
            Attr.value(value),
            Attr.on_input(on_input),
          ],
          [],
        ),
        splice_div,
      ],
    );
  };
};

module SliderLivelitMinView = {
  [@deriving sexp]
  type model = BuiltinLivelits.SliderLivelitMinCore.model;
  [@deriving sexp]
  type action = BuiltinLivelits.SliderLivelitMinCore.action;
  type trigger = action => Event.t;
  type sync = action => unit;

  let view_shape = _ => LivelitShape.Inline(14);
  let view = (model, trigger: trigger, _sync, _) => {
    let value = string_of_int(model);
    let on_input = (_, value_str) => trigger(int_of_string(value_str));
    Node.span(
      [Attr.classes(["slider-livelit"])],
      [
        Node.input(
          [
            Attr.classes(["slider"]),
            Attr.type_("range"),
            Attr.create("min", "0"),
            Attr.create("max", "100"),
            Attr.value(value),
            Attr.on_input(on_input),
          ],
          [],
        ),
      ],
    );
  };
};

module SliderLivelitView = {
  [@deriving sexp]
  type endpoint =
    | Min
    | Max;

  [@deriving sexp]
  type model = BuiltinLivelits.SliderLivelitCore.model;
  [@deriving sexp]
  type action = BuiltinLivelits.SliderLivelitCore.action;
  type trigger = action => Event.t;
  type sync = action => unit;

  let view = (model, trigger: trigger, sync) => {
    let _endpoint_view = (cls, value) => {
      let padding = "3px";
      let val_str = string_of_int(value);
      Node.label(
        [
          Attr.classes([cls]),
          attr_style(
            StringUtil.sep([
              prop_val("padding", padding),
              prop_val(
                "width",
                Printf.sprintf(
                  "calc(%dch + %s)",
                  String.length(val_str),
                  padding,
                ),
              ),
            ]),
          ),
        ],
        [Node.text(val_str)],
      );
    };

    let _tickmarks = (min: float, max: float) => {
      let val_of_percent = (p: int): string => {
        let p = Float.of_int(p) /. 100.0;
        Printf.sprintf("%f", (1. -. p) *. min +. p *. max);
      };
      Node.create(
        "datalist",
        [Attr.id("tickmarks")],
        [
          Node.option(
            [
              Attr.create("value", val_of_percent(0)),
              Attr.create("label", "0%"),
            ],
            [],
          ),
          Node.option([Attr.create("value", val_of_percent(10))], []),
          Node.option([Attr.create("value", val_of_percent(20))], []),
          Node.option([Attr.create("value", val_of_percent(30))], []),
          Node.option([Attr.create("value", val_of_percent(40))], []),
          Node.option(
            [
              Attr.create("value", val_of_percent(50)),
              Attr.create("label", "50%"),
            ],
            [],
          ),
          Node.option([Attr.create("value", val_of_percent(60))], []),
          Node.option([Attr.create("value", val_of_percent(70))], []),
          Node.option([Attr.create("value", val_of_percent(80))], []),
          Node.option([Attr.create("value", val_of_percent(90))], []),
          Node.option(
            [
              Attr.create("value", val_of_percent(100)),
              Attr.create("label", "100%"),
            ],
            [],
          ),
        ],
      );
    };

    let slider =
        (~disabled: bool, ~min: int=0, ~max: int=100, ~value: int=50, ()) =>
      Node.span(
        [Attr.classes(["slider-livelit"])],
        [
          // tickmarks(min, max),
          Node.input(
            [
              Attr.classes(["slider"]),
              Attr.type_("range"),
              Attr.create("min", string_of_int(min)),
              Attr.create("max", string_of_int(max)),
              // Attr.create("step", "0.01"),
              Attr.value(string_of_int(value)),
              Attr.on_input((_, value_str) =>
                trigger(Slide(int_of_string(value_str)))
              ),
              ...disabled ? [Attr.disabled] : [],
            ],
            [],
          ),
        ],
      );
    ({dargs, _}: LivelitView.splice_and_param_getters) => {
      switch (dargs) {
      | Some([
          ("min", Some((DHExp.IntLit(min), _))),
          ("max", Some((DHExp.IntLit(max), _))),
        ])
          when min <= max =>
        let value =
          switch (model) {
          | Some(n) when min <= n && n <= max => n
          | _ =>
            let new_value = (min + max) / 2;
            sync(Slide(new_value): action);
            new_value;
          };
        slider(~disabled=false, ~min, ~max, ~value, ());
      | _ =>
        switch (model) {
        | None => ()
        | Some(_) => sync(InvalidParams)
        };
        slider(~disabled=true, ());
      };
    };
  };

  let view_shape = _ => LivelitShape.Inline(14);
};

module SliderLivelitFloatView = {
  [@deriving sexp]
  type endpoint =
    | Min
    | Max;

  [@deriving sexp]
  type model = BuiltinLivelits.SliderLivelitFloatCore.model;
  [@deriving sexp]
  type action = BuiltinLivelits.SliderLivelitFloatCore.action;
  type trigger = action => Event.t;
  type sync = action => unit;

  let view = (model, trigger: trigger, sync) => {
    let _endpoint_view = (cls, value) => {
      let padding = "3px";
      let val_str = string_of_int(value);
      Node.label(
        [
          Attr.classes([cls]),
          attr_style(
            StringUtil.sep([
              prop_val("padding", padding),
              prop_val(
                "width",
                Printf.sprintf(
                  "calc(%dch + %s)",
                  String.length(val_str),
                  padding,
                ),
              ),
            ]),
          ),
        ],
        [Node.text(val_str)],
      );
    };

    let slider =
        (
          ~disabled: bool,
          ~min: float=0.,
          ~max: float=100.,
          ~value: float=50.,
          (),
        ) =>
      Node.span(
        [Attr.classes(["slider-livelit"])],
        [
          Node.input(
            [
              Attr.classes(["slider"]),
              Attr.type_("range"),
              Attr.create("min", Printf.sprintf("%f", min)),
              Attr.create("max", Printf.sprintf("%f", max)),
              Attr.create("step", "0.01"),
              Attr.value(Printf.sprintf("%f", value)),
              Attr.on_input((_, value_str) =>
                trigger(Slide(float_of_string(value_str)))
              ),
              ...disabled ? [Attr.disabled] : [],
            ],
            [],
          ),
        ],
      );
    ({dargs, _}: LivelitView.splice_and_param_getters) => {
      switch (dargs) {
      | Some([
          ("min", Some((DHExp.FloatLit(min), _))),
          ("max", Some((DHExp.FloatLit(max), _))),
        ])
          when min <= max =>
        let value =
          switch (model) {
          | Some(f) when min <= f && f <= max => f
          | _ =>
            let new_value = (min +. max) /. 2.;
            sync(Slide(new_value): action);
            new_value;
          };
        slider(~disabled=false, ~min, ~max, ~value, ());
      | _ =>
        switch (model) {
        | None => ()
        | Some(_) => sync(InvalidParams)
        };
        slider(~disabled=true, ());
      };
    };
  };

  let view_shape = _ => LivelitShape.Inline(14);
};

module DataFrameLivelitView = {
  let name = "$data_frame";
  let expansion_ty =
    HTyp.(Prod([List(String), List(Prod([String, List(Float)]))]));
  let param_tys = [];

  [@deriving sexp]
  type row = BuiltinLivelits.DataFrameLivelitCore.row;
  // assume nonzero height and width
  [@deriving sexp]
  type model = BuiltinLivelits.DataFrameLivelitCore.model;
  [@deriving sexp]
  type dim = BuiltinLivelits.DataFrameLivelitCore.dim;
  [@deriving sexp]
  type action = BuiltinLivelits.DataFrameLivelitCore.action;
  type trigger = action => Event.t;
  type sync = action => unit;

  let get_height = BuiltinLivelits.DataFrameLivelitCore.get_height;
  let get_width = BuiltinLivelits.DataFrameLivelitCore.get_width;

  let grid_area = ((row_start, col_start, row_end, col_end)) =>
    prop_val(
      "grid-area",
      Printf.sprintf(
        "%d / %d / %d / %d",
        row_start,
        col_start,
        row_end,
        col_end,
      ),
    );

  let view =
      (
        m: model,
        trig,
        _,
        {uhcode, dhcode, _}: LivelitView.splice_and_param_getters,
      ) => {
    let splice = (~clss, ~grid_coordinates, splice_name) =>
      Node.div(
        [
          attr_style(grid_area(grid_coordinates)),
          Attr.classes([
            "matrix-splice",
            splice_name == m.selected
              ? "matrix-selected" : "matrix-unselected",
            ...clss,
          ]),
          Attr.on_mousedown(_ => trig(Select(splice_name): action)),
          // Attr.on_click(_ => trig(Del(Col, j))),
        ],
        [
          switch (dhcode(splice_name)) {
          | None => Node.text("-")
          | Some((_, view)) => view
          },
          //Node.span([Attr.classes(["delete"])], [Node.text("x")]),
        ],
      );

    let width = get_width(m);
    let height = get_height(m);
    let col_headers =
      m.col_headers
      |> List.mapi((j, header) =>
           splice(
             ~clss=["col-header"],
             ~grid_coordinates=(1, j + 3, 2, j + 4),
             header,
           )
         );
    let row_headers =
      m.rows
      |> List.mapi((i, row: row) =>
           splice(
             ~clss=["row-header"],
             ~grid_coordinates=(i + 3, 1, i + 4, 2),
             row.header,
           )
         );
    let cells =
      m.rows
      |> List.mapi((i, row: row) =>
           row.cells
           |> List.mapi((j, cell) =>
                splice(
                  ~clss=["matrix-cell"],
                  ~grid_coordinates=(i + 3, j + 3, i + 4, j + 4),
                  cell,
                )
              )
         )
      |> List.flatten;

    let add_row_button =
      Node.button(
        [
          Attr.classes(["add-row", "add-button"]),
          Attr.on_click(_ => trig(Add(Row))),
        ],
        [Node.text("+")],
      );
    let add_col_button =
      Node.button(
        [
          Attr.classes(["add-col", "add-button"]),
          Attr.on_click(_ => trig(Add(Col))),
        ],
        [Node.text("+")],
      );

    let header_corner =
      Node.div(
        [
          attr_style(grid_area((1, 1, 2, 2))),
          Attr.classes(["header-corner"]),
        ],
        [],
      );

    let dim_template = dim =>
      // gap between headers and cells so that header cells
      // (which need higher z-index than regular cells to
      // support freezing ux) don't cover selection highlight\
      // of neighboring cells
      StringUtil.sep(["auto", "2px", ...ListUtil.replicate(dim, "auto")]);

    Node.div(
      [Attr.classes(["matrix-livelit"])],
      [
        Node.div(
          [Attr.classes(["formula-bar"])],
          [
            Node.div(
              [Attr.classes(["formula-bar-prompt"])],
              [Node.text(" > ")],
            ),
            uhcode(m.selected),
          ],
        ),
        Node.div(
          [Attr.classes(["outer-container"])],
          [
            Node.div(
              [
                Attr.classes(["cells", "grid-container"]),
                attr_style(
                  StringUtil.cat([
                    prop_val("grid-template-columns", dim_template(width)),
                    prop_val("grid-template-rows", dim_template(height)),
                  ]),
                ),
              ],
              List.concat([
                [header_corner, ...row_headers],
                col_headers,
                cells,
              ]),
            ),
            add_row_button,
            add_col_button,
          ],
        ),
      ],
    );
  };

  let view_shape = _ => LivelitShape.MultiLine(10);
};

/* ----------
   stuff below is infrastructure
   ---------- */

module PairLivelit: LIVELIT =
  MkLivelit(BuiltinLivelits.PairLivelitCore, PairLivelitView);
module GradeCutoffLivelit: LIVELIT =
  MkLivelit(BuiltinLivelits.GradeCutoffLivelitCore, GradeCutoffLivelitView);
module GrayscaleLivelit: LIVELIT =
  MkLivelit(BuiltinLivelits.GrayscaleLivelitCore, GrayscaleLivelitView);
module ColorLivelit: LIVELIT =
  MkLivelit(BuiltinLivelits.ColorLivelitCore, ColorLivelitView);
module CheckboxLivelit: LIVELIT =
  MkLivelit(BuiltinLivelits.CheckboxLivelitCore, CheckboxLivelitView);
module SliderLivelitMin: LIVELIT =
  MkLivelit(BuiltinLivelits.SliderLivelitMinCore, SliderLivelitMinView);
module SliderLivelitMinSplice: LIVELIT =
  MkLivelit(
    BuiltinLivelits.SliderLivelitMinSpliceCore,
    SliderLivelitMinSpliceView,
  );
module SliderLivelit: LIVELIT =
  MkLivelit(BuiltinLivelits.SliderLivelitCore, SliderLivelitView);
module SliderLivelitFloat: LIVELIT =
  MkLivelit(BuiltinLivelits.SliderLivelitFloatCore, SliderLivelitFloatView);
module MatrixLivelit: LIVELIT =
  MkLivelit(BuiltinLivelits.MatrixLivelitCore, MatrixLivelitView);
module DataFrameLivelit: LIVELIT =
  MkLivelit(BuiltinLivelits.DataFrameLivelitCore, DataFrameLivelitView);
//module GradientLivelit: LIVELIT =
//  MkLivelit(BuiltinLivelits.GradientLivelitCore, GradientLivelitView);

module ColorLivelitViewAdapter = LivelitViewAdapter(ColorLivelit);
module CheckboxLivelitViewAdapter = LivelitViewAdapter(CheckboxLivelit);
module PairLivelitViewAdapter = LivelitViewAdapter(PairLivelit);
module SliderLivelitViewAdapter = LivelitViewAdapter(SliderLivelit);
module SliderLivelitFloatViewAdapter = LivelitViewAdapter(SliderLivelitFloat);
module SliderLivelitMinViewAdapter = LivelitViewAdapter(SliderLivelitMin);
module SliderLivelitMinSpliceViewAdapter =
  LivelitViewAdapter(SliderLivelitMinSplice);
module MatrixLivelitViewAdapter = LivelitViewAdapter(MatrixLivelit);
module GradeCutoffLivelitViewAdapter = LivelitViewAdapter(GradeCutoffLivelit);
module DataFrameLivelitViewAdapter = LivelitViewAdapter(DataFrameLivelit);
module GrayscaleLivelitViewAdapter = LivelitViewAdapter(GrayscaleLivelit);
//module GradientLivelitViewAdapter = LivelitViewAdapter(GradientLivelit);

let initial_livelit_view_ctx =
  List.fold_left(
    LivelitViewContexts.extend,
    LivelitViewContexts.empty,
    [
      GrayscaleLivelitViewAdapter.contexts_entry,
      DataFrameLivelitViewAdapter.contexts_entry,
      GradeCutoffLivelitViewAdapter.contexts_entry,
      MatrixLivelitViewAdapter.contexts_entry,
      PairLivelitViewAdapter.contexts_entry,
      CheckboxLivelitViewAdapter.contexts_entry,
      SliderLivelitViewAdapter.contexts_entry,
      SliderLivelitFloatViewAdapter.contexts_entry,
      ColorLivelitViewAdapter.contexts_entry,
      SliderLivelitMinViewAdapter.contexts_entry,
      SliderLivelitMinSpliceViewAdapter.contexts_entry,
      //GradientLivelitViewAdapter.contexts_entry,
    ],
  );
