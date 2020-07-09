open Sexplib.Std;

module Dom_html = Js_of_ocaml.Dom_html;
module Dom = Js_of_ocaml.Dom;
module Js = Js_of_ocaml.Js;
module Vdom = Virtual_dom.Vdom;

module LivelitView = {
  type div_type = Vdom.Node.t;

  type splice_and_param_getters = {
    uhcode: SpliceName.t => div_type,
    dhcode: SpliceName.t => option((DHExp.t, div_type)),
    dargs: option(list((Var.t, option((DHExp.t, div_type))))),
  };

  type t = splice_and_param_getters => div_type;

  [@deriving sexp]
  type shape =
    | Inline(int)
    | MultiLine(int);
};

module type LIVELIT = {
  let name: LivelitName.t;
  let expansion_ty: HTyp.t;
  let param_tys: list((Var.t, HTyp.t));

  [@deriving sexp]
  type model;
  [@deriving sexp]
  type action;
  type trigger = action => Vdom.Event.t;
  type sync = action => unit;

  let init_model: SpliceGenCmd.t(model);
  let update: (model, action) => SpliceGenCmd.t(model);
  let view: (model, trigger, sync) => LivelitView.t;
  let view_shape: model => LivelitView.shape;
  let expand: model => UHExp.t;
};

module LivelitAdapter = (L: LIVELIT) => {
  let serialize_monad = model => SpliceGenCmd.return(L.sexp_of_model(model));

  /* generate livelit definition for Semantics */
  let livelit_defn =
    LivelitDefinition.{
      name: L.name,
      expansion_ty: L.expansion_ty,
      param_tys: L.param_tys,
      init_model: SpliceGenCmd.bind(L.init_model, serialize_monad),
      update: (serialized_model, serialized_action) =>
        SpliceGenCmd.bind(
          L.update(
            L.model_of_sexp(serialized_model),
            L.action_of_sexp(serialized_action),
          ),
          serialize_monad,
        ),
      expand: serialized_model =>
        L.expand(L.model_of_sexp(serialized_model)),
    };

  let serialized_view_fn = (serialized_model, trigger, sync) =>
    L.view(
      L.model_of_sexp(serialized_model),
      action => trigger(L.sexp_of_action(action)),
      action => sync(L.sexp_of_action(action)),
    );

  let serialized_view_shape_fn = serialized_model =>
    L.view_shape(L.model_of_sexp(serialized_model));

  let contexts_entry = (
    L.name,
    livelit_defn,
    serialized_view_fn,
    serialized_view_shape_fn,
  );
};

type trigger_serialized = SerializedAction.t => Vdom.Event.t;
type sync_serialized = SerializedAction.t => unit;
type serialized_view_fn_t =
  (SerializedModel.t, trigger_serialized, sync_serialized) => LivelitView.t;
type serialized_view_shape_fn_t = SerializedModel.t => LivelitView.shape;

module LivelitViewCtx = {
  type t = VarMap.t_((serialized_view_fn_t, serialized_view_shape_fn_t));
  include VarMap;
};

module LivelitContexts = {
  type t('a) = (LivelitCtx.t('a), LivelitViewCtx.t);
  let empty = (LivelitCtx.empty, LivelitViewCtx.empty);
  let extend =
      (
        (livelit_ctx, livelit_view_ctx),
        (
          name,
          def: LivelitDefinition.t,
          serialized_view_fn,
          serialized_view_shape_fn,
        ),
      ) => {
    if (!LivelitName.is_valid(name)) {
      failwith("Invalid livelit name " ++ name);
    };
    if (name != def.name) {
      failwith(
        "Livelit name " ++ name ++ " differs from def name " ++ def.name,
      );
    };
    let (param_names, _) = List.split(def.param_tys);
    let rec contains_dupl =
      fun
      | [] => false
      | [hd, ...tl] => List.mem(hd, tl) || contains_dupl(tl);
    if (contains_dupl(param_names)) {
      failwith(
        "Parameter names for livelit "
        ++ name
        ++ " must be unique: "
        ++ String.concat(", ", param_names),
      );
    };
    (
      VarMap.extend(livelit_ctx, (name, (def, []))),
      VarMap.extend(
        livelit_view_ctx,
        (name, (serialized_view_fn, serialized_view_shape_fn)),
      ),
    );
  };
};

let _to_uhvar = id =>
  UHExp.(Var(NotInHole, NotInVarHole, SpliceInfo.var_of_splice_name(id)));

let attr_style = Vdom.Attr.create("style");

let prop_val = (prop: string, value: string) =>
  StringUtil.cat([prop, ": ", value, ";"]);

module PairLivelit: LIVELIT = {
  let name = "$pair";
  let expansion_ty = HTyp.(Prod([Hole, Hole]));
  let param_tys = [];

  [@deriving sexp]
  type model = (int, int);
  [@deriving sexp]
  type action = unit;
  type trigger = action => Vdom.Event.t;
  type sync = action => unit;

  let init_model =
    SpliceGenCmd.bind(SpliceGenCmd.new_splice(HTyp.Hole), leftID =>
      SpliceGenCmd.bind(SpliceGenCmd.new_splice(HTyp.Hole), rightID =>
        SpliceGenCmd.return((leftID, rightID))
      )
    );
  let update = (m, _) => SpliceGenCmd.return(m);

  let view =
      (
        (leftID, rightID),
        _,
        _,
        {uhcode, _}: LivelitView.splice_and_param_getters,
      ) =>
    Vdom.(
      Node.div(
        [Attr.classes(["pair-livelit"])],
        [uhcode(leftID), uhcode(rightID)],
      )
    );
  let view_shape = _ =>
    LivelitView.Inline(
      // TODO fix brittle magic constant
      20,
    );

  let expand = ((leftID, rightID)) => {
    let pair_seq =
      Seq.mk(
        _to_uhvar(leftID),
        [(Operators_Exp.Comma, _to_uhvar(rightID))],
      );
    UHExp.Block.wrap'(UHExp.mk_OpSeq(pair_seq));
  };
};

module type MAT_INFO = {
  let name: LivelitName.t;
  let is_live: bool;
};

module MatrixLivelitFunctor = (I: MAT_INFO) : LIVELIT => {
  let name = I.name;
  let expansion_ty = HTyp.(List(List(Int)));
  let param_tys = [];

  // assume nonzero height and width
  [@deriving sexp]
  type model = (SpliceName.t, list(list(SpliceName.t)));
  [@deriving sexp]
  type dim =
    | Row
    | Col;
  [@deriving sexp]
  type action =
    | Select(SpliceName.t)
    | Add(dim)
    | Del(dim, int);
  type trigger = action => Vdom.Event.t;
  type sync = action => unit;

  let init_height = 2;
  let init_width = 2;

  let get_height = (m: list(list(SpliceName.t))): int => List.length(m);
  let get_width = (m: list(list(SpliceName.t))): int =>
    List.length(List.hd(m));

  let init_model =
    SpliceGenCmd.(
      MonadsUtil.bind_count(
        init_height,
        bind(
          MonadsUtil.bind_count(
            init_width,
            bind(new_splice(HTyp.Int)),
            return,
          ),
        ),
        grid =>
        return((grid |> List.hd |> List.hd, grid))
      )
    );

  let update = ((selected, m)) =>
    fun
    | Select(to_select) => {
        let to_select_in_grid =
          !(m |> List.for_all(List.for_all(s => s != to_select)));
        if (!to_select_in_grid) {
          JSUtil.log(
            Printf.sprintf(
              "Attempt to select splice name %d, which is not in the matrix",
              to_select,
            ),
          );
        };
        SpliceGenCmd.return((to_select_in_grid ? to_select : selected, m));
      }
    | Add(Row) =>
      SpliceGenCmd.(
        MonadsUtil.bind_count(
          get_width(m), bind(new_splice(HTyp.Int)), new_row =>
          return((selected, m @ [new_row]))
        )
      )
    | Add(Col) =>
      SpliceGenCmd.(
        MonadsUtil.bind_count(
          get_height(m), bind(new_splice(HTyp.Int)), new_col =>
          return((selected, List.map2((c, r) => r @ [c], new_col, m)))
        )
      )
    | Del(dim, i) => {
        let drop = (to_drop, ret) => {
          let selected_in_to_drop =
            !(to_drop |> List.for_all(s => s != selected));
          let to_select =
            selected_in_to_drop ? ret |> List.hd |> List.hd : selected;
          SpliceGenCmd.(
            MonadsUtil.bind_list(
              to_drop,
              d => bind(drop_splice(d)),
              _ => return((to_select, ret)),
            )
          );
        };
        switch (dim) {
        | Row =>
          if (get_height(m) <= 1) {
            SpliceGenCmd.return((selected, m));
          } else {
            let (before, rest) = ListUtil.split_index(m, i);
            let (to_drop, after) = (List.hd(rest), List.tl(rest));
            drop(to_drop, before @ after);
          }
        | Col =>
          if (get_width(m) <= 1) {
            SpliceGenCmd.return((selected, m));
          } else {
            let (before, rest) =
              m |> List.map(r => ListUtil.split_index(r, i)) |> List.split;
            let (to_drop, after) =
              rest |> List.map(r => (List.hd(r), List.tl(r))) |> List.split;
            drop(to_drop, List.map2((b, a) => b @ a, before, after));
          }
        };
      };

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
    open Vdom;
    let width = get_width(m);
    let height = get_height(m);
    let row_header =
      ListUtil.range(height)
      |> List.map(i =>
           Node.div(
             [
               attr_style(grid_area(i + 3, 1, i + 4, 2)),
               Attr.classes(["row-header", "pure-button"]),
               Attr.on_click(_ => trig(Del(Row, i))),
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
      if (!I.is_live) {
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
                  if (!I.is_live) {
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
    LivelitView.MultiLine(3 * num_rows + 2 + (I.is_live ? 1 : 0));
  };

  let expand = ((_, m)) => {
    let to_uhexp_list =
      fun
      | [] => UHExp.(Block.wrap(ListNil(NotInHole)))
      | [fst, ...rest] => {
          let rest' =
            (rest |> List.map(item => (Operators_Exp.Cons, item)))
            @ [(Operators_Exp.Cons, UHExp.ListNil(NotInHole))];
          let seq = Seq.mk(fst, rest');
          UHExp.Block.wrap'(UHExp.mk_OpSeq(seq));
        };
    let m' =
      m
      |> List.map(r =>
           r
           |> List.map(_to_uhvar)
           |> to_uhexp_list
           |> (q => UHExp.Parenthesized(q))
         );
    to_uhexp_list(m');
  };
};

module MatrixLivelitInfo: MAT_INFO = {
  let name = "$matrix";
  let is_live = false;
};
module MatrixLivelit = MatrixLivelitFunctor(MatrixLivelitInfo);
module LiveMatrixLivelitInfo: MAT_INFO = {
  let name = "$live_matrix";
  let is_live = true;
};
module LiveMatrixLivelit = MatrixLivelitFunctor(LiveMatrixLivelitInfo);

module GradeCutoffLivelit: LIVELIT = {
  let name = "$grade_cutoffs";
  let expansion_ty = HTyp.(Prod([Int, Int, Int, Int]));
  let param_tys = [("data", HTyp.(List(Int)))];

  [@deriving sexp]
  type letter_grade =
    | A
    | B
    | C
    | D;

  [@deriving sexp]
  type model = {
    a: int,
    b: int,
    c: int,
    d: int,
    selected_grade: option(letter_grade),
  };

  [@deriving sexp]
  type action =
    | SelectLetterGrade(option(letter_grade))
    | MoveSelectedGrade(int)
    | DragGrade(letter_grade, int);

  type trigger = action => Vdom.Event.t;
  type sync = action => unit;

  let init_model =
    SpliceGenCmd.return({a: 90, b: 80, c: 70, d: 60, selected_grade: None});

  let update = ({selected_grade, _} as m, a) => {
    // also deselects
    let set_new_cutoff = (grade, new_cutoff) =>
      if (new_cutoff < 0 || new_cutoff > 100) {
        JSUtil.log(Printf.sprintf("Invalid grade cutoff %d", new_cutoff));
        m;
      } else {
        let m =
          switch (grade) {
          | A => {...m, a: new_cutoff}
          | B => {...m, b: new_cutoff}
          | C => {...m, c: new_cutoff}
          | D => {...m, d: new_cutoff}
          };
        {...m, selected_grade: None};
      };
    SpliceGenCmd.return(
      switch (a) {
      | SelectLetterGrade(new_grade_opt) => {
          ...m,
          selected_grade: new_grade_opt,
        }
      | MoveSelectedGrade(new_cutoff) =>
        switch (selected_grade) {
        | None =>
          JSUtil.log("No grade selected");
          m;
        | Some(grade) => set_new_cutoff(grade, new_cutoff)
        }
      | DragGrade(grade, new_cutoff) => set_new_cutoff(grade, new_cutoff)
      },
    );
  };

  let line_y = 50;
  let line_width = 600;
  let margin_x_left = 15;
  let margin_x_right = 40;
  let margin_y = 20;
  let label_x_offset = (-5);
  let label_y_offset = 20;
  let letter_y_offset = 10;
  let grade_point_size = 10;
  let grade_point_half_size = grade_point_size / 2;
  let default_color = "black";
  let wrong_letter_order_color = "firebrick";
  // let cramped_cutoff_color = "coral";
  let selected_color = "olivedrab";
  let svg_height = line_y + margin_y;
  let svg_width = line_width + margin_x_left + margin_x_right;
  let soi = string_of_int;

  let _svg = (kind, create_attrs, ~attrs=[], ~children=[], ()) =>
    Vdom.Node.create_svg(
      kind,
      (
        create_attrs
        |> List.map(((name, val_)) => Vdom.Attr.create(name, val_))
      )
      @ attrs,
      children,
    );

  let get_cutoff_from_evt = evt => {
    let cur_tgt =
      Js.Opt.get(evt##.currentTarget, () =>
        failwith("No currentTarget for grade_cutoffs click")
      );
    (
      float(evt##.clientX)
      -.
      cur_tgt##getBoundingClientRect##.left
      -. float(margin_x_left)
    )
    /. float(line_width)
    *. 100.0
    |> Float.round
    |> Float.to_int;
  };

  let cutoff_svg = (lg, cutoff, color, selected_grade, trig) => {
    let x = line_width / 100 * cutoff + margin_x_left;
    let line_height = 25;
    let y = line_y - line_height;
    let lstr =
      switch (lg) {
      | A => "A"
      | B => "B"
      | C => "C"
      | D => "D"
      };
    [
      _svg(
        "line",
        [
          ("x1", x |> soi),
          ("y1", line_y |> soi),
          ("x2", x |> soi),
          ("y2", y |> soi),
          ("stroke", color),
          ("stroke-width", "2"),
        ],
        (),
      ),
      _svg(
        "text",
        [
          ("x", x + label_x_offset |> soi),
          ("y", y - letter_y_offset |> soi),
          ("fill", color),
        ],
        ~attrs=[
          Vdom.Attr.classes(["grade-letter"]),
          Vdom.Attr.on_mousedown(_ =>
            selected_grade == Some(lg)
              ? trig(SelectLetterGrade(None))
              : trig(SelectLetterGrade(Some(lg)))
          ),
        ],
        ~children=[Vdom.Node.text(lstr)],
        (),
      ),
    ];
  };

  let cutoff_svgs = (a, b, c, d, selected_grade, trig) => {
    let cutoff_svg' = (lg, cutoff, low_bound, high_bound) => {
      let color =
        if (selected_grade == Some(lg)) {
          selected_color;
        } else if (cutoff <= low_bound || cutoff >= high_bound) {
          wrong_letter_order_color;
        } else {
          default_color;
        };
      cutoff_svg(lg, cutoff, color, selected_grade, trig);
    };
    List.concat([
      cutoff_svg'(A, a, b, 101),
      cutoff_svg'(B, b, c, a),
      cutoff_svg'(C, c, d, b),
      cutoff_svg'(D, d, -1, c),
    ]);
  };

  let grade_labels = {
    let num_pos_grade_labels = 5;
    let grade_label_inc = 100 / num_pos_grade_labels;
    let line_inc = line_width / num_pos_grade_labels;
    List.init(num_pos_grade_labels + 1, i =>
      _svg(
        "text",
        [
          (
            "x",
            i
            * line_inc
            + margin_x_left
            + label_x_offset
            * String.length(i * grade_label_inc |> soi)
            |> soi,
          ),
          ("y", line_y + label_y_offset |> soi),
          ("fill", "black"),
        ],
        ~attrs=[Vdom.Attr.classes(["grade-labels"])],
        ~children=[Vdom.Node.text(i * grade_label_inc |> soi)],
        (),
      )
    );
  };

  let grades_invalids_to_svgs = ((grades, invalid_count)) => {
    let valid_grades =
      grades
      |> List.filter_map(grade =>
           if (grade >= 0 && grade <= 100) {
             Some(
               _svg(
                 "rect",
                 [
                   (
                     "x",
                     line_width
                     / 100
                     * grade
                     + margin_x_left
                     - grade_point_half_size
                     |> soi,
                   ),
                   ("y", line_y - grade_point_half_size |> soi),
                   ("rx", "3"),
                   ("ry", "3"),
                   ("height", grade_point_size |> soi),
                   ("width", grade_point_size |> soi),
                   ("fill", "blue"),
                 ],
                 ~attrs=[Vdom.Attr.classes(["grade-point"])],
                 (),
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
    | DHExp.Cons(DHExp.IntLit(g), d) =>
      dhexp_to_grades_invalids([g, ...rslt], invalid_count, d)
    | DHExp.Cons(_, d) =>
      dhexp_to_grades_invalids(rslt, invalid_count + 1, d)
    | DHExp.ListNil(_) => (List.rev(rslt), invalid_count)
    | _ => (List.rev(rslt), invalid_count + 1);

  let view =
      (
        {a, b, c, d, selected_grade},
        trig,
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
    let grades_svgs_invalids_opt =
      data_opt
      |> OptUtil.map(d =>
           dhexp_to_grades_invalids([], 0, d) |> grades_invalids_to_svgs
         );
    let (grades_svgs, data_err_msg) =
      switch (grades_svgs_invalids_opt) {
      | None => ([], [Vdom.Node.text("Grades data was never evaluated")])
      | Some((grades_svgs, invalid_count)) => (
          grades_svgs,
          invalid_count == 0
            ? []
            : [
              Vdom.Node.text(
                Printf.sprintf(
                  "%d grades were indeterminate or out of bounds",
                  invalid_count,
                ),
              ),
            ],
        )
      };
    Vdom.(
      Node.div(
        [Attr.classes(["grade-cutoffs-livelit"])],
        [
          Node.div([Attr.classes(["data-err-msg"])], data_err_msg),
          Node.div(
            [Attr.classes(["grade-display"])],
            [
              _svg(
                "svg",
                [
                  ("width", svg_width |> soi),
                  ("height", svg_height |> soi),
                ],
                ~attrs=[
                  Attr.on_mouseup(evt => {
                    let new_cutoff = get_cutoff_from_evt(evt);
                    let new_cutoff =
                      new_cutoff < 0 ? 0 : new_cutoff > 100 ? 100 : new_cutoff;
                    switch (selected_grade) {
                    | None => Event.Ignore
                    | Some(lg) =>
                      let old_cutoff =
                        switch (lg) {
                        | A => a
                        | B => b
                        | C => c
                        | D => d
                        };
                      if (new_cutoff == old_cutoff) {
                        Event.Ignore;
                      } else {
                        trig(DragGrade(lg, new_cutoff));
                      };
                    };
                  }),
                ],
                ~children=
                  [
                    _svg(
                      "line",
                      [
                        ("x1", margin_x_left |> soi),
                        ("y1", line_y |> soi),
                        ("x2", margin_x_left + line_width |> soi),
                        ("y2", line_y |> soi),
                        ("stroke", "black"),
                        ("stroke-width", "4"),
                      ],
                      ~attrs=[Attr.classes(["grade-line"])],
                      (),
                    ),
                  ]
                  @ grade_labels
                  @ cutoff_svgs(a, b, c, d, selected_grade, trig)
                  @ grades_svgs,
                (),
              ),
            ],
          ),
        ],
      )
    );
  };

  let view_shape = _ => {
    LivelitView.MultiLine(
      // TODO
      5,
    );
  };

  let expand = ({a, b, c, d, _}) => {
    let tupl_seq =
      UHExp.(
        Seq.mk(
          intlit'(a),
          [
            (Operators_Exp.Comma, intlit'(b)),
            (Operators_Exp.Comma, intlit'(c)),
            (Operators_Exp.Comma, intlit'(d)),
          ],
        )
      );
    UHExp.Block.wrap'(UHExp.mk_OpSeq(tupl_seq));
  };
};

module ColorLivelit: LIVELIT = {
  let name = "$color";
  let expansion_ty = HTyp.Prod([Float, Float, Float, Float]);
  let param_tys = [];

  // https://github.com/microsoft/vscode/blob/6d4f8310a96ae2dfb7eaa8d1807774fb14b3b263/src/vs/base/common/color.ts#L197
  let hsv_of_rgb = ((r, g, b)) => {
    let r = r /. 255.0;
    let g = g /. 255.0;
    let b = b /. 255.0;
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
      Float.round((r +. m) *. 255.0),
      Float.round((g +. m) *. 255.0),
      Float.round((b +. m) *. 255.0),
    );
  };

  [@deriving sexp]
  type model = {
    rgb: (SpliceName.t, SpliceName.t, SpliceName.t),
    a: SpliceName.t,
    hsv: (float, float, float),
    is_open: bool,
    selecting_sat_val: bool,
  };
  let init_model = {
    let (rval, gval, bval) as rgb_vals = (255.0, 0.0, 0.0);
    let hsv = hsv_of_rgb(rgb_vals);
    SpliceGenCmd.(
      bind(
        new_splice(
          ~init_uhexp_gen=
            u_gen => (UHExp.(Block.wrap(floatlit'(rval))), u_gen),
          HTyp.Float,
        ),
        r =>
        bind(
          new_splice(
            ~init_uhexp_gen=
              u_gen => (UHExp.(Block.wrap(floatlit'(gval))), u_gen),
            HTyp.Float,
          ),
          g =>
          bind(
            new_splice(
              ~init_uhexp_gen=
                u_gen => (UHExp.(Block.wrap(floatlit'(bval))), u_gen),
              HTyp.Float,
            ),
            b =>
            bind(
              new_splice(
                ~init_uhexp_gen=
                  u_gen => (UHExp.(Block.wrap(floatlit'(1.0))), u_gen),
                HTyp.Float,
              ),
              a =>
              return({
                rgb: (r, g, b),
                a,
                hsv,
                is_open: false,
                selecting_sat_val: false,
              })
            )
          )
        )
      )
    );
  };

  [@deriving sexp]
  type action =
    | Open
    | Close
    | StartSelectingSatVal
    | StopSelectingSatVal
    | SelectSatVal(float, float)
    | SelectHue(int);
  type trigger = action => Vdom.Event.t;
  type sync = action => unit;

  let update_hsv = (hsv, model) => {
    let (r, g, b) = model.rgb;
    let (rval, gval, bval) = rgb_of_hsv(hsv);
    SpliceGenCmd.(
      bind(
        map_splice(r, (_, u_gen) =>
          ((HTyp.Float, UHExp.(Block.wrap(floatlit'(rval)))), u_gen)
        ),
        _ =>
        bind(
          map_splice(g, (_, u_gen) =>
            ((HTyp.Float, UHExp.(Block.wrap(floatlit'(gval)))), u_gen)
          ),
          _ =>
          bind(
            map_splice(b, (_, u_gen) =>
              ((HTyp.Float, UHExp.(Block.wrap(floatlit'(bval)))), u_gen)
            ),
            _ =>
            return({...model, hsv})
          )
        )
      )
    );
  };

  let update = (m, action) =>
    switch (action) {
    | Open => SpliceGenCmd.return({...m, is_open: true})
    | Close => SpliceGenCmd.return({...m, is_open: false})
    | StartSelectingSatVal =>
      SpliceGenCmd.return({...m, selecting_sat_val: true})
    | StopSelectingSatVal =>
      SpliceGenCmd.return({...m, selecting_sat_val: false})
    | SelectSatVal(s, v) =>
      let (h, _, _) = m.hsv;
      update_hsv((h, s, v), m);
    | SelectHue(h) =>
      let (_, s, v) = m.hsv;
      update_hsv((h == 360 ? 0.0 : Float.of_int(h), s, v), m);
    };

  let view =
      (
        {rgb: (r, g, b), a, hsv, is_open: _, selecting_sat_val},
        trigger,
        _,
        {uhcode, dhcode, _}: LivelitView.splice_and_param_getters,
      ) => {
    open Vdom;

    let is_valid = color_value => 0.0 <= color_value && color_value < 256.0;
    let rgba_values =
      switch (dhcode(r), dhcode(g), dhcode(b), dhcode(a)) {
      | (
          Some((FloatLit(r), _)),
          Some((FloatLit(g), _)),
          Some((FloatLit(b), _)),
          Some((FloatLit(a), _)),
        )
          when
            is_valid(r)
            && is_valid(g)
            && is_valid(b)
            && 0.0 <= a
            && a <= 1.0 =>
        Some((r, g, b, a))
      | _ => None
      };

    let _color_box = {
      let on_click = Attr.on_click(_ => trigger(Open));
      switch (rgba_values) {
      | None =>
        Node.div(
          [Attr.classes(["color-box"]), on_click],
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
        Node.div(
          [
            attr_style(
              prop_val(
                "background-color",
                Printf.sprintf("rgba(%f, %f, %f, %f)", r, g, b, a),
              ),
            ),
            Attr.classes(["color-box"]),
            on_click,
          ],
          [],
        )
      };
    };

    let color_picker = {
      let height = 135.0;
      let width = 135.0;
      let sat_val_box =
        switch (rgba_values) {
        | None => []
        | Some(_) =>
          let (h, s, v) = hsv;
          let (sat_r, sat_g, sat_b) = rgb_of_hsv((h, 1.0, 1.0));
          // let (_h, s, v) = hsv_of_rgb(rgb);
          let px = Printf.sprintf("%f0px");
          [
            Node.div(
              [
                Attr.classes(["sat-val-box"]),
                attr_style(
                  StringUtil.cat([
                    prop_val("height", height |> px),
                    prop_val("width", width |> px),
                    prop_val(
                      "background-color",
                      Printf.sprintf("rgb(%f, %f, %f)", sat_r, sat_g, sat_b),
                    ),
                  ]),
                ),
                Attr.on_mousemove(evt =>
                  if (selecting_sat_val) {
                    let (offset_x, offset_y) = {
                      let target =
                        Js.Opt.get(evt##.target, () => failwith("no target"));
                      if (target |> JSUtil.elem_has_cls("sat-val-box")) {
                        (
                          Float.of_int(Js.Unsafe.get(evt, "offsetX")),
                          Float.of_int(Js.Unsafe.get(evt, "offsetY")),
                        );
                      } else {
                        let box = JSUtil.force_get_parent_elem(target);
                        let rect = box##getBoundingClientRect;
                        let client_x = Float.of_int(evt##.clientX);
                        let client_y = Float.of_int(evt##.clientY);
                        (client_x -. rect##.left, client_y -. rect##.top);
                      };
                    };
                    trigger(
                      SelectSatVal(
                        max(0.0, min(offset_x /. width, 1.0)),
                        max(0.0, min((height -. offset_y) /. height, 1.0)),
                      ),
                    );
                  } else {
                    Event.Many([]);
                  }
                ),
              ],
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
                    Attr.on_mouseup(_ => trigger(StopSelectingSatVal)),
                  ],
                  [],
                ),
              ],
            ),
          ];
        };
      let splice_label = lbl =>
        Node.label([Attr.classes(["splice-label"])], [Node.text(lbl)]);
      let splice = splice_name =>
        Node.div(
          [Attr.classes(["splice-content"])],
          [uhcode(splice_name)],
        );
      let (h, _, _) = hsv;
      Node.div(
        [
          Attr.classes([
            "color-picker",
            "open" /*is_open ? "open" : "closed"*/,
          ]),
        ],
        [
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
            sat_val_box
            @ [
              Node.div(
                [Attr.classes(["hue-slider-wrapper"])],
                [
                  Node.input(
                    [
                      Attr.classes(["hue-slider"]),
                      Attr.type_("range"),
                      Attr.create("min", "0"),
                      Attr.create("max", "360"),
                      Attr.value(string_of_int(Float.to_int(h))),
                      Attr.on_input((_, value_str) =>
                        trigger(SelectHue(int_of_string(value_str)))
                      ),
                      Attr.create(
                        "style",
                        Printf.sprintf("width: %fpx;", height),
                      ),
                    ],
                    [],
                  ),
                ],
              ),
            ],
          ),
        ],
      );
    };
    /*
     let modal_overlay =
       Node.div(
         [
           Attr.classes(["modal-overlay", is_open ? "open" : "closed"]),
           Attr.on_click(_ => trigger(Close)),
         ],
         [],
       );
     */
    Node.div(
      [Attr.classes(["color-livelit"])],
      [/*color_box,*/ color_picker] // modal_overlay],
    );
  };

  let view_shape = _ => LivelitView.MultiLine(8);

  let expand = ({rgb: (r, g, b), a, _}) => {
    let four_tuple =
      Seq.mk(
        _to_uhvar(r),
        [
          (Operators_Exp.Comma, _to_uhvar(g)),
          (Operators_Exp.Comma, _to_uhvar(b)),
          (Operators_Exp.Comma, _to_uhvar(a)),
        ],
      );
    UHExp.Block.wrap'(UHExp.mk_OpSeq(four_tuple));
  };
};

module ColorLivelitAdapter = LivelitAdapter(ColorLivelit);

module GradientLivelit: LIVELIT = {
  let name = "$gradient";
  let expansion_ty = ColorLivelit.expansion_ty;
  let param_tys = [];

  let (color_livelit_ctx, _) =
    LivelitContexts.extend(
      LivelitContexts.empty,
      ColorLivelitAdapter.contexts_entry,
    );
  let color_ctx = (VarCtx.empty, color_livelit_ctx);

  [@deriving sexp]
  type model = {
    lcolor: SpliceName.t,
    rcolor: SpliceName.t,
    slider_value: int,
  };
  [@deriving sexp]
  type action =
    | Slide(int);
  type trigger = action => Vdom.Event.t;
  type sync = action => unit;

  let slider_min = 0;
  let slider_max = 100;
  let init_slider_value = 50;
  let init_model = {
    let init_uhexp_gen = u_gen => {
      let (u, u_gen) = MetaVarGen.next_livelit(u_gen);
      let (e, _, u_gen) =
        Statics_Exp.syn_fix_holes(
          color_ctx,
          u_gen,
          UHExp.Block.wrap(FreeLivelit(u, "$color")),
        );
      (e, u_gen);
    };
    SpliceGenCmd.(
      bind(new_splice(~init_uhexp_gen, ColorLivelit.expansion_ty), lcolor =>
        bind(new_splice(~init_uhexp_gen, ColorLivelit.expansion_ty), rcolor =>
          return({lcolor, rcolor, slider_value: init_slider_value})
        )
      )
    );
  };

  let update = (model, a) =>
    switch (a) {
    | Slide(n) => SpliceGenCmd.return({...model, slider_value: n})
    };

  let view =
      (model, trigger, _, {uhcode, _}: LivelitView.splice_and_param_getters) => {
    Vdom.(
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
                trigger(Slide(new_value));
              }),
            ],
            [],
          ),
          uhcode(model.rcolor),
        ],
      )
    );
  };

  let view_shape = _ => LivelitView.Inline(10);

  let expand = ({lcolor, rcolor, slider_value}) => {
    let typ_opseq = (hd, tl) => UHTyp.mk_OpSeq(Seq.mk(hd, tl));
    let pat_opseq = (hd, tl) => UHPat.mk_OpSeq(Seq.mk(hd, tl));
    let exp_opseq = (hd, tl) => UHExp.mk_OpSeq(Seq.mk(hd, tl));
    let typ_triple = (x1, x2, x3) =>
      typ_opseq(x1, [(Operators_Typ.Prod, x2), (Operators_Typ.Prod, x3)]);
    let pat_triple = (x1, x2, x3) =>
      UHPat.(pat_opseq(var(x1), [(Comma, var(x2)), (Comma, var(x3))]));
    let scalar =
      UHExp.floatlit'(
        float_of_int(slider_value) /. float_of_int(slider_max),
      );
    let interpolate_vars = (x1, x2) =>
      UHExp.(
        Parenthesized(
          Block.wrap'(
            exp_opseq(
              var(x1),
              [
                (FPlus, scalar),
                (
                  FTimes,
                  Parenthesized(
                    Block.wrap'(exp_opseq(var(x2), [(FMinus, var(x1))])),
                  ),
                ),
              ],
            ),
          ),
        )
      );
    let interpolate =
      UHExp.lam(
        UHPat.(
          pat_opseq(
            Parenthesized(pat_triple("r1", "g1", "b1")),
            [(Comma, Parenthesized(pat_triple("r2", "g2", "b2")))],
          )
        ),
        ~ann=
          UHTyp.(
            typ_opseq(
              Parenthesized(typ_triple(Float, Float, Float)),
              [
                (
                  Operators_Typ.Prod,
                  Parenthesized(typ_triple(Float, Float, Float)),
                ),
              ],
            )
          ),
        UHExp.(
          Block.wrap'(
            exp_opseq(
              interpolate_vars("r1", "r2"),
              [
                (Comma, interpolate_vars("g1", "g2")),
                (Comma, interpolate_vars("b1", "b2")),
              ],
            ),
          )
        ),
      );
    UHExp.(
      Block.wrap'(
        exp_opseq(
          interpolate,
          [
            (
              Space,
              Parenthesized(
                Block.wrap'(
                  exp_opseq(
                    _to_uhvar(lcolor),
                    [(Comma, _to_uhvar(rcolor))],
                  ),
                ),
              ),
            ),
          ],
        ),
      )
    );
  };
};

module CheckboxLivelit: LIVELIT = {
  let name = "$checkbox";
  let expansion_ty = HTyp.Bool;
  let param_tys = [];

  [@deriving sexp]
  type model = bool;
  [@deriving sexp]
  type action =
    | Toggle;
  type trigger = action => Vdom.Event.t;
  type sync = action => unit;

  let init_model = SpliceGenCmd.return(false);
  let update = (m, Toggle) => SpliceGenCmd.return(!m);

  let view = (m, trig, _) => {
    let checked_state = m ? [Vdom.Attr.checked] : [];
    let input_elt =
      Vdom.(
        Node.input(
          [
            Attr.type_("checkbox"),
            Attr.on_input((_, _) => trig(Toggle)),
            ...checked_state,
          ],
          [],
        )
      );
    _ => Vdom.Node.span([], [input_elt]);
  };

  let view_shape = _ => LivelitView.Inline(/* TODO! */ 1);

  let expand = m => UHExp.Block.wrap(UHExp.BoolLit(NotInHole, m));
};

module SliderLivelit: LIVELIT = {
  let name = "$slider";
  let expansion_ty = HTyp.Float;
  let param_tys = [("min", HTyp.Float), ("max", HTyp.Float)];

  [@deriving sexp]
  type endpoint =
    | Min
    | Max;

  [@deriving sexp]
  type model = option(float);

  [@deriving sexp]
  type action =
    | InvalidParams
    | Slide(float);
  type trigger = action => Vdom.Event.t;
  type sync = action => unit;

  let init_model = SpliceGenCmd.return(Some(0.));

  let update = (_, a) =>
    switch (a) {
    | InvalidParams => SpliceGenCmd.return(None)
    | Slide(f) => SpliceGenCmd.return(Some(f))
    };

  let view = (model, trigger: trigger, sync) => {
    open Vdom;

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
      Vdom.(
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
        )
      );
    };

    let slider =
        (
          ~disabled: bool,
          ~min: float=0.0,
          ~max: float=100.0,
          ~value: float=50.0,
          (),
        ) =>
      Node.span(
        [Attr.classes(["slider-livelit"])],
        [
          // tickmarks(min, max),
          Node.input(
            [
              Attr.classes(["slider"]),
              Attr.type_("range"),
              Attr.create("min", FloatUtil.to_string_zero(min)),
              Attr.create("max", FloatUtil.to_string_zero(max)),
              Attr.create("step", "0.01"),
              Attr.create("list", "tickmarks"),
              Attr.value(FloatUtil.to_string_zero(value)),
              Attr.on_change((_, value_str) =>
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
          | Some(f) when f == 0.5 *. min +. 0.5 *. max => f
          | _ =>
            let new_value = 0.5 *. min +. 0.5 *. max;
            sync(Slide(new_value));
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

  let view_shape = _ => LivelitView.Inline(24);

  let expand =
    fun
    | None => UHExp.Block.wrap(UHExp.floatlit'(0.0))
    | Some(f) => UHExp.Block.wrap(UHExp.floatlit'(f));
};

/* ----------
   stuff below is infrastructure
   ---------- */

module GradientLivelitAdapter = LivelitAdapter(GradientLivelit);
module CheckboxLivelitAdapter = LivelitAdapter(CheckboxLivelit);
module PairLivelitAdapter = LivelitAdapter(PairLivelit);
module SliderLivelitAdapter = LivelitAdapter(SliderLivelit);
module MatrixLivelitAdapter = LivelitAdapter(MatrixLivelit);
module LiveMatrixLivelitAdapter = LivelitAdapter(LiveMatrixLivelit);
module GradeCutoffLivelitAdapter = LivelitAdapter(GradeCutoffLivelit);
let empty_livelit_contexts = LivelitContexts.empty;
let (initial_livelit_ctx, initial_livelit_view_ctx) =
  LivelitContexts.extend(
    LivelitContexts.extend(
      LivelitContexts.extend(
        LivelitContexts.extend(
          LivelitContexts.extend(
            LivelitContexts.extend(
              LivelitContexts.extend(
                LivelitContexts.extend(
                  empty_livelit_contexts,
                  GradeCutoffLivelitAdapter.contexts_entry,
                ),
                MatrixLivelitAdapter.contexts_entry,
              ),
              LiveMatrixLivelitAdapter.contexts_entry,
            ),
            PairLivelitAdapter.contexts_entry,
          ),
          CheckboxLivelitAdapter.contexts_entry,
        ),
        SliderLivelitAdapter.contexts_entry,
      ),
      ColorLivelitAdapter.contexts_entry,
    ),
    GradientLivelitAdapter.contexts_entry,
  );
