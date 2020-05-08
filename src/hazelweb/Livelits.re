open Sexplib.Std;

module Dom_html = Js_of_ocaml.Dom_html;
module Dom = Js_of_ocaml.Dom;
module Js = Js_of_ocaml.Js;
module Vdom = Virtual_dom.Vdom;

type div_type = Vdom.Node.t;

type splice_getters = {
  uhcode: SpliceName.t => div_type,
  dhcode: SpliceName.t => option((DHExp.t, div_type)),
};

type splice_getters_to_vdom = splice_getters => div_type;

module LivelitView = {
  type t =
    | Inline(splice_getters_to_vdom, int)
    | MultiLine(splice_getters_to_vdom, int);

  let get_splice_getters_to_vdom =
    fun
    | Inline(x, _)
    | MultiLine(x, _) => x;
};

module type LIVELIT = {
  let name: LivelitName.t;
  let expansion_ty: HTyp.t;

  [@deriving sexp]
  type model;
  [@deriving sexp]
  type action;
  type trigger = action => Vdom.Event.t;

  let init_model: SpliceGenCmd.t(model);
  let update: (model, action) => SpliceGenCmd.t(model);
  let view: (model, trigger) => LivelitView.t;
  let expand: model => UHExp.t;
};

let _to_uhvar = id =>
  UHExp.(Var(NotInHole, NotInVarHole, SpliceInfo.var_of_splice_name(id)));

let attr_style = Vdom.Attr.create("style");

let prop_val = (prop: string, value: string) =>
  StringUtil.cat([prop, ": ", value, ";"]);

module PairLivelit: LIVELIT = {
  let name = "$pair";
  let expansion_ty = HTyp.(Prod(Hole, Hole));

  [@deriving sexp]
  type model = (int, int);
  [@deriving sexp]
  type action = unit;
  type trigger = action => Vdom.Event.t;

  let init_model =
    SpliceGenCmd.bind(SpliceGenCmd.new_splice(HTyp.Hole), leftID =>
      SpliceGenCmd.bind(SpliceGenCmd.new_splice(HTyp.Hole), rightID =>
        SpliceGenCmd.return((leftID, rightID))
      )
    );
  let update = (m, _) => SpliceGenCmd.return(m);

  let view = ((leftID, rightID), _) =>
    LivelitView.Inline(
      ({uhcode, _}) =>
        Vdom.(
          Node.div(
            [Attr.classes(["pair-livelit"])],
            [uhcode(leftID), uhcode(rightID)],
          )
        ),
      // TODO fix brittle magic constant
      20,
    );

  let expand = ((leftID, rightID)) => {
    let pair_seq =
      Seq.mk(_to_uhvar(leftID), [(UHExp.Comma, _to_uhvar(rightID))]);
    UHExp.Block.wrap'(
      OpSeq.mk(~associate=Associator.Exp.associate, pair_seq),
    );
  };
};

module type MAT_INFO = {
  let name: LivelitName.t;
  let is_live: bool;
};

module MatrixLivelitFunctor = (I: MAT_INFO) : LIVELIT => {
  let name = I.name;
  let expansion_ty = HTyp.(List(List(Int)));

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

  let view = ((selected, m), trig) => {
    let splice_getters_to_vdom = ({uhcode, dhcode}) => {
      open Vdom;
      let width = get_width(m);
      let height = get_height(m);
      let row_header =
        ListUtil.range(height)
        |> List.map(i =>
             Node.div(
               [
                 attr_style(grid_area(i + 2, 1, i + 3, 2)),
                 Attr.classes(["row-header"]),
               ],
               [
                 Node.span(
                   [Attr.classes(["index"])],
                   [Node.text(string_of_int(i + 1))],
                 ),
                 Node.span(
                   [
                     Attr.classes(["delete"]),
                     Attr.on_click(_ => trig(Del(Row, i))),
                   ],
                   [Node.text("x")],
                 ),
               ],
             )
           );
      let col_header =
        ListUtil.range(width)
        |> List.map(j =>
             Node.span(
               [
                 attr_style(grid_area(1, j + 2, 2, j + 3)),
                 Attr.classes(["col-header"]),
               ],
               [
                 Node.span(
                   [Attr.classes(["index"])],
                   [Node.text(string_of_int(j + 1))],
                 ),
                 Node.span(
                   [
                     Attr.classes(["delete"]),
                     Attr.on_click(_ => trig(Del(Col, j))),
                   ],
                   [Node.text("x")],
                 ),
               ],
             )
           );
      let add_row_button =
        Node.button(
          [
            attr_style(grid_area(-1, 2, -2, -2)),
            Attr.classes(["add-row", "pure-button"]),
            Attr.on_click(_ => trig(Add(Row))),
          ],
          [Node.text("+")],
        );
      let add_col_button =
        Node.button(
          [
            attr_style(grid_area(2, -2, -2, -1)),
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
      let splices =
        m
        |> List.mapi((i, row) =>
             row
             |> List.mapi((j, splice) => {
                  let style =
                    attr_style(grid_area(i + 2, j + 2, i + 3, j + 3));
                  if (!I.is_live) {
                    Node.div(
                      [style, Attr.classes(["matrix-splice"])],
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
                        style,
                        Attr.classes([cls]),
                        Attr.on_mousedown(_ => trig(Select(splice))),
                      ],
                      [child],
                    );
                  };
                })
           )
        |> List.flatten;

      maybe_add_formula_bar(
        Node.div(
          [
            Attr.classes(["matrix-livelit"]),
            attr_style(
              StringUtil.cat([
                prop_val(
                  "grid-template-columns",
                  StringUtil.sep(ListUtil.replicate(width + 2, "auto")),
                ),
                prop_val(
                  "grid-template-rows",
                  StringUtil.sep(ListUtil.replicate(height + 2, "auto")),
                ),
              ]),
            ),
          ],
          List.concat([
            row_header,
            col_header,
            splices,
            [add_row_button, add_col_button],
          ]),
        ),
      );
    };
    let num_rows = List.length(m);
    LivelitView.MultiLine(
      splice_getters_to_vdom,
      num_rows + 2 + (I.is_live ? 1 : 0),
    );
  };

  let expand = ((_, m)) => {
    let to_uhexp_list =
      fun
      | [] => UHExp.(Block.wrap(ListNil(NotInHole)))
      | [fst, ...rest] => {
          let rest' =
            (rest |> List.map(item => (UHExp.Cons, item)))
            @ [(UHExp.Cons, UHExp.ListNil(NotInHole))];
          let seq = Seq.mk(fst, rest');
          UHExp.Block.wrap'(
            OpSeq.mk(~associate=Associator.Exp.associate, seq),
          );
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
  let expansion_ty = HTyp.(Prod(Int, Prod(Int, Prod(Int, Int))));

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
    dataID: SpliceName.t,
  };

  [@deriving sexp]
  type action =
    | SelectLetterGrade(option(letter_grade))
    | MoveSelectedGrade(int)
    | DragGrade(letter_grade, int);

  type trigger = action => Vdom.Event.t;

  let init_model =
    SpliceGenCmd.(
      bind(new_splice(HTyp.(List(Int))), dataID =>
        return({a: 90, b: 80, c: 70, d: 60, selected_grade: None, dataID})
      )
    );

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

  let svgs_of_grades = grades => {
    let valid_grades =
      grades
      |> List.for_all(grade => {
           let valid = grade >= 0 && grade <= 100;
           if (!valid) {
             JSUtil.log(Printf.sprintf("Invalid grade %d", grade));
           };
           valid;
         });
    if (valid_grades) {
      Some(
        grades
        |> List.map(grade =>
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
             )
           ),
      );
    } else {
      None;
    };
  };

  let rec dhexp_to_grades = rslt =>
    fun
    | DHExp.ListNil(_) => Some(List.rev(rslt))
    | DHExp.Cons(DHExp.IntLit(g), d) => dhexp_to_grades([g, ...rslt], d)
    | DHExp.LivelitInfo(_, _, _, _, _, d) => dhexp_to_grades(rslt, d)
    // currently, we are very strict, and the presence of any indet is immediate failure
    | _ => None;

  let view = ({a, b, c, d, selected_grade, dataID}, trig) => {
    let splice_getters_to_vdom = ({uhcode, dhcode}) => {
      let data_opt = dhcode(dataID);
      let grades_opt =
        data_opt |> OptUtil.and_then(((d, _)) => dhexp_to_grades([], d));
      let grades_svgs_opt = grades_opt |> OptUtil.and_then(svgs_of_grades);
      let grades_svgs = grades_svgs_opt |> OptUtil.get_default(~default=[]);
      let data_err_msg =
        switch (data_opt, grades_opt, grades_svgs_opt) {
        | (None, _, _) => [
            Vdom.Node.text("Grades data was never evaluated"),
          ]
        | (_, None, _) => [
            Vdom.Node.text(
              "Grades data was indeterminate (maybe it contains a hole?)",
            ),
          ]
        | (_, _, None) => [
            Vdom.Node.text(
              "Grades data was invalid (i.e. it contains a number < 0 or > 100)",
            ),
          ]
        | _ => []
        };
      Vdom.(
        Node.div(
          [Attr.classes(["grade-cutoffs-livelit"])],
          [
            Node.div(
              [Attr.classes(["data-splice"])],
              [Node.text("data = "), uhcode(dataID)],
            ),
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
                        new_cutoff < 0
                          ? 0 : new_cutoff > 100 ? 100 : new_cutoff;
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
    LivelitView.MultiLine(
      splice_getters_to_vdom,
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
            (Comma, intlit'(b)),
            (Comma, intlit'(c)),
            (Comma, intlit'(d)),
          ],
        )
      );
    UHExp.Block.wrap'(
      OpSeq.mk(~associate=Associator.Exp.associate, tupl_seq),
    );
  };
};

module ColorLivelit: LIVELIT = {
  let name = "$color";
  let expansion_ty = HTyp.Prod(Int, Prod(Int, Int));

  [@deriving sexp]
  type model = {
    r: SpliceName.t,
    g: SpliceName.t,
    b: SpliceName.t,
    is_open: bool,
  };
  let init_model =
    SpliceGenCmd.(
      bind(
        new_splice(
          ~init_uhexp_gen=
            u_gen => (UHExp.(Block.wrap(intlit'(255))), u_gen),
          HTyp.Int,
        ),
        r =>
        bind(
          new_splice(
            ~init_uhexp_gen=
              u_gen => (UHExp.(Block.wrap(intlit'(0))), u_gen),
            HTyp.Int,
          ),
          g =>
          bind(
            new_splice(
              ~init_uhexp_gen=
                u_gen => (UHExp.(Block.wrap(intlit'(0))), u_gen),
              HTyp.Int,
            ),
            b =>
            return({r, g, b, is_open: false})
          )
        )
      )
    );
  [@deriving sexp]
  type action =
    | Open
    | Close;
  type trigger = action => Vdom.Event.t;
  let update = (m, action) =>
    SpliceGenCmd.return(
      switch (action) {
      | Open => {...m, is_open: true}
      | Close => {...m, is_open: false}
      },
    );

  let view = ({r, g, b, is_open}, trigger) => {
    LivelitView.Inline(
      ({uhcode: _, dhcode}) => {
        open Vdom;
        let color_box =
          switch (dhcode(r), dhcode(g), dhcode(b)) {
          | (
              Some((IntLit(r), _)),
              Some((IntLit(g), _)),
              Some((IntLit(b), _)),
            ) =>
            Node.div(
              [
                attr_style(
                  prop_val(
                    "background-color",
                    Printf.sprintf("rgb(%d, %d, %d)", r, g, b),
                  ),
                ),
                Attr.classes(["color-box"]),
              ],
              [],
            )
          | _ =>
            Node.div(
              [Attr.classes(["color-box"])],
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
          };

        let color_picker =
          Node.div(
            [Attr.classes(["color-picker", is_open ? "open" : "closed"])],
            [],
          );
        Node.div(
          [
            Attr.classes(["color-livelit"]),
            Attr.create("tabindex", "0"),
            Attr.on_focus(_ => trigger(Open)),
            Attr.on_blur(_ => trigger(Close)),
          ],
          [color_box, color_picker],
        );
      },
      2,
    );
  };

  let expand = ({r, g, b, _}) => {
    let triple_seq =
      Seq.mk(
        _to_uhvar(r),
        [(UHExp.Comma, _to_uhvar(g)), (UHExp.Comma, _to_uhvar(b))],
      );
    UHExp.Block.wrap'(
      OpSeq.mk(~associate=Associator.Exp.associate, triple_seq),
    );
  };
};

module CheckboxLivelit: LIVELIT = {
  let name = "$checkbox";
  let expansion_ty = HTyp.Bool;

  [@deriving sexp]
  type model = bool;
  [@deriving sexp]
  type action =
    | Toggle;
  type trigger = action => Vdom.Event.t;

  let init_model = SpliceGenCmd.return(false);
  let update = (m, Toggle) => SpliceGenCmd.return(!m);

  let view = (m, trig) => {
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
    let view_span = Vdom.Node.span([], [input_elt]);
    LivelitView.Inline(_ => view_span, /* TODO! */ 1);
  };

  let expand = m => UHExp.Block.wrap(UHExp.BoolLit(NotInHole, m));
};

module SliderLivelit: LIVELIT = {
  let name = "$slider";
  let expansion_ty = HTyp.Int;

  module EditableValue = {
    [@deriving sexp]
    type t = {
      value: int,
      editing: option(string),
    };

    let init = value => {value, editing: None};

    let start_editing = editable => {
      ...editable,
      editing: Some(string_of_int(editable.value)),
    };

    let edit = (new_edit_value, editable) => {
      ...editable,
      editing: Some(new_edit_value),
    };

    let stop_editing = (new_value, editable) => {
      editing: None,
      value:
        switch (new_value) {
        | None => editable.value
        | Some(new_value) => new_value
        },
    };
  };

  [@deriving sexp]
  type endpoint =
    | Min
    | Max;

  [@deriving sexp]
  type model = {
    min: EditableValue.t,
    max: EditableValue.t,
    value: int,
  };

  let map_endpoint = (f, endpoint, model) =>
    switch (endpoint) {
    | Min => {...model, min: f(model.min)}
    | Max => {...model, max: f(model.max)}
    };

  let start_editing = map_endpoint(EditableValue.start_editing);
  let edit = (endpoint, new_value, model) =>
    model |> map_endpoint(EditableValue.edit(new_value), endpoint);
  let stop_editing = (endpoint, new_value, model) =>
    model |> map_endpoint(EditableValue.stop_editing(new_value), endpoint);

  [@deriving sexp]
  type action =
    | Slide(int)
    | StartEditing(endpoint)
    | Edit(endpoint, string)
    | StopEditing(endpoint, option(int));
  type trigger = action => Vdom.Event.t;

  /* overflow paranoia */
  let max_slider_value = 1000 * 1000 * 1000;
  let min_slider_value = (-1) * max_slider_value;
  /*let crop_slider_value = value =>
    max(min_slider_value, min(max_slider_value, value));*/

  let init_model =
    SpliceGenCmd.return({
      min: EditableValue.init(0),
      max: EditableValue.init(10),
      value: 5,
    });
  let update = (model, a) =>
    switch (a) {
    | Slide(n) => SpliceGenCmd.return({...model, value: n})
    | StartEditing(Min) => SpliceGenCmd.return(model |> start_editing(Min))
    | StartEditing(Max) => SpliceGenCmd.return(model |> start_editing(Max))
    | Edit(endpoint, new_value) =>
      SpliceGenCmd.return(model |> edit(endpoint, new_value))
    | StopEditing(endpoint, new_value) =>
      SpliceGenCmd.return(model |> stop_editing(endpoint, new_value))
    };

  let view = (model, trigger: trigger) => {
    open Vdom;

    let endpoint_view =
        (
          ~clss,
          ~is_valid: string => option(int),
          (endpoint, editable: EditableValue.t),
        ) => {
      let editing_clss =
        switch (editable.editing) {
        | None => []
        | Some(_) => ["editing"]
        };
      let err_clss =
        switch (editable.editing) {
        | None => []
        | Some(edit_value) =>
          switch (is_valid(edit_value)) {
          | Some(_) => []
          | None => ["err"]
          }
        };
      let display_value =
        switch (editable.editing) {
        | None => string_of_int(editable.value)
        | Some(edit_value) => edit_value
        };
      let padding = "3px";
      Node.input(
        [
          Attr.classes(clss @ editing_clss @ err_clss),
          Attr.type_("text"),
          attr_style(
            StringUtil.sep([
              prop_val("padding", padding),
              prop_val(
                "width",
                Printf.sprintf(
                  "calc(%dch + %s)",
                  String.length(display_value),
                  padding,
                ),
              ),
            ]),
          ),
          // only sets initial value
          Attr.value(display_value),
          Attr.on_focus(_ => trigger(StartEditing(endpoint))),
          Attr.on_blur(evt => {
            let new_value = Option.bind(editable.editing, is_valid);
            {
              let target = Js.Opt.get(evt##.target, () => assert(false));
              let new_value =
                switch (new_value) {
                | None => editable.value
                | Some(new_value) => new_value
                };
              Js.Unsafe.set(target, "value", string_of_int(new_value));
            };
            trigger(StopEditing(endpoint, new_value));
          }),
          Attr.on_input((_, new_edit_value) =>
            trigger(Edit(endpoint, new_edit_value))
          ),
        ],
        [],
      );
    };

    let view_span =
      Node.span(
        [Attr.classes(["slider-livelit"])],
        [
          endpoint_view(
            ~clss=["min-input"],
            ~is_valid=
              edit_value =>
                edit_value
                |> int_of_string_opt
                |> OptUtil.filter(new_min =>
                     min_slider_value < new_min && new_min < model.max.value
                   ),
            (Min, model.min),
          ),
          Node.input(
            [
              Attr.classes(["slider"]),
              Attr.type_("range"),
              Attr.create("min", string_of_int(model.min.value)),
              Attr.create("max", string_of_int(model.max.value)),
              Attr.value(string_of_int(model.value)),
              Attr.on_change((_, value_str) => {
                let new_value = int_of_string(value_str);
                trigger(Slide(new_value));
              }),
            ],
            [],
          ),
          endpoint_view(
            ~clss=["max-input"],
            ~is_valid=
              edit_value =>
                edit_value
                |> int_of_string_opt
                |> OptUtil.filter(new_max =>
                     model.min.value < new_max && new_max < max_slider_value
                   ),
            (Max, model.max),
          ),
        ],
      );
    LivelitView.Inline(_ => view_span, 10);
  };

  let expand = model => UHExp.Block.wrap(UHExp.intlit'(model.value));
};

/* ----------
   stuff below is infrastructure
   ---------- */

type trigger_serialized = SerializedAction.t => Vdom.Event.t;
type serialized_view_fn_t =
  (SerializedModel.t, trigger_serialized) => LivelitView.t;

module LivelitViewCtx = {
  type t = VarMap.t_(serialized_view_fn_t);
  include VarMap;
};

module LivelitContexts = {
  type t = (LivelitCtx.t, LivelitViewCtx.t);
  let empty = (LivelitCtx.empty, LivelitViewCtx.empty);
  let extend =
      ((livelit_ctx, livelit_view_ctx), (name, def, serialized_view_fn)) => {
    if (!LivelitName.is_valid(name)) {
      failwith("Invalid livelit name " ++ name);
    };
    (
      VarMap.extend(livelit_ctx, (name, def)),
      VarMap.extend(livelit_view_ctx, (name, serialized_view_fn)),
    );
  };
};

module LivelitAdapter = (L: LIVELIT) => {
  let serialize_monad = model => SpliceGenCmd.return(L.sexp_of_model(model));

  /* generate palette definition for Semantics */
  let livelit_defn =
    LivelitDefinition.{
      expansion_ty: L.expansion_ty,
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

  let serialized_view_fn = (serialized_model, update_fn) =>
    L.view(L.model_of_sexp(serialized_model), action =>
      update_fn(L.sexp_of_action(action))
    );

  let contexts_entry = (L.name, livelit_defn, serialized_view_fn);
};

module ColorLivelitAdapter = LivelitAdapter(ColorLivelit);
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
  );
