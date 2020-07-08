module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;

module Dec = {
  type rects = list(RectilinearPolygon.rect);

  let rects =
      (~vtrim=0.0, ~indent=0, start: CaretPosition.t, m: MeasuredLayout.t)
      : (CaretPosition.t, list(RectilinearPolygon.rect)) => {
    let mk_rect =
        (
          ~is_first=false,
          ~is_last=false,
          start: CaretPosition.t,
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
           (start: CaretPosition.t, (i, box: MeasuredLayout.box)) =>
             (
               {row: start.row + box.height, col: 0},
               mk_rect(~is_first=i == 0, start, box),
             ),
           start,
         );
    let end_: CaretPosition.t = {
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
        subject: MeasuredLayout.t,
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
        subject: MeasuredLayout.t,
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

  let tessera_rects =
      (start: CaretPosition.t, m: MeasuredLayout.t)
      : (CaretPosition.t, (rects, list(rects))) => {
    let (end_, highlighted_rs) = rects(start, m);
    let closed_child_rss =
      m
      |> MeasuredLayout.flatten
      |> ListUtil.map_with_accumulator(
           (line_start, line: list(MeasuredLayout.t)) => {
             line
             |> ListUtil.map_with_accumulator(
                  (word_start, word: MeasuredLayout.t) => {
                    switch (word) {
                    | {layout: Annot(ClosedChild(_), m), _} =>
                      let (word_end, rs) = rects(~vtrim=0.1, word_start, m);
                      (word_end, rs);
                    | _ => (
                        MeasuredLayout.next_position(
                          ~indent=0,
                          word_start,
                          word,
                        ),
                        [],
                      )
                    }
                  },
                  line_start,
                )
             |> (
               fun
               | (line_end, rss) => (
                   line_end,
                   List.filter(rs => !ListUtil.is_empty(rs), rss),
                 )
             )
           },
           start,
         )
      |> snd
      |> List.flatten;
    (end_, (highlighted_rs, closed_child_rss));
  };

  let inline_open_child_rects =
      (start: CaretPosition.t, m: MeasuredLayout.t): rects => {
    // TODO relax assumption
    assert(MeasuredLayout.height(m) == 1);
    // make singleton skinny rect along bottom
    [
      RectilinearPolygon.{
        min: {
          x: Float.of_int(start.col),
          y: Float.of_int(start.row) +. 1. -. 0.1,
        },
        height: 0.1, // TODO
        width: Float.of_int(MeasuredLayout.width(m)),
      },
    ];
  };

  let multiline_open_child_rects =
      (~overflow_left, start: CaretPosition.t, m: MeasuredLayout.t): rects => {
    let overflow_left = overflow_left ? 0.1 : 0.0;
    [
      // make singleton skinny rect
      RectilinearPolygon.{
        min: {
          x: Float.of_int(start.col) -. overflow_left,
          y: Float.of_int(start.row),
        },
        height: Float.of_int(MeasuredLayout.height(m)),
        width: 0.25 // TODO
      },
    ];
  };

  let line_rects =
      (
        ~overflow_left: bool,
        start: CaretPosition.t,
        line: list(MeasuredLayout.t),
      )
      : (CaretPosition.t, (rects, list(rects))) => {
    let (end_, zipped) =
      line
      |> ListUtil.map_with_accumulator(
           (word_start, word: MeasuredLayout.t) => {
             switch (word) {
             | {layout: Annot(Tessera, m), _} =>
               tessera_rects(word_start, m)
             | {layout: Annot(OpenChild({is_inline, _}), m), _} =>
               let highlighted_rs =
                 is_inline
                   ? inline_open_child_rects(word_start, m)
                   : multiline_open_child_rects(
                       ~overflow_left=true,
                       word_start,
                       m,
                     );
               let word_end =
                 MeasuredLayout.next_position(~indent=0, word_start, m);
               (word_end, (highlighted_rs, []));
             | _ =>
               failwith(
                 "Doc nodes annotated as Term should only contain Tessera and OpenChild (flat) children",
               )
             }
           },
           start,
         );
    let (highlighted_rs, closed_child_rss) = List.split(zipped);
    let highlighted_rs =
      switch (line) {
      | [{layout: Annot(Tessera, m), _}, ..._] when overflow_left =>
        let height = MeasuredLayout.height(m);
        [
          RectilinearPolygon.{
            min: {
              x: Float.of_int(start.col) -. 0.1,
              y: Float.of_int(start.row),
            },
            height: Float.of_int(height),
            width: 0.1,
          },
          ...List.flatten(highlighted_rs),
        ];
      | _ => List.flatten(highlighted_rs)
      };
    (end_, (highlighted_rs, List.flatten(closed_child_rss)));
  };

  let lines_rects =
      (
        ~overflow_left: bool,
        start: CaretPosition.t,
        lines: list(list(MeasuredLayout.t)),
      )
      : (CaretPosition.t, (rects, list(rects))) => {
    lines
    |> ListUtil.map_with_accumulator(
         (line_start, line: list(MeasuredLayout.t)) => {
           let (line_end, rss) =
             switch (line) {
             | [{layout: Annot(OpenChild(_), m), _}] =>
               let highlighted_rs =
                 multiline_open_child_rects(~overflow_left, line_start, m);
               let line_end =
                 MeasuredLayout.next_position(~indent=0, line_start, m);
               (line_end, (highlighted_rs, []));
             | _ => line_rects(~overflow_left, line_start, line)
             };
           ({row: line_end.row + 1, col: 0}, rss);
         },
         start,
       )
    |> (
      fun
      | (end_, zipped) => {
          let (highlighted_rs, closed_child_rss) = List.split(zipped);
          (
            {...end_, row: end_.row - 1},
            (List.flatten(highlighted_rs), List.flatten(closed_child_rss)),
          );
        }
    );
  };

  let subblock_rects =
      (~offset: int, subject: MeasuredLayout.t): (rects, list(rects)) => {
    let flattened = MeasuredLayout.flatten(subject);
    switch (flattened) {
    | [
        [{layout: Annot(Step(_), {layout: Annot(EmptyLine, _), _}), _}],
        ..._,
      ] =>
      // TODO undo hack
      ([], [])
    | _ =>
      flattened
      |> ListUtil.map_with_accumulator(
           (line_start, line: list(MeasuredLayout.t)) => {
             let (line_end, rss) =
               switch (line) {
               | [{layout: Annot(Step(_), m), _}] =>
                 lines_rects(
                   ~overflow_left=true,
                   line_start,
                   MeasuredLayout.peel_and_flatten(m),
                 )
               | [{layout: Annot(OpenChild(_), m), _}] =>
                 let highlighted_rs =
                   multiline_open_child_rects(
                     ~overflow_left=true,
                     line_start,
                     m,
                   );
                 let line_end =
                   MeasuredLayout.next_position(~indent=0, line_start, m);
                 (line_end, (highlighted_rs, []));
               | _ =>
                 failwith(
                   "Doc nodes annotated as SubBlock should only contain *Line and OpenChild (flat) children",
                 )
               };
             ({row: line_end.row + 1, col: 0}, rss);
           },
           {row: 0, col: offset},
         )
      |> (
        fun
        | (_, zipped) => {
            let (highlighted_rs, closed_child_rss) = List.split(zipped);
            (List.flatten(highlighted_rs), List.flatten(closed_child_rss));
          }
      )
    };
  };

  let current_term_view =
      (
        ~corner_radii: (float, float),
        ~offset: int,
        ~shape: TermShape.t,
        subject: MeasuredLayout.t,
      )
      : Vdom.Node.t => {
    let (highlighted_rs, closed_child_rss) =
      switch (shape) {
      | SubBlock(_) =>
        // special case for now
        subblock_rects(~offset, subject)
      | Rule
      | Var(_)
      | Operand(_)
      | Invalid
      | FreeLivelit
      | ApLivelit =>
        snd(
          lines_rects(
            ~overflow_left=false,
            {row: 0, col: offset},
            MeasuredLayout.flatten(subject),
          ),
        )
      | Case(_)
      | BinOp(_)
      | NTuple(_) =>
        snd(
          lines_rects(
            ~overflow_left=true,
            {row: 0, col: offset},
            MeasuredLayout.flatten(subject),
          ),
        )
      };
    let highlighted_vs =
      ListUtil.is_empty(highlighted_rs)
        ? []
        : [
          RectilinearPolygon.mk_svg(
            ~corner_radii,
            ~attrs=Vdom.[Attr.classes(["code-current-term"])],
            highlighted_rs,
          ),
        ];
    let closed_child_vs =
      closed_child_rss
      |> List.filter_map(
           fun
           | [] => None
           | rs =>
             Some(
               RectilinearPolygon.mk_svg(
                 ~corner_radii,
                 ~attrs=[Vdom.Attr.classes(["code-closed-child"])],
                 rs,
               ),
             ),
         );
    Vdom.(Node.create_svg("g", [], highlighted_vs @ closed_child_vs));
  };

  let caret_view =
      (~font_metrics: FontMetrics.t, {row, col}: CaretPosition.t)
      : Vdom.Node.t => {
    Vdom.(
      Node.span(
        [
          Attr.id("caret"),
          Attr.create(
            "style",
            Printf.sprintf(
              // TODO make more robust
              "top: %fpx; left: calc(%fpx - 1px);",
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

  let livelit_expression_view = (subject: MeasuredLayout.t): Vdom.Node.t => {
    Vdom.(
      Node.create_svg(
        "rect",
        [
          Attr.create("width", string_of_int(MeasuredLayout.width(subject))),
          Attr.create(
            "height",
            string_of_int(MeasuredLayout.height(subject)),
          ),
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
        ~origin: CaretPosition.t,
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
      | CurrentTerm(shape) =>
        current_term_view(~corner_radii, ~offset, ~shape, subject)
      | LivelitExpression => livelit_expression_view(subject)
      };

    let cls =
      switch (d) {
      | ErrHole => "err-hole"
      | VarErrHole => "var-err-hole"
      | CurrentTerm(_) => "current-term"
      | LivelitExpression => "livelit-exp"
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
              "top: %fpx; left: %fpx;",
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
};

/*
 let sort_clss: TermSort.t => list(cls) =
   fun
   | Typ => ["Typ"]
   | Pat => ["Pat"]
   | Exp => ["Exp"];
 */

// need to use mousedown instead of click to fire
// (and move caret) before cell focus event handler
let on_mousedown =
    (
      ~inject,
      ~id: string,
      ~font_metrics: FontMetrics.t,
      ~splice: Program.current_splice,
      evt,
    ) => {
  open Vdom;
  let container_rect = JSUtil.force_get_elem_by_id(id)##getBoundingClientRect;
  let (target_x, target_y) = (
    float_of_int(evt##.clientX),
    float_of_int(evt##.clientY),
  );
  let row_col =
    CaretPosition.{
      row:
        Float.to_int(
          (target_y -. container_rect##.top) /. font_metrics.row_height,
        ),
      col:
        Float.to_int(
          Float.round(
            (target_x -. container_rect##.left) /. font_metrics.col_width,
          ),
        ),
    };
  Event.Many([
    inject(ModelAction.MoveAction(Click(splice, row_col))),
    Event.Stop_propagation,
  ]);
};

let decoration_views =
    (
      ~inject,
      ~font_metrics: FontMetrics.t,
      ~is_focused: bool,
      ~current_splice: Program.current_splice,
      ~caret_pos: CaretPosition.t,
      ~selected_instances,
      ~llii,
      ds: Decorations.t,
      (m, splice_ms): MeasuredLayout.with_splices,
    )
    : list(Vdom.Node.t) => {
  let rec go =
          (
            ~indent: int=0,
            ~start: CaretPosition.t={row: 0, col: 0},
            ds: Decorations.t,
            m: MeasuredLayout.t,
          )
          : list(Vdom.Node.t) => {
    let go' = go(~indent, ~start);
    switch (m.layout) {
    | Linebreak
    | Text(_) => []
    | Cat(m1, m2) =>
      let mid_row = {
        let height1 =
          m1.metrics
          |> List.map((box: MeasuredLayout.box) => box.height)
          |> List.fold_left((+), 0);
        start.row + height1 - 1;
      };
      let mid_col = {
        let (leading, MeasuredLayout.{width: last_width, _}) =
          ListUtil.split_last(m1.metrics);
        let offset =
          switch (leading) {
          | [] => start.col
          | [_, ..._] => indent
          };
        offset + last_width;
      };
      let ds1 = go'(ds, m1);
      let ds2 = go(~indent, ~start={row: mid_row, col: mid_col}, ds, m2);
      ds1 @ ds2;
    | Align(m) => go(~indent=start.col, ~start, ds, m)
    | Annot(annot, m) =>
      switch (annot) {
      | Step(step) =>
        let stepped = Decorations.take_step(step, ds);
        Decorations.is_empty(stepped) ? [] : go'(stepped, m);
      | Term({shape, _}) =>
        let current_ds =
          Decorations.current(shape, ds)
          |> List.map(
               Dec.view(
                 ~font_metrics,
                 ~origin={row: start.row, col: indent},
                 ~offset=start.col - indent,
                 // ~shape,
                 ~subject=m,
               ),
             );
        current_ds @ go'(ds, m);
      | LivelitView({llu, base_llname, shape, model: m, _}) =>
        // TODO(livelit definitions): thread ctx
        let ctx = Livelits.initial_livelit_view_ctx;
        let (llview, _) =
          VarMap.lookup(ctx, base_llname)
          |> OptUtil.get(() => failwith("undefined livelit " ++ base_llname));

        let trigger = serialized_action =>
          inject(ModelAction.LivelitAction(llu, serialized_action));
        let livelit_view = llview(m, trigger);
        let vs = {
          let uhcode = splice_name => {
            let splice_m =
              splice_ms |> SpliceMap.get_splice(llu, splice_name);
            let id = Printf.sprintf("code-splice-%d-%d", llu, splice_name);
            let caret =
              switch (current_splice) {
              | Some((u, name))
                  when u == llu && name == splice_name && is_focused => [
                  Dec.caret_view(~font_metrics, caret_pos),
                ]
              | _ => []
              };
            let splice_ds = {
              let stepped = Decorations.take_step(splice_name, ds);
              Decorations.is_empty(stepped) ? [] : go(stepped, splice_m);
            };
            Vdom.(
              Node.div(
                [
                  Attr.id(id),
                  Attr.classes(["splice"]),
                  Attr.on_mousedown(
                    on_mousedown(
                      ~inject,
                      ~id,
                      ~font_metrics,
                      ~splice=Some((llu, splice_name)),
                    ),
                  ),
                ],
                caret @ splice_ds,
              )
            );
          };

          let selected_inst_opt =
            selected_instances
            |> UserSelectedInstances.find_opt(
                 TaggedNodeInstance.Livelit,
                 llu,
               )
            |> OptUtil.map(i => (llu, i));
          let inst_opt =
            switch (selected_inst_opt) {
            | None => LivelitInstanceInfo.default_instance(llii, llu)
            | Some(inst) => Some(inst)
            };
          let sim_dargs_opt =
            inst_opt
            |> OptUtil.and_then(LivelitInstanceInfo.lookup(llii))
            |> OptUtil.map(((_, _, (si, dargs))) =>
                 (SpliceInfo.splice_map(si), dargs)
               );

          let dhview =
            DHCode.view(
              ~inject,
              // TODO undo hardcoded width
              ~width=80,
            );

          let dhcode = splice_name =>
            sim_dargs_opt
            |> OptUtil.and_then(((splice_map, _)) =>
                 switch (IntMap.find_opt(splice_name, splice_map)) {
                 | None => raise(Not_found)
                 | Some((_, d_opt)) =>
                   d_opt |> OptUtil.map(d => (d, dhview(d)))
                 }
               );

          let dargs =
            sim_dargs_opt
            |> OptUtil.map(((_, dargs)) =>
                 dargs
                 |> List.map(((v, darg_opt)) =>
                      (
                        v,
                        darg_opt |> Option.map(darg => (darg, dhview(darg))),
                      )
                    )
               );

          [livelit_view({uhcode, dhcode, dargs})];
        };
        let dim_attr =
          switch (shape) {
          | Inline(width) =>
            Vdom.Attr.create(
              "style",
              Printf.sprintf("min-width: %dch;", width),
            )
          | MultiLine(height) =>
            Vdom.Attr.create(
              "style",
              Printf.sprintf(
                "min-height: %fpx;",
                float_of_int(height) *. font_metrics.row_height,
              ),
            )
          };
        Vdom.[
          Node.div(
            [
              Attr.classes([
                "LivelitView",
                switch (shape) {
                | Inline(_) => "Inline"
                | MultiLine(_) => "MultiLine"
                },
              ]),
              dim_attr,
              Attr.on_mousedown(_ => Event.Stop_propagation),
            ],
            vs,
          ),
        ];

      | _ => go'(ds, m)
      }
    };
  };

  let root_caret =
    switch (current_splice) {
    | Some(_) => []
    | None => [Dec.caret_view(~font_metrics, caret_pos)]
    };
  let root_ds = go(ds, m);
  root_caret @ root_ds;
};

let key_handlers =
    (~inject, ~is_mac: bool, ~cursor_info: CursorInfo_common.t)
    : list(Vdom.Attr.t) => {
  open Vdom;
  let prevent_stop_inject = a =>
    Event.Many([Event.Prevent_default, Event.Stop_propagation, inject(a)]);
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keydown(evt => {
      switch (MoveKey.of_key(Key.get_key(evt))) {
      | Some(move_key) =>
        prevent_stop_inject(ModelAction.MoveAction(Key(move_key)))
      | None =>
        switch (HazelKeyCombos.of_evt(evt)) {
        | Some(Ctrl_Z) =>
          if (is_mac) {
            Event.Ignore;
          } else {
            prevent_stop_inject(ModelAction.Undo);
          }
        | Some(Meta_Z) =>
          if (is_mac) {
            prevent_stop_inject(ModelAction.Undo);
          } else {
            Event.Ignore;
          }
        | Some(Ctrl_Shift_Z) =>
          if (is_mac) {
            Event.Ignore;
          } else {
            prevent_stop_inject(ModelAction.Redo);
          }
        | Some(Meta_Shift_Z) =>
          if (is_mac) {
            prevent_stop_inject(ModelAction.Redo);
          } else {
            Event.Ignore;
          }
        | Some(kc) =>
          prevent_stop_inject(
            ModelAction.EditAction(KeyComboAction.get(cursor_info, kc)),
          )
        | None =>
          switch (HazelKeyCombos.of_evt(evt)) {
          | Some(Ctrl_Z) => prevent_stop_inject(ModelAction.Undo)
          | Some(Ctrl_Shift_Z) => prevent_stop_inject(ModelAction.Redo)
          | Some(kc) =>
            prevent_stop_inject(
              ModelAction.EditAction(KeyComboAction.get(cursor_info, kc)),
            )
          | None =>
            switch (JSUtil.is_single_key(evt)) {
            | None => Event.Ignore
            | Some(single_key) =>
              prevent_stop_inject(
                ModelAction.EditAction(
                  Construct(SChar(JSUtil.single_key_string(single_key))),
                ),
              )
            }
          }
        }
      }
    }),
  ];
};

let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      ~font_metrics: FontMetrics.t,
      ~measure: bool,
      ~is_mac: bool,
      ~llii: LivelitInstanceInfo.t,
      ~selected_instances: UserSelectedInstances.t,
      program: Program.t,
    )
    : Vdom.Node.t =>
  TimeUtil.measure_time(
    "UHCode.view",
    measure,
    () => {
      open Vdom;

      let rec go = (box: UHBox.t): list(Vdom.Node.t) => {
        switch (box) {
        | Text(s) => StringUtil.is_empty(s) ? [] : [Node.text(s)]
        | HBox(boxes) => boxes |> List.map(go) |> List.flatten
        | VBox(boxes) =>
          let vs =
            boxes
            |> List.map(go)
            |> ListUtil.join([Node.br([])])
            |> List.flatten;
          [Node.div([Attr.classes(["VBox"])], vs)];
        | Annot(annot, box) =>
          let vs = go(box);
          switch (annot) {
          | Token({shape, _}) =>
            let clss =
              switch (shape) {
              | Text(_) => ["code-text"]
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
          | UserNewline => [Node.span([Attr.classes(["UserNewline"])], vs)]
          | _ => vs
          };
        };
      };

      let (l, _) =
        Program.get_layout(
          ~measure_program_get_doc=false,
          ~measure_layoutOfDoc_layout_of_doc=false,
          ~memoize_doc=true,
          program,
        );
      let code_text = go(Box.mk(l));

      let (caret_pos, current_splice) =
        Program.get_caret_position(
          ~measure_program_get_doc=false,
          ~measure_layoutOfDoc_layout_of_doc=false,
          ~memoize_doc=true,
          program,
        );

      let ds = Program.get_decorations(program);
      let decorations =
        decoration_views(
          ~inject,
          ~font_metrics,
          ~is_focused=program.is_focused,
          ~current_splice,
          ~caret_pos,
          ~selected_instances,
          ~llii,
          ds,
          Program.get_measured_layout(
            ~measure_program_get_doc=false,
            ~measure_layoutOfDoc_layout_of_doc=false,
            ~memoize_doc=true,
            program,
          ),
        );

      let key_handlers =
        program.is_focused
          ? key_handlers(
              ~inject,
              ~is_mac,
              ~cursor_info=Program.get_cursor_info(program),
            )
          : [];

      let id = "code-root";
      Node.div(
        [
          Attr.id(id),
          Attr.classes(["code", "presentation"]),
          Attr.on_mousedown(
            on_mousedown(~inject, ~id, ~font_metrics, ~splice=None),
          ),
          // necessary to make cell focusable
          Attr.create("tabindex", "0"),
          Attr.on_focus(_ => inject(ModelAction.FocusCell)),
          Attr.on_blur(_ => inject(ModelAction.BlurCell)),
          ...key_handlers,
        ],
        [Node.span([Attr.classes(["code"])], code_text), ...decorations],
      );
    },
  );
