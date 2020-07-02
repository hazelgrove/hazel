module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open ViewUtil;

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

let shape_clss: UHAnnot.term_shape => list(cls) =
  fun
  | Rule => ["Rule"]
  | Case({err}) => ["Case", ...clss_of_case_err(err)]
  | Var({err, verr, show_use}) =>
    ["Operand", "Var", ...clss_of_err(err)]
    @ clss_of_verr(verr)
    @ (show_use ? ["show-use"] : [])
  | Operand({err}) => ["Operand", ...clss_of_err(err)]
  | FreeLivelit => ["FreeLivelit"]
  | ApLivelit => ["ApLivelit"]
  | LivelitExpression => ["LivelitExpression"]
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

let caret_from_pos = (~font_metrics: FontMetrics.t, (row, col)): Vdom.Node.t => {
  let x = float_of_int(col) *. font_metrics.col_width;
  let y = float_of_int(row) *. font_metrics.row_height;
  let pos_attr =
    Vdom.Attr.style(
      Css_gen.combine(
        Css_gen.left(`Px(int_of_float(x))),
        Css_gen.top(`Px(int_of_float(y))),
      ),
    );
  Vdom.Node.span(
    [Vdom.Attr.id("caret"), pos_attr, Vdom.Attr.classes(["blink"])],
    [],
  );
};

let view =
    (
      ~inject,
      ~model: Model.t,
      ~font_metrics: FontMetrics.t,
      ~z: option(CursorMap.z),
      ~llii: LivelitInstanceInfo.t,
      ~selected_instances: UserSelectedInstances.t,
      (l, splice_ls): UHLayout.with_splices,
    )
    : Vdom.Node.t =>
  TimeUtil.measure_time(
    "UHCode.view",
    model.measurements.measurements && model.measurements.uhcode_view,
    () => {
      open Vdom;

      let on_mousedown =
          (~id: string, ~splice: option((MetaVar.t, SpliceName.t)), evt) => {
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
        Event.Many([
          inject(Update.Action.MoveAction(Click(splice, row_col))),
          Event.Stop_propagation,
        ]);
      };

      let rec go: UHLayout.t => _ =
        fun
        | Text(s) => StringUtil.is_empty(s) ? [] : [Node.text(s)]
        | Linebreak => [Node.br([])]
        | Align(l) => [Node.div([Attr.classes(["Align"])], go(l))]
        | Cat(l1, l2) => go(l1) @ go(l2)

        | Annot(Step(_) | EmptyLine | SpaceOp, l) => go(l)

        | Annot(Token({shape, _}), l) => {
            let clss =
              switch (shape) {
              | Text => ["code-text"]
              | Op => ["code-op"]
              | Delim(_) => ["code-delim"]
              };
            [Node.span([Attr.classes(clss)], go(l))];
          }
        | Annot(DelimGroup, l) => [
            Node.span([Attr.classes(["DelimGroup"])], go(l)),
          ]
        | Annot(LetLine, l) => [
            Node.span([Attr.classes(["LetLine"])], go(l)),
          ]
        | Annot(AbbrevLine, l) => [
            Node.span([Attr.classes(["LetLine"])], go(l)),
          ]

        | Annot(Padding, l) => [
            Node.span([Attr.classes(["Padding"])], go(l)),
          ]
        | Annot(Indent, l) => [
            Node.span([Attr.classes(["Indent"])], go(l)),
          ]

        | Annot(HoleLabel({len}), l) => {
            let font_width = font_metrics.col_width;
            let font_shrink = 0.65;
            let full_space = font_width *. float_of_int(len);
            let shrunk_space = full_space *. font_shrink;
            let per_side_padding = (full_space -. shrunk_space) /. 2.0;
            let padding =
              Css_gen.padding(
                ~left=`Px(int_of_float(per_side_padding)),
                ~right=`Px(int_of_float(per_side_padding)),
                (),
              );
            let font_size =
              Css_gen.font_size(
                `Percent(
                  Core_kernel.Percent.of_percentage(font_shrink *. 100.0),
                ),
              );
            let styling =
              Vdom.Attr.style(Css_gen.combine(padding, font_size));
            [Node.span([styling, Attr.classes(["HoleLabel"])], go(l))];
          }
        | Annot(UserNewline, l) => [
            Node.span([Attr.classes(["UserNewline"])], go(l)),
          ]

        | Annot(OpenChild({is_inline}), l) => [
            Node.span(
              [Attr.classes(["OpenChild", is_inline ? "Inline" : "Para"])],
              go(l),
            ),
          ]
        | Annot(ClosedChild({is_inline}), l) => [
            Node.span(
              [Attr.classes(["ClosedChild", is_inline ? "Inline" : "Para"])],
              go(l),
            ),
          ]

        | Annot(LivelitView({llu, llname, shape, model: m, _}), _) => {
            // TODO(livelit definitions): thread ctx
            let ctx = Livelits.initial_livelit_view_ctx;
            let (llview, _) =
              VarMap.lookup(ctx, llname)
              |> OptUtil.get(() => failwith("undefined livelit " ++ llname));

            let trigger = serialized_action =>
              inject(Update.Action.LivelitAction(llu, serialized_action));
            let livelit_view = llview(m, trigger);
            let vs = {
              let uhcode = splice_name => {
                let splice_layout =
                  splice_ls |> SpliceMap.get_splice(llu, splice_name);
                let id =
                  Printf.sprintf("code-splice-%d-%d", llu, splice_name);
                let children =
                  switch (z) {
                  | Some((Some((u, name)), ((row, col), _)))
                      when u == llu && name == splice_name =>
                    let caret = caret_from_pos(~font_metrics, (row, col));
                    [caret, ...go(splice_layout)];
                  | _ => go(splice_layout)
                  };
                Node.div(
                  [
                    Attr.id(id),
                    Attr.classes(["splice"]),
                    Attr.on_mousedown(
                      on_mousedown(~id, ~splice=Some((llu, splice_name))),
                    ),
                  ],
                  children,
                );
              };

              let selected_inst_opt =
                selected_instances
                |> UserSelectedInstances.lookup(
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
                     switch (NatMap.lookup(splice_map, splice_name)) {
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
                            darg_opt
                            |> OptUtil.map(darg => (darg, dhview(darg))),
                          )
                        )
                   );

              [livelit_view({uhcode, dhcode, dargs})];
            };
            let dim_attr =
              switch (shape) {
              | Inline(width) =>
                Attr.create(
                  "style",
                  Printf.sprintf("min-width: %dch;", width),
                )
              | MultiLine(height) =>
                Attr.create(
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
          }

        | Annot(Term({has_cursor, shape, sort}), l) => [
            Node.span(
              [
                Attr.classes(
                  List.concat([
                    ["Term"],
                    cursor_clss(has_cursor),
                    sort_clss(sort),
                    shape_clss(shape),
                    open_child_clss(
                      l |> UHLayout.has_inline_OpenChild,
                      l |> UHLayout.has_para_OpenChild,
                    ),
                    has_child_clss(l |> UHLayout.has_child),
                  ]),
                ),
              ],
              go(l),
            ),
          ];

      let children =
        switch (z) {
        | Some((None, ((row, col), _))) =>
          let caret = caret_from_pos(~font_metrics, (row, col));
          [caret, ...go(l)];
        | _ => go(l)
        };
      let id = "code-root";
      Node.div(
        [
          Attr.id(id),
          Attr.classes(["code", "presentation"]),
          Attr.on_mousedown(on_mousedown(~id, ~splice=None)),
        ],
        children,
      );
    },
  );
