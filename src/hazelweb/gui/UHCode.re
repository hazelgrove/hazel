module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;

module MeasuredPosition = Pretty.MeasuredPosition;
module MeasuredLayout = Pretty.MeasuredLayout;

exception NotElem;

[@warning "-32"]
let decode_livelit_view = (splices: int => Vdom.Node.t, d: DHExp.t) => {
  let rec decode_attrs: DHExp.t => list(Vdom.Attr.t) =
    d =>
      switch (d) {
      | ListNil(_) => []
      | Cons(Pair(StringLit(k), StringLit(v)), tl) => [
          Vdom.Attr.create(k, v),
          ...decode_attrs(tl),
        ]
      | _ => raise(NotElem)
      };
  let rec decode_elem = (d: DHExp.t) => {
    print_endline("decode_elem");
    print_endline(Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d)));
    switch (d) {
    | Pair(
        StringLit("editor"),
        Pair(Cons(Pair(StringLit("id"), IntLit(id)), _), _),
      ) =>
      splices(id)
    | Pair(StringLit(tag), Pair(d_attrs, d_children)) =>
      let attrs = decode_attrs(d_attrs);
      let children = decode_children(d_children);
      Vdom.Node.create(tag, attrs, children);
    | _ => raise(NotElem)
    };
  }
  and decode_children = (d: DHExp.t) => {
    switch (d) {
    | ListNil(_) => []
    | Cons(child, tl) => [decode_elem(child), ...decode_children(tl)]
    | _ => raise(NotElem)
    };
  };
  switch (decode_elem(DHExp.strip_casts'(d))) {
  | elem => Some(elem)
  | exception NotElem => None
  };
};

let widget_id_tbl = Hashtbl.create(5);

let _get_widget_id = llu =>
  switch (Hashtbl.find_opt(widget_id_tbl, llu)) {
  | Some(id) => id
  | None =>
    let id =
      Base__Type_equal.Id.create(
        ~name="livelit-state-" ++ string_of_int(llu), _ =>
        Sexplib.Sexp.List([])
      );
    Hashtbl.add(widget_id_tbl, llu, id);
    id;
  };

let rec _mk_view_monad = (splices, dhcode, d: DHExp.t): option(Vdom.Node.t) => {
  let run = _mk_view_monad(splices, dhcode);
  // two cases:
  // inj[L](d) : return(d) where d is pseudovdom
  // inj[R]    : bindEvalSplice(spliceno, f)
  print_endline("_mk_view_monad");
  print_endline(Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d)));
  switch (d) {
  | Inj(_, L, d0) => decode_livelit_view(splices, d0)
  | Inj(_, R, Pair(IntLit(spliceno), f)) =>
    let spliceval: string =
      spliceno
      |> dhcode
      |> (((d, _)) => d)
      |> DHExp.sexp_of_t
      |> Sexplib.Sexp.to_string;
    DHExp.Ap(f, StringLit(spliceval)) |> Statics_Exp.eval |> run;
  | _ => failwith("mk_view_monad unhandled")
  };
};

/**
 * A buffered container for SVG elements so that strokes along
 * the bounding box of the elements do not get clipped by the
 * viewBox boundaries
 */
let decoration_container =
    (
      ~font_metrics: FontMetrics.t,
      ~origin: MeasuredPosition.t,
      ~height: int,
      ~width: int,
      ~cls: string,
      svgs: list(Vdom.Node.t),
    )
    : Vdom.Node.t => {
  let buffered_height = height + 1;
  let buffered_width = width + 1;

  let buffered_height_px =
    Float.of_int(buffered_height) *. font_metrics.row_height;
  let buffered_width_px =
    Float.of_int(buffered_width) *. font_metrics.col_width;

  let container_origin_x =
    (Float.of_int(origin.row) -. 0.5) *. font_metrics.row_height;
  let container_origin_y =
    (Float.of_int(origin.col) -. 0.5) *. font_metrics.col_width;

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
            container_origin_x,
            container_origin_y,
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
              Printf.sprintf(
                "-0.5 -0.5 %d %d",
                buffered_width,
                buffered_height,
              ),
            ),
            Attr.create("width", Printf.sprintf("%fpx", buffered_width_px)),
            Attr.create(
              "height",
              Printf.sprintf("%fpx", buffered_height_px),
            ),
            Attr.create("preserveAspectRatio", "none"),
          ],
          svgs,
        ),
      ],
    )
  );
};

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
    MeasuredPosition.{
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

let box_table: WeakMap.t(UHBox.t, list(Vdom.Node.t)) = WeakMap.mk();
let rec view_of_box = (box: UHBox.t): list(Vdom.Node.t) => {
  Vdom.(
    switch (WeakMap.get(box_table, box)) {
    | Some(vs) => vs
    | None =>
      switch (box) {
      | Text(s) => StringUtil.is_empty(s) ? [] : [Node.text(s)]
      | HBox(boxes) => boxes |> List.map(view_of_box) |> List.flatten
      | VBox(boxes) =>
        let vs =
          boxes
          |> List.map(view_of_box)
          |> ListUtil.join([Node.br([])])
          |> List.flatten;
        [Node.div([Attr.classes(["VBox"])], vs)];
      | Annot(annot, box) =>
        let vs = view_of_box(box);
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
              [Attr.style(width), Attr.classes(["HoleLabel"])],
              [Node.span([Attr.classes(["HoleNumber"])], vs)],
            ),
          ];
        | UserNewline => [Node.span([Attr.classes(["UserNewline"])], vs)]
        | CommentLine => [Node.span([Attr.classes(["CommentLine"])], vs)]
        | ValidSeq => [Node.span([Attr.classes(["ValidSeq"])], vs)]
        | InvalidSeq => [Node.span([Attr.classes(["InvalidSeq"])], vs)]
        | String => [Node.span([Attr.classes(["String"])], vs)]
        | _ => vs
        };
      }
    }
  );
};

let decoration_cls: UHDecorationShape.t => string =
  fun
  | ErrHole => "err-hole"
  | VarErrHole => "var-err-hole"
  | VarUse => "var-use"
  | CurrentTerm => "current-term"
  | LivelitExpression(_) => "livelit-expression";

let decoration_view =
    (
      ~contains_current_term: bool,
      ~corner_radii: (float, float),
      ~term_sort: TermSort.t,
      ~term_shape: TermShape.t,
      shape: UHDecorationShape.t,
    ) =>
  UHDecoration.(
    switch (shape) {
    | ErrHole => ErrHole.view(~contains_current_term, ~corner_radii)
    | VarErrHole => VarErrHole.view(~contains_current_term, ~corner_radii)
    | VarUse => VarUse.view(~corner_radii)
    | CurrentTerm =>
      CurrentTerm.view(~corner_radii, ~sort=term_sort, ~shape=term_shape)
    | LivelitExpression(view_shape) => LivelitExpression.view(view_shape)
    }
  );

let decoration_views =
    (
      ~inject,
      ~font_metrics: FontMetrics.t,
      ~settings: Settings.t,
      ~caret_pos:
         option((Pretty.MeasuredPosition.t, Program.current_splice)),
      ~llii,
      ~selected_instances,
      ~sync_livelit,
      ~llview_ctx: Statics.livelit_web_view_ctx,
      dpaths: UHDecorationPaths.t,
      (l, splice_ls): UHLayout.with_splices,
    )
    : list(Vdom.Node.t) => {
  let corner_radius = 2.5;
  let corner_radii = (
    corner_radius /. font_metrics.col_width,
    corner_radius /. font_metrics.row_height,
  );

  let livelit_error_view =
    Vdom.(
      Node.div(
        [Attr.classes(["user-defined-livelit-container-error"])],
        [Node.text("Livelit View Error")],
      )
    );

  let create_ll_view = (start: MeasuredPosition.t, shape: LivelitShape.t, vs) => {
    let top = Float.of_int(start.row) *. font_metrics.row_height;
    let left = Float.of_int(start.col) *. font_metrics.col_width;
    let dim_attr =
      switch (shape) {
      | InvalidShape =>
        Vdom.Attr.create(
          "style",
          Printf.sprintf(
            "width: %dch; max-height: %fpx; top: %fpx; left: %fpx;",
            13,
            font_metrics.row_height,
            top,
            left,
          ),
        )
      | Inline(width) =>
        Vdom.Attr.create(
          "style",
          Printf.sprintf(
            "width: %dch; max-height: %fpx; top: %fpx; left: %fpx;",
            width,
            font_metrics.row_height,
            top,
            left,
          ),
        )
      | MultiLine(height) =>
        Vdom.Attr.create(
          "style",
          Printf.sprintf(
            "height: %fpx; top: %fpx; left: %fpx;",
            float_of_int(height) *. font_metrics.row_height,
            top,
            left,
          ),
        )
      };
    Vdom.[
      Node.div(
        [
          Attr.classes([
            "LivelitView",
            switch (shape) {
            | InvalidShape => "InvalidShape"
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
  };
  let rec go =
          (
            ~tl: list(Vdom.Node.t)=[], // tail-recursive
            ~indent=0, // indentation level of `m`
            ~start=MeasuredPosition.zero, // start position of `m`
            dpaths: UHDecorationPaths.t, // paths to decorations within `m`
            m: UHMeasuredLayout.t,
          )
          : list(Vdom.Node.t) => {
    let go' = go(~indent, ~start);
    switch (m.layout) {
    | Linebreak
    | Text(_) => tl
    | Cat(m1, m2) =>
      let mid_row = start.row + MeasuredLayout.height(m1) - 1;
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
      let mid_tl =
        go(~tl, ~indent, ~start={row: mid_row, col: mid_col}, dpaths, m2);
      go'(~tl=mid_tl, dpaths, m1);
    | Align(m) => go(~tl, ~indent=start.col, ~start, dpaths, m)
    | Annot(annot, m) =>
      switch (annot) {
      | Step(step) =>
        let stepped = UHDecorationPaths.take_step(step, dpaths);
        UHDecorationPaths.is_empty(stepped) ? tl : go'(~tl, stepped, m);
      | Term({shape, sort, _}) =>
        let offset = start.col - indent;
        let current_vs =
          UHDecorationPaths.current(shape, dpaths)
          |> List.map((dshape: UHDecorationShape.t) => {
               let cls = decoration_cls(dshape);
               let view =
                 decoration_view(
                   ~contains_current_term=Option.is_some(dpaths.current_term),
                   ~corner_radii,
                   ~term_shape=shape,
                   ~term_sort=sort,
                   dshape,
                   (offset, m),
                 );
               decoration_container(
                 ~font_metrics,
                 ~height=MeasuredLayout.height(m),
                 ~width=MeasuredLayout.width(~offset, m),
                 ~origin=MeasuredPosition.{row: start.row, col: indent},
                 ~cls,
                 [view],
               );
             });
        go'(~tl=current_vs @ tl, dpaths, m);
      | LivelitView({llu, base_llname, shape, model, hd_step, _}) =>
        let uhcode = splice_name => {
          let splice_l = SpliceMap.get_splice(llu, splice_name, splice_ls);
          let id = Printf.sprintf("code-splice-%d-%d", llu, splice_name);
          let caret =
            switch (caret_pos) {
            | Some((caret_pos, Some((u, name))))
                when u == llu && name == splice_name => [
                UHDecoration.Caret.view(~font_metrics, caret_pos),
              ]
            | _ => []
            };
          let splice_ds = {
            let stepped =
              dpaths
              |> UHDecorationPaths.take_step(hd_step)
              |> UHDecorationPaths.take_step(splice_name);
            UHDecorationPaths.is_empty(stepped)
              ? [] : go(stepped, UHMeasuredLayout.mk(splice_l));
          };
          let splice_code = view_of_box(UHBox.mk(splice_l));
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
              caret
              @ splice_ds
              @ [Node.span([Attr.classes(["code"])], splice_code)],
            )
          );
        };
        let current_vs =
          switch (IntMap.find_opt(llu, llview_ctx)) {
          | Some((llview, _)) =>
            switch (
              llview(llu, model)
              |> Option.map(DHExp.strip_casts')
              |> Option.map(decode_livelit_view(uhcode))
            ) {
            | Some(Some(view_vdom)) =>
              let vs = [view_vdom];
              create_ll_view(start, shape, vs);
            | _ => [livelit_error_view]
            }
          | None =>
            // TODO(livelit definitions): thread ctx
            let ctx = Livelits.initial_livelit_view_ctx;
            let (llview: Livelits.serialized_view_fn_t, _) =
              VarMap.lookup(ctx, base_llname)
              |> OptUtil.get(() =>
                   failwith("undefined livelit " ++ base_llname)
                 );

            let trigger = serialized_action => {
              inject(ModelAction.LivelitAction(llu, serialized_action));
            };

            let sync = serialized_action =>
              sync_livelit(
                ModelAction.LivelitAction(llu, serialized_action),
              );
            let livelit_view = llview(model, trigger, sync);
            let vs = {
              let selected_inst_opt =
                selected_instances
                |> UserSelectedInstances.find_opt(
                     TaggedNodeInstance.Livelit,
                     llu,
                   )
                |> Option.map(i => (llu, i));
              let inst_opt =
                switch (selected_inst_opt) {
                | None => LivelitInstanceInfo.default_instance(llii, llu)
                | Some(inst) => Some(inst)
                };
              let sim_dargs_opt =
                inst_opt
                |> OptUtil.and_then(LivelitInstanceInfo.lookup(llii))
                |> Option.map(((_, _, (si, dargs))) =>
                     (SpliceInfo.splice_map(si), dargs)
                   );

              let dhview =
                DHCode.view(
                  ~inject,
                  // TODO undo hardcoded width
                  ~width=80,
                  ~settings=settings.evaluation,
                );

              let dhcode = splice_name => {
                open OptUtil.Syntax;
                let* (splice_map, _) = sim_dargs_opt;
                let* (_, d_opt) = IntMap.find_opt(splice_name, splice_map);
                let+ d = d_opt;
                (d, dhview(d));
              };

              let dargs =
                sim_dargs_opt
                |> Option.map(((_, dargs)) =>
                     dargs
                     |> List.map(((v, darg_opt)) =>
                          (
                            v,
                            darg_opt
                            |> Option.map(darg => (darg, dhview(darg))),
                          )
                        )
                   );

              [livelit_view({uhcode, dhcode, dargs})];
            };
            create_ll_view(start, shape, vs);
          };
        go'(~tl=current_vs @ tl, dpaths, m);
      | _ => go'(~tl, dpaths, m)
      }
    };
  };

  let root_caret =
    switch (caret_pos) {
    | Some((caret_pos, None)) => [
        UHDecoration.Caret.view(~font_metrics, caret_pos),
      ]
    | _ => []
    };
  let root_ds = go(dpaths, UHMeasuredLayout.mk(l));
  root_caret @ root_ds;
};

let key_handlers =
    (~inject, ~is_mac: bool, ~cursor_info: CursorInfo.t): list(Vdom.Attr.t) => {
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
        let s = Key.get_key(evt);
        switch (cursor_info.cursor_term) {
        | Exp(OnText(_), StringLit(_))
            when KeyCombo.matches(KeyCombo.enter, evt) =>
          prevent_stop_inject(
            ModelAction.EditAction(Construct(SChar("\n"))),
          )
        | Exp(OnText(_), StringLit(_))
            when
              String.length(s) == 1
              && ModKeys.matches(ModKeys.no_ctrl_alt_meta, evt) =>
          prevent_stop_inject(ModelAction.EditAction(Construct(SChar(s))))
        | _ =>
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
              ModelAction.EditAction(
                KeyComboAction.get(Some(cursor_info), kc),
              ),
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
        };
      }
    }),
  ];
};

let root_id = "code-root";

let focus = () => {
  JSUtil.force_get_elem_by_id(root_id)##focus;
};

let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      ~font_metrics: FontMetrics.t,
      ~is_mac: bool,
      ~selected_instances: UserSelectedInstances.t,
      ~sync_livelit,
      ~settings: Settings.t,
      ~llview_ctx: Statics.livelit_web_view_ctx,
      program: Program.t,
    )
    : Vdom.Node.t =>
  TimeUtil.measure_time(
    "UHCode.view",
    settings.performance.measure && settings.performance.uhcode_view,
    () => {
      open Vdom;
      let (_, _, llii, _) = Program.get_result(program);

      let (l, splice_ls) = Program.get_layout(~settings, program);

      let caret_pos = Program.get_caret_position(~settings, program);

      // TODO need to render code text for splices... I think
      let code_text = view_of_box(UHBox.mk(l));
      let decorations = {
        let dpaths = Program.get_decoration_paths(program);
        decoration_views(
          ~inject,
          ~settings,
          ~font_metrics,
          ~caret_pos,
          ~selected_instances,
          ~llii,
          ~sync_livelit,
          ~llview_ctx,
          dpaths,
          (l, splice_ls),
        );
      };

      let key_handlers =
        switch (Program.get_cursor_info(program)) {
        | None => []
        | Some(cursor_info) => key_handlers(~inject, ~is_mac, ~cursor_info)
        };

      Node.div(
        [
          Attr.id(root_id),
          Attr.classes(["code", "presentation"]),
          // need to use mousedown instead of click to fire
          // (and move caret) before cell focus event handler
          Attr.on_mousedown(
            on_mousedown(~inject, ~id=root_id, ~font_metrics, ~splice=None),
          ),
          // necessary to make cell focusable
          Attr.create("tabindex", "0"),
          Attr.on_focus(_ => inject(ModelAction.FocusCell)),
          Attr.on_blur(_ =>
            JSUtil.window_has_focus()
              ? inject(ModelAction.BlurCell) : Event.Many([])
          ),
          ...key_handlers,
        ],
        [Node.span([Attr.classes(["code"])], code_text), ...decorations],
      );
    },
  );
