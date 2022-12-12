module Vdom = Virtual_dom.Vdom;

exception InvalidInstance;
let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      ~selected_instance: option(HoleInstance.t),
      ~settings: Settings.Evaluation.t,
      ~font_metrics: FontMetrics.t,
      program: Program.t,
      res: ModelResult.t,
    )
    : Vdom.Node.t => {
  open Vdom /* TODO: Fade out when not current? */;

  let hii =
    switch (res.current) {
    | ResultOk(r) => r |> ProgramResult.get_hii
    | ResultFail(_)
    | ResultTimeout
    | ResultPending => res.previous |> ProgramResult.get_hii
    };

  /**
   * Shows typing info for a context entry.
   */
  let static_info = ((x, ty)) =>
    Node.div(
      [Attr.classes(["static-info"])],
      [
        Node.div(
          [Attr.classes(["code"])],
          [
            Node.span([Attr.classes(["var"])], [Node.text(x)]),
            Node.text(" : "),
            HTypCode.view(~width=30, ~pos=Var.length(x) + 3, ty),
          ],
        ),
      ],
    );

  /**
   * Shows runtime value for a context entry.
   */
  let dynamic_info = (sigma, x) =>
    switch (VarMap.lookup(sigma, x)) {
    | None =>
      Some(
        Node.div(
          [Attr.classes(["dynamic-info"])],
          [Node.div([], [Node.span([], [Node.text("NONE!!!!!!")])])],
        ),
      )
    | Some(DHExp.BoundVar(x')) when Var.eq(x, x') => None
    | Some(d) =>
      Some(
        Node.div(
          [Attr.classes(["dynamic-info"])],
          [
            Node.div(
              [],
              [
                DHCode.view(
                  ~inject,
                  ~settings,
                  ~selected_instance,
                  ~font_metrics,
                  ~width=30,
                  d,
                ),
              ],
            ),
          ],
        ),
      )
    };

  let context_entry = (sigma, (x, ty)) => {
    let static_info = static_info((x, ty));
    let children =
      switch (dynamic_info(sigma, x)) {
      | Some(dynamic_info) => [static_info, dynamic_info]
      | None => [static_info]
      };
    Node.div([Attr.classes(["context-entry"])], children);
  };

  let instructional_msg = msg =>
    Node.div([Attr.classes(["instructional-msg"])], [Node.text(msg)]);

  let path_view_titlebar =
    Panel.view_of_other_title_bar("Closure above observed at ");

  let hii_summary = (hii, (u, i) as inst) => {
    let num_instances = HoleInstanceInfo.num_instances(hii, u);
    let msg =
      Node.div(
        [Attr.classes(["instance-info"])],
        [
          Node.div(
            [],
            [
              Node.div(
                [Attr.classes(["hii-summary-inst"])],
                [
                  DHCode.view_of_hole_instance(
                    ~inject,
                    ~width=30,
                    ~selected_instance,
                    ~settings,
                    ~font_metrics,
                    inst,
                  ),
                ],
              ),
              Node.text(" = hole "),
              Node.span(
                [Attr.classes(["hole-name-normal-txt"])],
                [Node.text(string_of_int(u + 1))],
              ),
              Node.text(" instance "),
              Node.span(
                [Attr.classes(["inst-number-normal-txt"])],
                [Node.text(string_of_int(i + 1))],
              ),
              Node.text(" of "),
              Node.span(
                [Attr.classes(["inst-number-normal-txt"])],
                [Node.text(string_of_int(num_instances))],
              ),
            ],
          ),
        ],
      );

    let prev_key = KeyCombo.alt_PageUp;
    let next_key = KeyCombo.alt_PageDown;

    let prev_title = "Previous instance (" ++ KeyCombo.name(prev_key) ++ ")";
    let next_title = "Next instance (" ++ KeyCombo.name(next_key) ++ ")";

    let prev_btn =
      if (i > 0) {
        let prev_inst = (u, i - 1);
        Node.div(
          [
            Attr.create("title", prev_title),
            Attr.classes(["instance-button-wrapper"]),
            Attr.on_click(_ => inject(SelectHoleInstance(prev_inst))),
            Attr.on_keydown(ev => {
              let updates =
                KeyCombo.matches(prev_key, ev)
                  ? [inject(SelectHoleInstance(prev_inst))] : [];
              Event.Many([Event.Prevent_default, ...updates]);
            }),
          ],
          [Icons.left_arrow(["prev-instance", "has-prev", "noselect"])],
        );
      } else {
        Node.div(
          [
            Attr.create("title", prev_title),
            Attr.classes(["instance-button-wrapper"]),
          ],
          [Icons.left_arrow(["prev-instance", "no-prev", "noselect"])],
        );
      };

    let next_btn =
      if (i < num_instances - 1) {
        let next_inst = (u, i + 1);
        Node.div(
          [
            Attr.create("title", next_title),
            Attr.classes(["instance-button-wrapper"]),
            Attr.on_click(_ => inject(SelectHoleInstance(next_inst))),
            Attr.on_keydown(ev => {
              let updates =
                KeyCombo.matches(next_key, ev)
                  ? [inject(SelectHoleInstance(next_inst))] : [];
              Event.Many([Event.Prevent_default, ...updates]);
            }),
          ],
          [Icons.right_arrow(["next-instance", "has-next", "noselect"])],
        );
      } else {
        Node.div(
          [
            Attr.create("title", next_title),
            Attr.classes(["instance-button-wrapper"]),
          ],
          [Icons.right_arrow(["next-instance", "no-next", "noselect"])],
        );
      };

    let controls =
      Node.div(
        [Attr.classes(["instance-controls"])],
        [prev_btn, next_btn],
      );

    Node.div([Attr.classes(["path-summary"])], [msg, controls]);
  };

  let view_of_path_item = ((inst, x)) =>
    Node.div(
      [Attr.classes(["path-item"])],
      [
        Node.div(
          [Attr.classes(["inst"])],
          [
            DHCode.view_of_hole_instance(
              ~inject,
              ~width=30,
              ~selected_instance,
              ~settings,
              ~font_metrics,
              inst,
            ),
          ],
        ),
        Node.div(
          [Attr.classes(["inst-var-separator"])],
          [Node.text("·")],
        ),
        Node.div([Attr.classes(["path-var"])], [DHCode.view_of_var(x)]),
      ],
    );

  let path_view = (inst, path: InstancePath.t) => {
    let (titlebar_txt, path_area_children) =
      switch (path) {
      | [] => (
          "which is in the result",
          [
            Node.div(
              [Attr.classes(["special-msg"])],
              [Node.div([], [Node.text("immediately")])],
            ),
          ],
        )
      | _ =>
        let titlebar_txt = "which is in the result via path";
        let path_area_children =
          List.fold_left(
            (acc, path_item) =>
              [
                view_of_path_item(path_item),
                Node.span(
                  [Attr.classes(["path-item-separator"])],
                  [Node.text(" 〉 ")],
                ),
                ...acc,
              ],
            [
              Node.div(
                [Attr.classes(["trailing-inst"])],
                [
                  DHCode.view_of_hole_instance(
                    ~inject,
                    ~width=30,
                    ~selected_instance,
                    ~settings,
                    ~font_metrics,
                    inst,
                  ),
                ],
              ),
            ],
            path,
          );

        (
          titlebar_txt,
          [Node.div([Attr.classes(["path-area"])], path_area_children)],
        );
      };

    Node.div(
      [Attr.classes(["path-view-with-path"])],
      [
        Panel.view_of_other_title_bar(titlebar_txt),
        Node.div([Attr.classes(["path-area-parent"])], path_area_children),
      ],
    );
  };

  let context_view = {
    let ctx =
      program
      |> Program.get_cursor_info
      |> CursorInfo_common.get_ctx
      |> Contexts.gamma;
    let sigma =
      if (settings.evaluate) {
        switch (selected_instance) {
        | None => Environment.id_env(ctx)
        | Some(inst) =>
          switch (HoleInstanceInfo.lookup(hii, inst)) {
          | None =>
            // raise(InvalidInstance)
            print_endline("[InvalidInstance]");
            Environment.id_env(ctx);
          | Some((sigma, _)) => sigma
          }
        };
      } else {
        Environment.id_env(ctx);
      };
    switch (VarCtx.to_list(ctx)) {
    | [] =>
      Node.div(
        [Attr.classes(["the-context"])],
        [
          Node.div(
            [Attr.classes(["context-is-empty-msg"])],
            [Node.text("no variables in scope")],
          ),
        ],
      )
    | ctx_lst =>
      Node.div(
        [Attr.classes(["the-context"])],
        List.map(context_entry(sigma), ctx_lst),
      )
    };
  };

  /**
   * Shows the `InstancePath` to the currently selected instance.
   */
  let path_viewer =
    if (settings.evaluate) {
      let ctx =
        program
        |> Program.get_cursor_info
        |> CursorInfo_common.get_ctx
        |> Contexts.gamma;
      if (VarMap.is_empty(ctx)) {
        Node.div([], []);
      } else {
        let children =
          switch (program |> Program.get_zexp |> ZExp.cursor_on_EmptyHole) {
          | None => [
              instructional_msg(
                "Move cursor to a hole, or click a hole instance in the result, to see closures.",
              ),
            ]
          | Some(u) =>
            switch (selected_instance) {
            | None => [
                instructional_msg("Click on a hole instance in the result"),
              ]
            | Some((u', _) as inst) =>
              if (MetaVar.eq(u, u')) {
                switch (HoleInstanceInfo.lookup(hii, inst)) {
                | None =>
                  // raise(InvalidInstance)
                  [instructional_msg("Internal Error: InvalidInstance")]
                | Some((_, path)) => [
                    path_view_titlebar,
                    hii_summary(hii, inst),
                    path_view(inst, path),
                  ]
                };
              } else {
                [
                  instructional_msg(
                    "Internal Error: cursor is not at the selected hole instance.",
                  ),
                ];
              }
            }
          };
        Node.div([Attr.classes(["the-path-viewer"])], children);
      };
    } else {
      Node.div([], []);
    };

  Node.div(
    [Attr.classes(["panel", "context-inspector-panel"])],
    [
      Panel.view_of_main_title_bar("context"),
      Node.div(
        [Attr.classes(["panel-body", "context-inspector-body"])],
        [context_view, path_viewer],
      ),
    ],
  );
};
