module Vdom = Virtual_dom.Vdom;

exception InvalidInstance;
let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      ~selected_hole_closure: option(HoleClosure.t),
      ~settings: Settings.Evaluation.t,
      ~font_metrics: FontMetrics.t,
      program: Program.t,
    )
    : Vdom.Node.t => {
  open Vdom;

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
  let dynamic_info = (sigma: Environment.t, x) =>
    switch (VarMap.lookup(sigma, x)) {
    | None => None
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
                  ~selected_hole_closure,
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

  let context_entry = (sigma: Environment.t, (x, ty)) => {
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
    let num_instances = HoleClosureInfo.num_unique_hcs(hii, u);
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
                  DHCode.view_of_hole_closure(
                    ~inject,
                    ~width=30,
                    ~selected_hole_closure,
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
            Attr.on_click(_ => inject(SelectHoleClosure(prev_inst))),
            Attr.on_keydown(ev => {
              let updates =
                KeyCombo.matches(prev_key, ev)
                  ? [inject(SelectHoleClosure(prev_inst))] : [];
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
            Attr.on_click(_ => inject(SelectHoleClosure(next_inst))),
            Attr.on_keydown(ev => {
              let updates =
                KeyCombo.matches(next_key, ev)
                  ? [inject(SelectHoleClosure(next_inst))] : [];
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

  let hc_parents_view = (hc_parents: HoleClosureParents.t) => {
    let parents_info =
      hc_parents
      |> List.map(((x, (u, i))) =>
           (u, i) == HoleClosure.result_hc
             ? Node.div([], [Node.text("directly in result")])
             : Node.div(
                 [Attr.classes(["path-area-parent"])],
                 [
                   Node.div(
                     [Attr.classes(["path-area"])],
                     [
                       Node.div(
                         [Attr.classes(["path-item"])],
                         [
                           Node.div(
                             [Attr.classes(["inst"])],
                             [
                               DHCode.view_of_hole_closure(
                                 ~inject,
                                 ~width=30,
                                 ~selected_hole_closure,
                                 ~settings,
                                 ~font_metrics,
                                 (u, i),
                               ),
                             ],
                           ),
                           Node.span(
                             [Attr.classes(["path-item-separator"])],
                             [Node.text(" 〉 " ++ x)],
                           ),
                         ],
                       ),
                     ],
                   ),
                 ],
               )
         );

    Node.div(
      [],
      [
        Panel.view_of_other_title_bar("Hole closure parents"),
        ...parents_info,
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
        let (_, hii, _) = program |> Program.get_result;
        switch (selected_hole_closure) {
        | None => Environment.empty
        | Some((u, i)) =>
          switch (HoleClosureInfo.find_hc_opt(hii, u, i)) {
          | None =>
            // raise(InvalidInstance)
            print_endline("[InvalidInstance]");
            Environment.empty;
          | Some((sigma, _)) => sigma |> EvalEnv.environment_of_evalenv
          }
        };
      } else {
        Environment.empty;
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
   * Shows the `HoleClosureParents.t` for the currently selected hole closure.
   */
  let path_viewer =
    if (settings.evaluate) {
      let (_, hii, _) = program |> Program.get_result;
      let children =
        switch (program |> Program.get_zexp |> ZExp.cursor_on_EmptyHole) {
        | None => [
            instructional_msg(
              "Move cursor to a hole, or click a hole instance in the result, to see closures.",
            ),
          ]
        | Some(u) =>
          switch (selected_hole_closure) {
          | None => [
              instructional_msg("Click on a hole instance in the result"),
            ]
          | Some((u', i) as inst) =>
            if (MetaVar.eq(u, u')) {
              switch (HoleClosureInfo.find_hc_opt(hii, u, i)) {
              | None =>
                // raise(InvalidInstance)
                [instructional_msg("Internal Error: InvalidInstance")]
              | Some((_, hc_parents)) => [
                  path_view_titlebar,
                  hii_summary(hii, inst),
                  hc_parents_view(hc_parents),
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
