module Vdom = Virtual_dom.Vdom;

exception InvalidInstance;
let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      ~selected_instance: option(TaggedNodeInstance.t),
      ~settings: Settings.Evaluation.t,
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
            HTypCode.view(~width=50, ~pos=Var.length(x) + 3, ty),
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
                  ~width=50,
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

  let path_view_titlebar = Panel.view_of_other_title_bar("Select a closure");

  [@warning "-26"]
  let mii_summary = (mii, (kind, (u, i) as inst): TaggedNodeInstance.t) => {
    let num_instances = NodeInstanceInfo.num_instances(mii, u);
    let kindstr =
      switch (kind) {
      | Hole => "hole"
      | Livelit => "livelit application"
      };
    let extra_space = Unicode.nbsp ++ Unicode.nbsp;

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
            Attr.on_mousedown(_ => inject(SelectInstance(kind, prev_inst))),
            Attr.on_keydown(ev => {
              let updates =
                KeyCombo.matches(prev_key, ev)
                  ? [inject(SelectInstance(kind, prev_inst))] : [];
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
            Attr.on_mousedown(_ => inject(SelectInstance(kind, next_inst))),
            Attr.on_keydown(ev => {
              let updates =
                KeyCombo.matches(next_key, ev)
                  ? [inject(SelectInstance(kind, next_inst))] : [];
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

    let msg =
      Node.div(
        [Attr.classes(["instance-info"])],
        [
          Node.div(
            [],
            [
              Node.text(kindstr ++ " "),
              Node.span(
                [Attr.classes(["hole-name-normal-txt"])],
                [Node.text(string_of_int(u + 1))],
              ),
              Node.text(extra_space ++ "|" ++ extra_space ++ "closure "),
              Node.span(
                [Attr.classes(["instance-selector"])],
                [
                  Node.text(Unicode.nbsp),
                  prev_btn,
                  Node.span(
                    [Attr.classes(["inst-number-normal-txt"])],
                    [
                      Node.text(
                        extra_space ++ string_of_int(i + 1) ++ extra_space,
                      ),
                    ],
                  ),
                  next_btn,
                  Node.text(Unicode.nbsp),
                ],
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

    let _controls =
      Node.div(
        [Attr.classes(["instance-controls"])],
        [prev_btn, next_btn],
      );

    Node.div([Attr.classes(["path-summary"])], [msg]);
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
              ~width=50,
              ~selected_instance,
              ~settings,
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

  let path_view = ((kind, inst): TaggedNodeInstance.t, path: InstancePath.t) => {
    let result_str =
      switch (kind) {
      | Hole => "result"
      | Livelit => "cc-result"
      };
    let (titlebar_txt, path_area_children) =
      switch (path) {
      | [] => (
          "which is in the " ++ result_str,
          [
            Node.div(
              [Attr.classes(["special-msg"])],
              [Node.div([], [Node.text("immediately")])],
            ),
          ],
        )
      | _ =>
        let titlebar_txt = "which is in the " ++ result_str ++ " via path";
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
                    ~width=50,
                    ~selected_instance,
                    ~settings,
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

  module Elaborator = Elaborator_Exp.M(Statics_Exp.M);

  let context_view = {
    let contents =
      switch (Program.get_cursor_info(program)) {
      | None => [
          Node.div(
            [Attr.classes(["context-is-empty-msg"])],
            [Node.text("place cursor to see variables in scope")],
          ),
        ]
      | Some({ctx, _}) =>
        let ctx = Contexts.gamma(ctx);
        let sigma =
          if (settings.evaluate) {
            let (_, hii, llii, _) = program |> Program.get_result;
            switch (selected_instance) {
            | None => Elaborator.id_env(ctx)
            | Some((kind, inst)) =>
              let lookup = ii =>
                switch (NodeInstanceInfo.lookup(ii, inst)) {
                | None =>
                  // raise(InvalidInstance);
                  print_endline("[InvalidInstance]");
                  Elaborator.id_env(ctx);
                | Some((sigma, _, _)) => sigma
                };
              switch (kind) {
              | Hole => lookup(hii)
              | Livelit => lookup(llii)
              };
            };
          } else {
            Elaborator.id_env(ctx);
          };
        switch (VarCtx.to_list(ctx)) {
        | [] => [
            Node.div(
              [Attr.classes(["context-is-empty-msg"])],
              [Node.text("no variables in scope")],
            ),
          ]
        | ctx_lst => List.map(context_entry(sigma), ctx_lst)
        };
      };
    Node.div([Attr.classes(["the-context"])], contents);
  };

  /**
   * Shows the `InstancePath` to the currently selected instance.
   */
  let path_viewer = {
    let contents =
      if (!settings.evaluate) {
        [];
      } else {
        switch (Program.get_cursor_info(program)) {
        | None => []
        | Some({ctx, _}) =>
          let ctx = Contexts.gamma(ctx);
          switch (ctx, Program.get_zexp(program)) {
          | ([], _)
          | (_, None) => []
          | ([_, ..._], Some(ze)) =>
            switch (ZExp.cursor_on_inst(ze)) {
            | None => [
                instructional_msg(
                  "Move cursor to a hole or livelit application, or click a hole instance in the result, to see closures.",
                ),
              ]
            | Some((kind, u)) =>
              switch (selected_instance) {
              | None => [
                  instructional_msg("Click on a hole instance in the result"),
                ]
              | Some((kind', (u', _) as inst)) =>
                if (kind == kind' && MetaVar.eq(u, u')) {
                  let (_, hii, llii, _) = program |> Program.get_result;
                  let helper = mii =>
                    switch (NodeInstanceInfo.lookup(mii, inst)) {
                    | None =>
                      // raise(InvalidInstance)
                      [
                        instructional_msg("Internal Error: [InvalidInstance]"),
                      ]
                    | Some((_, path, _)) => [
                        path_view_titlebar,
                        mii_summary(mii, (kind, inst)),
                        path_view((kind', inst), path),
                      ]
                    };
                  switch (kind) {
                  | Hole => helper(hii)
                  | Livelit => helper(llii)
                  };
                } else {
                  [
                    instructional_msg(
                      "Internal Error: cursor is not at the selected hole instance.",
                    ),
                  ];
                }
              }
            }
          };
        };
      };
    Node.div([Attr.classes(["the-path-viewer"])], contents);
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
