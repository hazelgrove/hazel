module Vdom = Virtual_dom.Vdom;
module KeyCombo = JSUtil.KeyCombo;

exception InvalidInstance;
let view =
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t => {
  let static_info = ((x, ty)) => {
    Vdom.(
      Node.div(
        [Attr.classes(["static-info"])],
        [
          Node.div(
            [Attr.classes(["code"])],
            [
              Node.span([Attr.classes(["var"])], [Node.text(x)]),
              Node.text(" : "),
              Code.view_of_htyp(~inject, ty),
            ],
          ),
        ],
      )
    );
  };

  let dynamic_info = (sigma, x) =>
    switch (VarMap.lookup(sigma, x)) {
    | None =>
      Some(
        Vdom.(
          Node.div(
            [Attr.classes(["dynamic-info"])],
            [
              Node.div(
                [Attr.classes(["code"])],
                [Node.span([], [Node.text("NONE!!!!!!")])],
              ),
            ],
          )
        ),
      )
    | Some(Dynamics.DHExp.BoundVar(x')) when Var.eq(x, x') => None
    | Some(d) =>
      Some(
        Vdom.(
          Node.div(
            [Attr.classes(["dynamic-info"])],
            [
              Node.div(
                [Attr.classes(["code"])],
                [Code.view_of_dhexp(~inject, d)],
              ),
            ],
          )
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
    Vdom.(Node.div([Attr.classes(["context-entry"])], children));
  };

  let context_view = {
    let ctx = Contexts.gamma(model.cursor_info.ctx);
    let (_, hii, _) = model.result;
    let sigma =
      switch (model.selected_instance) {
      | None => Dynamics.DHExp.id_env(ctx)
      | Some(inst) =>
        switch (Dynamics.DHExp.HoleInstanceInfo.lookup(hii, inst)) {
        | None => raise(InvalidInstance)
        | Some((sigma, _)) => sigma
        }
      };
    switch (VarCtx.to_list(ctx)) {
    | [] =>
      Vdom.(
        Node.div(
          [Attr.classes(["the-context"])],
          [
            Vdom.(
              Node.div(
                [Attr.classes(["context-is-empty-msg"])],
                [Node.text("no variables in scope")],
              )
            ),
          ],
        )
      )
    | ctx_lst =>
      Vdom.(
        Node.div(
          [Attr.classes(["the-context"])],
          List.map(context_entry(sigma), ctx_lst),
        )
      )
    };
  };

  let path_view_titlebar =
    Panel.view_of_other_title_bar("Closure above observed at ");
  let instructional_msg = msg =>
    Vdom.(
      Node.div([Attr.classes(["instructional-msg"])], [Node.text(msg)])
    );
  let view_of_path_item = ((inst, x)) =>
    Vdom.(
      Node.div(
        [Attr.classes(["path-item"])],
        [
          Node.div(
            [Attr.classes(["inst"])],
            [Code.view_of_hole_instance(~inject, inst)],
          ),
          Node.div(
            [Attr.classes(["inst-var-separator"])],
            [Node.text("·")],
          ),
          Node.div(
            [Attr.classes(["path-var"])],
            [Code.view_of_Var(~inject, x)],
          ),
        ],
      )
    );

  let path_view = (inst, path: Dynamics.DHExp.InstancePath.t) => {
    let (titlebar_txt, path_area_children) =
      switch (path) {
      | [] => (
          "which is in the result",
          [
            Vdom.(
              Node.div(
                [Attr.classes(["special-msg"])],
                [Node.div([], [Node.text("immediately")])],
              )
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
                Vdom.(
                  Node.span(
                    [Attr.classes(["path-item-separator"])],
                    [Node.text(" 〉 ")],
                  )
                ),
                ...acc,
              ],
            [
              Vdom.(
                Node.div(
                  [Attr.classes(["trailing-inst"])],
                  [Code.view_of_hole_instance(~inject, inst)],
                )
              ),
            ],
            path,
          );

        (
          titlebar_txt,
          [
            Vdom.(
              Node.div([Attr.classes(["path-area"])], path_area_children)
            ),
          ],
        );
      };

    Vdom.(
      Node.div(
        [Attr.classes(["path-view-with-path"])],
        [
          Panel.view_of_other_title_bar(titlebar_txt),
          Node.div(
            [Attr.classes(["path-area-parent"])],
            path_area_children,
          ),
        ],
      )
    );
  };

  let hii_summary = (hii, (u_, i_) as inst) => {
    let num_instances =
      Dynamics.DHExp.HoleInstanceInfo.num_instances(hii, u_);
    let msg =
      Vdom.(
        Node.div(
          [Attr.classes(["instance-info"])],
          [
            Node.div(
              [],
              [
                Node.div(
                  [Attr.classes(["hii-summary-inst"])],
                  [Code.view_of_hole_instance(~inject, inst)],
                ),
                Node.text(" = hole "),
                Node.span(
                  [Attr.classes(["hole-name-normal-txt"])],
                  [Node.text(string_of_int(u_ + 1))],
                ),
                Node.text(" instance "),
                Node.span(
                  [Attr.classes(["inst-number-normal-txt"])],
                  [Node.text(string_of_int(i_ + 1))],
                ),
                Node.text(" of "),
                Node.span(
                  [Attr.classes(["inst-number-normal-txt"])],
                  [Node.text(string_of_int(num_instances))],
                ),
              ],
            ),
          ],
        )
      );

    let prev_key = KeyCombo.Details.alt_PageUp;
    let next_key = KeyCombo.Details.alt_PageDown;

    let prev_title =
      "Previous instance (" ++ KeyCombo.Details.name(prev_key) ++ ")";
    let next_title =
      "Next instance (" ++ KeyCombo.Details.name(next_key) ++ ")";

    let prev_btn =
      switch (model.context_inspector.prev_state) {
      | Some((u, i)) =>
        Vdom.(
          Node.div(
            [
              Attr.create("title", prev_title),
              Attr.classes(["instance-button-wrapper"]),
              Attr.on_click(_ => inject(SelectHoleInstance(u, i))),
              Attr.on_keydown(ev => {
                let updates =
                  KeyCombo.Details.matches(prev_key, ev)
                    ? [inject(SelectHoleInstance(u, i))] : [];
                Event.Many([Event.Prevent_default, ...updates]);
              }),
            ],
            [
              SvgShapes.left_arrow(
                ["prev-instance", "has-prev", "noselect"],
                (),
              ),
            ],
          )
        )
      | None =>
        Vdom.(
          Node.div(
            [
              Attr.create("title", prev_title),
              Attr.classes(["instance-button-wrapper"]),
            ],
            [
              SvgShapes.left_arrow(
                ["prev-instance", "no-prev", "noselect"],
                (),
              ),
            ],
          )
        )
      };

    let next_btn =
      switch (model.context_inspector.next_state) {
      | Some((u, i)) =>
        Vdom.(
          Node.div(
            [
              Attr.create("title", next_title),
              Attr.classes(["instance-button-wrapper"]),
              Attr.on_click(_ => inject(SelectHoleInstance(u, i))),
              Attr.on_keydown(ev => {
                let updates =
                  KeyCombo.Details.matches(next_key, ev)
                    ? [inject(SelectHoleInstance(u, i))] : [];
                Event.Many([Event.Prevent_default, ...updates]);
              }),
            ],
            [
              SvgShapes.right_arrow(
                ["next-instance", "has-next", "noselect"],
                (),
              ),
            ],
          )
        )
      | None =>
        Vdom.(
          Node.div(
            [
              Attr.create("title", next_title),
              Attr.classes(["instance-button-wrapper"]),
            ],
            [
              SvgShapes.right_arrow(
                ["next-instance", "no-next", "noselect"],
                (),
              ),
            ],
          )
        )
      };

    let controls =
      Vdom.(
        Node.div(
          [Attr.classes(["instance-controls"])],
          [prev_btn, next_btn],
        )
      );

    Vdom.(Node.div([Attr.classes(["path-summary"])], [msg, controls]));
  };

  let path_viewer = {
    let (_, hii, _) = model.result;
    if (VarMap.is_empty(Contexts.gamma(model.cursor_info.ctx))) {
      Vdom.Node.div([], []);
    } else {
      let children =
        switch (model.cursor_info.node) {
        | CursorInfo.Exp(EmptyHole(u)) =>
          switch (model.selected_instance) {
          | Some((u', _) as inst) =>
            if (MetaVar.eq(u, u')) {
              switch (Dynamics.DHExp.HoleInstanceInfo.lookup(hii, inst)) {
              | Some((_, path)) => [
                  path_view_titlebar,
                  hii_summary(hii, inst),
                  path_view(inst, path),
                ]
              | None => raise(InvalidInstance)
              };
            } else {
              [
                instructional_msg(
                  "Internal Error: cursor is not at the selected hole instance.",
                ),
              ];
            }
          | None => [
              instructional_msg("Click on a hole instance in the result"),
            ]
          }
        | _ => [
            instructional_msg(
              "Move cursor to a hole, or click a hole instance in the result, to see  closures.",
            ),
          ]
        };
      Vdom.(Node.div([Attr.classes(["the-path-viewer"])], children));
    };
  };

  Vdom.(
    Node.div(
      [Attr.classes(["panel", "context-inspector-panel"])],
      [
        Panel.view_of_main_title_bar("context"),
        Node.div(
          [Attr.classes(["panel-body", "context-inspector-body"])],
          [context_view, path_viewer],
        ),
      ],
    )
  );
};
