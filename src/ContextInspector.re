open Js_of_ocaml_tyxml.Tyxml_js;
open Model;
let mk =
    (
      {
        cursor_info_rs,
        result_rs,
        user_selected_instances_rs,
        user_selected_instances_rf,
        selected_instance_rs,
        selected_instance_rf,
        _,
      },
      instance_click_fn,
    ) => {
  module M = {
    let type_width = 80000;
    let dhexp_width = 80000;
    let static_info = ((x, ty)) => {
      let ty_html = View.html_of_ty(type_width, "context-" ++ x, ty);
      Html5.(
        div(
          ~a=[a_class(["static-info"])],
          [
            div(
              ~a=[a_class(["code"])],
              [
                span(~a=[a_class(["var"])], [txt(x)]),
                txt(" : "),
                ty_html,
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
          Html5.(
            div(
              ~a=[a_class(["dynamic-info"])],
              [
                div(~a=[a_class(["code"])], [span([txt("NONE!!!!!!")])]),
              ],
            )
          ),
        )
      | Some(Dynamics.DHExp.BoundVar(x')) when Var.eq(x, x') => None
      | Some(d) =>
        let dhexp_html =
          View.html_of_dhexp(
            instance_click_fn,
            dhexp_width,
            "context-" ++ x,
            d,
          );

        Some(
          Html5.(
            div(
              ~a=[a_class(["dynamic-info"])],
              [div(~a=[a_class(["code"])], [dhexp_html])],
            )
          ),
        );
      };

    let context_entry = (sigma, (x, ty)) => {
      let static_info = static_info((x, ty));
      let children =
        switch (dynamic_info(sigma, x)) {
        | Some(dynamic_info) => [static_info, dynamic_info]
        | None => [static_info]
        };
      Html5.(div(~a=[a_class(["context-entry"])], children));
    };
    exception InvalidInstance;
    let context_view = (ctx: VarCtx.t, hii, selected_instance) => {
      let ctx_list = VarCtx.to_list(ctx);
      let sigma =
        switch (selected_instance) {
        | Some(inst) =>
          switch (Dynamics.DHExp.HoleInstanceInfo.lookup(hii, inst)) {
          | Some((sigma, _)) => sigma
          | None => raise(InvalidInstance)
          }
        | None => Dynamics.DHExp.id_env(ctx)
        };
      switch (ctx_list) {
      | [] =>
        Html5.(
          div(
            ~a=[a_class(["the-context"])],
            [
              Html5.(
                div(
                  ~a=[a_class(["context-is-empty-msg"])],
                  [txt("no variables in scope")],
                )
              ),
            ],
          )
        )
      | _ =>
        Html5.(
          div(
            ~a=[a_class(["the-context"])],
            List.map(context_entry(sigma), ctx_list),
          )
        )
      };
    };

    let path_view_titlebar =
      Panel.other_title_bar("Closure above observed at ");
    let instructional_msg = msg =>
      Html5.(div(~a=[a_class(["instructional-msg"])], [txt(msg)]));
    let html_of_path_item = ((inst, x)) =>
      Html5.(
        div(
          ~a=[a_class(["path-item"])],
          [
            div(
              ~a=[a_class(["inst"])],
              [
                View.html_of_hole_instance(
                  instance_click_fn,
                  80,
                  "path-view",
                  inst,
                ),
              ],
            ),
            div(~a=[a_class(["inst-var-separator"])], [txt("·")]),
            div(
              ~a=[a_class(["path-var"])],
              [View.html_of_var(80, "path-view", x)],
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
              Html5.(
                div(
                  ~a=[a_class(["special-msg"])],
                  [div([txt("immediately")])],
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
                  html_of_path_item(path_item),
                  Html5.(
                    span(
                      ~a=[a_class(["path-item-separator"])],
                      [Html5.txt(" 〉 ")],
                    )
                  ),
                  ...acc,
                ],
              [
                Html5.(
                  div(
                    ~a=[a_class(["trailing-inst"])],
                    [
                      View.html_of_hole_instance(
                        instance_click_fn,
                        80,
                        "path-view",
                        inst,
                      ),
                    ],
                  )
                ),
              ],
              path,
            );

          (
            titlebar_txt,
            [Html5.(div(~a=[a_class(["path-area"])], path_area_children))],
          );
        };

      Html5.(
        div(
          ~a=[a_class(["path-view-with-path"])],
          [
            Panel.other_title_bar(titlebar_txt),
            div(~a=[a_class(["path-area-parent"])], path_area_children),
          ],
        )
      );
    };

    type next_prev_state = (
      option(Dynamics.DHExp.HoleInstance.t),
      option(Dynamics.DHExp.HoleInstance.t),
    );
    let next_prev_state_initial: next_prev_state = (
      (None, None): next_prev_state
    );
    let next_key = JSUtil.KeyCombos.alt_PageDown;
    let prev_key = JSUtil.KeyCombos.alt_PageUp;
    let update_instance = inst => {
      let usi = React.S.value(user_selected_instances_rs);
      let usi' = UserSelectedInstances.update(usi, inst);
      user_selected_instances_rf(usi');
      selected_instance_rf(Some(inst));
    };
    let next_prev_state_rf = {
      let (next_prev_state_rs, next_prev_state_rf) =
        React.S.create(next_prev_state_initial);
      let _ =
        JSUtil.listen_for_key(
          next_key,
          _ => {
            let (next_state, _) = React.S.value(next_prev_state_rs);
            switch (next_state) {
            | Some(inst) => update_instance(inst)
            | None => ()
            };
          },
        );

      let _ =
        JSUtil.listen_for_key(
          prev_key,
          _ => {
            let (_, prev_state) = React.S.value(next_prev_state_rs);
            switch (prev_state) {
            | Some(inst) => update_instance(inst)
            | None => ()
            };
          },
        );

      next_prev_state_rf;
    };
    let hii_summary = (hii, (u_, i_) as inst, next_prev_state_rf) => {
      let num_instances =
        Dynamics.DHExp.HoleInstanceInfo.num_instances(hii, u_);
      let msg =
        Html5.(
          div(
            ~a=[a_class(["instance-info"])],
            [
              div([
                div(
                  ~a=[a_class(["hii-summary-inst"])],
                  [
                    View.html_of_hole_instance(
                      instance_click_fn,
                      80,
                      "hii-summary",
                      inst,
                    ),
                  ],
                ),
                txt(" = hole "),
                span(
                  ~a=[a_class(["hole-name-normal-txt"])],
                  [txt(string_of_int(u_ + 1))],
                ),
                txt(" instance "),
                span(
                  ~a=[a_class(["inst-number-normal-txt"])],
                  [txt(string_of_int(i_ + 1))],
                ),
                txt(" of "),
                span(
                  ~a=[a_class(["inst-number-normal-txt"])],
                  [txt(string_of_int(num_instances))],
                ),
              ]),
            ],
          )
        );

      let prev_state = i_ > 0 ? Some((u_, i_ - 1)) : None;

      let next_state = i_ < num_instances - 1 ? Some((u_, i_ + 1)) : None;
      let next_prev_state = (next_state, prev_state);
      next_prev_state_rf(next_prev_state);
      let onclick = (i', _) => {
        switch (i') {
        | Some(inst') => update_instance(inst')
        | None => ()
        };
        true;
      };
      let prev_title =
        "Previous instance (" ++ JSUtil.KeyCombo.name(prev_key) ++ ")";

      let next_title =
        "Next instance (" ++ JSUtil.KeyCombo.name(next_key) ++ ")";
      let prev_btn =
        switch (prev_state) {
        | Some(_) =>
          Html5.(
            div(
              ~a=[
                a_title(prev_title),
                a_class(["instance-button-wrapper"]),
                a_onclick(onclick(prev_state)),
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
          Html5.(
            div(
              ~a=[
                a_title(prev_title),
                a_class(["instance-button-wrapper"]),
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
        switch (next_state) {
        | Some(_) =>
          Html5.(
            div(
              ~a=[
                a_title(next_title),
                a_class(["instance-button-wrapper"]),
                a_onclick(onclick(next_state)),
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
          Html5.(
            div(
              ~a=[
                a_title(next_title),
                a_class(["instance-button-wrapper"]),
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
        Html5.(
          div(~a=[a_class(["instance-controls"])], [prev_btn, next_btn])
        );

      Html5.(div(~a=[a_class(["path-summary"])], [msg, controls]));
    };

    let path_viewer = (hii, selected_instance, sort, ctx, next_prev_state_rf) =>
      if (VarMap.is_empty(ctx)) {
        Html5.div([]);
      } else {
        let children =
          switch (sort) {
          | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.EmptyHole(u))) =>
            switch (selected_instance) {
            | Some((u', _) as inst) =>
              if (MetaVar.eq(u, u')) {
                switch (Dynamics.DHExp.HoleInstanceInfo.lookup(hii, inst)) {
                | Some((_, path)) => [
                    path_view_titlebar,
                    hii_summary(hii, inst, next_prev_state_rf),
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

        Html5.(div(~a=[a_class(["the-path-viewer"])], children));
      };

    let panel =
        (
          {CursorInfo.sort, CursorInfo.ctx: (gamma, _), _},
          (_, hii, _),
          selected_instance,
          next_prev_state_rf,
        ) => {
      let the_context_view = context_view(gamma, hii, selected_instance);
      let the_path_viewer =
        path_viewer(hii, selected_instance, sort, gamma, next_prev_state_rf);

      [
        Panel.main_title_bar("context"),
        Html5.(
          div(
            ~a=[a_class(["panel-body", "context-inspector-body"])],
            [the_context_view, the_path_viewer],
          )
        ),
      ];
    };
  };
  open M;
  let context_inspector_rs =
    React.S.l3(
      (cursor_info, result, selected_instance) =>
        panel(cursor_info, result, selected_instance, next_prev_state_rf),
      cursor_info_rs,
      result_rs,
      selected_instance_rs,
    );

  R.Html5.(
    div(
      ~a=[Html5.a_class(["panel", "context-inspector-panel"])],
      ReactiveData.RList.from_signal(context_inspector_rs),
    )
  );
};
