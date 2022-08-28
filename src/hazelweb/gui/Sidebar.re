open Virtual_dom.Vdom;
open Node;

let mk_sidebar =
    (
      ~panels,
      ~is_open: bool,
      ~id: string,
      ~icon_opened: t,
      ~icon_closed: t,
      ~on_toggle,
    ) =>
  div(
    ~attr=
      Attr.many([
        Attr.id(id),
        Attr.on_click(on_toggle),
        Attr.classes(["sidebar"] @ (is_open ? [] : ["sidebar-collapsed"])),
      ]),
    [
      is_open
        ? div(
            ~attr=
              Attr.many([
                Attr.class_("sidebar-body"),
                Attr.on_click(_ => Effect.Stop_propagation),
              ]),
            panels(),
          )
        : div([]),
      div(
        ~attr=Attr.class_("sidebar-tab"),
        [is_open ? icon_opened : icon_closed],
      ),
    ],
  );

let left = (~inject, ~model: Model.t) =>
  mk_sidebar(
    ~panels=() => [ActionPanel.view(~inject, model)],
    ~is_open=model.left_sidebar_open,
    ~id="sidebar-left",
    ~icon_opened=Icons.left_side_bar_icon_opened,
    ~icon_closed=Icons.question_mark_circle,
    ~on_toggle=_ => inject(ModelAction.ToggleLeftSidebar),
  );

let right =
    (~inject, ~model: Model.t, ~result as {test_map, hii, _}: Result.t) => {
  let program = Model.get_program(model);
  let test_path = Program.get_path_to_test(program);
  mk_sidebar(
    ~panels=
      () =>
        [
          TestPanel.view(~inject, ~model, ~test_map, ~test_path),
          ContextInspector.view(~inject, ~model, ~hii),
          UndoHistoryPanel.view(~inject, model),
          SettingsPanel.view(~inject, model.settings),
        ],
    ~is_open=model.right_sidebar_open,
    ~id="sidebar-right",
    ~icon_opened=Icons.right_arrow(["sidebar-tab-icon"]),
    ~icon_closed=Icons.left_arrow(["sidebar-tab-icon"]),
    ~on_toggle=_ => inject(ModelAction.ToggleRightSidebar),
  );
};
