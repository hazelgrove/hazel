open Virtual_dom.Vdom;
open Node;

let mk_sidebar =
    (
      panels_thunk,
      collapsible_sidebar_id: string,
      tab_opened_icon: t,
      tab_closed_icon: t,
      sidebar_open: bool,
      ~on_toggle,
    ) =>
  div(
    [
      Attr.id(collapsible_sidebar_id),
      Attr.on_click(on_toggle),
      Attr.classes(
        ["sidebar"] @ (sidebar_open ? [] : ["sidebar-collapsed"]),
      ),
    ],
    [
      sidebar_open
        ? div(
            [
              Attr.class_("sidebar-body"),
              Attr.on_click(_ => Event.Stop_propagation),
            ],
            panels_thunk(),
          )
        : div([], []),
      div(
        [Attr.class_("sidebar-tab")],
        [sidebar_open ? tab_opened_icon : tab_closed_icon],
      ),
    ],
  );

let left = (~inject, ~is_open: bool, left_panels) =>
  mk_sidebar(
    left_panels,
    "sidebar-left",
    Icons.left_side_bar_icon_opened,
    Icons.question_mark_circle,
    is_open,
    ~on_toggle=_ =>
    inject(ModelAction.ToggleLeftSidebar)
  );

let right = (~inject, ~is_open: bool, right_panels) =>
  mk_sidebar(
    right_panels,
    "sidebar-right",
    Icons.right_arrow(["sidebar-tab-icon"]),
    Icons.left_arrow(["sidebar-tab-icon"]),
    is_open,
    ~on_toggle=_ =>
    inject(ModelAction.ToggleRightSidebar)
  );
