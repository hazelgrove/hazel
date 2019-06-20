open Incr_dom;
module Js = Js_of_ocaml.Js;

let make_sidebar =
    (
      panels,
      collapsible_sidebar_id: string,
      tab_id: string,
      tab_opened_icon,
      tab_closed_icon,
      slidable_body_id: string,
      body_padding_id: string,
      body_id: string,
      sidebar_open: bool,
      ~on_toggle: Vdom.Event.t,
    ) =>
  Vdom.Node.(
    div(
      [Attr.id(collapsible_sidebar_id), Attr.classes(["collapsible-sidebar"] @ (sidebar_open ? [] : ["collapsed-sidebar"]))],
      [
        div(
          [Attr.classes(["sidebar"])],
          [
            div(
              [Attr.id(slidable_body_id), Attr.classes(["sidebar-body-slider"])],
              [
                div(
                  [
                    Attr.id(body_padding_id),
                    Attr.classes(["sidebar-body-padding"] @ (sidebar_open ? [] : ["sidebar-body-padding-expanded"])),
                    Attr.on_click(_ => on_toggle),
                  ],
                  [],
                ),
                div(~a=[a_id(body_id), a_class(["sidebar-body"])], panels),
              ],
            ),
            div(
              ~a=[a_id(tab_id), a_class(["sidebar-tab"]), onclick],
              [sidebar_open ? tab_opened_icon() : tab_closed_icon()],
            ),
          ],
        ),
      ],
    )
  );

let left = (~inject, sidebar_open, left_panels) => {
  make_sidebar(
    left_panels,
    "collapsible-left-bar",
    "left-tab",
    SvgShapes.left_arrow(["sidebar-tab-icon"]),
    SvgShapes.right_arrow(["sidebar-tab-icon"]),
    "slidable-left-bar-body",
    "left-bar-body-padding",
    "left-bar-body",
    sidebar_open,
    ~on_toggle=inject(ToggleLeftSidebar),
  );
};

let right = (~inject, sidebar_open, right_panels) => {
  make_sidebar(
    right_panels,
    "collapsible-right-bar",
    "right-tab",
    SvgShapes.right_arrow(["sidebar-tab-icon"]),
    SvgShapes.left_arrow(["sidebar-tab-icon"]),
    "slidable-right-bar-body",
    "right-bar-body-padding",
    "right-bar-body",
    sidebar_open,
    ~on_toggle=inject(ToggleRightSidebar),
  );
};
