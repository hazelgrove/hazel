open Incr_dom;
module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

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
      ~on_toggle: Js.t(Dom_html.mouseEvent) => Vdom.Event.t,
    ) =>
  Vdom.(
    Node.div(
      [
        Attr.id(collapsible_sidebar_id),
        Attr.classes(
          ["collapsible-sidebar"]
          @ (sidebar_open ? [] : ["collapsed-sidebar"]),
        ),
      ],
      [
        Node.div(
          [Attr.classes(["sidebar"])],
          [
            Node.div(
              [
                Attr.id(slidable_body_id),
                Attr.classes(["sidebar-body-slider"]),
              ],
              [
                Node.div(
                  [
                    Attr.id(body_padding_id),
                    Attr.classes(
                      ["sidebar-body-padding"]
                      @ (
                        sidebar_open ? [] : ["sidebar-body-padding-expanded"]
                      ),
                    ),
                    Attr.on_click(on_toggle),
                  ],
                  [],
                ),
                Node.div(
                  [Attr.id(body_id), Attr.classes(["sidebar-body"])],
                  panels,
                ),
              ],
            ),
            Node.div(
              [
                Attr.id(tab_id),
                Attr.classes(["sidebar-tab"]),
                Attr.on_click(on_toggle),
              ],
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
    ~on_toggle=_ =>
    inject(Update.Action.ToggleLeftSidebar)
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
    ~on_toggle=_ =>
    inject(Update.Action.ToggleRightSidebar)
  );
};
