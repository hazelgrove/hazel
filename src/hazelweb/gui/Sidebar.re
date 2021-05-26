open Incr_dom;
module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

let mk_sidebar =
    (
      panels_thunk,
      collapsible_sidebar_id: string,
      tab_id: string,
      tab_opened_icon: Vdom.Node.t,
      tab_closed_icon: Vdom.Node.t,
      slidable_body_id: string,
      body_id: string,
      sidebar_open: bool,
      ~on_toggle: Js.t(Dom_html.mouseEvent) => Vdom.Event.t,
    ) => {
  let panels = sidebar_open ? panels_thunk() : [];
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
          [Attr.classes(["sidebar"]), Attr.on_click(on_toggle)],
          [
            Node.div(
              [
                Attr.id(slidable_body_id),
                Attr.classes(["sidebar-body-slider"]),
              ],
              [
                Node.div(
                  [
                    Attr.classes(
                      ["sidebar-body-padding"]
                      @ (
                        sidebar_open ? [] : ["sidebar-body-padding-expanded"]
                      ),
                    ),
                  ],
                  [],
                ),
                Node.div(
                  [
                    Attr.id(body_id),
                    Attr.on_click(_ => {Vdom.Event.Stop_propagation}),
                    Attr.classes(["sidebar-body"]),
                  ],
                  panels,
                ),
              ],
            ),
            Node.div(
              [Attr.id(tab_id), Attr.classes(["sidebar-tab"])],
              [sidebar_open ? tab_opened_icon : tab_closed_icon],
            ),
          ],
        ),
      ],
    )
  );
};

let mk_tabbed_sidebar =
    (
      panels_thunk:
        list((bool, Vdom.Node.t, Vdom.Event.t, unit => Vdom.Node.t)),
      collapsible_sidebar_id: string,
      tab_id: string,
      tab_opened_icon: Vdom.Node.t,
      tab_closed_icon: Vdom.Node.t,
      slidable_body_id: string,
      body_id: string,
      sidebar_open: bool,
      ~on_toggle: Js.t(Dom_html.mouseEvent) => Vdom.Event.t,
    ) => {
  let filtered =
    List.filter(((tab_open, _, _, _)) => tab_open, panels_thunk);
  let panels =
    List.map(
      ((_, _, _, tab_content_thunk)) => tab_content_thunk(),
      filtered,
    );
  let icons =
    List.map(
      ((_tab_open, icon, toggle, _)) => {
        Vdom.(
          Node.div(
            [
              Attr.on_click(_ =>
                Event.Many([
                  Event.Prevent_default,
                  Event.Stop_propagation,
                  toggle,
                ])
              ),
            ],
            [icon],
          )
        )
      },
      panels_thunk,
    );
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
          [
            Attr.classes(["sidebar", "right-sidebar"]),
            Attr.on_click(on_toggle),
          ],
          [
            Node.div(
              [],
              icons
              @ [
                Node.div(
                  [Attr.id(tab_id), Attr.classes(["sidebar-tab"])],
                  [sidebar_open ? tab_opened_icon : tab_closed_icon],
                ),
              ],
            ),
            Node.div(
              [
                Attr.id(slidable_body_id),
                Attr.classes(["sidebar-body-slider"]),
              ],
              [
                Node.div(
                  [
                    Attr.classes(
                      ["sidebar-body-padding"]
                      @ (
                        sidebar_open ? [] : ["sidebar-body-padding-expanded"]
                      ),
                    ),
                  ],
                  [],
                ),
                Node.div(
                  [
                    Attr.id(body_id),
                    Attr.on_click(_ => {Vdom.Event.Stop_propagation}),
                    Attr.classes(["sidebar-body"]),
                  ],
                  panels,
                ),
              ],
            ),
          ],
        ),
      ],
    )
  );
};

let left_side_bar_icon_opened =
  Vdom.(
    Node.div(
      [],
      [
        Icons.left_arrow(["left-sidebar-tab-icon-opened"]),
        Icons.question_mark_circle,
      ],
    )
  );
let left = (~inject, ~is_open: bool, left_panels) => {
  mk_sidebar(
    left_panels,
    "collapsible-left-bar",
    "left-tab",
    left_side_bar_icon_opened,
    Icons.question_mark_circle,
    "slidable-left-bar-body",
    "left-bar-body",
    is_open,
    ~on_toggle=_ =>
    inject(ModelAction.ToggleLeftSidebar)
  );
};

let right =
    (
      ~inject,
      ~is_open: bool,
      right_panels:
        list((bool, Vdom.Node.t, Vdom.Event.t, unit => Vdom.Node.t)),
    ) => {
  mk_tabbed_sidebar(
    right_panels,
    "collapsible-right-bar",
    "right-tab",
    Icons.right_arrow(["sidebar-tab-icon"]),
    Icons.left_arrow(["sidebar-tab-icon"]),
    "slidable-right-bar-body",
    "right-bar-body",
    is_open,
    ~on_toggle=_ =>
    inject(ModelAction.UpdateSettings(RightPanel(Toggle_open)))
  );
};
