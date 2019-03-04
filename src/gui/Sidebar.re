open Tyxml_js;
module Js = Js_of_ocaml.Js;

let make_sidebar =
    (
      panels,
      collapsible_sidebar_id: string,
      tab_id: string,
      tab_opened_str: string,
      tab_closed_str: string,
      slidable_body_id: string,
      body_padding_id: string,
      body_id: string,
    ) => {
  /* Adds and removes CSS classes to trigger the show/hide animation. */
  let onclick =
    Html.a_onclick(_ => {
      let sidebar = JSUtil.forceGetElementById(collapsible_sidebar_id);
      let collapsed_sidebar_class = Js.string("collapsed-sidebar");

      let tab = JSUtil.forceGetElementById(tab_id);

      let body_padding = JSUtil.forceGetElementById(body_padding_id);
      let body_padding_expanded_class =
        Js.string("sidebar-body-padding-expanded");

      if (Js.to_bool(sidebar##.classList##contains(collapsed_sidebar_class))) {
        sidebar##.classList##remove(collapsed_sidebar_class);
        tab##.innerHTML := Js.string(tab_opened_str);
        body_padding##.classList##remove(body_padding_expanded_class);
      } else {
        sidebar##.classList##add(collapsed_sidebar_class);
        tab##.innerHTML := Js.string(tab_closed_str);
        body_padding##.classList##add(body_padding_expanded_class);
      };

      true;
    });

  Html.(
    div(
      ~a=[a_id(collapsible_sidebar_id), a_class(["collapsible-sidebar"])],
      [
        div(
          ~a=[a_class(["sidebar"])],
          [
            div(
              ~a=[a_id(tab_id), a_class(["sidebar-tab"]), onclick],
              [txt(tab_opened_str)],
            ),
            div(
              ~a=[a_id(slidable_body_id), a_class(["sidebar-body-slider"])],
              [
                div(
                  ~a=[
                    a_id(body_padding_id),
                    a_class(["sidebar-body-padding"]),
                    onclick,
                  ],
                  [],
                ),
                div(~a=[a_id(body_id), a_class(["sidebar-body"])], panels),
              ],
            ),
          ],
        ),
      ],
    )
  );
};

let left = left_panels => {
  make_sidebar(
    left_panels,
    "collapsible-left-bar",
    "left-tab",
    "◀",
    "▶",
    "slidable-left-bar-body",
    "left-bar-body-padding",
    "left-bar-body",
  );
};

let right = right_panels => {
  make_sidebar(
    right_panels,
    "collapsible-right-bar",
    "right-tab",
    "▶",
    "◀",
    "slidable-right-bar-body",
    "right-bar-body-padding",
    "right-bar-body",
  );
};
