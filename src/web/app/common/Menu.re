/* Menu — primitives for the menu surface.

   Distinct from the workspace surface (see `Components.re`). The menu
   surface has its own background, typography, and item-row patterns;
   used for nut menus, dropdowns, context menus.

   These generate the HTML structure expected by
   src/web/www/style/menu/menu.css. */

open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* ============================================================
   Surface — menu shell. Optional corner anchor; without one the menu
   floats and all four corners are rounded.
   ============================================================ */

let surface =
    (
      ~attrs=[],
      ~anchor: option([ | `TL | `TR | `BL | `BR])=?,
      children,
    ) => {
  let anchor_cls =
    switch (anchor) {
    | Some(`TL) => ["tl"]
    | Some(`TR) => ["tr"]
    | Some(`BL) => ["bl"]
    | Some(`BR) => ["br"]
    | None => []
    };
  div(~attrs=[clss(["menu-surface"] @ anchor_cls)] @ attrs, children);
};

/* ============================================================
   Menu item — clickable row with leading icon, label, optional
   trailing shortcut.
   ============================================================ */

let item =
    (
      ~attrs=[],
      ~selected=false,
      ~icon: option(Node.t)=?,
      ~shortcut: option(string)=?,
      ~on_click=?,
      label,
    ) => {
  let icon_children =
    switch (icon) {
    | Some(n) => [n]
    | None => []
    };
  let shortcut_children =
    switch (shortcut) {
    | Some(s) => [div(~attrs=[clss(["shortcut"])], [text(s)])]
    | None => []
    };
  let event_attrs =
    switch (on_click) {
    | Some(action) => [Attr.on_click(_ => action)]
    | None => []
    };
  div(
    ~attrs=
      [clss(["menu-item"] @ (selected ? ["selected"] : []))]
      @ event_attrs
      @ attrs,
    icon_children
    @ [div(~attrs=[clss(["label"])], [label])]
    @ shortcut_children,
  );
};

/* ============================================================
   Menu group — named section. Adjacent groups get a hairline divider
   between them automatically via the CSS `:has(+ .menu-group)`.
   ============================================================ */

let group = (~attrs=[], name: string, items: list(Node.t)) =>
  div(
    ~attrs=[clss(["menu-group"])] @ attrs,
    [
      div(~attrs=[clss(["name"])], [text(name)]),
      div(~attrs=[clss(["contents"])], items),
    ],
  );

/* ============================================================
   Menu divider — thin separator between items in a group. Distinct
   from `Components.divider`.
   ============================================================ */

let divider = (~attrs=[], ()) =>
  div(~attrs=[clss(["menu-divider"])] @ attrs, []);
