open Util;
open Language;
open TableCore;

/* TableRenderer - A reusable module for rendering interactive tables with column operations.
   This is the logic half (parsing, menu state, serialization); the Vdom
   rendering half (column menus, table view, badge) lives in
   src/web/projectors/TableRendererView.re. */

[@deriving (show({with_path: false}), sexp, yojson)]
type v = (list(option(string)), list(list(Exp.t))); /* (headers, rows) */

/* Open column menu: column index + Menu state (path + selected_idx).
 * The Menu module owns the inner state — projector code never reads it. */
[@deriving (show({with_path: false}), sexp, yojson)]
type menu_state = option((int, Menu.t));
[@deriving (show({with_path: false}), sexp, yojson)]
type m = {menu_state};
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | CloseMenu
  | ShowMenu(int)
  | MenuAction(Menu.action);

[@deriving (show({with_path: false}), sexp, yojson)]
type model = m;

/* Table actions that can be performed on columns */
[@deriving (show({with_path: false}), sexp, yojson)]
type action = a;

[@deriving (show({with_path: false}), sexp, yojson)]
type value = v;

/* Parse an expression into table structure */
let parse = (_sort: Sort.t, exp: Exp.t) => parse_table(exp);

/* Initialize table model from parsed value */
let empty = {menu_state: None};
let init = (_: v) => empty;

let menu_col = (st: menu_state): option(int) =>
  Option.map(((c, _)) => c, st);

let update: (model, action) => model =
  (model, action) => {
    switch (action) {
    | CloseMenu => {menu_state: None}
    | ShowMenu(i) when Some(i) == menu_col(model.menu_state) => {
        menu_state: None,
      }
    | ShowMenu(i) => {menu_state: Some((i, Menu.opened))}
    | MenuAction(a) =>
      switch (model.menu_state) {
      | Some((col, menu_t)) =>
        let new_menu = Menu.update(a, menu_t);
        switch (new_menu) {
        | None => {menu_state: None}
        | Some(_) => {menu_state: Some((col, new_menu))}
        };
      | None => model
      }
    };
  };
