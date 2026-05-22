open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* ListRenderer - Visualize a list value as a numbered, vertical list with a
 * small toolbar of list-wide actions (currently: Reverse). Serves as the
 * second reference renderer alongside TableRenderer. */

[@deriving (show({with_path: false}), sexp, yojson)]
type v = list(Exp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type m = {
  selected: option(int),
  menu_state: Menu.t,
};
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | Select(option(int))
  | ToggleMenu
  | CloseMenu
  | MenuAction(Menu.action);

[@deriving (show({with_path: false}), sexp, yojson)]
type model = m;
[@deriving (show({with_path: false}), sexp, yojson)]
type action = a;
[@deriving (show({with_path: false}), sexp, yojson)]
type value = v;

let parse = (_sort: Sort.t, exp: Exp.t): option(value) =>
  switch (exp.term) {
  | ListLit(es) => Some(es)
  | _ => None
  };

let init = (_: value): model => {
  selected: None,
  menu_state: Menu.closed,
};

/* One row for the header, one per list item. */
let placeholder = (value: value, _: m): ProjectorCore.Shape.t =>
  ProjectorCore.Shape.{
    vertical: Block(1 + List.length(value)),
    horizontal: 0,
  };

let update = (model: model, action: action): model =>
  switch (action) {
  | Select(s) => {
      ...model,
      selected: s,
    }
  | ToggleMenu => {
      ...model,
      menu_state: Menu.update(Toggle, model.menu_state),
    }
  | CloseMenu => {
      ...model,
      menu_state: Menu.closed,
    }
  | MenuAction(a) => {
      ...model,
      menu_state: Menu.update(a, model.menu_state),
    }
  };

/* Rewrite the projected expression `xs` to `f(xs)` for a simple
 * single-arg wrapper, or to `g(?, xs)` when an extra hole-filled arg
 * is needed (e.g. `map(?, xs)` for the user's function). */
let wrap_call =
    (info: info, fname: string, ~hole_first: bool): option(Base.segment) =>
  info.utility.lift_syntax(
    ~inline=false,
    fun
    | Exp(exp) =>
      IdTagged.FreshGrammar.(
        Exp(
          hole_first
            ? Exp.ap(
                Forward,
                Exp.var(fname),
                Exp.tuple([Exp.empty_hole(), exp]),
              )
            : Exp.ap(Forward, Exp.var(fname), exp),
        )
      )
    | other => other,
    info.syntax,
  );

let item_view =
    (
      ~utility: utility,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~selected: bool,
      ~on_click,
      idx: int,
      exp: Exp.t,
    )
    : Node.t => {
  let (seg, _) = ProbeUtil.seg_of_exp(utility, exp);
  Node.div(
    ~attrs=[
      Attr.classes(["list-item"] @ (selected ? ["selected"] : [])),
      Attr.on_click(on_click),
    ],
    [
      Node.span(
        ~attrs=[Attr.classes(["list-index"])],
        [Node.text(string_of_int(idx))],
      ),
      Node.div(
        ~attrs=[Attr.classes(["list-value"])],
        [view_seg(Sort.Exp, seg)],
      ),
    ],
  );
};

type menu_data = list(Menu.item(unit => Ui_effect.t(unit)));

let build_menu =
    (
      info: info,
      local: action => Ui_effect.t(unit),
      parent: external_action => Ui_effect.t(unit),
    )
    : menu_data => {
  let apply = (seg_opt: option(Base.segment)): Ui_effect.t(unit) =>
    switch (seg_opt) {
    | Some(seg) => Effect.Many([local(CloseMenu), parent(SetSyntax(seg))])
    | None => local(CloseMenu)
    };
  let leaf = (~tooltip, label, action) =>
    Menu.action_item(~tooltip, ~on_hover=true, label, action);
  let call = (name, ~hole_first) =>
    apply(wrap_call(info, name, ~hole_first));
  [
    leaf(~tooltip="Wrap with reverse(xs)", "Reverse", () =>
      call("reverse", ~hole_first=false)
    ),
    leaf(~tooltip="Wrap with sort(xs)", "Sort", () =>
      call("sort", ~hole_first=false)
    ),
    leaf(~tooltip="Wrap with length(xs)", "Length", () =>
      call("length", ~hole_first=false)
    ),
    Menu.divider,
    leaf(~tooltip="Wrap with map(f, xs) — fill in f", "Map…", () =>
      call("map", ~hole_first=true)
    ),
    leaf(~tooltip="Wrap with filter(p, xs) — fill in p", "Filter…", () =>
      call("filter", ~hole_first=true)
    ),
    Menu.divider,
    leaf(~tooltip="Wrap with head(xs)", "Head", () =>
      call("head", ~hole_first=false)
    ),
    leaf(~tooltip="Wrap with tail(xs)", "Tail", () =>
      call("tail", ~hole_first=false)
    ),
  ];
};

let menu_button_id = "list-menu-button";

let toolbar =
    (
      ~is_readonly: bool,
      ~menu_open: bool,
      ~local: action => Ui_effect.t(unit),
    )
    : Node.t => {
  let menu_button =
    Node.div(
      ~attrs=[
        Attr.id(menu_button_id),
        Attr.classes(["icon", "closure-nav-button", "menu-trigger"]),
        Attr.on_click(_ => local(ToggleMenu)),
        Attr.title("List options"),
      ],
      [Node.text("⋮")],
    );
  Node.div(
    ~attrs=[
      Attr.classes(["list-toolbar"] @ (menu_open ? ["menu-open"] : [])),
    ],
    is_readonly ? [] : [menu_button],
  );
};

let render =
    (
      ~info: info,
      ~exp as _: Exp.t,
      ~value: value,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~model: model,
      ~local: action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      ~sort: Sort.t,
      _: unit,
    )
    : Node.t => {
  let is_readonly = sort != Sort.Exp;
  let menu_open = Menu.is_open(model.menu_state) && !is_readonly;
  let items =
    List.mapi(
      (i, e) =>
        item_view(
          ~utility=info.utility,
          ~view_seg,
          ~selected=model.selected == Some(i),
          ~on_click=
            _ => local(Select(model.selected == Some(i) ? None : Some(i))),
          i,
          e,
        ),
      value,
    );
  let count =
    Node.span(
      ~attrs=[Attr.classes(["list-count"])],
      [
        Node.text(
          string_of_int(List.length(value))
          ++ (List.length(value) == 1 ? " item" : " items"),
        ),
      ],
    );

  /* Keyboard / click-outside listener (reuses the table column-menu
   * machinery via the shared "column-menu" CSS class). */
  let menu_items = build_menu(info, local, parent);
  let handle_key = (key: string): option(Ui_effect.t(unit)) =>
    Menu.key_dispatcher(
      ~items=menu_items,
      ~dispatch_menu=a => local(MenuAction(a)),
      ~dispatch_action=thunk => thunk(),
      model.menu_state,
      key,
    );
  ColumnMenuListener.sync(
    ~menu_open,
    ~on_close=local(CloseMenu),
    ~handle_key,
    (),
  );

  let menu_node =
    if (menu_open) {
      let dir =
        Menu.direction_from_id(
          ~menu_height=200.0,
          ~menu_width=180.0,
          menu_button_id,
        );
      let dir_class =
        switch (dir) {
        | {vertical: `Down, horizontal: `Right} => "cm-down-right"
        | {vertical: `Down, horizontal: `Left} => "cm-down-left"
        | {vertical: `Up, horizontal: `Right} => "cm-up-right"
        | {vertical: `Up, horizontal: `Left} => "cm-up-left"
        };
      let menu_nodes =
        Menu.render(
          ~inject_action=thunk => thunk(),
          ~inject_menu=a => local(MenuAction(a)),
          ~item_class="named-menu-item",
          ~items=menu_items,
          model.menu_state,
        );
      Node.div(
        ~attrs=[
          Attr.classes([
            "context-menu",
            "column-menu",
            "list-menu",
            dir_class,
          ]),
        ],
        [WebUtil.div_c("group", [WebUtil.div_c("contents", menu_nodes)])],
      );
    } else {
      Node.none;
    };

  Node.div(
    ~attrs=[Attr.classes(["list-renderer"])],
    [
      Node.div(
        ~attrs=[Attr.classes(["list-header"])],
        [count, toolbar(~is_readonly, ~menu_open, ~local), menu_node],
      ),
      Node.div(~attrs=[Attr.classes(["list-items"])], items),
    ],
  );
};

let icon_size = 20.;

let list_icon =
  Node.create_svg(
    "svg",
    ~attrs=
      Attr.[
        create("viewBox", "0 0 8 8"),
        create("width", Printf.sprintf("%fpx", icon_size)),
        create("height", Printf.sprintf("%fpx", icon_size)),
        create("preserveAspectRatio", "none"),
      ],
    [
      Node.create_svg(
        "circle",
        ~attrs=
          Attr.[
            create("cx", "1.5"),
            create("cy", "2"),
            create("r", "0.5"),
          ],
        [],
      ),
      Node.create_svg(
        "circle",
        ~attrs=
          Attr.[
            create("cx", "1.5"),
            create("cy", "4"),
            create("r", "0.5"),
          ],
        [],
      ),
      Node.create_svg(
        "circle",
        ~attrs=
          Attr.[
            create("cx", "1.5"),
            create("cy", "6"),
            create("r", "0.5"),
          ],
        [],
      ),
      Node.create_svg(
        "rect",
        ~attrs=
          Attr.[
            create("x", "3"),
            create("y", "1.7"),
            create("width", "4"),
            create("height", "0.6"),
          ],
        [],
      ),
      Node.create_svg(
        "rect",
        ~attrs=
          Attr.[
            create("x", "3"),
            create("y", "3.7"),
            create("width", "4"),
            create("height", "0.6"),
          ],
        [],
      ),
      Node.create_svg(
        "rect",
        ~attrs=
          Attr.[
            create("x", "3"),
            create("y", "5.7"),
            create("width", "4"),
            create("height", "0.6"),
          ],
        [],
      ),
    ],
  );

let badge =
  Node.span(
    ~attrs=[
      Attr.classes(["list-badge"]),
      Attr.title("Click to view as list"),
    ],
    [list_icon],
  );
