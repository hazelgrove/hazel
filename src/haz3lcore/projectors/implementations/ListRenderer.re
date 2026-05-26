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

/* Each menu item is described as a `RendererTransforms.transform`,
 * a function-valued Exp that gets composed into `xs |> t` via
 * `apply_transforms`. The argument shapes match Hazel's stdlib
 * (`BuiltinsList.re`): `map(xs, f)`, `filter(xs, p)`, `sort(cmp, xs)`,
 * and the single-arg `reverse`/`length`/`head`/`tail`. */
let build_menu =
    (
      info: info,
      local: action => Ui_effect.t(unit),
      parent: external_action => Ui_effect.t(unit),
    )
    : menu_data => {
  let apply = (ts: list(RendererTransforms.transform)): Ui_effect.t(unit) =>
    switch (RendererTransforms.to_segment(info, ts)) {
    | Some(seg) => Effect.Many([local(CloseMenu), parent(SetSyntax(seg))])
    | None => local(CloseMenu)
    };
  let leaf = (~tooltip, label, action) =>
    Menu.action_item(~tooltip, ~on_hover=true, label, action);
  /* `xs |> name(?)` — single list argument. */
  let single = name =>
    RendererTransforms.Listwise(IdTagged.FreshGrammar.Exp.var(name));
  /* `xs |> map(?, fun x -> ?)` / `xs |> filter(?, fun x -> ?)`.
   * Deferral(InAp) is the `?` that the `|>` fills with `xs`, putting
   * the list in the first arg position to match `map(xs, f)`. */
  let row_fn = name =>
    RendererTransforms.Listwise(
      IdTagged.FreshGrammar.(
        Exp.(
          deferred_ap(
            var(name),
            [deferral(InAp), fn(Pat.var("x"), empty_hole(), None, None)],
          )
        )
      ),
    );
  /* `xs |> sort(fun (a, b) -> ?, ?)` — comparator-first to match
   * `sort(cmp, xs)`. Body is a hole because, unlike a typed table
   * column, we don't know the element type generically. */
  let sort_t =
    RendererTransforms.Listwise(
      IdTagged.FreshGrammar.(
        Exp.(
          deferred_ap(
            var("sort"),
            [
              fn(
                Pat.tuple([Pat.var("a"), Pat.var("b")]),
                empty_hole(),
                None,
                None,
              ),
              deferral(InAp),
            ],
          )
        )
      ),
    );
  [
    leaf(~tooltip="Wrap with reverse(xs)", "Reverse", () =>
      apply([single("reverse")])
    ),
    leaf(
      ~tooltip="Wrap with sort(cmp, xs) — fill in the comparator",
      "Sort…",
      () =>
      apply([sort_t])
    ),
    leaf(~tooltip="Wrap with length(xs)", "Length", () =>
      apply([single("length")])
    ),
    Menu.divider,
    leaf(~tooltip="Wrap with map(xs, f) — fill in f", "Map…", () =>
      apply([row_fn("map")])
    ),
    leaf(~tooltip="Wrap with filter(xs, p) — fill in p", "Filter…", () =>
      apply([row_fn("filter")])
    ),
    Menu.divider,
    leaf(~tooltip="Wrap with head(xs)", "Head", () =>
      apply([single("head")])
    ),
    leaf(~tooltip="Wrap with tail(xs)", "Tail", () =>
      apply([single("tail")])
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
    RendererMenu.menu_trigger_button(
      ~id=menu_button_id, ~title="List options", ~on_click=() =>
      local(ToggleMenu)
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
  RendererMenu.sync_listener(
    ~menu_open,
    ~on_close=local(CloseMenu),
    ~items=menu_items,
    ~inject_menu_action=a => local(MenuAction(a)),
    ~menu_state=model.menu_state,
  );

  let menu_node =
    if (menu_open) {
      RendererMenu.floating_menu_node(
        ~menu_button_id,
        ~menu_state=model.menu_state,
        ~items=menu_items,
        ~inject_menu_action=a => local(MenuAction(a)),
        ~extra_classes=["list-menu"],
        (),
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
