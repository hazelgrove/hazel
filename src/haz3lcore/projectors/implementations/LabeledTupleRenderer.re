open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* LabeledTupleRenderer - View a labeled tuple as a key/value card with
 * per-field actions (Extract, Drop, Rename). */

[@deriving (show({with_path: false}), sexp, yojson)]
type v = list((option(string), Exp.t));

[@deriving (show({with_path: false}), sexp, yojson)]
type menu_state = option((string, Menu.t));
[@deriving (show({with_path: false}), sexp, yojson)]
type m = {menu_state};
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | CloseMenu
  | ShowMenu(string)
  | MenuAction(Menu.action);

[@deriving (show({with_path: false}), sexp, yojson)]
type model = m;
[@deriving (show({with_path: false}), sexp, yojson)]
type action = a;
[@deriving (show({with_path: false}), sexp, yojson)]
type value = v;

/* Pull a (label?, value) out of a tuple entry, peeling Parens. Labeled
 * entries return `Some(label)`; bare entries return `None`. */
let rec extract_entry = (e: Exp.t): (option(string), Exp.t) =>
  switch (e.term) {
  | Parens(inner) => extract_entry(inner)
  | TupLabel({term: Label(l), _}, v) => (Some(l), v)
  | _ => (None, e)
  };

let rec strip_parens = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(inner) => strip_parens(inner)
  | _ => e
  };

/* Accept any tuple, labeled or unlabeled. We require at least one labeled
 * entry to justify the "labeled tuple" framing — otherwise the user gets
 * no operations and the modal is purely cosmetic. */
let parse = (_sort: Sort.t, exp: Exp.t): option(value) => {
  let exp = strip_parens(exp);
  let entries =
    switch (exp.term) {
    | Tuple(es) when List.length(es) > 0 =>
      Some(List.map(extract_entry, es))
    | _ =>
      switch (extract_entry(exp)) {
      | (Some(_), _) as p => Some([p])
      | _ => None
      }
    };
  switch (entries) {
  | Some(es) when List.exists(((l, _)) => Option.is_some(l), es) =>
    Some(es)
  | _ => None
  };
};

let init = (_: value): model => {menu_state: None};

/* One row per labeled-tuple field. */
let placeholder = (value: value, _: model): ProjectorCore.Shape.t =>
  ProjectorCore.Shape.{
    vertical: Block(List.length(value)),
    horizontal: 0,
  };

let menu_field = (st: menu_state): option(string) => Option.map(fst, st);

let update = (model: model, action: action): model =>
  switch (action) {
  | CloseMenu => {menu_state: None}
  | ShowMenu(field) when menu_field(model.menu_state) == Some(field) => {
      menu_state: None,
    }
  | ShowMenu(field) => {menu_state: Some((field, Menu.opened))}
  | MenuAction(a) =>
    switch (model.menu_state) {
    | Some((f, menu_t)) =>
      let new_menu = Menu.update(a, menu_t);
      switch (new_menu) {
      | None => {menu_state: None}
      | Some(_) => {menu_state: Some((f, new_menu))}
      };
    | None => model
    }
  };

/* --- Syntax rewrites --- */

/* Rewrite the projected expression `r` to `f(r)`. */
let rewrite = (info: info, f: Exp.t => Exp.t): option(Base.segment) =>
  info.utility.lift_syntax(
    ~inline=false,
    fun
    | Exp(exp) => Exp(f(exp))
    | other => other,
    info.syntax,
  );

let drop_field = (info: info, field: string): option(Base.segment) =>
  rewrite(info, r =>
    IdTagged.FreshGrammar.(
      Exp.ap(
        Forward,
        Exp.var("omit_labels"),
        Exp.tuple([r, Exp.label(field)]),
      )
    )
  );

let extract_field = (info: info, field: string): option(Base.segment) =>
  rewrite(info, r => IdTagged.FreshGrammar.(Exp.dot(r, Exp.label(field))));

let rename_field =
    (info: info, old_name: string, new_name: string): option(Base.segment) =>
  rewrite(info, r =>
    IdTagged.FreshGrammar.(
      Exp.let_(
        Pat.var("r"),
        r,
        Exp.tuple_extension(
          Exp.ap(
            Forward,
            Exp.var("omit_labels"),
            Exp.tuple([Exp.var("r"), Exp.label(old_name)]),
          ),
          Exp.tuple([
            Exp.tup_label(
              Exp.label(new_name),
              Exp.dot(Exp.var("r"), Exp.label(old_name)),
            ),
          ]),
        ),
      )
    )
  );

/* --- Menu --- */

type menu_data = list(Menu.item(unit => Ui_effect.t(unit)));

let menu_for_field =
    (
      info: info,
      field: string,
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
  [
    leaf(
      ~tooltip="Replace this tuple with just this field's value", "Extract", () =>
      apply(extract_field(info, field))
    ),
    leaf(~tooltip="Remove this field from the tuple", "Drop", () =>
      apply(drop_field(info, field))
    ),
    leaf(~tooltip="Change this field's label", "Rename…", () =>
      switch (JsUtil.prompt("New field name:", field)) {
      | None => local(CloseMenu)
      | Some(new_name) when new_name == field => local(CloseMenu)
      | Some(new_name) => apply(rename_field(info, field, new_name))
      }
    ),
  ];
};

/* --- View --- */

let icon_button = (~tooltip, ~on_click, text) =>
  Node.div(
    ~attrs=[
      Attr.classes(["icon", "closure-nav-button"]),
      Attr.on_click(on_click),
      Attr.title(tooltip),
    ],
    [Node.text(text)],
  );

let field_row =
    (
      ~info: info,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~is_readonly: bool,
      ~menu_state: menu_state,
      ~local: action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      ~position: int,
      (label_opt, value): (option(string), Exp.t),
    )
    : Node.t => {
  let (seg, _) = ProbeUtil.seg_of_exp(info.utility, value);
  /* Unlabeled entries have no key in the menu state, so they never open
   * a menu and don't get an action button. */
  let menu_open =
    switch (label_opt, menu_state) {
    | (Some(label), Some((f, _))) => f == label
    | _ => false
    };
  let menu_node =
    switch (label_opt, menu_state) {
    | (Some(field), Some((f, menu_t))) when f == field && !is_readonly =>
      let items = menu_for_field(info, field, local, parent);
      let menu_nodes =
        Menu.render(
          ~inject_action=thunk => thunk(),
          ~inject_menu=a => local(MenuAction(a)),
          ~item_class="named-menu-item",
          ~items,
          menu_t,
        );
      [
        Node.div(
          ~attrs=[
            Attr.classes([
              "context-menu",
              "nut-menu",
              "column-menu",
              "labeled-tuple-menu",
            ]),
          ],
          [
            WebUtil.div_c("group", [WebUtil.div_c("contents", menu_nodes)]),
          ],
        ),
      ];
    | _ => []
    };
  let menu_button =
    switch (label_opt) {
    | Some(label) when !is_readonly =>
      icon_button(
        ~tooltip="Field options",
        ~on_click=_ => local(ShowMenu(label)),
        "⋮",
      )
    | _ => Node.none
    };
  let (label_text, label_classes) =
    switch (label_opt) {
    | Some(name) => (name, ["labeled-tuple-label"])
    | None => (
        "_" ++ string_of_int(position),
        ["labeled-tuple-label", "unlabeled"],
      )
    };
  Node.div(
    ~attrs=[
      Attr.classes(
        ["labeled-tuple-row"]
        @ (menu_open ? ["menu-open"] : [])
        @ (Option.is_none(label_opt) ? ["unlabeled"] : []),
      ),
    ],
    [
      Node.div(
        ~attrs=[Attr.classes(label_classes)],
        [Node.text(label_text)],
      ),
      Node.div(
        ~attrs=[Attr.classes(["labeled-tuple-value"])],
        [view_seg(Sort.Exp, seg)],
      ),
      menu_button,
      ...menu_node,
    ],
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
  let rows =
    List.mapi(
      (i, entry) =>
        field_row(
          ~info,
          ~view_seg,
          ~is_readonly,
          ~menu_state=model.menu_state,
          ~local,
          ~parent,
          ~position=i,
          entry,
        ),
      value,
    );

  /* Keyboard / click-outside listener (reuses the table column-menu
   * machinery via the shared "column-menu" CSS class). */
  let handle_key = (key: string): option(Ui_effect.t(unit)) =>
    switch (model.menu_state) {
    | None => None
    | Some((f, menu_t)) =>
      let items = menu_for_field(info, f, local, parent);
      Menu.key_dispatcher(
        ~items,
        ~dispatch_menu=a => local(MenuAction(a)),
        ~dispatch_action=thunk => thunk(),
        menu_t,
        key,
      );
    };
  ColumnMenuListener.sync(
    ~menu_open=model.menu_state != None,
    ~on_close=local(CloseMenu),
    ~handle_key,
    (),
  );

  Node.div(~attrs=[Attr.classes(["labeled-tuple-renderer"])], rows);
};

/* Small braces icon. */
let icon_size = 20.;
let labeled_tuple_icon =
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
        "path",
        ~attrs=
          Attr.[
            create(
              "d",
              "M2.4 1.2 C 1.6 1.2 1.4 1.6 1.4 2.2 L 1.4 3.6 C 1.4 4 1.2 4 1 4 L 1 4.4 C 1.2 4.4 1.4 4.5 1.4 4.9 L 1.4 6.2 C 1.4 6.8 1.6 7.2 2.4 7.2",
            ),
            create("fill", "none"),
            create("stroke", "currentColor"),
            create("stroke-width", "0.4"),
          ],
        [],
      ),
      Node.create_svg(
        "path",
        ~attrs=
          Attr.[
            create(
              "d",
              "M5.6 1.2 C 6.4 1.2 6.6 1.6 6.6 2.2 L 6.6 3.6 C 6.6 4 6.8 4 7 4 L 7 4.4 C 6.8 4.4 6.6 4.5 6.6 4.9 L 6.6 6.2 C 6.6 6.8 6.4 7.2 5.6 7.2",
            ),
            create("fill", "none"),
            create("stroke", "currentColor"),
            create("stroke-width", "0.4"),
          ],
        [],
      ),
    ],
  );
let badge =
  Node.span(
    ~attrs=[
      Attr.classes(["labeled-tuple-badge"]),
      Attr.title("Click to view as labeled tuple"),
    ],
    [labeled_tuple_icon],
  );
