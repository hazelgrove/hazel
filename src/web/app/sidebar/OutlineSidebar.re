open Virtual_dom.Vdom;
open Node;

/* the collapsible module/definition outline. a click jumps; the cell
   button toggles a definition in the focus stack (stacked cells replace
   the master editor), and with a stack open a click adds or moves to that
   cell. without a stack (~stack_controls=false) it only navigates, and
   collapse is the native <details> toggle */

let clss = cs => Attr.classes(cs);

module TestStatus = Language.TestStatus;

/* the header row: the program and the zoom trail as a breadcrumb, and
   how many pins the current level shows or keeps parked */
type header = {
  h_program: string,
  h_trail: list((Language.Id.t, string)),
  h_open: int,
  h_parked: int,
};

let kind_glyph = (k: OutlineTree.kind): string =>
  switch (k) {
  | KModule => {js|⛁|js}
  | KFn => {js|ƒ|js}
  | KConst => {js|·|js}
  | KType => {js|τ|js}
  | KTest
  | KTests => {js|◦|js} /* overridden by the test's live status */
  | KStmt => {js|;|js}
  | KTrail => {js|⇒|js}
  };

let kind_cls = (k: OutlineTree.kind): string =>
  switch (k) {
  | KModule => "ol-module"
  | KFn => "ol-fn"
  | KConst => "ol-const"
  | KType => "ol-type"
  | KTest => "ol-test"
  | KTests => "ol-tests"
  | KStmt => "ol-stmt"
  | KTrail => "ol-trail"
  };

type edit_ctl = {
  current: option(OutlineEdit.t),
  /* read at the keypress: keys can outrun renders */
  get: unit => option(OutlineEdit.t),
  set: option(OutlineEdit.t) => Effect.t(unit),
  /* save, the cursor going as [after] says */
  commit: (OutlineEdit.t, OutlineEdit.after) => Effect.t(unit),
};

/* what the outline shows */
type props = {
  /* rows fold from an arrow; without, from their sigil */
  arrows: bool,
  /* pins, zoom and item edits: only where a slide has cells */
  stack_controls: bool,
  can_open: Language.Id.t => bool,
  is_collapsed: OutlineTree.path => bool,
  header,
  zoom_root: option(Language.Id.t),
  cursor: option(OutlineTree.path),
  created: option((Language.Id.t, string)),
  /* every pin, shown or not: a hidden one gets a quiet mark */
  pinned: list(Language.Id.t),
  focused_entries: list((Language.Id.t, option(string))),
  error_items: list(Language.Id.t),
  error_subtree: list(Language.Id.t),
  menu: option((Language.Id.t, bool, float, float)),
  /* the menu row the keys have selected */
  menu_sel: int,
  test_status: Language.Id.t => option(TestStatus.t),
};

/* what the outline asks for */
type handlers = {
  jump: Language.Id.t => Effect.t(unit),
  focus: Language.Id.t => Effect.t(unit),
  toggle: Language.Id.t => Effect.t(unit),
  toggle_run: Language.Id.t => Effect.t(unit),
  toggle_collapse: OutlineTree.path => Effect.t(unit),
  zoom_to: option(Language.Id.t) => Effect.t(unit),
  zoom_in: Language.Id.t => Effect.t(unit),
  show_whole: bool => Effect.t(unit),
  discard: Effect.t(unit),
  zoom_out: Effect.t(unit),
  get_cursor: unit => option(OutlineTree.path),
  set_cursor: option(OutlineTree.path) => Effect.t(unit),
  focused: Effect.t(unit),
  leave: Effect.t(unit),
  edit: edit_ctl,
  menu_open: (Language.Id.t, bool, float, float) => Effect.t(unit),
  menu_close: Effect.t(unit),
  menu_select: int => Effect.t(unit),
  get_menu: unit => option((Language.Id.t, bool, float, float)),
  get_menu_sel: unit => int,
  def_op: (OutlineEdit.def_op, Language.Id.t) => Effect.t(unit),
};

/* the typed text with its caret; a new definition's keyword colored */
let edit_text = (ed: OutlineEdit.t): list(Node.t) => {
  let t = ed.ed_text;
  let n = String.length(t);
  let c = max(0, min(ed.ed_caret, n));
  let p =
    ed.ed_row == None ? String.length(snd(OutlineEdit.new_kind(t))) : 0;
  let piece = (a, b) => text(String.sub(t, a, b - a));
  let caret = span(~attrs=[clss(["outline-edit-caret"])], []);
  (
    p > 0
      ? [
        span(
          ~attrs=[clss(["outline-edit-kw"])],
          c <= p ? [piece(0, c), caret, piece(c, p)] : [piece(0, p)],
        ),
      ]
      : []
  )
  @ (c > p || p == 0 ? [piece(p, c), caret, piece(c, n)] : [piece(p, n)]);
};

let edit_hint = (ed: OutlineEdit.t): list(Node.t) =>
  switch (ed.ed_error) {
  | Some(why) => [div(~attrs=[clss(["outline-edit-hint"])], [text(why)])]
  | None => []
  };

/* the row a new definition is typed into, below its anchor */
let new_row_view = (ed: OutlineEdit.t): Node.t => {
  let (k, _) = OutlineEdit.new_kind(ed.ed_text);
  div(
    ~attrs=[clss(["outline-leaf"])],
    [
      div(
        ~attrs=[
          clss([
            "outline-label",
            "outline-editing",
            "outline-new",
            kind_cls(k),
          ]),
        ],
        [
          span(~attrs=[clss(["outline-glyph"])], [text(kind_glyph(k))]),
          span(~attrs=[clss(["outline-edit-text"])], edit_text(ed)),
        ],
      ),
    ]
    @ edit_hint(ed),
  );
};

/* live glyph + status class for test rows; the container joins its
   children's statuses (any fail => ✗) */
let test_glyph =
    (
      ~test_status: Language.Id.t => option(TestStatus.t),
      n: OutlineTree.node,
    )
    : option((string, string)) => {
  let of_status = (s: TestStatus.t) =>
    switch (s) {
    | Pass => ({js|✓|js}, "ol-pass")
    | Fail => ({js|✗|js}, "ol-fail")
    | Indet => ({js|?|js}, "ol-indet")
    };
  switch (n.o_kind) {
  | KTest => Option.map(of_status, Option.bind(n.o_test, test_status))
  | KTests =>
    let sts =
      List.filter_map(
        (c: OutlineTree.node) => Option.bind(c.o_test, test_status),
        n.o_children,
      );
    sts == [] ? None : Some(of_status(TestStatus.join_all(sts)));
  | _ => None
  };
};

/* drag-to-resize: pointerdown attaches DOCUMENT-level move/up
   listeners (pointer events — canceled pointerdown suppresses compat
   mouseup); width rides a ROOT css variable */
let resize_attrs: list(Attr.t) = [
  Attr.on_pointerdown(_ => {
    let listen = (name, f) =>
      Js_of_ocaml.(
        Dom_html.addEventListener(
          Dom_html.document,
          Dom.Event.make(name),
          Dom_html.handler(e => {
            f(e);
            Js._true;
          }),
          Js._false,
        )
      );
    let move =
      listen("pointermove", e =>
        Util.JsUtil.set_css_custom_property(
          "--outline-w",
          string_of_int(
            max(140, min(420, Js_of_ocaml.Js.Unsafe.coerce(e)##.clientX)),
          )
          ++ "px",
        )
      );
    let up = ref(Option.none);
    up :=
      Some(
        listen("pointerup", _ => {
          Js_of_ocaml.Dom_html.removeEventListener(move);
          Option.iter(Js_of_ocaml.Dom_html.removeEventListener, up^);
        }),
      );
    Effect.Prevent_default;
  }),
];

/* the keyboard to the outline: a click anywhere in it takes focus */
let take_focus =
  Effect.of_sync_fun(
    () =>
      if (!Util.JsUtil.outline_has_focus()) {
        Util.JsUtil.focus_outline();
      },
    (),
  );

/* the cursor to [c], with the keys: after the focus event's own cursor
   (the caret's row), so set at the event's end */
let take_cursor = (on: handlers, c: option(OutlineTree.path)) =>
  Effect.Many([
    Effect.Prevent_default,
    take_focus,
    Effect.lazy_(lazy(on.set_cursor(c))),
  ]);

/* this event only: no default action, no other listeners */
let stop = eff =>
  Effect.Many([Effect.Prevent_default, Effect.Stop_propagation, eff]);

let in_stack = (props: props, id: Language.Id.t): bool =>
  List.mem_assoc(id, props.focused_entries);

/* an open cell's row shows its header's live text */
let live_label = (props: props, n: OutlineTree.node): OutlineTree.node =>
  switch (
    Option.bind(n.o_id, id => List.assoc_opt(id, props.focused_entries))
  ) {
  | Some(Some(live)) => {
      ...n,
      o_label: live,
    }
  | _ => n
  };

/* a row's click: with a stack open, add or move to its cell (a jump would
   target the hidden master) */
let show = (props: props, on: handlers, id: Language.Id.t): Effect.t(unit) =>
  props.focused_entries != [] ? on.focus(id) : on.jump(id);

/* the cell button's act: open or close the row's cell; a tests container
   opens its whole run as one cell, at any depth. None: nothing to open */
let open_close =
    (props: props, on: handlers, n: OutlineTree.node)
    : option(unit => Effect.t(unit)) =>
  switch (n.o_id, n.o_kind) {
  | (Some(id), _) when in_stack(props, id) || props.can_open(id) =>
    Some(() => on.toggle(id))
  | (None, KTests) =>
    switch (List.filter_map((c: OutlineTree.node) => c.o_id, n.o_children)) {
    | [first, ..._] => Some(() => on.toggle_run(first))
    | [] => None
    }
  | _ => None
  };

let rec node_view =
        (
          ~props: props,
          ~on: handlers,
          ~path: OutlineTree.path,
          ~seg: OutlineTree.path_seg, /* this row's name among its siblings */
          n: OutlineTree.node,
        )
        : Node.t => {
  let editing =
    switch (on.edit.current) {
    | Some(ed) when ed.ed_row != None && ed.ed_row == n.o_id => Some(ed)
    | _ => None
    };
  let status = test_glyph(~test_status=props.test_status, n);
  /* a subtree's error: badge shown by CSS only while COLLAPSED (the
     deepest visible row owns the error otherwise) */
  let (has_err, has_roll_err, stacked) =
    switch (n.o_id) {
    | Some(id) =>
      let has_err = List.mem(id, props.error_items);
      (
        has_err,
        !has_err && List.mem(id, props.error_subtree),
        in_stack(props, id),
      );
    | None => (false, false, false)
    };
  /* the focused row's name tracks its header editor LIVE */
  let n = live_label(props, n);
  /* paths name the row by its outline label, not the live header text
     (`inc(x)` for `inc`): collapse state and the cursor survive pinning */
  let row_path = path @ [seg];
  let badge = (cls, title) =>
    span(
      ~attrs=[clss(["outline-err-badge", ...cls]), Attr.title(title)],
      [text({js|●|js})],
    );
  let cell_button = (~cls=[], ~title, act) =>
    span(
      ~attrs=
        [clss(["outline-focus-btn", ...cls]), Attr.title(title)]
        @ (
          switch (act) {
          | Some(act) => [Attr.on_click(_ => stop(act()))]
          | None => []
          }
        ),
      [],
    );
  let label =
    div(
      ~attrs=
        [
          clss(
            ["outline-label", kind_cls(n.o_kind)]
            @ (props.cursor == Some(row_path) ? ["outline-cursor"] : [])
            @ (editing != None ? ["outline-editing"] : [])
            @ (stacked ? ["outline-focused"] : [])
            @ (has_err ? ["outline-has-err"] : [])
            @ (
              switch (status) {
              | Some((_, cls)) => [cls]
              | None => []
              }
            ),
          ),
          /* a click takes the keyboard to the outline, at this row: focus
             first, so the focus event's cursor lands before this one.
             without a stack there are no keys, so no cursor */
          Attr.on_mousedown(_ =>
            switch (on.edit.get()) {
            | Some(ed) when ed.ed_row == None || ed.ed_row != n.o_id =>
              /* clicking away from a name saves it */
              on.edit.commit(ed, MoveTo(Some(row_path)))
            | Some(_) => Effect.Ignore
            | None =>
              props.stack_controls
                ? take_cursor(on, Some(row_path))
                : Effect.Many([Effect.Prevent_default, take_focus])
            }
          ),
        ]
        @ (
          switch (n.o_id) {
          | Some(id) =>
            [
              Attr.create("data-ol-id", Language.Id.to_string(id)),
              /* Prevent_default: label clicks must not toggle the row's
                 <details> */
              Attr.on_click(_ => stop(show(props, on, id))),
            ]
            /* every block level gets the menu, except trailing-expression
               rows */
            @ (
              props.stack_controls && n.o_kind != KTrail
                ? [
                  Attr.on_contextmenu(evt =>
                    Effect.Many([
                      Effect.Prevent_default,
                      on.menu_open(
                        id,
                        n.o_kind == KModule,
                        float_of_int(evt##.clientX),
                        float_of_int(evt##.clientY),
                      ),
                    ])
                  ),
                ]
                : []
            )
          | None => []
          }
        ),
      [
        span(
          ~attrs=
            [clss(["outline-glyph"])]
            /* without arrows a branch's sigil folds it */
            @ (
              !props.arrows && n.o_children != []
                ? [
                  Attr.on_click(evt =>
                    stop(
                      props.stack_controls
                        ? on.toggle_collapse(row_path)
                        : Effect.of_sync_fun(
                            () => {
                              let details =
                                Js_of_ocaml.Js.Unsafe.meth_call(
                                  Js_of_ocaml.Js.Unsafe.coerce(evt)##.currentTarget,
                                  "closest",
                                  [|
                                    Js_of_ocaml.Js.Unsafe.inject(
                                      Js_of_ocaml.Js.string("details"),
                                    ),
                                  |],
                                );
                              Js_of_ocaml.Js.Unsafe.set(
                                details,
                                "open",
                                Js_of_ocaml.Js.bool(
                                  !
                                    Js_of_ocaml.Js.to_bool(
                                      Js_of_ocaml.Js.Unsafe.get(
                                        details,
                                        "open",
                                      ),
                                    ),
                                ),
                              );
                            },
                            (),
                          ),
                    )
                  ),
                ]
                : []
            ),
          [
            text(
              switch (status) {
              | Some((g, _)) => g
              | None => kind_glyph(n.o_kind)
              },
            ),
          ],
        ),
      ]
      @ (
        switch (editing, props.created) {
        | (Some(ed), _) => [
            span(~attrs=[clss(["outline-edit-text"])], edit_text(ed)),
          ]
        | (None, Some((id, kw))) when n.o_id == Some(id) => [
            /* the keyword typed to make it a type or module, leaving */
            span(~attrs=[clss(["outline-kw-leaving"])], [text(kw)]),
            text(n.o_label),
          ]
        /* rows with no name of their own get a word for what they are,
           set apart from real names */
        | _ when n.o_kind == KTrail && n.o_label == "" => [
            span(~attrs=[clss(["outline-synthetic"])], [text("result")]),
          ]
        | _ when n.o_kind == KTests => [
            span(~attrs=[clss(["outline-synthetic"])], [text(n.o_label)]),
          ]
        | _ => [text(n.o_label)]
        }
      )
      @ (has_err ? [badge([], "contains type errors")] : [])
      @ (
        has_roll_err
          ? [
            badge(["outline-err-roll"], "contains type errors (collapsed)"),
          ]
          : []
      )
      @ (
        switch (n.o_id) {
        | _ when !props.stack_controls => []
        | None when n.o_kind == KTests =>
          let kid_ids =
            List.filter_map((c: OutlineTree.node) => c.o_id, n.o_children);
          let all_pinned =
            kid_ids != [] && List.for_all(in_stack(props), kid_ids);
          [
            cell_button(
              ~cls=all_pinned ? ["outline-btn-on"] : [],
              ~title=
                all_pinned
                  ? "close the tests cell" : "open the tests as one cell",
              open_close(props, on, n),
            ),
          ];
        | Some(id) =>
          switch (open_close(props, on, n)) {
          | None => [
              cell_button(
                ~cls=["outline-btn-disabled"],
                ~title="finish the definition to open it",
                None,
              ),
            ]
          | act => [
              cell_button(
                ~cls=
                  stacked
                    ? ["outline-btn-on"]
                    : List.mem(id, props.pinned)
                        ? ["outline-btn-parked"] : [],
                ~title=
                  stacked ? "close this cell" : "open in the editor stack",
                act,
              ),
            ]
          }
        | None => []
        }
      ),
    );
  let hint =
    switch (editing) {
    | Some(ed) => edit_hint(ed)
    | None => []
    };
  /* a new member being named last inside this module: shown open, even
     when collapsed or empty */
  let inside_row =
    switch (on.edit.current) {
    | Some({ed_row: None, ed_anchor: Some(a), ed_inside: true, _} as ed)
        when n.o_id == Some(a) =>
      Some(new_row_view(ed))
    | _ => None
    };
  switch (n.o_children, inside_row) {
  | ([], None) => div(~attrs=[clss(["outline-leaf"])], [label, ...hint])
  | (kids, _) =>
    create(
      "details",
      ~attrs=
        [clss(["outline-branch"])]
        @ (
          props.is_collapsed(row_path) && Option.is_none(inside_row)
            ? [] : [Attr.create("open", "")]
        ),
      [
        create(
          "summary",
          ~attrs=
            [clss(["outline-summary"])]
            /* with stack controls collapse is MODEL state (per slide,
               persisted): the summary click dispatches the toggle and
               suppresses the native one */
            @ (
              props.stack_controls
                ? [
                  Attr.on_click(_ =>
                    Effect.Many([
                      Effect.Prevent_default,
                      on.toggle_collapse(row_path),
                    ])
                  ),
                  /* the chevron too: the summary would take focus
                     itself */
                  Attr.on_mousedown(_ =>
                    switch (on.edit.get()) {
                    | Some(_) => Effect.Ignore
                    | None => take_cursor(on, Some(row_path))
                    }
                  ),
                ]
                : []
            ),
          [label],
        ),
      ]
      @ hint
      @ [
        div(
          ~attrs=[clss(["outline-kids"])],
          rows_view(~props, ~on, ~path=row_path, kids)
          @ Option.to_list(inside_row),
        ),
      ],
    )
  };
}

/* sibling rows under [path], each followed by a new definition typed
   below it */
and rows_view =
    (~props: props, ~on: handlers, ~path: OutlineTree.path, ns): list(Node.t) =>
  List.concat_map(
    ((n: OutlineTree.node, seg)) =>
      [node_view(~props, ~on, ~path, ~seg, n)] @ new_row_after(on.edit, n),
    OutlineTree.segs(ns),
  )

/* a new definition typed below [n] */
and new_row_after = (edit: edit_ctl, n: OutlineTree.node): list(Node.t) =>
  switch (edit.current) {
  | Some({ed_row: None, ed_anchor: Some(a), ed_inside: false, _} as ed)
      when n.o_id == Some(a) => [
      new_row_view(ed),
    ]
  | _ => []
  };

/* the context menu's rows, as data: the menu draws them and the keys
   walk them. An item's act is a thunk: [edit.set] takes effect when
   called, so drawing the menu must not call it */
type menu_row =
  | Item(string, option(string), unit => Effect.t(unit)) /* label, shortcut, act */
  | Divider;

let menu_rows =
    (~on: handlers, ~is_module: bool, id: Language.Id.t): list(menu_row) => {
  let mac = Util.Os.is_mac^;
  let op = (~keys=?, o, label) => Item(label, keys, () => on.def_op(o, id));
  /* the name row the keyboard makes; [prefix] picks the kind */
  let new_row = (~inside=false, label, prefix) =>
    Item(
      label,
      None,
      () =>
        Effect.Many([
          take_focus,
          on.edit.set(Some(OutlineEdit.mk(~anchor=id, ~inside, prefix))),
        ]),
    );
  (
    is_module
      ? [
        Item(
          "Zoom in",
          Some(mac ? {js|⌥→|js} : {js|Alt+→|js}),
          () => on.zoom_in(id),
        ),
        new_row(~inside=true, "New definition inside", ""),
        Divider,
      ]
      : []
  )
  @ [
    new_row("New definition below", ""),
    new_row("New type below", "type "),
    new_row("New module below", "module "),
    Divider,
    op(~keys=mac ? {js|⌘D|js} : "Ctrl+D", Duplicate, "Duplicate"),
    op(~keys=mac ? {js|⌥↑|js} : {js|Alt+↑|js}, MoveUp, "Move up"),
    op(~keys=mac ? {js|⌥↓|js} : {js|Alt+↓|js}, MoveDown, "Move down"),
    Divider,
    op(~keys=mac ? {js|⌘⌫|js} : "Del", Delete, "Delete"),
  ];
};

/* the rows that act, in order: the keyboard's selection counts these */
let menu_items = (rows: list(menu_row)) =>
  List.filter_map(
    fun
    | Item(_, _, act) => Some(act)
    | Divider => None,
    rows,
  );

/* the right-click menu: the editor's context menu (its classes, flip,
   dividers and key chips), fixed at the click */
let menu_view =
    (
      ~menu_close: Effect.t(unit),
      ~rows: list(menu_row),
      ~selected: int,
      ~is_module: bool,
      (x: float, y: float),
    )
    : list(Node.t) => {
  let (_, rows) =
    List.fold_left_map(
      (i, r) =>
        switch (r) {
        | Item(label, keys, act) => (
            i + 1,
            div(
              ~attrs=[
                clss(
                  ["named-menu-item"] @ (i == selected ? ["selected"] : []),
                ),
                Attr.on_click(_ => Effect.Many([menu_close, act()])),
              ],
              [text(label)]
              @ (
                switch (keys) {
                | Some(k) => [
                    span(~attrs=[clss(["menu-shortcut"])], [text(k)]),
                  ]
                | None => []
                }
              ),
            ),
          )
        | Divider => (i, div(~attrs=[clss(["menu-divider"])], []))
        },
      0,
      rows,
    );
  let dir =
    Util.Menu.direction_of(
      ~menu_height=is_module ? 230. : 176.,
      ~menu_width=230.,
      Util.Menu.space_from(
        ~anchor_top=y,
        ~anchor_bot=y,
        ~anchor_left=x,
        ~anchor_right=x,
      ),
    );
  [
    div(
      ~attrs=[
        clss(["outline-menu-backdrop"]),
        /* the keys stay in the outline through the menu */
        Attr.on_mousedown(_ => Effect.Prevent_default),
        Attr.on_click(_ => menu_close),
        Attr.on_wheel(_ => menu_close),
        Attr.on_contextmenu(_ =>
          Effect.Many([Effect.Prevent_default, menu_close])
        ),
      ],
      [],
    ),
    div(
      ~attrs=[
        clss([
          "context-menu",
          "outline-menu",
          ContextMenu.direction_class(dir),
        ]),
        Attr.create(
          "style",
          Printf.sprintf("position: fixed; left: %.0fpx; top: %.0fpx;", x, y),
        ),
        Attr.on_mousedown(_ => Effect.Prevent_default),
      ],
      [
        div(
          ~attrs=[clss(["group"])],
          [div(~attrs=[clss(["contents"])], rows)],
        ),
      ],
    ),
  ];
};

type visible_row = {
  r_path: OutlineTree.path,
  /* its label may be an open cell's live header text */
  r_node: OutlineTree.node,
  /* the row's name in the program */
  r_name: string,
  r_parent: option(OutlineTree.path),
  r_expanded: bool,
};

/* the outline's keys: arrows move and fold, Page keys move a screenful,
   Enter shows, Space opens as a cell; Alt with arrows moves rows and
   zooms; ⌘D duplicates, ⌘⌫ deletes; Esc (or Alt+O) returns */
let keys =
    (~props: props, ~on: handlers, ~visible: array(visible_row), evt)
    : Effect.t(unit) => {
  let n = Array.length(visible);
  let idx =
    Option.bind(on.get_cursor(), p =>
      Array.find_index(r => r.r_path == p, visible)
    )
    |> Option.value(~default=-1);
  let target = (i): option(OutlineTree.path) =>
    i < 0 || n == 0 ? None : Some(visible[min(i, n - 1)].r_path);
  let go = i => on.set_cursor(target(i));
  let cur = idx >= 0 ? Some(visible[idx]) : None;
  let k = Util.Key.mk(KeyDown, evt);
  let key =
    switch (k.key) {
    | D(key)
    | U(key) => key
    };
  let (code, alt, shift) = (k.code, k.alt == Down, k.shift == Down);
  let meta = k.meta == Down || k.ctrl == Down;
  let id_of = (r: visible_row) => r.r_node.o_id;
  let is_module = (r: visible_row) => r.r_node.o_kind == OutlineTree.KModule;
  /* the context menu at the cursor's row, as the editor opens its own */
  let open_menu = (r: visible_row) =>
    Option.map(
      id => {
        let (x, y) =
          Option.value(
            Util.JsUtil.outline_cursor_anchor(),
            ~default=(0., 0.),
          );
        on.menu_open(id, is_module(r), x, y);
      },
      id_of(r),
    );
  let nav_key = () =>
    switch (key, alt, meta, cur) {
    | (".", false, true, Some(r)) => open_menu(r)
    | ("F10", false, false, Some(r)) when shift => open_menu(r)
    | ("Escape", _, _, _) => Some(on.leave)
    | _ when alt && code == "KeyO" => Some(on.leave)
    | ("ArrowDown", false, false, _) =>
      Some(idx + 1 < n ? go(idx + 1) : Effect.Ignore)
    | ("ArrowUp", false, false, _) =>
      Some(idx >= 0 ? go(idx - 1) : Effect.Ignore)
    | ("Home", false, false, _) => Some(go(-1))
    | ("End", false, false, _) => Some(go(n - 1))
    | ("PageDown", false, false, _) =>
      Some(go(min(n - 1, idx + Util.JsUtil.outline_page_rows())))
    | ("PageUp", false, false, _) =>
      Some(
        go(idx > 0 ? max(0, idx - Util.JsUtil.outline_page_rows()) : (-1)),
      )
    | ("ArrowRight", false, false, None) => Some(go(0))
    | ("ArrowRight", false, false, Some(r)) =>
      Some(
        r.r_node.o_children == []
          ? Effect.Ignore
          : r.r_expanded ? go(idx + 1) : on.toggle_collapse(r.r_path),
      )
    | ("ArrowLeft", false, false, Some(r)) =>
      Some(
        r.r_expanded
          ? on.toggle_collapse(r.r_path) : on.set_cursor(r.r_parent),
      )
    | ("Enter", false, false, None) =>
      Some(
        props.header.h_open > 0
          ? on.show_whole(true)
          : props.header.h_parked > 0 ? on.show_whole(false) : Effect.Ignore,
      )
    | ("Enter", false, false, Some(r)) =>
      Some(
        switch (id_of(r)) {
        | Some(id) => show(props, on, id)
        | None => on.toggle_collapse(r.r_path)
        },
      )
    | (" ", false, false, Some(r)) =>
      Some(
        switch (open_close(props, on, r.r_node)) {
        | Some(act) => act()
        | None => Effect.Ignore
        },
      )
    | ("ArrowUp", true, false, Some(r)) =>
      Option.map(id => on.def_op(MoveUp, id), id_of(r))
    | ("ArrowDown", true, false, Some(r)) =>
      Option.map(id => on.def_op(MoveDown, id), id_of(r))
    | ("ArrowRight", true, false, Some(r)) when is_module(r) =>
      Option.map(on.zoom_in, id_of(r))
    | ("ArrowLeft", true, false, _) => Some(on.zoom_out)
    | (_, false, true, Some(r)) when code == "KeyD" =>
      Option.map(id => on.def_op(Duplicate, id), id_of(r))
    /* the cursor moves only once the delete lands (DefOp) */
    | ("Backspace", false, true, Some(r))
    | ("Delete", false, false, Some(r)) =>
      Option.map(id => on.def_op(Delete, id), id_of(r))
    | _ => None
    };
  let nameable = (r: visible_row) =>
    switch (r.r_node.o_kind) {
    | KFn
    | KConst
    | KType
    | KModule => r.r_node.o_id != None
    | _ => false
    };
  let start_edit = (r: visible_row) =>
    nameable(r)
      ? on.edit.set(Some(OutlineEdit.mk(~row=?r.r_node.o_id, r.r_name)))
      : Effect.Ignore;
  /* typing a name: Enter saves and starts a new definition below, ↑↓
     save and move, Esc cancels */
  let edit_key = (ed: OutlineEdit.t) => {
    let t = ed.ed_text;
    let c = max(0, min(ed.ed_caret, String.length(t)));
    let put = (t, c) =>
      on.edit.set(
        Some({
          ...ed,
          ed_text: t,
          ed_caret: c,
          ed_error: None,
        }),
      );
    let anchor =
      Option.bind(ed.ed_anchor, a =>
        Array.find_opt(r => r.r_node.o_id == Some(a), visible)
      );
    let back_to_anchor =
      /* an unfinished new definition vanishes; the cursor returns */
      switch (anchor) {
      | Some(r) => on.set_cursor(Some(r.r_path))
      | None => Effect.Ignore
      };
    switch (key) {
    | "Enter" => Some(on.edit.commit(ed, Next))
    | "Escape" => Some(Effect.Many([on.edit.set(None), back_to_anchor]))
    | "ArrowUp" when ed.ed_row != None =>
      Some(on.edit.commit(ed, MoveTo(target(idx - 1))))
    | "ArrowDown" when ed.ed_row != None =>
      Some(on.edit.commit(ed, MoveTo(target(idx + 1))))
    | "ArrowUp"
    | "ArrowDown" =>
      Some(
        on.edit.commit(
          ed,
          switch (anchor) {
          | Some(r) => MoveTo(Some(r.r_path))
          | None => Stay
          },
        ),
      )
    | "ArrowLeft" => Some(put(t, max(0, c - 1)))
    | "ArrowRight" => Some(put(t, min(String.length(t), c + 1)))
    | "Home" => Some(put(t, 0))
    | "End" => Some(put(t, String.length(t)))
    | "Backspace" when t == "" && ed.ed_row == None =>
      Some(Effect.Many([on.edit.set(None), back_to_anchor]))
    | "Backspace" =>
      Some(
        c == 0
          ? Effect.Ignore
          : put(
              String.sub(t, 0, c - 1)
              ++ String.sub(t, c, String.length(t) - c),
              c - 1,
            ),
      )
    | "Delete" =>
      Some(
        c >= String.length(t)
          ? Effect.Ignore
          : put(
              String.sub(t, 0, c)
              ++ String.sub(t, c + 1, String.length(t) - c - 1),
              c,
            ),
      )
    | "Tab" => Some(Effect.Ignore)
    | _ when String.length(key) == 1 && !meta =>
      Some(
        put(
          String.sub(t, 0, c)
          ++ key
          ++ String.sub(t, c, String.length(t) - c),
          c + 1,
        ),
      )
    | _ => None
    };
  };
  /* a thunk: building a key's effect can move the cursor at once, so
     keys the menu takes must not build it */
  let act = () =>
    switch (on.edit.get(), key, meta, cur) {
    | (Some(ed), _, _, _) => edit_key(ed)
    | (None, "Enter", true, Some(r))
    | (None, "F2", _, Some(r)) => Some(start_edit(r))
    | (None, _, _, _) => nav_key()
    };
  /* an open menu takes ↑↓, Enter and Esc; any other key closes it, then
     acts */
  let menu = on.get_menu();
  let menu_sel = on.get_menu_sel();
  let items =
    switch (menu) {
    | Some((id, is_module, _, _)) =>
      menu_items(menu_rows(~on, ~is_module, id))
    | None => []
    };
  let n_items = List.length(items);
  switch (menu, key) {
  | (Some(_), "Escape") => stop(on.menu_close)
  | (Some(_), "ArrowDown") when n_items > 0 =>
    stop(on.menu_select((menu_sel + 1) mod n_items))
  | (Some(_), "ArrowUp") when n_items > 0 =>
    stop(on.menu_select((menu_sel + n_items - 1) mod n_items))
  | (Some(_), "Enter") =>
    switch (List.nth_opt(items, menu_sel)) {
    | Some(item) => stop(Effect.Many([on.menu_close, item()]))
    | None => stop(on.menu_close)
    }
  | (Some(_), "Shift" | "Alt" | "Meta" | "Control") => Effect.Ignore
  | (Some(_), _) =>
    switch (act()) {
    | Some(eff) => stop(Effect.Many([on.menu_close, eff]))
    | None => on.menu_close
    }
  | (None, _) =>
    switch (act()) {
    | Some(eff) => stop(eff)
    | None => Effect.Ignore
    }
  };
};

/* the breadcrumb: the program, then each zoomed module. An ancestor
   crumb zooms out to it; the last one shows its level's whole */
let header_view = (~on: handlers, header: header): list(Node.t) => {
  /* documentation slides are named "Folder / Name" */
  let program = {
    let s = header.h_program;
    let n = String.length(s);
    let rec last = (i, found) =>
      i + 3 > n
        ? found
        : last(i + 1, String.sub(s, i, 3) == " / " ? Some(i) : found);
    switch (last(0, None)) {
    | Some(i) => String.sub(s, i + 3, n - i - 3)
    | None => s
    };
  };
  let crumbs =
    [(Option.none, program)]
    @ List.map(((id, l)) => (Option.some(id), l), header.h_trail);
  let n = List.length(crumbs);
  let sep =
    span(~attrs=[clss(["outline-crumb-sep"])], [text({js|›|js})]);
  let crumb_nodes =
    List.concat(
      List.mapi(
        (i, (id, label)) => {
          let last = i == n - 1;
          let node =
            span(
              ~attrs=[
                clss(
                  ["outline-crumb"] @ (last ? ["outline-crumb-here"] : []),
                ),
                Attr.title(
                  last
                    ? header.h_open > 0 ? "show all of it" : label
                    : "zoom out to " ++ label,
                ),
                Attr.on_click(_ =>
                  stop(
                    last
                      ? header.h_open > 0
                          ? on.show_whole(true) : Effect.Ignore
                      : on.zoom_to(id),
                  )
                ),
              ],
              [text(label)],
            );
          i == 0 ? [node] : [sep, node];
        },
        crumbs,
      ),
    );
  let tail =
    header.h_open > 0
      ? [
        sep,
        span(
          ~attrs=[clss(["outline-crumb-count"])],
          [text(string_of_int(header.h_open) ++ " open")],
        ),
        span(
          ~attrs=[
            clss(["outline-crumb-btn"]),
            Attr.title("close these cells"),
            Attr.on_click(_ => stop(on.discard)),
          ],
          [text({js|×|js})],
        ),
      ]
      : header.h_parked > 0
          ? [
            span(
              ~attrs=[
                clss(["outline-crumb-chip"]),
                Attr.title("back to the cells"),
                Attr.on_click(_ => stop(on.show_whole(false))),
              ],
              [
                text(
                  {js|↩ |js} ++ string_of_int(header.h_parked) ++ " open",
                ),
              ],
            ),
          ]
          : [];
  [
    span(~attrs=[clss(["outline-menu-glyph"])], [text({js|☰|js})]),
    span(~attrs=[clss(["outline-word"])], [text("outline")]),
    span(~attrs=[clss(["outline-crumbs"])], crumb_nodes @ tail),
  ];
};

let view = (~props: props, ~on: handlers, term: Language.Exp.t): Node.t => {
  /* zoomed, the module's members are the top level; collapse paths
     stay rooted at the program */
  let (roots, root_path) =
    switch (props.zoom_root) {
    | Some(m) =>
      switch (OutlineTree.node_of(m, term), OutlineTree.label_path(m, term)) {
      | (Some(n), Some(path)) => (n.o_children, path)
      | _ => (OutlineTree.of_term(term), [])
      }
    | None => (OutlineTree.of_term(term), [])
    };
  /* the rows on screen, in order: the keyboard's list */
  let visible: array(visible_row) = {
    let rec walk = (parent, prefix, ns: list(OutlineTree.node)) =>
      List.concat_map(
        ((n: OutlineTree.node, seg)) => {
          let path = prefix @ [seg];
          let name = n.o_label;
          let n = live_label(props, n);
          let branch = n.o_children != [];
          let expanded = branch && !props.is_collapsed(path);
          [
            {
              r_path: path,
              r_node: n,
              r_name: name,
              r_parent: parent,
              r_expanded: expanded,
            },
          ]
          @ (expanded ? walk(Some(path), path, n.o_children) : []);
        },
        OutlineTree.segs(ns),
      );
    Array.of_list(walk(None, root_path, roots));
  };
  create(
    "details",
    ~attrs=
      [Attr.id("outline-sidebar"), Attr.create("open", "")]
      @ (props.arrows ? [] : [clss(["no-arrows"])]),
    [
      create(
        "summary",
        ~attrs=
          [
            clss(
              ["outline-title"]
              @ (props.cursor == None ? ["outline-cursor"] : []),
            ),
          ]
          @ (
            props.stack_controls
              ? [Attr.on_mousedown(_ => take_cursor(on, None))] : []
          ),
        props.stack_controls
          ? header_view(~on, props.header) : [text({js|☰ outline|js})],
      ),
      div(~attrs=[clss(["outline-resize"]), ...resize_attrs], []),
      div(
        ~attrs=
          [clss(["outline-body"])]
          @ (
            props.stack_controls
              ? [
                Attr.tabindex(0),
                Attr.on_keydown(keys(~props, ~on, ~visible)),
                Attr.on_focus(_ =>
                  Effect.Many([Effect.Stop_propagation, on.focused])
                ),
                /* leaving the outline saves a name being typed */
                Attr.on_blur(_ =>
                  switch (on.edit.get()) {
                  | Some(ed) =>
                    Effect.Many([
                      Effect.Stop_propagation,
                      on.edit.commit(ed, Stay),
                    ])
                  | None => Effect.Stop_propagation
                  }
                ),
              ]
              : []
          ),
        roots == []
          ? [
            div(
              ~attrs=[clss(["outline-empty"])],
              [text("no definitions")],
            ),
          ]
          : rows_view(~props, ~on, ~path=root_path, roots),
      ),
    ]
    @ (
      switch (props.menu) {
      | Some((id, is_module, x, y)) when props.stack_controls =>
        menu_view(
          ~menu_close=on.menu_close,
          ~rows=menu_rows(~on, ~is_module, id),
          ~selected=props.menu_sel,
          ~is_module,
          (x, y),
        )
      | _ => []
      }
    ),
  );
};
