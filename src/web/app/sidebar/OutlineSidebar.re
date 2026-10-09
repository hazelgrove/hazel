open Virtual_dom.Vdom;
open Node;

/* the collapsible module/definition outline. a click jumps; the cell
   button toggles a definition in the focus stack (stacked cells replace
   the master editor), and with a stack open a click adds or moves to that
   cell. without a stack (~stack_controls=false) it only navigates, and
   collapse is the native <details> toggle */

let clss = cs => Attr.classes(cs);

module TestStatus = Language.TestStatus;

/* structural operations on a definition, from its row's context menu;
   ItemEdit applies them at the item's owning block */
[@deriving (show({with_path: false}), sexp, yojson)]
type def_op =
  | NewBelow
  | NewTypeBelow
  | NewModuleBelow
  | Duplicate
  | MoveUp
  | MoveDown
  | Delete;

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

type name_edit =
  OutlineEdit.t = {
    ed_row: option(Language.Id.t),
    ed_anchor: option(Language.Id.t),
    ed_inside: bool,
    ed_text: string,
    ed_caret: int,
    ed_error: option(string),
  };

type edit_ctl = {
  current: option(name_edit),
  /* read at the keypress: keys can outrun renders */
  get: unit => option(name_edit),
  set: option(name_edit) => Effect.t(unit),
  /* save; with [true], start a new definition below */
  commit: (name_edit, bool) => Effect.t(unit),
};

/* a new definition's kind from its leading keyword */
let new_kind = (text: string): (OutlineTree.kind, string) => {
  let starts = p =>
    String.length(text) >= String.length(p)
    && String.sub(text, 0, String.length(p)) == p;
  starts("type ")
    ? (KType, "type ")
    : starts("module ") ? (KModule, "module ") : (KConst, "");
};

/* the typed text with its caret; a new definition's keyword colored */
let edit_text = (ed: name_edit): list(Node.t) => {
  let t = ed.ed_text;
  let n = String.length(t);
  let c = max(0, min(ed.ed_caret, n));
  let p = ed.ed_row == None ? String.length(snd(new_kind(t))) : 0;
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

let edit_hint = (ed: name_edit): list(Node.t) =>
  switch (ed.ed_error) {
  | Some(why) => [div(~attrs=[clss(["outline-edit-hint"])], [text(why)])]
  | None => []
  };

/* the row a new definition is typed into, below its anchor */
let new_row_view = (ed: name_edit): Node.t => {
  let (k, _) = new_kind(ed.ed_text);
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
let resize_attrs: list(Attr.t) = {
  Js_of_ocaml.[
    Attr.on_pointerdown(_ => {
      let doc = Js.Unsafe.coerce(Dom_html.document);
      let move_ref = ref(Js.Unsafe.inject(Js.null));
      let up_ref = ref(Js.Unsafe.inject(Js.null));
      let on_move =
        Js.Unsafe.callback(evt => {
          let x: int = Js.Unsafe.coerce(evt)##.clientX;
          let w = max(140, min(420, x));
          let root =
            Js.Unsafe.coerce(Dom_html.document)##.documentElement##.style;
          let _ =
            Js.Unsafe.meth_call(
              root,
              "setProperty",
              [|
                Js.Unsafe.inject(Js.string("--outline-w")),
                Js.Unsafe.inject(Js.string(string_of_int(w) ++ "px")),
              |],
            );
          ();
        });
      let on_up =
        Js.Unsafe.callback(_ => {
          let _ =
            Js.Unsafe.meth_call(
              doc,
              "removeEventListener",
              [|Js.Unsafe.inject(Js.string("pointermove")), move_ref^|],
            );
          let _ =
            Js.Unsafe.meth_call(
              doc,
              "removeEventListener",
              [|Js.Unsafe.inject(Js.string("pointerup")), up_ref^|],
            );
          ();
        });
      move_ref := Js.Unsafe.inject(on_move);
      up_ref := Js.Unsafe.inject(on_up);
      let _ =
        Js.Unsafe.meth_call(
          doc,
          "addEventListener",
          [|Js.Unsafe.inject(Js.string("pointermove")), move_ref^|],
        );
      let _ =
        Js.Unsafe.meth_call(
          doc,
          "addEventListener",
          [|Js.Unsafe.inject(Js.string("pointerup")), up_ref^|],
        );
      Effect.Prevent_default;
    }),
  ];
};

/* the keyboard to the outline: a click anywhere in it takes focus */
let take_focus =
  Effect.of_sync_fun(
    () =>
      if (!Util.JsUtil.outline_has_focus()) {
        Util.JsUtil.focus_outline();
      },
    (),
  );

let rec node_view =
        (
          ~arrows: bool,
          ~stack_controls: bool,
          ~can_open: Language.Id.t => bool,
          ~jump: Language.Id.t => Effect.t(unit),
          ~focus: Language.Id.t => Effect.t(unit),
          ~toggle: Language.Id.t => Effect.t(unit),
          ~toggle_run: Language.Id.t => Effect.t(unit),
          ~is_collapsed: OutlineTree.path => bool,
          ~toggle_collapse: OutlineTree.path => Effect.t(unit),
          ~path: OutlineTree.path,
          ~seg: OutlineTree.path_seg, /* this row's name among its siblings */
          ~menu_open: (Language.Id.t, bool, float, float) => Effect.t(unit),
          ~error_subtree: list(Language.Id.t),
          ~focused_entries: list((Language.Id.t, option(string))),
          ~error_items: list(Language.Id.t),
          ~test_status: Language.Id.t => option(TestStatus.t),
          ~cursor: option(OutlineTree.path),
          ~set_cursor: option(OutlineTree.path) => Effect.t(unit),
          ~edit: edit_ctl,
          ~created: option((Language.Id.t, string)),
          ~pinned: list(Language.Id.t),
          n: OutlineTree.node,
        )
        : Node.t => {
  let editing =
    switch (edit.current) {
    | Some(ed) when ed.ed_row != None && ed.ed_row == n.o_id => Some(ed)
    | _ => None
    };
  let status = test_glyph(~test_status, n);
  let has_err =
    switch (n.o_id) {
    | Some(id) => List.mem(id, error_items)
    | None => false
    };
  /* subtree carries an error: badge shown by CSS only while COLLAPSED
     (the deepest visible row owns the error otherwise) */
  let has_roll_err =
    !has_err
    && (
      switch (n.o_id) {
      | Some(id) => List.mem(id, error_subtree)
      | None => false
      }
    );
  let stacked =
    switch (n.o_id) {
    | Some(id) => List.mem_assoc(id, focused_entries)
    | None => false
    };
  let any_focus = focused_entries != [];
  /* the focused row's name tracks its header editor LIVE */
  let n =
    switch (n.o_id) {
    | Some(id) =>
      switch (List.assoc_opt(id, focused_entries)) {
      | Some(Some(live)) =>
        OutlineTree.{
          ...n,
          o_label: live,
        }
      | _ => n
      }
    | None => n
    };
  /* paths name the row by its outline label, not the live header text
     (`inc(x)` for `inc`): collapse state and the cursor survive pinning */
  let row_path = path @ [seg];
  let label =
    div(
      ~attrs=
        [
          clss(
            ["outline-label", kind_cls(n.o_kind)]
            @ (cursor == Some(row_path) ? ["outline-cursor"] : [])
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
        ]
        @ (
          switch (n.o_id) {
          | Some(id) => [
              Attr.create("data-ol-id", Language.Id.to_string(id)),
            ]
          | None => []
          }
        )
        /* a click takes the keyboard to the outline, at this row: focus
           first, so the focus event's cursor lands before this one */
        @ [
          Attr.on_mousedown(_ =>
            switch (edit.get()) {
            | Some(ed) when ed.ed_row == None || ed.ed_row != n.o_id =>
              /* clicking away from a name saves it */
              Effect.Many([
                edit.commit(ed, false),
                set_cursor(Some(row_path)),
              ])
            | Some(_) => Effect.Ignore
            | None =>
              Effect.Many([
                Effect.Prevent_default,
                take_focus,
                set_cursor(Some(row_path)),
              ])
            }
          ),
        ]
        @ (
          switch (n.o_id) {
          /* with a stack open, a click adds or moves to the cell (a jump
             would target the hidden master). Prevent_default: label
             clicks must not toggle the row's <details> */
          | Some(id) when any_focus => [
              Attr.on_click(_ =>
                Effect.Many([
                  Effect.Prevent_default,
                  Effect.Stop_propagation,
                  focus(id),
                ])
              ),
            ]
          | Some(id) => [
              Attr.on_click(_ =>
                Effect.Many([
                  Effect.Prevent_default,
                  Effect.Stop_propagation,
                  jump(id),
                ])
              ),
            ]
          | None => []
          }
        )
        @ (
          /* every block level gets the menu, except trailing-expression
             rows */
          switch (n.o_id) {
          | Some(id) when stack_controls && n.o_kind != OutlineTree.KTrail => [
              Attr.on_contextmenu(evt => {
                let x =
                  float_of_int(Js_of_ocaml.Js.Unsafe.coerce(evt)##.clientX);
                let y =
                  float_of_int(Js_of_ocaml.Js.Unsafe.coerce(evt)##.clientY);
                Effect.Many([
                  Effect.Prevent_default,
                  menu_open(id, n.o_kind == OutlineTree.KModule, x, y),
                ]);
              }),
            ]
          | _ => []
          }
        ),
      [
        span(
          ~attrs=
            [clss(["outline-glyph"])]
            /* without arrows a branch's sigil folds it */
            @ (
              !arrows && n.o_children != []
                ? [
                  Attr.on_click(evt =>
                    Effect.Many([
                      Effect.Prevent_default,
                      Effect.Stop_propagation,
                      stack_controls
                        ? toggle_collapse(row_path)
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
                    ])
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
        switch (editing, created) {
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
        | _ when n.o_kind == OutlineTree.KTrail && n.o_label == "" => [
            span(~attrs=[clss(["outline-synthetic"])], [text("result")]),
          ]
        | _ when n.o_kind == OutlineTree.KTests => [
            span(~attrs=[clss(["outline-synthetic"])], [text(n.o_label)]),
          ]
        | _ => [text(n.o_label)]
        }
      )
      @ (
        has_err
          ? [
            span(
              ~attrs=[
                clss(["outline-err-badge"]),
                Attr.title("contains type errors"),
              ],
              [text({js|●|js})],
            ),
          ]
          : []
      )
      @ (
        has_roll_err
          ? [
            span(
              ~attrs=[
                clss(["outline-err-badge", "outline-err-roll"]),
                Attr.title("contains type errors (collapsed)"),
              ],
              [text({js|●|js})],
            ),
          ]
          : []
      )
      @ (
        switch (n.o_id) {
        | _ when !stack_controls => []
        | None when n.o_kind == OutlineTree.KTests =>
          /* the tests container pins/unpins its whole run */
          let kid_ids =
            List.filter_map((c: OutlineTree.node) => c.o_id, n.o_children);
          let all_pinned =
            kid_ids != []
            && List.for_all(
                 id => List.mem_assoc(id, focused_entries),
                 kid_ids,
               );
          [
            span(
              ~attrs=[
                clss(
                  ["outline-focus-btn"]
                  @ (all_pinned ? ["outline-btn-on"] : []),
                ),
                Attr.title(
                  all_pinned
                    ? "close the tests cell" : "open the tests as one cell",
                ),
                Attr.on_click(_ =>
                  Effect.Many(
                    [Effect.Prevent_default, Effect.Stop_propagation]
                    /* one cell spanning the whole run, at any depth */
                    @ (
                      switch (kid_ids) {
                      | [first, ..._] => [toggle_run(first)]
                      | [] => []
                      }
                    ),
                  )
                ),
              ],
              [],
            ),
          ];
        | Some(id) when !stacked && !can_open(id) => [
            span(
              ~attrs=[
                clss(["outline-focus-btn", "outline-btn-disabled"]),
                Attr.title("finish the definition to open it"),
              ],
              [],
            ),
          ]
        | Some(id) => [
            span(
              ~attrs=[
                clss(
                  ["outline-focus-btn"]
                  @ (
                    stacked
                      ? ["outline-btn-on"]
                      : List.mem(id, pinned) ? ["outline-btn-parked"] : []
                  ),
                ),
                Attr.title(
                  stacked ? "close this cell" : "open in the editor stack",
                ),
                Attr.on_click(_ =>
                  Effect.Many([
                    Effect.Prevent_default,
                    Effect.Stop_propagation,
                    toggle(id),
                  ])
                ),
              ],
              [],
            ),
          ]
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
    switch (edit.current) {
    | Some({ed_row: None, ed_anchor: Some(a), ed_inside: true, _} as ed)
        when n.o_id == Some(a) =>
      Some(new_row_view(ed))
    | _ => None
    };
  switch (n.o_children, inside_row) {
  | ([], None) => div(~attrs=[clss(["outline-leaf"])], [label, ...hint])
  | (kids, _) =>
    let my_path = row_path;
    create(
      "details",
      ~attrs=
        [clss(["outline-branch"])]
        @ (
          is_collapsed(my_path) && Option.is_none(inside_row)
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
              stack_controls
                ? [
                  Attr.on_click(_ =>
                    Effect.Many([
                      Effect.Prevent_default,
                      toggle_collapse(my_path),
                    ])
                  ),
                  /* the chevron too: the summary would take focus
                     itself */
                  Attr.on_mousedown(_ =>
                    switch (edit.get()) {
                    | Some(_) => Effect.Ignore
                    | None =>
                      Effect.Many([
                        Effect.Prevent_default,
                        take_focus,
                        set_cursor(Some(my_path)),
                      ])
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
          List.concat_map(
            ((kid: OutlineTree.node, kseg)) =>
              [
                node_view(
                  ~arrows,
                  ~stack_controls,
                  ~can_open,
                  ~jump,
                  ~focus,
                  ~toggle,
                  ~toggle_run,
                  ~is_collapsed,
                  ~toggle_collapse,
                  ~path=my_path,
                  ~seg=kseg,
                  ~menu_open,
                  ~error_subtree,
                  ~focused_entries,
                  ~error_items,
                  ~test_status,
                  ~cursor,
                  ~set_cursor,
                  ~edit,
                  ~created,
                  ~pinned,
                  kid,
                ),
              ]
              @ new_row_after(edit, kid),
            OutlineTree.segs(kids),
          )
          @ Option.to_list(inside_row),
        ),
      ],
    );
  };
}
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
    (
      ~def_op: (def_op, Language.Id.t) => Effect.t(unit),
      ~zoom_in: Language.Id.t => Effect.t(unit),
      ~edit: edit_ctl,
      ~is_module: bool,
      id: Language.Id.t,
    )
    : list(menu_row) => {
  let mac = Util.Os.is_mac^;
  let op = (~keys=?, o, label) => Item(label, keys, () => def_op(o, id));
  /* the name row the keyboard makes; [prefix] picks the kind */
  let new_row = (~inside=false, label, prefix) =>
    Item(
      label,
      None,
      () =>
        Effect.Many([
          take_focus,
          edit.set(
            Some({
              ed_row: None,
              ed_anchor: Some(id),
              ed_inside: inside,
              ed_text: prefix,
              ed_caret: String.length(prefix),
              ed_error: None,
            }),
          ),
        ]),
    );
  (
    is_module
      ? [
        Item(
          "Zoom in",
          Some(mac ? {js|⌥→|js} : {js|Alt+→|js}),
          () => zoom_in(id),
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
    (
      ~visible: array(visible_row),
      ~header: header,
      /* read at the keypress: keys can outrun renders */
      ~get_cursor: unit => option(OutlineTree.path),
      ~set_cursor: option(OutlineTree.path) => Effect.t(unit),
      ~any_focus: bool,
      ~stacked: Language.Id.t => bool,
      ~can_open: Language.Id.t => bool,
      ~jump: Language.Id.t => Effect.t(unit),
      ~focus: Language.Id.t => Effect.t(unit),
      ~toggle: Language.Id.t => Effect.t(unit),
      ~toggle_run: Language.Id.t => Effect.t(unit),
      ~toggle_collapse: OutlineTree.path => Effect.t(unit),
      ~zoom_in: Language.Id.t => Effect.t(unit),
      ~zoom_out: Effect.t(unit),
      ~show_whole: bool => Effect.t(unit),
      ~def_op: (def_op, Language.Id.t) => Effect.t(unit),
      ~leave: Effect.t(unit),
      ~edit: edit_ctl,
      /* read at the keypress, like the cursor */
      ~get_menu: unit => option((Language.Id.t, bool, float, float)),
      ~get_menu_sel: unit => int,
      ~menu_select: int => Effect.t(unit),
      ~menu_open: (Language.Id.t, bool, float, float) => Effect.t(unit),
      ~menu_close: Effect.t(unit),
      evt,
    )
    : Effect.t(unit) => {
  let n = Array.length(visible);
  let idx =
    switch (get_cursor()) {
    | None => (-1)
    | Some(p) =>
      let rec find = i =>
        i >= n ? (-1) : visible[i].r_path == p ? i : find(i + 1);
      find(0);
    };
  let go = i =>
    i < 0 || n == 0
      ? set_cursor(None) : set_cursor(Some(visible[min(i, n - 1)].r_path));
  let cur = idx >= 0 ? Some(visible[idx]) : None;
  let e = Js_of_ocaml.Js.Unsafe.coerce(evt);
  let key: string = Js_of_ocaml.Js.to_string(e##.key);
  let code: string = Js_of_ocaml.Js.to_string(e##.code);
  let alt: bool = Js_of_ocaml.Js.to_bool(e##.altKey);
  let shift: bool = Js_of_ocaml.Js.to_bool(e##.shiftKey);
  let meta: bool =
    Js_of_ocaml.Js.to_bool(e##.metaKey)
    || Js_of_ocaml.Js.to_bool(e##.ctrlKey);
  let id_of = (r: visible_row) => r.r_node.o_id;
  let show = (r: visible_row) =>
    switch (id_of(r)) {
    | Some(id) => any_focus ? focus(id) : jump(id)
    | None => toggle_collapse(r.r_path)
    };
  let open_close = (r: visible_row) =>
    switch (id_of(r), r.r_node.o_kind) {
    | (Some(id), _) when stacked(id) || can_open(id) => toggle(id)
    | (None, OutlineTree.KTests) =>
      switch (
        List.filter_map((c: OutlineTree.node) => c.o_id, r.r_node.o_children)
      ) {
      | [first, ..._] => toggle_run(first)
      | [] => Effect.Ignore
      }
    | _ => Effect.Ignore
    };
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
        menu_open(id, is_module(r), x, y);
      },
      id_of(r),
    );
  let nav_key = () =>
    switch (key, alt, meta, cur) {
    | (".", false, true, Some(r)) => open_menu(r)
    | ("F10", false, false, Some(r)) when shift => open_menu(r)
    | ("Escape", _, _, _) => Some(leave)
    | _ when alt && code == "KeyO" => Some(leave)
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
          : r.r_expanded ? go(idx + 1) : toggle_collapse(r.r_path),
      )
    | ("ArrowLeft", false, false, Some(r)) =>
      Some(
        r.r_expanded ? toggle_collapse(r.r_path) : set_cursor(r.r_parent),
      )
    | ("Enter", false, false, None) =>
      Some(
        header.h_open > 0
          ? show_whole(true)
          : header.h_parked > 0 ? show_whole(false) : Effect.Ignore,
      )
    | ("Enter", false, false, Some(r)) => Some(show(r))
    | (" ", false, false, Some(r)) => Some(open_close(r))
    | ("ArrowUp", true, false, Some(r)) =>
      Option.map(id => def_op(MoveUp, id), id_of(r))
    | ("ArrowDown", true, false, Some(r)) =>
      Option.map(id => def_op(MoveDown, id), id_of(r))
    | ("ArrowRight", true, false, Some(r)) when is_module(r) =>
      Option.map(zoom_in, id_of(r))
    | ("ArrowLeft", true, false, _) => Some(zoom_out)
    | (_, false, true, Some(r)) when code == "KeyD" =>
      Option.map(id => def_op(Duplicate, id), id_of(r))
    | ("Backspace", false, true, Some(r))
    | ("Delete", false, false, Some(r)) =>
      Option.map(
        id =>
          Effect.Many([
            set_cursor(
              idx + 1 < n
                ? Some(visible[idx + 1].r_path)
                : idx > 0 ? Some(visible[idx - 1].r_path) : None,
            ),
            def_op(Delete, id),
          ]),
        id_of(r),
      )
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
      ? edit.set(
          Some({
            ed_row: r.r_node.o_id,
            ed_anchor: None,
            ed_inside: false,
            ed_text: r.r_name,
            ed_caret: String.length(r.r_name),
            ed_error: None,
          }),
        )
      : Effect.Ignore;
  /* typing a name: Enter saves and starts a new definition below, ↑↓
     save and move, Esc cancels */
  let edit_key = (ed: name_edit) => {
    let t = ed.ed_text;
    let c = max(0, min(ed.ed_caret, String.length(t)));
    let put = (t, c) =>
      edit.set(
        Some({
          ...ed,
          ed_text: t,
          ed_caret: c,
          ed_error: None,
        }),
      );
    let back_to_anchor =
      /* an unfinished new definition vanishes; the cursor returns */
      switch (ed.ed_anchor) {
      | Some(a) =>
        let rec find = (i): option(OutlineTree.path) =>
          i >= n
            ? Option.none
            : visible[i].r_node.o_id == Option.some(a)
                ? Option.some(visible[i].r_path) : find(i + 1);
        switch (find(0)) {
        | Some(p) => set_cursor(Some(p))
        | None => Effect.Ignore
        };
      | None => Effect.Ignore
      };
    switch (key) {
    | "Enter" => Some(edit.commit(ed, true))
    | "Escape" => Some(Effect.Many([edit.set(None), back_to_anchor]))
    | "ArrowUp" when ed.ed_row != None =>
      Some(Effect.Many([edit.commit(ed, false), go(idx - 1)]))
    | "ArrowDown" when ed.ed_row != None =>
      Some(Effect.Many([edit.commit(ed, false), go(idx + 1)]))
    | "ArrowUp"
    | "ArrowDown" =>
      Some(Effect.Many([edit.commit(ed, false), back_to_anchor]))
    | "ArrowLeft" => Some(put(t, max(0, c - 1)))
    | "ArrowRight" => Some(put(t, min(String.length(t), c + 1)))
    | "Home" => Some(put(t, 0))
    | "End" => Some(put(t, String.length(t)))
    | "Backspace" when t == "" && ed.ed_row == None =>
      Some(Effect.Many([edit.set(None), back_to_anchor]))
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
    switch (edit.get(), key, meta, cur) {
    | (Some(ed), _, _, _) => edit_key(ed)
    | (None, "Enter", true, Some(r))
    | (None, "F2", _, Some(r)) => Some(start_edit(r))
    | (None, _, _, _) => nav_key()
    };
  let handled = eff =>
    Effect.Many([Effect.Prevent_default, Effect.Stop_propagation, eff]);
  /* an open menu takes ↑↓, Enter and Esc; any other key closes it, then
     acts */
  let menu = get_menu();
  let menu_sel = get_menu_sel();
  let items =
    switch (menu) {
    | Some((id, is_module, _, _)) =>
      menu_items(menu_rows(~def_op, ~zoom_in, ~edit, ~is_module, id))
    | None => []
    };
  let n_items = List.length(items);
  switch (menu, key) {
  | (Some(_), "Escape") => handled(menu_close)
  | (Some(_), "ArrowDown") when n_items > 0 =>
    handled(menu_select((menu_sel + 1) mod n_items))
  | (Some(_), "ArrowUp") when n_items > 0 =>
    handled(menu_select((menu_sel + n_items - 1) mod n_items))
  | (Some(_), "Enter") =>
    switch (List.nth_opt(items, menu_sel)) {
    | Some(item) => handled(Effect.Many([menu_close, item()]))
    | None => handled(menu_close)
    }
  | (Some(_), "Shift" | "Alt" | "Meta" | "Control") => Effect.Ignore
  | (Some(_), _) =>
    switch (act()) {
    | Some(eff) => handled(Effect.Many([menu_close, eff]))
    | None => menu_close
    }
  | (None, _) =>
    switch (act()) {
    | Some(eff) => handled(eff)
    | None => Effect.Ignore
    }
  };
};

/* the breadcrumb: the program, then each zoomed module. An ancestor
   crumb zooms out to it; the last one shows its level's whole */
let header_view =
    (
      ~header: header,
      ~zoom_to: option(Language.Id.t) => Effect.t(unit),
      ~show_whole: bool => Effect.t(unit),
      ~discard: Effect.t(unit),
    )
    : list(Node.t) => {
  let stop = eff =>
    Effect.Many([Effect.Prevent_default, Effect.Stop_propagation, eff]);
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
                      ? header.h_open > 0 ? show_whole(true) : Effect.Ignore
                      : zoom_to(id),
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
            Attr.on_click(_ => stop(discard)),
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
                Attr.on_click(_ => stop(show_whole(false))),
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
  def_op: (def_op, Language.Id.t) => Effect.t(unit),
};

let view = (~props: props, ~on: handlers, term: Language.Exp.t): Node.t => {
  let {
    arrows,
    stack_controls,
    can_open,
    is_collapsed,
    header,
    zoom_root,
    cursor,
    created,
    pinned,
    focused_entries,
    error_items,
    error_subtree,
    menu,
    menu_sel,
    test_status,
  } = props;
  let {
    jump,
    focus,
    toggle,
    toggle_run,
    toggle_collapse,
    zoom_to,
    zoom_in,
    show_whole,
    discard,
    zoom_out,
    get_cursor,
    set_cursor,
    focused,
    leave,
    edit,
    menu_open,
    menu_close,
    menu_select,
    get_menu,
    get_menu_sel,
    def_op,
  } = on;
  /* zoomed, the module's members are the top level; collapse paths
     stay rooted at the program */
  let (roots, root_path) =
    switch (zoom_root) {
    | Some(m) =>
      switch (OutlineTree.node_of(m, term), OutlineTree.label_path(m, term)) {
      | (Some(n), Some(path)) => (n.o_children, path)
      | _ => (OutlineTree.of_term(term), [])
      }
    | None => (OutlineTree.of_term(term), [])
    };
  let live_label = (n: OutlineTree.node): OutlineTree.node =>
    switch (Option.bind(n.o_id, id => List.assoc_opt(id, focused_entries))) {
    | Some(Some(live)) => {
        ...n,
        o_label: live,
      }
    | _ => n
    };
  /* the rows on screen, in order: the keyboard's list */
  let visible: array(visible_row) = {
    let rec walk = (parent, prefix, ns: list(OutlineTree.node)) =>
      List.concat_map(
        ((n: OutlineTree.node, seg)) => {
          let path = prefix @ [seg];
          let name = n.o_label;
          let n = live_label(n);
          let branch = n.o_children != [];
          let expanded = branch && !is_collapsed(path);
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
  let key_handler =
    keys(
      ~visible,
      ~header,
      ~get_cursor,
      ~set_cursor,
      ~any_focus=focused_entries != [],
      ~stacked=id => List.mem_assoc(id, focused_entries),
      ~can_open,
      ~jump,
      ~focus,
      ~toggle,
      ~toggle_run,
      ~toggle_collapse,
      ~zoom_in,
      ~zoom_out,
      ~show_whole,
      ~def_op,
      ~leave,
      ~edit,
      ~get_menu,
      ~get_menu_sel,
      ~menu_select,
      ~menu_open,
      ~menu_close,
    );
  create(
    "details",
    ~attrs=
      [Attr.id("outline-sidebar"), Attr.create("open", "")]
      @ (arrows ? [] : [clss(["no-arrows"])]),
    [
      create(
        "summary",
        ~attrs=
          [
            clss(
              ["outline-title"] @ (cursor == None ? ["outline-cursor"] : []),
            ),
          ]
          @ (
            stack_controls
              ? [
                Attr.on_mousedown(_ =>
                  Effect.Many([
                    Effect.Prevent_default,
                    take_focus,
                    set_cursor(None),
                  ])
                ),
              ]
              : []
          ),
        stack_controls
          ? header_view(~header, ~zoom_to, ~show_whole, ~discard)
          : [text({js|☰ outline|js})],
      ),
      div(~attrs=[clss(["outline-resize"]), ...resize_attrs], []),
      div(
        ~attrs=
          [clss(["outline-body"])]
          @ (
            stack_controls
              ? [
                Attr.tabindex(0),
                Attr.on_keydown(key_handler),
                Attr.on_focus(_ =>
                  Effect.Many([Effect.Stop_propagation, focused])
                ),
                /* leaving the outline saves a name being typed */
                Attr.on_blur(_ =>
                  switch (edit.get()) {
                  | Some(ed) =>
                    Effect.Many([
                      Effect.Stop_propagation,
                      edit.commit(ed, false),
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
          : List.concat_map(
              ((root: OutlineTree.node, rseg)) =>
                [
                  node_view(
                    ~arrows,
                    ~stack_controls,
                    ~can_open,
                    ~jump,
                    ~focus,
                    ~toggle,
                    ~toggle_run,
                    ~is_collapsed,
                    ~toggle_collapse,
                    ~path=root_path,
                    ~seg=rseg,
                    ~menu_open,
                    ~error_subtree,
                    ~focused_entries,
                    ~error_items,
                    ~test_status,
                    ~cursor,
                    ~set_cursor,
                    ~edit,
                    ~created,
                    ~pinned,
                    root,
                  ),
                ]
                @ new_row_after(edit, root),
              OutlineTree.segs(roots),
            ),
      ),
    ]
    @ (
      switch (menu) {
      | Some((id, is_module, x, y)) when stack_controls =>
        menu_view(
          ~menu_close,
          ~rows=menu_rows(~def_op, ~zoom_in, ~edit, ~is_module, id),
          ~selected=menu_sel,
          ~is_module,
          (x, y),
        )
      | _ => []
      }
    ),
  );
};
