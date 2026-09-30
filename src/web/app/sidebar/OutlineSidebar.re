open Virtual_dom.Vdom;
open Node;

/* OutlineSidebar — the collapsible module/definition outline.
   Navigation: click = jump. Focus: the cell button TOGGLES a definition
   in the focus STACK (stacked header/body cells replace the master
   editor); plain click while a stack is open ADDS that definition to
   the stack (or moves to it if present) — it never replaces the
   stack. The banner splices everything home. Modes without a focus
   stack (~stack_controls=false) get navigation only: no cell buttons, no
   context menu, and collapse is the native <details> toggle. */

let clss = cs => Attr.classes(cs);

module TestStatus = Language.TestStatus;

/* structural operations on a TOP-LEVEL definition, offered from the
   row's context menu; handled by ScratchMode (segment surgery on the
   master program) */
[@deriving (show({with_path: false}), sexp, yojson)]
type def_op =
  | NewBelow
  | NewTypeBelow
  | NewModuleBelow
  | NewInside /* module rows: append a member inside the body */
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

let rec node_view =
        (
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
        /* a click moves the outline's cursor when the outline has
           focus; otherwise focus stays where it was (in the editor) */
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
              Util.JsUtil.outline_has_focus()
                ? set_cursor(Some(row_path)) : Effect.Prevent_default
            }
          ),
        ]
        @ (
          switch (n.o_id) {
          /* while a stack is open, a plain click ADDS/moves-to that
             cell (jumping at master ids would target the hidden
             editor). Prevent_default: label clicks must not toggle
             the row's <details> (collapse is the chevron's job). */
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
          /* structural ops work at every block level (Restructure
             recurses to the owning block); trailing-expression rows
             stay menu-less at any depth */
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
          ~attrs=[clss(["outline-glyph"])],
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
                    /* one cell spanning the whole run, at any depth
                       (test_run_deep) */
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
  switch (n.o_children) {
  | [] => div(~attrs=[clss(["outline-leaf"])], [label, ...hint])
  | kids =>
    let my_path = row_path;
    create(
      "details",
      ~attrs=
        [clss(["outline-branch"])]
        @ (is_collapsed(my_path) ? [] : [Attr.create("open", "")])
        @ (
          switch (n.o_id) {
          | Some(id) => [Attr.id("ol-b-" ++ Language.Id.to_string(id))]
          | None => []
          }
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
          ),
        ),
      ],
    );
  };
}
/* a new definition typed below [n] */
and new_row_after = (edit: edit_ctl, n: OutlineTree.node): list(Node.t) =>
  switch (edit.current) {
  | Some({ed_row: None, ed_anchor: Some(a), _} as ed) when n.o_id == Some(a) => [
      new_row_view(ed),
    ]
  | _ => []
  };

let menu_view =
    (
      ~menu_close: Effect.t(unit),
      ~def_op: (def_op, Language.Id.t) => Effect.t(unit),
      ~zoom_in: Language.Id.t => Effect.t(unit),
      ~is_module: bool,
      (id: Language.Id.t, x: float, y: float),
    )
    : list(Node.t) => {
  let item = (op, label_txt) =>
    div(
      ~attrs=[
        clss(["outline-def-menu-item"]),
        Attr.on_click(_ => Effect.Many([menu_close, def_op(op, id)])),
      ],
      [text(label_txt)],
    );
  [
    div(
      ~attrs=[
        clss(["outline-menu-backdrop"]),
        Attr.on_click(_ => menu_close),
        Attr.on_wheel(_ => menu_close),
        Attr.on_contextmenu(_ =>
          Effect.Many([Effect.Prevent_default, menu_close])
        ),
      ],
      [],
    ),
    {
      /* flip away from viewport edges (same Menu helpers the editor
         context menu uses); 7 items ≈ 190px tall, ~200px wide */
      let dir =
        Util.Menu.direction_of(
          ~menu_height=190.,
          ~menu_width=200.,
          Util.Menu.space_from(
            ~anchor_top=y,
            ~anchor_bot=y,
            ~anchor_left=x,
            ~anchor_right=x,
          ),
        );
      let vh: float = Js_of_ocaml.Js.Unsafe.global##.innerHeight;
      let vw: float = Js_of_ocaml.Js.Unsafe.global##.innerWidth;
      let v =
        dir.vertical == `Down
          ? Css_gen.create(~field="top", ~value=Printf.sprintf("%.0fpx", y))
          : Css_gen.create(
              ~field="bottom",
              ~value=Printf.sprintf("%.0fpx", vh -. y),
            );
      let h =
        dir.horizontal == `Right
          ? Css_gen.create(
              ~field="left",
              ~value=Printf.sprintf("%.0fpx", x),
            )
          : Css_gen.create(
              ~field="right",
              ~value=Printf.sprintf("%.0fpx", vw -. x),
            );
      div(
        ~attrs=[
          clss(["outline-def-menu"]),
          Attr.style(Css_gen.combine(h, v)),
        ],
        (
          is_module
            ? [
              div(
                ~attrs=[
                  clss(["outline-def-menu-item"]),
                  Attr.on_click(_ => Effect.Many([menu_close, zoom_in(id)])),
                ],
                [text("zoom in")],
              ),
              item(NewInside, "new definition inside"),
            ]
            : []
        )
        @ [
          item(NewBelow, "new definition below"),
          item(NewTypeBelow, "new type below"),
          item(NewModuleBelow, "new module below"),
          item(Duplicate, "duplicate"),
          item(MoveUp, "move up"),
          item(MoveDown, "move down"),
          item(Delete, "delete"),
        ],
      );
    },
  ];
};

type visible_row = {
  r_path: OutlineTree.path,
  r_node: OutlineTree.node,
  r_parent: option(OutlineTree.path),
  r_expanded: bool,
};

/* the outline's keys (plans/outline-ui.md): arrows move and fold,
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
  let nav_key = () =>
    switch (key, alt, meta, cur) {
    | ("Escape", _, _, _) => Some(leave)
    | _ when alt && code == "KeyO" => Some(leave)
    | ("ArrowDown", false, false, _) =>
      Some(idx + 1 < n ? go(idx + 1) : Effect.Ignore)
    | ("ArrowUp", false, false, _) =>
      Some(idx >= 0 ? go(idx - 1) : Effect.Ignore)
    | ("Home", false, false, _) => Some(go(-1))
    | ("End", false, false, _) => Some(go(n - 1))
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
            ed_text: r.r_node.o_label,
            ed_caret: String.length(r.r_node.o_label),
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
  let act =
    switch (edit.get(), key, meta, cur) {
    | (Some(ed), _, _, _) => edit_key(ed)
    | (None, "Enter", true, Some(r))
    | (None, "F2", _, Some(r)) => Some(start_edit(r))
    | (None, _, _, _) => nav_key()
    };
  switch (act) {
  | Some(eff) =>
    Effect.Many([Effect.Prevent_default, Effect.Stop_propagation, eff])
  | None => Effect.Ignore
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

let view =
    (
      ~stack_controls: bool,
      ~can_open: Language.Id.t => bool,
      ~jump: Language.Id.t => Effect.t(unit),
      ~focus: Language.Id.t => Effect.t(unit),
      ~toggle: Language.Id.t => Effect.t(unit),
      ~toggle_run: Language.Id.t => Effect.t(unit),
      ~is_collapsed: OutlineTree.path => bool,
      ~toggle_collapse: OutlineTree.path => Effect.t(unit),
      ~header: header,
      ~zoom_root: option(Language.Id.t),
      ~zoom_to: option(Language.Id.t) => Effect.t(unit),
      ~zoom_in: Language.Id.t => Effect.t(unit),
      ~show_whole: bool => Effect.t(unit),
      ~discard: Effect.t(unit),
      ~zoom_out: Effect.t(unit),
      ~cursor: option(OutlineTree.path),
      ~get_cursor: unit => option(OutlineTree.path),
      ~set_cursor: option(OutlineTree.path) => Effect.t(unit),
      ~focused: Effect.t(unit),
      ~leave: Effect.t(unit),
      ~edit: edit_ctl,
      ~created: option((Language.Id.t, string)),
      /* every pin, shown or not: a hidden one gets a quiet mark */
      ~pinned: list(Language.Id.t),
      ~focused_entries: list((Language.Id.t, option(string))),
      ~error_items: list(Language.Id.t),
      ~error_subtree: list(Language.Id.t),
      ~menu: option((Language.Id.t, bool, float, float)),
      ~menu_open: (Language.Id.t, bool, float, float) => Effect.t(unit),
      ~menu_close: Effect.t(unit),
      ~def_op: (def_op, Language.Id.t) => Effect.t(unit),
      ~test_status: Language.Id.t => option(TestStatus.t),
      term: Language.Exp.t,
    )
    : Node.t => {
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
          let n = live_label(n);
          let branch = n.o_children != [];
          let expanded = branch && !is_collapsed(path);
          [
            {
              r_path: path,
              r_node: n,
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
    );
  create(
    "details",
    ~attrs=[Attr.id("outline-sidebar"), Attr.create("open", "")],
    [
      create(
        "summary",
        ~attrs=[
          clss(
            ["outline-title"] @ (cursor == None ? ["outline-cursor"] : []),
          ),
        ],
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
        menu_view(~menu_close, ~def_op, ~zoom_in, ~is_module, (id, x, y))
      | _ => []
      }
    ),
  );
};
