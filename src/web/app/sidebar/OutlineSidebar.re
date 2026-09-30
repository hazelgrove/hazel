open Virtual_dom.Vdom;
open Node;

/* OutlineSidebar — the collapsible module/definition outline.
   Navigation: click = jump. Focus: the ⊙ button TOGGLES a definition
   in the focus STACK (stacked header/body cells replace the master
   editor); plain click while a stack is open ADDS that definition to
   the stack (or moves to it if present) — it never replaces the
   stack. The banner splices everything home. Modes without a focus
   stack (~stack_controls=false) get navigation only: no ⊙ buttons, no
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
          ~occ: int, /* this node's occurrence among same-labeled siblings */
          ~menu_open: (Language.Id.t, bool, float, float) => Effect.t(unit),
          ~error_subtree: list(Language.Id.t),
          ~focused_entries: list((Language.Id.t, option(string))),
          ~error_items: list(Language.Id.t),
          ~test_status: Language.Id.t => option(TestStatus.t),
          ~cursor: option(OutlineTree.path),
          ~set_cursor: option(OutlineTree.path) => Effect.t(unit),
          n: OutlineTree.node,
        )
        : Node.t => {
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
  let row_path =
    path
    @ [
      OutlineTree.{
        s_label: n.o_label,
        s_occ: occ,
      },
    ];
  let label =
    div(
      ~attrs=
        [
          clss(
            ["outline-label", kind_cls(n.o_kind)]
            @ (cursor == Some(row_path) ? ["outline-cursor"] : [])
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
            Util.JsUtil.outline_has_focus()
              ? set_cursor(Some(row_path)) : Effect.Prevent_default
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
        text(n.o_label),
      ]
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
              [text(all_pinned ? {js|⊖|js} : {js|⊙|js})],
            ),
          ];
        | Some(id) when !stacked && !can_open(id) => [
            span(
              ~attrs=[
                clss(["outline-focus-btn", "outline-btn-disabled"]),
                Attr.title("finish the definition to open it"),
              ],
              [text({js|⊙|js})],
            ),
          ]
        | Some(id) => [
            span(
              ~attrs=[
                clss(
                  ["outline-focus-btn"] @ (stacked ? ["outline-btn-on"] : []),
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
              [text(stacked ? {js|⊖|js} : {js|⊙|js})],
            ),
          ]
        | None => []
        }
      ),
    );
  switch (n.o_children) {
  | [] => div(~attrs=[clss(["outline-leaf"])], [label])
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
        div(
          ~attrs=[clss(["outline-kids"])],
          List.map(
            ((kid, kocc)) =>
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
                ~occ=kocc,
                ~menu_open,
                ~error_subtree,
                ~focused_entries,
                ~error_items,
                ~test_status,
                ~cursor,
                ~set_cursor,
                kid,
              ),
            OutlineTree.with_occurrences(kids),
          ),
        ),
      ],
    );
  };
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
  let act =
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
          [text({js|⊖|js})],
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
        ((n: OutlineTree.node, occ)) => {
          let n = live_label(n);
          let path =
            prefix
            @ [
              OutlineTree.{
                s_label: n.o_label,
                s_occ: occ,
              },
            ];
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
        OutlineTree.with_occurrences(ns),
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
                Attr.on_blur(_ => Effect.Stop_propagation),
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
          : List.map(
              ((root, rocc)) =>
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
                  ~occ=rocc,
                  ~menu_open,
                  ~error_subtree,
                  ~focused_entries,
                  ~error_items,
                  ~test_status,
                  ~cursor,
                  ~set_cursor,
                  root,
                ),
              OutlineTree.with_occurrences(roots),
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
