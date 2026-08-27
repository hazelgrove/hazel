open Virtual_dom.Vdom;
open Node;

/* OutlineSidebar — the collapsible module/definition outline.
   Navigation: click = jump. Focus: the ⊙ button TOGGLES a definition
   in the focus STACK (stacked header/body cells replace the master
   editor); plain click while a stack is open ADDS that definition to
   the stack (or moves to it if present) — it never replaces the
   stack. The banner splices everything home. */

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
  | Duplicate
  | MoveUp
  | MoveDown
  | Delete;

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
          ~jump: Language.Id.t => Effect.t(unit),
          ~focus: Language.Id.t => Effect.t(unit),
          ~toggle: Language.Id.t => Effect.t(unit),
          ~toggle_run: Language.Id.t => Effect.t(unit),
          ~top_level: bool,
          ~menu_open: (Language.Id.t, float, float) => Effect.t(unit),
          ~error_subtree: list(Language.Id.t),
          ~focused_entries: list((Language.Id.t, option(string))),
          ~error_items: list(Language.Id.t),
          ~test_status: Language.Id.t => option(TestStatus.t),
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
  let label =
    div(
      ~attrs=
        [
          clss(
            ["outline-label", kind_cls(n.o_kind)]
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
          /* while a stack is open, a plain click ADDS/moves-to that
             cell (jumping at master ids would target the hidden
             editor). Prevent_default: label clicks must not toggle
             the row's <details> (collapse is the chevron's job). */
          | Some(id) when any_focus => [
              Attr.on_click(_ =>
                Effect.Many([Effect.Prevent_default, focus(id)])
              ),
            ]
          | Some(id) => [
              Attr.on_click(_ =>
                Effect.Many([Effect.Prevent_default, jump(id)])
              ),
            ]
          | None => []
          }
        )
        @ (
          /* structural ops are TOP-LEVEL only for now (member
             granularity is docketed) */
          switch (n.o_id) {
          | Some(id) when top_level && n.o_kind != OutlineTree.KTrail => [
              Attr.on_contextmenu(evt => {
                let x =
                  float_of_int(Js_of_ocaml.Js.Unsafe.coerce(evt)##.clientX);
                let y =
                  float_of_int(Js_of_ocaml.Js.Unsafe.coerce(evt)##.clientY);
                Effect.Many([Effect.Prevent_default, menu_open(id, x, y)]);
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
    create(
      "details",
      ~attrs=
        [clss(["outline-branch"]), Attr.create("open", "")]
        @ (
          /* pin↔collapse sync (ScratchMode pokes details.open) */
          switch (n.o_id) {
          | Some(id) => [Attr.id("ol-b-" ++ Language.Id.to_string(id))]
          | None => []
          }
        ),
      [
        create("summary", ~attrs=[clss(["outline-summary"])], [label]),
        div(
          ~attrs=[clss(["outline-kids"])],
          List.map(
            node_view(
              ~jump,
              ~focus,
              ~toggle,
              ~toggle_run,
              ~top_level=false,
              ~menu_open,
              ~error_subtree,
              ~focused_entries,
              ~error_items,
              ~test_status,
            ),
            kids,
          ),
        ),
      ],
    )
  };
};

let menu_view =
    (
      ~menu_close: Effect.t(unit),
      ~def_op: (def_op, Language.Id.t) => Effect.t(unit),
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
        [
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

let view =
    (
      ~jump: Language.Id.t => Effect.t(unit),
      ~focus: Language.Id.t => Effect.t(unit),
      ~toggle: Language.Id.t => Effect.t(unit),
      ~toggle_run: Language.Id.t => Effect.t(unit),
      ~unfocus: Effect.t(unit),
      ~focused_entries: list((Language.Id.t, option(string))),
      ~error_items: list(Language.Id.t),
      ~error_subtree: list(Language.Id.t),
      ~menu: option((Language.Id.t, float, float)),
      ~menu_open: (Language.Id.t, float, float) => Effect.t(unit),
      ~menu_close: Effect.t(unit),
      ~def_op: (def_op, Language.Id.t) => Effect.t(unit),
      ~test_status: Language.Id.t => option(TestStatus.t),
      term: Language.Exp.t,
    )
    : Node.t => {
  let roots = OutlineTree.of_term(term);
  /* the banner slot is ALWAYS present: prepending it only while a
     stack is open shifted every sibling, and the positional vdom diff
     then recreated each <details> with its default-open attribute —
     pinning was expanding collapsed branches (andrew's bug report) */
  let banner = [
    switch (focused_entries) {
    | [] => div(~attrs=[clss(["outline-unfocus", "outline-hidden"])], [])
    | [_, ..._] =>
      div(
        ~attrs=[clss(["outline-unfocus"]), Attr.on_click(_ => unfocus)],
        [text({js|✕ close all — whole program|js})],
      )
    },
  ];
  create(
    "details",
    ~attrs=[Attr.id("outline-sidebar"), Attr.create("open", "")],
    [
      create(
        "summary",
        ~attrs=[clss(["outline-title"])],
        [text({js|☰ outline|js})],
      ),
      div(~attrs=[clss(["outline-resize"]), ...resize_attrs], []),
      div(
        ~attrs=[clss(["outline-body"])],
        banner
        @ (
          roots == []
            ? [
              div(
                ~attrs=[clss(["outline-empty"])],
                [text("no definitions")],
              ),
            ]
            : List.map(
                node_view(
                  ~jump,
                  ~focus,
                  ~toggle,
                  ~toggle_run,
                  ~top_level=true,
                  ~menu_open,
                  ~error_subtree,
                  ~focused_entries,
                  ~error_items,
                  ~test_status,
                ),
                roots,
              )
        ),
      ),
    ]
    @ (
      switch (menu) {
      | Some(m) => menu_view(~menu_close, ~def_op, m)
      | None => []
      }
    ),
  );
};
