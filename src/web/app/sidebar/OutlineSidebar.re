open Virtual_dom.Vdom;
open Node;

/* OutlineSidebar — the collapsible module/definition outline.
   Navigation: click = jump. Focus: the ⊙ button TOGGLES a definition
   in the focus STACK (stacked header/body cells replace the master
   editor); plain click while a stack is open replaces the stack with
   that one definition. The banner splices everything home. */

let clss = cs => Attr.classes(cs);

let kind_glyph = (k: OutlineTree.kind): string =>
  switch (k) {
  | KModule => {js|⛁|js}
  | KFn => {js|ƒ|js}
  | KConst => {js|·|js}
  | KType => {js|τ|js}
  };

let kind_cls = (k: OutlineTree.kind): string =>
  switch (k) {
  | KModule => "ol-module"
  | KFn => "ol-fn"
  | KConst => "ol-const"
  | KType => "ol-type"
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
          ~focused_entries: list((Language.Id.t, option(string))),
          n: OutlineTree.node,
        )
        : Node.t => {
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
            @ (stacked ? ["outline-focused"] : []),
          ),
        ]
        @ (
          switch (n.o_id) {
          /* while a stack is open, a plain click REPLACES the stack
             (jumping at master ids would target the hidden editor) */
          | Some(id) when any_focus => [Attr.on_click(_ => focus(id))]
          | Some(id) => [Attr.on_click(_ => jump(id))]
          | None => []
          }
        ),
      [
        span(
          ~attrs=[clss(["outline-glyph"])],
          [text(kind_glyph(n.o_kind))],
        ),
        text(n.o_label),
      ]
      @ (
        switch (n.o_id) {
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
                  Effect.Many([Effect.Stop_propagation, toggle(id)])
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
      ~attrs=[clss(["outline-branch"]), Attr.create("open", "")],
      [
        create("summary", ~attrs=[clss(["outline-summary"])], [label]),
        div(
          ~attrs=[clss(["outline-kids"])],
          List.map(
            node_view(~jump, ~focus, ~toggle, ~focused_entries),
            kids,
          ),
        ),
      ],
    )
  };
};

let view =
    (
      ~jump: Language.Id.t => Effect.t(unit),
      ~focus: Language.Id.t => Effect.t(unit),
      ~toggle: Language.Id.t => Effect.t(unit),
      ~unfocus: Effect.t(unit),
      ~focused_entries: list((Language.Id.t, option(string))),
      term: Language.Exp.t,
    )
    : Node.t => {
  let roots = OutlineTree.of_term(term);
  let banner =
    switch (focused_entries) {
    | [] => []
    | [_, ..._] => [
        div(
          ~attrs=[clss(["outline-unfocus"]), Attr.on_click(_ => unfocus)],
          [text({js|✕ close all — whole program|js})],
        ),
      ]
    };
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
                node_view(~jump, ~focus, ~toggle, ~focused_entries),
                roots,
              )
        ),
      ),
    ],
  );
};
