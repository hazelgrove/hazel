open Virtual_dom.Vdom;
open Node;

/* OutlineSidebar — the collapsible module/definition outline
   (plans/modular-editors.md §1). Phase 1: navigation (click = jump).
   Phase 2: per-definition FOCUS (the ⊙ button swaps the editor to
   just that definition; the banner splices it back). Expansion state
   rides native <details>/<summary> — no model state. */

let clss = cs => Attr.classes(cs);

/* drag-to-resize: pointer capture on the edge handle writes the width
   to a ROOT css variable (--outline-w), so it survives re-renders and
   needs no model state */
let resize_attrs: list(Attr.t) = {
  Js_of_ocaml.[
    Attr.on_pointerdown(evt => {
      let target = Js.Unsafe.coerce(evt)##.target;
      let _ =
        Js.Unsafe.meth_call(
          target,
          "setPointerCapture",
          [|Js.Unsafe.get(Js.Unsafe.coerce(evt), "pointerId")|],
        );
      Effect.Prevent_default;
    }),
    Attr.on_mousemove(evt => {
      let buttons: int = Js.Unsafe.coerce(evt)##.buttons;
      if (buttons land 1 != 0) {
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
      };
      Effect.Ignore;
    }),
  ];
};

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

let rec node_view =
        (
          ~jump: Language.Id.t => Effect.t(unit),
          ~focus: Language.Id.t => Effect.t(unit),
          ~focused: option(Language.Id.t),
          n: OutlineTree.node,
        )
        : Node.t => {
  let is_focused = n.o_id != None && n.o_id == focused;
  /* type items have no Exp-rooted body to focus (their RHS is a TYPE);
     typ-rooted cells are future work */
  let focusable = n.o_kind != OutlineTree.KType;
  let label =
    div(
      ~attrs=
        [
          clss(
            ["outline-label", kind_cls(n.o_kind)]
            @ (is_focused ? ["outline-focused"] : []),
          ),
        ]
        @ (
          switch (n.o_id) {
          /* while focused, a plain click RETARGETS the focus: jumping
             at master ids would raise (they don't exist in the cell) */
          | Some(id) when focused != None && focusable => [
              Attr.on_click(_ => focus(id)),
            ]
          | Some(id) when focused == None => [Attr.on_click(_ => jump(id))]
          | Some(_)
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
        | Some(id) when focusable => [
            span(
              ~attrs=[
                clss(["outline-focus-btn"]),
                Attr.title("focus this definition"),
                Attr.on_click(_ =>
                  Effect.Many([Effect.Stop_propagation, focus(id)])
                ),
              ],
              [text({js|⊙|js})],
            ),
          ]
        | Some(_)
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
          List.map(node_view(~jump, ~focus, ~focused), kids),
        ),
      ],
    )
  };
};

let view =
    (
      ~jump: Language.Id.t => Effect.t(unit),
      ~focus: Language.Id.t => Effect.t(unit),
      ~unfocus: Effect.t(unit),
      ~focused: option(Language.Id.t),
      term: Language.Exp.t,
    )
    : Node.t => {
  let roots = OutlineTree.of_term(term);
  let banner =
    switch (focused) {
    | Some(_) => [
        div(
          ~attrs=[clss(["outline-unfocus"]), Attr.on_click(_ => unfocus)],
          [text({js|✕ unfocus — whole program|js})],
        ),
      ]
    | None => []
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
            : List.map(node_view(~jump, ~focus, ~focused), roots)
        ),
      ),
    ],
  );
};
