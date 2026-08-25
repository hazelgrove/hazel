open Virtual_dom.Vdom;
open Node;

/* OutlineSidebar — the collapsible module/definition outline
   (plans/modular-editors.md §1). Phase 1: navigation (click = jump).
   Phase 2: per-definition FOCUS (the ⊙ button swaps the editor to
   just that definition; the banner splices it back). Expansion state
   rides native <details>/<summary> — no model state. */

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

let rec node_view =
        (
          ~jump: Language.Id.t => Effect.t(unit),
          ~focus: Language.Id.t => Effect.t(unit),
          ~focused: option(Language.Id.t),
          n: OutlineTree.node,
        )
        : Node.t => {
  let is_focused = n.o_id != None && n.o_id == focused;
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
                clss(["outline-focus-btn"]),
                Attr.title("focus this definition"),
                Attr.on_click(_ =>
                  Effect.Many([Effect.Stop_propagation, focus(id)])
                ),
              ],
              [text({js|⊙|js})],
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
