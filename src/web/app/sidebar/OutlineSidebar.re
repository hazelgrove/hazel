open Virtual_dom.Vdom;
open Node;

/* OutlineSidebar — the collapsible module/definition outline
   (plans/modular-editors.md §1). v1: read-only navigation. Expansion
   state rides native <details>/<summary> (no model state, no
   re-render churn); clicking a name jumps the caret to the
   definition. The whole panel is itself a details element docked on
   the left, collapsed by default. */

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
        (~jump: Language.Id.t => Effect.t(unit), n: OutlineTree.node): Node.t => {
  let label =
    div(
      ~attrs=
        [clss(["outline-label", kind_cls(n.o_kind)])]
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
      ],
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
          List.map(node_view(~jump), kids),
        ),
      ],
    )
  };
};

let view =
    (~jump: Language.Id.t => Effect.t(unit), term: Language.Exp.t): Node.t => {
  let roots = OutlineTree.of_term(term);
  create(
    "details",
    ~attrs=[Attr.id("outline-sidebar")],
    [
      create(
        "summary",
        ~attrs=[clss(["outline-title"])],
        [text({js|☰ outline|js})],
      ),
      div(
        ~attrs=[clss(["outline-body"])],
        roots == []
          ? [
            div(
              ~attrs=[clss(["outline-empty"])],
              [text("no definitions")],
            ),
          ]
          : List.map(node_view(~jump), roots),
      ),
    ],
  );
};
