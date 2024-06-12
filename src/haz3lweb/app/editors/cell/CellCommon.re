open Virtual_dom.Vdom;
open Node;

/* Helpers for creating cell ui components - mostly used by exercise mode */

let narrative_cell = (content: Node.t) =>
  div(
    ~attr=Attr.class_("cell"),
    [div(~attr=Attr.class_("cell-chapter"), [content])],
  );

let simple_cell_item = (content: list(Node.t)) =>
  div(~attr=Attr.classes(["cell-item"]), content);

let caption = (~rest: option(string)=?, bolded: string) =>
  div(
    ~attr=Attr.many([Attr.classes(["cell-caption"])]),
    [strong([text(bolded)])] @ (rest |> Option.map(text) |> Option.to_list),
  );

let simple_cell_view = (items: list(t)) =>
  div(~attr=Attr.class_("cell"), items);

let report_footer_view = content => {
  div(~attr=Attr.classes(["cell-item", "cell-report"]), content);
};

let panel = (~classes=[], content, ~footer: option(t)) => {
  simple_cell_view(
    [div(~attr=Attr.classes(["cell-item", "panel"] @ classes), content)]
    @ Option.to_list(footer),
  );
};

let title_cell = title => {
  simple_cell_view([
    div(
      ~attr=Attr.class_("title-cell"),
      [div(~attr=Attr.class_("title-text"), [text(title)])],
    ),
  ]);
};
