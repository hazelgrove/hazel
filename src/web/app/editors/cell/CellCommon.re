open Virtual_dom.Vdom;
open Node;

/* Helpers for creating cell ui components - mostly used by exercise mode */

let narrative_cell = (content: Node.t) =>
  div(
    ~attrs=[Attr.class_("cell")],
    [div(~attrs=[Attr.class_("cell-chapter")], [content])],
  );

let simple_cell_item = (content: list(Node.t)) =>
  div(~attrs=[Attr.classes(["cell-item"])], content);

let caption = (~rest: option(string)=?, bolded: string) =>
  div(
    ~attrs=[Attr.classes(["cell-caption"])],
    [strong([text(bolded)])] @ (rest |> Option.map(text) |> Option.to_list),
  );

let simple_cell_view = (items: list(t)) =>
  div(~attrs=[Attr.class_("cell")], items);

let report_footer_view = content => {
  div(~attrs=[Attr.classes(["cell-item", "cell-report"])], content);
};

let panel = (~classes=[], content, ~footer: option(t)) => {
  simple_cell_view(
    [
      div(~attrs=[Attr.classes(["cell-item", "panel"] @ classes)], content),
    ]
    @ Option.to_list(footer),
  );
};

let title_cell = title => {
  simple_cell_view([
    div(
      ~attrs=[Attr.class_("title-cell")],
      [div(~attrs=[Attr.class_("title-text")], [text(title)])],
    ),
  ]);
};

let wrong_impl_caption = (~inject_delete, sub: string, n: int) => {
  div(
    ~attrs=[Attr.class_("wrong-impl-cell-caption")],
    [
      caption("", ~rest=sub),
      div(
        ~attrs=[
          Attr.class_("instructor-edit-icon"),
          Attr.on_mousedown(_ =>
            Virtual_dom.Vdom.Effect.(
              Many([Prevent_default, Stop_propagation])
            )
          ),
        ],
        [
          Widgets.button(
            Icons.delete,
            _ => inject_delete(n),
            ~tooltip="Delete Buggy Implementation",
          ),
        ],
      ),
    ],
  );
};
