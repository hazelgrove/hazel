open Virtual_dom.Vdom;
open Node;

/* Helpers for creating cell UI components — used by exercise & tutorial
   modes. Cell layout (`.cell`, `.cell-item`, `.title-cell`, etc.) is
   defined in cell.css and stays bespoke; everything visual on the cell
   (captions, titles, buttons, surfaces) goes through Components. */

let narrative_cell = (content: Node.t) =>
  div(
    ~attrs=[Attr.class_("cell")],
    [div(~attrs=[Attr.class_("cell-chapter")], [content])],
  );

let simple_cell_item = (content: list(Node.t)) =>
  div(~attrs=[Attr.classes(["cell-item"])], content);

/* Caption — section label inside a cell. The bolded part uses the
   design-system h2 heading; the optional `~rest` is rendered as a
   non-uppercase Components.subtitle next to it in a Row. */
let caption = (~rest: option(string)=?, bolded: string) =>
  switch (rest) {
  | None => Components.heading(~level=`H2, [text(bolded)])
  | Some(r) =>
    Components.row([
      Components.heading(~level=`H2, [text(bolded)]),
      Components.subtitle([text(r)]),
    ])
  };

let simple_cell_view = (items: list(t)) =>
  div(~attrs=[Attr.class_("cell")], items);

/* Unlocked cell — used in derivation mode for cells that get the
   editable left-border accent. */
let unlocked_cell_view = (items: list(t)) =>
  div(~attrs=[Attr.classes(["cell", "unlocked"])], items);

let report_footer_view = content =>
  div(~attrs=[Attr.classes(["cell-item", "cell-report"])], content);

let panel = (~classes=[], content, ~footer: option(t)) =>
  simple_cell_view(
    [
      div(~attrs=[Attr.classes(["cell-item", "panel"] @ classes)], content),
    ]
    @ Option.to_list(footer),
  );

/* Title cell — large exercise title. Uses the design-system h1 heading
   inside the `.title-cell` layout wrapper. */
let title_cell = title =>
  simple_cell_view([
    div(
      ~attrs=[Attr.class_("title-cell")],
      [Components.heading(~level=`H1, [text(title)])],
    ),
  ]);

/* Wrong-impl caption — caption row with a trailing delete button.
   Used inline above each buggy implementation cell. */
let wrong_impl_caption = (~inject_delete, sub: string, n: int) =>
  div(
    ~attrs=[Attr.class_("wrong-impl-cell-caption")],
    [
      caption("", ~rest=sub),
      div(
        ~attrs=[
          Attr.class_("instructor-edit-icon"),
          Attr.on_mousedown(_ =>
            Effect.Many([Effect.Prevent_default, Effect.Stop_propagation])
          ),
        ],
        [
          Components.button(
            ~tooltip="Delete Buggy Implementation",
            [Icons.delete],
            _ => inject_delete(n),
          ),
        ],
      ),
    ],
  );

/* Add-impl button row — sits below the buggy implementations as a
   "+" button. Used only by CodeExerciseMode. */
let add_impl_caption = (~tooltip, ~action: Effect.t(unit)) =>
  div(
    ~attrs=[Attr.class_("wrong-impl-cell-caption")],
    [
      div(
        ~attrs=[
          Attr.class_("instructor-edit-icon"),
          Attr.id("add-icon"),
        ],
        [Components.button(~tooltip, [Icons.add], _ => action)],
      ),
    ],
  );

/* Empty-state placeholder — used when a cell has no contents to show
   (e.g. "No context available"). Renders a single subtitle line in
   a simple cell item. */
let empty_state = (message: string) =>
  simple_cell_item([Components.subtitle([text(message)])]);
