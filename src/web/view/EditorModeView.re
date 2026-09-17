open Virtual_dom.Vdom;
open Node;
open Widgets;

let option_view = (selected: bool, n) =>
  option(
    ~attrs=selected ? [Attr.create("selected", "selected")] : [],
    [text(n)],
  );

type event =
  | Previous
  | Next
  | Add
  | Rename
  | Delete;

let view =
    (
      ~nav_buttons: bool,
      ~edit_buttons: bool,
      ~extra_edit_buttons: list(Node.t)=[],
      ~unit_name: string="Slide",
      ~add_tooltip: option(string)=?,
      ~signal: event => 'a,
      ~indicator: list(Node.t),
      (),
    ) => {
  let add_tooltip =
    Option.value(add_tooltip, ~default="Add New " ++ unit_name);
  let edit_buttons_list =
    [
      button(~tooltip="Rename Current " ++ unit_name, Icons.rename, _ =>
        signal(Rename)
      ),
      button(~tooltip="Delete Current " ++ unit_name, Icons.trash, _ =>
        signal(Delete)
      ),
      button(~tooltip=add_tooltip, Icons.new_buffer, _ => signal(Add)),
    ]
    @ extra_edit_buttons;

  [
    div(
      ~attrs=[Attr.id("slide-navigation")],
      (nav_buttons ? [button(Icons.back, _ => signal(Previous))] : [])
      @ indicator
      @ (nav_buttons ? [button(Icons.forward, _ => signal(Next))] : [])
      @ (
        edit_buttons
          ? [div(~attrs=[Attr.id("edit-buttons")], edit_buttons_list)] : []
      ),
    ),
  ];
};

let indicator_n = (cur_slide, num_slides) => [
  text(Printf.sprintf("%d / %d", cur_slide + 1, num_slides)),
];

let indicator_select =
    (~signal: int => Effect.t(unit), cur_slide, paths: list(SlidePath.t))
    : list(t) => {
  SlidePath.breadcrumb(~current=cur_slide, paths)
  |> List.map(({selected, options}: SlidePath.crumb) =>
       select(
         ~attrs=[
           // drive the <select> from the model via the `value` property: the
           // <option selected> attr alone doesn't move a live select on
           // programmatic (arrow/keyboard) slide change
           Attr.string_property("value", selected),
           // Signal the selected slide index when the dropdown value changes
           Attr.on_change((_, name) =>
             switch (List.find_opt(((_, n)) => n == name, options)) {
             | Some((i, _)) => signal(i)
             | None => Effect.Ignore
             }
           ),
         ],
         List.map(
           ((_, name: string)) => option_view(name == selected, name),
           options,
         ),
       )
     )
  |> Util.ListUtil.intersperse(text("/"));
};
