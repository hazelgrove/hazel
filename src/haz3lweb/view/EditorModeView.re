open Virtual_dom.Vdom;
open Node;
open Widgets;
open Util;

let option_view = (name, n) =>
  option(
    ~attrs=n == name ? [Attr.create("selected", "selected")] : [],
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
      ~signal: event => 'a,
      ~indicator: list(Node.t),
    ) => {
  let navigation_buttons =
    nav_buttons
      ? [
        button(Icons.back, _ => signal(Previous)),
        button(Icons.forward, _ => signal(Next)),
      ]
      : [];

  let edit_buttons_list = [
    button(~tooltip="Rename Current Slide", Icons.rename, _ =>
      signal(Rename)
    ),
    button(~tooltip="Delete Current Slide", Icons.trash, _ => signal(Delete)),
    button(~tooltip="Add New Slide", Icons.new_buffer, _ => signal(Add)),
  ];

  [
    div(
      ~attrs=[Attr.id("slide-navigation")],
      indicator
      @ navigation_buttons
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

let indicator_select = (~signal: int => 'a, cur_slide, names) => [
  select(
    ~attrs=[
      Attr.on_change((_, name) =>
        signal(
          ListUtil.findi_opt(n => n == name, names) |> Option.get |> fst,
        )
      ),
    ],
    List.mapi(
      (i, name) => option_view(i == cur_slide ? name : name ++ "+", name),
      names,
    ),
  ),
];
