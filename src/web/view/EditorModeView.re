open Virtual_dom.Vdom;
open Node;
open Widgets;
open Util;

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
      ~signal: event => 'a,
      ~indicator: list(Node.t),
    ) => {
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

let indicator_select = (~signal: int => 'a, cur_slide, names): list(t) => {
  let break = s => String.split_on_char('/', s) |> List.map(String.trim, _);

  let names_split: list((int, list(string))) =
    List.map(break, names) |> List.mapi((i, n) => (i, n));
  let current: string = List.nth(names, cur_slide);
  let parts = break(current);
  List.to_seq(parts)
  |> Seq.mapi(
       (prefix_depth, current: string) => {
         let prefix = ListUtil.take(prefix_depth, parts);
         let matching_names =
           List.filter(
             ((_, n: list(string))) => {
               Util.ListUtil.take(prefix_depth, n) == prefix
             },
             names_split,
           )
           |> List.map(((idx, n: list(string))) => {
                (idx, List.nth(n, prefix_depth))
              })
           |> Util.ListUtil.dedup_f(((_, a), (_, b)) => a == b, _);
         select(
           ~attrs=[
             Attr.on_change((_, name) => {
               signal(
                 List.find_opt(((_, n)) => n == name, matching_names)  // TODO This doesn't deal with duplicate names. We should prohibit those or make it work
                 |> Option.get
                 |> fst,
               )
             }),
           ],
           {
             List.map(
               ((_, name: string)) => {option_view(name == current, name)}, // TODO Handle duplicates
               matching_names,
             );
           },
         );
       },
       _,
     )
  |> List.of_seq
  |> Util.ListUtil.intersperse(text("/"));
};
