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
  text(Stdlib.Printf.sprintf("%d / %d", cur_slide + 1, num_slides)),
];

let indicator_select =
    (~signal: int => Effect.t(unit), cur_slide, slide_names): list(t) => {
  open Util;

  let split_filepath = (s: string): list(string) =>
    String.split(s, ~on='/') |> List.map(~f=String.strip, _);

  // Decompose slide names into a list of tuples (index, list of path components)
  let slides_decomposed: list((int, list(string))) =
    List.map(~f=split_filepath, slide_names)
    |> List.mapi(~f=(i, n) => (i, n));

  // Get the path components of the current slide
  let parts: list(string) =
    List.nth_exn(slides_decomposed, cur_slide) |> snd;

  // Iterate over each path component of the current slide
  parts
  |> List.mapi(~f=(prefix_depth, slide_segment: string) => {
       // Take the prefix of the path up to the current depth
       let prefix = ListUtil.take(prefix_depth, parts);

       // Find all slides that match the current prefix
       let matching_names =
         slides_decomposed
         |> Stdlib.List.to_seq
         // Filter slides that share the same prefix
         |> Stdlib.Seq.filter(((_, parts': list(string))) => {
              List.equal(
                String.equal,
                ListUtil.take(prefix_depth, parts'),
                prefix,
              )
            })
         // Map the matching slides to their index and the current path component
         |> Stdlib.Seq.map(PairUtil.map_snd(List.nth_exn(_, prefix_depth)))
         // Deduplicate the matching names based on the path component
         |> Stdlib.List.of_seq
         |> Util.ListUtil.dedup_f(
              ((_, a), (_, b)) => String.equal(a, b),
              _,
            );

       // Create a dropdown (select element) for the current path component
       select(
         ~attrs=[
           // Signal the selected slide index when the dropdown value changes
           Attr.on_change((_, name) => {
             signal(
               List.find(
                 ~f=((_, n)) => String.equal(n, name),
                 matching_names,
               )
               |> Option.value_exn
               |> fst,
             )
           }),
         ],
         {
           List.map(
             ~f=
               ((_, name: string)) => {
                 option_view(String.equal(name, slide_segment), name)
               },
             matching_names,
           );
         },
       );
     })
  |> Util.ListUtil.intersperse(text("/"));
};
