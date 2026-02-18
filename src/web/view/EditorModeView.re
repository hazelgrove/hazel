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

let indicator_select =
    (~signal: int => Effect.t(unit), cur_slide, slide_names): list(t) => {
  open Util;

  let split_filepath = (s: string): list(string) =>
    String.split_on_char('/', s) |> List.map(String.trim, _);

  // Decompose slide names into a list of tuples (index, list of path components)
  let slides_decomposed: list((int, list(string))) =
    List.map(split_filepath, slide_names) |> List.mapi((i, n) => (i, n));

  // Get the path components of the current slide
  let parts: list(string) = List.nth(slides_decomposed, cur_slide) |> snd;

  // Iterate over each path component of the current slide
  parts
  |> List.mapi((prefix_depth, slide_segment: string) => {
       // Take the prefix of the path up to the current depth
       let prefix = ListUtil.take(prefix_depth, parts);

       // Find all slides that match the current prefix
       let matching_names =
         slides_decomposed
         |> List.to_seq
         // Filter slides that share the same prefix
         |> Seq.filter(((_, parts': list(string))) => {
              ListUtil.take(prefix_depth, parts') == prefix
            })
         // Map the matching slides to their index and the current path component
         |> Seq.map(PairUtil.map_snd(List.nth(_, prefix_depth)))
         // Deduplicate the matching names based on the path component
         |> List.of_seq
         |> Util.ListUtil.dedup_f(((_, a), (_, b)) => a == b, _);

       // Create a dropdown (select element) for the current path component
       select(
         ~attrs=[
           Attr.value(slide_segment),
           // Signal the selected slide index when the dropdown value changes
           Attr.on_change((_, name) => {
             signal(
               List.find_opt(((_, n)) => n == name, matching_names)
               |> Option.get
               |> fst,
             )
           }),
         ],
         {
           List.map(
             ((_, name: string)) => {
               option_view(name == slide_segment, name)
             },
             matching_names,
           );
         },
       );
     })
  |> Util.ListUtil.intersperse(text("/"));
};
