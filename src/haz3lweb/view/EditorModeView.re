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
  | Next;

let view = (~signal: event => 'a, ~indicator: list(Node.t)) =>
  [button(Icons.back, _ => signal(Previous))]
  @ indicator
  @ [button(Icons.forward, _ => signal(Next))];

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
