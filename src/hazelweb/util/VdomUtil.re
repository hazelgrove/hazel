module Vdom = Virtual_dom.Vdom;
module Node = Virtual_dom.Vdom.Node;

/* more meaningful type aliases */
type tag = string;
type key = string;
type value = string;
type attr = (key, value);
type children = list(Node.t);
type mapper = (tag, list(attr), children) => option(Node.t);

type html =
  | Text(string)
  | Element(string, list(attr), list(html));

let parse_html = s =>
  Markup.(
    string(s)
    |> parse_html
    |> (
      x =>
        {
          print_endline("0");
          x;
        }
        |> signals
        |> (
          x =>
            {
              print_endline("1");
              x;
            }
            |> tree(
                 ~text=_ss => {Text("nada")},
                 ~element=
                   ((_, name), _attrs, children) => {
                     /* let simple_attrs =
                        List.map(
                          (((_, key), value)) => (key, value),
                          attrs,
                        ); */
                     Element(
                       name,
                       [],
                       children,
                     )
                   },
               )
        )
    )
  );

let rec _node_of_html = h =>
  switch (h) {
  | Text(s) => Node.text(s)
  | Element(tag, attrs, children) =>
    print_endline(tag);
    let node_attrs =
      List.map(((key, value)) => Vdom.Attr.create(key, value), attrs);
    let children_nodes = List.map(_node_of_html, children);
    Node.create(tag, node_attrs, children_nodes);
  };

let parsemap = (_s, _f) => {
  print_endline(_s);
  let s = "<span class=\"slider-livelit\"><input class=\"slider\" id=\"llu\" type=\"range\" min=0 max=100 value=50></input></span>";
  switch (parse_html(String.trim(s))) {
  | None => None
  | Some(_h) =>
    print_endline("got an html!!");
    failwith("End");
  /* Some(node_of_html(h)); */
  };
};

/* let parsemap = (s, _f) => {
     print_endline("A" ++ s);
     let a =
       Markup.string(
         "<span class=\"slider-livelit\">
         <input type=\"range\" min=0 max=100 value=50 oninput=\"alert('abc')\">\n</input></span>",
       );
     parse_test(a)
     print_endline("B");
     let b = Markup.parse_html(a);
     print_endline("C");
     let c = Markup.signals(b) |> Markup.html5 |> Markup.trim;
     print_endline("D");
     let d =
       Markup.tree(
         ~text=
           ss => {
             let data = String.concat("", ss);
             print_endline("data=" ++ data);
             Node.text(data);
           },
         ~element=
           ((_, name), attrs, children) => {
             print_endline("B" ++ name);
             print_endline(string_of_int(List.length(attrs)));
             print_endline("num children = " ++ List.length(children));
             let simple_attrs =
               List.map((((_, key), value)) => (key, value), attrs);
             print_endline("B1");
             /* switch (f(name, simple_attrs, children)) { */
             switch (None) {
             | Some(node) => node
             | None =>
               print_endline("B2");
               let node_attrs =
                 List.map(
                   ((key, value)) => Vdom.Attr.create(key, value),
                   simple_attrs,
                 );
               print_endline("B3");
               let n = Node.create(name, node_attrs, children);
               print_endline("B4");
               n;
             };
           },
         c,
       );
     print_endline("E");
     d;
   }; */
let parse = s => parsemap(s, (_, _, _) => None);

type css_field = string;
let css_float_px = (field, px) =>
  Css_gen.create(~field, ~value=string_of_float(px) ++ "px");
