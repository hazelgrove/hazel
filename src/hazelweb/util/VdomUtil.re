module Vdom = Virtual_dom.Vdom;
module Node = Virtual_dom.Vdom.Node;

/* more meaningful type aliases */
type tag = string;
type key = string;
type value = string;
type attr = (key, value);
type children = list(Node.t);
type mapper = (tag, list(attr), children) => option(Node.t);

let parsemap = (s, f) =>
  Markup.(
    string(s)
    |> parse_html
    |> signals
    |> tree(
         ~text=
           ss => {
             let data = String.concat("", ss);
             Node.text(data);
           },
         ~element=
           ((_, name), attrs, children) => {
             let simple_attrs =
               List.map((((_, key), value)) => (key, value), attrs);
             switch (f(name, simple_attrs, children)) {
             | Some(node) => node
             | None =>
               let node_attrs =
                 List.map(
                   ((key, value)) => Vdom.Attr.create(key, value),
                   simple_attrs,
                 );
               Node.create(name, node_attrs, children);
             };
           },
       )
  );

let parse = s => parsemap(s, (_, _, _) => None);
