open Virtual_dom.Vdom;

type t = {
  caption: string,
  chapter: option(Node.t),
  initial: string,
};
