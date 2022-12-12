open Virtual_dom.Vdom;
open Node;
open Util.Web;

// TODO: (poly) Finish the skeleton

// TODO: (poly) Finish the skeleton
let view = (ty: Haz3lcore.Kind.t): list(Node.t) =>
  switch (ty) {
  | Singleton(ty) => [
      text("::"),
      div(
        ~attr=clss(["kind-view"]),
        [div(~attr=clss(["kind-view", "Singleton"]), [Type.view(ty)])],
      ),
    ]
  };

let view_entry = (name, kind) => [
  text("type "),
  text(name),
  text(" "),
  ...view(kind),
];
