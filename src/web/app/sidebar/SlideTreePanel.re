open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

type context = {
  current: int,
  paths: list(SlidePath.t),
  select: int => Effect.t(unit),
};

let rec holds = (current: int, tree: SlidePath.tree): bool =>
  switch (tree) {
  | Leaf(i, _) => i == current
  | Folder(_, children) => List.exists(holds(current), children)
  };

/* `folders` is the path of the folder `tree` sits in, which is the key its
   own folders are opened and closed under. */
let rec tree_view =
        (
          ~globals: Globals.t,
          ~context: context,
          ~folders: list(string),
          tree: SlidePath.tree,
        )
        : Node.t =>
  switch (tree) {
  | Leaf(i, name) =>
    div(
      ~attrs=[
        clss(
          ["slide-tree-leaf"] @ (i == context.current ? ["current"] : []),
        ),
        Attr.on_click(_ => context.select(i)),
      ],
      [text(name)],
    )
  | Folder(name, children) =>
    let folders = folders @ [name];
    let collapsed =
      SidebarModel.Settings.is_slide_folder_collapsed(
        folders,
        globals.settings.sidebar,
      );
    /* A closed folder can't show the current slide, so it marks itself. */
    let holds_current =
      collapsed && List.exists(holds(context.current), children);
    div(
      ~attrs=[clss(["slide-tree-folder"])],
      [
        div(
          ~attrs=[
            clss(
              ["slide-tree-folder-name"]
              @ (holds_current ? ["holds-current"] : []),
            ),
            Attr.on_click(_ =>
              globals.inject_global(
                Set(Sidebar(ToggleSlideFolder(folders))),
              )
            ),
          ],
          [text((collapsed ? "▸ " : "▾ ") ++ name)],
        ),
      ]
      @ (
        collapsed
          ? []
          : [
            div(
              ~attrs=[clss(["slide-tree-children"])],
              List.map(tree_view(~globals, ~context, ~folders), children),
            ),
          ]
      ),
    );
  };

let view = (~globals: Globals.t, context: context): Node.t =>
  div(
    ~attrs=[clss(["slide-tree"])],
    SlidePath.tree(context.paths)
    |> List.map(tree_view(~globals, ~context, ~folders=[])),
  );
