open Virtual_dom.Vdom;
open Node;
open Util.Web;

let jump_to = entry =>
  UpdateAction.PerformAction(Jump(TileId(Haz3lcore.Ctx.get_id(entry))));

let context_entry_view = (~inject, entry: Haz3lcore.Ctx.entry): Node.t => {
  let div_name =
    div(
      ~attrs=[clss(["name"]), Attr.on_click(_ => inject(jump_to(entry)))],
    );
  switch (entry) {
  | VarEntry({name, typ, _})
  | ConstructorEntry({name, typ, _}) =>
    div_c(
      "context-entry",
      [
        div_name([text(name)]),
        div(~attrs=[clss(["seperator"])], [text(":")]),
        Type.view(typ),
      ],
    )
  | TVarEntry({name, kind, _}) =>
    div_c(
      "context-entry",
      [
        div_name([Type.alias_view(name)]),
        div(~attrs=[clss(["seperator"])], [text("::")]),
        Kind.view(kind),
      ],
    )
  };
};

let ctx_view = (~inject, ctx: Haz3lcore.Ctx.t): Node.t =>
  div(
    ~attrs=[clss(["context-entries"])],
    List.map(
      context_entry_view(~inject),
      ctx |> Haz3lcore.Ctx.filter_duplicates |> List.rev,
    ),
  );

let ctx_sorts_view = (~inject, ci: Haz3lcore.Statics.Info.t) =>
  Haz3lcore.Info.ctx_of(ci)
  |> Haz3lcore.Ctx.filter_duplicates
  |> List.rev
  |> List.map(context_entry_view(~inject));

let view =
    (~inject, ~settings: Settings.t, ci: Haz3lcore.Statics.Info.t): Node.t => {
  let clss =
    clss(
      ["context-inspector"] @ (settings.context_inspector ? ["visible"] : []),
    );
  div(~attrs=[clss], ctx_sorts_view(~inject, ci));
};
