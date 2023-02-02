open Virtual_dom.Vdom;
open Node;
open Util.Web;

let jump_to = entry =>
  UpdateAction.PerformAction(Jump(TileId(Haz3lcore.Ctx.get_id(entry))));

let context_entry_view = (~inject, entry: Haz3lcore.Ctx.entry): Node.t =>
  div(
    ~attr=
      Attr.many([
        clss(["context-entry"]),
        Attr.on_click(_ => inject(jump_to(entry))),
      ]),
    switch (entry) {
    | VarEntry({name, typ, _})
    | TagEntry({name, typ, _}) => Type.view_entry(name, typ)
    | TVarEntry({name, kind, _}) => Kind.view_entry(name, kind)
    },
  );

let div_ctx = div(~attr=clss(["context-entries"]));

let ctx_view = (~inject, ctx: Haz3lcore.Ctx.t): Node.t =>
  div_ctx(
    List.map(
      context_entry_view(~inject),
      ctx |> Haz3lcore.Ctx.filter_duplicates |> List.rev,
    ),
  );

let ctx_sorts_view = (~inject, ci: Haz3lcore.Info.t): Node.t => {
  switch (ci) {
  | InfoExp({ctx, _})
  | InfoPat({ctx, _})
  | InfoTyp({ctx, _})
  | InfoTPat({ctx, _}) => ctx_view(~inject, ctx)
  | Invalid(_) => div_ctx([])
  };
};

let inspector_view =
    (~inject, ~settings: Model.settings, _id: int, ci: Haz3lcore.Info.t)
    : Node.t => {
  let clss =
    clss(
      ["context-inspector"] @ (settings.context_inspector ? ["visible"] : []),
    );
  div(~attr=clss, [ctx_sorts_view(~inject, ci)]);
};

let view =
    (
      ~inject,
      ~settings: Model.settings,
      index': option(int),
      info_map: Haz3lcore.Statics.map,
    ) => {
  let (index, ci) =
    switch (index') {
    | Some(index) => (index, Haz3lcore.Id.Map.find_opt(index, info_map))
    | None => ((-1), None)
    };
  switch (ci) {
  | None =>
    div(~attr=clss(["context-inspector"]), [text("No Static Data")])
  | Some(ci) => inspector_view(~inject, ~settings, index, ci)
  };
};
