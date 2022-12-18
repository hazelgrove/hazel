open Virtual_dom.Vdom;
open Node;
open Util.Web;

let context_entry_view = (~inject, entry: Haz3lcore.Ctx.entry): Node.t =>
  div(
    ~attr=
      Attr.many([
        clss(["context-entry"]),
        Attr.on_click(_ =>
          inject(
            UpdateAction.PerformAction(
              Jump(TileId(Haz3lcore.Ctx.get_id(entry))),
            ),
          )
        ),
      ]),
    switch (entry) {
    | VarEntry({name, typ, _}) => [text(name), text(":"), Type.view(typ)]
    | TVarEntry({name, kind, _}) => [
        text("type "),
        text(name),
        text("::"),
        Kind.view(kind),
      ]
    },
  );

let ctxc = "context-entries";

let exp_ctx_view = (~inject, ctx: Haz3lcore.Ctx.t): Node.t => {
  let ctx = ctx |> Haz3lcore.Ctx.filter_duplicates;
  div(
    ~attr=clss([ctxc, "exp"]),
    List.map(context_entry_view(~inject), List.rev(ctx)),
  );
};

let pat_ctx_view = (~inject, ctx: Haz3lcore.Ctx.t): Node.t => {
  let ctx = ctx |> Haz3lcore.Ctx.filter_duplicates;
  div(
    ~attr=clss([ctxc, "pat"]),
    List.map(context_entry_view(~inject), List.rev(ctx)),
  );
};

let ctx_sorts_view = (~inject, ci: Haz3lcore.Statics.t): Node.t => {
  switch (ci) {
  | Invalid(_) => div(~attr=clss([ctxc, "invalid"]), [text("")])
  | InfoExp({ctx, _}) => exp_ctx_view(~inject, ctx)
  | InfoPat({ctx, _}) => pat_ctx_view(~inject, ctx)
  | InfoTyp(_) => div(~attr=clss([ctxc, "typ"]), [])
  | InfoRul(_) => div(~attr=clss([ctxc, "rul"]), [])
  };
};

let inspector_view =
    (
      ~inject,
      ~settings: Model.settings,
      _id: Haz3lcore.Id.t,
      ci: Haz3lcore.Statics.t,
    )
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
      index': option(Haz3lcore.Id.t),
      info_map: Haz3lcore.Statics.map,
    ) => {
  let (index, ci) =
    switch (index') {
    | Some(index) => (index, Haz3lcore.Id.Map.find_opt(index, info_map))
    | None => (Haz3lcore.Id.invalid, None)
    };
  switch (ci) {
  | None =>
    div(~attr=clss(["context-inspector"]), [text("No Static Data")])
  | Some(ci) => inspector_view(~inject, ~settings, index, ci)
  };
};
