open Virtual_dom.Vdom;
open Node;
open Util.Web;

let context_entry_view =
    ((name: string, {typ, _}: Core.Typ.Ctx.entry)): Node.t =>
  div(
    [clss(["context-entry"])],
    [text(name), text(":"), Type.view(typ)],
  );

let ctxc = "context-entries";

let exp_ctx_view = (ctx: Core.Typ.Ctx.t): Node.t =>
  div([clss([ctxc, "exp"])], List.map(context_entry_view, ctx));

let pat_ctx_view = (ctx: Core.Typ.Ctx.t): Node.t =>
  div([clss([ctxc, "pat"])], List.map(context_entry_view, ctx));

let ctx_sorts_view = (ci: Core.Statics.t): Node.t => {
  switch (ci) {
  | Invalid(_) => div([clss([ctxc, "invalid"])], [text("")])
  | InfoExp({ctx, _}) => exp_ctx_view(ctx)
  | InfoPat({ctx, _}) => pat_ctx_view(ctx)
  | InfoTyp(_) => div([clss([ctxc, "typ"])], [])
  };
};

let inspector_view =
    (~settings: Model.settings, _id: int, ci: Core.Statics.t): Node.t => {
  let clss =
    clss(
      ["context-inspector"] @ (settings.context_inspector ? ["visible"] : []),
    );
  div([clss], [ctx_sorts_view(ci)]);
};

let view =
    (
      ~settings: Model.settings,
      index': option(int),
      info_map: Core.Statics.map,
    ) => {
  let (index, ci) =
    switch (index') {
    | Some(index) => (index, Core.Id.Map.find_opt(index, info_map))
    | None => ((-1), None)
    };
  switch (ci) {
  | None => div([clss(["context-inspector"])], [text("No Static Data")])
  | Some(ci) => inspector_view(~settings, index, ci)
  };
};
