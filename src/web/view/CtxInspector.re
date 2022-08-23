open Virtual_dom.Vdom;
open Node;
open Util.Web;

let context_entry_view = (entry: Core.Typ.Ctx.entry): Node.t =>
  switch (entry) {
  | VarEntry({name, typ, _}) =>
    div(
      [clss(["context-var-entry"])],
      [text(name), text(":"), Type.view(typ)],
    )
  | TyVarEntry({name, _}) =>
    div([clss(["context-tyvar-entry"])], [text(name)])
  };

let ctxc = "context-entries";

let exp_ctx_view = (ctx: Core.Typ.Ctx.t): Node.t =>
  div(
    [clss([ctxc, "exp"])],
    ctx |> Core.Typ.Ctx.entries |> List.map(context_entry_view),
  );

let pat_ctx_view = (ctx: Core.Typ.Ctx.t): Node.t =>
  div(
    [clss([ctxc, "pat"])],
    ctx |> Core.Typ.Ctx.entries |> List.map(context_entry_view),
  );

let tpat_ctx_view = (ctx: Core.Typ.Ctx.t): Node.t =>
  div(
    [clss([ctxc, "tpat"])],
    ctx |> Core.Typ.Ctx.entries |> List.map(context_entry_view),
  );

let ctx_sorts_view = (ci: Core.Statics.t): Node.t => {
  switch (ci) {
  | Invalid(_) => div([clss([ctxc, "invalid"])], [text("")])
  | InfoExp({ctx, _}) => exp_ctx_view(ctx)
  | InfoPat({ctx, _}) => pat_ctx_view(ctx)
  | InfoTyp(_) => div([clss([ctxc, "typ"])], [])
  | InfoTPat({ctx, _}) => tpat_ctx_view(ctx)
  | InfoRul(_) => div([clss([ctxc, "rul"])], [])
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
