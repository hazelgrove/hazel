open Virtual_dom.Vdom;
open Node;
open Util.Web;

let alias_view = (s: string): Node.t =>
  div(~attr=clss(["typ-alias-view"]), [text(s)]);

let jump_to = entry =>
  Globals.Update.JumpToTile(Haz3lcore.Ctx.get_id(entry));

let context_entry_view =
    (~globals, ~inject, entry: Haz3lcore.Ctx.entry): Node.t => {
  let view_type = CodeViewable.view_typ(~globals, ~inline=true);
  let view_expr = CodeViewable.view_exp(~globals, ~inline=true);
  let div_name =
    div(
      ~attr=
        Attr.many([
          clss(["name"]),
          Attr.on_click(_ => inject(jump_to(entry))),
        ]),
    );
  switch (entry) {
  | VarEntry({name, typ, _})
  | ConstructorEntry({name, typ, _}) =>
    div_c(
      "context-entry",
      [
        div_name([text(name)]),
        div(~attr=clss(["seperator"]), [text(":")]),
        view_type(typ),
      ],
    )
  | TVarEntry({name, kind, _}) =>
    div_c(
      "context-entry",
      [
        div_name([alias_view(name)]),
        div(~attr=clss(["seperator"]), [text("::")]),
        Kind.view(~globals, kind),
      ],
    )
  | RewriteEntry({lhs, rhs, _}) =>
    div_c(
      "context-entry",
      [
        view_expr(lhs),
        div(~attr=clss(["seperator"]), [text("≡")]),
        view_expr(rhs),
      ],
    )
  };
};

let ctx_view = (~globals, ~inject, ctx: Haz3lcore.Ctx.t): Node.t =>
  div(
    ~attr=clss(["context-entries"]),
    List.map(
      context_entry_view(~globals, ~inject),
      ctx |> Haz3lcore.Ctx.filter_duplicates |> List.rev,
    ),
  );

let ctx_sorts_view = (~globals, ~inject, ci: Haz3lcore.Statics.Info.t) =>
  Haz3lcore.Info.ctx_of(ci)
  |> Haz3lcore.Ctx.filter_duplicates
  |> List.rev
  |> List.map(context_entry_view(~globals, ~inject));

let view = (~globals: Globals.t, ci: Haz3lcore.Statics.Info.t): Node.t => {
  let clss =
    clss(
      ["context-inspector"]
      @ (globals.settings.context_inspector ? ["visible"] : []),
    );
  div(
    ~attr=clss,
    ctx_sorts_view(~globals, ~inject=globals.inject_global, ci),
  );
};
