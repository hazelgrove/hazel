open Virtual_dom.Vdom;
open Node;
open Util.Web;

let alias_view = (s: string): Node.t =>
  div(~attrs=[clss(["typ-alias-view"])], [text(s)]);

let jump_to = entry =>
  Globals.Update.JumpToTile(Semantics.Ctx.get_id(entry));

let context_entry_view = (~globals, entry: Semantics.Ctx.entry): Node.t => {
  let view_type =
    CodeViewable.view_typ(
      ~globals,
      ~settings={
        inline: true,
        fold_case_clauses: false,
        fold_fn_bodies: false,
        hide_fixpoints: false,
        show_filters: false,
        show_unknown_as_hole: true,
      },
    );
  let div_name = div(~attrs=[clss(["name"])]);
  switch (entry) {
  | VarEntry({name, typ, _})
  | ConstructorEntry({name, typ, _}) =>
    div(
      ~attrs=[
        Attr.on_click(_ => globals.inject_global(jump_to(entry))),
        clss(["context-entry", "code"]),
      ],
      [
        div_name([text(name)]),
        div(~attrs=[clss(["seperator"])], [text(":")]),
        view_type(typ),
      ],
    )
  | TVarEntry({name, kind, _}) =>
    div(
      ~attrs=[
        Attr.on_click(_ => globals.inject_global(jump_to(entry))),
        clss(["context-entry", "code"]),
      ],
      [
        div_name([alias_view(name)]),
        div(~attrs=[clss(["seperator"])], [text("::")]),
        Kind.view(~globals, kind),
      ],
    )
  | LivelitEntry({name, expansion_t, _}) =>
    div(
      ~attrs=[
        Attr.on_click(_ => globals.inject_global(jump_to(entry))),
        clss(["context-entry", "code", "livelit-entry"]),
      ],
      [
        div_name([text("^" ++ name)]),
        div(~attrs=[clss(["seperator"])], [text(":")]),
        view_type(expansion_t),
      ],
    )
  };
};

let ctx_view = (~globals, ctx: Semantics.Ctx.t): Node.t =>
  div(
    ~attrs=[clss(["context-inspector"])],
    List.map(
      context_entry_view(~globals),
      ctx
      |> Semantics.Ctx.filter_duplicates
      |> Semantics.Ctx.filter_stepper_filter_variables
      |> (x => x.entries)
      |> List.rev,
    ),
  );

let ctx_sorts_view = (~globals, ci: Semantics.Statics.Info.t) =>
  Semantics.Info.ctx_of(ci)
  |> Semantics.Ctx.filter_duplicates
  |> Semantics.Ctx.filter_stepper_filter_variables
  |> (x => x.entries)
  |> List.rev
  |> List.map(context_entry_view(~globals));

let view =
    (~globals: Globals.t, ci: option(Semantics.Statics.Info.t)): Node.t => {
  let clss =
    clss(
      ["context-inspector"]
      @ (globals.settings.context_inspector ? ["visible"] : []),
    );
  switch (ci) {
  | Some(ci) when globals.settings.context_inspector =>
    div(~attrs=[clss], ctx_sorts_view(~globals, ci))
  | _ => div([])
  };
};
