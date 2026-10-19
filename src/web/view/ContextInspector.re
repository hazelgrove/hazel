open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

let alias_view = (s: string): Node.t =>
  div(~attrs=[clss(["typ-alias-view"])], [text(s)]);

let jump_to = entry => Globals.Update.JumpToTile(Language.Ctx.get_id(entry));

/* Γ prints each entry's type whole, as tiles, at every redraw. On the
   Scenes slide one entry, a module of livelit definitions, printed to
   474,304 characters (91,567 type nodes; every member spells out Html.T
   and the command types): 6 s to print and measure, and then the page
   froze laying it out (^ticket(396)). The next largest entry on the
   playground slides is 693 nodes. So past a budget an entry shows an
   outline instead: a module's member names, else an ellipsis. */
let typ_budget = 2000;

let typ_within = (budget: int, ty: Language.Typ.t): bool => {
  let count = ref(0);
  let f = (cont, x: Language.Typ.t) => {
    incr(count);
    count^ > budget ? x : cont(x);
  };
  switch (Language.Typ.map_term(~f_typ=f, ty)) {
  | _ => count^ <= budget
  | exception _ => false
  };
};

let outline_text = (ty: Language.Typ.t): string =>
  switch (ty.term) {
  | Sig(items) =>
    "{ "
    ++ (
      Language.Sig.members(items)
      |> List.map((m: Language.Sig.member) =>
           switch (m) {
           | Val(x, _) => "let " ++ x
           | TypeManifest(x, _)
           | TypeAbstract(x) => "type " ++ x
           }
         )
      |> String.concat("; ")
    )
    ++ " }"
  | _ => "…"
  };

let too_large_view = (ty: Language.Typ.t): Node.t =>
  div(
    ~attrs=[
      clss(["typ-too-large"]),
      Attr.title(
        "This type has more than "
        ++ string_of_int(typ_budget)
        ++ " parts, too many to show here; click to go to its definition",
      ),
    ],
    [text(outline_text(ty))],
  );

let context_entry_view = (~globals, entry: Language.Ctx.entry): Node.t => {
  let view_type =
    CodeViewable.view_typ(
      ~globals,
      ~settings={
        secondary: AutoFormat,
        parenthesization: Defensive,
        label_format: QuoteWhenNecessary,
        inline: true,
        fold_case_clauses: false,
        fold_fn_bodies: `NoFold,
        hide_fixpoints: false,
        show_ascriptions: true,
        show_filters: false,
        show_unknown_as_hole: true,
        hole_tiles: false,
        project_tables: false,
      },
    );
  let view_type = ty =>
    typ_within(typ_budget, ty) ? view_type(ty) : too_large_view(ty);
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

let ctx_view = (~globals, ctx: Language.Ctx.t): Node.t =>
  div(
    ~attrs=[clss(["context-inspector"])],
    List.map(
      context_entry_view(~globals),
      ctx
      |> Language.Ctx.filter_shadowed
      |> Language.Ctx.filter_stepper_filter_variables
      |> (x => x.entries)
      |> List.rev,
    ),
  );

let ctx_sorts_view = (~globals, ci: Language.Statics.Info.t) =>
  Language.Info.ctx_of(ci)
  |> Language.Ctx.filter_shadowed
  |> Language.Ctx.filter_stepper_filter_variables
  |> (x => x.entries)
  |> List.rev
  |> List.map(context_entry_view(~globals));

let view = (~globals: Globals.t, ci: option(Language.Statics.Info.t)): Node.t => {
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
