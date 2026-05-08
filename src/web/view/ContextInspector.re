open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* Inline `span` (not a `div`) so an alias name placed alongside
   another inline node — e.g. the `type` keyword — sits on the same
   line. The parent `.context-entry` is `display: flex` so it
   shrinks to its single-line content; only the wrapper line as a
   whole is wrapped/expanded on hover. */
let alias_view = (s: string): Node.t =>
  span(~attrs=[clss(["typ-alias-view"])], [text(s)]);

let jump_to = entry => Globals.Update.JumpToTile(Language.Ctx.get_id(entry));

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
      },
    );
  let div_name = div(~attrs=[clss(["name"])]);
  /* Render a type alias entry as a declaration:
       `type Name(a, b) = body` for parameterized aliases,
       `type Name = body` for non-parameterized,
       `Name :: Type` for abstract type variables.
     Strip the outer `Rec(_, _)` (for self-referential aliases) —
     the displayed name binds the recursive reference implicitly.
     Emit the `type` keyword and the alias name in a single
     `div_name` flex child so the parent's `gap: 0.5em` doesn't
     widen the gap between them. */
  let typ_keyword = span(~attrs=[clss(["typ-keyword"])], [text("type ")]);
  let view_tvar_entry = (name: string, kind: Language.Ctx.kind): list(Node.t) =>
    switch (kind) {
    | Abstract => [
        div_name([alias_view(name)]),
        div(~attrs=[clss(["seperator"])], [text("::")]),
        Kind.view(~globals, kind),
      ]
    | Singleton(ty) =>
      let unwrapped =
        switch (Language.Typ.term_of(ty)) {
        | Rec(_, body) => body
        | _ => ty
        };
      switch (Language.Typ.term_of(unwrapped)) {
      | TypFun(binder, body) => [
          div_name([
            typ_keyword,
            alias_view(
              name ++ "(" ++ Language.Typ.pretty_print_tvar(binder) ++ ")",
            ),
          ]),
          div(~attrs=[clss(["seperator"])], [text("=")]),
          view_type(body),
        ]
      | _ => [
          div_name([typ_keyword, alias_view(name)]),
          div(~attrs=[clss(["seperator"])], [text("=")]),
          view_type(unwrapped),
        ]
      };
    };
  let attrs = [
    Attr.on_click(_ => globals.inject_global(jump_to(entry))),
    clss(["context-entry", "code"]),
  ];
  switch (entry) {
  | VarEntry({name, typ, _})
  | ConstructorEntry({name, typ, _}) =>
    div(
      ~attrs,
      [
        div_name([text(name)]),
        div(~attrs=[clss(["seperator"])], [text(":")]),
        view_type(typ),
      ],
    )
  | TVarEntry({name, kind, _}) => div(~attrs, view_tvar_entry(name, kind))
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
