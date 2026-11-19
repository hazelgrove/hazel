open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

let alias_view = (s: string): Node.t =>
  div(~attrs=[clss(["typ-alias-view"])], [text(s)]);

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
        use_literal_lexemes: false,
        hole_tiles: false,
        project_tables: false,
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
  | TheoremEntry({name, _}) =>
    div(
      ~attrs=[
        Attr.on_click(_ => globals.inject_global(jump_to(entry))),
        clss(["context-entry", "code", "hypothesis-entry"]),
      ],
      [
        div_name([text(name)]),
        div(~attrs=[clss(["seperator"])], [text(":")]),
        div(~attrs=[clss(["prop"])], [text("theorem")]),
      ],
    )
  };
};

let is_axiom: Language.Ctx.entry => bool =
  fun
  | TheoremEntry({id, _}) =>
    List.exists(
      ~f=(axiom: Language.Ctx.theorem_entry) => Util.Id.equal(axiom.id, id),
      Language.Statics.initial_theorems,
    )
  | VarEntry(_)
  | ConstructorEntry(_)
  | TVarEntry(_)
  | LivelitEntry(_) => false;

/* The built-in axioms are listed only where a proof can cite them: on a
   proof step, or on a term inside a proof the checker has reached. */
let lists_axioms = (~proof_map, ci: Language.Info.t): bool =>
  switch (ci) {
  | InfoProof(_) => true
  | _ =>
    List.exists(
      ~f=id => Option.is_some(Language.ProofMap.lookup(id, proof_map)),
      Language.Info.ancestors_of(ci),
    )
  };

let entries = (~axioms: bool, ctx: Language.Ctx.t): list(Language.Ctx.entry) =>
  ctx
  |> Language.Ctx.filter_shadowed
  |> Language.Ctx.filter_stepper_filter_variables
  |> (x => x.entries)
  |> List.filter(~f=entry => axioms || !is_axiom(entry))
  |> List.rev;

let ctx_view = (~globals, ctx: Language.Ctx.t): Node.t =>
  div(
    ~attrs=[clss(["context-inspector"])],
    List.map(~f=context_entry_view(~globals), entries(~axioms=true, ctx)),
  );

let view =
    (
      ~globals: Globals.t,
      ~proof_map: Language.ProofMap.t,
      ci: option(Language.Statics.Info.t),
    )
    : Node.t => {
  let clss =
    clss(
      ["context-inspector"]
      @ (globals.settings.context_inspector ? ["visible"] : []),
    );
  switch (ci) {
  | Some(ci) when globals.settings.context_inspector =>
    div(
      ~attrs=[clss],
      entries(
        ~axioms=lists_axioms(~proof_map, ci),
        Language.Info.ctx_of(ci),
      )
      |> List.map(~f=context_entry_view(~globals)),
    )
  | _ => div([])
  };
};
