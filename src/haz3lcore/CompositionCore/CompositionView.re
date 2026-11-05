open Language;
open Language.Statics;
open HighLevelNodeMap.Public;

/*
 Follows a programming practice/pattern here of separating logic local to the file into a "local" module,
 and then exposing a public module for the logic that is intended to be used externally.

 module Local = {
   **utils/helpers/etc.**
 };
 module Public = {
   **functions/methods/etc.**
 };

 The local code can get pretty messy and maze-like, thus showing a clear separation of what is
 exposed and what is not will hopefully counteract this.
 */

module Local = {
  module Utils = {
    /* The definition a `let` or a binding item folds to; type definitions
       are not folded. */
    let def_id_of_binding = (term: Info.t): option(Id.t) =>
      switch (term) {
      | InfoExp({user_term, _}) =>
        switch (Exp.term_of(user_term)) {
        | Let(_, def, _) => Some(Exp.rep_id(def))
        | _ => None
        }
      | InfoMod({user_term, _}) =>
        switch (Mod.def(user_term)) {
        | Some(Exp(def)) => Some(Exp.rep_id(def))
        | _ => None
        }
      | _ => None
      };
  };

  module ViewUtils = {
    let rec fold_terms = (z: Zipper.t, ids: list(Id.t), ~root) => {
      switch (ids) {
      | [] => z
      | [id, ...rest] =>
        // Fold the term
        switch (
          Select.term(
            ~defs_exclude_bodies=false,
            ~case_rules=false,
            CachedSyntax.init(z).term_data,
            id,
            z,
          )
        ) {
        | Some(z') =>
          switch (
            ProjectorPerform.go(
              CachedSyntax.init(z').term_data,
              SetIndicated(Specific(Fold)),
              z',
              [],
              [],
              ~elaborated=CachedStatics.empty.elaborated,
              ~root,
            )
          ) {
          | Ok(z'') => fold_terms(z'', rest, ~root)
          | _ => fold_terms(z', rest, ~root)
          }
        | None => fold_terms(z, rest, ~root)
        }
      };
    };

    let is_term_folded = (term_data: TermData.t, id: Id.t, z: Zipper.t): bool => {
      switch (
        Select.term(
          ~defs_exclude_bodies=false,
          ~case_rules=false,
          term_data,
          id,
          z,
        )
      ) {
      | Some(z') =>
        switch (z'.selection.content) {
        | [Piece.Projector(pr)] => pr.kind == ProjectorCore.Kind.Fold
        | _ => false
        }
      | None => false
      };
    };

    let collapse_terms = (~z: Zipper.t, ~ids: list(Id.t), ~root) => {
      // Retain only the ids which are not folded
      let ids =
        List.filter_map(
          id =>
            if (!is_term_folded(CachedSyntax.init(z).term_data, id, z)) {
              Some(id);
            } else {
              None;
            },
          ids,
        );
      fold_terms(z, ids, ~root);
    };

    let collapse_definitions =
        (~z: Zipper.t, ~ids: list(Id.t), ~info_map: Id.Map.t(Info.t), ~root) => {
      let def_ids =
        List.filter_map(
          (id: Id.t) =>
            Option.bind(
              Id.Map.find_opt(id, info_map),
              Utils.def_id_of_binding,
            ),
          ids,
        );
      collapse_terms(~z, ~ids=def_ids, ~root);
    };
  };

  let projector_to_segment = (pr: Base.projector): Segment.t =>
    switch (pr.kind) {
    | Fold => [Piece.mk_tile((Form.Tok({|⋱|}), Sort.Any), [])]
    | _ => Triggers.projector_to_invoke(pr)
    };

  module Printer = {
    let print_segment = (segment: Segment.t): string =>
      Printer.of_segment(~holes="?", ~projector_to_segment, segment);

    let print_zipper = (z: Zipper.t): string =>
      Printer.of_zipper(
        ~holes="?",
        ~concave_holes="~",
        ~projector_to_segment,
        z,
      );

    /* Zipper after applying the same collapse rules as [print] (before stringification). */
    let zipper_for_agent_context =
        (
          ~info_map: option(Statics.Map.t)=?,
          editor: Editor.t,
          agent_context: AgentContext.Model.t,
        )
        : Zipper.t => {
      let z = editor.state.zipper;
      /* callers that already hold this program's statics pass them: a
         fresh run here was most of a tool call's cost */
      let info_map =
        switch (info_map) {
        | Some(m) => m
        | None => CompositionGo.Public.mk_statics(z)
        };
      switch (HighLevelNodeMap.build(z, info_map)) {
      | None => z
      | Some(node_map) =>
        let all_top_level_ids = Id.Map.bindings(node_map) |> List.map(fst);
        let expanded_ids =
          List.filter_map(
            (path: string) => path_to_id_opt(node_map, path),
            agent_context.expanded_paths,
          );
        let ids_to_collapse =
          all_top_level_ids
          |> List.filter((id: Id.t) => !List.mem(id, expanded_ids));
        ViewUtils.collapse_definitions(
          ~z,
          ~ids=ids_to_collapse,
          ~info_map,
          ~root=editor.root,
        );
      };
    };

    let print =
        (
          ~probe_map: Language.Sample.Map.t=Language.Sample.Map.empty,
          ~info_map: option(Statics.Map.t)=?,
          editor: Editor.t,
          agent_context: AgentContext.Model.t,
        )
        : string => {
      let z' = zipper_for_agent_context(~info_map?, editor, agent_context);
      let has_probes = !List.is_empty(z'.refractors.manuals);
      if (has_probes) {
        ProbeText.of_zipper(~projector_to_segment, ~probe_map, z');
      } else {
        print_zipper(z');
      };
    };

    let segment_for_agent_context =
        (editor: Editor.t, agent_context: AgentContext.Model.t): Segment.t => {
      let z' = zipper_for_agent_context(editor, agent_context);
      Select.all(z').selection.content;
    };
  };
};

module Public = {
  let print_segment = Local.Printer.print_segment;
  let print = Local.Printer.print;
  let print_zipper = Local.Printer.print_zipper;
  let segment_for_agent_context = Local.Printer.segment_for_agent_context;
};
