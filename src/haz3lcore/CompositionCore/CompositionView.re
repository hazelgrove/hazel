open Util;
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
    let get_individual_ids_of_let = (term: Info.t): (Id.t, Id.t, Id.t) => {
      switch (term) {
      | InfoExp({user_term: term, _}) =>
        switch (Exp.term_of(term)) {
        | Let(pat, def, body) => (
            Pat.rep_id(pat),
            Exp.rep_id(def),
            Exp.rep_id(body),
          )
        // We won't fold/abstract away type definitions, so no need for this helper to handle them yet
        | _ => (Id.invalid, Id.invalid, Id.invalid)
        }
      | _ => (Id.invalid, Id.invalid, Id.invalid)
      };
    };

    let get_def_id_of_let = (term: Info.t): Id.t => {
      let (_, def, _) = get_individual_ids_of_let(term);
      def;
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
      let infos =
        List.map((id: Id.t) => Id.Map.find_opt(id, info_map), ids);
      let def_ids =
        List.filter_map(
          (info: option(Info.t)) =>
            switch (info) {
            | Some(info) => Some(Utils.get_def_id_of_let(info))
            | None => None
            },
          infos,
        )
        |> List.filter((id: Id.t) => id != Id.invalid);
      collapse_terms(~z, ~ids=def_ids, ~root);
    };
  };

  module ContextUtils = {
    let context_of = (node: HighLevelNodeMap.node): string => {
      switch (node.info) {
      | InfoExp({ctx, _}) =>
        let bindings: Binding.s =
          List.filter_map(
            (entry: Ctx.entry) => {
              let b =
                switch (entry) {
                | Ctx.VarEntry(entry) => Ctx.binding_of(ctx, entry.name)
                | Ctx.TVarEntry(entry) => Ctx.binding_of(ctx, entry.name)
                | Ctx.ConstructorEntry(entry) =>
                  Ctx.binding_of(ctx, entry.name)
                | _ => Ctx.binding_of(ctx, "") // invalid
                };
              if (b.id == Id.invalid) {
                None;
              } else {
                Some(b);
              };
            },
            ctx.entries,
          );
        "Typing Context: ["
        ++ String.concat(
             "\n",
             List.mapi(
               (i: int, b: Binding.t) =>
                 b.name ++ "(Index: " ++ string_of_int(i) ++ ")",
               bindings,
             ),
           )
        ++ "]";
      | _ => ""
      };
    };

    let refs_in =
        (
          ~exclude_rec_refs: bool=false,
          ~exclude_body_refs: bool=false,
          info_map: Id.Map.t(Info.t),
          node: HighLevelNodeMap.node,
        )
        : list(Binding.t) => {
      let id = Utils.get_def_id_of_let(node.info);
      let refs_of_def = Statics.Map.refs_in(info_map, id);
      let refs_of_node =
        Statics.Map.refs_in(info_map, HighLevelNodeMap.id_of(node));

      // Intersect based on binding IDs
      // This allows us to ignore references in the body AND recursive references in the def
      // (refs_of_def will not have body refs)
      // (refs_of_node will not have recursive refs)
      let intersected_refs =
        ListUtil.intersection_f(
          (b: Binding.t) => b.id,
          refs_of_def,
          refs_of_node,
        );

      let refs =
        switch (exclude_rec_refs, exclude_body_refs) {
        | (true, true) => intersected_refs
        | (true, false) => refs_of_node
        | (false, true) => refs_of_def
        | (false, false) => refs_of_def @ refs_of_node
        };

      // remove duplicates
      let refs' =
        ListUtil.dedup_f(
          (b1: Binding.t, b2: Binding.t) => b1.id == b2.id,
          refs,
        );

      let refs'' = List.filter((b: Binding.t) => b.id != Id.invalid, refs');

      refs'';
    };

    let str_of_refs_in = (references: list(Binding.t)): string => {
      "Variables referenced: ["
      ++ String.concat(
           ", ",
           List.mapi(
             (i: int, b: Binding.t) =>
               b.name ++ " (Index: " ++ string_of_int(i) ++ ")",
             references,
           ),
         )
      ++ "]";
    };

    let str_refs_in =
        (
          ~exclude_rec_refs: bool=false,
          ~exclude_body_refs: bool=false,
          info_map: Id.Map.t(Info.t),
          node: HighLevelNodeMap.node,
        )
        : string => {
      refs_in(~exclude_rec_refs, ~exclude_body_refs, info_map, node)
      |> str_of_refs_in;
    };
  };

  let projector_to_segment = (pr: Base.projector): Segment.t =>
    switch (pr.kind) {
    /* Unmolded placeholder: display-only (feeds Printer.of_segment,
     * which reads only labels) */
    | Fold => [Piece.mk_tile(FormId.Unmolded({|⋱|}), [])]
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
        (editor: Editor.t, agent_context: AgentContext.Model.t): Zipper.t => {
      let z = editor.state.zipper;
      let info_map = CompositionGo.Public.mk_statics(z);
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
          editor: Editor.t,
          agent_context: AgentContext.Model.t,
        )
        : string => {
      let z' = zipper_for_agent_context(editor, agent_context);
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
