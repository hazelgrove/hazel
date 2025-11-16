open Util;
open Language;
open Language.Statics;
open AssistantTreeHelper.HighLevelNodeMap.Public;

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
      | InfoExp({term, _}) =>
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
    let rec fold_terms = (z: Zipper.t, ids: list(Id.t)) => {
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
            )
          ) {
          | Ok(z'') => fold_terms(z'', rest)
          | _ => fold_terms(z', rest)
          }
        | None => fold_terms(z, rest)
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

    let expand_terms = (~z: Zipper.t, ~ids: list(Id.t)) => {
      // Retain only the ids which are already folded
      let ids =
        List.filter_map(
          id =>
            if (is_term_folded(CachedSyntax.init(z).term_data, id, z)) {
              Some(id);
            } else {
              None;
            },
          ids,
        );
      fold_terms(z, ids);
    };

    let collapse_terms = (~z: Zipper.t, ~ids: list(Id.t)) => {
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
      fold_terms(z, ids);
    };

    let expand_definitions =
        (~z: Zipper.t, ~ids: list(Id.t), ~info_map: Id.Map.t(Info.t)) => {
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
      expand_terms(~z, ~ids=def_ids);
    };

    let collapse_definitions =
        (~z: Zipper.t, ~ids: list(Id.t), ~info_map: Id.Map.t(Info.t)) => {
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
      collapse_terms(~z, ~ids=def_ids);
    };

    let fold_body = (z: Zipper.t, term: Info.t) => {
      let id =
        switch (term) {
        | InfoExp({term, _}) =>
          switch (Exp.term_of(term)) {
          | Let(_, _, body) => Exp.rep_id(body)
          | TyAlias(_, _, body) => Exp.rep_id(body)
          | _ => Id.invalid
          }
        | _ => Id.invalid
        };
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
        let z'' =
          ProjectorPerform.go(
            CachedSyntax.init(z').term_data,
            SetIndicated(Specific(Fold)),
            z',
          );
        z'';
      | _ => Ok(z)
      };
    };
  };

  module ContextUtils = {
    let context_of = (node: AssistantTreeHelper.HighLevelNodeMap.node): string => {
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
          node: AssistantTreeHelper.HighLevelNodeMap.node,
        )
        : list(Binding.t) => {
      let id = Utils.get_def_id_of_let(node.info);
      let refs_of_def = Statics.Map.refs_in(info_map, id);
      let refs_of_node =
        Statics.Map.refs_in(
          info_map,
          AssistantTreeHelper.HighLevelNodeMap.id_of(node),
        );

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
          node: AssistantTreeHelper.HighLevelNodeMap.node,
        )
        : string => {
      refs_in(~exclude_rec_refs, ~exclude_body_refs, info_map, node)
      |> str_of_refs_in;
    };
  };

  module Printer = {
    let convex_char = "?";
    let concave_char = "~";

    let print = (~z: Zipper.t, ~info_map: Id.Map.t(Info.t)): string => {
      let node_map =
        Option.get(AssistantTreeHelper.HighLevelNodeMap.build(z, info_map));
      // Step 1: Expand everything for agent view
      print_endline("here #1, expanding everything");
      let all_ids = Id.Map.bindings(info_map) |> List.map(fst);
      let z' = ViewUtils.expand_terms(~z, ~ids=all_ids);

      // Step 2: Collapse all top level definitions for agent view,
      // except for the ones that are expanded, given by the agent view's expanded list
      print_endline(
        "here #2, collapsing all top level definitions for agent view, except for the ones that are expanded",
      );
      let all_top_level_ids = Id.Map.bindings(node_map) |> List.map(fst);
      let expanded_ids =
        List.map(
          (path: string) => path_to_node(node_map, path),
          z.agent_view.expanded_paths,
        );
      let ids_to_collapse =
        all_top_level_ids
        |> List.filter((id: Id.t) => !List.mem(id, expanded_ids));
      print_endline(
        "here #2.1, ids to collapse: "
        ++ String.concat(", ", List.map(Id.to_string, ids_to_collapse)),
      );
      let z'' =
        ViewUtils.collapse_definitions(
          ~z=z',
          ~ids=ids_to_collapse,
          ~info_map,
        );

      print_endline("here #3, printing zipper");
      Printer.of_zipper(
        ~holes=convex_char,
        ~concave_holes=concave_char,
        ~special_folds=true,
        z'',
      );
    };
  };
};

module Public = {
  let print = (~z: Zipper.t, ~info_map: Id.Map.t(Info.t)): string => {
    Local.Printer.print(~z, ~info_map);
  };
};
