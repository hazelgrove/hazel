open Util;
open Language;
open Language.Statics;
open OptUtil.Syntax;

module Utils = {
  let exp_to_info = (term: Exp.t, info_map: Id.Map.t(Info.t)): Info.t => {
    // Helper function
    let e = Exp.rep_id(term);
    Id.Map.find(e, info_map);
  };

  let exp_to_exp = (term: Exp.t, info_map: Id.Map.t(Info.t)): Info.exp => {
    switch (exp_to_info(term, info_map)) {
    | InfoExp(exp_info) => exp_info
    | _ => raise(Failure("No exp info found"))
    };
  };

  let pat_to_info = (term: Pat.t, info_map: Id.Map.t(Info.t)): Info.t => {
    // Helper function
    let e = Pat.rep_id(term);
    Id.Map.find(e, info_map);
  };

  let pat_to_pat = (term: Pat.t, info_map: Id.Map.t(Info.t)): Info.pat => {
    switch (pat_to_info(term, info_map)) {
    | InfoPat(pat_info) => pat_info
    | _ => raise(Failure("No pat info found"))
    };
  };

  let tpat_to_info = (term: TPat.t, info_map: Id.Map.t(Info.t)): Info.t => {
    // Helper function
    let e = TPat.rep_id(term);
    Id.Map.find(e, info_map);
  };

  let tpat_to_tpat = (term: TPat.t, info_map: Id.Map.t(Info.t)): Info.tpat => {
    switch (tpat_to_info(term, info_map)) {
    | InfoTPat(tpat_info) => tpat_info
    | _ => raise(Failure("No tpat info found"))
    };
  };

  let typ_to_info = (term: Typ.t, info_map: Id.Map.t(Info.t)): Info.t => {
    // Helper function
    let e = Typ.rep_id(term);
    Id.Map.find(e, info_map);
  };

  let typ_to_typ = (term: Typ.t, info_map: Id.Map.t(Info.t)): Info.typ => {
    switch (typ_to_info(term, info_map)) {
    | InfoTyp(typ_info) => typ_info
    | _ => raise(Failure("No typ info found"))
    };
  };

  let child_expressions_of_exp = (term: TermBase.exp_t): list(TermBase.exp_t) => {
    /*
     Returns the child expressions within the given expression.
     */
    switch (Exp.term_of(term)) {
    | Fun(_, e, _, _)
    | UnOp(_, e)
    | Test(e)
    | Parens(e)
    | Filter(_, e)
    | Closure(_, e)
    | Probe(e, _)
    | Asc(e, _)
    | FixF(_, e, _)
    | Use(_, e)
    | TypAp(e, _)
    | TypFun(_, e, _)
    | DeferredAp(e, _)
    | HintedTest(e, _) => [e]
    | Let(_, e1, e2)
    | Seq(e1, e2)
    | Ap(_, e1, e2)
    | Dot(e1, e2)
    | TupleExtension(e1, e2)
    | TupLabel(e1, e2)
    | Cons(e1, e2)
    | ListConcat(e1, e2)
    | BinOp(_, e1, e2) => [e1, e2]
    | Tuple(es)
    | ListLit(es) => es
    | If(e1, e2, e3) => [e1, e2, e3]
    | Match(e, branches) => [e, ...List.map(snd, branches)]
    | BuiltinFun(_)
    | Label(_)
    | EmptyHole
    | Undefined
    | Invalid(_)
    | MultiHole(_)
    | DynamicErrorHole(_, _)
    | Deferral(_)
    | Atom(_)
    | Constructor(_, _)
    | LivelitName(_)
    | TyAlias(_)
    | Var(_) => []
    };
  };

  let top_level_term_of =
      (~start_point: Info.t, ~info_map: Id.Map.t(Info.t)): option(Info.t) => {
    let initial_nominee =
      switch (start_point) {
      | InfoExp({term, _}) =>
        switch (Exp.term_of(term)) {
        | Let(_) => Some(start_point) // if it is a let binding, then this term is a nominee
        | TyAlias(_) => Some(start_point) // if it is a type binding, then this term is a nominee
        | _ => None
        }
      | _ => None
      };

    let rec search_for_top_level_node =
            (
              ~candidates: list(Id.t),
              ~departure_point: Info.t,
              ~nominee: option(Info.t),
            ) => {
      switch (candidates) {
      | [candidate, ...rest] =>
        let candidate = Id.Map.find(candidate, info_map);
        let nominee =
          switch (candidate) {
          | InfoExp({term, _}) =>
            switch (Exp.term_of(term)) {
            | Let(_, def, _) =>
              let def_id = Exp.rep_id(def);
              // Check if departure point is in the definition position of the candidate
              if (Id.equal(def_id, Info.id_of(departure_point))) {
                // it is, set this is as the new nominee for the top-level node
                Some(
                  candidate,
                );
              } else {
                nominee;
              };
            | TyAlias(_, def, _) =>
              let def_id = Typ.rep_id(def);
              if (Id.equal(def_id, Info.id_of(departure_point))) {
                Some(candidate);
              } else {
                nominee;
              };
            | _ => nominee
            }
          | _ => nominee
          };
        search_for_top_level_node(
          ~candidates=rest,
          ~departure_point=candidate,
          ~nominee,
        );
      | [] => nominee
      };
    };

    search_for_top_level_node(
      ~candidates=Info.ancestors_of(start_point),
      ~departure_point=start_point,
      ~nominee=initial_nominee,
    );
  };

  let get_oldest_ancestor =
      (info: Info.t, info_map: Id.Map.t(Info.t)): Info.t => {
    switch (
      {
        let* id = ListUtil.hd_opt(List.rev(Info.ancestors_of(info)));
        Id.Map.find_opt(id, info_map);
      }
    ) {
    | Some(info) => info
    | None => info
    };
  };
};

module MoveOffWhitespaceHelper = {
  /*
   Ad-hoc methods to help move the cursor off whitespace and to the nearest
   non-whitespace term.
   */

  let is_on_whitespace = (z: Zipper.t): bool => {
    // Use for_index which only ignores secondary pieces, not grout pieces
    switch (Indicated.for_index(z)) {
    | Some((piece, _, _)) =>
      Piece.is_secondary(piece)
      || Piece.is_grout(piece)
      || Piece.is_convex(piece)
    | None => false
    };
  };

  let rec move_to_non_whitespace = (z: Zipper.t, d: Direction.t): Zipper.t => {
    /*
     Iteratively move the cursor to the left.
     If moving left becomes stagnant, try and move right.
     */
    is_on_whitespace(z)
      ? {
        switch (Move.by_char(d, z)) {
        | Some(z') => move_to_non_whitespace(z', d)
        | None =>
          if (d == Left) {
            move_to_non_whitespace(z, Right);
          } else {
            raise(Failure("Couldn't move to non-whitespace"));
          }
        };
      }
      : z;
  };
};

module Namer = {
  /*
   Convert patterns to purely string names.
   */

  let rec mk_name_from_pat = (pat: TermBase.pat_t) => {
    switch (pat.term) {
    | Var(name)
    | Constructor(name, _)
    | Label(name) => name
    | Probe(pat, _)
    | Parens(pat)
    | Asc(pat, _) => mk_name_from_pat(pat)
    | Cons(pat1, pat2) =>
      mk_name_from_pat(pat1) ++ "::" ++ mk_name_from_pat(pat2)
    | Tuple(pats) =>
      "(" ++ String.concat(", ", List.map(mk_name_from_pat, pats)) ++ ")"
    | ListLit(pats) =>
      "[" ++ String.concat(", ", List.map(mk_name_from_pat, pats)) ++ "]"
    | Wild => "{wild}"
    | EmptyHole => "{empty pattern hole}"
    | MultiHole(_) => "{multi hole}"
    | Ap(pat1, pat2) =>
      mk_name_from_pat(pat1) ++ "(" ++ mk_name_from_pat(pat2) ++ ")"
    | TupLabel(pat1, pat2) =>
      "(" ++ mk_name_from_pat(pat1) ++ " : " ++ mk_name_from_pat(pat2) ++ ")"
    | Atom(_) => "{atom}"
    | Invalid(_) => "{invalid}"
    };
  };

  let mk_name_from_tpat = (tpat: TermBase.tpat_t) => {
    switch (tpat.term) {
    | Var(name)
    | Invalid(name) => name
    | EmptyHole => "{empty type pattern hole}"
    | MultiHole(_) => "{multi type pattern hole}"
    };
  };

  let mk_name = (info: Info.t): string => {
    switch (info) {
    | InfoExp({term, _}) =>
      switch (Exp.term_of(term)) {
      | Let(pat, _, _) => mk_name_from_pat(pat)
      | TyAlias(tpat, _, _) => mk_name_from_tpat(tpat)
      | _ => raise(Failure("Not a valid expression to make a name from"))
      }
    | _ => raise(Failure("Not a valid term to make a name from"))
    };
  };
};

module HighLevelNodeMap = {
  type node = {
    // The term associated with this node
    info: Info.t,
    // The high-level path to this node from the root of the AST, as a list of their ids
    // Ordering: the first element is the root of the AST, and the last element is this node
    path: list(Id.t),
    // Ids of the outgoing children nodes in the AST
    children: list(Id.t),
    // Ids of the outgoing sibling nodes in the AST
    siblings: list(Id.t),
    // Index of this node in the list of siblings
    sibling_idx: int,
    // The name of this node. Constructed through recursively
    // unwrapping the pattern(s) associated with the node
    name: string,
    // ...
    // May optionally choose to store more metadata about terms here, such as type, probes, etc.
    // ...
  };

  type t = Id.Map.t(node);

  let id_of = (node: node) => {
    Info.id_of(node.info);
  };

  let unwrap = (t: option(t)): t => {
    Option.get(t);
  };

  let find = (node_map: t, id: Id.t) => {
    switch (Id.Map.find_opt(id, node_map)) {
    | Some(node) => node
    | None =>
      raise(
        Failure("Node with id " ++ Id.to_string(id) ++ " not found in map"),
      )
    };
  };

  let id_to_name = (node_map: t, id: Id.t): string => {
    find(node_map, id).name;
  };

  let parent_id_from_path = (path: list(Id.t)): option(Id.t) => {
    switch (List.rev(path)) {
    | [_, parent, ..._] => Some(parent) // the first element is the dummy root
    | _ => None
    };
  };

  let parent_id_of = (node: node): option(Id.t) => {
    // the parent is the second element in the path
    parent_id_from_path(
      node.path,
    );
  };

  let parent_of = (node_map: t, node: node): option(node) => {
    switch (parent_id_of(node)) {
    | Some(p_id) => Some(find(node_map, p_id))
    | None => None
    };
  };

  let children_of = (node_map: t, node: node): list(node) => {
    List.map((id: Id.t) => find(node_map, id), node.children);
  };

  let descendants_of = (node_map: t, node: node): list(list(Id.t)) => {
    let rec build_levels =
            (current_level: list(Id.t), acc: list(list(Id.t)))
            : list(list(Id.t)) =>
      if (List.length(current_level) == 0) {
        List.rev(acc);
      } else {
        // Get all children of nodes in the current level
        let next_level =
          current_level
          |> List.map(id => find(node_map, id))
          |> List.map(node => node.children)
          |> List.flatten;

        // Add current level to accumulator and recurse
        build_levels(next_level, [current_level, ...acc]);
      };

    // Start with the children of the given node
    build_levels(node.children, []);
  };

  let siblings_of = (node_map: t, node: node): list(node) => {
    List.map((id: Id.t) => find(node_map, id), node.siblings);
  };

  let next_sibling_of = (node: node): option(Id.t) => {
    List.nth_opt(
      node.siblings,
      node.sibling_idx + 1 mod List.length(node.siblings),
    );
  };

  let prev_sibling_of = (node: node): option(Id.t) => {
    List.nth_opt(
      node.siblings,
      node.sibling_idx - 1 mod List.length(node.siblings),
    );
  };

  let add_child = (node_map: t, parent: Id.t, child: Id.t): t => {
    switch (Id.Map.find_opt(parent, node_map)) {
    | Some(parent_node) =>
      Id.Map.add(
        parent,
        {
          ...parent_node,
          children: parent_node.children @ [child],
        },
        node_map,
      )
    | None => node_map
    };
  };

  let init_node = (info: Info.t, path: list(Id.t), node_map: t): t => {
    print_endline(
      "Constructing node for the id: " ++ Id.to_string(Info.id_of(info)),
    );
    // 1. Add this node to the children of its parent (head of rev(path)), if one exists
    let node_map =
      switch (parent_id_from_path(path)) {
      | Some(parent) => add_child(node_map, parent, Info.id_of(info))
      | None => node_map
      };
    // 2. Initialize the node
    let node = {
      info,
      path,
      children: [], // must propogate after
      siblings: [], // must propogate after
      sibling_idx: (-1),
      name: Namer.mk_name(info),
    };
    print_endline("just processed: " ++ node.name);
    // 3. Add the node to the node map
    let node_map = Id.Map.add(Info.id_of(info), node, node_map);
    // 4. Build the children of the node
    print_endline(
      "Added node: "
      ++ node.name
      ++ " (id: "
      ++ Id.to_string(Info.id_of(info))
      ++ ")",
    );
    node_map;
  };
  let rec build_children =
          (
            candidate: Info.t,
            path: list(Id.t),
            node_map: t,
            info_map: Id.Map.t(Info.t),
          )
          : t => {
    print_endline(
      "Building children for the id: " ++ Id.to_string(Info.id_of(candidate)),
    );

    let exp_to_info = (term: Exp.t): Info.t =>
      Utils.exp_to_info(term, info_map);

    let typ_to_info = (term: Typ.t): Info.t =>
      Utils.typ_to_info(term, info_map);

    switch (candidate) {
    | InfoExp({term, _}) =>
      switch (Exp.term_of(term)) {
      | Let(_, def, body) =>
        print_endline(
          "Found Let expression, creating node and processing body",
        );
        // Add this node to the node map
        let new_path = path @ [Info.id_of(candidate)];
        let node_map = init_node(candidate, new_path, node_map);
        // 1. Find children
        let node_map =
          build_children(exp_to_info(def), new_path, node_map, info_map);
        // 2. Find siblings
        // use "old" path for siblings!!
        build_children(exp_to_info(body), path, node_map, info_map);
      // It is also useful to add type defintions to the def-structured AST
      | TyAlias(_, typ, body) =>
        print_endline(
          "Found TyAlias expression, creating node and processing body",
        );
        // Add this node to the node map
        let new_path = path @ [Info.id_of(candidate)];
        let node_map = init_node(candidate, new_path, node_map);
        // 1. Find children (although, not sure if necessary for type defs)
        let node_map =
          build_children(typ_to_info(typ), new_path, node_map, info_map);
        // 2. Find siblings
        // use "old" path for siblings!!
        build_children(exp_to_info(body), path, node_map, info_map);
      | _ =>
        let es = Utils.child_expressions_of_exp(term);
        print_endline(
          "Found other expression type with "
          ++ string_of_int(List.length(es))
          ++ " child expressions",
        );
        let es_mapped = List.map(exp_to_info, es);
        List.fold_left(
          (acc_map: t, e: Info.t) => {
            let new_node_map = build_children(e, path, acc_map, info_map);
            Id.Map.merge(
              (_: Id.t, n1: option(node), n2: option(node)) =>
                switch (n1, n2) {
                | (Some(_v1), Some(v2)) =>
                  // If both maps have the same key, prefer the one with more complete data
                  // (e.g., the one that was built later with more context)
                  Some(v2)
                | (Some(v), None) => Some(v)
                | (None, Some(v)) => Some(v)
                | (None, None) => None
                },
              acc_map,
              new_node_map,
            );
          },
          node_map,
          es_mapped,
        );
      }
    | _ => node_map
    };
  };

  let sibling_idx_of = (path: list(Id.t), self: Id.t): int => {
    path
    |> List.mapi((i, id) => (i, id))
    |> List.find_opt(((_, id)) => id == self)
    |> Option.map(fst)
    |> Option.value(~default=-1);
  };

  let build_siblings_and_trim = (node_map: t): t => {
    /*
     * Builds the siblings list for each node by:
     * 1. First trimming the dummy root from all paths (removing the head)
     * 2. Iterating through each node in the map
     * 3. Finding its parent (second-to-last element in the trimmed path)
     * 4. Looking up the parent's children and setting them as siblings (excluding self)
     * 5. Calculating the sibling index based on position in parent's children list
     *
     * Example: For path [dummy_root, x, y, z], the parent of z is y.
     * The siblings of z are all of y's children except z itself.
     * XXX: A little hacky, but it works for now...
     */
    let node_map =
      node_map
      |> Id.Map.fold(
           (current_id, current_node, acc_map) => {
             switch (parent_of(node_map, current_node)) {
             | Some(parent) =>
               Id.Map.add(
                 current_id,
                 {
                   ...current_node,
                   path: List.tl(current_node.path), // removes dummy root
                   siblings: List.map(id_of, children_of(node_map, parent)),
                   sibling_idx: sibling_idx_of(parent.children, current_id),
                 },
                 acc_map,
               )
             | None => Id.Map.add(current_id, current_node, acc_map)
             }
           },
           node_map,
         );
    node_map;
  };

  let gather_top_level = (node_map: t): list(Id.t) => {
    // If the node has no parent, then it is a top-level node
    // XXX: Hacky, cause it could be done more efficiently either on the fly or in
    // build_siblings_and_trim...
    node_map
    |> Id.Map.bindings
    |> List.filter_map(((id: Id.t, node: node)) => {
         switch (parent_of(node_map, node)) {
         | Some(_) => None
         | None => Some(id)
         }
       });
  };

  let split_path = (path: string): list(string) => {
    String.split_on_char('/', path);
  };

  let id_path_to_name_path =
      (id_path: list(Id.t), node_map: t): list(string) => {
    List.map((id: Id.t) => id_to_name(node_map, id), id_path);
  };

  let path_to_node = (node_map: t, path: string): Id.t => {
    let path_names = split_path(path);
    // Convert each node's path (list of Ids) to a list of names and compare
    // XXX: Very hacky and ineffecient (O(n) in size of node_map)
    // TODO: Implement a method which improves the efficiency of this operation
    Option.get(
      node_map
      |> Id.Map.bindings
      |> List.find_map(((id: Id.t, node: node)) =>
           if (id_path_to_name_path(node.path, node_map) == path_names) {
             Some(id);
           } else {
             None;
           }
         ),
    );
  };

  let build = (zipper: Zipper.t, info_map: Id.Map.t(Info.t)): option(t) => {
    // Move to a valid, non-secondary, non-grout, non-convex term
    let valid_zipper =
      MoveOffWhitespaceHelper.move_to_non_whitespace(zipper, Left);

    switch (
      {
        let* current_term = Indicated.ci_of(valid_zipper, info_map);
        Utils.top_level_term_of(~start_point=current_term, ~info_map);
      }
    ) {
    | None => None
    | Some(top_level_term) =>
      let oldest_ancestor =
        Utils.get_oldest_ancestor(top_level_term, info_map);
      let dummy_root = Id.mk();
      print_endline("Dummy root is: " ++ Id.to_string(dummy_root));
      let node_map =
        Id.Map.add(
          dummy_root,
          {
            // This info is NEVER used, just for the sake of the algorithm and
            // to avoid making this an optional type solely for the dummy root...
            info: oldest_ancestor,
            path: [dummy_root],
            children: [],
            siblings: [],
            sibling_idx: (-1),
            name: "{dummy root}",
          },
          Id.Map.empty,
        );
      let node_map =
        build_children(oldest_ancestor, [dummy_root], node_map, info_map);
      // Build siblings for all nodes and trim dummy root from paths
      let node_map = build_siblings_and_trim(node_map);
      // Remove the dummy root from the node map
      let node_map = Id.Map.remove(dummy_root, node_map);
      Some(node_map);
    };
  };

  module Public = {
    /*
     ================================
     Public methods to build and interact with the tree
     ================================
     */
    let build =
      // Builds the definition-based tree
      // Main driver function, does most the heavy lifting
      build;
    let id_of =
      // Gets the id of a node
      id_of;
    let unwrap =
      // Unwraps the result of building the tree
      unwrap;
    let find =
      // Finds a node in the tree by its id
      find;
    let id_to_name =
      // Gets the name of a node given its id
      id_to_name;
    let parent_of =
      // Gets the parent of a node
      parent_of;
    let children_of =
      // Gets the children of a node
      children_of;
    let descendants_of =
      // Gets all of the descendants of a node
      descendants_of;
    let siblings_of =
      // Gets the siblings of a node
      siblings_of;
    let next_sibling_of =
      // Gets the next sibling of a node
      next_sibling_of;
    let prev_sibling_of =
      // Gets the previous sibling of a node
      prev_sibling_of;
    let gather_top_level =
      // Gathers the top-level nodes from the tree
      gather_top_level;
    let path_to_node =
      // Gets the node id from a path
      path_to_node;
  };
};
