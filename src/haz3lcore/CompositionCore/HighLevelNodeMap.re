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
    | FumolaPeek(_)
    | TyAlias(_)
    | ExplicitNonlabel
    | Theorem(_, _, _)
    | ProofObject(_)
    | Forall(_, _)
    | Projector(_, _)
    | Var(_)
    | Module(_) => []
    | ModuleExp(_, def, body) => [def, body]
    | DrvQuote(_) => []
    };
  };

  let top_level_term_of =
      (~start_point: Info.t, ~info_map: Id.Map.t(Info.t)): option(Info.t) => {
    let initial_nominee =
      switch (start_point) {
      | InfoExp({user_term: term, _}) =>
        switch (Exp.term_of(term)) {
        | Let(_) => Some(start_point) // if it is a let binding, then this term is a nominee
        | TyAlias(_) => Some(start_point) // if it is a type binding, then this term is a nominee
        | ModuleExp(_) => Some(start_point) // if it is a module binding, then this term is a nominee
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
          | InfoExp({user_term: term, _}) =>
            switch (Exp.term_of(term)) {
            | Let(pat, def, body) =>
              let pat_id = Pat.rep_id(pat);
              let def_id = Exp.rep_id(def);
              let body_id = Exp.rep_id(body);
              /* Fn-sugar lets desugar to Let(f, Fun(args, def), body) in
                 statics, so the ancestor chain reaches this Let via the
                 synthetic Fun wrapper rather than pat/def/body directly;
                 any chain position is necessarily inside the binding. */
              if (Option.is_some(FunctionSugar.detect(pat))
                  || Id.equal(def_id, Info.id_of(departure_point))
                  || Id.equal(body_id, Info.id_of(departure_point))
                  || Id.equal(pat_id, Info.id_of(departure_point))) {
                // if it is, then set this is as the new nominee for the top-level node
                Some(
                  candidate,
                );
              } else {
                nominee;
              };
            | TyAlias(tpat, tdef, body) =>
              let tpat_id = TPat.rep_id(tpat);
              let tdef_id = Typ.rep_id(tdef);
              let body_id = Exp.rep_id(body);
              if (Id.equal(tdef_id, Info.id_of(departure_point))
                  || Id.equal(body_id, Info.id_of(departure_point))
                  || Id.equal(tpat_id, Info.id_of(departure_point))) {
                Some(candidate);
              } else {
                nominee;
              };
            | ModuleExp(mp, def, body) =>
              let mp_id = MPat.rep_id(mp);
              let def_id = Exp.rep_id(def);
              let body_id = Exp.rep_id(body);
              if (Id.equal(def_id, Info.id_of(departure_point))
                  || Id.equal(body_id, Info.id_of(departure_point))
                  || Id.equal(mp_id, Info.id_of(departure_point))) {
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

module Namer = {
  /*
   Convert patterns to purely string names.
   */

  let rec mk_name_from_pat = (pat: TermBase.pat_t) => {
    switch (pat.term) {
    | Var(name)
    | Constructor(name, _)
    | Label(name) => name
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
    /* Fn-definition sugar `let f(x, y) = ...`: the binding is addressed by
       the bare head name; collisions fall to the #k ambiguity machinery. */
    | Ap(pat1, _) => mk_name_from_pat(pat1)
    | TupLabel(pat1, pat2) =>
      "(" ++ mk_name_from_pat(pat1) ++ " : " ++ mk_name_from_pat(pat2) ++ ")"
    | Atom(_) => "{atom}"
    | ExplicitNonlabel => "{explicit nonlabel}"
    | Invalid(_) => "{invalid}"
    | Projector(_, _) => "{projector}"
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

  let rec mk_name_from_mpat = (mp: MPat.t) => {
    switch (mp.term) {
    | Var(name) => name
    | Asc(mp_inner, _) => mk_name_from_mpat(mp_inner)
    | Invalid(name) => name
    | EmptyHole => "{empty module pattern hole}"
    | MultiHole(_) => "{multi module pattern hole}"
    };
  };

  let mk_name = (info: Info.t): string => {
    switch (info) {
    | InfoExp({user_term: term, _}) =>
      switch (Exp.term_of(term)) {
      | Let(pat, _, _) => mk_name_from_pat(pat)
      | TyAlias(tpat, _, _) => mk_name_from_tpat(tpat)
      | ModuleExp(mp, _, _) => mk_name_from_mpat(mp)
      | _ => raise(Failure("Not a valid expression to make a name from"))
      }
    | _ => raise(Failure("Not a valid term to make a name from"))
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
    | Some({piece, _}) =>
      Piece.is_secondary(piece)
      || Piece.is_grout(piece)
      || Piece.is_convex(piece)
    | None => false
    };
  };

  let rec move_to_non_whitespace =
          (z: Zipper.t, d: Direction.t): option(Zipper.t) => {
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
            None;
          }
        };
      }
      : Some(z);
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type node = HighLevelNodeMapModel.node;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = HighLevelNodeMapModel.t;

let id_of = (node: node) => {
  Info.id_of(node.info);
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
        |> List.map((id: Id.t) => find(node_map, id))
        |> List.map((node: node) => node.children)
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

/* `mod` binds tighter than `+/-`, so parens matter; also wrap negatives before mod. */
let next_sibling_of = (node: node): option(Id.t) => {
  let len = List.length(node.siblings);
  if (len == 0) {
    None;
  } else {
    List.nth_opt(node.siblings, (node.sibling_idx + 1) mod len);
  };
};

let prev_sibling_of = (node: node): option(Id.t) => {
  let len = List.length(node.siblings);
  if (len == 0) {
    None;
  } else {
    List.nth_opt(node.siblings, (node.sibling_idx - 1 + len) mod len);
  };
};

let lowest_enclosing_node_of = (info: Info.t, node_map: t): option(node) => {
  switch (Id.Map.find_opt(Info.id_of(info), node_map)) {
  | Some(node) => Some(node)
  | None =>
    let ancestors = Info.ancestors_of(info);
    List.find_map(
      (ancestor: Id.t) => Id.Map.find_opt(ancestor, node_map),
      ancestors,
    );
  };
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
  // 1. Add this node to the children of its parent (head of rev(path)), if one exists
  let node_map =
    switch (parent_id_from_path(path)) {
    | Some(parent) => add_child(node_map, parent, Info.id_of(info))
    | None => node_map
    };
  // 2. Initialize the node
  let node: node = {
    info,
    path,
    children: [], // must propogate after
    siblings: [], // must propogate after
    sibling_idx: (-1),
    name: Namer.mk_name(info),
  };
  // 3. Add the node to the node map
  let node_map = Id.Map.add(Info.id_of(info), node, node_map);
  // 4. Build the children of the node
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
  let exp_to_info = (term: Exp.t): Info.t =>
    Utils.exp_to_info(term, info_map);

  let typ_to_info = (term: Typ.t): Info.t =>
    Utils.typ_to_info(term, info_map);

  switch (candidate) {
  | InfoExp({user_term: term, _}) =>
    switch (Exp.term_of(term)) {
    | Let(_, def, body) =>
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
      // Add this node to the node map
      let new_path = path @ [Info.id_of(candidate)];
      let node_map = init_node(candidate, new_path, node_map);
      // 1. Find children (although, not sure if necessary for type defs)
      let node_map =
        build_children(typ_to_info(typ), new_path, node_map, info_map);
      // 2. Find siblings
      // use "old" path for siblings!!
      build_children(exp_to_info(body), path, node_map, info_map);
    | ModuleExp(_, def, body) =>
      // Add this node to the node map (module M = def in body)
      let new_path = path @ [Info.id_of(candidate)];
      let node_map = init_node(candidate, new_path, node_map);
      // 1. Find children (the module body)
      let node_map =
        build_children(exp_to_info(def), new_path, node_map, info_map);
      // 2. Find siblings (continuation after "in")
      build_children(exp_to_info(body), path, node_map, info_map);
    | _ =>
      let es = Utils.child_expressions_of_exp(term);
      let es_mapped = List.map(exp_to_info, es);
      /* [build_children] threads the accumulated map, so each result already
         contains everything in the accumulator; no per-child merge needed. */
      List.fold_left(
        (acc_map: t, e: Info.t) =>
          build_children(e, path, acc_map, info_map),
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

let id_path_to_name_path = (id_path: list(Id.t), node_map: t): list(string) => {
  List.map((id: Id.t) => id_to_name(node_map, id), id_path);
};

/** Paths may carry a trailing `#k` (1-based) selecting the k-th binding, in
    program order, among several that share the same name path (shadowing).
    A `#` not followed by a positive integer is treated as part of the name. */
let split_disambiguator = (path: string): (string, option(int)) => {
  switch (String.rindex_opt(path, '#')) {
  | None => (path, None)
  | Some(i) =>
    let base = String.sub(path, 0, i);
    let suffix = String.sub(path, i + 1, String.length(path) - i - 1);
    switch (int_of_string_opt(suffix)) {
    | Some(k) when k >= 1 => (base, Some(k))
    | _ => (path, None)
    };
  };
};

/** Deterministic order for bindings sharing a name path: lowest
    [[sibling_idx]] first (program order within a chain), [[Id.compare]] as
    tiebreak. Defines the numbering used by the `#k` disambiguator. */
let sort_ids_in_program_order = (node_map: t, ids: list(Id.t)): list(Id.t) => {
  let cmp = (id1: Id.t, id2: Id.t): int => {
    let n1 = find(node_map, id1);
    let n2 = find(node_map, id2);
    switch (Int.compare(n1.sibling_idx, n2.sibling_idx)) {
    | 0 => Id.compare(id1, id2)
    | c => c
    };
  };
  List.sort(cmp, ids);
};

let matches_for_path = (node_map: t, path_names: list(string)): list(Id.t) => {
  // Convert each node's path (list of Ids) to a list of names and compare
  // XXX: O(n) in bindings (not program size); fine until node maps get large.
  Id.Map.bindings(node_map)
  |> List.filter_map(((id: Id.t, node: node)) =>
       id_path_to_name_path(node.path, node_map) == path_names
         ? Some(id) : None
     )
  |> sort_ids_in_program_order(node_map);
};

let ambiguous_path_message = (base: string, ids: list(Id.t)): string => {
  let n = List.length(ids);
  let options =
    List.init(n, i => "\"" ++ base ++ "#" ++ string_of_int(i + 1) ++ "\"")
    |> String.concat(", ");
  "Path \""
  ++ base
  ++ "\" is ambiguous: "
  ++ string_of_int(n)
  ++ " bindings share this path (same name, e.g. shadowing)."
  ++ "\nRetry with one of the disambiguated paths: "
  ++ options
  ++ " (\"#1\" is the earliest binding in program order).";
};

let path_to_id_opt = (node_map: t, path: string): option(Id.t) => {
  let (base, occurrence) = split_disambiguator(path);
  let matches = matches_for_path(node_map, split_path(base));
  switch (matches, occurrence) {
  | ([], _) => None
  | (ids, Some(k)) => List.nth_opt(ids, k - 1)
  | ([id], None) => Some(id)
  | ([_, _, ..._], None) => None // ambiguous: refuse to guess (see path_to_id)
  };
};

/** Map nodes are keyed by the **binding** expression (Let / TyAlias / ModuleExp).
    [[Select.term]] + syntax projectors must target the **definition** (rhs), not the whole binding. */
let binding_id_to_syntax_projector_target_id =
    (node_map: t, binding_id: Id.t): option(Id.t) => {
  switch (Id.Map.find_opt(binding_id, node_map)) {
  | None => None
  | Some(node) =>
    switch (node.info) {
    | InfoExp({user_term: term, _}) =>
      switch (Exp.term_of(term)) {
      | Let(_, def, _) => Some(Exp.rep_id(def))
      | TyAlias(_, typ, _) => Some(Typ.rep_id(typ))
      | ModuleExp(_, def, _) => Some(Exp.rep_id(def))
      | _ => None
      }
    | _ => None
    }
  };
};

let path_to_syntax_projector_target_id_opt =
    (node_map: t, path: string): option(Id.t) => {
  switch (path_to_id_opt(node_map, path)) {
  | None => None
  | Some(binding_id) =>
    binding_id_to_syntax_projector_target_id(node_map, binding_id)
  };
};

let closest_valid_path_to_ill_path = (node_map: t, path: string): string => {
  // Prefer matching the **last** path segment (binding name) so nested bindings
  // like outer/t rank above top-level outer when the user typed "t".
  let path_names = split_path(path);

  let path_str = path;
  let want_last = ListUtil.last_opt(path_names) |> Option.value(~default="");

  /* suffix_dist, list_dist, char_dist, neg_depth (deeper wins on tie). */
  let distance_for_node = (node: node): (int, int, int, int) => {
    let candidate_names = id_path_to_name_path(node.path, node_map);
    let cand_last =
      ListUtil.last_opt(candidate_names) |> Option.value(~default="");
    let suffix_dist = StringUtil.levenshtein_distance(want_last, cand_last);
    let list_dist =
      StringUtil.levenshtein_list_distance(path_names, candidate_names);
    let candidate_str = String.concat("/", candidate_names);
    let char_dist = StringUtil.levenshtein_distance(path_str, candidate_str);
    let neg_depth = - List.length(node.path);
    (suffix_dist, list_dist, char_dist, neg_depth);
  };

  let is_better =
      (
        (s1, l1, c1, d1): (int, int, int, int),
        (s2, l2, c2, d2): (int, int, int, int),
      )
      : bool =>
    if (s1 < s2) {
      true;
    } else if (s1 > s2) {
      false;
    } else if (l1 < l2) {
      true;
    } else if (l1 > l2) {
      false;
    } else if (c1 < c2) {
      true;
    } else if (c1 > c2) {
      false;
    } else {
      d1 < d2;
    };

  switch (Id.Map.bindings(node_map)) {
  /* Called from error-handling paths (path_to_id failure branch); must not raise. */
  | [] => ""
  | bindings =>
    let (first_id, first_node) = List.hd(bindings);
    let d0 = distance_for_node(first_node);
    let initial_acc = (first_id, first_node, d0);

    let (best_id, _best_node, _best_dist) =
      List.fold_left(
        ((acc_id, acc_node, acc_d), (id, node)) => {
          let d = distance_for_node(node);
          if (is_better(d, acc_d)) {
            (id, node, d);
          } else {
            (acc_id, acc_node, acc_d);
          };
        },
        initial_acc,
        List.tl(bindings),
      );

    let path = find(node_map, best_id).path;
    String.concat(
      "/",
      List.map((id: Id.t) => id_to_name(node_map, id), path),
    );
  };
};

/** All binding paths in [[node_map]] rendered as their slash-joined name
    strings, sorted for stable output. Used for failure diagnostics so the
    agent can self-correct rather than guess. */
let all_path_strings = (node_map: t): list(string) => {
  Id.Map.bindings(node_map)
  |> List.map(((_, node: node)) =>
       String.concat(
         "/",
         List.map((id: Id.t) => id_to_name(node_map, id), node.path),
       )
     )
  |> List.sort_uniq(String.compare);
};

/** If [[path]] is a bare name (no `/`) and exactly one binding in the map has
    that as its last name segment, return its full qualified path. This catches
    the nested-binding case ("meant Square/Piece?") where the bare name didn't
    match top-level but is uniquely present deeper. */
let unique_suffix_match = (node_map: t, path: string): option(string) =>
  if (String.contains(path, '/')) {
    None;
  } else {
    let matches =
      Id.Map.bindings(node_map)
      |> List.filter_map(((_, node: node)) => {
           let names =
             List.map((id: Id.t) => id_to_name(node_map, id), node.path);
           switch (ListUtil.last_opt(names)) {
           | Some(last) when String.equal(last, path) =>
             Some(String.concat("/", names))
           | _ => None
           };
         });
    switch (matches) {
    | [qualified] => Some(qualified)
    | _ => None
    };
  };

let path_to_id = (node_map: t, path: string): Id.t => {
  let (base, occurrence) = split_disambiguator(path);
  let matches = matches_for_path(node_map, split_path(base));
  switch (matches, occurrence) {
  | ([], _) =>
    let closest = closest_valid_path_to_ill_path(node_map, base);
    let suffix_hint =
      switch (unique_suffix_match(node_map, base)) {
      | Some(qualified) when !String.equal(qualified, closest) =>
        "\nOr the fully qualified nested path \"" ++ qualified ++ "\"."
      | _ => ""
      };
    let available = all_path_strings(node_map);
    let available_str =
      switch (available) {
      | [] => ""
      | xs => "\nAvailable paths in the node map: " ++ String.concat(", ", xs)
      };
    raise(
      Failure(
        "Path \""
        ++ path
        ++ "\" not found in node map"
        ++ "\nPerhaps you meant \""
        ++ closest
        ++ "\"?"
        ++ suffix_hint
        ++ "\nNested bindings use paths like outer/inner (not just the inner name)."
        ++ available_str,
      ),
    );
  | (ids, Some(k)) =>
    switch (List.nth_opt(ids, k - 1)) {
    | Some(id) => id
    | None =>
      raise(
        Failure(
          "Path \""
          ++ path
          ++ "\": occurrence #"
          ++ string_of_int(k)
          ++ " is out of range; only "
          ++ string_of_int(List.length(ids))
          ++ " binding(s) match \""
          ++ base
          ++ "\".",
        ),
      )
    }
  | ([id], None) => id
  | ([_, _, ..._] as ids, None) =>
    raise(Failure(ambiguous_path_message(base, ids)))
  };
};

let path_to_node = (node_map: t, path: string): node => {
  find(node_map, path_to_id(node_map, path));
};

let node_of_cursor =
    (node_map: t, z: Zipper.t, info_map: Id.Map.t(Info.t)): node => {
  switch (
    {
      let* valid_zipper =
        MoveOffWhitespaceHelper.move_to_non_whitespace(z, Left);
      Indicated.ci_of(valid_zipper, info_map);
    }
  ) {
  | None =>
    raise(
      Failure(
        "No current position found for cursor when trying to get node of cursor",
      ),
    )
  | Some(info) =>
    switch (lowest_enclosing_node_of(info, node_map)) {
    | Some(node) => node
    | None => raise(Failure("No lowest enclosing node found for cursor"))
    }
  };
};

let build = (zipper: Zipper.t, info_map: Id.Map.t(Info.t)): option(t) => {
  // Move to a valid, non-secondary, non-grout, non-convex term
  switch (
    {
      let* valid_zipper =
        MoveOffWhitespaceHelper.move_to_non_whitespace(zipper, Left);
      let* current_term = Indicated.ci_of(valid_zipper, info_map);
      Utils.top_level_term_of(~start_point=current_term, ~info_map);
    }
  ) {
  | None => None
  | Some(top_level_term) =>
    let oldest_ancestor = Utils.get_oldest_ancestor(top_level_term, info_map);
    let dummy_root = Id.mk();
    let node_map: t =
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
        }: node,
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
  let path_to_id =
    // Gets the node id from a path
    path_to_id;
  let path_to_id_opt =
    // Gets the node id from a path, returns None if the path is not found
    path_to_id_opt;
  let path_to_syntax_projector_target_id_opt =
    // Rep id of the binding's definition term (for place_syntax_projector / toggle / remove)
    path_to_syntax_projector_target_id_opt;
  let path_to_node =
    // Gets the node from a path
    path_to_node;
  let node_of_cursor =
    // Gets the node at the cursor position
    // Useful for post-edit checks and operations
    node_of_cursor;
};
