open Util;
open Language;
open Language.Statics;
open OptUtil.Syntax;

type node = {
  // The term associated with this node
  info: Info.t,
  // The incoming parent node in the AST
  parent: option(node),
  // The outgoing children nodes in the AST
  children: list(node),
  // The sibling nodes in the AST, aka the nodes that share the same parent
  siblings: list(node),
  // The index this node is at in the list of siblings
  sibling_idx: int,
  // The name of this node. Constructed through recursively
  // unwrapping the pattern(s) associated with the node
  name: string,
};

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

let rec move_to_non_whitespace = (z: Zipper.t): Zipper.t => {
  is_on_whitespace(z)
    ? {
      switch (Move.primary(ByChar, Left, z)) {
      | Some(z') => move_to_non_whitespace(z')
      | None => raise(Failure("Couldn't move to non-whitespace"))
      };
    }
    : z;
};

// Helper function to get the id of a node, which is
// just its Info.id, as we reuse them
let id_of = (node: node) => Info.id_of(node.info);

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

// Finds the least upper binding node of the current term
let is_node = (candidate: option(Info.t)): option(node) => {
  switch (candidate) {
  | Some(candidate) =>
    switch (candidate) {
    | InfoExp({term, _}) =>
      switch (Exp.term_of(term)) {
      | Let(pat, _, _) =>
        Some({
          info: candidate,
          parent: None,
          siblings: [],
          children: [],
          sibling_idx: 0,
          name: mk_name_from_pat(pat),
        })
      | TyAlias(tpat, _, _) =>
        Some({
          info: candidate,
          parent: None,
          siblings: [],
          children: [],
          sibling_idx: 0,
          name: mk_name_from_tpat(tpat),
        })
      | _ => None
      }
    | _ => None
    }
  | None => None
  };
};

// Requires: The list of ancestor terms must come from the info of the current node.
let rec parent_node_of =
        (
          ancestors: list(Id.t),
          departure_point: Info.t,
          info_map: Id.Map.t(Info.t),
        )
        : option(node) =>
  switch (ancestors) {
  | [candidate, ...rest] =>
    let candidate = Id.Map.find(candidate, info_map);
    switch (candidate) {
    | InfoExp({term, _}) =>
      switch (Exp.term_of(term)) {
      | Let(pat, def, _) =>
        let def_if = Exp.rep_id(def);
        if (Id.equal(def_if, Info.id_of(departure_point))) {
          Some({
            info: candidate,
            parent: None,
            siblings: [],
            children: [],
            sibling_idx: 0,
            name: mk_name_from_pat(pat),
          });
        } else {
          parent_node_of(rest, candidate, info_map);
        };
      | _ => parent_node_of(rest, candidate, info_map)
      }
    | _ => None
    };
  | _ => None
  };

let get_path_to_node =
    (curr_term: Info.t, info_map: Id.Map.t(Info.t)): string => {
  let rec path_to_node = (node: node, path_so_far: string) => {
    switch (
      parent_node_of(Info.ancestors_of(node.info), node.info, info_map)
    ) {
    | Some(parent) =>
      print_endline("Ope! Found node: " ++ parent.name);
      let path_so_far = parent.name ++ " -> " ++ path_so_far;
      path_to_node(parent, path_so_far);
    | None =>
      print_endline("No parent node found");
      path_so_far;
    };
  };
  // Find lowest enclosing let/type binding then continue to bubble up to the root
  switch (
    {
      List.find_map(
        ancestor => is_node(Id.Map.find_opt(ancestor, info_map)),
        [Info.id_of(curr_term), ...Info.ancestors_of(curr_term)],
      );
    }
  ) {
  | Some(node) => path_to_node(node, node.name)
  | None => raise(Failure("No node found"))
  };
};

// Builds a localized AST centered around the current node
// This is likely preferred to building the AST of the entire program.
// All we ever use and show the LLM per tool call is this localized neighborhood anyways.
// Making it inefficient to build the entire AST.
// This AST is built with the current node at the center, and the parent node
// as the root. Only the first generation of children are included for each
// the parent and the current node.
// Time Complexity: O(n + m)
//     where n is the number of child terms of the current node,
//     and m is the number of child terms of the parent node.
// Compare this to the O(p) complexity of the full AST, where p is the number of
// terms in the entire program.
let build_curr_node_info =
    (zipper: Zipper.t, info_map: Id.Map.t(Info.t)): option(node) =>
  // 1. Bubble up from the current term to the lowest enclosing
  //    let binding. This is the current node.
  // 2. Bubble up from here, to the parent node. This is the root node.
  // 3. Recursively find all children of the current and root nodes.
  // 4.
  // Problem: And this is the initial reason to build the entire AST, top-down,
  // but we don't know whether the lowest ancestor has the current term in its
  // body or definiton. If it is the former, then that node is just a sibling,
  // it is the former that we want.
  // The term the cursor is currently at
  // This actually is not needed for building the AST, and was used
  // as an ad-hoc path to get the root term of the InfoMap
  // Todo: find simpler, sensible way to get the root term
  try({
    let zipper = move_to_non_whitespace(zipper);
    let curr_term = Indicated.ci_of(zipper, info_map);
    let curr_node =
      switch (
        {
          let* term = curr_term;
          List.find_map(
            ancestor => is_node(Id.Map.find_opt(ancestor, info_map)),
            [Info.id_of(term), ...Info.ancestors_of(term)],
          );
        }
      ) {
      | Some(node) => node
      | None => raise(Failure("No current node found in the info map"))
      };

    // Finding the children of a node is just
    // recursively accumulating let bindings in in the definion of the current node
    // The base case being once we reach atomic terms in the body.
    // Note: We never recurse on the definition of am inner (child) let binding,
    // as this would imply going on a level deeper in the AST
    // Requires: The initial term must be the info of the current node.
    let child_nodes_of =
        (curr_node: option(node), initial_candidate: option(Info.t))
        : list(node) => {
      let mk_child_node = (name: string, child: Info.t, idx: int): node => {
        info: child,
        parent: curr_node,
        siblings: [],
        children: [],
        sibling_idx: idx,
        name,
      };
      let exp_to_info = (term: Exp.t): Info.t => {
        let e = Exp.rep_id(term);
        Id.Map.find(e, info_map);
      };
      let rec find_children =
              (candidate: Info.t, children: list(node), count: int)
              : list(node) =>
        switch (candidate) {
        | InfoExp({term, _}) =>
          switch (Exp.term_of(term)) {
          | Let(pat, _, body) =>
            let node =
              mk_child_node(mk_name_from_pat(pat), candidate, count);
            find_children(exp_to_info(body), children @ [node], count + 1);
          // It is also useful to add type defintions to the def-structured AST
          | TyAlias(tpat, _, body) =>
            let node =
              mk_child_node(mk_name_from_tpat(tpat), candidate, count);
            find_children(exp_to_info(body), children @ [node], count + 1);
          | Fun(_, e, _, _)
          // As for the rest of the expression cases, we can just recurse on their child
          // expressions, passing the current parent/level as the arguments. (This
          // maintains the idea of an AST structured into levels based on definitions)
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
          | DeferredAp(e, _)
          | Seq(e, _)
          | HintedTest(e, _) =>
            find_children(exp_to_info(e), children, count)
          | Ap(_, e1, e2)
          | Dot(e1, e2)
          | TupLabel(e1, e2)
          | Cons(e1, e2)
          | ListConcat(e1, e2)
          | BinOp(_, e1, e2) =>
            let children' = find_children(exp_to_info(e1), children, count);
            find_children(exp_to_info(e2), children', count);
          | Tuple(es)
          | ListLit(es) =>
            List.fold_left(
              (children, e) => {
                find_children(exp_to_info(e), children, count)
              },
              children,
              es,
            )
          | If(e1, e2, e3) =>
            let children' = find_children(exp_to_info(e1), children, count);
            let children'' =
              find_children(exp_to_info(e2), children', count);
            find_children(exp_to_info(e3), children'', count);
          | Match(e, branches) =>
            let children' = find_children(exp_to_info(e), children, count);
            List.fold_left(
              (children, (_pat, branch_e)) => {
                find_children(exp_to_info(branch_e), children, count)
              },
              children',
              branches,
            );
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
          | TypFun(_, _, _)
          | LivelitName(_)
          | Var(_) => children
          }
        | _ => children
        };
      switch (initial_candidate) {
      | None =>
        switch (curr_node) {
        | None => []
        | Some(node) =>
          let initial_candidate =
            switch (node.info) {
            | InfoExp({term, _}) =>
              switch (Exp.term_of(term)) {
              | Let(_, def, _) =>
                Some(Id.Map.find(Exp.rep_id(def), info_map))
              | _ => None
              }
            | _ => None
            };
          switch (initial_candidate) {
          | Some(initial_candidate) =>
            find_children(initial_candidate, [], 0)
          | None => []
          };
        }
      | Some(initial_candidate) => find_children(initial_candidate, [], 0)
      };
    };

    // Requires: The curr node must have already had its parent attempted to be found.
    let get_siblings_of = (node: node): list(node) =>
      switch (node.parent) {
      | Some(parent) =>
        // don't include the current node in the siblings
        // List.filter(
        //   n => n.name !== node.name,
        //   child_nodes_of(Some(parent), None),
        // )
        child_nodes_of(Some(parent), None)
      | None =>
        // This is a special case.
        // Type/let expression at the top level of the program don't have an explicit "parent",
        // however, they can still be thought of as being siblings of each other.
        // This becomes more clear if we wrap them all in a singular "root"/"global" program binding.
        // We handle this special case here.
        let oldest_ancestor_id =
          ListUtil.hd_opt(List.rev(Info.ancestors_of(node.info)));
        let oldest_ancestor =
          switch (oldest_ancestor_id) {
          | Some(id) => Id.Map.find(id, info_map)
          | None => node.info
          };
        child_nodes_of(None, Some(oldest_ancestor));
      // List.filter(
      //   n => n.name !== node.name,
      //   child_nodes_of(None, Some(oldest_ancestor)),
      // );
      };

    let curr_node = {
      ...curr_node,
      parent:
        parent_node_of(
          Info.ancestors_of(curr_node.info),
          curr_node.info,
          info_map,
        ),
    };
    let curr_node = {
      ...curr_node,
      children: child_nodes_of(Some(curr_node), None),
      siblings: get_siblings_of(curr_node),
    };
    // print the current node, parent, children, siblings, and their naems
    print_endline("curr node: " ++ curr_node.name);
    print_endline(
      "parent: "
      ++ (
        switch (curr_node.parent) {
        | Some(parent) => parent.name
        | None => "None"
        }
      ),
    );
    print_endline(
      "children: "
      ++ String.concat(
           ", ",
           List.map(child => child.name, curr_node.children),
         ),
    );
    print_endline(
      "siblings: "
      ++ String.concat(
           ", ",
           List.map(sibling => sibling.name, curr_node.siblings),
         ),
    );
    Some(curr_node);
  }) {
  | _ => None
  };

let get_node = (curr_node_info: option(node)) => {
  switch (curr_node_info) {
  | Some(curr_node_info) => curr_node_info
  | None => raise(Failure("No current node found in the info map"))
  };
};

// TODO: Build a function to get the path to the current node.

// TODO: safe_move should work in most edit cases, EXCEPT for insert
//       before/after, because the zipper/info map are outdated
//       and I cannot figure out how to update them to successfully do this.
let safe_move =
    (z: Zipper.t, info_map: Statics.Map.t, module M: Move.S)
    : option(Zipper.t) => {
  // Try moving to the first parent first, otherwise, move to the first sibling
  // otherwise, move to the top level of the program
  // TODO: make this even safer. also make more clear to llm how we moved after a deletion.
  // also handle the case of an empty program.

  module Select = Select.Make(M);
  module Move = Move.Make(M);

  print_endline("here #8.0 safe_move");

  // If we are moving, then it should be the case that we are at an existing node.
  let curr_node_info = get_node(build_curr_node_info(z, info_map));

  print_endline("here #8.1 safe_move (after building sub AST)");

  switch (Select.tile(Info.id_of(curr_node_info.info), z)) {
  | Some(z) =>
    print_endline("here #8.2 safe_move (after selecting current term)");
    Some(z);
  // Otherwise, try moving to the parent
  | None =>
    print_endline("here #8.2.1 safe_move (trying to select parent instead)");
    switch (
      {
        let* parent = curr_node_info.parent;
        print_endline(
          "here #8.2.1.1 safe_move (trying to select parent term)",
        );
        let+ z' = Select.tile(Info.id_of(parent.info), z);
        print_endline(
          "here #8.2.1.2 safe_move (after selecting parent term)",
        );
        z';
      }
    ) {
    | Some(z) =>
      print_endline("here #8.3 safe_move (after selecting parent)");
      Some(z);
    | None =>
      print_endline(
        "here #8.2.2 safe_move (trying to select preceding sibling instead)",
      );
      switch (
        {
          let* prec_sibling =
            ListUtil.nth_opt(
              curr_node_info.sibling_idx - 1,
              curr_node_info.siblings,
            );
          Select.tile(Info.id_of(prec_sibling.info), z);
        }
      ) {
      | Some(z) =>
        print_endline(
          "here #8.4 safe_move (after selecting preceding sibling)",
        );
        Some(z);
      | None =>
        print_endline("here #8.2.3 safe_move (no preceding sibling found)");
        print_endline(
          "here #8.5 safe_move (after selecting preceding sibling)",
        );
        print_endline("no siblings still exist in info map");
        // TODO: this is a bad case. it means the program is empty :(
        // or has no let/type expressions. or something else very bad.
        let z' =
          switch (Move.do_extreme(Move.primary(ByToken), Up, z)) {
          | Some(z') => z'
          | None => z
          };
        Select.go(Extreme(Down), z');
      };
    };
  };
};
