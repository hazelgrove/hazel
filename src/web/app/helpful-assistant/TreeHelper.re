open Util;
open Haz3lcore;
open Language;
open Language.Statics;

type node = {
  // The term associated with this node
  info: Info.t,
  // The incoming parent node in the AST
  parent: option(node),
  // The outgoing children nodes in the AST
  children: list(node),
  // The sibling nodes in the AST, aka the nodes that share the same parent
  siblings: list(node),
  // The name of this node. Constructed through recursively
  // unwrapping the pattern(s) associated with the node
  name: string,
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
let rec curr_node_of =
        (candidate: option(Info.t), info_map: Id.Map.t(Info.t))
        : option(node) => {
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
          name: mk_name_from_pat(pat),
        })
      | TyAlias(tpat, _, _) =>
        Some({
          info: candidate,
          parent: None,
          siblings: [],
          children: [],
          name: mk_name_from_tpat(tpat),
        })
      | _ =>
        switch (Info.ancestors_of(candidate)) {
        | [ancestor, ..._] =>
          curr_node_of(Id.Map.find_opt(ancestor, info_map), info_map)
        | _ => None
        }
      }
    | _ =>
      switch (Info.ancestors_of(candidate)) {
      | [ancestor, ..._] =>
        curr_node_of(Id.Map.find_opt(ancestor, info_map), info_map)
      | _ => None
      }
    }
  | None => None
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
let build_sub_AST = (editor: CodeWithStatics.Model.t): node => {
  // 1. Bubble up from the current term to the lowest enclosing
  //    let binding. This is the current node.
  // 2. Bubble up from here, to the parent node. This is the root node.
  // 3. Recursively find all children of the current and root nodes.
  // 4.
  // Problem: And this is the initial reason to build the entire AST, top-down,
  // but we don't know whether the lowest ancestor has the current term in its
  // body or definiton. If it is the former, then that node is just a sibling,
  // it is the former that we want.
  let zipper = editor.editor.state.zipper;
  // The current datastructure with AST information
  let info_map = editor.statics.info_map;

  // The term the cursor is currently at
  // This actually is not needed for building the AST, and was used
  // as an ad-hoc path to get the root term of the InfoMap
  // Todo: find simpler, sensible way to get the root term
  let curr_id: option(Id.t) = Indicated.index(zipper);
  let curr_term =
    switch (curr_id) {
    | Some(id) => Id.Map.find_opt(id, info_map)
    | None => raise(Failure("No current term"))
    };

  // Requires: The list of ancestor terms must come from the info of the current node.
  let rec parent_node_of =
          (ancestors: list(Id.t), departure_point: Info.t): option(node) => {
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
              name: mk_name_from_pat(pat),
            });
          } else {
            parent_node_of(rest, candidate);
          };
        | _ => parent_node_of(rest, candidate)
        }
      | _ => None
      };
    | _ => None
    };
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
    let mk_child_node = (name: string, child: Info.t): node => {
      {
        info: child,
        parent: curr_node,
        siblings: [],
        children: [],
        name,
      };
    };
    let convert_for_recursion = (term: Exp.t): Info.t => {
      let e = Exp.rep_id(term);
      Id.Map.find(e, info_map);
    };
    let rec find_children =
            (candidate: Info.t, children: list(node)): list(node) => {
      switch (candidate) {
      | InfoExp({term, _}) =>
        switch (Exp.term_of(term)) {
        | Let(pat, _, body) =>
          let node = mk_child_node(mk_name_from_pat(pat), candidate);
          find_children(convert_for_recursion(body), children @ [node]);
        // It is also useful to add type defintions to the def-structured AST
        | TyAlias(tpat, _, body) =>
          let node = mk_child_node(mk_name_from_tpat(tpat), candidate);
          find_children(convert_for_recursion(body), children @ [node]);
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
          find_children(convert_for_recursion(e), children)
        | Ap(_, e1, e2)
        | Dot(e1, e2)
        | TupLabel(e1, e2)
        | Cons(e1, e2)
        | ListConcat(e1, e2)
        | BinOp(_, e1, e2) =>
          let children' = find_children(convert_for_recursion(e1), children);
          find_children(convert_for_recursion(e2), children');
        | Tuple(es)
        | ListLit(es) =>
          List.fold_left(
            (children, e) => {
              find_children(convert_for_recursion(e), children)
            },
            children,
            es,
          )
        | If(e1, e2, e3) =>
          let children' = find_children(convert_for_recursion(e1), children);
          let children'' =
            find_children(convert_for_recursion(e2), children');
          find_children(convert_for_recursion(e3), children'');
        | Match(e, branches) =>
          let children' = find_children(convert_for_recursion(e), children);
          List.fold_left(
            (children, (_pat, branch_e)) => {
              find_children(convert_for_recursion(branch_e), children)
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
        | Some(initial_candidate) => find_children(initial_candidate, [])
        | None => []
        };
      }
    | Some(initial_candidate) => find_children(initial_candidate, [])
    };
  };

  // Requires: The curr node must have already had its parent attempted to be found.
  let get_siblings_of = (node: node): list(node) => {
    switch (node.parent) {
    | Some(parent) =>
      // don't include the current node in the siblings
      List.filter(
        n => n.name !== node.name,
        child_nodes_of(Some(parent), None),
      )
    | None =>
      // This is a special case.
      // Type/let expression at the top level of the program don't have an explicit "parent",
      // however, they can still be thought of as all being siblings of each other.
      // This becomes more clear if we wrap them all in a singular "root"/"global" program binding.info
      // Nevertheless, we handle the special case here.
      let oldest_ancestor_id =
        ListUtil.hd_opt(List.rev(Info.ancestors_of(node.info)));
      let oldest_ancestor =
        switch (oldest_ancestor_id) {
        | Some(id) => Id.Map.find(id, info_map)
        | None => node.info
        };
      List.filter(
        n => n.name !== node.name,
        child_nodes_of(None, Some(oldest_ancestor)),
      );
    };
  };

  let curr_node = Option.get(curr_node_of(curr_term, info_map));
  let curr_node = {
    ...curr_node,
    parent:
      parent_node_of(Info.ancestors_of(curr_node.info), curr_node.info),
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
    ++ String.concat(", ", List.map(child => child.name, curr_node.children)),
  );
  print_endline(
    "siblings: "
    ++ String.concat(
         ", ",
         List.map(sibling => sibling.name, curr_node.siblings),
       ),
  );
  curr_node;
};

// TODO: Build a function to get the path to the current node.

module View = {
  // The following functions are to help with viewing the AST
  // as a modified version of the editor.
  // This allows use to modify the term-base of the editor itself,
  // covering up inner child definitions with folds,
  // and any other modifications we might want to make to the editor
  // before displaying to the LLM.
  let mk_syntax: Zipper.t => Editor.CachedSyntax.t =
    Editor.CachedSyntax.init(
      ~info_map=Language.Statics.Map.empty,
      ~dyn_map=Language.Dynamics.Map.empty,
    );
  let mk_state: Zipper.t => Editor.State.t =
    z => {
      zipper: z,
      col_target: None,
    };
  let mk_move = (z: Zipper.t): (module Move.S) =>
    Editor.Model.to_move_s({
      state: mk_state(z),
      syntax: mk_syntax(z),
    });
  let perform = (a: Action.t, z: Zipper.t) =>
    Perform.go_z(
      ~settings=Language.CoreSettings.off,
      CachedStatics.empty,
      a,
      mk_move(z),
      z,
    );

  let definition = (z: Zipper.t, curr_node: node) => {
    let rec fold_term = (z: Zipper.t, ids: list(Id.t)) => {
      switch (ids) {
      | [] => z
      | [id, ...rest] =>
        let z' = perform(Action.Select(Term(Id(id, Direction.Right))), z);
        switch (z') {
        | Ok(z') =>
          let z'' =
            perform(Action.Project(SetIndicated(Specific(Fold))), z');
          switch (z'') {
          | Ok(z'') => fold_term(z'', rest)
          | _ => fold_term(z', rest)
          };
        | _ => fold_term(z, rest)
        };
      };
    };
    let get_def_id_of_let = (term: Info.t): Id.t => {
      switch (term) {
      | InfoExp({term, _}) =>
        switch (Exp.term_of(term)) {
        | Let(_, def, _) => Exp.rep_id(def)
        | _ => Id.invalid
        }
      | _ => Id.invalid
      };
    };
    let children_def_ids =
      List.map((c: node) => get_def_id_of_let(c.info), curr_node.children);
    let siblings_def_ids =
      List.map((c: node) => get_def_id_of_let(c.info), curr_node.siblings);

    let z = fold_term(z, children_def_ids);
    let z' = fold_term(z, siblings_def_ids);

    let z'' =
      switch (curr_node.parent) {
      | Some(parent) =>
        switch (
          perform(
            Action.Select(Tile(Id(id_of(parent), Direction.Right))),
            z',
          )
        ) {
        | Ok(z'') => z''
        | _ => z
        }
      | None =>
        switch (perform(Action.Select(All), z')) {
        | Ok(z'') => z''
        | _ => z
        }
      };

    let seg = z''.selection.content;
    seg;
  };
};
