open Language;
open Haz3lcore;
open Util;

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

type ast = Id.Map.t(node);

// Helper function to get the id of a node, which is
// just its Info.id, as we reuse them
let id_of = (node: node) => Info.id_of(node.info);

// Helper function for getting the current node in the AST
// Uses the cursor position in the given editor to find the corresponding
// node in the AST.
// Note this doesn't just return the term the cursor is at, but rather it's
// least-most ancestor term that exists in the AST.
let get_curr_node = (editor: CodeWithStatics.Model.t, ast: ast): option(node) => {
  let info_map = editor.statics.info_map;
  switch (Indicated.index(editor.editor.state.zipper)) {
  | Some(id) =>
    switch (Id.Map.find_opt(id, ast)) {
    // If the id is a node in the AST, return it as the current node
    | Some(node) => Some(node)
    // Otherwise, recursively bubble up through the ancestors of the current term
    // until we reach a node in the AST
    | None =>
      switch (Id.Map.find_opt(id, info_map)) {
      | Some(term) =>
        let ancestors = Info.ancestors_of(term);
        let rec get_nearest_ancestor_node = (ancestors: list(Id.t)) => {
          switch (ancestors) {
          // No ancestors were found in the AST
          | [] => None
          | [ancestor, ...rest] =>
            switch (Id.Map.find_opt(ancestor, ast)) {
            // This ancestor is in the AST, return it
            | Some(node) => Some(node)
            // Otherwise, continue bubbling up
            | None => get_nearest_ancestor_node(rest)
            }
          };
        };
        get_nearest_ancestor_node(ancestors);
      | None => None
      }
    }
  | None => None
  };
};

// let build_AST = (editor: CodeWithStatics.Model.t): ast => {
//   let zipper = editor.editor.state.zipper;
//   // The current datastructure with AST information
//   let info_map = editor.statics.info_map;

//   // The term the cursor is currently at
//   // This actually is not needed for building the AST, and was used
//   // as an ad-hoc path to get the root term of the InfoMap
//   // Todo: find simpler, sensible way to get the root term
//   let curr_id: option(Id.t) = Indicated.index(zipper);
//   let curr_term =
//     switch (curr_id) {
//     | Some(id) => Id.Map.find_opt(id, info_map)
//     | None => raise(Failure("No current term"))
//     };

//   // The recursive AST builder function.
//   // We begin from a root node, visiting all children and possible subtrees from that root.
//   let rec build =
//           (ast: ast, level: int, parent_id: Id.t, curr_term: Info.t): ast => {
//     let rec mk_name_from_pat = (pat: TermBase.pat_t) => {
//       switch (pat.term) {
//       | Var(name)
//       | Constructor(name, _)
//       | Label(name) => name
//       | Probe(pat, _)
//       | Parens(pat)
//       | Asc(pat, _) => mk_name_from_pat(pat)
//       | Cons(pat1, pat2) =>
//         mk_name_from_pat(pat1) ++ "::" ++ mk_name_from_pat(pat2)
//       | Tuple(pats) =>
//         "(" ++ String.concat(", ", List.map(mk_name_from_pat, pats)) ++ ")"
//       | ListLit(pats) =>
//         "[" ++ String.concat(", ", List.map(mk_name_from_pat, pats)) ++ "]"
//       | Wild => "{wild}"
//       | EmptyHole => "{empty pattern hole}"
//       | MultiHole(_) => "{multi hole}"
//       | Ap(pat1, pat2) =>
//         mk_name_from_pat(pat1) ++ "(" ++ mk_name_from_pat(pat2) ++ ")"
//       | TupLabel(pat1, pat2) =>
//         "("
//         ++ mk_name_from_pat(pat1)
//         ++ " : "
//         ++ mk_name_from_pat(pat2)
//         ++ ")"
//       | Atom(_) => "{atom}"
//       | Invalid(_) => "{invalid}"
//       };
//     };
//     let mk_name_from_tpat = (tpat: TermBase.tpat_t) => {
//       switch (tpat.term) {
//       | Var(name)
//       | Invalid(name) => name
//       | EmptyHole => "{empty type pattern hole}"
//       | MultiHole(_) => "{multi type pattern hole}"
//       };
//     };
//     let mk_node =
//         (name: string, def: option(TermBase.exp_t), body: TermBase.exp_t) => {
//       let def_id =
//         switch (def) {
//         | Some(def) => Some(Exp.rep_id(def))
//         | None => None
//         };
//       let body_id = Exp.rep_id(body);
//       let def_term =
//         switch (def_id) {
//         | Some(def_id) => Id.Map.find_opt(def_id, info_map)
//         | None => None
//         };
//       let body_term = Option.get(Id.Map.find_opt(body_id, info_map));
//       let new_node = {
//         self: curr_term,
//         parent: parent_id,
//         children: [],
//         level,
//         name,
//       };
//       // We can guarantee the parent exists, as only the root node is parentless, and we never
//       // pass the root node as a curr_term being built from
//       let parent = Option.get(Id.Map.find_opt(parent_id, ast));
//       // Update the parent's children in the AST
//       let updated_parent = {
//         ...parent,
//         children: parent.children @ [id_of(new_node)],
//       };
//       let ast' = Id.Map.add(parent_id, updated_parent, ast);
//       // Add the updated parent to the AST
//       let ast'' = Id.Map.add(id_of(new_node), new_node, ast');
//       // Recurse on the definition
//       let ast''' =
//         switch (def_term) {
//         | Some(def_term) =>
//           build(ast'', level + 1, id_of(new_node), def_term)
//         | None => ast''
//         };
//       build(ast''', level, parent_id, body_term);
//     };

//     switch (curr_term) {
//     | InfoExp({term, _}) =>
//       switch (Exp.term_of(term)) {
//       | Let(pat, def, body) =>
//         mk_node(mk_name_from_pat(pat), Some(def), body)
//       // It is also useful to add type defintions to the def-structured AST
//       | TyAlias(tpat, _, body) =>
//         mk_node("[TYPE DEF] " ++ mk_name_from_tpat(tpat), None, body)
//       | Fun(_, e, _, _)
//       // As for the rest of the expression cases, we can just recurse on their child
//       // expressions, passing the current parent/level as the arguments. (This
//       // maintains the idea of an AST structured into levels based on definitions)
//       | UnOp(_, e)
//       | Test(e)
//       | Parens(e)
//       | Filter(_, e)
//       | Closure(_, e)
//       | Probe(e, _)
//       | Asc(e, _) =>
//         let e_id = Exp.rep_id(e);
//         let e_term = Option.get(Id.Map.find_opt(e_id, info_map));
//         build(ast, level, parent_id, e_term);
//       | FixF(_, e, _)
//       | Use(_, e)
//       | TypAp(e, _)
//       | DeferredAp(e, _)
//       | Seq(e, _)
//       | HintedTest(e, _) =>
//         let e_id = Exp.rep_id(e);
//         let e_term = Option.get(Id.Map.find_opt(e_id, info_map));
//         build(ast, level, parent_id, e_term);
//       | Ap(_, e1, e2)
//       | Dot(e1, e2)
//       | TupLabel(e1, e2)
//       | Cons(e1, e2)
//       | ListConcat(e1, e2)
//       | BinOp(_, e1, e2) =>
//         let e1_id = Exp.rep_id(e1);
//         let e1_term = Option.get(Id.Map.find_opt(e1_id, info_map));
//         let ast' = build(ast, level, parent_id, e1_term);
//         let e2_id = Exp.rep_id(e2);
//         let e2_term = Option.get(Id.Map.find_opt(e2_id, info_map));
//         build(ast', level, parent_id, e2_term);
//       | Tuple(es)
//       | ListLit(es) =>
//         List.fold_left(
//           (ast, e) => {
//             let e_id = Exp.rep_id(e);
//             let e_term = Option.get(Id.Map.find_opt(e_id, info_map));
//             build(ast, level, parent_id, e_term);
//           },
//           ast,
//           es,
//         )
//       | If(e1, e2, e3) =>
//         let e1_id = Exp.rep_id(e1);
//         let e1_term = Option.get(Id.Map.find_opt(e1_id, info_map));
//         let ast' = build(ast, level, parent_id, e1_term);
//         let e2_id = Exp.rep_id(e2);
//         let e2_term = Option.get(Id.Map.find_opt(e2_id, info_map));
//         let ast'' = build(ast', level, parent_id, e2_term);
//         let e3_id = Exp.rep_id(e3);
//         let e3_term = Option.get(Id.Map.find_opt(e3_id, info_map));
//         build(ast'', level, parent_id, e3_term);
//       | Match(e, branches) =>
//         let e_id = Exp.rep_id(e);
//         let e_term = Option.get(Id.Map.find_opt(e_id, info_map));
//         let ast' = build(ast, level, parent_id, e_term);
//         List.fold_left(
//           (ast, (_pat, branch_e)) => {
//             let branch_id = Exp.rep_id(branch_e);
//             let branch_term =
//               Option.get(Id.Map.find_opt(branch_id, info_map));
//             build(ast, level, parent_id, branch_term);
//           },
//           ast',
//           branches,
//         );
//       | BuiltinFun(_)
//       | Label(_)
//       | EmptyHole
//       | Undefined
//       | Invalid(_)
//       | MultiHole(_)
//       | DynamicErrorHole(_, _)
//       | Deferral(_)
//       | Atom(_)
//       | Constructor(_, _)
//       | TypFun(_, _, _)
//       | LivelitName(_)
//       | Var(_) => ast
//       }
//     | _ => ast
//     };
//   };
//   // We'll consider the entire program as the sort of "root" node of our AST,
//   // setting it as the sole node at depth=0.
//   // This node won't have a parent, but it guarantees that all actual terms
//   // have a parent. It also guarantees a sole root.
//   try({
//     let empty_info: Info.t =
//       Secondary({
//         // WARNING/todo: This Id does NOT correspond to any term in the InfoMap
//         // This is a major pitfall
//         id: Id.mk(),
//         cls: Cls.Exp(Invalid),
//         sort: Sort.Exp,
//         ctx: Ctx.empty,
//       });
//     let root_node = {
//       self: empty_info,
//       parent: Id.invalid, // no parent for the root node
//       children: [],
//       level: 0,
//       name: "root",
//     };
//     // todo: adhoc, find better way
//     let oldest_ancestor_id =
//       switch (curr_term) {
//       | Some(term) =>
//         let ancestors = Info.ancestors_of(term);
//         List.hd(List.rev(ancestors));
//       | None => raise(Failure("No current term"))
//       };
//     let oldest_ancestor_term =
//       switch (Id.Map.find_opt(oldest_ancestor_id, info_map)) {
//       | Some(term) => term
//       | None => raise(Failure("Root id does not correspond to a root term"))
//       };
//     // end todo
//     let ast =
//       build(
//         Id.Map.singleton(id_of(root_node), root_node),
//         1,
//         id_of(root_node),
//         oldest_ancestor_term,
//       );
//     // todo: two-pass approach for siblings
//     // decide whether to store or just find at runtime...
//     // until we incrementally build the AST, rather than build it
//     // lazily until needed, we should just use the dynamic approach (since
//     // it's built from scratch at runtime anyways)
//     // let ast =
//     //   Id.Map.map(
//     //     (node: node) => {
//     //       ...node,

//     //     },
//     //     ast,
//     //   );
//     print_endline("AST built successfully");

//     let root_node = Option.get(Id.Map.find_opt(id_of(root_node), ast));
//     let rec print_AST = (node: node) => {
//       print_endline(String.make(node.level, '.') ++ node.name);
//       List.iter(
//         child_id => {
//           let child = Option.get(Id.Map.find_opt(child_id, ast));
//           print_AST(child);
//         },
//         node.children,
//       );
//     };
//     print_AST(root_node);
//     print_endline(
//       "children of root node: "
//       ++ String.concat(", ", List.map(Id.to_string, root_node.children)),
//     );
//     ast;
//   }) {
//   | Failure(msg) =>
//     print_endline("Error building AST: " ++ msg);
//     Id.Map.empty;
//   | _ =>
//     print_endline("Error building AST");
//     Id.Map.empty;
//   };
// };

/* This is a place to add ad-hoc debugging print actions.
   It was originally directly in Keyboard, but that added a handler
   dependency on the model, which is technically against architecture */

let show_ctx = (editor: CodeWithStatics.Model.t): unit => {
  let zipper = editor.editor.state.zipper;
  let info_map = editor.statics.info_map;

  let curr_id = Indicated.index(zipper);
  let ctx =
    switch (curr_id) {
    | Some(id) =>
      switch (Id.Map.find_opt(id, info_map)) {
      | Some(InfoExp({ctx, _})) => ctx
      | Some(InfoTyp({ctx, _})) => ctx
      | Some(InfoPat({ctx, _})) => ctx
      | Some(InfoTPat({ctx, _})) => ctx
      | Some(Secondary(_)) => Ctx.empty
      | None => Ctx.empty
      }
    | None => Ctx.empty
    };

  List.iter(
    entry => {
      switch (entry) {
      | Ctx.VarEntry(var_entry) => print_endline(var_entry.name)
      | Ctx.ConstructorEntry(var_entry) => print_endline(var_entry.name)
      | Ctx.TVarEntry(tvar_entry) => print_endline(tvar_entry.name)
      | Ctx.LivelitEntry(livelit_entry) => print_endline(livelit_entry.name)
      }
    },
    ctx.entries,
  );
};

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
  // Finds the least upper binding node of the current term
  let rec curr_node_of = (candidate: option(Info.t)): option(node) => {
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
        | _ =>
          switch (Info.ancestors_of(candidate)) {
          | [ancestor, ..._] =>
            curr_node_of(Id.Map.find_opt(ancestor, info_map))
          | _ => None
          }
        }
      | _ =>
        switch (Info.ancestors_of(candidate)) {
        | [ancestor, ..._] =>
          curr_node_of(Id.Map.find_opt(ancestor, info_map))
        | _ => None
        }
      }
    | None => None
    };
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

  let curr_node = Option.get(curr_node_of(curr_term));
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

let print =
    (~settings: Settings.t, editor: CodeWithStatics.Model.t, key: string)
    : unit => {
  let {editor: {state: {zipper, _}, _}, statics, _}: CodeWithStatics.Model.t = editor;
  let term = statics.term;
  let map = statics.info_map;
  let print = print_endline;
  switch (key) {
  | "F1" => zipper |> Zipper.show |> print
  | "F2" => zipper |> Zipper.unselect_and_zip |> Segment.show |> print
  | "F3" => term |> Language.Exp.show |> print
  | "F4" => map |> Language.Statics.Map.show |> print
  | "F5" when settings.core.dynamics =>
    let env_init = Language.Builtins.env_init;
    statics.elaborated
    |> Language.Evaluator.evaluate(~env=env_init)
    |> fst
    |> Language.DHExp.show
    |> print;
  | "F5" => print("Dynamics disabled, cannot show evaluation.")
  | "F6" =>
    let index = Indicated.index(zipper);
    switch (index) {
    | Some(index) =>
      print("id:" ++ Id.to_string(index));
      switch (Id.Map.find_opt(index, map)) {
      | Some(ci) => print(Language.Info.show(ci))
      | None => print("DEBUG: No CI found for index")
      };
    | None => print("DEBUG: No indicated index")
    };
  | "F9" =>
    let _ = build_sub_AST(editor);
    ();
  | _ => print("DEBUG: No action for key: " ++ key)
  };
};
