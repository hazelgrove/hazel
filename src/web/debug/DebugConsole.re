open Language;
open Id;
open Language.Statics;
open Language.Exp;
open Util;
open Haz3lcore;

type node = {
  self: Info.t,
  parent: Id.t,
  children: list(Id.t),
  level: int,
  name: string,
};

let build_AST = (editor: CodeWithStatics.Model.t): unit => {
  let zipper = editor.editor.state.zipper;
  // The current datastructure with AST information
  let info_map = editor.statics.info_map;

  // The term the cursor is currently at
  // This actually is not needed for building the AST, and was used
  // as an ad-hoc path to get the "root" term (as opposed to what we'll later
  // call root in our AST) from the InfoMap.
  let curr_id: option(Id.t) = Indicated.index(zipper);
  let curr_term =
    switch (curr_id) {
    | Some(id) => Id.Map.find_opt(id, info_map)
    | None => raise(Failure("No current term"))
    };

  // Helper function to get the id of a node, which is
  // just its Info.id, as we reuse them
  let id_of = (node: node) => Info.id_of(node.self);

  // The recursive AST builder function.
  // We begin from a root node, visiting all children and possible subtrees from that root.
  let rec build =
          (
            ast: Id.Map.t(node),
            level: int,
            parent_id: Id.t,
            curr_term: Info.t,
          )
          : Id.Map.t(node) => {
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
        "("
        ++ mk_name_from_pat(pat1)
        ++ " : "
        ++ mk_name_from_pat(pat2)
        ++ ")"
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
    let handle_let = (name: string, def: TermBase.exp_t, body: TermBase.exp_t) => {
      let def_id = Exp.rep_id(def);
      let body_id = Exp.rep_id(body);
      let def_term = Option.get(Id.Map.find_opt(def_id, info_map));
      let body_term = Option.get(Id.Map.find_opt(body_id, info_map));
      let new_node = {
        self: curr_term,
        parent: parent_id,
        children: [],
        level,
        name,
      };
      // We can guarantee the parent exists, as only the root node is parentless, and we never
      // pass the root node as a curr_term being built from
      let parent = Option.get(Id.Map.find_opt(parent_id, ast));
      // Update the parent's children in the AST
      let updated_parent = {
        ...parent,
        children: parent.children @ [id_of(new_node)],
      };
      let ast' = Id.Map.add(parent_id, updated_parent, ast);
      let parent = Option.get(Id.Map.find_opt(parent_id, ast'));
      print_endline("Parent is now " ++ parent.name);
      print_endline(
        "Children of parent are "
        ++ String.concat(", ", List.map(Id.to_string, parent.children)),
      );
      // Add this node to the AST
      let ast'' = Id.Map.add(id_of(new_node), new_node, ast');
      // Recurse on the definition
      let ast''' = build(ast'', level + 1, id_of(new_node), def_term);
      // Recurse on the body
      build(ast''', level, parent_id, body_term);
    };
    let handle_tyalias = (name: string, body: TermBase.exp_t) => {
      let body_id = Exp.rep_id(body);
      let body_term = Option.get(Id.Map.find_opt(body_id, info_map));
      let new_node = {
        self: curr_term,
        parent: parent_id,
        children: [],
        level,
        name,
      };
      // We can guarantee the parent exists, as only the root node is parentless, and we never
      // pass the root node as a curr_term being built from
      let parent = Option.get(Id.Map.find_opt(parent_id, ast));
      // Update the parent's children in the AST
      let updated_parent = {
        ...parent,
        children: parent.children @ [id_of(new_node)],
      };
      let ast' = Id.Map.add(parent_id, updated_parent, ast);
      let parent = Option.get(Id.Map.find_opt(parent_id, ast'));
      print_endline("Parent is now " ++ parent.name);
      print_endline(
        "Children of parent are "
        ++ String.concat(", ", List.map(Id.to_string, parent.children)),
      );
      // Add this node to the AST
      let ast'' = Id.Map.add(id_of(new_node), new_node, ast');
      // Recurse on the body
      build(ast'', level, parent_id, body_term);
    };
    switch (curr_term) {
    | InfoExp({term, _}) =>
      switch (Exp.term_of(term)) {
      | Let(pat, def, body) => handle_let(mk_name_from_pat(pat), def, body)
      // It is also useful to add type defintions to the def-structured AST
      | TyAlias(tpat, _, body) =>
        handle_tyalias("[TYPE DEF] " ++ mk_name_from_tpat(tpat), body)
      // As for the rest of the expression cases, we can just recurse on their child
      // expressions, passing the current parent/level as the arguments. (This
      // maintains the idea of an AST structured into levels based on definitions)
      | UnOp(_, e)
      | Test(e)
      | Parens(e)
      | Filter(_, e)
      | Closure(_, e)
      | Probe(e, _)
      | Asc(e, _) =>
        let e_id = Exp.rep_id(e);
        let e_term = Option.get(Id.Map.find_opt(e_id, info_map));
        build(ast, level, parent_id, e_term);
      | Fun(_, e, _, _)
      | FixF(_, e, _)
      | Use(_, e)
      | TypAp(e, _)
      | DeferredAp(e, _)
      | Seq(e, _)
      | HintedTest(e, _) =>
        let e_id = Exp.rep_id(e);
        let e_term = Option.get(Id.Map.find_opt(e_id, info_map));
        build(ast, level, parent_id, e_term);
      | Ap(_, e1, e2)
      | Dot(e1, e2)
      | TupLabel(e1, e2)
      | Cons(e1, e2)
      | ListConcat(e1, e2)
      | BinOp(_, e1, e2) =>
        let e1_id = Exp.rep_id(e1);
        let e1_term = Option.get(Id.Map.find_opt(e1_id, info_map));
        let ast' = build(ast, level, parent_id, e1_term);
        let e2_id = Exp.rep_id(e2);
        let e2_term = Option.get(Id.Map.find_opt(e2_id, info_map));
        build(ast', level, parent_id, e2_term);
      | Tuple(es)
      | ListLit(es) =>
        List.fold_left(
          (ast, e) => {
            let e_id = Exp.rep_id(e);
            let e_term = Option.get(Id.Map.find_opt(e_id, info_map));
            build(ast, level, parent_id, e_term);
          },
          ast,
          es,
        )
      | If(e1, e2, e3) =>
        let e1_id = Exp.rep_id(e1);
        let e1_term = Option.get(Id.Map.find_opt(e1_id, info_map));
        let ast' = build(ast, level, parent_id, e1_term);
        let e2_id = Exp.rep_id(e2);
        let e2_term = Option.get(Id.Map.find_opt(e2_id, info_map));
        let ast'' = build(ast', level, parent_id, e2_term);
        let e3_id = Exp.rep_id(e3);
        let e3_term = Option.get(Id.Map.find_opt(e3_id, info_map));
        build(ast'', level, parent_id, e3_term);
      | Match(e, branches) =>
        let e_id = Exp.rep_id(e);
        let e_term = Option.get(Id.Map.find_opt(e_id, info_map));
        let ast' = build(ast, level, parent_id, e_term);
        List.fold_left(
          (ast, (_pat, branch_e)) => {
            let branch_id = Exp.rep_id(branch_e);
            let branch_term =
              Option.get(Id.Map.find_opt(branch_id, info_map));
            build(ast, level, parent_id, branch_term);
          },
          ast',
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
      | Var(_) => ast
      }
    | _ => ast
    };
  };
  // We'll consider the entire program as the sort of "root" node of our AST,
  // setting it as the sole node at depth=0.
  // This node won't have a parent, but it guarantees that all actual terms
  // have a parent. It also guarantees a sole root.
  try({
    let empty_info: Info.t =
      Secondary({
        id: Id.mk(),
        cls: Cls.Exp(Invalid),
        sort: Sort.Exp,
        ctx: Ctx.empty,
      });
    let root_node = {
      self: empty_info,
      parent: Id.invalid, // no parent for the root node
      children: [],
      level: 0,
      name: "root",
    };
    let oldest_ancestor_id =
      switch (curr_term) {
      | Some(term) =>
        let ancestors = Info.ancestors_of(term);
        List.hd(List.rev(ancestors));
      | None => raise(Failure("No current term"))
      };
    let oldest_ancestor_term =
      switch (Id.Map.find_opt(oldest_ancestor_id, info_map)) {
      | Some(term) => term
      | None => raise(Failure("Root id does not correspond to a root term"))
      };
    let ast =
      build(
        Id.Map.singleton(id_of(root_node), root_node),
        1,
        id_of(root_node),
        oldest_ancestor_term,
      );
    print_endline("AST built successfully");

    print_endline(
      "Num nodes in AST: " ++ string_of_int(Id.Map.cardinal(ast)),
    );
    let root_node = Option.get(Id.Map.find_opt(id_of(root_node), ast));
    let rec print_AST = (node: node) => {
      print_endline(String.make(node.level, '.') ++ node.name);
      List.iter(
        child_id => {
          let child = Option.get(Id.Map.find_opt(child_id, ast));
          print_AST(child);
        },
        node.children,
      );
    };
    print_AST(root_node);
  }) {
  | Failure(msg) => print_endline("Error building AST: " ++ msg)
  | _ => print_endline("Error building AST")
  };
};

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

let show_parent = (editor: CodeWithStatics.Model.t): unit => {
  let zipper = editor.editor.state.zipper;
  let info_map = editor.statics.info_map;
  let curr_id: option(Id.t) = Indicated.index(zipper);

  switch (curr_id) {
  | Some(id) =>
    switch (Id.Map.find_opt(id, info_map)) {
    | None => ()
    | Some(ci) =>
      let ancestors = Info.ancestors_of(ci);
      // We want to find the parent enclosing let binding
      // This is tricky, as we want to find the parent let binding where the current term
      // is in its definition, not in its body.contents

      ();
    }
  | None => ()
  };
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
  | "F8" => show_parent(editor)
  | "F9" => show_ctx(editor)
  | "F10" => build_AST(editor)
  | _ => print("DEBUG: No action for key: " ++ key)
  };
};
