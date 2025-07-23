open Util;
open Haz3lcore;
open Language;
open Language.Statics;

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
    let handle_let_exp =
        (name: string, def: TermBase.exp_t, body: TermBase.exp_t) => {
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
    let handle_body_only_exp = (name: string, body: TermBase.exp_t) => {
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
      | Let(pat, def, body) =>
        handle_let_exp(mk_name_from_pat(pat), def, body)
      // It is also useful to add type defintions to the def-structured AST
      | TyAlias(tpat, _, body) =>
        handle_body_only_exp("[TYPE DEF] " ++ mk_name_from_tpat(tpat), body)
      | Fun(pat, body, _, _) =>
        handle_body_only_exp(mk_name_from_pat(pat), body)
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

let uniquify =
    (editor: CodeWithStatics.Model.t): (Segment.t, VarMap.t_(Id.t)) => {
  // Extract the segment from the editor
  let sketch =
    Zipper.smart_seg(
      ~dump_backpack=true,
      ~erase_buffer=true,
      editor.editor.state.zipper,
    );

  // Helper function to check if a token is a variable name
  let is_var_token = (token: Token.t): bool => {
    Form.is_var(token);
  };

  // Helper function to uniquify a variable name
  let uniquify_var_name = (name: string, suffix: int): string => {
    name ++ "^" ++ string_of_int(suffix);
  };

  // Collect all variable names and their occurrences from the static info map
  // Separate bindings from references
  let collect_vars_from_statics =
      (): (list((string, Id.t)), list((string, Id.t))) => {
    let statics = CodeWithStatics.Model.get_statics(editor);
    Id.Map.fold(
      (id, info, (bindings, references)) => {
        switch (info) {
        | Info.InfoExp({term, _}) =>
          switch (term.term) {
          | Var(name) =>
            // This is a variable reference - add it to references
            (bindings, [(name, id), ...references])
          | _ => (bindings, references)
          }
        | InfoPat({term, _}) =>
          switch (term.term) {
          | Var(name) =>
            // This is a variable binding - add it to bindings
            ([(name, id), ...bindings], references)
          | _ => (bindings, references)
          }
        | InfoTyp({term, _}) =>
          switch (term.term) {
          | Var(name) =>
            // This is a type variable reference - add it to references
            (bindings, [(name, id), ...references])
          | _ => (bindings, references)
          }
        | InfoTPat({term, _}) =>
          switch (term.term) {
          | Var(name) =>
            // This is a type variable binding - add it to bindings
            ([(name, id), ...bindings], references)
          | _ => (bindings, references)
          }
        | _ => (bindings, references)
        }
      },
      statics.info_map,
      ([], []),
    );
  };

  // Group variables by name and assign unique suffixes
  let (bindings, references) = collect_vars_from_statics();
  let _ = bindings @ references;

  // Group bindings by name and assign unique suffixes
  let binding_groups =
    List.fold_left(
      (acc, (name, id)) => {
        let existing = List.assoc_opt(name, acc);
        switch (existing) {
        | Some(ids) =>
          // Remove the old entry and add the updated one
          let filtered_acc = List.filter(((n, _)) => n != name, acc);
          [(name, [id, ...ids]), ...filtered_acc];
        | None => [(name, [id]), ...acc]
        };
      },
      [],
      bindings,
    );

  // Create mapping from binding ID to uniquified name
  let binding_id_to_uniquified =
    List.fold_left(
      (acc, (name, ids)) => {
        List.fold_left(
          (acc, (index, id)) => {
            let uniquified_name = uniquify_var_name(name, index);
            [(id, uniquified_name), ...acc];
          },
          acc,
          List.mapi((index, id) => (index, id), ids),
        )
      },
      [],
      binding_groups,
    );

  // Get the TermMap to map tile IDs to terms
  let terms = editor.editor.syntax.terms;

  // Create universal context mapping from uniquified names to original IDs
  let universal_ctx =
    List.fold_left(
      (acc, (name, ids)) => {
        List.fold_left(
          (acc, (index, id)) => {
            let uniquified_name = uniquify_var_name(name, index);
            [(uniquified_name, id), ...acc];
          },
          acc,
          List.mapi((index, id) => (index, id), ids),
        )
      },
      [],
      binding_groups,
    );

  // Helper function to replace variable names in a segment
  let rec replace_vars = (seg: Segment.t): Segment.t => {
    List.map(replace_vars_piece, seg);
  }
  and replace_vars_piece = (piece: Piece.t): Piece.t => {
    switch (piece) {
    | Tile(tile) =>
      // Check if this tile represents a variable
      let is_var_tile =
        List.length(tile.label) == 1 && is_var_token(List.hd(tile.label));

      let new_label =
        if (is_var_tile) {
          let var_name = List.hd(tile.label);

          // Look up the term ID for this tile using the TermMap
          let term_id_opt = Id.Map.find_opt(tile.id, terms);

          // Look up the uniquified name using the term ID
          let uniquified_name =
            switch (term_id_opt) {
            | Some(term) =>
              let term_id = Any.rep_id(term);
              // First check if this is a binding
              let binding_result =
                List.assoc_opt(term_id, binding_id_to_uniquified);
              switch (binding_result) {
              | Some(name) => Some(name)
              | None =>
                // This is a reference - find which binding it refers to
                let statics = CodeWithStatics.Model.get_statics(editor);
                switch (Id.Map.find_opt(term_id, statics.info_map)) {
                | Some(Info.InfoExp({ctx, _})) =>
                  switch (Ctx.lookup_var(ctx, var_name)) {
                  | Some(entry) =>
                    let ref_result =
                      List.assoc_opt(entry.id, binding_id_to_uniquified);
                    ref_result;
                  | None => Some(var_name)
                  }
                | Some(Info.InfoTyp({ctx, _})) =>
                  switch (Ctx.lookup_tvar_id(ctx, var_name)) {
                  | Some(id) =>
                    let ref_result =
                      List.assoc_opt(id, binding_id_to_uniquified);
                    ref_result;
                  | None => Some(var_name)
                  }
                | _ => Some(var_name)
                };
              };
            | None => None
            };

          switch (uniquified_name) {
          | Some(name) => [name]
          | None => [var_name]
          };
        } else {
          tile.label;
        };

      let new_children = List.map(replace_vars, tile.children);

      Tile({
        ...tile,
        label: new_label,
        children: new_children,
      });
    | Grout(grout) => Grout(grout)
    | Secondary(secondary) => Secondary(secondary)
    | Projector(projector) =>
      let new_syntax = replace_vars_piece(projector.syntax);
      Projector({
        ...projector,
        syntax: new_syntax,
      });
    };
  };

  let uniquified_sketch = replace_vars(sketch);

  (uniquified_sketch, universal_ctx);
};

let get_sketch_and_error_ctx =
    (editor: CodeWithStatics.Model.t): list(string) => {
  let _ =
    // todo: dead code. remove if keeping uniquify. reinstate if removing uniquify.
    Zipper.smart_seg(
      ~dump_backpack=true,
      ~erase_buffer=true,
      editor.editor.state.zipper,
    );
  let (sketch_seg, _) = uniquify(editor);
  let errors = ErrorPrint.all(editor.statics.info_map);
  let static_error_arr =
    switch (errors) {
    | [] => ["No static errors found"]
    | _ => errors
    };
  let ctx =
    [
      "PROGRAM SKETCH: ```"
      ++ ErrorPrint.Print.seg(~holes="?", sketch_seg)
      ++ "```",
    ]
    @ ["STATIC ERRORS: "]
    @ static_error_arr;
  ctx;
};

module Options = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    params: OpenRouter.params,
    instructions: bool,
    syntax_notes: bool,
    num_examples: int,
    expected_type: bool,
    relevant_ctx: bool,
    error_rounds_max: int,
  };

  let init: t = {
    params: OpenRouter.default_params,
    instructions: true,
    syntax_notes: true,
    num_examples: 9,
    expected_type: true,
    relevant_ctx: true,
    error_rounds_max: 2,
  };
};

module SystemPrompt = {
  let prelude = ["You are a helpful coding assistant in Hazel. \n"];

  let normal_completion_prompt = (completion_token: string) =>
    CompletionPrompt_normal.self(completion_token);

  let cot_completion_prompt = (completion_token: string) =>
    CompletionPrompt_cot.self(completion_token);

  let hazel_syntax_notes = HazelSyntaxNotes.self;

  let composition_prompt = CompositionPrompt.self;

  let mk_suggestion_prompt =
      (
        {instructions, syntax_notes, _}: Options.t,
        completion_token: string,
        advanced_reasoning: bool,
      )
      : string =>
    String.concat(
      "\n",
      (
        instructions
          ? prelude
            @ (
              advanced_reasoning
                ? cot_completion_prompt(completion_token)
                : normal_completion_prompt(completion_token)
            )
          : []
      )
      @ (syntax_notes ? hazel_syntax_notes : []),
    );
};
