open Util;
open Haz3lcore;
open Language;
open Language.Statics;

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

  let definition = (z: Zipper.t, curr_node: AssistantTreeHelper.node) => {
    let rec fold_terms = (z: Zipper.t, ids: list(Id.t)) => {
      switch (ids) {
      | [] => z
      | [id, ...rest] =>
        // Fold the *term* of the definition
        let z' = perform(Action.Select(Term(Id(id, Direction.Right))), z);
        switch (z') {
        | Ok(z') =>
          let z'' =
            perform(Action.Project(SetIndicated(Specific(Fold))), z');
          switch (z'') {
          | Ok(z'') => fold_terms(z'', rest)
          | _ => fold_terms(z', rest)
          };
        | _ => fold_terms(z, rest)
        };
      };
    };
    let remove_body = (z: Zipper.t, term: Info.t) => {
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
      let z' = perform(Action.Select(Term(Id(id, Direction.Right))), z);
      switch (z') {
      | Ok(z') =>
        let z'' = perform(Action.Paste(String("")), z');
        z'';
      | _ => Ok(z)
      };
    };
    let get_def_id_of_let = (term: Info.t): Id.t => {
      switch (term) {
      | InfoExp({term, _}) =>
        switch (Exp.term_of(term)) {
        | Let(_, def, _) => Exp.rep_id(def)
        // We won't fold/abstract away type definitions.
        | _ => Id.invalid
        }
      | _ => Id.invalid
      };
    };
    let children_def_ids =
      List.map(
        (c: AssistantTreeHelper.node) => get_def_id_of_let(c.info),
        curr_node.children,
      );
    let siblings_def_ids =
      List.map(
        (c: AssistantTreeHelper.node) => get_def_id_of_let(c.info),
        curr_node.siblings,
      );

    let z = fold_terms(z, children_def_ids);
    let z' = fold_terms(z, siblings_def_ids);

    let z'' =
      switch (curr_node.parent) {
      | Some(parent) =>
        // this switch is a temporary workaround for below mentioned bug
        switch (remove_body(z', parent.info)) {
        | Ok(z'') =>
          switch (
            perform(
              // Select the *tile* of the parent
              // TODO: throws failure when trying to select tile
              // note: this bug only started happening after passing the segment
              // directly for view
              // I suspect this is because the body expression is missing when selecting term
              Action.Select(
                Term(
                  Id(AssistantTreeHelper.id_of(parent), Direction.Right),
                ),
              ),
              z'',
            )
          ) {
          | Ok(z''') => z'''
          | _ => z''
          }
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

  let context = (local_information: AssistantTreeHelper.node): string => {
    let info = local_information.info;
    switch (info) {
    | InfoExp(info) =>
      let ctx = info.ctx;
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

  let references_in =
      (
        local_information: AssistantTreeHelper.node,
        info_map: Id.Map.t(Info.t),
      )
      : string => {
    let id = AssistantTreeHelper.id_of(local_information);
    let references = Statics.Map.refs_in(info_map, id);
    "References: ["
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
};
