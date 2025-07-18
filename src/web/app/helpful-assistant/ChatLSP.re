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
