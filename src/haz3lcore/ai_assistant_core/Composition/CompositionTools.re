open Util;
open OptUtil.Syntax;
open CompositionView;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(API.Json.t);

let tools = [
  // NavTools.go_to_parent, // No current node should have a parent anymore... we nav the top-level nodes
  // NavTools.go_to_child,
  NavTools.go_to_sibling, // Only navigation to and from siblings is allowed
  NavTools.go_to_binding_site,
  EditTools.initialize, // For initializing empty or nodeless programs
  EditTools.update_definition,
  EditTools.update_body,
  EditTools.update_pattern,
  EditTools.update_binding_clause,
  EditTools.delete_binding_clause,
  EditTools.delete_body,
  EditTools.insert_after,
  EditTools.insert_before,
  // ViewTools.view_entire_definition, // No longer needed is this top-level refactor... this is done by default
  // ViewTools.view_context,
];

type action = CompositionActions.composition_action;

let get_string_arg = (~arg: option(string), ~fail_with: string) => {
  switch (arg) {
  | Some(arg) => arg
  | None => raise(Failure(fail_with))
  };
};

let action_of = (~tool_name: string, ~args: Maps.StringMap.t(string)): action => {
  /* Possible arguments */
  /* Parsing here to avoid redundancy */
  /* Argument(s) may or may not be provided depending on the tool called */
  let name = Maps.StringMap.find_opt("name", args);
  let index =
    Option.map(int_of_string, Maps.StringMap.find_opt("index", args));
  let code = Maps.StringMap.find_opt("code", args);

  switch (tool_name) {
  | "select_current" => Nav(SelectCurrent)
  | "go_to_parent" => Nav(GoToParent)
  | "go_to_child" =>
    let name =
      OptUtil.get_or_fail(
        "A name must be provided for the child node to navigate to",
        name,
      );
    Nav(GoToChild(name, index));
  | "go_to_sibling" =>
    let name =
      OptUtil.get_or_fail(
        "A name must be provided for the sibling node to navigate to",
        name,
      );
    Nav(GoToSibling(NameAndIdx(name, index)));
  | "go_to_binding_site" =>
    let name =
      OptUtil.get_or_fail(
        "A name must be provided for the variable to navigate to",
        name,
      );
    Nav(GoToBindingSite(name, index));
  | "initialize" =>
    let code =
      OptUtil.get_or_fail(
        "You must specify a code for the program you wish to update",
        code,
      );
    Edit(Initialize(LLM(code)));
  | "update_definition" =>
    let code =
      OptUtil.get_or_fail(
        "You must specify a code for the definition you wish to update",
        code,
      );
    Edit(UpdateDefinition(LLM(code)));
  | "update_body" =>
    let code =
      OptUtil.get_or_fail(
        "You must specify a code for the body you wish to update",
        code,
      );
    Edit(UpdateBody(LLM(code)));
  | "update_pattern" =>
    let code =
      OptUtil.get_or_fail(
        "You must specify a code for the pattern you wish to update",
        code,
      );
    Edit(UpdatePattern(LLM(code)));
  | "update_binding_clause" =>
    let code =
      OptUtil.get_or_fail(
        "You must specify a code for the expression you wish to update",
        code,
      );
    Edit(UpdateBindingClause(LLM(code)));
  | "insert_after" =>
    let code =
      OptUtil.get_or_fail(
        "You must specify a code for the expression you wish to insert after",
        code,
      );
    Edit(InsertAfter(LLM(code)));
  | "insert_before" =>
    let code =
      OptUtil.get_or_fail(
        "You must specify a code for the expression you wish to insert before",
        code,
      );
    Edit(InsertBefore(LLM(code)));
  | "delete_binding_clause" => Edit(DeleteBindingClause)
  | "delete_body" => Edit(DeleteBody)
  | "view_entire_definition" => Read(ViewEntireDefintion)
  | "show_use_sites" => Read(ShowUseSites)
  | _ => raise(Failure("The tool called does not exist."))
  };
};

let code_of = (user: CompositionActions.user) => {
  switch (user) {
  | LLM(code) => code
  | Human => JsUtil.prompt("Enter code argument:", "Code") |> Option.get
  };
};

let string_of = (action: action) => {
  switch (action) {
  | Nav(SelectCurrent) => "select_current"
  | Nav(GoToParent) => "go_to_parent"
  | Nav(GoToChild(name, index)) =>
    "go_to_child(\""
    ++ name
    ++ "\""
    ++ (
      switch (index) {
      | Some(index) => ", " ++ string_of_int(index)
      | None => ""
      }
    )
    ++ ")"
  | Nav(GoToSibling(NameAndIdx(name, index))) =>
    "go_to_sibling(\""
    ++ name
    ++ "\""
    ++ (
      switch (index) {
      | Some(index) => ", " ++ string_of_int(index)
      | None => ""
      }
    )
    ++ ")"
  | Nav(GoToSibling(Stepwise(d))) =>
    "go_to_sibling("
    ++ (
      switch (d) {
      | Left => "Prev"
      | Right => "Next"
      }
    )
    ++ ")"
  | Nav(GoToBindingSite(name, index)) =>
    "go_to_binding_site(\""
    ++ name
    ++ "\""
    ++ (
      switch (index) {
      | Some(index) => ", " ++ string_of_int(index)
      | None => ""
      }
    )
    ++ ")"
  | Nav(GoToUseSite(name, index)) =>
    "go_to_use_site(\""
    ++ name
    ++ "\""
    ++ (
      switch (index) {
      | Some(index) => ", " ++ string_of_int(index)
      | None => ""
      }
    )
    ++ ")"
  | Edit(Initialize(u)) => "initialize(\"" ++ code_of(u) ++ "\")"
  | Edit(UpdateDefinition(u)) =>
    "update_definition(\"" ++ code_of(u) ++ "\")"
  | Edit(UpdateBody(u)) => "update_body(\"" ++ code_of(u) ++ "\")"
  | Edit(UpdatePattern(u)) => "update_pattern(\"" ++ code_of(u) ++ "\")"
  | Edit(UpdateBindingClause(u)) =>
    "update_binding_clause(\"" ++ code_of(u) ++ "\")"
  | Edit(DeleteBindingClause) => "delete_binding_clause"
  | Edit(DeleteBody) => "delete_body"
  | Edit(InsertAfter(u)) => "insert_after(\"" ++ code_of(u) ++ "\")"
  | Edit(InsertBefore(u)) => "insert_before(\"" ++ code_of(u) ++ "\")"
  | Read(ViewEntireDefintion) => "view_entire_definition"
  | Read(ShowUseSites) => "show_use_sites"
  };
};

module Perform = {
  open Util;
  open Language;
  open AssistantTreeHelper.HighLevelNode;

  type inner_term =
    | Pat
    | Def
    | Body;

  let get_inner_term_id = (node_info: t, inner_term: inner_term): Id.t => {
    /*
     Returns the specified "inner_term" from the "curr_node_info"

     E.g. If current node is "x" in a program "let x : Int = 2 + 3 in 100 + 200",
     calling get_inner_term_id(curr_node_info, Pat) will return the id of the pattern "x : Int",
     calling get_inner_term_id(curr_node_info, Def) will return the id of the definition "2 + 3",
     calling get_inner_term_id(curr_node_info, Body) will return the id of the body "100 + 200".
     */
    switch (current_of(node_info).info) {
    | InfoExp({term, _}) =>
      switch (Exp.term_of(term)) {
      | Let(pat, def, body) =>
        switch (inner_term) {
        | Pat => Pat.rep_id(pat)
        | Def => Exp.rep_id(def)
        | Body => Exp.rep_id(body)
        }
      | TyAlias(tpat, tdef, body) =>
        switch (inner_term) {
        | Pat => TPat.rep_id(tpat)
        | Def => Typ.rep_id(tdef)
        | Body => Exp.rep_id(body)
        }
      | _ =>
        raise(
          Failure(
            "UNIMPLEMENTED_NODE_TYPE: Only let and type alias expressions are currently supported as nodes",
          ),
        )
      }
    | _ =>
      raise(
        Failure(
          "Current node is not a let or type alias expression, so no pattern to update",
        ),
      )
    };
  };

  let static_error_check =
      (
        ~old_z: option(Zipper.t), // optional arg -- if not provided, assume no errors
        ~new_z: Zipper.t,
        ~mk_statics: Zipper.t => StaticsBase.Map.t,
        ~of_pat: bool,
        ~of_def: bool,
        ~of_body: bool,
      ) => {
    /*
     A localized static error check to ensure that newly inserted segments do not introduce any errors.

     This is a localized check, as obligations occuring elsewhere in the program are inevitable for
     many types of edits.

     of_pat, of_def, and of_body are used to specify which parts of the program to check for errors.
     */
    let old_errors =
      switch (old_z) {
      | None => []
      | Some(old_z) =>
        let old_info_map = mk_statics(old_z);
        let old_node_info = build(old_z, old_info_map);
        let old_node = old_node_info |> unwrap |> current_of;
        let old_subtree =
          GeneralTreeUtils.subtree_of(
            ~info=old_node.info,
            ~orig_info_map=old_info_map,
            ~of_pat,
            ~of_def,
            ~of_body,
          );
        ErrorPrint.all(old_subtree);
      };
    let new_info_map = mk_statics(new_z);
    let new_node_info = build(new_z, new_info_map);
    let new_node = new_node_info |> unwrap |> current_of;
    let new_subtree =
      GeneralTreeUtils.subtree_of(
        ~info=new_node.info,
        ~orig_info_map=new_info_map,
        ~of_pat,
        ~of_def,
        ~of_body,
      );
    let new_errors = ErrorPrint.all(new_subtree);
    if (List.length(new_errors) > List.length(old_errors)) {
      Error(
        Action.Failure.Composition_action_failure(
          "Not applying the action you requested as it would have the following static error(s): "
          ++ String.concat(", ", new_errors),
        ),
      );
    } else {
      Ok(new_z);
    };
  };

  let statics_map_new_ids =
      (old_statics: StaticsBase.Map.t, new_statics: StaticsBase.Map.t) => {
    // Returns only the IDs of the new statics map that are not in the old statics map
    // This is useful to identify which new static information was added
    Id.Map.fold(
      (id, _info, acc) =>
        // Check if the ID exists in the old statics map
        switch (StaticsBase.Map.lookup(id, old_statics)) {
        | Some(_) => acc // ID exists in old map, don't include it
        | None => [id, ...acc] // ID doesn't exist in old map, include it
        },
      new_statics,
      [],
    );
  };

  let introduce =
      (
        z: Zipper.t,
        code: string,
        return:
          (Action.Failure.t, option(Zipper.t)) =>
          result(Zipper.t, Action.Failure.t),
      ) => {
    // A wrapper function for trying to paste code into the zipper
    // Note that we paste a segment; so, we convert the string to a segment
    // first, and then insert the segment into the zipper. This helps to
    // avoid potential current buggy parsing issues.
    Parser.to_segment(code)
    |> OptUtil.and_then((segment: Segment.t) =>
         Some(Zipper.insert_segment(z, segment))
       )
    |> return(CantPaste);
  };

  let destruct =
      (
        ~defs_exclude_bodies: bool,
        z: Zipper.t,
        target_id: Id.t,
        syntax: CachedSyntax.t,
      ) => {
    switch (
      Select.term(
        ~defs_exclude_bodies,
        ~case_rules=false,
        syntax.term_data,
        target_id,
        z,
      )
    ) {
    | Some(z') =>
      switch (Destruct.go(Left, z')) {
      | None => Error(Action.Failure.Cant_destruct)
      | Some(z'') => Ok(z'')
      }
    | None => Error(Action.Failure.Cant_select)
    };
  };

  let overwrite_term =
      (
        z: Zipper.t,
        target_id: Id.t,
        code: string,
        defs_exclude_bodies: bool,
        syntax: CachedSyntax.t,
        return:
          (Action.Failure.t, option(Zipper.t)) =>
          result(Zipper.t, Action.Failure.t),
      ) => {
    // Select the respective term (in this case the definition term)
    switch (
      Select.term(
        ~defs_exclude_bodies,
        ~case_rules=false,
        syntax.term_data, // todo: not sure about this arg
        target_id,
        z,
      )
    ) {
    | Some(z') =>
      // Paste the code over the selected tile
      introduce(z', code, return)
    | None => Error(Action.Failure.Cant_select)
    };
  };
  let insert_term =
      (
        z: Zipper.t,
        target_id: Id.t,
        code: string,
        d: Direction.t,
        syntax: CachedSyntax.t,
        return:
          (Action.Failure.t, option(Zipper.t)) =>
          result(Zipper.t, Action.Failure.t),
      ) => {
    switch (
      // ' let a = 0 in'
      Select.term(
        ~defs_exclude_bodies=true,
        ~case_rules=false,
        syntax.term_data, // todo: not sure about this arg, is it right?
        target_id,
        z,
      )
    ) {
    | Some(z') =>
      switch (Move.by_token(d, z')) {
      | Some(z'') => introduce(z'', code, return)
      | None => Error(Action.Failure.Cant_move)
      }
    | None => Error(Action.Failure.Cant_select)
    };
  };

  // Tempory wrapper that helps me localize myself while implementing (remove)
  let composition_dispatch =
      (
        a: CompositionActions.composition_action,
        syntax: CachedSyntax.t,
        z: Zipper.t,
        mk_statics: Zipper.t => StaticsBase.Map.t,
        return:
          (Action.Failure.t, option(Zipper.t)) =>
          result(Zipper.t, Action.Failure.t),
        node_info: option(t),
      ) => {
    switch (node_info) {
    | None =>
      switch (a) {
      | Edit(Initialize(u)) =>
        switch (introduce(Select.all(z), code_of(u), return)) {
        | Ok(new_z) =>
          let new_statics = mk_statics(new_z);
          // For initialization, check the entire program for errors
          let new_errors = ErrorPrint.all(new_statics);
          if (List.length(new_errors) > 0) {
            Error(
              Action.Failure.Composition_action_failure(
                "Not applying the action you requested as it would have the following static error(s): "
                ++ String.concat(", ", new_errors),
              ),
            );
          } else {
            Ok(new_z);
          };
        | Error(e) => Error(e)
        }
      | _ => Error(Action.Failure.Cant_derive_local_AST_information)
      }
    | Some(node_info) =>
      let node = node_info |> current_of;
      switch (a) {
      | Nav(n) =>
        switch (n) {
        | SelectCurrent =>
          switch (
            Select.term(
              ~defs_exclude_bodies=true,
              ~case_rules=false,
              syntax.term_data,
              Info.id_of(node.info),
              z,
            )
          ) {
          | Some(z) => Ok(z)
          | None => Error(Action.Failure.Cant_select)
          }
        | GoToParent =>
          switch (parent_of(node_info.node_map, node)) {
          | None => Error(Action.Failure.Cant_move)
          | Some(parent) =>
            switch (Select.tile(Info.id_of(parent.info), z)) {
            | Some(z) => Ok(z)
            | None => Error(Action.Failure.Cant_select)
            }
          }
        | GoToChild(who, which) =>
          switch (which) {
          | None =>
            // the llm provided no index, thus, use the name
            let cands =
              List.filter(
                (child: node) => child.name == who,
                children_of(node_info.node_map, node),
              );
            if (List.length(cands) > 1) {
              // No child with this name
              Error(Action.Failure.Cant_move);
            } else {
              switch (ListUtil.hd_opt(cands)) {
              // More than one child node with the same name
              // needs to specify how to resolve the ambiguity
              | None => Error(Action.Failure.Cant_move)
              | Some(child) =>
                Select.tile(id_of(child), z)
                |> return(Action.Failure.Cant_select)
              };
            };
          | Some(nth) =>
            // this means the llm provided an index to move to, in which case
            // we default on using that as opposed to the name
            switch (List.nth_opt(children_of(node_info.node_map, node), nth)) {
            // Index does not exist
            | None => Error(Action.Failure.Cant_move)
            | Some(child) =>
              Select.tile(id_of(child), z)
              |> return(Action.Failure.Cant_select)
            }
          }
        | GoToSibling(via) =>
          switch (via) {
          | NameAndIdx(who, which) =>
            switch (which) {
            | None =>
              // the llm provided no index, thus, use the name
              let cands =
                List.filter(
                  (sibling: node) => sibling.name == who,
                  siblings_of(node_info.node_map, node),
                );
              if (List.length(cands) > 1) {
                Error(Action.Failure.Cant_move);
              } else {
                switch (ListUtil.hd_opt(cands)) {
                | None => Error(Action.Failure.Cant_move)
                | Some(sibling) =>
                  Select.tile(id_of(sibling), z)
                  |> return(Action.Failure.Cant_select)
                };
              };
            | Some(nth) =>
              // this means the llm provided an index to move to, in which case
              // we default on using that as opposed to the name
              switch (
                List.nth_opt(siblings_of(node_info.node_map, node), nth)
              ) {
              | None => Error(Action.Failure.Cant_move)
              | Some(sibling) =>
                Select.tile(id_of(sibling), z)
                |> return(Action.Failure.Cant_select)
              }
            }
          | Stepwise(d) =>
            let len = List.length(siblings_of(node_info.node_map, node));
            let target_id =
              switch (d) {
              | Left =>
                List.nth(
                  siblings_of(node_info.node_map, node),
                  (node.sibling_idx - 1 + len) mod len,
                )
                |> id_of
              | Right =>
                // Don't add 1 here because we filtered out the current node
                List.nth(
                  siblings_of(node_info.node_map, node),
                  (node.sibling_idx + 1 + len) mod len,
                )
                |> id_of
              };
            Select.tile(target_id, z) |> return(Action.Failure.Cant_select);
          }
        | GoToBindingSite(who, which) =>
          // Returns a list of binding sites (id, name)
          let cands = refs_in(node_info, mk_statics(z));
          // We want to do the following:
          // 1. Find the target variable based on the args provided
          // 2. Navigate to the binding site of this target variable
          let target =
            switch (which) {
            | None =>
              // No index provided, so use the name
              List.find_opt(
                (binding: Binding.t) => binding.name == who,
                cands,
              )
            | Some(nth) =>
              // Index provided, so use the index
              List.nth_opt(cands, nth)
            };
          switch (
            {
              let* target = target;
              Move.jump_to_id_indicated(z, target.id);
            }
          ) {
          | Some(z'') => Ok(z'')
          | None => Error(Action.Failure.Cant_move)
          };
        | GoToUseSite(_who, _which) => Ok(z)
        }
      | Read(r) =>
        switch (r) {
        | ShowUseSites => Ok(z)
        | ViewEntireDefintion => Ok(z)
        }
      | Edit(e) =>
        // let select_curr_node = z => {
        //   // Recalculate info map with new statics and measurements to assert fresh, non-stale state
        //   let curr_node_info =
        //     AssistantTreeHelper.build_curr_node_info(z, mk_statics(z));
        //   switch (curr_node_info) {
        //   | Some(node) =>
        //     switch (Select.tile(Info.id_of(node.info), z)) {
        //     | Some(z) => Ok(z)
        //     | None => Error(Action.Failure.Cant_select)
        //     }
        //   | None => Error(Action.Failure.Cant_derive_local_AST_information)
        //   };
        // };
        switch (e) {
        | UpdateDefinition(u) =>
          let target_id = get_inner_term_id(node_info, Def);
          switch (
            overwrite_term(z, target_id, code_of(u), false, syntax, return)
          ) {
          | Error(e) => Error(e)
          | Ok(new_z) =>
            static_error_check(
              ~old_z=Some(z),
              ~new_z,
              ~mk_statics,
              ~of_pat=true, // set pat to true here because could be case def is made recursive
              // or something else... errors in def can be dependent on pat (in Hazel)
              ~of_def=true,
              ~of_body=false,
            )
          };
        | UpdateBody(u) =>
          let target_id = get_inner_term_id(node_info, Body);
          switch (
            overwrite_term(z, target_id, code_of(u), false, syntax, return)
          ) {
          | Error(e) => Error(e)
          | Ok(new_z) =>
            static_error_check(
              ~old_z=Some(z),
              ~new_z,
              ~mk_statics,
              ~of_pat=true, // two reasons this is set to true
              // 1. updating the body of something should not break the pattern
              // 2. the body is highly dependent on the pattern (possibly dep on def as well?)
              ~of_def=true,
              ~of_body=true,
            )
          };
        | UpdatePattern(u) =>
          let target_id = get_inner_term_id(node_info, Pat);
          let old_pat =
            StaticsBase.Map.lookup(target_id, mk_statics(z)) |> Option.get;
          switch (
            overwrite_term(z, target_id, code_of(u), false, syntax, return)
          ) {
          | Error(e) => Error(e)
          | Ok(new_z) =>
            switch (
              static_error_check(
                ~old_z=Some(z),
                ~new_z,
                ~mk_statics,
                ~of_pat=true,
                ~of_def=false,
                ~of_body=false,
              )
            ) {
            | Error(e) => Error(e)
            | Ok(safe_z) =>
              let new_info_map = mk_statics(safe_z);
              let new_node_info = build(safe_z, new_info_map);
              let target_id = get_inner_term_id(new_node_info |> unwrap, Pat);
              let new_pat =
                StaticsBase.Map.lookup(target_id, new_info_map) |> Option.get;
              let updated_z =
                GeneralTreeUtils.update_use_sites_of_pat(
                  ~z=safe_z,
                  ~co_ctx=
                    GeneralTreeUtils.get_refs_to(node.info, new_info_map),
                  ~old_names=GeneralTreeUtils.get_var_names_from_pat(old_pat),
                  ~new_names=GeneralTreeUtils.get_var_names_from_pat(new_pat),
                );
              // Jump back to the original node
              Move.jump_to_id_indicated(updated_z, target_id)
              |> return(Action.Failure.Cant_move);
            }
          };
        | UpdateBindingClause(u) =>
          let target_id = Info.id_of(node.info);
          switch (
            overwrite_term(z, target_id, code_of(u), true, syntax, return)
          ) {
          | Error(e) => Error(e)
          | Ok(new_z) =>
            static_error_check(
              ~old_z=Some(z),
              ~new_z,
              ~mk_statics,
              ~of_pat=true,
              ~of_def=true,
              ~of_body=false,
            )
          };
        | InsertBefore(u) =>
          // todo: figure out a better method than magic space
          switch (
            insert_term(
              z,
              Info.id_of(node.info),
              code_of(u) ++ " ",
              Direction.Left,
              syntax,
              return,
            )
          ) {
          | Error(e) => Error(e)
          | Ok(new_z) =>
            // This single character move is to place the cursor off the following
            // term in the case it is one it, critically, this helps to assert that
            // the trailing node in the new insertion is selected
            switch (Move.by_char_left(new_z)) {
            | Some(new_z) =>
              static_error_check(
                ~old_z=None,
                ~new_z,
                ~mk_statics,
                ~of_pat=true,
                ~of_def=true,
                ~of_body=false // optionally, we could make this true
              )
            | None => Error(Action.Failure.Cant_move)
            }
          }
        | InsertAfter(u) =>
          // todo: figure out a better method than magic space
          switch (
            insert_term(
              z,
              Info.id_of(node.info),
              " " ++ code_of(u),
              Direction.Right,
              syntax,
              return,
            )
          ) {
          | Error(e) => Error(e)
          | Ok(new_z) =>
            // Same logic as insert_before, (see above)
            switch (Move.by_char_left(new_z)) {
            | Some(new_z) =>
              static_error_check(
                ~old_z=None,
                ~new_z,
                ~mk_statics,
                ~of_pat=true,
                ~of_def=true,
                ~of_body=false // optionally, we could make this true
              )
            | None => Error(Action.Failure.Cant_move)
            }
          }
        | DeleteBindingClause =>
          destruct(
            ~defs_exclude_bodies=true,
            z,
            Info.id_of(node.info),
            syntax,
          )
        | DeleteBody =>
          let target_id = get_inner_term_id(node_info, Body);
          destruct(~defs_exclude_bodies=false, z, target_id, syntax);
        | Initialize(_) =>
          Error(
            Action.Failure.Composition_action_failure(
              "Once a program has let/type alias expressions, you can never use initialize on it ever again.",
            ),
          )
        }
      };
    };
  };

  let reselect_current_node =
      (res: result(Zipper.t, Action.Failure.t), mk_statics) => {
    // Reselects the current node
    // This is expected to be used after an action is performed on the editor
    // Useful for testing and nice for UI/UX looks
    // Technically we select the current node twice now, before and after...
    // through the midst of it all, I've learned it is better to be safe than to be sorry.
    switch (res) {
    | Ok(z) =>
      switch (build(z, mk_statics(z))) {
      | Some(node_info) =>
        switch (Select.tile(id_of(current_of(node_info)), z)) {
        | Some(z) => Ok(z)
        | None => res
        }
      | None => res
      }
    | Error(e) => Error(e)
    };
  };

  let go = (~syntax, ~z, ~a, ~mk_statics, ~return, ~schedule_tool_response) => {
    let node_info = build(z, mk_statics(z));

    let res =
      composition_dispatch(a, syntax, z, mk_statics, return, node_info);

    // // Select the current node.
    let res = reselect_current_node(res, mk_statics);

    //todo: handle res and use schedule_assistant_action to send the result to the assistant and loop
    switch (schedule_tool_response) {
    | Some(schedule_tool_response) =>
      switch (res) {
      | Ok(_) =>
        schedule_tool_response(
          AssistantUpdateAction.Success(
            "Action has been applied to the editor",
          ),
        )
      | Error(Composition_action_failure(e)) =>
        schedule_tool_response(AssistantUpdateAction.Failure(e))
      | _ =>
        schedule_tool_response(
          AssistantUpdateAction.Failure(
            "An error occured when applying your changes to the editor",
          ),
        )
      }
    | None =>
      // Composition action was not sourced from an ai assistant tool call,
      // just return the action performed on the editor (no feedback to send)
      ()
    };
    res;
  };
};
