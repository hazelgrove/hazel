open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(API.Json.t);

let tools = [
  NavTools.go_to_parent,
  NavTools.go_to_child,
  NavTools.go_to_sibling,
  EditTools.update_definition,
  EditTools.update_body,
  EditTools.update_pattern,
  EditTools.update_binding_clause,
  EditTools.delete_binding_clause,
  EditTools.delete_body,
  EditTools.insert_after,
  EditTools.insert_before,
  //ViewTools.view_definition,
];

type action = Action.composition_action;

let action_of = (~tool_name: string, ~args: Maps.StringMap.t(string)): action => {
  /* Possible arguments */
  /* Parsing here to avoid redundancy */
  /* Argument(s) may or may not be provided depending on the tool called */
  let name = Maps.StringMap.find_opt("name", args);
  let index =
    Option.map(int_of_string, Maps.StringMap.find_opt("index", args));
  let code = Maps.StringMap.find_opt("code", args);

  switch (tool_name) {
  | "go_to_parent" => Nav(GoToParent)
  | "go_to_child" =>
    let name =
      switch (name) {
      | Some(name) => name
      | None =>
        raise(
          Failure(
            "You must specify a name for the child you wish to navigate to",
          ),
        )
      };
    Nav(GoToChild(name, index));
  | "go_to_sibling" =>
    let name =
      switch (name) {
      | Some(name) => name
      | None =>
        raise(
          Failure(
            "You must specify a name for the sibling you wish to navigate to",
          ),
        )
      };
    Nav(GoToSibling(NameAndIdx(name, index)));
  | "view_definition" => Read(ViewDefinition)
  | "update_definition" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure(
            "You must specify a code for the definition you wish to update",
          ),
        )
      };
    Edit(UpdateDefinition(code));
  | "update_body" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure("You must specify a code for the body you wish to update"),
        )
      };
    Edit(UpdateBody(code));
  | "update_pattern" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure(
            "You must specify a code for the pattern you wish to update",
          ),
        )
      };
    Edit(UpdatePattern(code));
  | "update_binding_clause" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure(
            "You must specify a code for the expression you wish to update",
          ),
        )
      };
    Edit(UpdateBindingClause(code));
  | "insert_after" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure(
            "You must specify a code for the expression you wish to insert after",
          ),
        )
      };
    Edit(InsertAfter(code));
  | "insert_before" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure(
            "You must specify a code for the expression you wish to insert before",
          ),
        )
      };
    Edit(InsertBefore(code));
  | "delete_binding_clause" => Edit(DeleteBindingClause)
  | "delete_body" => Edit(DeleteBody)
  | _ => Nav(GoToParent) // default fallback
  };
};

let string_of = (action: action) => {
  switch (action) {
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
  | Read(ViewDefinition) => "view_definition"
  | Edit(UpdateDefinition(code)) => "update_definition(\"" ++ code ++ "\")"
  | Edit(UpdateBody(code)) => "update_body(\"" ++ code ++ "\")"
  | Edit(UpdatePattern(code)) => "update_pattern(\"" ++ code ++ "\")"
  | Edit(UpdateBindingClause(code)) =>
    "update_binding_clause(\"" ++ code ++ "\")"
  | Edit(DeleteBindingClause) => "delete_binding_clause"
  | Edit(DeleteBody) => "delete_body"
  | Edit(InsertAfter(code)) => "insert_after(\"" ++ code ++ "\")"
  | Edit(InsertBefore(code)) => "insert_before(\"" ++ code ++ "\")"
  };
};

module Perform = {
  open Util;
  open Language;

  type inner_term =
    | Pat
    | Def
    | Body;

  let get_inner_term_id =
      (curr_node_info: AssistantTreeHelper.node, inner_term: inner_term): Id.t => {
    switch (curr_node_info.info) {
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
        raise(Failure("Current node is not a let or type alias expression"))
      }
    | _ =>
      raise(
        Failure(
          "Current node is not a let or type alias expression, so no pattern to update",
        ),
      )
    };
  };
  // Tempory wrapper that helps me localize myself while implementing (remove)
  let go =
      (
        a: Action.composition_action,
        syntax: CachedSyntax.t,
        z: Zipper.t,
        mk_statics: Zipper.t => StaticsBase.Map.t,
        return:
          (Action.Failure.t, option(Zipper.t)) =>
          result(Zipper.t, Action.Failure.t),
      ) => {
    let curr_node_info =
      AssistantTreeHelper.build_curr_node_info(z, mk_statics(z));
    switch (curr_node_info) {
    | None => Error(Action.Failure.Cant_derive_local_AST_information) //todo, add failure case
    | Some(node) =>
      switch (a) {
      | Nav(n) =>
        switch (n) {
        | GoToParent =>
          switch (node.parent) {
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
                (child: AssistantTreeHelper.node) => child.name == who,
                node.children,
              );
            if (List.length(cands) > 1) {
              Error(Action.Failure.Cant_move);
            } else {
              switch (ListUtil.hd_opt(cands)) {
              | None => Error(Action.Failure.Cant_move)
              | Some(child) =>
                switch (Select.tile(Info.id_of(child.info), z)) {
                | Some(z) => Ok(z)
                | None => Error(Action.Failure.Cant_select)
                }
              };
            };
          | Some(nth) =>
            // this means the llm provided an index to move to, in which case
            // we default on using that as opposed to the name
            switch (List.nth_opt(node.children, nth)) {
            | None => Error(Action.Failure.Cant_move)
            | Some(child) =>
              switch (Select.tile(Info.id_of(child.info), z)) {
              | Some(z) => Ok(z)
              | None => Error(Action.Failure.Cant_select)
              }
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
                  (sibling: AssistantTreeHelper.node) => sibling.name == who,
                  node.siblings,
                );
              if (List.length(cands) > 1) {
                Error(Action.Failure.Cant_move);
              } else {
                switch (ListUtil.hd_opt(cands)) {
                | None => Error(Action.Failure.Cant_move)
                | Some(sibling) =>
                  switch (Select.tile(Info.id_of(sibling.info), z)) {
                  | Some(z) => Ok(z)
                  | None => Error(Action.Failure.Cant_select)
                  }
                };
              };
            | Some(nth) =>
              // this means the llm provided an index to move to, in which case
              // we default on using that as opposed to the name
              switch (List.nth_opt(node.siblings, nth)) {
              | None => Error(Action.Failure.Cant_move)
              | Some(sibling) =>
                switch (Select.tile(Info.id_of(sibling.info), z)) {
                | Some(z) => Ok(z)
                | None => Error(Action.Failure.Cant_select)
                }
              }
            }
          | Stepwise(d) =>
            let len = List.length(node.siblings);
            print_endline(
              "node.sibling_idx: "
              ++ string_of_int(node.sibling_idx)
              ++ " len: "
              ++ string_of_int(len),
            );
            let target_id =
              switch (d) {
              | Left =>
                List.nth(node.siblings, (node.sibling_idx - 1 + len) mod len).
                  info
                |> Info.id_of
              | Right =>
                // Don't add 1 here because we filtered out the current node
                List.nth(node.siblings, (node.sibling_idx + len) mod len).info
                |> Info.id_of
              };
            switch (Select.tile(target_id, z)) {
            | Some(z) => Ok(z)
            | None => Error(Action.Failure.Cant_select)
            };
          }
        }
      | Read(_r) => Ok(z) // todo
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
        let introduce = (z, code) => {
          // Just a helper function for trying to paste code into the zipper
          // Note that we paste a segment; so, we convert the string to a segment
          // first, and then insert the segment into the zipper. This helps to
          // avoid potential current buggy parsing issues.
          Parser.to_segment(code)
          |> OptUtil.and_then((segment: Segment.t) =>
               Some(Zipper.insert_segment(z, segment))
             )
          |> return(CantPaste);
        };
        let overwrite_term = (z, target_id, code) => {
          // Select the respective tile (in this case the definition tile)
          switch (
            Select.term(
              ~defs_exclude_bodies=true,
              ~case_rules=false,
              syntax.term_data, // todo: not sure about this arg
              target_id,
              z,
            )
          ) {
          | Some(z') =>
            // Paste the code over the selected tile
            introduce(z', code)
          | None => Error(Action.Failure.Cant_select)
          };
        };
        let insert_term = (z, target_id, code, direction) => {
          switch (
            Select.term(
              ~defs_exclude_bodies=true,
              ~case_rules=false,
              syntax.term_data, // todo: not sure about this arg
              target_id,
              z,
            )
          ) {
          | Some(z') =>
            switch (Move.by_token(direction, z')) {
            | Some(z'') => introduce(z'', code)
            | None => Error(Action.Failure.Cant_move)
            }
          | None => Error(Action.Failure.Cant_select)
          };
        };
        let destruct_term = (~defs_exclude_bodies, z, target_id) => {
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
        switch (e) {
        | UpdateDefinition(code) =>
          let target_id = get_inner_term_id(node, Def);
          overwrite_term(z, target_id, code);
        | UpdateBody(code) =>
          let target_id = get_inner_term_id(node, Body);
          overwrite_term(z, target_id, code);
        | UpdatePattern(code) =>
          let target_id = get_inner_term_id(node, Pat);
          overwrite_term(z, target_id, code);
        | UpdateBindingClause(code) =>
          let target_id = Info.id_of(node.info);
          overwrite_term(z, target_id, code);
        | DeleteBindingClause =>
          destruct_term(~defs_exclude_bodies=true, z, Info.id_of(node.info))
        | DeleteBody =>
          let target_id = get_inner_term_id(node, Body);
          destruct_term(~defs_exclude_bodies=false, z, target_id);
        | InsertBefore(code) =>
          insert_term(z, Info.id_of(node.info), code ++ " ", Direction.Left)
        | InsertAfter(code) =>
          // todo: figure out a better method than magic space
          insert_term(z, Info.id_of(node.info), " " ++ code, Direction.Right)
        };
      }
    };
    // todo
    // todo: not sure about this arg
    // todo: not sure about this arg
  };
};
