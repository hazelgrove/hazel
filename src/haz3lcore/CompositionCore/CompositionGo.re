open Util_web;
open HighLevelNodeMap.Public;
open Language;
open OptUtil.Syntax;

type node_map = HighLevelNodeMap.t;
type node = HighLevelNodeMap.node;

module Local = {
  type inner_term =
    | Pat
    | Def
    | Body;

  module Utils = {
    let get_inner_term_id = (inner_term: inner_term, node: node): Id.t => {
      /*
       Returns the specified "inner_term" from the "curr_node_info"

       E.g. If current node is "x" in a program "let x : Int = 2 + 3 in 100 + 200",
       calling get_inner_term_id(curr_node_info, Pat) will return the id of the pattern "x : Int",
       calling get_inner_term_id(curr_node_info, Def) will return the id of the definition "2 + 3",
       calling get_inner_term_id(curr_node_info, Body) will return the id of the body "100 + 200".
       */
      switch (node.info) {
      | InfoExp({user_term: term, _}) =>
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
        | ModuleExp(mp, def, body) =>
          switch (inner_term) {
          | Pat => MPat.rep_id(mp)
          | Def => Exp.rep_id(def)
          | Body => Exp.rep_id(body)
          }
        | _ =>
          raise(
            Failure(
              "UNIMPLEMENTED_NODE_TYPE: Only let, type alias, and module expressions are currently supported as nodes",
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
  };

  let segment_of_term =
      (zipper: Zipper.t, target_id: option(Id.t), syntax: CachedSyntax.t)
      : option(Segment.t) => {
    switch (target_id) {
    | Some(target_id) =>
      let* zipper =
        Select.term(
          ~defs_exclude_bodies=true,
          ~case_rules=false,
          syntax.term_data,
          target_id,
          zipper,
        );
      Some(zipper.selection.content);
    | _ =>
      let zipper = Select.all(zipper);
      Some(zipper.selection.content);
    };
  };

  let get_diff =
      (
        old_zipper: Zipper.t,
        new_zipper: Zipper.t,
        action: Action.Structural.t,
        mk_statics: Zipper.t => StaticsBase.Map.t,
        syntax: CachedSyntax.t,
      )
      : option((Segment.t, option(Segment.t))) => {
    switch (action) {
    | Insert(_, _, _) =>
      let* old_segment = segment_of_term(old_zipper, None, syntax);
      let new_segment = segment_of_term(new_zipper, None, syntax);
      Some((old_segment, new_segment));
    | Update(_, path, _)
    | Delete(_, path) =>
      /* Delete (especially BindingClause) removes the target from the new
         program; Delete(Pattern) replaces the naming pat with a hole so the
         path no longer resolves by name. Resolve the new-side target with
         [[path_to_id_opt]] and treat non-resolution as "no replacement
         segment" — the diff becomes "old segment deleted" instead of
         raising. Historical bug: using [[path_to_id]] here raised
         "Path X not found in node map" after every successful delete,
         surfacing to the agent as a spurious tool-call failure. */
      let* old_node_map =
        HighLevelNodeMap.build(old_zipper, mk_statics(old_zipper));
      let* new_node_map =
        HighLevelNodeMap.build(new_zipper, mk_statics(new_zipper));
      let old_target_id = path_to_id(old_node_map, path);
      let* old_segment =
        segment_of_term(old_zipper, Some(old_target_id), syntax);
      let new_segment =
        switch (path_to_id_opt(new_node_map, path)) {
        | Some(new_target_id) =>
          segment_of_term(new_zipper, Some(new_target_id), syntax)
        | None => None
        };
      Some((old_segment, new_segment));
    };
  };

  module PerformUtils = {
    let edit_action_to_static_error_scrutiny =
        (~edit_action: Action.Structural.t): (bool, bool, bool) => {
      // Returns (of_pat, of_def, of_body), i.e. which parts of the program to check for static errors.
      switch (edit_action) {
      | Update(Definition, _, _) => (true, true, false)
      | Update(Body, _, _) => (true, true, true)
      | Update(Pattern, _, _) => (true, false, false)
      | Update(BindingClause, _, _) => (false, true, false)
      | Insert(_, _, _) => (false, false, false)
      | Delete(BindingClause, _) => (false, true, false)
      | Delete(Body, _) => (false, false, true)
      | Delete(Definition | Pattern, _) => (false, false, false)
      };
    };

    let static_error_check =
        (
          ~edit_action: Action.Structural.t,
          ~initial_node: option(node),
          ~initial_info_map: Id.Map.t(Info.t),
          ~new_node: node,
          ~new_info_map: Id.Map.t(Info.t),
        )
        : option(string) => {
      /*
       A localized static error check to ensure that newly inserted segments do not introduce any errors.

       This is a localized check, as obligations occuring elsewhere in the program are inevitable for
       many types of edits.

       of_pat, of_def, and of_body are used to specify which parts of the program to check for errors.
       */
      let (of_pat, of_def, of_body) =
        edit_action_to_static_error_scrutiny(~edit_action);
      let initial_errors =
        switch (initial_node) {
        | None => []
        | Some(initial_node) =>
          let initial_subtree =
            GeneralTreeUtils.subtree_of(
              ~info=initial_node.info,
              ~orig_info_map=initial_info_map,
              ~of_pat,
              ~of_def,
              ~of_body,
            );
          ErrorPrint.all(initial_subtree);
        };
      let new_subtree =
        GeneralTreeUtils.subtree_of(
          ~info=new_node.info,
          ~orig_info_map=new_info_map,
          ~of_pat,
          ~of_def,
          ~of_body,
        );
      let new_errors = ErrorPrint.all(new_subtree);
      if (List.length(new_errors) > List.length(initial_errors)) {
        Some(
          "Not applying the action you requested as it would have the following static error(s): "
          ++ String.concat(", ", new_errors),
        );
      } else {
        None;
      };
    };

    /* [[Zipper.insert_segment]] replaces the selection with the segment,
       so a token bordering the selection can end up flush against the
       segment's edge token. If the two would lex as one token the result
       renders (and reparses) fused, e.g. overwriting the bare hole body of
       `let x = 1 in` — a grout directly abutting `in`, carrying no
       whitespace — with `let e = ...` yields `inlet e = ...`. Pad the
       segment with a space on any side where its edge token would fuse
       with the piece outside the selection. */
    let pad_fusing_edges = (z: Zipper.t, seg: Segment.t): Segment.t => {
      let edge_token = (d: Direction.t, p: Piece.t): option(Token.t) =>
        switch (p) {
        | Secondary({content: Whitespace(s) | Comment(s), _}) => Some(s)
        | Grout(_)
        | Projector(_) => None
        | Tile(t) =>
          let* shard =
            d == Left
              ? ListUtil.hd_opt(t.shards) : ListUtil.last_opt(t.shards);
          List.nth_opt(t.label, shard);
        };
      let outer_token = (d: Direction.t): option(Token.t) => {
        let (l_sibs, r_sibs) = z.relatives.siblings;
        switch (
          d == Left ? ListUtil.last_opt(l_sibs) : ListUtil.hd_opt(r_sibs)
        ) {
        | Some(p) => edge_token(Direction.toggle(d), p)
        | None =>
          let* a = Ancestors.parent(z.relatives.ancestors);
          let* shard =
            d == Left
              ? ListUtil.last_opt(fst(a.shards))
              : ListUtil.hd_opt(snd(a.shards));
          List.nth_opt(a.label, shard);
        };
      };
      let fuses = (l: option(Token.t), r: option(Token.t)): bool =>
        switch (l, r) {
        | (Some(l), Some(r)) => Token.is_potential_token(Token.append(l, r))
        | _ => false
        };
      let space = () =>
        Piece.Secondary({
          id: Id.mk(),
          content: Secondary.Whitespace(Token.space),
        });
      let seg_edge = (d: Direction.t, seg: Segment.t) => {
        let* p = d == Left ? ListUtil.hd_opt(seg) : ListUtil.last_opt(seg);
        edge_token(d, p);
      };
      let seg =
        fuses(outer_token(Left), seg_edge(Left, seg))
          ? [space(), ...seg] : seg;
      fuses(seg_edge(Right, seg), outer_token(Right))
        ? seg @ [space()] : seg;
    };

    /* Post-edit vertical-whitespace normalization for the agent structural
       edit path. Operates on the zipper's top-level segment only (the
       outermost binding chain); inner definition/body layout is untouched.
       Motivation: the insert/update arms wrap pasted code as "\n"++code++"\n"
       ("magic space"), and nothing collapses the resulting linebreak runs, so
       (1) prepends leave a leading blank line and (2) trailing linebreaks
       accumulate across successive edits near the program end. Policy:
       - no blank lines before the first piece,
       - exactly one blank line (two linebreaks) between consecutive top-level
         bindings (tiles whose form ends in `in`),
       - a single linebreak on any other inter-piece boundary that already had
         one, and at most a single trailing linebreak at program end.
       Only maximal runs of linebreaks are rewritten; spaces (including body
       indentation) and comment secondaries are left in place and bound the
       runs. Fresh ids on the linebreaks are overlay-safe: statics/probes key
       to term ids. */
    let is_linebreak = (p: Piece.t): bool =>
      switch (p) {
      | Secondary({content: Whitespace(s), _}) => s == Token.linebreak
      | _ => false
      };
    let is_binding_tile = (p: Piece.t): bool =>
      switch (p) {
      | Tile(t) => ListUtil.last_opt(t.label) == Some("in")
      | _ => false
      };
    let linebreak = () =>
      Piece.Secondary({
        id: Id.mk(),
        content: Secondary.Whitespace(Token.linebreak),
      });
    let normalize_top_level_whitespace = (seg: Segment.t): Segment.t => {
      /* Group into maximal linebreak runs and everything else (tokens), so a
         run's immediately bounding tokens decide its normalized count. */
      let items =
        List.fold_right(
          (p, acc) =>
            switch (is_linebreak(p), acc) {
            | (true, [`Run(n), ...rest]) => [`Run(n + 1), ...rest]
            | (true, _) => [`Run(1), ...acc]
            | (false, _) => [`Tok(p), ...acc]
            },
          seg,
          [],
        );
      let rec go = (prev_tok: option(Piece.t), items) =>
        switch (items) {
        | [] => []
        | [`Tok(p), ...rest] => [p, ...go(Some(p), rest)]
        | [`Run(_), ...rest] =>
          let next_tok =
            switch (rest) {
            | [`Tok(p), ..._] => Some(p)
            | _ => None
            };
          let replacement =
            switch (prev_tok, next_tok) {
            | (None, _) => [] /* start of program: no leading blank */
            | (_, None) => [linebreak()] /* end: single trailing linebreak */
            | (Some(l), Some(r)) =>
              is_binding_tile(l) && is_binding_tile(r)
                ? [linebreak(), linebreak()] : [linebreak()]
            };
          replacement @ go(prev_tok, rest);
        };
      go(None, items);
    };

    /* Zip to the top-level segment, normalize its whitespace, and rebuild a
       zipper. Idempotent. The agent edit path rebuilds the editor from this
       zipper, so resetting the caret to the segment start is harmless. */
    let normalize_top_level = (z: Zipper.t): Zipper.t =>
      z
      |> Zipper.unselect_and_zip
      |> normalize_top_level_whitespace
      |> Zipper.unzip;

    /* Form delimiters that lex like identifiers; using one as a variable
       name makes the surrounding code misparse. */
    let reserved_words: list(Token.t) =
      List.filter(Token.is_var, Form.delims);

    let identifier_words = (s: string): list(string) => {
      let is_id_char = c =>
        c >= 'a'
        && c <= 'z'
        || c >= 'A'
        && c <= 'Z'
        || c >= '0'
        && c <= '9'
        || c == '_'
        || c == '\'';
      let (words, last) =
        String.fold_left(
          ((words, cur), c) =>
            is_id_char(c)
              ? (words, cur ++ String.make(1, c))
              : cur == "" ? (words, "") : ([cur, ...words], ""),
          ([], ""),
          s,
        );
      List.rev(last == "" ? words : [last, ...words]);
    };

    /* Reserved word in binder position (after let/fun/type), or as the
       entire code string: the misuse behind most agent paste failures,
       e.g. `let eval = ...` where `eval` opens a filter form. */
    let find_reserved_binder = (code: string): option(string) => {
      let reserved = w => List.mem(w, reserved_words);
      let trimmed = String.trim(code);
      if (reserved(trimmed)) {
        Some(trimmed);
      } else {
        let rec scan = words =>
          switch (words) {
          | [intro, w, ..._]
              when List.mem(intro, ["let", "fun", "type"]) && reserved(w) =>
            Some(w)
          | [_, ...rest] => scan(rest)
          | [] => None
          };
        scan(identifier_words(code));
      };
    };

    let reserved_word_note = (code: string): string =>
      switch (find_reserved_binder(code)) {
      | Some(w) =>
        " Note: `"
        ++ w
        ++ "` is a reserved keyword in Hazel and cannot be used as a variable name."
      | None => ""
      };

    /* Every agent-supplied code string funnels through here. Strip
       per-line leading whitespace (models emit indented code; Hazel
       re-indents structurally on render), then parse to a segment and
       paste it. Safe: Hazel strings and comments are single-line, so
       no token can span a linebreak. */
    let introduce =
        (z: Zipper.t, code: string): result(Zipper.t, Action.Failure.t) => {
      let code = StringUtil.trim_leading(code) |> Unicode.nfc_outside_strings;
      switch (Parser.to_segment(code, ~root=Exp)) {
      | Some(segment) =>
        Ok(
          Zipper.insert_segment(z, pad_fusing_edges(z, segment), ~root=Exp),
        )
      | None =>
        Error(
          Action.Failure.Composition_action_failure(
            "Inserted code failed to parse." ++ reserved_word_note(code),
          ),
        )
      };
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
        switch (Destruct.go(Left, z', ~root=Exp)) {
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
        introduce(z', code)
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
        | Some(z'') => introduce(z'', code)
        | None => Error(Action.Failure.Cant_move)
        }
      | None => Error(Action.Failure.Cant_select)
      };
    };
  };

  let edit_dispatch =
      (
        ~e: Action.Structural.t,
        ~initial_z: Zipper.t,
        ~initial_node_map: node_map,
        ~initial_info_map: Id.Map.t(Info.t),
        ~syntax: CachedSyntax.t,
        ~mk_statics: Zipper.t => StaticsBase.Map.t,
      ) => {
    switch (e) {
    | Update(Definition, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(Def, initial_node);
      switch (
        PerformUtils.overwrite_term(initial_z, target_id, code, false, syntax)
      ) {
      | Error(e) => Error(e)
      | Ok(new_z) =>
        let new_info_map = mk_statics(new_z);
        switch (build(new_z, new_info_map)) {
        | None => Error(Action.Failure.Cant_derive_local_AST_information)
        | Some(new_node_map) =>
          switch (
            PerformUtils.static_error_check(
              ~edit_action=e,
              ~initial_node=Some(initial_node),
              ~initial_info_map,
              ~new_node=node_of_cursor(new_node_map, new_z, new_info_map),
              ~new_info_map,
            )
          ) {
          | Some(e) =>
            Error(
              Action.Failure.Composition_action_failure(
                e ++ PerformUtils.reserved_word_note(code),
              ),
            )
          | None =>
            let z_after_projectors =
              try({
                let fresh_syn = CachedSyntax.init(new_z);
                let binding_node = path_to_node(new_node_map, path);
                let def_id = Utils.get_inner_term_id(Def, binding_node);
                switch (
                  Select.term(
                    ~defs_exclude_bodies=false,
                    ~case_rules=false,
                    fresh_syn.term_data,
                    def_id,
                    new_z,
                  )
                ) {
                | None => new_z
                | Some(z_sel) =>
                  let seg = z_sel.selection.content;
                  let focus = z_sel.selection.focus;
                  let (z_sel', new_seg, did_change) =
                    ProjectorPerform.revalidate_projectors_in_segment(
                      z_sel,
                      seg,
                    );
                  did_change
                    ? Zipper.replace_selection(focus, new_seg, z_sel') : new_z;
                };
              }) {
              | Failure(_) => new_z
              };
            Ok(z_after_projectors);
          }
        };
      };
    | Update(Body, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(Body, initial_node);
      switch (
        PerformUtils.overwrite_term(initial_z, target_id, code, false, syntax)
      ) {
      | Error(e) => Error(e)
      | Ok(new_z) =>
        let new_info_map = mk_statics(new_z);
        switch (build(new_z, new_info_map)) {
        | None => Error(Action.Failure.Cant_derive_local_AST_information)
        | Some(new_node_map) =>
          switch (
            PerformUtils.static_error_check(
              ~edit_action=e,
              ~initial_node=Some(initial_node),
              ~initial_info_map,
              ~new_node=node_of_cursor(new_node_map, new_z, new_info_map),
              ~new_info_map,
            )
          ) {
          | Some(e) =>
            Error(
              Action.Failure.Composition_action_failure(
                e ++ PerformUtils.reserved_word_note(code),
              ),
            )
          | None => Ok(new_z)
          }
        };
      };
    | Update(Pattern, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(Pat, initial_node);
      let old_pat =
        StaticsBase.Map.lookup(target_id, initial_info_map)
        |> OptUtil.get_or_fail(
             "Failed trying to rename all occurences of the pattern. Could not find the old pattern in the statics map.",
           );
      switch (
        PerformUtils.overwrite_term(initial_z, target_id, code, false, syntax)
      ) {
      | Error(e) => Error(e)
      | Ok(new_z) =>
        let new_info_map = mk_statics(new_z);
        switch (build(new_z, new_info_map)) {
        | None => Error(Action.Failure.Cant_derive_local_AST_information)
        | Some(new_node_map) =>
          let new_node = node_of_cursor(new_node_map, new_z, new_info_map);
          switch (
            PerformUtils.static_error_check(
              ~edit_action=e,
              ~initial_info_map,
              ~initial_node=Some(initial_node),
              ~new_info_map,
              ~new_node,
            )
          ) {
          | Some(e) =>
            Error(
              Action.Failure.Composition_action_failure(
                e ++ PerformUtils.reserved_word_note(code),
              ),
            )
          | None =>
            let new_target_id = Utils.get_inner_term_id(Pat, new_node);
            let new_pat =
              StaticsBase.Map.lookup(new_target_id, new_info_map)
              |> OptUtil.get_or_fail(
                   "Failed trying to rename all occurences of the pattern. Could not find the new pattern in the statics map.",
                 );
            let old_names = GeneralTreeUtils.get_var_names_from_pat(old_pat);
            let new_names = GeneralTreeUtils.get_var_names_from_pat(new_pat);
            /* Old pattern binding no names (hole/wild) has no use sites to
               rewrite; any other bound-name count change makes old→new use
               site mapping ambiguous, so reject rather than silently leaving
               stale references. */
            if (old_names != []
                && List.length(old_names) != List.length(new_names)) {
              Error(
                Action.Failure.Composition_action_failure(
                  "Cannot rewrite use sites: the old pattern binds "
                  ++ string_of_int(List.length(old_names))
                  ++ " name(s) ("
                  ++ String.concat(", ", old_names)
                  ++ ") but the new pattern binds "
                  ++ string_of_int(List.length(new_names))
                  ++ " ("
                  ++ String.concat(", ", new_names)
                  ++ "). Keep the same number of bound names, or update the definition and body references explicitly.",
                ),
              );
            } else {
              /* Capture pre-check: statics won't flag capture (the reference
                 still resolves), so reject genuinely-new names that already
                 occur anywhere in this binding's scope. Conservative:
                 over-rejects some shadow-safe cases. */
              let added_names =
                List.filter(n => !List.mem(n, old_names), new_names);
              let scope_root = id_of(initial_node);
              let taken_name =
                List.find_opt(
                  GeneralTreeUtils.name_occurs_within(
                    ~root_id=scope_root,
                    ~info_map=initial_info_map,
                  ),
                  added_names,
                );
              switch (taken_name) {
              | Some(name) =>
                Error(
                  Action.Failure.Composition_action_failure(
                    "Not renaming to \""
                    ++ name
                    ++ "\": that name already occurs as a binder or variable reference within this binding's scope, so the rename could silently change which binding existing references point to. Choose a name that is unused in this scope.",
                  ),
                )
              | None =>
                /* Hybrid refs: pre-edit [[co_ctx]] for the filter, post-edit
                   term + [[new_info_map]] for the body, so renames see stale
                   spellings but paths stay consistent after follow-up edits
                   (see [[GeneralTreeUtils.get_refs_to_after_pattern_edit]]). */
                let final_z =
                  old_names == []
                    ? new_z
                    : GeneralTreeUtils.update_use_sites_of_pat(
                        ~z=new_z,
                        ~co_ctx=
                          GeneralTreeUtils.get_refs_to_after_pattern_edit(
                            ~pre_edit_let_info=initial_node.info,
                            ~post_edit_let_info=new_node.info,
                            new_info_map,
                          ),
                        ~old_names,
                        ~new_names,
                      );
                /* Belt-and-suspenders: re-validate after the use-site rewrite;
                   anything the pre-checks missed must not grow the error count. */
                let initial_errors = ErrorPrint.all(initial_info_map);
                let final_errors = ErrorPrint.all(mk_statics(final_z));
                if (List.length(final_errors) > List.length(initial_errors)) {
                  Error(
                    Action.Failure.Composition_action_failure(
                      "Not applying the rename: rewriting the use sites would introduce new static error(s): "
                      ++ String.concat(", ", final_errors),
                    ),
                  );
                } else {
                  Ok(final_z);
                };
              };
            };
          };
        };
      };
    | Update(BindingClause, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = path_to_id(initial_node_map, path);
      switch (
        PerformUtils.overwrite_term(initial_z, target_id, code, true, syntax)
      ) {
      | Error(e) => Error(e)
      | Ok(new_z) =>
        let new_info_map = mk_statics(new_z);
        switch (build(new_z, new_info_map)) {
        | None => Error(Action.Failure.Cant_derive_local_AST_information)
        | Some(new_node_map) =>
          switch (
            PerformUtils.static_error_check(
              ~edit_action=e,
              ~initial_info_map,
              ~initial_node=Some(initial_node),
              ~new_info_map,
              ~new_node=node_of_cursor(new_node_map, new_z, new_info_map),
            )
          ) {
          | Some(e) =>
            Error(
              Action.Failure.Composition_action_failure(
                e ++ PerformUtils.reserved_word_note(code),
              ),
            )
          | None => Ok(new_z)
          }
        };
      };
    | Insert(Before, path, code) =>
      // todo: figure out a better method than magic space
      let target_id = path_to_id(initial_node_map, path);
      switch (
        PerformUtils.insert_term(
          initial_z,
          target_id,
          "\n" ++ code ++ "\n",
          Direction.Left,
          syntax,
        )
      ) {
      | Error(e) => Error(e)
      | Ok(new_z) =>
        let new_info_map = mk_statics(new_z);
        let old_errors = ErrorPrint.all(initial_info_map);
        let new_errors = ErrorPrint.all(new_info_map);
        if (List.length(new_errors) > List.length(old_errors)) {
          Error(
            Action.Failure.Composition_action_failure(
              "Not applying the action you requested as it would introduce new static error(s): "
              ++ String.concat(", ", new_errors)
              ++ PerformUtils.reserved_word_note(code),
            ),
          );
        } else {
          Ok(new_z);
        };
      };
    | Insert(After, path, code) =>
      // todo: figure out a better method than magic space
      let target_id = path_to_id(initial_node_map, path);
      switch (
        PerformUtils.insert_term(
          initial_z,
          target_id,
          "\n" ++ code ++ "\n",
          Direction.Right,
          syntax,
        )
      ) {
      | Error(e) => Error(e)
      | Ok(new_z) =>
        let new_info_map = mk_statics(new_z);
        let old_errors = ErrorPrint.all(initial_info_map);
        let new_errors = ErrorPrint.all(new_info_map);
        if (List.length(new_errors) > List.length(old_errors)) {
          Error(
            Action.Failure.Composition_action_failure(
              "Not applying the action you requested as it would introduce new static error(s): "
              ++ String.concat(", ", new_errors)
              ++ PerformUtils.reserved_word_note(code),
            ),
          );
        } else {
          Ok(new_z);
        };
      };
    | Delete(BindingClause, path) =>
      let target_id = path_to_id(initial_node_map, path);
      PerformUtils.destruct(
        ~defs_exclude_bodies=true,
        initial_z,
        target_id,
        syntax,
      );
    | Delete(Body, path) =>
      let node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(Body, node);
      PerformUtils.destruct(
        ~defs_exclude_bodies=false,
        initial_z,
        target_id,
        syntax,
      );
    | Delete(Definition | Pattern, _) =>
      Error(
        Action.Failure.Composition_action_failure(
          "Deleting a definition or pattern is not yet implemented.",
        ),
      )
    };
  };

  let composition_dispatch =
      (
        a: Action.Structural.t,
        syntax: CachedSyntax.t,
        z: Zipper.t,
        mk_statics: Zipper.t => StaticsBase.Map.t,
      ) => {
    let initial_info_map = mk_statics(z);
    switch (build(z, initial_info_map)) {
    | None => Error(Action.Failure.Cant_derive_local_AST_information)
    | Some(initial_node_map) =>
      edit_dispatch(
        ~e=a,
        ~initial_z=z,
        ~initial_node_map,
        ~initial_info_map,
        ~syntax,
        ~mk_statics,
      )
    };
  };

  let go =
      (
        ~mk_statics: Zipper.t => StaticsBase.Map.t,
        ~syntax: CachedSyntax.t,
        ~z: Zipper.t,
        ~a: Action.Structural.t,
      )
      : result(Zipper.t, Action.Failure.t) => {
    let res =
      try(
        switch (composition_dispatch(a, syntax, z, mk_statics)) {
        | Ok(new_z) =>
          Ok(
            PerformUtils.normalize_top_level(
              Dump.to_zipper(new_z, ~root=Exp),
            ),
          )
        | Error(e) => Error(e)
        }
      ) {
      | Failure(e) => Error(Action.Failure.Composition_action_failure(e))
      };

    res;
  };
};

module Public = {
  let mk_statics = (z: Zipper.t): Language.StaticsBase.Map.t =>
    Language.(
      fst(
        Statics.mk(
          CoreSettings.on,
          Builtins.ctx_init(Some(Operators.default_mode)),
          MakeTerm.from_zip_for_sem(z, ~root=Exp).term,
        ),
      )
    );
  let go = Local.go(~mk_statics);
};
