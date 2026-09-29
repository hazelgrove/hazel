open Util;
open HighLevelNodeMap.Public;
open Language;
open OptUtil.Syntax;

/* phase timers for the journal's perf lines */
let build = (z, info_map) =>
  PerfTimer.time("node-map", () => build(z, info_map));

type node_map = HighLevelNodeMap.t;
type node = HighLevelNodeMap.node;

module Local = {
  type inner_term =
    | Pat
    | Def
    | Body
    | TypeAnn;

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
          | TypeAnn =>
            switch (Pat.term_of(pat)) {
            | Asc(_, typ) => Typ.rep_id(typ)
            | _ =>
              raise(
                Failure("No type annotation found on this binding's pattern"),
              )
            }
          }
        | TyAlias(tpat, tdef, body) =>
          switch (inner_term) {
          | Pat => TPat.rep_id(tpat)
          | Def => Typ.rep_id(tdef)
          | Body => Exp.rep_id(body)
          | TypeAnn => Typ.rep_id(tdef) /* For type alias, the "annotation" is the definition */
          }
        | ModuleExp(mp, def, body) =>
          switch (inner_term) {
          | Pat => MPat.rep_id(mp)
          | Def => Exp.rep_id(def)
          | Body => Exp.rep_id(body)
          | TypeAnn =>
            switch (mp.term) {
            | Asc(_, sig_typ) => Typ.rep_id(sig_typ)
            | _ =>
              raise(
                Failure(
                  "No type annotation found on this module binding's pattern",
                ),
              )
            }
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

    /* Module members are nodes whose info is the statics expansion's
       Let/TyAlias wrapper, reclassified to a Mod cls. Their pat/def ids are
       real syntax; their "body" is the expansion continuation (the REST of
       the members), which must never be an edit target. */
    let is_module_member = (node: node): bool =>
      switch (node.info) {
      | InfoExp({cls: Mod(_), _}) => true
      | _ => false
      };

    let member_body_error = (what: string) =>
      Error(
        Action.Failure.Composition_action_failure(
          "Cannot "
          ++ what
          ++ " of a module member: members end at their `;` and have no body. Target the member's definition (update_definition), the whole member (update_binding_clause / delete_binding_clause), or the enclosing module binding.",
        ),
      );
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

  /* each side is selected against ITS OWN syntax: term data of the old
     program does not describe the new one, and Select.term on a stale
     table fell to its slow extremes search (1.3 s of a 1.9 s
     update_definition at 170 lines) */
  let get_diff =
      (
        old_zipper: Zipper.t,
        new_zipper: Zipper.t,
        action: Action.Structural.t,
        mk_statics: Zipper.t => StaticsBase.Map.t,
        ~old_syntax: CachedSyntax.t,
        ~new_syntax: CachedSyntax.t,
      )
      : option((Segment.t, option(Segment.t))) => {
    switch (action) {
    | Insert(_, _, _) =>
      let* old_segment =
        PerfTimer.time("diff/segment", () =>
          segment_of_term(old_zipper, None, old_syntax)
        );
      let new_segment =
        PerfTimer.time("diff/segment", () =>
          segment_of_term(new_zipper, None, new_syntax)
        );
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
      let old_statics =
        PerfTimer.time("diff/statics", () => mk_statics(old_zipper));
      let* old_node_map =
        PerfTimer.time("diff/node-map", () =>
          HighLevelNodeMap.build(old_zipper, old_statics)
        );
      let new_statics =
        PerfTimer.time("diff/statics", () => mk_statics(new_zipper));
      let* new_node_map =
        PerfTimer.time("diff/node-map", () =>
          HighLevelNodeMap.build(new_zipper, new_statics)
        );
      let old_target_id =
        PerfTimer.time("diff/path", () => path_to_id(old_node_map, path));
      let* old_segment =
        PerfTimer.time("diff/segment", () =>
          segment_of_term(old_zipper, Some(old_target_id), old_syntax)
        );
      let new_segment =
        switch (
          PerfTimer.time("diff/path", () =>
            path_to_id_opt(new_node_map, path)
          )
        ) {
        | Some(new_target_id) =>
          PerfTimer.time("diff/segment", () =>
            segment_of_term(new_zipper, Some(new_target_id), new_syntax)
          )
        | None => None
        };
      Some((old_segment, new_segment));
    | SelectorUpdate(_, _)
    | SelectorDelete(_)
    | SelectorInsertBefore(_, _)
    | SelectorInsertAfter(_, _) =>
      /* Selector-driven edits: diff the whole program */
      let* old_segment = segment_of_term(old_zipper, None, syntax);
      let new_segment = segment_of_term(new_zipper, None, syntax);
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
      | Update(TypeAnnotation, _, _) => (true, true, false)
      | Update(BindingClause, _, _) => (false, true, false)
      | Insert(_, _, _) => (false, false, false)
      | Delete(BindingClause, _) => (false, true, false)
      | Delete(Body, _) => (false, false, true)
      | Delete(Definition | Pattern | TypeAnnotation, _) => (
          false,
          false,
          false,
        )
      /* Selector-driven: check everything since we don't know the target */
      | SelectorUpdate(_, _) => (true, true, true)
      | SelectorDelete(_) => (true, true, true)
      | SelectorInsertBefore(_, _) => (false, false, false)
      | SelectorInsertAfter(_, _) => (false, false, false)
      };
    };

    let parse_error_check = (z: Zipper.t): option(string) => {
      /* Check for parse errors:
         1. Unmatched delimiters (orphaned shards in the backpack)
         2. Invalid nodes in the term tree
         3. MultiHole nodes in the term tree */
      let backpack = Zipper.local_missing_shards(z);
      switch (backpack) {
      | [_, ..._] as tiles =>
        let labels =
          tiles
          |> List.map((t: Tile.t) =>
               String.concat(" ", Tile.effective_label(t))
             );
        Some(
          "Parse error: unmatched delimiter(s) ["
          ++ String.concat(", ", labels)
          ++ "]. Check for missing or extra delimiters in your code.",
        );
      | [] =>
        /* Check for Invalid/MultiHole nodes in the parsed term */
        let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
        let errors = ref([]);
        let _ =
          Exp.map_term(
            ~f_exp=
              (continue, e) => {
                switch (Exp.term_of(e)) {
                | Invalid(token) =>
                  errors := ["Invalid token: \"" ++ token ++ "\"", ...errors^];
                  e;
                | MultiHole(_) =>
                  errors := ["MultiHole (malformed expression)", ...errors^];
                  e;
                | _ => continue(e)
                }
              },
            term,
          );
        switch (errors^) {
        | [] => None
        | errs =>
          Some(
            "Parse error: "
            ++ String.concat("; ", List.rev(errs))
            ++ ". Check your syntax.",
          )
        };
      };
    };

    /* Count empty holes in the program. Returns the number of
       expression, pattern, and type holes. Useful for the agent to
       know if the program is "complete" (no unfilled holes). */
    let count_holes = (z: Zipper.t): (int, int, int) => {
      let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
      let exp_holes = ref(0);
      let pat_holes = ref(0);
      let typ_holes = ref(0);
      let _ =
        Exp.map_term(
          ~f_exp=
            (continue, e) => {
              switch (Exp.term_of(e)) {
              | EmptyHole =>
                exp_holes := exp_holes^ + 1;
                e;
              | _ => continue(e)
              }
            },
          ~f_pat=
            (continue, p) => {
              switch (Pat.term_of(p)) {
              | EmptyHole =>
                pat_holes := pat_holes^ + 1;
                p;
              | _ => continue(p)
              }
            },
          ~f_typ=
            (continue, t) => {
              switch (Typ.term_of(t)) {
              | Unknown(Hole(EmptyHole)) =>
                typ_holes := typ_holes^ + 1;
                t;
              | _ => continue(t)
              }
            },
          term,
        );
      (exp_holes^, pat_holes^, typ_holes^);
    };

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

    /* When the batch parser rejected the chunk, its position-bearing
       message is far more useful than term-level static-error soup;
       attach it to rejections. */
    let parse_hint = (): string =>
      switch (FastParse.bail_note^) {
      | Some(n)
          when String.length(n) >= 7 && String.sub(n, 0, 7) == "menhir:" =>
        "\nSyntax hint (batch parser): "
        ++ n
        ++ " — if the code was meant to be complete, start there."
      | _ => ""
      };

    /* Compute a static error warning for an edit. Returns Some(warning)
       if the edit introduces new errors, None otherwise. This is purely
       informational — it does NOT block the edit. The agent can use
       get_statics to investigate and fix any errors. */
    let static_error_warning =
        (
          ~edit_action: Action.Structural.t,
          ~initial_node: option(node),
          ~initial_info_map: Id.Map.t(Info.t),
          ~new_node: node,
          ~new_info_map: Id.Map.t(Info.t),
        )
        : option(string) => {
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
          "Warning: this edit introduced new static error(s): "
          ++ String.concat(", ", new_errors)
          ++ ". Use get_statics to investigate.",
        );
      } else {
        None;
      };
    };

    /* Combined check: parse errors are hard failures (unmatched delimiters,
       invalid tokens). Static errors produce warnings but do NOT block the
       edit, allowing multi-step refactoring where intermediate states have
       type errors. validate_edit_full returns intermediate results
       (info_map, node_map) for callers that need them (e.g. Pattern rename). */
    let validate_edit_full =
        (
          ~edit_action: Action.Structural.t,
          ~initial_node: option(node),
          ~initial_info_map: Id.Map.t(Info.t),
          ~new_z: Zipper.t,
          ~mk_statics: Zipper.t => StaticsBase.Map.t,
          ~code: string,
        )
        : result(
            (Zipper.t, Id.Map.t(Info.t), node_map, option(string)),
            Action.Failure.t,
          ) => {
      switch (parse_error_check(new_z)) {
      | Some(parse_err) =>
        Error(
          Action.Failure.Composition_action_failure(
            parse_err ++ reserved_word_note(code) ++ parse_hint(),
          ),
        )
      | None =>
        let new_info_map = mk_statics(new_z);
        switch (build(new_z, new_info_map)) {
        | None => Error(Action.Failure.Cant_derive_local_AST_information)
        | Some(new_node_map) =>
          let warning =
            static_error_warning(
              ~edit_action,
              ~initial_node,
              ~initial_info_map,
              ~new_node=node_of_cursor(new_node_map, new_z, new_info_map),
              ~new_info_map,
            );
          Ok((new_z, new_info_map, new_node_map, warning));
        };
      };
    };

    let validate_edit =
        (
          ~edit_action: Action.Structural.t,
          ~initial_node: option(node),
          ~initial_info_map: Id.Map.t(Info.t),
          ~new_z: Zipper.t,
          ~mk_statics: Zipper.t => StaticsBase.Map.t,
          ~code: string,
        )
        : result((Zipper.t, option(string)), Action.Failure.t) =>
      switch (
        validate_edit_full(
          ~edit_action,
          ~initial_node,
          ~initial_info_map,
          ~new_z,
          ~mk_statics,
          ~code,
        )
      ) {
      | Ok((z, _, _, warning)) => Ok((z, warning))
      | Error(e) => Error(e)
      };

    /* Hard-gate counterpart of [[static_error_warning]]: rejects the edit
       outright. Used where soft gating is unsafe (pattern renames). */
    let static_error_check =
        (
          ~edit_action: Action.Structural.t,
          ~initial_node: option(node),
          ~initial_info_map: Id.Map.t(Info.t),
          ~new_node: node,
          ~new_info_map: Id.Map.t(Info.t),
        )
        : option(string) => {
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
          PerfTimer.time("errors", () => ErrorPrint.all(initial_subtree));
        };
      let new_subtree =
        GeneralTreeUtils.subtree_of(
          ~info=new_node.info,
          ~orig_info_map=new_info_map,
          ~of_pat,
          ~of_def,
          ~of_body,
        );
      let new_errors =
        PerfTimer.time("errors", () => ErrorPrint.all(new_subtree));
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
          List.nth_opt(Tile.label(t), shard);
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
          List.nth_opt(Ancestor.label(a), shard);
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

    /* Local boundary hygiene after a structural splice. Existing whitespace
       is part of the document: preserve it when its neighbours survive.
       New joins collapse the edit's padding to one blank line between
       bindings, one newline elsewhere, and none at program start. Reuse
       the run's linebreak ids and allocate only missing pieces. */
    let is_linebreak = (p: Piece.t): bool =>
      switch (p) {
      | Secondary({content: Whitespace(s), _}) => s == Token.linebreak
      | _ => false
      };
    let is_binding_tile = (p: Piece.t): bool =>
      switch (p) {
      | Tile(t) => Tile.ends_with_in(t)
      | _ => false
      };
    let linebreak = () =>
      Piece.Secondary({
        id: Id.mk(),
        content: Secondary.Whitespace(Token.linebreak),
      });
    /* Preserve an existing whitespace run when its two neighbours survive.
       Newly inserted runs and newly joined boundaries receive the local
       spacing policy; pre-existing formatting elsewhere is not our scope. */
    let normalize_runs = (~before=[], ~module_body=false, seg: Segment.t) => {
      let rec runs = (prev, member, ps) =>
        switch (ps) {
        | [] => []
        | [p, ..._] when is_linebreak(p) =>
          let rec take = (acc, ps) =>
            switch (ps) {
            | [p, ...rest]
                when
                  is_linebreak(p)
                  || module_body
                  && (
                    switch (p) {
                    | Secondary({content: Whitespace(_), _}) => true
                    | _ => false
                    }
                  ) =>
              take([p, ...acc], rest)
            | _ => (List.rev(acc), ps)
            };
          let (run, rest) = take([], ps);
          let witness =
            switch (prev) {
            | Some(Piece.Tile(t)) when Tile.is_semi(t) =>
              Option.to_list(member)
            | _ => Option.to_list(Option.map(Piece.id, prev))
            };
          [
            `Run((prev, run, List.nth_opt(rest, 0), witness)),
            ...runs(prev, member, rest),
          ];
        | [p, ...rest] =>
          let member =
            switch (p) {
            | Piece.Tile(t) =>
              switch (Tile.label(t)) {
              | ["let" | "type" | "module", ..._] => Some(t.id)
              | _ => member
              }
            | _ => member
            };
          [`Tok(p), ...runs(Some(p), member, rest)];
        };
      let id = Option.map(Piece.id);
      let old_runs =
        runs(None, None, before)
        |> List.filter_map(
             fun
             | `Run(_, [p, ..._] as run, r, witness) =>
               Some((Piece.id(p), (witness, run, id(r))))
             | _ => None,
           )
        |> List.to_seq
        |> Id.Map.of_seq;
      runs(None, None, seg)
      |> List.concat_map(
           fun
           | `Tok(p) => [p]
           | `Run(left, run, right, witness) => {
               let preserved =
                 switch (Id.Map.find_opt(Piece.id(List.hd(run)), old_runs)) {
                 | Some((l, old, r)) =>
                   l == witness && r == id(right) && old == run
                 | None => false
                 };
               if (preserved) {
                 run;
               } else {
                 let n =
                   module_body
                     ? switch (left) {
                       | Some(Piece.Tile(t)) when Tile.is_semi(t) => 2
                       | _ => 1
                       }
                     : (
                       switch (left, right) {
                       | (None, _) => 0
                       | (Some(l), Some(r))
                           when is_binding_tile(l) && is_binding_tile(r) => 2
                       | _ => 1
                       }
                     );
                 if (module_body) {
                   /* A retained newline keeps its indent pieces too. The
                      region reindenter only visits newly allocated lines. */
                   let rec lines = ps =>
                     switch (ps) {
                     | [] => []
                     | [lb, ...rest] =>
                       let rec spaces = (acc, ps) =>
                         switch (ps) {
                         | [p, ...rest] when !is_linebreak(p) =>
                           spaces([p, ...acc], rest)
                         | _ => (List.rev(acc), ps)
                         };
                       let (indent, rest) = spaces([], rest);
                       [(lb, indent), ...lines(rest)];
                     };
                   let existing = lines(run);
                   let last_indent = snd(List.hd(List.rev(existing)));
                   List.init(n, i =>
                     switch (List.nth_opt(existing, i)) {
                     | Some((lb, indent)) => [
                         lb,
                         ...i == n - 1 ? last_indent : indent,
                       ]
                     | None => [linebreak()]
                     }
                   )
                   |> List.concat;
                 } else {
                   let kept = List.filteri((i, _) => i < n, run);
                   kept
                   @ List.init(max(0, n - List.length(kept)), _ =>
                       linebreak()
                     );
                 };
               };
             },
         );
    };
    let normalize_top_level_whitespace =
        (~before=[], seg: Segment.t): Segment.t =>
      normalize_runs(~before, seg);

    /* Module-body hygiene, applied recursively wherever a module literal
       appears. Two passes over a ModBody child segment:
       - separators: drop dangling member `;` (leading after `{`, trailing
         before `}`, or doubled after a member delete);
       - vertical whitespace: linebreak runs that FOLLOW a member `;`
         normalize to one blank line (mirroring the top-level policy);
         every other run to a single linebreak. Runs-only, like the
         top-level pass: single-line modules are never exploded. */
    let is_semi = (p: Piece.t): bool =>
      switch (p) {
      | Tile(t) => Tile.is_semi(t)
      | _ => false
      };
    let is_space = (p: Piece.t): bool =>
      switch (p) {
      | Secondary({content: Whitespace(w), _}) => w != Token.linebreak
      | _ => false
      };
    let is_mod_body = (t: Tile.t): bool =>
      t.form == Form.Compound(ModBody) && Tile.mold(t).in_ == [Sort.Mod];
    let clean_member_separators = (~before=[], seg: Segment.t): Segment.t => {
      let rec next_tok = ps =>
        switch (ps) {
        | [] => None
        | [Piece.Secondary(_), ...rest] => next_tok(rest)
        | [p, ..._] => Some(p)
        };
      /* Cleanup is confined to new joins. Even an incomplete old member or
         hand-spaced separator elsewhere in this module is outside the edit. */
      let id = Option.map(Piece.id);
      let rec boundaries = (prev, ps, acc) =>
        switch (ps) {
        | [] => acc
        | [p, ...rest] =>
          let acc =
            Id.Map.add(Piece.id(p), (id(prev), id(next_tok(rest))), acc);
          let prev =
            switch (p) {
            | Piece.Secondary(_) => prev
            | _ => Some(p)
            };
          boundaries(prev, rest, acc);
        };
      let old_boundaries = boundaries(None, before, Id.Map.empty);
      let unchanged = (prev, p, rest) =>
        Id.Map.find_opt(Piece.id(p), old_boundaries)
        == Some((id(prev), id(next_tok(rest))));
      /* Deleting a member leaves a convex grout in its slot (destruct
         replaces, it does not remove); a hole standing alone between
         separators/edges is that leftover, and goes together with the
         separator collapse below. Holes INSIDE a member (e.g. `let x = ?`)
         have a non-separator neighbor and are kept. */
      let is_member_boundary = (tok: option(Piece.t)): bool =>
        switch (tok) {
        | None => true
        | Some(t) => is_semi(t)
        };
      let rec drop_hole_members = (prev_tok: option(Piece.t), ps) =>
        switch (ps) {
        | [] => []
        | [Piece.Grout(_) as g, ...rest] =>
          !unchanged(prev_tok, g, rest)
          && is_member_boundary(prev_tok)
          && is_member_boundary(next_tok(rest))
            ? drop_hole_members(prev_tok, rest)
            : [g, ...drop_hole_members(Some(g), rest)]
        | [Piece.Secondary(_) as p, ...rest] => [
            p,
            ...drop_hole_members(prev_tok, rest),
          ]
        | [p, ...rest] => [p, ...drop_hole_members(Some(p), rest)]
        };
      let rec go = (prev_tok: option(Piece.t), ps: list(Piece.t)) =>
        switch (ps) {
        | [] => []
        | [p, ...rest] when is_semi(p) =>
          let dangling =
            switch (prev_tok, next_tok(rest)) {
            | (None, _) => true /* leading */
            | (_, None) => true /* trailing */
            | (_, Some(r)) => is_semi(r) /* doubled */
            };
          dangling && !unchanged(prev_tok, p, rest)
            ? go(prev_tok, rest) : [p, ...go(Some(p), rest)];
        | [Piece.Secondary(_) as p, ...rest] => [p, ...go(prev_tok, rest)]
        | [p, ...rest] => [p, ...go(Some(p), rest)]
        };
      /* Canonical `x;` — drop space runs that sit directly before a
         member separator (deletes leave one behind). */
      let rec trim_space_before_semi = (prev, ps: list(Piece.t)) =>
        switch (ps) {
        | [] => []
        | [p, ...rest] when is_space(p) =>
          let rec upcoming = qs =>
            switch (qs) {
            | [q, ...more] when is_space(q) => upcoming(more)
            | [q, ..._] when is_semi(q) => true
            | _ => false
            };
          upcoming(rest) && !unchanged(prev, p, rest)
            ? trim_space_before_semi(prev, rest)
            : [p, ...trim_space_before_semi(prev, rest)];
        | [Piece.Secondary(_) as p, ...rest] => [
            p,
            ...trim_space_before_semi(prev, rest),
          ]
        | [p, ...rest] => [p, ...trim_space_before_semi(Some(p), rest)]
        };
      seg
      |> drop_hole_members(None, _)
      |> go(None, _)
      |> trim_space_before_semi(None, _);
    };
    let normalize_member_whitespace = (~before=[], seg: Segment.t): Segment.t =>
      normalize_runs(~before, ~module_body=true, seg);

    let normalize_module_bodies = (~before=[], seg: Segment.t): Segment.t => {
      let originals = EditIdentity.index(before);
      let rec walk = ps => {
        let next = List.map(piece, ps);
        Segment.ptr_eq(next, ps) ? ps : next;
      }
      and piece = (p: Piece.t) =>
        switch (Id.Map.find_opt(Piece.id(p), originals)) {
        | Some(old) when old === p || compare(old, p) == 0 => old
        | previous =>
          switch (p) {
          | Tile(t) =>
            let old_children =
              switch (previous) {
              | Some(Tile(old))
                  when List.length(old.children) == List.length(t.children) =>
                old.children
              | _ => List.map(_ => [], t.children)
              };
            let children =
              List.map2(
                (old, child) => {
                  let child = walk(child);
                  is_mod_body(t)
                    ? normalize_member_whitespace(
                        ~before=old,
                        clean_member_separators(~before=old, child),
                      )
                    : child;
                },
                old_children,
                t.children,
              );
            List.for_all2(
              (a, b) => Segment.ptr_eq(a, b),
              children,
              t.children,
            )
              ? p
              : Piece.Tile({
                  ...t,
                  children,
                });
          | _ => p
          }
        };
      walk(seg);
    };

    /* The edit's old program supplies boundary witnesses and sharing.
       Unchanged subtrees are skipped; only new joins receive formatting.
       Unchanged results avoid reconstruction; rebuilt results retain overlays. */
    let normalize_top_level = (~before=?, z: Zipper.t): Zipper.t => {
      let after = Zipper.unselect_and_zip(z);
      let old =
        Option.map(Zipper.unselect_and_zip, before)
        |> Option.value(~default=[]);
      let next =
        after
        |> normalize_top_level_whitespace(~before=old)
        |> normalize_module_bodies(~before=old)
        |> EditIdentity.restore(before == None ? after : old, _);
      Segment.ptr_eq(next, after)
        ? z
        : {
          ...Zipper.unzip(next),
          refractors: z.refractors,
        };
    };

    /* Every agent-supplied code string funnels through here. Strip
       per-line leading whitespace (models emit indented code; Hazel
       re-indents structurally on render), then parse to a segment and
       paste it. Safe: Hazel strings and comments are single-line, so
       no token can span a linebreak. */
    /* Text-to-segment parsing simulates typing (Insert.go per char with a
       full remold/regrout each), so cost is quadratic in chunk size:
       ~0.3s at 500 chars, ~0.8s at 1000, ~8s at 3700 (measured on the
       graph-livelit module). It runs on the UI thread, so an oversized
       chunk reads as a hung editor. Only menhir-refused chunks land here
       (the fast path is uncapped), so the cap trades a rare multi-second
       stall against refusing the edit outright; 3000 chars is a ~5s
       worst case. */
    let max_chunk_chars = 3000;
    /* Agent structural edits try the linear Menhir zip first (FastParse).
       Incomplete syntax falls back to the size-limited typing parser. */
    /* Edge whitespace (the insert flow's baked-in separator newlines)
       must survive the fast path's trim: re-attach it as Secondary. */
    let ws_secondaries = (ws: string): Segment.t =>
      ws
      |> String.to_seq
      |> Seq.filter_map(c =>
           switch (c) {
           | ' '
           | '\t' =>
             Some(
               Piece.Secondary({
                 id: Id.mk(),
                 content: Secondary.Whitespace(Token.space),
               }),
             )
           | '\n' =>
             Some(
               Piece.Secondary({
                 id: Id.mk(),
                 content: Secondary.Whitespace(Token.linebreak),
               }),
             )
           | _ => None
           }
         )
      |> List.of_seq;
    let edge_ws = (code: string): (string, string) => {
      let trimmed = String.trim(code);
      switch (Util.StringUtil.plain_search(trimmed, code, 0)) {
      | i when i >= 0 => (
          String.sub(code, 0, i),
          String.sub(
            code,
            i + String.length(trimmed),
            String.length(code) - i - String.length(trimmed),
          ),
        )
      | _ => ("", "")
      };
    };
    /* ~root: the sort the CODE is parsed at (Mod for module members, so
       their `;` is the member separator). ~splice_root: the sort of the
       program the result is spliced into — the whole zipper is remolded at
       that root after the splice, and a scratch program's root is Exp
       whatever the code's own root. Remolding the program at Mod re-derived
       every mold from the wrong root: case rules inside a spliced member
       came out with Any/Exp-sorted patterns (dungeon runs: `nth`). */
    /* one leading and/or one trailing `;` of a module-member chunk, with
       the whitespace between it and the member: (lead, core, trail) */
    let split_separators =
        (code: string): (option(string), string, option(string)) => {
      let is_ws = c => c == ' ' || c == '\t' || c == '\n' || c == '\r';
      let t = String.trim(code);
      let n = String.length(t);
      let (lead, t) =
        if (n > 0 && t.[0] == ';') {
          let rest = String.sub(t, 1, n - 1);
          let k = ref(0);
          while (k^ < String.length(rest) && is_ws(rest.[k^])) {
            incr(k);
          };
          (
            Some(String.sub(rest, 0, k^)),
            String.sub(rest, k^, String.length(rest) - k^),
          );
        } else {
          (None, t);
        };
      let n = String.length(t);
      let (trail, t) =
        if (n > 0 && t.[n - 1] == ';') {
          let rest = String.sub(t, 0, n - 1);
          let k = ref(String.length(rest));
          while (k^ > 0 && is_ws(rest.[k^ - 1])) {
            decr(k);
          };
          (
            Some(String.sub(rest, k^, String.length(rest) - k^)),
            String.sub(rest, 0, k^),
          );
        } else {
          (None, t);
        };
      (lead, t, trail);
    };
    /* Backup molds keep the parser total, so a reserved binder no
       longer guarantees parse failure; the rejection can't key on
       to_segment returning None. Two-part gate: the text scan names
       the misuse (reserved word in binder position) AND the segment
       shows the word molded as a form-opener tile, not a variable.
       Completeness is no signal: the stray form can steal delimiters
       from the enclosing form. A reserved word inside a string
       literal never produces a tile. */
    let reserved_binder_garbage = (code: string, segment): option(string) =>
      switch (find_reserved_binder(code)) {
      | None => None
      | Some(w) =>
        let rec has_opener = (sg: Segment.t): bool =>
          sg
          |> List.exists((p: Piece.t) =>
               switch (p) {
               | Tile(t) =>
                 (
                   switch (Tile.label(t), t.shards) {
                   | ([tok, ..._], [0, ..._]) => tok == w
                   | _ => false
                   }
                 )
                 || List.exists(has_opener, t.children)
               | _ => false
               }
             );
        has_opener(segment) ? Some(w) : None;
      };

    let rec introduce =
            (
              ~root=Sort.Exp,
              ~splice_root=Sort.Exp,
              ~fast=false,
              ~keep_edge_ws=false,
              z: Zipper.t,
              code: string,
            )
            : result(Zipper.t, Action.Failure.t) => {
      let code = StringUtil.trim_leading(code) |> Unicode.nfc_outside_strings;
      /* module-member chunks carry their `;` separator (insert_member:
         `;\n` ++ m / m ++ `;\n`); the wrap parse cannot take a bare
         separator, so it is split off here and spliced back as a tile
         (molded at splice time like everything else) — else every
         member insert fell to the quadratic parser */
      let (lead_sep, core, trail_sep) =
        root == Sort.Mod ? split_separators(code) : (None, code, None);
      switch (
        fast
          ? PerfTimer.time("fast-parse", () =>
              FastParse.of_text(
                ~materialize=Triggers.invoked_projector,
                ~collect_refractors=false,
                ~root,
                String.trim(core),
              )
            )
          : None
      ) {
      | Some(segment) when reserved_binder_garbage(code, segment) != None =>
        Error(
          Action.Failure.Composition_action_failure(
            "Inserted code does not parse as intended."
            ++ reserved_word_note(code),
          ),
        )
      | Some(segment) =>
        /* Source tokens + formatting verbatim, molds from ExpToSegment +
           splice-time remold. No size cap needed on this path. */
        let sep_tile = (): Piece.t =>
          Tile({
            id: Id.mk(),
            form: Form.Compound(CellJoin),
            sort: Sort.Mod,
            shards: [0],
            children: [],
          });
        let segment =
          switch (lead_sep) {
          | Some(ws) => [sep_tile(), ...ws_secondaries(ws)] @ segment
          | None => segment
          };
        let segment =
          switch (trail_sep) {
          | Some(ws) => segment @ ws_secondaries(ws) @ [sep_tile()]
          | None => segment
          };
        let segment =
          if (keep_edge_ws) {
            let (lead, trail) = edge_ws(code);
            ws_secondaries(lead) @ segment @ ws_secondaries(trail);
          } else {
            segment;
          };
        let z' =
          PerfTimer.time("splice", ()
            /* Keep a generated terminal hole after the inserted separator
               newline, matching the typing parser's fragment insertion. */
            =>
              Zipper.replace_selection(
                Right,
                pad_fusing_edges(
                  z,
                  EditIdentity.reuse(z.selection.content, segment),
                ),
                z,
              )
              |> Zipper.unselect
              |> Zipper.remold_regrout(
                   keep_edge_ws ? Left : Right,
                   ~root=splice_root,
                 )
            );
        Ok(z');
      | None =>
        if (fast) {
          /* console-visible fallback telemetry (dev): which construct
             pushed us onto the quadratic path, and roughly how bad */
          print_endline(
            "FastParse fallback ("
            ++ string_of_int(String.length(code))
            ++ " chars): "
            ++ Option.value(FastParse.bail_note^, ~default="no note"),
          );
        };
        introduce_slow(~root, ~splice_root, z, code);
      };
    }
    and introduce_slow =
        (~root, ~splice_root=Sort.Exp, z: Zipper.t, code: string)
        : result(Zipper.t, Action.Failure.t) =>
      if (String.length(code) > max_chunk_chars) {
        Error(
          Action.Failure.Composition_action_failure(
            "Code chunk too large for error recovery ("
            ++ string_of_int(String.length(code))
            ++ " chars; limit "
            ++ string_of_int(max_chunk_chars)
            ++ "): it failed the batch parse, and chunks this size stall the editor in the recovering parser."
            ++ parse_hint()
            ++ " Fix the syntax if the code was meant to be complete, or split the edit: insert a skeleton whose complex parts are holes (?), then fill each part with its own update_definition call — nested paths (\"f/helper\") and module member paths (\"M/view\") address the parts directly.",
          ),
        );
      } else {
        switch (
          PerfTimer.time("typing-parse", () => Parser.to_segment(code, ~root))
        ) {
        | Some(segment) when reserved_binder_garbage(code, segment) != None =>
          Error(
            Action.Failure.Composition_action_failure(
              "Inserted code does not parse as intended."
              ++ reserved_word_note(code),
            ),
          )
        | Some(segment) =>
          Ok(
            PerfTimer.time("splice", () =>
              Zipper.insert_segment(
                z,
                pad_fusing_edges(
                  z,
                  EditIdentity.reuse(z.selection.content, segment),
                ),
                ~root=splice_root,
              )
            ),
          )
        | None =>
          Error(
            Action.Failure.Composition_action_failure(
              "Inserted code failed to parse."
              ++ reserved_word_note(code)
              ++ parse_hint(),
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
        switch (Destruct.go(Local(Left, ByChar), z', ~root=Exp)) {
        | None => Error(Action.Failure.Cant_destruct)
        | Some(z'') => Ok(z'')
        }
      | None => Error(Action.Failure.Cant_select)
      };
    };

    let overwrite_term =
        (
          ~root=Sort.Exp,
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
        introduce(~root, ~fast=true, z', code)
      | None => Error(Action.Failure.Cant_select)
      };
    };
    /* Insert a new module member adjacent to an existing one. Collapse the
       member's span to the near edge, then let the separator anchor the
       splice: after → ";\n" ++ code (the member's original following `;` —
       or `}` for the last member — ends the new code), before → code ++
       ";\n". `introduce` trims leading whitespace, so the `;` must lead. */
    /* Member boundaries: `;` at bracket depth 0, outside strings and
       single-line #...# comments. */
    let split_members = (code: string): list(string) => {
      let n = String.length(code);
      let parts = ref([])
      and start = ref(0)
      and depth = ref(0)
      and in_str = ref(false)
      and in_comment = ref(false);
      let i = ref(0);
      while (i^ < n) {
        let c = code.[i^];
        if (in_comment^) {
          if (c == '#' || c == '\n') {
            in_comment := false;
          };
        } else if (in_str^) {
          if (c == '\\') {
            incr(i);
          } else if (c == '"') {
            in_str := false;
          };
        } else {
          switch (c) {
          | '#' => in_comment := true
          | '"' => in_str := true
          | '('
          | '['
          | '{' => incr(depth)
          | ')'
          | ']'
          | '}' => decr(depth)
          | ';' when depth^ == 0 =>
            parts := [String.sub(code, start^, i^ - start^), ...parts^];
            start := i^ + 1;
          | _ => ()
          };
        };
        incr(i);
      };
      let last = String.sub(code, start^, n - start^);
      List.rev([last, ...parts^])
      |> List.map(String.trim)
      |> List.filter(m => m != "");
    };

    let insert_member =
        (
          ~root=Sort.Mod,
          ~fast=true,
          z: Zipper.t,
          target_id: Id.t,
          code: string,
          d: Direction.t,
          syntax: CachedSyntax.t,
        ) => {
      switch (
        Select.term(
          ~defs_exclude_bodies=true,
          ~case_rules=false,
          syntax.term_data,
          target_id,
          z,
        )
      ) {
      | None => Error(Action.Failure.Cant_select)
      | Some(z_sel) =>
        let z_caret = Zipper.directional_unselect(d, z_sel);
        /* Mod root so the member `;` molds as the member separator, not
           the Exp sequence operator. */
        /* one member at a time: a chunk of several members parsed together
           at Mod root tripped the incremental molding of the later members
           (their case-rule patterns came out Exp — dungeon runs, `nth`);
           each member alone parses reliably, and the caret lands after each
           splice exactly where the next one goes */
        let members =
          switch (split_members(code)) {
          | [] => [code]
          | ms => ms
          };
        List.fold_left(
          (acc, m) =>
            switch (acc) {
            | Error(e) => Error(e)
            | Ok(z) =>
              introduce(
                ~root,
                ~fast,
                ~keep_edge_ws=true,
                z,
                d == Left ? m ++ ";\n" : ";\n" ++ m,
              )
            },
          Ok(z_caret),
          members,
        );
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
        | Some(z'') => introduce(~fast=true, ~keep_edge_ws=true, z'', code)
        | None => Error(Action.Failure.Cant_move)
        }
      | None => Error(Action.Failure.Cant_select)
      };
    };
  };

  /* TODO: stubbed — see merge brief. Module-member edits at "M/x" paths
     (insert_before / insert_after, update_binding_clause,
     delete_binding_clause) are implemented twice: dev #2427's zipper path
     (PerformUtils.insert_member; overwrite_term(~root=Mod); destruct plus
     the member-separator cleanup in normalize_top_level) and this
     branch's term-level path (TermEdit.module_insert /
     module_update_binding / module_delete). false = dev's path, true =
     this branch's. Keep one and delete the other. */
  let module_members_via_term_edit = false;

  /* Shared by Insert(Before|After): sequence/module targets edit at term
     level via TermEdit; plain bindings go through the zipper-level paste
     funnel. Parse errors and reserved-keyword misuse are hard failures;
     other new static errors warn (multi-step refactoring). */
  let insert_relative =
      (
        ~d: Direction.t,
        ~path: string,
        ~code: string,
        ~initial_z: Zipper.t,
        ~initial_node_map: node_map,
        ~initial_info_map: Id.Map.t(Info.t),
        ~syntax: CachedSyntax.t,
        ~mk_statics: Zipper.t => StaticsBase.Map.t,
      )
      : result((Zipper.t, option(string)), Action.Failure.t) => {
    let target_id = path_to_id(initial_node_map, path);
    /* Static gate only: an incomplete pasted binding legitimately parks its
       closing delimiter in the backpack until [[Dump.to_zipper]], so a parse
       check here would false-positive. Reserved-keyword misuse can't be
       fixed by follow-up edits, so it stays a hard failure; other new
       static errors warn. */
    let finish = (new_z: Zipper.t) => {
      let new_info_map = mk_statics(new_z);
      let old_errors =
        PerfTimer.time("errors", () => ErrorPrint.all(initial_info_map));
      let new_errors =
        PerfTimer.time("errors", () => ErrorPrint.all(new_info_map));
      if (List.length(new_errors) <= List.length(old_errors)) {
        Ok((new_z, None));
      } else if (PerformUtils.reserved_word_note(code) != "") {
        Error(
          Action.Failure.Composition_action_failure(
            "Not applying the action you requested as it would introduce new static error(s): "
            ++ String.concat(", ", new_errors)
            ++ PerformUtils.reserved_word_note(code),
          ),
        );
      } else {
        Ok((
          new_z,
          Some(
            "Warning: this edit introduced new static error(s): "
            ++ String.concat(", ", new_errors)
            ++ ". Use get_statics to investigate.",
          ),
        ));
      };
    };
    let side = d == Direction.Left ? "before" : "after";
    /* Term-level rebuilds carry no backpack, so a parse check is sound
       there and catches invalid arm/element code. */
    let term_edit = (result, kind) =>
      switch (result) {
      | Some(new_z) =>
        switch (PerformUtils.parse_error_check(new_z)) {
        | Some(parse_err) =>
          Error(
            Action.Failure.Composition_action_failure(
              parse_err ++ PerformUtils.reserved_word_note(code),
            ),
          )
        | None => finish(new_z)
        }
      | None =>
        Error(
          Action.Failure.Composition_action_failure(
            "Failed to insert "
            ++ kind
            ++ " "
            ++ side
            ++ " \""
            ++ path
            ++ "\": could not parse \""
            ++ code
            ++ "\" as valid code.",
          ),
        )
      };
    if (TermEdit.is_case_arm(initial_z, target_id)) {
      term_edit(
        TermEdit.case_insert_arm(initial_z, target_id, code, d),
        "case arm",
      );
    } else if (TermEdit.is_list_element(initial_z, target_id)) {
      term_edit(
        TermEdit.list_insert_element(initial_z, target_id, code, d),
        "list element",
      );
    } else if (TermEdit.is_tuple_element(initial_z, target_id)) {
      term_edit(
        TermEdit.tuple_insert_element(initial_z, target_id, code, d),
        "tuple element",
      );
    } else if (!module_members_via_term_edit
               && Utils.is_module_member(path_to_node(initial_node_map, path))) {
      switch (
        PerformUtils.insert_member(initial_z, target_id, code, d, syntax)
      ) {
      | Error(e) => Error(e)
      | Ok(new_z) => finish(new_z)
      };
    } else if (TermEdit.is_module_item(initial_z, target_id)) {
      term_edit(
        TermEdit.module_insert(initial_z, target_id, code, d),
        "module item",
      );
    } else {
      switch (
        PerformUtils.insert_term(
          initial_z,
          target_id,
          "\n" ++ code ++ "\n",
          d,
          syntax,
        )
      ) {
      | Error(e) => Error(e)
      | Ok(new_z) =>
        if (Zipper.local_missing_shards(new_z) != []) {
          /* Incomplete binding (e.g. `let b = 2` without `in`): the pasted
             form parks its closer in the backpack and Dump can misplace it.
             Prefer the term-level insert, which completes the form; keep
             the paste result if TermEdit can't do better. */
          switch (TermEdit.insert_binding(initial_z, target_id, code, d)) {
          | Some(z_te) when PerformUtils.parse_error_check(z_te) == None =>
            finish(z_te)
          | _ => finish(new_z)
          };
        } else {
          finish(new_z);
        }
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
      )
      : result((Zipper.t, option(string)), Action.Failure.t) => {
    switch (e) {
    | Update(Definition, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(Def, initial_node);
      /* TyAlias definitions are types, not expressions: go through
         update_type_annotation (parse_typ + replace_typ_by_id). */
      let is_type_alias =
        switch (initial_node.info) {
        | InfoExp({user_term: term, _}) =>
          switch (Exp.term_of(term)) {
          | TyAlias(_, _, _) => true
          | _ => false
          }
        | _ => false
        };
      if (is_type_alias) {
        switch (TermEdit.update_type_annotation(initial_z, target_id, code)) {
        | Some(new_z) =>
          PerformUtils.validate_edit(
            ~edit_action=e,
            ~initial_node=Some(initial_node),
            ~initial_info_map,
            ~new_z,
            ~mk_statics,
            ~code,
          )
        | None =>
          Error(
            Action.Failure.Composition_action_failure(
              "Failed to update type alias definition: could not parse \""
              ++ code
              ++ "\" as valid code.",
            ),
          )
        };
      } else {
        switch (
          PerfTimer.time("overwrite", () =>
            PerformUtils.overwrite_term(
              initial_z,
              target_id,
              code,
              false,
              syntax,
            )
          )
        ) {
        | Error(e) => Error(e)
        | Ok(new_z) =>
          switch (
            PerformUtils.validate_edit_full(
              ~edit_action=e,
              ~initial_node=Some(initial_node),
              ~initial_info_map,
              ~new_z,
              ~mk_statics,
              ~code,
            )
          ) {
          | Error(e) => Error(e)
          | Ok((new_z, _, new_node_map, warning)) =>
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
            Ok((z_after_projectors, warning));
          }
        };
      };
    | Update(Body, path, code)
        when Utils.is_module_member(path_to_node(initial_node_map, path)) =>
      ignore(code);
      Utils.member_body_error("update the body");
    | Update(Body, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = path_to_id(initial_node_map, path);
      /* Case arms and list/tuple elements: the path points directly at the
         element; edit at term level. Otherwise use the binding's body. */
      if (TermEdit.is_sequence_element(initial_z, target_id)) {
        let (term_edit_result, kind) =
          if (TermEdit.is_case_arm(initial_z, target_id)) {
            (
              TermEdit.case_update_arm_body(initial_z, target_id, code),
              "case arm body",
            );
          } else if (TermEdit.is_list_element(initial_z, target_id)) {
            (
              TermEdit.list_update_element(initial_z, target_id, code),
              "list element",
            );
          } else {
            (
              TermEdit.tuple_update_element(initial_z, target_id, code),
              "tuple element",
            );
          };
        switch (term_edit_result) {
        | Some(new_z) =>
          PerformUtils.validate_edit(
            ~edit_action=e,
            ~initial_node=Some(initial_node),
            ~initial_info_map,
            ~new_z,
            ~mk_statics,
            ~code,
          )
        | None =>
          Error(
            Action.Failure.Composition_action_failure(
              "Failed to update "
              ++ kind
              ++ ": could not parse \""
              ++ code
              ++ "\" as valid code.",
            ),
          )
        };
      } else {
        let target_id = Utils.get_inner_term_id(Body, initial_node);
        switch (
          PerfTimer.time("overwrite", () =>
            PerformUtils.overwrite_term(
              initial_z,
              target_id,
              code,
              false,
              syntax,
            )
          )
        ) {
        | Error(e) => Error(e)
        | Ok(new_z) =>
          PerformUtils.validate_edit(
            ~edit_action=e,
            ~initial_node=Some(initial_node),
            ~initial_info_map,
            ~new_z,
            ~mk_statics,
            ~code,
          )
        };
      };
    | Update(Pattern, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = path_to_id(initial_node_map, path);
      /* Case arms: term-level pattern update. List/tuple elements: Pattern
         update doesn't apply. Otherwise dev's rename-validating path. */
      if (TermEdit.is_list_element(initial_z, target_id)) {
        Error(
          Action.Failure.Composition_action_failure(
            "Update(Pattern) is not applicable to list elements. "
            ++ "Use Update(Body, ...) to replace the element value.",
          ),
        );
      } else if (TermEdit.is_tuple_element(initial_z, target_id)) {
        Error(
          Action.Failure.Composition_action_failure(
            "Update(Pattern) is not applicable to tuple elements. "
            ++ "Use Update(Body, ...) to replace the element value.",
          ),
        );
      } else if (TermEdit.is_case_arm(initial_z, target_id)) {
        switch (TermEdit.case_update_arm_pattern(initial_z, target_id, code)) {
        | Some(new_z) =>
          PerformUtils.validate_edit(
            ~edit_action=e,
            ~initial_node=Some(initial_node),
            ~initial_info_map,
            ~new_z,
            ~mk_statics,
            ~code,
          )
        | None =>
          Error(
            Action.Failure.Composition_action_failure(
              "Failed to update case arm pattern: could not parse \""
              ++ code
              ++ "\" as a valid pattern.",
            ),
          )
        };
      } else {
        let target_id = Utils.get_inner_term_id(Pat, initial_node);
        let old_pat =
          StaticsBase.Map.lookup(target_id, initial_info_map)
          |> OptUtil.get_or_fail(
               "Failed trying to rename all occurences of the pattern. Could not find the old pattern in the statics map.",
             );
        switch (
          PerfTimer.time("overwrite", () =>
            PerformUtils.overwrite_term(
              initial_z,
              target_id,
              code,
              false,
              syntax,
            )
          )
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
                  e
                  ++ PerformUtils.reserved_word_note(code)
                  ++ PerformUtils.parse_hint(),
                ),
              )
            | None =>
              let new_target_id = Utils.get_inner_term_id(Pat, new_node);
              let new_pat =
                StaticsBase.Map.lookup(new_target_id, new_info_map)
                |> OptUtil.get_or_fail(
                     "Failed trying to rename all occurences of the pattern. Could not find the new pattern in the statics map.",
                   );
              let old_names =
                GeneralTreeUtils.get_var_names_from_pat(old_pat);
              let new_names =
                GeneralTreeUtils.get_var_names_from_pat(new_pat);
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
                  let initial_errors =
                    PerfTimer.time("errors", () =>
                      ErrorPrint.all(initial_info_map)
                    );
                  let final_errors =
                    PerfTimer.time("errors", () =>
                      ErrorPrint.all(mk_statics(final_z))
                    );
                  if (List.length(final_errors) > List.length(initial_errors)) {
                    Error(
                      Action.Failure.Composition_action_failure(
                        "Not applying the rename: rewriting the use sites would introduce new static error(s): "
                        ++ String.concat(", ", final_errors),
                      ),
                    );
                  } else {
                    Ok((final_z, None));
                  };
                };
              };
            };
          };
        };
      };
    | Update(BindingClause, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = path_to_id(initial_node_map, path);
      /* Case arms, list elements, and tuple elements don't have
         "binding clauses" — use Update(Body) for those. */
      if (TermEdit.is_case_arm(initial_z, target_id)) {
        Error(
          Action.Failure.Composition_action_failure(
            "Update(BindingClause) is not applicable to case arms. "
            ++ "Use Update(Body, ...) or Update(Pattern, ...) instead.",
          ),
        );
      } else if (TermEdit.is_list_element(initial_z, target_id)
                 || TermEdit.is_tuple_element(initial_z, target_id)) {
        Error(
          Action.Failure.Composition_action_failure(
            "Update(BindingClause) is not applicable to list/tuple elements. "
            ++ "Use Update(Body, ...) to replace the element value.",
          ),
        );
      } else if (!module_members_via_term_edit
                 && Utils.is_module_member(initial_node)) {
        /* a member's clause parses at Mod root, so its `;` is the member
           separator */
        switch (
          PerformUtils.overwrite_term(
            ~root=Sort.Mod,
            initial_z,
            target_id,
            code,
            true,
            syntax,
          )
        ) {
        | Error(e) => Error(e)
        | Ok(new_z) =>
          PerformUtils.validate_edit(
            ~edit_action=e,
            ~initial_node=Some(initial_node),
            ~initial_info_map,
            ~new_z,
            ~mk_statics,
            ~code,
          )
        };
      } else if (TermEdit.is_module_item(initial_z, target_id)) {
        switch (TermEdit.module_update_binding(initial_z, target_id, code)) {
        | Some(new_z) =>
          PerformUtils.validate_edit(
            ~edit_action=e,
            ~initial_node=Some(initial_node),
            ~initial_info_map,
            ~new_z,
            ~mk_statics,
            ~code,
          )
        | None =>
          Error(
            Action.Failure.Composition_action_failure(
              "Failed to update module item: could not parse \""
              ++ code
              ++ "\" as a valid binding.",
            ),
          )
        };
      } else {
        switch (
          PerformUtils.overwrite_term(
            initial_z,
            target_id,
            code,
            true,
            syntax,
          )
        ) {
        | Error(e) => Error(e)
        | Ok(new_z) =>
          PerformUtils.validate_edit(
            ~edit_action=e,
            ~initial_node=Some(initial_node),
            ~initial_info_map,
            ~new_z,
            ~mk_statics,
            ~code,
          )
        };
      };
    | Update(TypeAnnotation, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(TypeAnn, initial_node);
      switch (TermEdit.update_type_annotation(initial_z, target_id, code)) {
      | Some(new_z) =>
        PerformUtils.validate_edit(
          ~edit_action=e,
          ~initial_node=Some(initial_node),
          ~initial_info_map,
          ~new_z,
          ~mk_statics,
          ~code,
        )
      | None =>
        Error(
          Action.Failure.Composition_action_failure(
            "Failed to update type annotation: could not parse \""
            ++ code
            ++ "\" as a valid type.",
          ),
        )
      };
    | Insert(Before, path, code) =>
      insert_relative(
        ~d=Direction.Left,
        ~path,
        ~code,
        ~initial_z,
        ~initial_node_map,
        ~initial_info_map,
        ~syntax,
        ~mk_statics,
      )
    | Insert(After, path, code) =>
      insert_relative(
        ~d=Direction.Right,
        ~path,
        ~code,
        ~initial_z,
        ~initial_node_map,
        ~initial_info_map,
        ~syntax,
        ~mk_statics,
      )
    | Delete(BindingClause, path)
        when
          !module_members_via_term_edit
          && Utils.is_module_member(path_to_node(initial_node_map, path)) =>
      /* destruct leaves a hole in the member's slot; the member-separator
         cleanup in normalize_top_level drops it with its `;` */
      PerformUtils.destruct(
        ~defs_exclude_bodies=true,
        initial_z,
        path_to_id(initial_node_map, path),
        syntax,
      )
      |> Result.map(z => (z, None));
    | Delete(BindingClause, path) =>
      let target_id = path_to_id(initial_node_map, path);
      let (term_edit_result, kind) =
        if (TermEdit.is_case_arm(initial_z, target_id)) {
          (TermEdit.case_delete_arm(initial_z, target_id), "case arm");
        } else if (TermEdit.is_list_element(initial_z, target_id)) {
          (
            TermEdit.list_delete_element(initial_z, target_id),
            "list element",
          );
        } else if (TermEdit.is_tuple_element(initial_z, target_id)) {
          (
            TermEdit.tuple_delete_element(initial_z, target_id),
            "tuple element",
          );
        } else if (TermEdit.is_module_item(initial_z, target_id)) {
          (TermEdit.module_delete(initial_z, target_id), "module item");
        } else {
          (TermEdit.delete_binding(initial_z, target_id), "binding");
        };
      switch (term_edit_result) {
      | Some(new_z) => Ok((new_z, None))
      | None =>
        Error(
          Action.Failure.Composition_action_failure(
            "Failed to delete "
            ++ kind
            ++ " at \""
            ++ path
            ++ "\": the element could not be found in the term tree.",
          ),
        )
      };
    | Delete(Body, path)
        when Utils.is_module_member(path_to_node(initial_node_map, path)) =>
      Utils.member_body_error("delete the body")
    | Delete(Body, path) =>
      let node = path_to_node(initial_node_map, path);
      let target_id = Utils.get_inner_term_id(Body, node);
      switch (TermEdit.delete_body(initial_z, target_id)) {
      | Some(new_z) => Ok((new_z, None))
      | None =>
        Error(
          Action.Failure.Composition_action_failure(
            "Failed to delete body at \""
            ++ path
            ++ "\": the body expression could not be found in the term tree.",
          ),
        )
      };
    | Delete(Definition | Pattern | TypeAnnotation, _) =>
      Error(
        Action.Failure.Composition_action_failure(
          "Deleting a definition, pattern, or type annotation is not yet implemented.",
        ),
      )

    /* --- Selector-driven edits --- */

    | SelectorUpdate(selector, code) =>
      let term = MakeTerm.from_zip_for_sem(initial_z, ~root=Exp).term;
      switch (Selector.query_unique(selector, term)) {
      | Error(msg) => Error(Action.Failure.Composition_action_failure(msg))
      | Ok({focused, focused_id, _}) =>
        let parse_err = msg =>
          Error(
            Action.Failure.Composition_action_failure(
              "Failed to parse replacement code: " ++ msg,
            ),
          );
        switch (focused) {
        | FocusExp(_) =>
          switch (TermEdit.parse_exp(code)) {
          | None => parse_err(code)
          | Some(new_exp) =>
            let new_term =
              TermEdit.replace_exp_by_id(focused_id, _ => new_exp, term);
            let new_z = TermEdit.term_to_zipper(new_term);
            Ok((new_z, None));
          }
        | FocusPat(_) =>
          switch (TermEdit.parse_pat(code)) {
          | None => parse_err(code ++ " (as pattern)")
          | Some(new_pat) =>
            let new_term =
              TermEdit.replace_pat_by_id(focused_id, new_pat, term);
            let new_z = TermEdit.term_to_zipper(new_term);
            Ok((new_z, None));
          }
        | FocusTyp(_) =>
          switch (TermEdit.parse_typ(code)) {
          | None => parse_err(code ++ " (as type)")
          | Some(new_typ) =>
            let new_term =
              TermEdit.replace_typ_by_id(focused_id, new_typ, term);
            let new_z = TermEdit.term_to_zipper(new_term);
            Ok((new_z, None));
          }
        | FocusMod(_) =>
          switch (TermEdit.find_module_containing_item(focused_id, term)) {
          | None =>
            Error(
              Action.Failure.Composition_action_failure(
                "Module item not found for id",
              ),
            )
          | Some((module_id, items, idx)) =>
            switch (TermEdit.exp_to_mod_item(code)) {
            | None => parse_err(code ++ " (as module item)")
            | Some(new_item) =>
              let new_items = TermEdit.replace_item(items, idx, new_item);
              let new_term =
                TermEdit.replace_module_items(module_id, new_items, term);
              let new_z = TermEdit.term_to_zipper(new_term);
              Ok((new_z, None));
            }
          }
        };
      };

    | SelectorDelete(selector) =>
      let term = MakeTerm.from_zip_for_sem(initial_z, ~root=Exp).term;
      switch (Selector.query_unique(selector, term)) {
      | Error(msg) => Error(Action.Failure.Composition_action_failure(msg))
      | Ok({focused, focused_id, _}) =>
        let new_term =
          switch (focused) {
          | FocusExp(_) =>
            let hole = Exp.fresh(EmptyHole);
            TermEdit.replace_exp_by_id(focused_id, _ => hole, term);
          | FocusMod(_) =>
            switch (TermEdit.find_module_containing_item(focused_id, term)) {
            | None => term
            | Some((module_id, items, idx)) =>
              let new_items = TermEdit.delete_item(items, idx);
              TermEdit.replace_module_items(module_id, new_items, term);
            }
          | FocusPat(_) =>
            let hole = Pat.fresh(EmptyHole);
            TermEdit.replace_pat_by_id(focused_id, hole, term);
          | FocusTyp(_) =>
            let hole = Typ.fresh(Unknown(Hole(EmptyHole)));
            TermEdit.replace_typ_by_id(focused_id, hole, term);
          };
        let new_z = TermEdit.term_to_zipper(new_term);
        Ok((new_z, None));
      };

    | SelectorInsertBefore(selector, code)
    | SelectorInsertAfter(selector, code) =>
      let dir =
        switch (e) {
        | SelectorInsertBefore(_, _) => Direction.Left
        | _ => Direction.Right
        };
      let term = MakeTerm.from_zip_for_sem(initial_z, ~root=Exp).term;
      switch (Selector.query_unique(selector, term)) {
      | Error(msg) => Error(Action.Failure.Composition_action_failure(msg))
      | Ok({focused_id, _}) =>
        let (term_edit_result, kind) =
          if (TermEdit.is_case_arm(initial_z, focused_id)) {
            (
              TermEdit.case_insert_arm(initial_z, focused_id, code, dir),
              "case arm",
            );
          } else if (TermEdit.is_list_element(initial_z, focused_id)) {
            (
              TermEdit.list_insert_element(initial_z, focused_id, code, dir),
              "list element",
            );
          } else if (TermEdit.is_tuple_element(initial_z, focused_id)) {
            (
              TermEdit.tuple_insert_element(initial_z, focused_id, code, dir),
              "tuple element",
            );
          } else {
            /* Check if focused_id is inside a module item (directly or
               as a sub-expression like the definition). If so, use
               module_insert with the item's ID. */
            switch (TermEdit.find_module_item_id(initial_z, focused_id)) {
            | Some(item_id) => (
                TermEdit.module_insert(initial_z, item_id, code, dir),
                "module item",
              )
            | None => (
                TermEdit.insert_binding(initial_z, focused_id, code, dir),
                "binding",
              )
            };
          };
        switch (term_edit_result) {
        | Some(new_z) =>
          switch (PerformUtils.parse_error_check(new_z)) {
          | Some(parse_err) =>
            Error(Action.Failure.Composition_action_failure(parse_err))
          | None => Ok((new_z, None))
          }
        | None =>
          Error(
            Action.Failure.Composition_action_failure(
              "Failed to insert "
              ++ kind
              ++ " via selector \""
              ++ selector
              ++ "\": could not parse \""
              ++ code
              ++ "\" as valid code.",
            ),
          )
        };
      };
    };
  };

  let format_typ = (ty: Typ.t): string => ErrorPrint.Print.typ(ty);

  /* TODO: stubbed — see merge brief.
     The dev branch removed the `Info.status_exp` ADT in favor of a unified
     `marks: list(Mark.t)` field on `Info.exp`. This formatter previously
     pattern-matched on NotInHole/InHole/Consistent/AnaDeferralConsistent
     to produce a human-readable status string. The closest replacement is
     ErrorPrint.string_of_marks, which renders marks but does not surface
     the synthesized/expected type the way the old version did. Reproduces
     a partial status string here (synth type only) and lists marks if any.
     The selector and path-based read-dispatch handlers below depend on
     this; they may produce less detailed output than before. */
  let format_status_exp = (info: Info.exp): string => {
    let ty_str = "Synthesized type: " ++ format_typ(info.ty);
    switch (info.marks) {
    | [] => ty_str
    | marks =>
      ty_str
      ++ "\nStatus: error\nError: "
      ++ ErrorPrint.string_of_marks(InfoExp(info), marks)
    };
  };

  let read_dispatch =
      (
        ~action: CompositionActions.read_action,
        ~z: Zipper.t,
        ~info_map: Id.Map.t(Info.t),
        ~syntax: CachedSyntax.t,
      )
      : result(string, Action.Failure.t) => {
    /* Select uses the selector language directly on the term tree,
       bypassing the HighLevelNodeMap path system */
    switch (action) {
    | Select(selector_str) =>
      let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
      switch (Selector.query(selector_str, term)) {
      | [] =>
        Error(
          Composition_action_failure(
            "No match for selector: " ++ selector_str,
          ),
        )
      | matches =>
        let results = matches |> List.map(Selector.print_match);
        Ok(String.concat("\n", results));
      };
    | GetCanonical(selector_str) =>
      let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
      switch (Selector.query_unique(selector_str, term)) {
      | Error(e) =>
        Error(Composition_action_failure("Selector error: " ++ e))
      | Ok(m) =>
        let id = m.focused_id;
        let num =
          switch (Selector.canonical_numeric(id, term)) {
          | Some(path) => Selector.deparse(path)
          | None => "(not found)"
          };
        let named =
          switch (Selector.canonical_named(id, term)) {
          | Some(path) => Selector.deparse(path)
          | None => "(not found)"
          };
        Ok("numeric: " ++ num ++ "\nnamed: " ++ named);
      };
    | GetCompleteness =>
      let (exp_holes, pat_holes, typ_holes) = PerformUtils.count_holes(z);
      let total = exp_holes + pat_holes + typ_holes;
      if (total == 0) {
        Ok("Complete: no unfilled holes.");
      } else {
        let parts = ref([]);
        if (typ_holes > 0) {
          parts := [string_of_int(typ_holes) ++ " type", ...parts^];
        };
        if (pat_holes > 0) {
          parts := [string_of_int(pat_holes) ++ " pattern", ...parts^];
        };
        if (exp_holes > 0) {
          parts := [string_of_int(exp_holes) ++ " expression", ...parts^];
        };
        Ok(
          "Incomplete: "
          ++ string_of_int(total)
          ++ " unfilled hole(s) ("
          ++ String.concat(", ", parts^)
          ++ ").",
        );
      };
    | SelectorGetStatics(selector_str) =>
      let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
      switch (Selector.query_unique(selector_str, term)) {
      | Error(e) =>
        Error(Composition_action_failure("Selector error: " ++ e))
      | Ok(m) =>
        let id = m.focused_id;
        switch (Id.Map.find_opt(id, info_map)) {
        | None =>
          Error(
            Composition_action_failure(
              "No statics info for selector-resolved node",
            ),
          )
        | Some(info) =>
          /* TODO: stubbed — see merge brief. Was previously decomposing
             the `status` field of Info.exp/pat which has been removed in
             favor of `marks: list(Mark.t)`. */
          let result =
            switch (info) {
            | InfoExp(exp_info) =>
              "Selector: "
              ++ selector_str
              ++ "\nAnalytic (expected) type: "
              ++ format_typ(exp_info.ana)
              ++ "\n"
              ++ format_status_exp(exp_info)
            | InfoPat(pat_info) =>
              "Selector: "
              ++ selector_str
              ++ "\nAnalytic type: "
              ++ format_typ(pat_info.ana)
              ++ "\nStatus: "
              ++ (
                switch (pat_info.marks) {
                | [] => "ok"
                | marks =>
                  "error: "
                  ++ ErrorPrint.string_of_marks(InfoPat(pat_info), marks)
                }
              )
            | info =>
              "Selector: "
              ++ selector_str
              ++ "\nClass: "
              ++ Cls.show(Info.cls_of(info))
              ++ (
                Info.is_error(info)
                  ? "\nStatus: error: "
                    ++ ErrorPrint.string_of_marks(info, Info.marks_of(info))
                  : "\nStatus: ok"
              )
            };
          /* Also gather errors from the subtree */
          let subtree =
            GeneralTreeUtils.subtree_of(
              ~info,
              ~orig_info_map=info_map,
              ~of_pat=true,
              ~of_def=true,
              ~of_body=true,
            );
          let errors = ErrorPrint.all(subtree);
          let result =
            switch (errors) {
            | [] => result
            | _ =>
              result
              ++ "\nErrors in subtree:\n"
              ++ String.concat("\n", errors)
            };
          Ok(result);
        };
      };
    | SelectorGetContext(selector_str) =>
      let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
      switch (Selector.query_unique(selector_str, term)) {
      | Error(e) =>
        Error(Composition_action_failure("Selector error: " ++ e))
      | Ok(m) =>
        let id = m.focused_id;
        switch (Id.Map.find_opt(id, info_map)) {
        | None =>
          Error(
            Composition_action_failure(
              "No statics info for selector-resolved node",
            ),
          )
        | Some(info) =>
          let ctx = Info.ctx_of(info) |> Ctx.filter_shadowed;
          let vars =
            ctx.entries
            |> List.filter_map(entry =>
                 switch (entry) {
                 | Ctx.VarEntry(ve) => Some(ve)
                 | _ => None
                 }
               );
          let constructors =
            ctx.entries
            |> List.filter_map(entry =>
                 switch (entry) {
                 | Ctx.ConstructorEntry(ve) => Some(ve)
                 | _ => None
                 }
               );
          let type_aliases =
            ctx.entries
            |> List.filter_map(entry =>
                 switch (entry) {
                 | Ctx.TVarEntry(te) => Some(te)
                 | _ => None
                 }
               );
          let fmt_var = (ve: Ctx.var_entry) =>
            "  " ++ ve.name ++ " : " ++ format_typ(ve.typ);
          let fmt_tvar = (te: Ctx.tvar_entry) =>
            switch (te.kind) {
            | Singleton(ty) => "  " ++ te.name ++ " = " ++ format_typ(ty)
            | Abstract => "  " ++ te.name ++ " (abstract)"
            };
          let result = "Context at selector: " ++ selector_str;
          let result =
            switch (vars) {
            | [] => result
            | _ =>
              result
              ++ "\nVariables:\n"
              ++ String.concat("\n", List.map(fmt_var, vars))
            };
          let result =
            switch (type_aliases) {
            | [] => result
            | _ =>
              result
              ++ "\nType aliases:\n"
              ++ String.concat("\n", List.map(fmt_tvar, type_aliases))
            };
          let result =
            switch (constructors) {
            | [] => result
            | _ =>
              result
              ++ "\nConstructors:\n"
              ++ String.concat("\n", List.map(fmt_var, constructors))
            };
          Ok(result);
        };
      };
    | _ =>
      switch (build(z, info_map)) {
      | None => Error(Cant_derive_local_AST_information)
      | Some(node_map) =>
        switch (action) {
        | GetSyntax(path) =>
          let node = path_to_node(node_map, path);
          let target_id = Info.id_of(node.info);
          switch (segment_of_term(z, Some(target_id), syntax)) {
          | Some(segment) =>
            let code = Printer.of_segment(~holes="?", segment);
            Ok(code);
          | None =>
            Error(
              Composition_action_failure(
                "Could not select the term at path: " ++ path,
              ),
            )
          };
        | GetStatics(path) =>
          let node = path_to_node(node_map, path);
          /* TODO: stubbed — see merge brief. Status field replaced with marks. */
          let result =
            switch (node.info) {
            | InfoExp(exp_info) =>
              "Path: "
              ++ path
              ++ "\nBinding: "
              ++ node.name
              ++ "\nAnalytic (expected) type: "
              ++ format_typ(exp_info.ana)
              ++ "\n"
              ++ format_status_exp(exp_info)
            | InfoPat(pat_info) =>
              "Path: "
              ++ path
              ++ "\nBinding: "
              ++ node.name
              ++ "\nAnalytic type: "
              ++ format_typ(pat_info.ana)
              ++ "\nStatus: "
              ++ (
                switch (pat_info.marks) {
                | [] => "ok"
                | marks =>
                  "error: "
                  ++ ErrorPrint.string_of_marks(InfoPat(pat_info), marks)
                }
              )
            | info =>
              "Path: "
              ++ path
              ++ "\nBinding: "
              ++ node.name
              ++ "\nClass: "
              ++ Cls.show(Info.cls_of(info))
              ++ (
                Info.is_error(info)
                  ? "\nStatus: error: "
                    ++ ErrorPrint.string_of_marks(info, Info.marks_of(info))
                  : "\nStatus: ok"
              )
            };
          /* Also gather errors from the node's subtree */
          let subtree =
            GeneralTreeUtils.subtree_of(
              ~info=node.info,
              ~orig_info_map=info_map,
              ~of_pat=true,
              ~of_def=true,
              ~of_body=true,
            );
          let errors = ErrorPrint.all(subtree);
          let result =
            switch (errors) {
            | [] => result
            | _ =>
              result
              ++ "\nErrors in subtree:\n"
              ++ String.concat("\n", errors)
            };
          Ok(result);
        | GetContext(path) =>
          let node = path_to_node(node_map, path);
          let ctx = Info.ctx_of(node.info) |> Ctx.filter_shadowed;
          let vars =
            ctx.entries
            |> List.filter_map(entry =>
                 switch (entry) {
                 | Ctx.VarEntry(ve) => Some(ve)
                 | _ => None
                 }
               );
          let constructors =
            ctx.entries
            |> List.filter_map(entry =>
                 switch (entry) {
                 | Ctx.ConstructorEntry(ve) => Some(ve)
                 | _ => None
                 }
               );
          let type_aliases =
            ctx.entries
            |> List.filter_map(entry =>
                 switch (entry) {
                 | Ctx.TVarEntry(te) => Some(te)
                 | _ => None
                 }
               );
          let fmt_var = (ve: Ctx.var_entry) =>
            "  " ++ ve.name ++ " : " ++ format_typ(ve.typ);
          let fmt_tvar = (te: Ctx.tvar_entry) =>
            switch (te.kind) {
            | Singleton(ty) => "  " ++ te.name ++ " = " ++ format_typ(ty)
            | Abstract => "  " ++ te.name ++ " (abstract)"
            };
          let result = "Context at path: " ++ path;
          let result =
            switch (vars) {
            | [] => result
            | _ =>
              result
              ++ "\nVariables:\n"
              ++ String.concat("\n", List.map(fmt_var, vars))
            };
          let result =
            switch (type_aliases) {
            | [] => result
            | _ =>
              result
              ++ "\nType aliases:\n"
              ++ String.concat("\n", List.map(fmt_tvar, type_aliases))
            };
          let result =
            switch (constructors) {
            | [] => result
            | _ =>
              result
              ++ "\nConstructors:\n"
              ++ String.concat("\n", List.map(fmt_var, constructors))
            };
          Ok(result);
        | Select(_)
        | GetCanonical(_)
        | GetCompleteness
        | SelectorGetStatics(_)
        | SelectorGetContext(_) => assert(false) /* handled above */
        }
      }
    };
  };

  let composition_dispatch =
      (
        ~initial_info_map: option(StaticsBase.Map.t)=None,
        a: Action.Structural.t,
        syntax: CachedSyntax.t,
        z: Zipper.t,
        mk_statics: Zipper.t => StaticsBase.Map.t,
      )
      : result((Zipper.t, option(string)), Action.Failure.t) => {
    /* the editor's map for this very zipper when the caller has one
       (saves a full statics pass per tool); else compute */
    let initial_info_map =
      switch (initial_info_map) {
      | Some(m) => m
      | None => mk_statics(z)
      };
    /* Selector-driven edits bypass the HighLevelNodeMap entirely —
       they resolve against the term tree directly via Selector */
    switch (a) {
    | SelectorUpdate(_)
    | SelectorDelete(_)
    | SelectorInsertBefore(_)
    | SelectorInsertAfter(_) =>
      /* Pass an empty node map — selector edits don't use it */
      edit_dispatch(
        ~e=a,
        ~initial_z=z,
        ~initial_node_map=Id.Map.empty,
        ~initial_info_map,
        ~syntax,
        ~mk_statics,
      )
    | _ =>
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
      }
    };
  };

  let go =
      (
        ~mk_statics: Zipper.t => StaticsBase.Map.t,
        ~initial_info_map: option(StaticsBase.Map.t),
        ~syntax: CachedSyntax.t,
        ~z: Zipper.t,
        ~a: Action.Structural.t,
      )
      : result((Zipper.t, option(string)), Action.Failure.t) => {
    let res =
      try(
        switch (
          composition_dispatch(~initial_info_map, a, syntax, z, mk_statics)
        ) {
        | Ok((new_z, warning)) =>
          Ok((
            PerfTimer.time("normalize", () =>
              PerformUtils.normalize_top_level(
                ~before=z,
                Materialize.all(new_z, ~root=Exp),
              )
            ),
            warning,
          ))
        | Error(e) => Error(e)
        }
      ) {
      | Failure(e) =>
        /* an exception out of a structural action is OUR bug: dump the
           pre-action program and the action so it can be replayed in a test */
        Js_of_ocaml.(
          Firebug.console##error_3(
            Js.string("[structural action] Failure: " ++ e),
            Js.string(Action.Structural.show(a)),
            Js.string(Printer.of_zipper(~holes="?", z)),
          )
        );
        Error(Action.Failure.Composition_action_failure(e));
      };

    res;
  };
};

module Public = {
  let mk_statics_with = (settings: Language.CoreSettings.t, z: Zipper.t) =>
    Language.(
      fst(
        Statics.mk(
          settings,
          Builtins.ctx_init(Some(Operators.default_mode)),
          MakeTerm.from_zip_for_sem(z, ~root=Exp).term,
        ),
      )
    );
  let mk_statics = (z: Zipper.t): Language.StaticsBase.Map.t =>
    mk_statics_with(Language.CoreSettings.on, z);

  /* Stores the warning from the most recent structural edit.
     Set by go(), read by Agent.re to include in success messages.
     Cleared on each call to go(). */
  let last_warning: ref(option(string)) = ref(None);

  let record_warning =
      (
        go:
          (~syntax: CachedSyntax.t, ~z: Zipper.t, ~a: Action.Structural.t) =>
          result((Zipper.t, option(string)), Action.Failure.t),
        ~syntax: CachedSyntax.t,
        ~z: Zipper.t,
        ~a: Action.Structural.t,
      )
      : result(Zipper.t, Action.Failure.t) => {
    last_warning := None;
    switch (go(~syntax, ~z, ~a)) {
    | Ok((z, warning)) =>
      last_warning := warning;
      Ok(z);
    | Error(e) => Error(e)
    };
  };

  let go =
    record_warning(
      Local.go(
        ~mk_statics=z => PerfTimer.time("statics", () => mk_statics(z)),
        ~initial_info_map=None,
      ),
    );
  /* With the editor's statics for this zipper in hand (CachedStatics.
     for_zipper), both maps of the error check are computed the editor's
     way — same settings — so the comparison stays fair while the initial
     pass is skipped. */
  let go_with_editor_statics =
      (~settings: Language.CoreSettings.t, ~initial: CachedStatics.t) =>
    record_warning(
      Local.go(
        ~mk_statics=
          z =>
            PerfTimer.time("statics", () => {
              /* The full record, computed with the editor's settings and
                 offered: the editor's recompute for this program takes it
                 instead of running statics again */
              let full =
                CachedStatics.init(
                  ~settings,
                  ~is_dynamic_term=false,
                  ~stitch=x => x,
                  ~root=Sort.Exp,
                  z,
                );
              CachedStatics.offer(~settings, z, full);
              full.info_map;
            }),
        ~initial_info_map=Some(initial.info_map),
      ),
    );

  let read_dispatch =
      (~action: CompositionActions.read_action, ~z: Zipper.t)
      : result(string, Action.Failure.t) => {
    let info_map = mk_statics(z);
    let syntax = CachedSyntax.init(z);
    Local.read_dispatch(~action, ~z, ~info_map, ~syntax);
  };
};
