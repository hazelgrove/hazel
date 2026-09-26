open Util;
open HighLevelNodeMap.Public;
open Language;
open OptUtil.Syntax;

/* ===== ITEMS MODE (plans/agent-items-convergence.md) =====
   The tool path on the per-item statics engine: the node map from the
   item chain, the maps from DefStatics (a diff-walk plus the dirty items
   instead of a whole-program pass). Set for the duration of one structural
   action by Public.go_items; the editor keeps its own statics meanwhile. */
let items_mode: ref(option(Language.CoreSettings.t)) = ref(None);
/* the per-item analysis of a zipper's program, the way the editor makes
   it (CachedStatics.init_compositional): the incremental per-item parse,
   so the tool path and the editor share DefStatics' memo slot for the
   same program. One structural action asks for the same zipper several
   times (statics, node map, diff): memoized on zipper identity. */
let items_memo: ref(option((Zipper.t, DefStatics.t))) = ref(None);
let items_for = (settings: Language.CoreSettings.t, z: Zipper.t): DefStatics.t =>
  switch (items_memo^) {
  | Some((z0, ds)) when z0 === z => ds
  | _ =>
    let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
    let term =
      Segment.global_missing_shards(seg) == []
        ? MakeTerm.Incr.term_of_root(~root=Exp, seg)
        : MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    let ds =
      DefStatics.calc_auto(
        ~settings,
        ~probe_ids=CachedStatics.probe_ids_of_zipper(z),
        term,
      );
    items_memo := Some((z, ds));
    ds;
  };

/* phase timers for the journal's perf lines */
let build = (z, info_map) =>
  PerfTimer.time("node-map", () =>
    switch (items_mode^) {
    | Some(settings) =>
      HighLevelNodeMap.build_from_items(items_for(settings, z))
    | None => build(z, info_map)
    }
  );

/* web-side listener for FastParse fallback telemetry (the journal);
   core stays UI-agnostic */
let fallback_notice: ref(option(string => unit)) = ref(None);

/* DIAGNOSTIC (merge verification): why the node map could not be built */
let derive_fail_note = (tag: string, z: Zipper.t, info_map: StaticsBase.Map.t) => {
  let id = Indicated.index(z);
  let ci =
    switch (id) {
    | Some(id) => Id.Map.find_opt(id, info_map)
    | None => None
    };
  let anc =
    switch (ci) {
    | Some(i) => Info.ancestors_of(i)
    | None => []
    };
  let ctor = (id: Id.t): string =>
    switch (Id.Map.find_opt(id, info_map)) {
    | Some(InfoExp({user_term, _})) =>
      switch (Exp.term_of(user_term)) {
      | Let(_) => "Let"
      | Seq(_) => "Seq"
      | ModuleExp(_) => "ModuleExp"
      | TyAlias(_) => "TyAlias"
      | Parens(_) => "Parens"
      | EmptyHole => "Hole"
      | _ => "exp"
      }
    | Some(_) => "non-exp"
    | None => "MISSING"
    };
  let chain =
    anc
    |> List.filteri((k, _) => k < 14)
    |> List.map(ctor)
    |> String.concat(">");
  let text = Printer.of_zipper(~holes="?", z);
  let n = String.length(text);
  let tail = n > 90 ? String.sub(text, n - 90, 90) : text;
  Js_of_ocaml.Firebug.console##error(
    Js_of_ocaml.Js.string(
      Printf.sprintf(
        "[derive-fail %s] indicated=%s in_map=%b map=%d anc=%d chain=%s tail=%S",
        tag,
        switch (id) {
        | Some(id) => Id.to_string(id)
        | None => "none"
        },
        ci != None,
        Id.Map.cardinal(info_map),
        List.length(anc),
        chain,
        tail,
      ),
    ),
  );
};

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

    /* Module members are nodes whose info is the statics expansion's
       Let/TyAlias wrapper, reclassified to a Mod cls. Their pat/def ids are
       real syntax; their "body" is the expansion continuation (the REST of
       the members), which must never be an edit target. */
    /* structural: the node's parent binds a module literal. (The info's
       cls is not a reliable signal — monolithic statics leaves the LAST
       member's wrapper classed as a plain let, and per-item statics
       classes every member so; both sent last-member inserts down the
       expression path, which adds no `;` — dungeon runs: everything
       inserted after the last member of Creatures was swallowed into it.) */
    let is_module_member = (node_map: node_map, node: node): bool =>
      switch (HighLevelNodeMap.parent_id_of(node)) {
      | None => false
      | Some(pid) =>
        switch (Id.Map.find_opt(pid, node_map)) {
        | Some({info: InfoExp({user_term, _}), _}) =>
          switch (Exp.term_of(user_term)) {
          | Let(_, def, _)
          | ModuleExp(_, def, _) =>
            switch (Exp.term_of(def)) {
            | Module(_) => true
            | _ => false
            }
          | _ => false
          }
        | _ => false
        }
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
        PerfTimer.time("diff/node-map", () => build(old_zipper, old_statics));
      let new_statics =
        PerfTimer.time("diff/statics", () => mk_statics(new_zipper));
      let* new_node_map =
        PerfTimer.time("diff/node-map", () => build(new_zipper, new_statics));
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
    /* Text-to-segment parsing simulates typing (Insert.go per char with a
       full remold/regrout each), so cost is quadratic in chunk size:
       ~0.3s at 500 chars, ~0.8s at 1000, ~8s at 3700 (measured on the
       graph-livelit module). It runs on the UI thread, so an oversized
       chunk reads as a hung editor. Only menhir-refused chunks land here
       (the fast path is uncapped), so the cap trades a rare multi-second
       stall against refusing the edit outright; 3000 chars is a ~5s
       worst case. */
    let max_chunk_chars = 3000;
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
    /* ~fast: try the linear Menhir zip first (FastParse) — used by the
       big-chunk overwrite path (update_definition / update_binding_clause),
       where the quadratic typing parse froze the editor. The small insert
       paths keep the typing parser so their whitespace conventions (magic
       spaces, boundary newlines) are untouched. */
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
          PerfTimer.time("splice", () =>
            Zipper.insert_segment(
              z,
              pad_fusing_edges(
                z,
                EditIdentity.reuse(z.selection.content, segment),
              ),
              ~root=splice_root,
            )
          );
        Ok(z');
      | None =>
        if (fast) {
          /* fallback telemetry: which construct pushed us onto the
             quadratic path, and roughly how bad — console + any
             registered listener (the constellation journal) */
          let msg =
            "FastParse fallback ("
            ++ string_of_int(String.length(code))
            ++ " chars): "
            ++ Option.value(FastParse.bail_note^, ~default="no note");
          print_endline(msg);
          switch (fallback_notice^) {
          | Some(f) => f(msg)
          | None => ()
          };
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
    /* member boundaries of a chunk of member code: `;` at bracket depth 0,
       outside string literals */
    let split_members = (code: string): list(string) => {
      let n = String.length(code);
      let parts = ref([])
      and start = ref(0)
      and depth = ref(0)
      and in_str = ref(false);
      let i = ref(0);
      while (i^ < n) {
        let c = code.[i^];
        if (in_str^) {
          if (c == '\\') {
            incr(i);
          } else if (c == '"') {
            in_str := false;
          };
        } else {
          switch (c) {
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
        | None =>
          derive_fail_note("new_z", new_z, new_info_map);
          Error(Action.Failure.Cant_derive_local_AST_information);
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
                e
                ++ PerformUtils.reserved_word_note(code)
                ++ PerformUtils.parse_hint(),
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
    | Update(Body, path, code)
        when
          Utils.is_module_member(
            initial_node_map,
            path_to_node(initial_node_map, path),
          ) =>
      ignore(code);
      Utils.member_body_error("update the body");
    | Update(Body, path, code) =>
      let initial_node = path_to_node(initial_node_map, path);
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
        let new_info_map = mk_statics(new_z);
        switch (build(new_z, new_info_map)) {
        | None =>
          derive_fail_note("new_z", new_z, new_info_map);
          Error(Action.Failure.Cant_derive_local_AST_information);
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
                e
                ++ PerformUtils.reserved_word_note(code)
                ++ PerformUtils.parse_hint(),
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
        | None =>
          derive_fail_note("new_z", new_z, new_info_map);
          Error(Action.Failure.Cant_derive_local_AST_information);
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
      let root =
        Utils.is_module_member(initial_node_map, initial_node)
          ? Sort.Mod : Sort.Exp;
      switch (
        PerformUtils.overwrite_term(
          ~root,
          initial_z,
          target_id,
          code,
          true,
          syntax,
        )
      ) {
      | Error(e) => Error(e)
      | Ok(new_z) =>
        let new_info_map = mk_statics(new_z);
        switch (build(new_z, new_info_map)) {
        | None =>
          derive_fail_note("new_z", new_z, new_info_map);
          Error(Action.Failure.Cant_derive_local_AST_information);
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
                e
                ++ PerformUtils.reserved_word_note(code)
                ++ PerformUtils.parse_hint(),
              ),
            )
          | None => Ok(new_z)
          }
        };
      };
    | Insert(Before, path, code) =>
      // todo: figure out a better method than magic space
      let target_id = path_to_id(initial_node_map, path);
      let is_member =
        Utils.is_module_member(
          initial_node_map,
          path_to_node(initial_node_map, path),
        );
      switch (
        is_member
          ? PerformUtils.insert_member(
              initial_z,
              target_id,
              code,
              Direction.Left,
              syntax,
            )
          : PerformUtils.insert_term(
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
        let old_errors =
          PerfTimer.time("errors", () => ErrorPrint.all(initial_info_map));
        let new_errors =
          PerfTimer.time("errors", () => ErrorPrint.all(new_info_map));
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
      let is_member =
        Utils.is_module_member(
          initial_node_map,
          path_to_node(initial_node_map, path),
        );
      switch (
        is_member
          ? PerformUtils.insert_member(
              initial_z,
              target_id,
              code,
              Direction.Right,
              syntax,
            )
          : PerformUtils.insert_term(
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
        let old_errors =
          PerfTimer.time("errors", () => ErrorPrint.all(initial_info_map));
        let new_errors =
          PerfTimer.time("errors", () => ErrorPrint.all(new_info_map));
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
    | Delete(Body, path)
        when
          Utils.is_module_member(
            initial_node_map,
            path_to_node(initial_node_map, path),
          ) =>
      Utils.member_body_error("delete the body")
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
        ~initial_info_map: option(StaticsBase.Map.t)=None,
        a: Action.Structural.t,
        syntax: CachedSyntax.t,
        z: Zipper.t,
        mk_statics: Zipper.t => StaticsBase.Map.t,
      ) => {
    /* the editor's map for this very zipper when the caller has one
       (saves a full statics pass per tool); else compute */
    let initial_info_map =
      switch (initial_info_map) {
      | Some(m) => m
      | None => mk_statics(z)
      };
    switch (build(z, initial_info_map)) {
    | None =>
      derive_fail_note("z", z, initial_info_map);
      Error(Action.Failure.Cant_derive_local_AST_information);
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

  let mentions_trigger = (code: string): bool => {
    let n = String.length(code);
    let rec go = i =>
      i + 1 < n && (code.[i] == '^' && code.[i + 1] == '^' || go(i + 1));
    go(0);
  };
  let action_mentions_trigger = (a: Action.Structural.t): bool =>
    switch (a) {
    | Insert(_, _, code)
    | Update(_, _, code) => mentions_trigger(code)
    | _ => false
    };

  let go =
      (
        ~mk_statics: Zipper.t => StaticsBase.Map.t,
        ~initial_info_map: option(StaticsBase.Map.t),
        ~syntax: CachedSyntax.t,
        ~z: Zipper.t,
        ~a: Action.Structural.t,
      )
      : result(Zipper.t, Action.Failure.t) => {
    let res =
      try(
        switch (
          composition_dispatch(~initial_info_map, a, syntax, z, mk_statics)
        ) {
        | Ok(new_z) =>
          /* projector triggers (^^kind) are materialized program-wide; a
             chunk without one leaves nothing to materialize, and the walk
             costs ~100 ms at 170 lines */
          let materialized =
            action_mentions_trigger(a)
              ? PerfTimer.time("materialize", () =>
                  Materialize.all(new_z, ~root=Exp)
                )
              : new_z;
          let final =
            PerfTimer.time("normalize", () =>
              PerformUtils.normalize_top_level(~before=z, materialized)
            );
          Ok(final);
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
  /* per-item statics for a zipper: the merged map with spine ancestors */
  let mk_statics_items =
      (~settings: Language.CoreSettings.t, z: Zipper.t)
      : Language.StaticsBase.Map.t =>
    PerfTimer.time("items-statics", () =>
      ItemsSpine.merged_with_spine(items_for(settings, z))
    );
  /* display-side callers without an editor record in hand */
  let mk_statics = (z: Zipper.t): Language.StaticsBase.Map.t =>
    mk_statics_items(~settings=Language.CoreSettings.on, z);
  let node_map_of = (z: Zipper.t): option(HighLevelNodeMap.t) =>
    HighLevelNodeMap.build_from_items(
      items_for(Language.CoreSettings.on, z),
    );
  /* the diff of a structural action, both programs on per-item statics */
  let get_diff =
      (
        ~settings: Language.CoreSettings.t,
        old_z,
        new_z,
        action,
        ~old_syntax,
        ~new_syntax,
      ) => {
    let saved = items_mode^;
    items_mode := Some(settings);
    let res =
      try(
        Local.get_diff(
          old_z,
          new_z,
          action,
          mk_statics_items(~settings),
          ~old_syntax,
          ~new_syntax,
        )
      ) {
      | e =>
        items_mode := saved;
        raise(e);
      };
    items_mode := saved;
    res;
  };
  /* the whole structural action on per-item statics */
  let go_items =
      (~settings: Language.CoreSettings.t, ~syntax, ~z, ~a)
      : result(Zipper.t, Action.Failure.t) => {
    let saved = items_mode^;
    items_mode := Some(settings);
    let res =
      try(
        Local.go(
          ~mk_statics=z => mk_statics_items(~settings, z),
          ~initial_info_map=None,
          ~syntax,
          ~z,
          ~a,
        )
      ) {
      | e =>
        items_mode := saved;
        raise(e);
      };
    items_mode := saved;
    res;
  };
};
