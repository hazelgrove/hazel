open Util;
open Language;

/* ---------- Problem category ---------- */

[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type problem_category =
  | Syntax
  | Hole
  | Static
  | Warning;

/* ---------- Problem data types ---------- */

type problem_source =
  | Structural(string)
  | FromInfo(Info.t);

type problem = {
  id: Id.t,
  category: problem_category,
  source: problem_source,
};

/* ---------- Problem context ---------- */

type problem_context = {
  info_map: Statics.Map.t,
  syntax_error_ids: list((Id.t, Info.t)),
  hole_ids: list(Grout.t),
  concave_holes: list(Grout.t),
  static_error_ids: list((Id.t, Info.t)),
  warning_ids: list((Id.t, Info.t)),
  segment: Segment.t,
  measured: Measured.t,
  row_to_line: int => option(int),
  pos: Id.t => Point.t,
  /* Nearest ancestor of id (including id itself) that appears in
     `measured`. Ids beneath a projector are absent from `measured` but
     still live in the editor's term; this resolver lets the sidebar
     attribute such problems to the projector's visible line. */
  nearest_measured_id: Id.t => option(Id.t),
};

/* Walk `id` and its ancestors (innermost-first via Statics.Map) and
   return the first one present in `measured`. */
let nearest_measured_id =
    (~info_map: Statics.Map.t, ~measured: Measured.t, id: Id.t): option(Id.t) => {
  let rec walk =
    fun
    | [] => None
    | [anc, ...rest] =>
      switch (Measured.find_by_id(anc, measured)) {
      | Some(_) => Some(anc)
      | None => walk(rest)
      };
  switch (Measured.find_by_id(id, measured)) {
  | Some(_) => Some(id)
  | None => walk(Statics.Map.ancestors_of(id, info_map))
  };
};

let make_problem_context =
    (
      ~display_warnings: bool,
      ~statics: CachedStatics.t,
      ~syntax: CachedSyntax.t,
    )
    : problem_context => {
  let measured = syntax.measured;
  /* Build row→display-line mapping: skip empty rows added by projectors */
  let row_to_line = {
    let reversed = List.rev(measured.piece_rows);
    let (line_numbers_rev, _) =
      List.fold_left(
        ((acc, line_count), row) =>
          switch (row) {
          | [] => ([None, ...acc], line_count)
          | _ => ([Some(line_count), ...acc], line_count + 1)
          },
        ([], 1),
        reversed,
      );
    let mapping = Array.of_list(List.rev(line_numbers_rev));
    let num_rows = Array.length(mapping);
    row => row >= 0 && row < num_rows ? mapping[row] : None;
  };
  let info_map = statics.info_map;
  let resolve_nearest = id => nearest_measured_id(~info_map, ~measured, id);
  let pos = id =>
    switch (resolve_nearest(id)) {
    | Some(anc) =>
      switch (Measured.find_by_id(anc, measured)) {
      | Some(m) => m.origin
      | None => {
          row: max_int,
          col: max_int,
        }
      }
    | None => {
        row: max_int,
        col: max_int,
      }
    };
  /* An editor's statics may be computed over a stitched term that spans
     multiple editors (e.g. Exercise mode's `user_tests` statics covers
     `user_impl_term ⊕ your_tests.tests`). Filter error/warning ids to
     those whose piece actually lives in *this* editor's segment; otherwise
     a type error in `your_impl` would leak into the "Your Tests" group.
     Using TermData (not Measured) so ids hidden under a projector — which
     are absent from `measured` but still part of the term — are correctly
     recognized as belonging to this editor. */
  let id_in_this_editor = id =>
    TermData.root_piece(id, syntax.term_data) != None;
  /* Partition error_ids into syntax and static in a single pass */
  let (syntax_error_ids, static_error_ids) =
    List.fold_right(
      (id, (syn, stat)) =>
        switch (Statics.Map.lookup(id, info_map)) {
        | Some(ci) when Info.is_error(ci) && id_in_this_editor(id) =>
          if (Info.is_syntax_error(ci)) {
            ([(id, ci), ...syn], stat);
          } else {
            (syn, [(id, ci), ...stat]);
          }
        | _ => (syn, stat)
        },
      statics.error_ids,
      ([], []),
    );
  /* Collect warning ids with their info */
  let warning_ids =
    if (display_warnings) {
      List.filter_map(
        id =>
          switch (Statics.Map.lookup(id, info_map)) {
          | Some(ci) when Info.is_warning(ci) && id_in_this_editor(id) =>
            Some((id, ci))
          | _ => None
          },
        statics.warning_ids,
      );
    } else {
      [];
    };
  /* Collect holes once and partition into convex (empty holes) and concave (missing operators) */
  let all_holes = Segment.holes(syntax.segment);
  let (hole_ids, concave_holes) =
    List.partition((g: Grout.t) => g.shape == Convex, all_holes);
  {
    info_map,
    syntax_error_ids,
    hole_ids,
    concave_holes,
    static_error_ids,
    warning_ids,
    segment: syntax.segment,
    measured,
    row_to_line,
    pos,
    nearest_measured_id: resolve_nearest,
  };
};

/* ---------- Sorting helper (exposed for consumers) ---------- */

let sort_by_pos =
    (ctx: problem_context, problems: list(problem)): list(problem) =>
  List.sort((a, b) => compare(ctx.pos(a.id), ctx.pos(b.id)), problems);

/* ---------- Per-category collection (lazy) ---------- */

let collect_category =
    (ctx: problem_context, cat: problem_category): Seq.t(problem) =>
  switch (cat) {
  | Syntax =>
    let grout_seq =
      ctx.concave_holes
      |> List.to_seq
      |> Seq.map((g: Grout.t) =>
           {
             id: g.id,
             category: Syntax,
             source: Structural("Missing operator"),
           }
         );
    let incomplete_seq =
      Segment.incomplete_tiles_deep(ctx.segment)
      |> List.to_seq
      |> Seq.map((t: Tile.t) => {
           let all_indices = List.init(List.length(t.label), Fun.id);
           let missing_labels =
             List.filter(i => !List.mem(i, t.shards), all_indices)
             |> List.map(i => List.nth(t.label, i));
           let description =
             "Incomplete: missing " ++ String.concat(", ", missing_labels);
           {
             id: t.id,
             category: Syntax,
             source: Structural(description),
           };
         });
    let syntax_info_seq =
      ctx.syntax_error_ids
      |> List.to_seq
      |> Seq.map(((id, ci)) =>
           {
             id,
             category: Syntax,
             source: FromInfo(ci),
           }
         );
    Seq.append(grout_seq, Seq.append(incomplete_seq, syntax_info_seq));
  | Hole =>
    ctx.hole_ids
    |> List.to_seq
    |> Seq.map((g: Grout.t) =>
         {
           id: g.id,
           category: Hole,
           source: Structural("Empty hole"),
         }
       )
  | Static =>
    ctx.static_error_ids
    |> List.to_seq
    |> Seq.map(((id, ci)) =>
         {
           id,
           category: Static,
           source: FromInfo(ci),
         }
       )
  | Warning =>
    ctx.warning_ids
    |> List.to_seq
    |> Seq.map(((id, ci)) =>
         {
           id,
           category: Warning,
           source: FromInfo(ci),
         }
       )
  };

/* ---------- Convenience: all problems ---------- */

let collect_all_problems = (ctx: problem_context): list(problem) => {
  [Syntax, Hole, Static, Warning]
  |> List.concat_map(cat => collect_category(ctx, cat) |> List.of_seq);
};

/* ---------- Grouped multi-editor collection ---------- */

/* Input for one editor contributing problems to the sidebar. `label` is
   the display name shown as a section header when the collection has more
   than one group. */
type editor_input = {
  label: string,
  statics: CachedStatics.t,
  syntax: CachedSyntax.t,
};

/* Problems attributed to one editor, pre-sorted per category by position.
   `measured` / `row_to_line` are carried so the view can render L# labels
   and resolve caret positions using the originating editor's geometry. */
type problem_group = {
  label: string,
  measured: Measured.t,
  row_to_line: int => option(int),
  /* See `problem_context.nearest_measured_id`. */
  nearest_measured_id: Id.t => option(Id.t),
  problems_by_category: list((problem_category, list(problem))),
  counts: list((problem_category, int)),
};

/* Top-level payload for the Problems sidebar. `counts` is aggregated
   across groups (drives the tab badge). */
type problem_collection = {
  groups: list(problem_group),
  counts: list((problem_category, int)),
};

/* Collect problems across several editors into a single coherent payload.
   De-duplicates by `(id, category)` in caller-provided order — the first
   editor to claim a given (id, category) keeps it. This handles editors
   that share an underlying zipper (e.g. exercise `user_tests` and
   `test_validation` share `your_tests.tests`, so hole/syntax ids coincide):
   shared structural problems land in exactly one group while any
   context-specific static error still surfaces in the group where it
   actually occurs. */
let make =
    (~display_warnings: bool, editors: list(editor_input))
    : problem_collection => {
  let seen: Hashtbl.t((Id.t, problem_category), unit) = Hashtbl.create(64);
  let groups =
    List.map(
      (input: editor_input) => {
        let ctx =
          make_problem_context(
            ~display_warnings,
            ~statics=input.statics,
            ~syntax=input.syntax,
          );
        let problems_by_category =
          List.map(
            cat => {
              let deduped =
                collect_category(ctx, cat)
                |> List.of_seq
                |> List.filter((p: problem) => {
                     let key = (p.id, cat);
                     if (Hashtbl.mem(seen, key)) {
                       false;
                     } else {
                       Hashtbl.add(seen, key, ());
                       true;
                     };
                   });
              (cat, sort_by_pos(ctx, deduped));
            },
            [Syntax, Hole, Static, Warning],
          );
        let counts =
          List.map(
            ((cat, ps)) => (cat, List.length(ps)),
            problems_by_category,
          );
        {
          label: input.label,
          measured: ctx.measured,
          row_to_line: ctx.row_to_line,
          nearest_measured_id: ctx.nearest_measured_id,
          problems_by_category,
          counts,
        };
      },
      editors,
    );
  let counts =
    [Syntax, Hole, Static, Warning]
    |> List.map(cat =>
         (
           cat,
           List.fold_left(
             (n, g: problem_group) =>
               n
               + (
                 try(List.assoc(cat, g.counts)) {
                 | Not_found => 0
                 }
               ),
             0,
             groups,
           ),
         )
       );
  {
    groups,
    counts,
  };
};
