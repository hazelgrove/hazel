open Util_web;
open Language;

/* ---------- Problem category ---------- */

[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type problem_category =
  | Syntax
  | Hole
  | Static
  | Warning
  | Projector;

/* ---------- Problem data types ---------- */

type problem_source =
  | Structural(string)
  | FromInfo(Info.t)
  | FromProjector(ProjectorKind.t, ProjectorBase.error);

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
  projector_errors: list((Id.t, ProjectorKind.t, ProjectorBase.error)),
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
   return the first one present in `measured`. The ancestor lookup is
   only forced if `id` itself is missing from `measured`. */
let nearest_measured_id =
    (~info_map: Statics.Map.t, ~measured: Measured.t, id: Id.t): option(Id.t) =>
  Seq.cons(id, () => List.to_seq(Statics.Map.ancestors_of(id, info_map), ()))
  |> Seq.find(anc => Measured.find_by_id(anc, measured) != None);

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
    switch (
      Option.bind(resolve_nearest(id), anc =>
        Measured.find_by_id(anc, measured)
      )
    ) {
    | Some(m) => m.origin
    | None => {
        row: max_int,
        col: max_int,
      }
    };
  /* Statics may span multiple editors; restrict ids to this editor's
     own term so problems don't leak across groups. */
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
  /* Collect projector errors with their kinds */
  let projector_errors =
    Id.Map.fold(
      (id, err: ProjectorBase.error, acc) =>
        switch (Id.Map.find_opt(id, syntax.projectors)) {
        | Some(p) => [(id, p.kind, err), ...acc]
        | None => acc
        },
      syntax.projector_errors,
      [],
    );
  {
    info_map,
    syntax_error_ids,
    hole_ids,
    concave_holes,
    static_error_ids,
    warning_ids,
    projector_errors,
    segment: syntax.segment,
    measured,
    row_to_line,
    pos,
    nearest_measured_id: resolve_nearest,
  };
};

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
  | Projector =>
    ctx.projector_errors
    |> List.to_seq
    |> Seq.map(((id, kind, err)) =>
         {
           id,
           category: Projector,
           source: FromProjector(kind, err),
         }
       )
  };

/* ---------- Convenience: all problems ---------- */

let collect_all_problems = (ctx: problem_context): list(problem) => {
  [Syntax, Hole, Static, Warning, Projector]
  |> List.concat_map(cat => collect_category(ctx, cat) |> List.of_seq);
};

/* ---------- Grouped multi-editor collection ---------- */

/* One editor contributing problems to a sidebar group. */
type editor_source = {
  statics: CachedStatics.t,
  syntax: CachedSyntax.t,
};

/* Input for one section of the Problems sidebar. A group with multiple
   sources renders as one section; L# labels are suppressed in that case
   because they would refer to different editors' geometries. */
type editor_group_input = {
  label: option(string),
  sources: list(editor_source),
};

/* A problem paired with the geometry of the source editor it came from,
   so the view can resolve fold-projector ancestors and render L# labels
   without needing to know which editor in a group the problem originated
   in. */
type located_problem = {
  problem,
  measured: Measured.t,
  row_to_line: int => option(int),
  /* See `problem_context.nearest_measured_id`. */
  nearest_measured_id: Id.t => option(Id.t),
  pos: Point.t,
};

/* Problems for one sidebar section. `single_source` is true iff the group
   came from exactly one editor, and drives whether the view shows L#
   labels. */
type problem_group = {
  label: option(string),
  single_source: bool,
  problems_by_category: list((problem_category, list(located_problem))),
  counts: list((problem_category, int)),
};

/* Top-level payload for the Problems sidebar. `counts` is aggregated
   across groups (drives the tab badge). */
type problem_collection = {
  groups: list(problem_group),
  counts: list((problem_category, int)),
};

/* Collect problems across several sidebar groups into a single coherent
   payload. De-duplicates by `(id, category)` in caller-provided order
   (groups in order, sources within each group in order) — the first
   source to claim a given (id, category) keeps it. This handles editors
   that share an underlying zipper (e.g. exercise `user_tests` and
   `test_validation` share `your_tests.tests`, so hole/syntax ids
   coincide): shared structural problems land in exactly one group while
   any context-specific static error still surfaces in the group where it
   actually occurs. */
let make =
    (~display_warnings: bool, inputs: list(editor_group_input))
    : problem_collection => {
  let seen: Hashtbl.t((Id.t, problem_category), unit) = Hashtbl.create(64);
  let collect_source = (source: editor_source) => {
    let ctx =
      make_problem_context(
        ~display_warnings,
        ~statics=source.statics,
        ~syntax=source.syntax,
      );
    let problems_by_category =
      List.map(
        cat => {
          let deduped =
            collect_category(ctx, cat)
            |> Seq.filter((p: problem) => {
                 let key = (p.id, cat);
                 if (Hashtbl.mem(seen, key)) {
                   false;
                 } else {
                   Hashtbl.add(seen, key, ());
                   true;
                 };
               })
            |> List.of_seq;
          let located =
            deduped
            |> List.map(p =>
                 {
                   problem: p,
                   measured: ctx.measured,
                   row_to_line: ctx.row_to_line,
                   nearest_measured_id: ctx.nearest_measured_id,
                   pos: ctx.pos(p.id),
                 }
               )
            |> List.sort((a, b) => compare(a.pos, b.pos));
          (cat, located);
        },
        [Syntax, Hole, Static, Warning],
      );
    problems_by_category;
  };
  let groups =
    List.map(
      (input: editor_group_input) => {
        let per_source = List.map(collect_source, input.sources);
        /* Concat per-source problems per category, preserving input
           order across sources. */
        let problems_by_category =
          List.map(
            cat =>
              (
                cat,
                List.concat_map(
                  pbc => Option.value(List.assoc_opt(cat, pbc), ~default=[]),
                  per_source,
                ),
              ),
            [Syntax, Hole, Static, Warning],
          );
        let counts =
          List.map(
            ((cat, ps)) => (cat, List.length(ps)),
            problems_by_category,
          );
        {
          label: input.label,
          single_source:
            switch (input.sources) {
            | []
            | [_] => true
            | _ => false
            },
          problems_by_category,
          counts,
        };
      },
      inputs,
    );
  let counts =
    [Syntax, Hole, Static, Warning]
    |> List.map(cat =>
         (
           cat,
           groups
           |> List.to_seq
           |> Seq.map((g: problem_group) =>
                Option.value(List.assoc_opt(cat, g.counts), ~default=0)
              )
           |> Seq.fold_left((+), 0),
         )
       );
  {
    groups,
    counts,
  };
};
