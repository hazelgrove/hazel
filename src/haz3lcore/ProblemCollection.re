open Util;
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
  /* virtual-grout: holes are structural (shape conflicts in Skel),
     not Grout pieces, so we store their neighbor IDs instead */
  hole_ids: list(Id.t),
  concave_holes: list(Id.t),
  static_error_ids: list((Id.t, Info.t)),
  warning_ids: list((Id.t, Info.t)),
  projector_errors: list((Id.t, ProjectorKind.t, ProjectorBase.error)),
  segment: Segment.t,
  measured: Measured.t,
  row_to_line: int => option(int),
  pos: Id.t => Point.t,
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
  let pos = id =>
    switch (Measured.find_by_id(id, measured)) {
    | Some(m) => m.origin
    | None => {
        row: max_int,
        col: max_int,
      }
    };
  /* Partition error_ids into syntax and static in a single pass */
  let (syntax_error_ids, static_error_ids) =
    List.fold_right(
      (id, (syn, stat)) =>
        switch (Statics.Map.lookup(id, info_map)) {
        | Some(ci) when Info.is_error(ci) =>
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
          | Some(ci) when Info.is_warning(ci) => Some((id, ci))
          | _ => None
          },
        statics.warning_ids,
      );
    } else {
      [];
    };
  /* virtual-grout: holes are structural (shape conflicts between adjacent
     pieces), not Grout tiles. Walk segment detecting conflicts and record
     the nearest tile ID for each hole. Convex holes = missing operand,
     Concave holes = missing operator. */
  let (hole_ids, concave_holes) = {
    let boundary = Nib.Shape.concave();
    let rec collect_seg = (seg: Segment.t) => {
      let (convex_rev, concave_rev, prev_r, last_id) =
        List.fold_left(
          ((convex, concave, prev_r, last_id), p: Piece.t) =>
            switch (p) {
            | Secondary(_) => (convex, concave, prev_r, last_id)
            | Tile(t) =>
              let (l_shape, r_shape) = Tile.shapes(t);
              let (convex, concave) =
                if (Nib.Shape.fits(prev_r, l_shape)) {
                  (convex, concave);
                } else {
                  switch (Nib.Shape.flip(prev_r)) {
                  | Convex => ([t.id, ...convex], concave)
                  | Concave(_) => (convex, [t.id, ...concave])
                  };
                };
              /* Also collect from children */
              let (child_convex, child_concave) =
                List.fold_left(
                  ((cv, cc), child) => {
                    let (cv2, cc2) = collect_seg(child);
                    (cv2 @ cv, cc2 @ cc);
                  },
                  ([], []),
                  t.children,
                );
              (
                child_convex @ convex,
                child_concave @ concave,
                r_shape,
                Some(t.id),
              );
            | Projector(pr) =>
              let (l_shape, r_shape) = ProjectorCore.shapes(pr);
              let (convex, concave) =
                if (Nib.Shape.fits(prev_r, l_shape)) {
                  (convex, concave);
                } else {
                  switch (Nib.Shape.flip(prev_r)) {
                  | Convex => ([pr.id, ...convex], concave)
                  | Concave(_) => (convex, [pr.id, ...concave])
                  };
                };
              (convex, concave, r_shape, Some(pr.id));
            },
          ([], [], boundary, None),
          seg,
        );
      /* Check trailing boundary for conflict */
      let (convex_rev, concave_rev) =
        switch (last_id) {
        | None => (convex_rev, concave_rev)
        | Some(id) =>
          if (Nib.Shape.fits(prev_r, boundary)) {
            (convex_rev, concave_rev);
          } else {
            switch (Nib.Shape.flip(prev_r)) {
            | Convex => ([id, ...convex_rev], concave_rev)
            | Concave(_) => (convex_rev, [id, ...concave_rev])
            };
          }
        };
      (List.rev(convex_rev), List.rev(concave_rev));
    };
    collect_seg(syntax.segment);
  };
  /* Collect projector errors with their kinds (new in dev — #1998 added
     CachedSyntax.projector_errors). */
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
      |> Seq.map((id: Id.t) =>
           {
             id,
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
    |> Seq.map((id: Id.t) =>
         {
           id,
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

/* ---------- Counts summary ---------- */

let counts_of_context =
    (ctx: problem_context): list((problem_category, int)) => {
  all_of_problem_category
  |> List.map(cat => (cat, collect_category(ctx, cat) |> Seq.length));
};

/* ---------- Convenience: all problems ---------- */

let collect_all_problems = (ctx: problem_context): list(problem) => {
  [Syntax, Hole, Static, Warning, Projector]
  |> List.concat_map(cat => collect_category(ctx, cat) |> List.of_seq);
};
