open Util;

/* ---------- Problem data types ---------- */

type problem_source =
  | Structural(string)
  | FromInfo(Language.Info.t);

type problem = {
  id: Id.t,
  category: SidebarModel.Settings.error_category,
  source: problem_source,
};

/* ---------- Error context ---------- */

type error_context = {
  info_map: Language.Statics.Map.t,
  syntax_error_ids: list((Id.t, Language.Info.t)),
  hole_ids: list(Haz3lcore.Grout.t),
  concave_holes: list(Haz3lcore.Grout.t),
  static_error_ids: list((Id.t, Language.Info.t)),
  warning_ids: list((Id.t, Language.Info.t)),
  segment: Haz3lcore.Segment.t,
  measured: Haz3lcore.Measured.t,
  row_to_line: int => option(int),
  pos: Id.t => Point.t,
};

let make_error_context =
    (~settings: Settings.t, ~editor: CodeWithStatics.Model.t): error_context => {
  let measured = editor.editor.syntax.measured;
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
  let info_map = editor.statics.info_map;
  let pos = id =>
    switch (Haz3lcore.Measured.find_by_id(id, measured)) {
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
        switch (Language.Statics.Map.lookup(id, info_map)) {
        | Some(ci) when Language.Info.is_error(ci) =>
          if (Language.Info.is_syntax_error(ci)) {
            ([(id, ci), ...syn], stat);
          } else {
            (syn, [(id, ci), ...stat]);
          }
        | _ => (syn, stat)
        },
      editor.statics.error_ids,
      ([], []),
    );
  /* Collect warning ids with their info */
  let warning_ids =
    if (settings.core.display_warnings) {
      List.filter_map(
        id =>
          switch (Language.Statics.Map.lookup(id, info_map)) {
          | Some(ci) when Language.Info.is_warning(ci) => Some((id, ci))
          | _ => None
          },
        editor.statics.warning_ids,
      );
    } else {
      [];
    };
  /* Collect holes once and partition into convex (empty holes) and concave (missing operators) */
  let all_holes = Haz3lcore.Segment.holes(editor.editor.syntax.segment);
  let (hole_ids, concave_holes) =
    List.partition((g: Haz3lcore.Grout.t) => g.shape == Convex, all_holes);
  {
    info_map,
    syntax_error_ids,
    hole_ids,
    concave_holes,
    static_error_ids,
    warning_ids,
    segment: editor.editor.syntax.segment,
    measured,
    row_to_line,
    pos,
  };
};

/* ---------- Sorting helper (exposed for consumers) ---------- */

let sort_by_pos =
    (ctx: error_context, problems: list(problem)): list(problem) =>
  List.sort((a, b) => compare(ctx.pos(a.id), ctx.pos(b.id)), problems);

/* ---------- Per-category collection (lazy) ---------- */

let collect_category =
    (ctx: error_context, cat: SidebarModel.Settings.error_category)
    : Seq.t(problem) =>
  switch (cat) {
  | Syntax =>
    let grout_seq =
      ctx.concave_holes
      |> List.to_seq
      |> Seq.map((g: Haz3lcore.Grout.t) =>
           {
             id: g.id,
             category: Syntax,
             source: Structural("Missing operator"),
           }
         );
    let incomplete_seq =
      Haz3lcore.Segment.incomplete_tiles_deep(ctx.segment)
      |> List.to_seq
      |> Seq.map((t: Haz3lcore.Tile.t) => {
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
    |> Seq.map((g: Haz3lcore.Grout.t) =>
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

/* ---------- Counts summary ---------- */

let counts_of_context =
    (ctx: error_context): list((SidebarModel.Settings.error_category, int)) => {
  SidebarModel.Settings.all_of_error_category
  |> List.map(cat => (cat, collect_category(ctx, cat) |> Seq.length));
};

/* ---------- Convenience: all problems ---------- */

let collect_all_problems = (ctx: error_context): list(problem) => {
  [SidebarModel.Settings.Syntax, Hole, Static, Warning]
  |> List.concat_map(cat => collect_category(ctx, cat) |> List.of_seq);
};
