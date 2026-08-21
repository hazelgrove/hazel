open Virtual_dom.Vdom;
open Util;
open Haz3lcore;
open Language;

/* Sample-value rendering for the canvas focus strip, on the probe display
   pipeline: real Abbreviate (structural, with … ellipsis terms) instead of
   string truncation, and segments rendered through the editor's own
   Code.view so values get probe-style syntax coloring.

   The expanded (2D) view has no upstream width-aware value layout to call
   into on this branch (the rich probe drawer lives in probes-iv), so
   layout is a small depth-limited breaker over the generated segment:
   break the outermost commas first, descending a level at a time until the
   widest line fits, and let Measured's native indentation do the rest. */

let seg_of = (~available: int, v: DHExp.t): Segment.t =>
  v
  |> DHExp.strip_ascriptions
  |> Exp.strip_projectors
  |> Abbreviate.abbreviate_exp(~available)
  |> fst
  |> ExpToSegment.exp_to_segment(
       ~settings={
         ...ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
         /* project_tables re-parses list segments via MakeTerm, which
            crashes in Skel on Abbreviate's … elements — and a table is
            the wrong rendering at well scale anyway. Canonical value
            spelling, no hole tiles: matches probe value formatting. */
         project_tables: false,
         use_literal_lexemes: true,
         show_unknown_as_hole: false,
         hole_tiles: false,
       },
     );

let string_of = (seg: Segment.t): string =>
  Printer.of_segment(~holes="?", seg);

let max_line_cols = (seg: Segment.t): int =>
  string_of(seg)
  |> String.split_on_char('\n')
  |> List.map(Unicode.Width.columns_of_string)
  |> List.fold_left(max, 0);

/* Insert a linebreak after every comma at depth < k, dropping the space
   that follows a broken comma (it would otherwise dangle at line start). */
let rec break_upto = (k: int, seg: Segment.t): Segment.t => {
  let seg =
    k > 0
      ? List.map(
          fun
          | Piece.Tile(t) =>
            Piece.Tile({
              ...t,
              children: List.map(break_upto(k - 1), t.children),
            })
          | p => p,
          seg,
        )
      : seg;
  let rec go = (ps: Segment.t): Segment.t =>
    switch (ps) {
    | [] => []
    | [Piece.Tile({label: [","], _}) as comma, ...rest] =>
      let rest =
        switch (rest) {
        | [Piece.Secondary(s), ...tl] when Haz3lcore.Secondary.is_space(s) => tl
        | _ => rest
        };
      [
        comma,
        Piece.Secondary(Haz3lcore.Secondary.mk_newline(Id.mk())),
        ...go(rest),
      ];
    | [p, ...rest] => [p, ...go(rest)]
    };
  go(seg);
};

/* Fit a value to a column budget: as-is if it fits, else the shallowest
   comma-break depth that does (or the deepest attempted). */
let fit_to_width = (~cols: int, seg: Segment.t): Segment.t =>
  if (max_line_cols(seg) <= cols) {
    seg;
  } else {
    let rec try_depth = (k: int) => {
      let broken = break_upto(k, seg);
      k >= 4 || max_line_cols(broken) <= cols ? broken : try_depth(k + 1);
    };
    try_depth(1);
  };

/* Inline chip: abbreviated to the well's budget, single line. */
let chip = (~font_metrics, ~available: int, v: DHExp.t): Node.t =>
  ProjectorView.flex_code(
    ~font_metrics,
    ~single_line=true,
    Sort.Exp,
    seg_of(~available, v),
  );

/* Expanded 2D view: generous abbreviation budget, comma-broken to fit. */
let expanded_budget = 2000;

let expanded = (~font_metrics, ~cols: int, v: DHExp.t): Node.t => {
  let seg =
    seg_of(~available=expanded_budget, v)
    |> fit_to_width(~cols=max(cols, 20));
  ProjectorView.flex_code(~font_metrics, Sort.Exp, seg);
};
