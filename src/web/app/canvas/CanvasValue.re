open Virtual_dom.Vdom;
open Haz3lcore;
open Language;

/* Inline value chips for the canvas type view: real Abbreviate (structural,
   with … ellipsis terms) rendered through the editor's own Code.view.
   Function-slot wells don't use this — they are full probe views
   (CanvasProbe); this remains only for the type view's aggregated
   distinct-value tally, which has no single probe site to render. */

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
            the wrong rendering at chip scale anyway. Canonical value
            spelling, no hole tiles: matches probe value formatting. */
         project_tables: false,
         use_literal_lexemes: true,
         show_unknown_as_hole: false,
         hole_tiles: false,
       },
     );

let string_of = (seg: Segment.t): string =>
  Printer.of_segment(~holes="?", seg);

/* Inline chip: abbreviated to the given budget, single line. */
let chip = (~font_metrics, ~available: int, v: DHExp.t): Node.t =>
  ProjectorView.flex_code(
    ~font_metrics,
    ~single_line=true,
    Sort.Exp,
    seg_of(~available, v),
  );
