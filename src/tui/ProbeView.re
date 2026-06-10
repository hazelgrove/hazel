open Haz3lcore;

/* Offside display of probe sample values: `≡ value` appended after the
   line containing each probed expression. Reuses haz3lcore's ProbeText
   (built for text-only probe output): probes locate by refractor +
   Measured, values format through the same recipe.

   This is the v1 of probe display — latest sample only, no sample
   focus / step-into / pinning (the web's full probe UI). */

let style = Style.fg(Theme.green);

/* (row, spans) entries to append after line ends */
let by_line =
    (~probe_map: Language.Dynamics.Map.t, editor: Editor.Model.t)
    : list((int, Frame.row)) => {
  let z = editor.state.zipper;
  let refractors = z.refractors.manuals;
  if (refractors == []) {
    [];
  } else {
    let probes_by_line =
      ProbeText.get_probes_by_line(refractors, editor.syntax.measured);
    Util.IntMap.bindings(probes_by_line)
    |> List.filter_map(((row, ids)) =>
         switch (
           ProbeText.format_probe_values(~window=Single, ~probe_map, ids)
         ) {
         | "" => None
         | text => Some((row, [(style, String.trim(text))]))
         | exception _ => None
         }
       );
  };
};
