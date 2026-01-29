open Util;
open Language;

/* Text-only probe display for LLM/agent consumption.
 * Outputs program text with probed expressions wrapped in ^^probe(...)
 * and sample values appended at line ends. */

/* Divider between expression and values */
let value_divider = " ≡ ";

/* Separator between multiple values in many mode */
let value_separator = " ⫽ ";

/* Empty status indicators (matching GUI) */
let no_samples_indicator = "∅";
let hidden_by_pin_indicator = "⍟";
let not_aligned_indicator = "⊖";

/* Spacing before value section */
let value_spacing = "    ";

/* Compute which line each refractor (probe) is on using Measured.
 * Returns a map from line number to list of probe IDs on that line. */
let get_probes_by_line =
    (refractors: Zipper.Refractor.RefractorList.t, measured: Measured.t)
    : IntMap.t(list(Id.t)) =>
  List.fold_right(
    ((tile_id, entry: Zipper.Refractor.entry), acc) =>
      if (entry.kind != Probe) {
        acc;
      } else {
        switch (Measured.find_by_id(tile_id, measured)) {
        | Some(m) =>
          let row = m.origin.row;
          let existing =
            IntMap.find_opt(row, acc) |> Option.value(~default=[]);
          IntMap.add(row, existing @ [tile_id], acc);
        | None => acc
        };
      },
    refractors,
    IntMap.empty,
  );

/* Format a single sample value as text */
let format_value = (~max_length: int=50, value: Exp.t): string => {
  let seg =
    ExpToSegment.exp_to_segment(
      ~settings={
        ...ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
        show_unknown_as_hole: false,
      },
      value |> DHExp.strip_ascriptions,
    );
  let str = Printer.of_segment(~holes="?", seg);
  /* Remove any remaining newlines */
  let str = StringUtil.replace(StringUtil.regexp("\n"), str, " ");
  /* Truncate if too long */
  if (String.length(str) > max_length) {
    String.sub(str, 0, max_length - 3) ++ "...";
  } else {
    str;
  };
};

/* Determine empty status for a probe.
 * Returns None if samples exist, Some(status) if empty. */
let get_empty_status =
    (~window: Sample.Window.mode, samples: list(Sample.t))
    : option(Sample.Selection.empty_status) =>
  switch (samples) {
  | [] => Some(NoSamplesExist)
  | _ when window == Single =>
    /* TODO: implement cursor alignment check */
    None
  | _ => None
  };

/* Format probe values for a line */
let format_probe_values =
    (
      ~window: Sample.Window.mode,
      ~probe_map: Sample.Map.t,
      probe_ids: list(Id.t),
    )
    : string => {
  let format_one = (probe_id: Id.t): option(string) => {
    let samples =
      Sample.Map.lookup(probe_id, probe_map) |> Option.value(~default=[]);

    switch (get_empty_status(~window, samples)) {
    | Some(NoSamplesExist) => Some(no_samples_indicator)
    | Some(HiddenByPin) => Some(hidden_by_pin_indicator)
    | Some(NotAligned) => Some(not_aligned_indicator)
    | Some(Evaluating) => Some("...")
    | None =>
      let max_samples =
        switch (window) {
        | Single => 1
        | Many => 5
        };
      let selected = ListUtil.take(max_samples, samples);
      let formatted =
        List.map(
          (s: Sample.t) => format_value(~max_length=40, s.value),
          selected,
        );
      switch (formatted) {
      | [] => None
      | [single] => Some(single)
      | multiple => Some(String.concat(value_separator, multiple))
      };
    };
  };

  let formatted_probes = List.filter_map(format_one, probe_ids);
  switch (formatted_probes) {
  | [] => ""
  | values =>
    value_spacing ++ value_divider ++ String.concat(value_separator, values)
  };
};

/* Main entry point: generate text representation of program with probes */
let of_segment =
    (
      ~window: Sample.Window.mode=Single,
      ~probe_map: Sample.Map.t,
      ~refractors: Zipper.Refractor.RefractorList.t,
      segment: Segment.t,
    )
    : string => {
  /* Convert segment to string using Printer, which uses Triggers to wrap
   * probed expressions with ^^probe(...) notation */
  let base_text = Printer.of_segment(~holes=" ", ~refractors, segment);

  /* If no refractors, just return the base text */
  if (List.is_empty(refractors)) {
    base_text;
  } else {
    /* Compute measured to get probe line positions */
    let measured =
      Measured.of_segment(
        segment,
        ProjectorCore.Shape.Map.empty,
        Id.Map.empty,
      );

    /* Build map of probes by line */
    let probes_by_line = get_probes_by_line(refractors, measured);

    /* If no probes found, just return base text */
    if (IntMap.is_empty(probes_by_line)) {
      base_text;
    } else {
      /* Split into lines and append probe values */
      let lines = String.split_on_char('\n', base_text);
      let augmented_lines =
        List.mapi(
          (line_num: int, line: string): string => {
            switch (IntMap.find_opt(line_num, probes_by_line)) {
            | None => line
            | Some(probe_ids) =>
              let values =
                format_probe_values(~window, ~probe_map, probe_ids);
              line ++ values;
            }
          },
          lines,
        );
      String.concat("\n", augmented_lines);
    };
  };
};

/* Convenience function for use from zipper */
let of_zipper =
    (
      ~window: Sample.Window.mode=Single,
      ~probe_map: Sample.Map.t,
      zipper: Zipper.t,
    )
    : string => {
  let segment = Zipper.unselect_and_zip(~erase_buffer=true, zipper);
  let refractors = zipper.refractors.manuals;
  of_segment(~window, ~probe_map, ~refractors, segment);
};
