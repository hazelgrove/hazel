open Util;
open Language;

/* Text-only probe display for LLM/agent consumption.
 * Outputs program text with probed expressions decorated
 * and sample values appended at line ends. */

/* Unicode markers for probed expressions */
let probe_open = "⟦";
let probe_close = "⟧";

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

/* State for tracking probes during segment traversal */
type probe_on_line = {
  probe_id: Id.t,
  ap_id: option(Id.t),
};

/* Mutable state to collect probes per line during traversal */
let current_line: ref(int) = ref(0);
let probes_by_line: ref(IntMap.t(list(probe_on_line))) = ref(IntMap.empty);

let reset_state = (): unit => {
  current_line := 0;
  probes_by_line := IntMap.empty;
};

let record_probe = (probe_id: Id.t, ap_id: option(Id.t)): unit => {
  let line = current_line^;
  let existing =
    IntMap.find_opt(line, probes_by_line^) |> Option.value(~default=[]);
  probes_by_line :=
    IntMap.add(
      line,
      existing
      @ [
        {
          probe_id,
          ap_id,
        },
      ],
      probes_by_line^,
    );
};

let count_newlines = (s: string): int =>
  String.fold_left((acc, c) => c == '\n' ? acc + 1 : acc, 0, s);

/* Custom projector_to_segment that wraps probes with markers */
let probe_projector_to_segment = (pr: Base.projector): Base.segment => {
  switch (pr.kind) {
  | Probe =>
    /* Record this probe for the current line */
    record_probe(pr.id, None);
    /* Wrap the underlying syntax with probe markers */
    let syntax_seg = Piece.unparenthesize(pr.syntax);
    [
      Piece.Secondary({
        id: Id.mk(),
        content: Whitespace(probe_open),
      }),
      ...syntax_seg,
    ]
    @ [
      Piece.Secondary({
        id: Id.mk(),
        content: Whitespace(probe_close),
      }),
    ];
  | _ =>
    /* For non-probe projectors, use default behavior */
    Triggers.projector_to_invoke(pr)
  };
};

/* Convert segment to string while tracking probe positions.
 * Handles refractors (probes stored by term ID) by wrapping
 * probed terms with markers during skeleton traversal. */
let segment_to_string_with_probes =
    (
      ~refractors: Id.Map.t(Base.projector)=Id.Map.empty,
      ~holes: string=" ",
      ~concave_holes: string=" ",
      seg: Base.segment,
    )
    : string => {
  reset_state();

  /* Process a segment slice (list of pieces) to string */
  let rec process_segment = (seg: Base.segment): string =>
    seg |> List.map(process_piece) |> String.concat("")
  and process_piece = (p: Base.piece): string => {
    switch (p) {
    | Tile(t) => process_tile(t)
    | Grout({shape: Concave, _}) => concave_holes
    | Grout({shape: Convex, _}) => holes
    | Secondary(w) =>
      let s =
        switch (w.content) {
        | Whitespace(str)
        | Comment(str) => str
        };
      /* Track newlines */
      current_line := current_line^ + count_newlines(s);
      s;
    | Projector(pr) =>
      switch (pr.kind) {
      | Probe =>
        /* Record this probe for the current line */
        record_probe(pr.id, None);
        /* Process underlying syntax with markers */
        let inner = process_segment(Piece.unparenthesize(pr.syntax));
        probe_open ++ inner ++ probe_close;
      | _ =>
        /* For other projectors, expand to invocation syntax */
        process_segment(Triggers.projector_to_invoke(pr))
      }
    };
  }
  and process_tile = (t: Base.tile): string => {
    /* Process tile and children - refractor check happens in process_child_segment */
    Aba.mk(t.shards, t.children)
    |> Aba.join(List.nth(t.label), process_child_segment)
    |> String.concat("");
  }
  /* Process a child segment of a compound operator - run skeleton check on it */
  and process_child_segment = (child_seg: Base.segment): string =>
    try({
      let skel = Segment.skel(child_seg);
      let (skel_start, skel_end) = Skel.range(skel);
      let leading =
        process_segment(ListUtil.sublist((0, skel_start), child_seg));
      let main = go_with_seg(child_seg, skel);
      let trailing =
        process_segment(
          ListUtil.sublist(
            (skel_end + 1, List.length(child_seg)),
            child_seg,
          ),
        );
      leading ++ main ++ trailing;
    }) {
    | Skel.Nonconvex_segment
    | Failure(_) => process_segment(child_seg)
    }
  /* Process segment with refractor handling via skeleton traversal.
   * This mimics Triggers.refractor_seg_to_seg but outputs strings
   * with probe markers instead of producing a new segment. */

  /* Process an Aba root from the skeleton, using the given segment */
  and go_aba_with_seg = (cur_seg: Base.segment, root: Skel.root): string => {
    let indices = Aba.get_as(root);
    let children = Aba.get_bs(root);
    switch (indices, children) {
    | ([single_idx], []) =>
      /* Atomic operator: just process this piece */
      process_segment(
        ListUtil.sublist((single_idx, single_idx + 1), cur_seg),
      )
    | ([first_idx, ...rest_indices], children) =>
      /* Compound operator: interleave slices with processed children */
      let rec go_interleave =
              (prev_idx: int, indices: list(int), children: list(Skel.t))
              : string =>
        switch (indices, children) {
        | ([], []) =>
          process_segment(
            ListUtil.sublist((prev_idx, prev_idx + 1), cur_seg),
          )
        | ([next_idx, ...rest_indices], [child, ...rest_children]) =>
          let (child_start, child_end) = Skel.range(child);
          let before_child =
            process_segment(
              ListUtil.sublist((prev_idx, child_start), cur_seg),
            );
          let child_result = go_with_seg(cur_seg, child);
          let after_child =
            process_segment(
              ListUtil.sublist((child_end + 1, next_idx), cur_seg),
            );
          let rest_result =
            go_interleave(next_idx, rest_indices, rest_children);
          before_child ++ child_result ++ after_child ++ rest_result;
        | _ => failwith("Aba invariant violated")
        };
      go_interleave(first_idx, rest_indices, children);
    | ([], _) => failwith("Aba invariant violated: empty indices")
    };
  }
  and go_with_seg = (cur_seg: Base.segment, skel: Skel.t): string => {
    let result =
      switch (skel) {
      | Op(root) => go_aba_with_seg(cur_seg, root)
      | Pre(root, child) =>
        let root_indices = Aba.get_as(root);
        let root_end = ListUtil.last(root_indices);
        let (child_start, _) = Skel.range(child);
        let root_result = go_aba_with_seg(cur_seg, root);
        let between =
          process_segment(
            ListUtil.sublist((root_end + 1, child_start), cur_seg),
          );
        let child_result = go_with_seg(cur_seg, child);
        root_result ++ between ++ child_result;
      | Post(child, root) =>
        let (_, child_end) = Skel.range(child);
        let root_indices = Aba.get_as(root);
        let root_start = List.hd(root_indices);
        let child_result = go_with_seg(cur_seg, child);
        let between =
          process_segment(
            ListUtil.sublist((child_end + 1, root_start), cur_seg),
          );
        let root_result = go_aba_with_seg(cur_seg, root);
        child_result ++ between ++ root_result;
      | Bin(left, root, right) =>
        let (_, left_end) = Skel.range(left);
        let (right_start, _) = Skel.range(right);
        let root_indices = Aba.get_as(root);
        let root_start = List.hd(root_indices);
        let root_end = ListUtil.last(root_indices);
        let left_result = go_with_seg(cur_seg, left);
        let before_root =
          process_segment(
            ListUtil.sublist((left_end + 1, root_start), cur_seg),
          );
        let root_result = go_aba_with_seg(cur_seg, root);
        let after_root =
          process_segment(
            ListUtil.sublist((root_end + 1, right_start), cur_seg),
          );
        let right_result = go_with_seg(cur_seg, right);
        left_result ++ before_root ++ root_result ++ after_root ++ right_result;
      };

    /* Check if this skeleton node's root has a probe refractor */
    let root_id = Segment.root_id(skel, cur_seg);
    switch (Id.Map.find_opt(root_id, refractors)) {
    | Some(pr) when pr.kind == Probe =>
      record_probe(pr.id, None);
      probe_open ++ result ++ probe_close;
    | _ => result
    };
  };

  /* If no refractors, just process directly */
  if (Id.Map.is_empty(refractors)) {
    process_segment(seg);
  } else {
    /* Try to build skeleton; fall back to direct processing if it fails */
    try({
      let skel = Segment.skel(seg);
      let (skel_start, skel_end) = Skel.range(skel);
      /* Process leading secondary (before skeleton) */
      let leading = process_segment(ListUtil.sublist((0, skel_start), seg));
      /* Process skeleton */
      let main = go_with_seg(seg, skel);
      /* Process trailing secondary */
      let trailing =
        process_segment(
          ListUtil.sublist((skel_end + 1, List.length(seg)), seg),
        );
      leading ++ main ++ trailing;
    }) {
    | Skel.Nonconvex_segment
    | Failure(_) => process_segment(seg)
    };
  };
};

/* Format a single sample value as text */
let format_value =
    (~max_length: int=50, _utility: ProjectorBase.utility, value: Exp.t)
    : string => {
  /* Note: _utility is available for future use with term_to_seg if needed */
  let seg =
    ExpToSegment.exp_to_segment(
      ~settings={
        ...ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
        show_unknown_as_hole: false,
      },
      value |> DHExp.strip_ascriptions,
    );
  let str =
    Printer.of_segment(~holes="?", ~indent="", ~is_single_line=true, seg);
  /* Remove any remaining newlines */
  let str = StringUtil.replace(StringUtil.regexp("\n"), str, " ");
  /* Truncate if too long */
  if (String.length(str) > max_length) {
    String.sub(str, 0, max_length - 3) ++ "...";
  } else {
    str;
  };
};

/* Determine empty status for a probe (simplified from ProbeProj) */
type empty_status =
  | NoSamplesExist
  | HiddenByPin
  | NotAligned
  | HasSamples;

let get_empty_status =
    (~window: ProbeProj.Settings.window, samples: list(Sample.t))
    : empty_status =>
  switch (samples) {
  | [] => NoSamplesExist
  | _ when window == Single =>
    /* In single mode, we'd need cursor alignment check.
     * For now, just show first sample. */
    HasSamples
  | _ => HasSamples
  };

/* Format empty status as text */
let format_empty_status = (status: empty_status): string =>
  switch (status) {
  | NoSamplesExist => no_samples_indicator
  | HiddenByPin => hidden_by_pin_indicator
  | NotAligned => not_aligned_indicator
  | HasSamples => ""
  };

/* Format probe values for a line */
let format_probe_values =
    (
      ~window: ProbeProj.Settings.window,
      ~utility: ProjectorBase.utility,
      ~probe_map: Sample.Map.t,
      probes: list(probe_on_line),
    )
    : string => {
  let format_one = (probe: probe_on_line): option(string) => {
    let samples =
      Id.Map.find_opt(probe.probe_id, probe_map) |> Option.value(~default=[]);

    switch (get_empty_status(~window, samples)) {
    | NoSamplesExist => Some(no_samples_indicator)
    | HiddenByPin => Some(hidden_by_pin_indicator)
    | NotAligned => Some(not_aligned_indicator)
    | HasSamples =>
      let max_samples =
        switch (window) {
        | Single => 1
        | Many => 5
        };
      let selected = ListUtil.take(max_samples, samples);
      let formatted =
        List.map(
          (s: Sample.t) => format_value(~max_length=40, utility, s.value),
          selected,
        );
      switch (formatted) {
      | [] => None
      | [single] => Some(single)
      | multiple => Some(String.concat(value_separator, multiple))
      };
    };
  };

  let formatted_probes = List.filter_map(format_one, probes);
  switch (formatted_probes) {
  | [] => ""
  | values =>
    value_spacing ++ value_divider ++ String.concat(value_separator, values)
  };
};

/* Main entry point: generate text representation of program with probes */
let of_segment =
    (
      ~window: ProbeProj.Settings.window=Single,
      ~probe_map: Sample.Map.t,
      ~refractors: Id.Map.t(Base.projector)=Id.Map.empty,
      ~utility: ProjectorBase.utility,
      segment: Segment.t,
    )
    : string => {
  /* First pass: convert segment to string while recording probe positions */
  let base_text = segment_to_string_with_probes(~refractors, segment);
  let recorded_probes = probes_by_line^;

  /* If no probes were found, just return the base text */
  if (IntMap.is_empty(recorded_probes)) {
    base_text;
  } else {
    /* Split into lines and append probe values */
    let lines = String.split_on_char('\n', base_text);
    let augmented_lines =
      List.mapi(
        (line_num: int, line: string): string => {
          switch (IntMap.find_opt(line_num, recorded_probes)) {
          | None => line
          | Some(probes) =>
            let values =
              format_probe_values(~window, ~utility, ~probe_map, probes);
            line ++ values;
          }
        },
        lines,
      );
    String.concat("\n", augmented_lines);
  };
};

/* Convenience function for use from DebugConsole */
let of_zipper =
    (
      ~window: ProbeProj.Settings.window=Single,
      ~probe_map: Sample.Map.t,
      zipper: Zipper.t,
    )
    : string => {
  let segment = Zipper.unselect_and_zip(~erase_buffer=true, zipper);
  let refractors = zipper.refractors.manuals;
  let utility = ProjectorInfo.utility;
  of_segment(~window, ~probe_map, ~refractors, ~utility, segment);
};
