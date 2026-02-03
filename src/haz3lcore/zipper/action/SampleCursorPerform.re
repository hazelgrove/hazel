open Util;
open Language;

let update = (z: Zipper.t, f: Sample.Cursor.t => Sample.Cursor.t) =>
  Zipper.update_refractors(z, refractors =>
    {
      ...refractors,
      sample_cursor: f(refractors.sample_cursor),
    }
  );

let update_pinned_call =
    (z: Zipper.t, f: option(Sample.call_stack) => option(Sample.call_stack)) =>
  update(z, sample_cursor =>
    {
      ...sample_cursor,
      pinned_stack: f(sample_cursor.pinned_stack),
    }
  );

let capture = (z: Zipper.t, sample: Sample.t, id): Zipper.t =>
  update(z, sample_cursor =>
    {
      ...sample_cursor,
      time: Some(sample.time),
      seq: sample.seq,
      indicated_call: id /*!= None ? id : z.refractors.sample_cursor.indicated_call*/,
      call_stack:
        !ListUtil.is_suffix_of(sample.call_stack, sample_cursor.call_stack)
          ? sample.call_stack : sample_cursor.call_stack,
      index: List.length(sample.call_stack) - 1,
      step_range: Some((sample.step_start, sample.step_end)),
    }
  );

let toggle_pin_call = (z: Zipper.t, call_stack): Zipper.t =>
  update_pinned_call(z, pinned_call => {
    /* Compare by ID only - function names may differ */
    switch (pinned_call) {
    | Some(existing)
        when Sample.ids_of_stack(call_stack) == Sample.ids_of_stack(existing) =>
      None
    | _ => Some(call_stack)
    }
  });

let reset = (z: Zipper.t): Zipper.t =>
  update(z, _ => Language.Sample.Cursor.init);

/* Resolve pending focus after step-into by finding and focusing
   the sample that matches the target stack. Called from Probes
   after it looks up the samples from dynamics. */
let resolve_pending_focus =
    (z: Zipper.t, samples: list(Sample.t), target_stack: Sample.call_stack)
    : Zipper.t => {
  /* Compare by ID only - target_stack may have None for function names */
  let target_ids = Sample.ids_of_stack(target_stack);
  let matching_sample =
    List.find_opt(
      (s: Sample.t) => Sample.ids_of_stack(s.call_stack) == target_ids,
      samples,
    );
  switch (matching_sample) {
  | Some(sample) =>
    update(z, sample_cursor =>
      {
        ...sample_cursor,
        time: Some(sample.time),
        seq: sample.seq,
        indicated_call: None,
        call_stack: sample.call_stack,
        index: List.length(sample.call_stack) - 1,
        step_range: Some((sample.step_start, sample.step_end)),
        pending_focus: None,
      }
    )
  | None => z
  };
};

let set_index = (z: Zipper.t, i: int): Zipper.t =>
  update(
    z,
    sample_cursor => {
      let max_index = List.length(sample_cursor.call_stack) - 1;
      let clamped_index = max(0, min(i, max_index));
      {
        ...sample_cursor,
        index: clamped_index,
      };
    },
  );

let go = (z: Zipper.t, a: Action.sample_cursor): Zipper.t =>
  switch (a) {
  | Capture(sample, id) => capture(z, sample, id)
  | TogglePin(call_stack) => toggle_pin_call(z, call_stack)
  | SetIndex(i) => set_index(z, i)
  | Reset => reset(z)
  };
