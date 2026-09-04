open Util_web;
open Language;

let update = (z: Zipper.t, f: Sample.Focus.t => Sample.Focus.t) =>
  Zipper.update_refractors(z, refractors =>
    {
      ...refractors,
      sample_focus: f(refractors.sample_focus),
    }
  );

let update_pinned_call =
    (z: Zipper.t, f: option(CallStack.t) => option(CallStack.t)) =>
  update(z, sample_focus =>
    {
      ...sample_focus,
      pinned_stack: f(sample_focus.pinned_stack),
    }
  );

/* Sightline write side: updates the sightline on click/navigation.
 *
 * Suffix preservation: when the new sample's stack is a suffix of the
 * current sightline, keep the full sightline and lower the index.
 * This retains below-focus frames for alignment recovery.
 *
 * Perspective extension: when clicking an app probe (id = Some(ap_id)),
 * prepend the application as a frame below the focus. This extends
 * the sightline downward (peeking into a call without entering it).
 *
 * See Sample.Focus module comment and plans/sample-focus-sightline.md. */
let capture = (z: Zipper.t, data: Sample.Capture.t, id): Zipper.t => {
  update(z, sample_focus =>
    {
      ...sample_focus,
      time: Some(data.time),
      seq: data.seq,
      indicated_call:
        id != None ? id : z.refractors.sample_focus.indicated_call,
      call_stack:
        switch (id) {
        | Some(ap_id) =>
          /* Perspective extension: see CallStack.extend. Index stays at
             the original depth, so this frame appears "below" (ghosted). */
          CallStack.extend(ap_id, data.call_stack)
        | None =>
          !
            ListUtil.is_suffix_of(
              ~eq=CallStack.equal_frame,
              data.call_stack,
              sample_focus.call_stack,
            )
            ? data.call_stack : sample_focus.call_stack
        },
      index: List.length(data.call_stack) - 1,
      step_range: Some((data.step_start, data.step_end)),
    }
  );
};

let toggle_pin_call = (z: Zipper.t, call_stack): Zipper.t =>
  update_pinned_call(z, pinned_call => {
    switch (pinned_call) {
    | Some(existing) when CallStack.equal(call_stack, existing) => None
    | _ => Some(call_stack)
    }
  });

let reset = (z: Zipper.t): Zipper.t =>
  update(z, _ => Language.Sample.Focus.init);

/* Resolve pending focus after step-into by finding and focusing
   the sample that matches the target stack. Called from Probes
   after it looks up the samples from dynamics. */
let resolve_pending_focus =
    (z: Zipper.t, samples: list(Sample.t), target_stack: CallStack.t)
    : Zipper.t => {
  let matching_sample =
    List.find_opt(
      (s: Sample.t) => CallStack.equal(s.call_stack, target_stack),
      samples,
    );
  switch (matching_sample) {
  | Some(sample) =>
    update(z, sample_focus =>
      {
        ...sample_focus,
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
    sample_focus => {
      let max_index = List.length(sample_focus.call_stack) - 1;
      /* Allow -1 for top-level (outside all calls) */
      let clamped_index = max(-1, min(i, max_index));
      {
        ...sample_focus,
        index: clamped_index,
      };
    },
  );

let go = (z: Zipper.t, a: Action.sample_focus): Zipper.t =>
  switch (a) {
  | Capture(sample, id) => capture(z, sample, id)
  | TogglePin(call_stack) => toggle_pin_call(z, call_stack)
  | SetIndex(i) => set_index(z, i)
  | Reset => reset(z)
  };
