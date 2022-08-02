open Core;

type t = {
  succeeded: (list((Perform.Action.t, Zipper.state)) as 'affix, 'affix),
  just_failed: option(FailedInput.t),
  // TODO(d): forgetting why we need this...
  // not seeing it get read anywhere. possibly
  // to trigger view redraw? review blame
  last_attempt: option(float),
};

let empty = {succeeded: ([], []), just_failed: None, last_attempt: None};

let can_undo = ({succeeded: (prefix, _), _}: t) => prefix != [];
let can_redo = ({succeeded: (_, suffix), _}: t) => suffix != [];

let clear_just_failed = history => {...history, just_failed: None};

let just_failed = (reason: FailedInput.reason, history: t) => {
  let last_attempt = Some(JsUtil.timestamp());
  let just_failed =
    Some(
      switch (history.just_failed) {
      | None => FailedInput.mk(reason)
      | Some(failed_input) =>
        FailedInput.replace_or_increment_attempts(reason, failed_input)
      },
    );
  {...history, last_attempt, just_failed};
};
let unrecognized_input = just_failed(Unrecognized);
let failure = f => just_failed(Failure(f));

let succeeded = (a: Perform.Action.t, zipper: Zipper.state, history: t) => {
  let (before, _) = history.succeeded;
  {
    succeeded: ([(a, zipper), ...before], []),
    just_failed: None,
    last_attempt: Some(JsUtil.timestamp()),
  };
};

let escaped = (history: t) => {
  ...history,
  just_failed: None,
  last_attempt: Some(JsUtil.timestamp()),
};

let undo = (z_id: Zipper.state, history: t): option((Zipper.state, t)) =>
  switch (history.succeeded) {
  | ([], _) => None
  | ([(a, prev), ...before], after) =>
    let succeeded = (before, [(a, z_id), ...after]);
    Some((prev, {...history, just_failed: None, succeeded}));
  };

let redo = (z_id: Zipper.state, history: t): option((Zipper.state, t)) =>
  switch (history.succeeded) {
  | (_, []) => None
  | (before, [(a, next), ...after]) =>
    let succeeded = ([(a, z_id), ...before], after);
    Some((next, {...history, just_failed: None, succeeded}));
  };
