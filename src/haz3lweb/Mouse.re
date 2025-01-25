open Util;

module MkState = () => {
  /* State Machine Diagram:
   *
   * This is a simple state machine representing mouse button state.
   * The button can be either down or up, and keeps a count of consecutive
   * clicks, where 'consecutive' means they are made no more than `delay_ms`
   * apart from each-other. */

  [@deriving (show({with_path: false}), sexp, yojson)]
  type count = int;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type button =
    | Up
    | Down;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = (button, count);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type timer = option((state, state));

  let init: state = (Up, 0);

  let state: ref(state) = ref(init);

  let delay_ms = 310.0;

  let toggle = (old_button): button =>
    switch (old_button) {
    | Up => Down
    | Down => Up
    };

  let count_reset_timer = (old_count): unit =>
    JsUtil.delay(delay_ms, () =>
      if ((Up, old_count + 1) == state^) {
        state := init;
      }
    );

  let is_button_down = (): bool => {
    switch (state^ |> fst) {
    | Up => false
    | Down => true
    };
  };

  let down_transition = (): count => {
    let (old_button, old_count) = state^;
    state := (toggle(old_button), old_count);
    old_count;
  };

  let up_transition = (): unit => {
    let (old_button, old_count) = state^;
    state := (toggle(old_button), old_count + 1);
    count_reset_timer(old_count);
  };
};
