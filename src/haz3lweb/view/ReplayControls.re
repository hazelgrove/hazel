open Virtual_dom.Vdom;
open Node;
open Widgets;
open Util.Web;

let view_log_entries = (replay: Replay.t) => {
  let view_log_entry = (i: int) => {
    let abs_pos = replay.pos + i;
    let element =
      switch (Replay.ReplayData.find_opt(abs_pos, replay.data)) {
      | None => Node.None
      | Some((_timestamp, update)) =>
        let msg =
          Printf.sprintf(
            "%d: %s",
            abs_pos,
            update |> UpdateAction.sexp_of_t |> Sexplib.Sexp.to_string_hum,
          );
        text(msg);
      };
    let cur_clss =
      if (i == 0) {
        "current-update";
      } else {
        "other-update";
      };
    div(~attr=clss([cur_clss]), [element]);
  };
  let viewable_indices = {
    // Try to view 10 actions on either side (past/future) of current one
    let k = 10;
    // Optimistically, if we are at the beginning of replay, display more actions at the end
    let leftover_left = max(k - replay.pos, 0);
    // Optimistically, if we are at the end of replay, display more actions at the beginning
    let leftover_right = max(k + replay.pos - replay.len, 0);
    Base.List.range(
      - k - leftover_right,
      k + leftover_left,
      ~stop=`inclusive,
    );
  };
  List.map(view_log_entry, viewable_indices);
};

let view = (~inject, replay: Replay.t) => {
  div(
    ~attr=clss(["replay-control"]),
    [
      div(
        ~attr=clss(["top-bar"]),
        [
          button_d(
            Icons.undo,
            inject(UpdateAction.ReplayAction(BackwardReplay)),
            ~disabled=!Replay.can_backward(replay),
            ~tooltip="Step Backward",
          ),
          button_d(
            Icons.redo,
            inject(ReplayAction(ForwardReplay)),
            ~disabled=!Replay.can_forward(replay),
            ~tooltip="Step Forward",
          ),
          button(
            replay.is_playing ? Icons.eye : Icons.circle_question,
            _ => inject(ReplayAction(TogglePlayReplay)),
            ~tooltip="Toggle Play",
          ),
          button(
            text("X"),
            _ => inject(ReplayAction(DisableReplay)),
            ~tooltip="End Replay",
          ),
        ],
      ),
      div(view_log_entries(replay)),
      div(
        ~attr=clss(["help-text"]),
        /*
         There are a number of tricky-to-resolve issues with replay functionality.

         (1) Replay initial state may differ from current state, which will cause
         a divergence in the replay. We currently don't track initial state in the log,
         so this is hard to resolve. Notably, this can mean a replay may occur in a different
         slide than the one that the user is currently viewing.

         (2) User actions during a replay may cause inconsistency in the replay by diverging
         the current state from the replayed state. Worse, the replay Undo function hooks into
         the underlying Undo functionality, so in the presence of user actions, an Undo may
         incorrectly scroll the replay back an action while undoing a user action.
         */
        [text("Warning: Replay viewer in development")],
      ),
    ],
  );
};
