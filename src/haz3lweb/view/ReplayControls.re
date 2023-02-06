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
  let k = 10;
  let viewable_indices = Base.List.range(- k, k, ~stop=`inclusive);
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
            inject(UpdateAction.BackwardReplay),
            ~disabled=!Replay.can_backward(replay),
            ~tooltip="Step Backward",
          ),
          button_d(
            Icons.redo,
            inject(ForwardReplay),
            ~disabled=!Replay.can_forward(replay),
            ~tooltip="Step Forward",
          ),
          button(
            replay.is_playing ? Icons.eye : Icons.circle_question,
            _ => inject(TogglePlayReplay),
            ~tooltip="Toggle Play",
          ),
          button(
            text("X"),
            _ => inject(DisableReplay),
            ~tooltip="End Replay",
          ),
        ],
      ),
      div(~attr=clss(["content"]), view_log_entries(replay)),
    ],
  );
};
