module ReplayData = Map.Make(Int);

type t = {
  data: ReplayData.t(LogEntry.t),
  len: int,
  pos: int,
  is_playing: bool,
};

let of_log = log => {
  let logi = List.mapi((i, e) => (i, e), log);
  let data =
    List.fold_left(
      (replay, (i, e)) => ReplayData.add(i, e, replay),
      ReplayData.empty,
      logi,
    );
  {data, len: List.length(log), pos: 0, is_playing: false};
};

let can_forward = t => {
  t.pos < t.len;
};

let can_backward = t => {
  t.pos > 0;
};

let toggle_play = t => {...t, is_playing: !t.is_playing};

let forward = t => {
  switch (ReplayData.find_opt(t.pos, t.data)) {
  | None => None
  | Some((_timestamp, update)) => Some(({...t, pos: t.pos + 1}, update))
  };
};

let backward = t => {
  switch (ReplayData.find_opt(t.pos - 1, t.data)) {
  | None => None
  | Some((_timestamp, update)) => Some(({...t, pos: t.pos - 1}, update))
  };
};

module StepResult = {
  type nonrec t =
    | Step((t, int, UpdateAction.t))
    | EndOfReplay(t, UpdateAction.t)
    | Fail;
};

let step = t => {
  switch (
    ReplayData.find_opt(t.pos, t.data),
    ReplayData.find_opt(t.pos + 1, t.data),
  ) {
  | (None, None)
  | (None, Some(_)) => StepResult.Fail
  | (Some((_t, update)), None) =>
    StepResult.EndOfReplay({...t, pos: t.pos + 1, is_playing: false}, update)
  | (Some((t1, update)), Some((t2, _next_update))) =>
    StepResult.Step((
      {...t, pos: t.pos + 1},
      t2 -. t1 |> Float.to_int,
      update,
    ))
  };
};
