module ReplayData = Map.Make(Int);

type t = {
  replay_data: ReplayData.t(LogEntry.t),
  pos: int,
};

let asdf = true;
