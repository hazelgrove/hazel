open Util;
open Ppx_yojson_conv_lib.Yojson_conv;
module Sexp = Sexplib.Sexp;

module ReplayData = {
  include Map.Make(Int);

  let pp = (pp_v, f, map) =>
    iter((k, v) => Format.fprintf(f, "%n -> %a@\n", k, pp_v, v), map);

  let sexp_of_t = (sexp_of_v: 'v => Sexp.t, map: t('v)): Sexp.t =>
    map |> bindings |> sexp_of_list(LogEntry.sexp_of_t(sexp_of_v));
  let t_of_sexp = (v_of_sexp: Sexp.t => 'v, sexp: Sexp.t): t('v) =>
    sexp |> list_of_sexp(binding_of_sexp(v_of_sexp)) |> List.to_seq |> of_seq;

  let yojson_of_t =
      (yojson_of_v: 'v => Yojson.Safe.t, map: t('v)): Yojson.Safe.t =>
    map |> bindings |> yojson_of_list(yojson_of_binding(yojson_of_v));
  let t_of_yojson =
      (v_of_yojson: Yojson.Safe.t => 'v, yojson: Yojson.Safe.t): t('v) =>
    yojson
    |> list_of_yojson(binding_of_yojson(v_of_yojson))
    |> List.to_seq
    |> of_seq;
};

[@deriving (show({with_path: false}), sexp, yojson)]
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
