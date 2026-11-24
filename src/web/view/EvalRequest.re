/* Shared wiring for modes that evaluate cells on the worker: translates
 * worker keys back to mode positions and turns the worker's lifecycle
 * callbacks into EvalResult actions. `dispatch` delivers one action to one
 * position; `on_timeout` handles a timed-out batch (exercise modes mark
 * every stitched cell, not just the batch items). */
let request =
    (
      batch: WorkerServer.Request.batch,
      ~pos_of_key: WorkerServer.key => 'pos,
      ~dispatch: ('pos, EvalResult.Update.t) => unit,
      ~on_timeout: WorkerServer.Request.batch => unit,
    )
    : unit =>
  WorkerClient.request(
    batch,
    ~on_result=
      List.iter(~f=((key, result)) => {
        let result: Language.ProgramResult.t(Language.ProgramResult.inner) =
          switch (result) {
          | Ok((r, s)) =>
            ResultOk({
              result: r,
              state: s,
            })
          | Error(e) => ResultFail(e)
          };
        dispatch(pos_of_key(key), EvalResult.Update.UpdateResult(result));
      }),
    ~on_timeout,
    ~on_ack=
      List.iter(~f=((key, stream)) =>
        dispatch(
          pos_of_key(key),
          EvalResult.Update.UpdateStreamingEval(
            Language.IncrEval.outbox_of_completed(stream),
          ),
        )
      ),
    ~on_stream=(key, stream) =>
    dispatch(pos_of_key(key), EvalResult.Update.MergeStreamingEval(stream))
  );
