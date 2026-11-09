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
    : unit => {
  /* Replies are tagged with the generation of the request they answer, so
   * EvalResult can refuse replies to a request it has since replaced. */
  let gen_of_key = key =>
    switch (List.assoc_opt(key, batch)) {
    | Some(v: WorkerServer.Request.value) => v.generation
    | None => (-1)
    };
  WorkerClient.request(
    batch,
    ~on_result=
      List.iter(((key, result)) => {
        let result: Language.ProgramResult.t(Language.ProgramResult.inner) =
          switch (result) {
          | Ok((r, s)) =>
            ResultOk({
              result: r,
              state: s,
            })
          | Error(e) => ResultFail(e)
          };
        dispatch(
          pos_of_key(key),
          EvalResult.Update.UpdateResult(gen_of_key(key), result),
        );
      }),
    ~on_timeout,
    ~on_ack=
      List.iter(((key, stream)) =>
        dispatch(
          pos_of_key(key),
          EvalResult.Update.UpdateStreamingEval(
            gen_of_key(key),
            Language.IncrEval.outbox_of_completed(stream),
          ),
        )
      ),
    ~on_stream=(key, stream) =>
    dispatch(
      pos_of_key(key),
      EvalResult.Update.MergeStreamingEval(gen_of_key(key), stream),
    )
  );
};
