/* Shared wiring for modes that evaluate cells on the worker: translates
 * worker keys back to mode positions and turns the worker's lifecycle
 * callbacks into EvalResult actions. `dispatch` delivers one action to one
 * position; `on_timeout` handles a timed-out batch (exercise modes mark
 * every stitched cell, not just the batch items). */

/* How long the theorem steppers may keep showing their previous data after
 * the worker acks a new eval, if the result hasn't arrived by then. */
let stepper_hold_ms = 2000.;

/* Distinguishes holds across acks so the timeout for an earlier hold can't
 * release a later one. */
let hold_gen: ref(int) = ref(0);

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
        dispatch(pos_of_key(key), EvalResult.Update.UpdateResult(result));
      }),
    ~on_timeout,
    ~on_ack=
      List.iter(((key, stream)) => {
        let pos = pos_of_key(key);
        incr(hold_gen);
        let gen = hold_gen^;
        dispatch(
          pos,
          EvalResult.Update.UpdateStreamingEval(
            gen,
            Language.IncrEval.outbox_of_completed(stream),
          ),
        );
        let _: Js_of_ocaml.Dom_html.timeout_id =
          Js_of_ocaml.Dom_html.window##setTimeout(
            Js_of_ocaml.Js.wrap_callback(() =>
              dispatch(pos, EvalResult.Update.ReleaseStepperHold(gen))
            ),
            stepper_hold_ms,
          );
        ();
      }),
    ~on_stream=(key, stream) =>
    dispatch(pos_of_key(key), EvalResult.Update.MergeStreamingEval(stream))
  );
