/* Shared wiring for modes that evaluate cells on the worker: translates
 * worker keys back to mode positions and turns the worker's lifecycle
 * callbacks into EvalResult actions. `dispatch` delivers one action to one
 * position; `on_timeout` handles a timed-out batch (exercise modes mark
 * every stitched cell, not just the batch items). */

/* How long the reuse plan waits for the result, or the first stream,
   before it is shown on its own. The page recomputes its view after every
   action, and the plan only restates what is on screen (the reused
   results) and marks the result as evaluating: for a short evaluation,
   like a livelit slider release, it cost a redraw that showed nothing new.
   Held, it rides along with the result in one action
   (ApplyWorkerMessages), or with the first stream. On Kids' Choice the result comes ~150 ms after
   the plan, so 100 ms was too short. */
let plan_hold_ms = 300.;

let request =
    (
      batch: WorkerServer.Request.batch,
      ~pos_of_key: WorkerServer.key => 'pos,
      ~dispatch: ('pos, EvalResult.Update.t) => unit,
      ~on_timeout: WorkerServer.Request.batch => unit,
    )
    : unit => {
  let held_plan:
    ref(
      option(
        list((WorkerServer.key, WorkerServer.ServerMessage.stream_update)),
      ),
    ) =
    ref(None);
  let held_plan_timer = ref(None);
  /* this request's id, once posted: a held plan is shown only while it is
     still the latest (not replaced, cancelled or timed out) */
  let mine = ref(None);
  /* Take the held plan, if any, cancelling its timer. */
  let take_plan = () => {
    switch (held_plan_timer^) {
    | Some(t) => Js_of_ocaml.Dom_html.clearTimeout(t)
    | None => ()
    };
    held_plan_timer := None;
    let plan = held_plan^;
    held_plan := None;
    plan;
  };
  let show_plan = plan =>
    List.iter(
      ((key, stream)) =>
        dispatch(
          pos_of_key(key),
          EvalResult.Update.UpdateStreamingEval(stream),
        ),
      plan,
    );
  WorkerClient.request(
    batch,
    ~on_result=
      (~final_streams, results) => {
        let plan = Option.value(take_plan(), ~default=[]);
        List.iter(
          ((key, result)) => {
            let result: Language.ProgramResult.t(Language.ProgramResult.inner) =
              switch (result) {
              | Ok((r, s)) =>
                ResultOk({
                  result: r,
                  state: s,
                })
              | Error(e) => ResultFail(e)
              };
            /* with this item's held plan and final streams, as one
               action: one redraw */
            let of_key = l =>
              List.filter_map(((k, x)) => k == key ? Some(x) : None, l);
            switch (of_key(plan), of_key(final_streams)) {
            | ([], []) =>
              dispatch(
                pos_of_key(key),
                EvalResult.Update.UpdateResult(result),
              )
            | (plan, streams) =>
              dispatch(
                pos_of_key(key),
                EvalResult.Update.ApplyWorkerMessages(
                  List.nth_opt(plan, 0),
                  streams,
                  Some(result),
                ),
              )
            };
          },
          results,
        );
      },
    ~on_timeout=
      batch => {
        ignore(take_plan());
        on_timeout(batch);
      },
    ~on_ack=
      initial => {
        ignore(take_plan());
        held_plan :=
          Some(
            List.map(
              ((key, stream)) =>
                (key, Language.IncrEval.outbox_of_completed(stream)),
              initial,
            ),
          );
        held_plan_timer :=
          Some(
            Js_of_ocaml.Dom_html.setTimeout(
              () =>
                switch (take_plan()) {
                | Some(plan)
                    when
                      mine^ != None
                      && WorkerClient.latest_request_id() == mine^ =>
                  show_plan(plan)
                | _ => ()
                },
              plan_hold_ms,
            ),
          );
      },
    ~on_stream=
      (key, stream) =>
        /* with this key's held plan, as one action: one redraw */
        switch (take_plan()) {
        | None =>
          dispatch(
            pos_of_key(key),
            EvalResult.Update.MergeStreamingEval(stream),
          )
        | Some(plan) =>
          show_plan(List.filter(((k, _)) => k != key, plan));
          dispatch(
            pos_of_key(key),
            EvalResult.Update.ApplyWorkerMessages(
              List.assoc_opt(key, plan),
              [stream],
              None,
            ),
          );
        },
  );
  mine := WorkerClient.latest_request_id();
};
