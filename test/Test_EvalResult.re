open Alcotest;
open Web;
open Haz3lcore;

/* `EvalResult.Update.calculate` owns the decisions that make evaluation
 * incremental: whether to ask for evaluation at all this frame, and what
 * `prev` to send when it does. Both are easy to get wrong in a way nothing
 * else notices -- a request that fires every frame is only slow, and a `prev`
 * that is wrong or empty silently turns the incremental evaluator off.
 *
 * `~queue_worker` is the seam. Passing `Some(collector)` captures the requests
 * the editor would have posted, without a worker; passing `None` makes
 * `calculate` evaluate synchronously through `WorkerServer.evaluate_sync` and
 * store the resulting `incr_eval` in the model, which is what the next frame
 * sends as `prev`. */

let settings = Settings.Model.init.core;

let statics_of_text = (text: string): CachedStatics.t =>
  switch (Parser.to_zipper(~root=Sort.Exp, text)) {
  | None => failwith("could not parse: " ++ text)
  | Some(z) =>
    let model =
      Editor.Model.mk(z, ~root=Sort.Exp) |> CodeWithStatics.Model.mk;
    CodeWithStatics.Update.calculate(
      ~settings,
      ~is_edited=true,
      ~stitch=x => x,
      ~dynamics=model.dynamics,
      ~is_dynamic_term=false,
      model,
    ).
      statics;
  };

let calculate = (~settings=settings, ~queue_worker, statics, model) =>
  EvalResult.Update.calculate(
    ~settings,
    ~queue_worker,
    ~is_edited=true,
    statics,
    model,
  );

/* Collect the requests this frame would have posted to the worker. */
let recording = () => {
  let posted = ref([]);
  let queue_worker =
    Some((req: WorkerServer.Request.value) => posted := posted^ @ [req]);
  (queue_worker, () => posted^);
};

let a = statics_of_text("let f = fun x -> x + 1 in f(2)");
let b = statics_of_text("let g = fun y -> y * 3 in g(4)");

let tests = (
  "EvalResult",
  [
    /* With evaluation switched off there is nothing to ask for, and asking
       anyway would spin the worker for results the UI will not show. */
    test_case(
      "no request when dynamics is off",
      `Quick,
      () => {
        let (queue_worker, posted) = recording();
        let _ =
          calculate(
            ~settings={
              ...settings,
              dynamics: false,
            },
            ~queue_worker,
            a,
            EvalResult.Model.init,
          );
        check(int, "requests posted", 0, List.length(posted()));
      },
    ),
    test_case(
      "a new elaboration is requested",
      `Quick,
      () => {
        let (queue_worker, posted) = recording();
        let _ = calculate(~queue_worker, a, EvalResult.Model.init);
        check(int, "requests posted", 1, List.length(posted()));
      },
    ),
    /* The gate that keeps typing from re-evaluating on frames where the
       program did not actually change (`Calc.set(~eq=Exp.fast_equal, ...)`). */
    test_case(
      "an unchanged elaboration is not re-requested",
      `Quick,
      () => {
        let (queue_worker, posted) = recording();
        let model = calculate(~queue_worker, a, EvalResult.Model.init);
        let _ = calculate(~queue_worker, a, model);
        check(
          int,
          "requests posted across two frames",
          1,
          List.length(posted()),
        );
      },
    ),
    test_case(
      "a changed elaboration is re-requested",
      `Quick,
      () => {
        let (queue_worker, posted) = recording();
        let model = calculate(~queue_worker, a, EvalResult.Model.init);
        let _ = calculate(~queue_worker, b, model);
        check(
          int,
          "requests posted across two frames",
          2,
          List.length(posted()),
        );
      },
    ),
    /* Evaluating in-process (no worker) has to leave the reuse data behind,
       otherwise the next frame has nothing to be incremental with. */
    test_case(
      "synchronous evaluation records reuse data",
      `Quick,
      () => {
        let model = calculate(~queue_worker=None, a, EvalResult.Model.init);
        check(
          bool,
          "incr_eval is non-empty after evaluating",
          false,
          Language.IncrEval.is_empty(EvalResult.Model.incr_eval(model)),
        );
      },
    ),
    /* The threading itself: the next request must carry the previous run's
       `incr_eval` as `prev`. If this sends `IncrEval.empty` the incremental
       evaluator is off and nothing else in the app can tell. */
    test_case(
      "the next request carries the previous incr_eval as prev",
      `Quick,
      () => {
        let model = calculate(~queue_worker=None, a, EvalResult.Model.init);
        let recorded = EvalResult.Model.incr_eval(model);
        let (queue_worker, posted) = recording();
        let _ = calculate(~queue_worker, b, model);
        switch (posted()) {
        | [req] =>
          check(
            bool,
            "prev is non-empty",
            false,
            Language.IncrEval.is_empty(req.prev),
          );
          check(
            bool,
            "prev is the incr_eval the previous frame recorded",
            true,
            req.prev === recorded,
          );
        | posted =>
          failf("expected exactly one request, got %d", List.length(posted))
        };
      },
    ),
  ],
);
