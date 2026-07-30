/*
  This module defines the custom stack machine for the evaluator (so that we
  don't get OCaml stack overflows).

  We also include the ability to yield a computation so that the webworker can
  pause and communicate with the main thread.
 */

// Building blocks of the stack machine.

type t('a) =
  | Bind(t('b), 'b => t('a)): t('a)
  | Next(unit => t('a)): t('a)
  | Done('a): t('a);

type callstack('a, 'b) =
  | Finished: callstack('a, 'a)
  | Continue('a => t('b), callstack('b, 'c)): callstack('a, 'c);

let return = x => Done(x);

let bind = (t, f) => Bind(t, f);

module Syntax = {
  let (let.trampoline) = (x, f) => bind(x, f);
};

// Running the stack machine (without yielding).

let rec run: type a b. (t(b), callstack(b, a)) => a =
  (t: t(b), callstack: callstack(b, a)) => {
    switch (t) {
    | Bind(t, f) => run(t, Continue(f, callstack))
    | Next(f) => run(f(), callstack)
    | Done(x) =>
      switch (callstack) {
      | Finished => x
      | Continue(f, callstack) => run(f(x), callstack)
      }
    };
  };

let run = t => run(t, Finished);

// Running the stack machine (with yielding).

module Yielding = {
  /* The int is the cumulative step count across all slices so far; it feeds
   * Evaluator.yielding_step_count (total-step runaway backstop). */
  type continuation('a) =
    | Continuation(t('b), callstack('b, 'a), int): continuation('a);

  type slice('a) =
    | SliceDone('a)
    | SliceYielded(continuation('a));

  let start = t => Continuation(t, Finished, 0);

  let rec run_slice:
    type a b.
      (
        ~step_budget: int,
        ~step_counter: int,
        ~slice_counter: int,
        t(b),
        callstack(b, a)
      ) =>
      slice(a) =
    (
      ~step_budget,
      ~step_counter,
      ~slice_counter,
      t: t(b),
      callstack: callstack(b, a),
    ) =>
      if (slice_counter >= step_budget) {
        SliceYielded(Continuation(t, callstack, step_counter));
      } else {
        switch (t) {
        | Bind(t, f) =>
          run_slice(
            ~step_budget,
            ~step_counter=step_counter + 1,
            ~slice_counter=slice_counter + 1,
            t,
            Continue(f, callstack),
          )
        | Next(f) =>
          run_slice(
            ~step_budget,
            ~step_counter=step_counter + 1,
            ~slice_counter=slice_counter + 1,
            f(),
            callstack,
          )
        | Done(x) =>
          switch (callstack) {
          | Finished => SliceDone(x)
          | Continue(f, callstack) =>
            run_slice(
              ~step_budget,
              ~step_counter=step_counter + 1,
              ~slice_counter=slice_counter + 1,
              f(x),
              callstack,
            )
          }
        };
      };

  let run_slice = (~step_budget, continuation) => {
    let Continuation(t, callstack, step_counter) = continuation;
    run_slice(~step_budget, ~step_counter, ~slice_counter=0, t, callstack);
  };
};
