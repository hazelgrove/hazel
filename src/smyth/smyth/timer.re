let now = () => Unix.gettimeofday();

module Single = {
  type t =
    | Total
    | Eval;

  let (total, eval) = (ref(0.0), ref(0.0));

  let info = timer =>
    switch (timer) {
    | Total => (total, Params.max_total_time^)
    | Eval => (eval, Params.max_eval_time^)
    };

  let start = timer => {
    let (initial, _) = info(timer);

    initial := now();
  };

  let elapsed = timer => {
    let (initial, _) = info(timer);

    now() -. initial^;
  };

  let check = timer => {
    let (_, cutoff) = info(timer);

    elapsed(timer) < cutoff;
  };
};

module Multi = {
  type t =
    | Guess;

  let guess = ref(0.0);

  let info = timer =>
    switch (timer) {
    | Guess => (guess, Params.max_guess_time^)
    };

  let reset = timer => {
    let (time_taken, _) = info(timer);

    time_taken := 0.0;
  };

  let accumulate = (timer, computation) => {
    let initial_time = now();

    let output = computation();

    let final_time = now();

    let (time_taken, _) = info(timer);

    time_taken := time_taken^ +. (final_time -. initial_time);
    output;
  };

  let check = timer => {
    let (time_taken, max_time) = info(timer);

    time_taken^ < max_time;
  };
};

exception Timeout(string);

/* Does NOT nest!!! Does NOT work with JavaScript!!! */
let itimer_timeout = (unique_id, cutoff, f, arg, default_value) =>
  if (Compilation2.is_js) {
    (f(arg), 0.0, false);
  } else if (cutoff <= 0.0) {
    (default_value, 0.0, true);
  } else {
    Sys.set_signal(
      Sys.sigalrm,
      Sys.Signal_handle(_ => raise(Timeout(unique_id))),
    );
    ignore(
      Unix.setitimer(
        Unix.ITIMER_REAL,
        Unix.{it_value: cutoff, it_interval: 0.0},
      ),
    );
    try({
      let res = f(arg);

      let time_elapsed = cutoff -. Unix.(getitimer(ITIMER_REAL).it_value);

      (res, time_elapsed, false);
    }) {
    | exc =>
      if (exc == Timeout(unique_id)) {
        let time_elapsed = cutoff -. Unix.(getitimer(ITIMER_REAL).it_value);

        (default_value, time_elapsed, true);
      } else {
        raise(exc);
      }
    };
  };
