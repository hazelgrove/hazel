/** Synthesis and system parameters.

    Many of these options are also configurable with command-line flags.

    {b Note:} all timing units are in seconds. */;

/** The overall version of the system. */

let version: string = ("0.1.0": string);

/** Whether or not debugging should be enabled. */

let debug_mode: ref(bool) = (ref(false): ref(bool));

/** The available methods for ranking synthesis solutions. */

type ranking_method =
  | Size; /* Rank by AST size */

/** The method to rank synthesis solutions. */

let ranking_method: ref(ranking_method) = (ref(Size): ref(ranking_method));

/** The maximum synthesis solution count. [None] indicates no bound/infinity. */

let max_solution_count: ref(option(int)) = (
  ref(Some(40)): ref(option(int))
);

/** The unevaluation case budget; sets a limit to the number of times U-Case is
    allowed to be called. */

let uneval_case_budget: ref(int) = (ref(10): ref(int));

/** The unevluation limiter; immediately cuts off any unevaluation path that
    exceeds a certain number of nondeterministic possibilities (thereby
    eliminating pathological search paths that are unlikely to succeed). */

let uneval_limiter: ref(int) = (ref(10): ref(int));

/** The total time a single synthesis task is allowed to use. */

let max_total_time: ref(float) = (ref(20.0): ref(float));

/** The time a single call to evaluation is allowed to use. */

let max_eval_time: ref(float) = (ref(0.1): ref(float));

/** The time a single call to Guess is allowed to use. */

let max_guess_time: ref(float) = (ref(0.25): ref(float));

/** The initial fuel count for evaluation and resumption. */

let initial_fuel: ref(int) = (ref(100): ref(int));

/** Whether or not to enable info-level logging. */

let log_info: ref(bool) = (ref(true): ref(bool));

/** Whether or not to enable warn-level logging. */

let log_warn: ref(bool) = (ref(true): ref(bool));
