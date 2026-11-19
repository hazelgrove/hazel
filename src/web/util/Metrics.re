/* The gating and bounded-history machinery every debug-panel collector shares
 * (WorkerMetrics, EvalMetrics, PerfMetrics).
 *
 * `enabled` is synced from settings once per update cycle in
 * Page.Update.calculate, and every recorder a collector exposes runs through
 * `when_enabled` — so no call site tests whether a panel is open, and nothing
 * is measured or retained while it is closed. */

/* What a collector must say about its rows: their type, and how many to keep. */
module type S = {
  type t;
  let limit: int;
};

module Make = (R: S) => {
  let enabled = ref(false);

  let sync = (~enabled as is_enabled: bool): unit => enabled := is_enabled;

  /* Run f only while this collector's panel is open. */
  let when_enabled = (f: unit => unit): unit =>
    if (enabled^) {
      f();
    };

  /* Newest first, capped at `limit`. */
  let history: ref(list(R.t)) = ref([]);

  let push = (r: R.t): unit =>
    when_enabled(() =>
      history := [r, ...Util.ListUtil.take(R.limit - 1, history^)]
    );

  /* Rewrite the rows `pred` matches. A no-op once a row has aged out of the
   * history, or when it was never recorded because the panel opened while the
   * work it describes was already in flight. */
  let update = (pred: R.t => bool, f: R.t => R.t): unit =>
    when_enabled(() =>
      history := List.map(r => pred(r) ? f(r) : r, history^)
    );
};
