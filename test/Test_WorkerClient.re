open Alcotest;
open Web;

/* WorkerClient.check_deadline: a deadline timer that fires late is postponed
   so a reply already queued is handled first, but only a bounded number of
   times, so a hidden tab (whose timers are throttled and so always late)
   still times out a runaway evaluation. */
let check = (name, ~late_ms, ~postponed, expected) =>
  test_case(name, `Quick, () =>
    Alcotest.check(
      bool,
      name,
      true,
      Poly.equal(
        WorkerClient.check_deadline(~late_ms, ~postponed),
        expected,
      ),
    )
  );

let slack = WorkerClient.late_slack_ms;
let cap = WorkerClient.max_postponements;

let tests = (
  "WorkerClient.check_deadline",
  [
    check("on time expires", ~late_ms=0., ~postponed=0, WorkerClient.Expire),
    check(
      "late within the slack expires",
      ~late_ms=slack,
      ~postponed=0,
      WorkerClient.Expire,
    ),
    check(
      "late past the slack postpones",
      ~late_ms=slack +. 1.,
      ~postponed=0,
      WorkerClient.Postpone,
    ),
    check(
      "a late grace timer postpones again below the cap",
      ~late_ms=20000.,
      ~postponed=cap - 1,
      WorkerClient.Postpone,
    ),
    check(
      "a late grace timer expires at the cap",
      ~late_ms=20000.,
      ~postponed=cap,
      WorkerClient.Expire,
    ),
    test_case(
      "throttled timers (always ~500 ms late) expire",
      `Quick,
      () => {
        /* A hidden tab: every timer, the grace timers included, fires about
           500 ms late. Count the postponements before the deadline decides. */
        let rec run = postponed =>
          switch (WorkerClient.check_deadline(~late_ms=500., ~postponed)) {
          | WorkerClient.Postpone =>
            postponed < 100 ? run(postponed + 1) : postponed
          | WorkerClient.Expire => postponed
          };
        Alcotest.check(int, "postponements", cap, run(0));
      },
    ),
  ],
);
