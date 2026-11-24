open Alcotest;
open Language;
open Poly;
module M = Web.EvalResult.Model;

/* the results' status line and the ⇓ drawer show the last finished run,
   and keep it while the next one goes */
let settles = () => {
  let v = Exp.fresh(EmptyHole);
  let ok: ProgramResult.t(Exp.t) = ResultOk(v);
  let fail: ProgramResult.t(Exp.t) = ResultFail(Timeout);
  let pending: ProgramResult.t(Exp.t) = ProgramResult.evaluating;
  let probes = Sample.Map.empty;
  let failed = (s: option(M.settled)) =>
    Option.map(s, ~f=(s: M.settled) => s.failed);
  check(
    bool,
    "before any run",
    true,
    M.settle(None, pending, probes) == None,
  );
  let s = M.settle(None, ok, probes);
  check(bool, "a run", true, failed(s) == Some(None));
  check(
    bool,
    "its value",
    true,
    Option.bind(s, ~f=(s: M.settled) => s.value) == Some(v),
  );
  check(
    bool,
    "kept while one runs",
    true,
    M.settle(s, pending, probes) == s,
  );
  let s = M.settle(s, fail, probes);
  check(
    bool,
    "a failure",
    true,
    failed(s) == Some(Some(ProgramResult.Timeout)),
  );
  check(
    bool,
    "kept while one runs",
    true,
    M.settle(s, pending, probes) == s,
  );
};

let tests = (
  "ResultLine",
  [test_case("the last finished run", `Quick, settles)],
);
