open Alcotest;
open Language;
open Poly;
module M = Web.EvalResult.Model;

/* the results' status line shows the last finished run, and keeps it
   while the next one goes */
let settles = () => {
  let ok: ProgramResult.t(unit) = ResultOk();
  let fail: ProgramResult.t(unit) = ResultFail(Timeout);
  let pending: ProgramResult.t(unit) = ProgramResult.evaluating;
  check(
    bool,
    "before any run",
    true,
    Option.is_none(M.settle(None, pending)),
  );
  let s = M.settle(None, ok);
  check(bool, "a run", true, Poly.equal(s, Some(None)));
  check(bool, "kept while one runs", true, M.settle(s, pending) == s);
  let s = M.settle(s, fail);
  check(
    bool,
    "a failure",
    true,
    Poly.equal(s, Some(Some(ProgramResult.Timeout))),
  );
  check(bool, "kept while one runs", true, M.settle(s, pending) == s);
};

let tests = (
  "ResultLine",
  [test_case("the last finished run", `Quick, settles)],
);
