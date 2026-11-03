/* (probe, ^ticket(666)) What the worker boundary costs with ShareCodec, on
   Test_ShareCodec's program of n definitions: the bytes Marshal sends with
   and without sharing, a first send, the same value again, and after an
   edit to one literal (ids kept, as an edit does); the ms of each; and
   whether the value sent again decodes to the very object it did before.
   Prints one line per size; asserts nothing. */
open Test_ShareCodec;

let ms = (f: unit => 'a): ('a, float) => {
  let t0 = Sys.time();
  let r = f();
  (r, (Sys.time() -. t0) *. 1000.);
};

let raw = (v: Language.Exp.t): int =>
  String.length(Marshal.to_string(v, []));

let run = (n: int) => {
  reset();
  let e = elab(program(n));
  let ((b1, d1), t1) = ms(() => send(e));
  let ((b2, d2), t2) = ms(() => send(e));
  let e' = edited(e);
  let ((b3, _), t3) = ms(() => send(e'));
  Printf.printf(
    "SHARE n=%d: raw %d B | first %d B %.1f ms | again %d B %.1f ms, same object %b | edited: raw %d B, sent %d B %.1f ms\n",
    n,
    raw(e),
    b1,
    t1,
    b2,
    t2,
    d1 === d2,
    raw(e'),
    b3,
    t3,
  );
};

let tests = (
  "SharingProbe",
  [
    Alcotest.test_case("bytes and time across the boundary", `Quick, () =>
      List.iter(run, [50, 200, 800])
    ),
  ],
);
