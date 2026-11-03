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

/* one Int literal changed, the first one found, ids kept */
let bump = (e: Language.Exp.t): Language.Exp.t => {
  let seen = ref(false);
  Language.TermBase.Exp.map_term(
    ~f_exp=
      (continue, d: Language.Exp.t) =>
        switch (d.term) {
        | Atom(Int(n)) when ! seen^ =>
          seen := true;
          {...d, term: Atom(Int(Bigint.of_int(Bigint.to_int_exn(n) + 1)))};
        | _ => continue(d)
        },
    e,
  );
};

/* the worker's reply: its sender's tables encode, the page's decode, as a
   request's do (WorkerServer: Client.encode), so here the same send, from
   tables reset for it */
let reply = send;

/* a slide's program, by its file: the request (its elaborated program) and
   the reply (its value), each first, again, and after an edit */
let run_file = (name: string, path: string) => {
  reset();
  let text = In_channel.with_open_bin(path, In_channel.input_all);
  let e = elab(text);
  let ((b1, _), t1) = ms(() => send(e));
  let ((b2, _), t2) = ms(() => send(e));
  let ((b3, _), t3) = ms(() => send(bump(e)));
  Printf.printf(
    "SHARE %s request: raw %d B | first %d B %.1f ms | again %d B %.1f ms | edited %d B %.1f ms\n",
    name,
    raw(e),
    b1,
    t1,
    b2,
    t2,
    b3,
    t3,
  );
  let value = exp => {
    let (v, _, _) = Test_PerfBudgets.work(exp);
    v;
  };
  reset();
  let exp = Test_Evaluator_Prelude.parse_exp(text);
  let v = value(exp);
  let ((r1, c1), u1) = ms(() => reply(v));
  let ((r2, c2), u2) = ms(() => reply(v));
  let v' = value(bump(exp));
  let ((r3, _), u3) = ms(() => reply(v'));
  Printf.printf(
    "SHARE %s reply: raw %d B | first %d B %.1f ms | again %d B %.1f ms, same object %b | edited: raw %d B, sent %d B %.1f ms\n",
    name,
    raw(v),
    r1,
    u1,
    r2,
    u2,
    c1 === c2,
    raw(v'),
    r3,
    u3,
  );
};

let tests = (
  "SharingProbe",
  [
    Alcotest.test_case("bytes and time across the boundary", `Quick, () =>
      List.iter(run, [50, 200, 800])
    ),
    Alcotest.test_case("a slide's program and its value", `Quick, () => {
      run_file("emotion", "hazel-programs/docs/livelits/emotion.hz");
      run_file("full-body", "hazel-programs/docs/livelits/playground/full-body.hz");
    }),
  ],
);
