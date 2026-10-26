/* Util.ShareCodec: a request carries only the values the worker does not
   hold yet, the rest as references by Merkle hash. The client and the
   worker tables live in one JS runtime here, with Marshal between them as
   on the real boundary. */
open Alcotest;
open Language;

let elab = text =>
  switch (Haz3lcore.PersistentZipper.parse_text(~source="t", ~root=Exp, text)) {
  | None => fail("did not parse")
  | Some(z) =>
    let Haz3lcore.MakeTerm.{term, _} =
      Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp);
    snd(
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        term,
      ),
    );
  };

/* across the boundary: what the client sends, as the worker gets it */
let send = (v: Exp.t): (int, Exp.t) => {
  let wire = Marshal.to_string(Util.ShareCodec.Client.encode(v), []);
  (
    String.length(wire),
    Util.ShareCodec.Worker.decode(Marshal.from_string(wire, 0)),
  );
};

let program = n =>
  String.concat(
    "",
    List.init(n, i =>
      Printf.sprintf(
        "let f%d = fun x -> (x + %d, [x, x * 2, x * 3], \"s%d\") in\n",
        i,
        i,
        i,
      )
    ),
  )
  ++ "f1(7)";

/* the same program with one literal changed, ids kept (as an edit does) */
let edited = (e: Exp.t): Exp.t =>
  TermBase.Exp.map_term(
    ~f_exp=
      (continue, d: Exp.t) =>
        switch (d.term) {
        | Atom(Int(n)) when Bigint.to_string(n) == "7" => {
            ...d,
            term: Atom(Int(Bigint.of_int(8))),
          }
        | _ => continue(d)
        },
    e,
  );

let reset = () => {
  Util.ShareCodec.Client.reset();
  Util.ShareCodec.Worker.reset();
};

let tests = (
  "ShareCodec",
  [
    test_case(
      "a value arrives equal to what was sent",
      `Quick,
      () => {
        reset();
        let e = elab(program(20));
        let (_, got) = send(e);
        check(bool, "equal", true, Exp.fast_equal(e, got));
        check(bool, "same build", true, Util.ValueHash.same(e, got));
      },
    ),
    test_case(
      "a second request sends only what changed",
      `Quick,
      () => {
        reset();
        let e = elab(program(40));
        let (first, got1) = send(e);
        let e2 = edited(e);
        let (second, got2) = send(e2);
        check(bool, "second much smaller", true, second * 5 < first);
        check(
          bool,
          "equal to what was sent",
          true,
          Util.ValueHash.same(e2, got2),
        );
        check(
          bool,
          "the edit arrived",
          false,
          Util.ValueHash.same(got1, got2),
        );
        /* the unchanged definitions are the very values the worker kept */
        let first_def = (d: Exp.t) =>
          switch (d.term) {
          | Let(_, def, _) => def
          | _ => fail("not a let")
          };
        check(
          bool,
          "shared, not copied",
          true,
          first_def(got1) === first_def(got2),
        );
      },
    ),
    test_case(
      "a reference the worker does not hold is an error, not a wrong value",
      `Quick,
      () => {
        reset();
        let e = elab(program(10));
        ignore(send(e));
        Util.ShareCodec.Worker.reset(); /* as a new worker */
        let wire =
          Marshal.to_string(Util.ShareCodec.Client.encode(edited(e)), []);
        check(
          bool,
          "raises",
          true,
          switch (
            Util.ShareCodec.Worker.decode(Marshal.from_string(wire, 0))
          ) {
          | _ => false
          | exception _ => true
          },
        );
      },
    ),
    test_case(
      "after a reset, everything is sent again",
      `Quick,
      () => {
        reset();
        let e = elab(program(10));
        let (first, _) = send(e);
        reset();
        let (again, got) = send(e);
        check(int, "same size as the first time", first, again);
        check(bool, "equal", true, Util.ValueHash.same(e, got));
      },
    ),
  ],
);
