/* M1: Blackboard as an editor sort.  These tests type Blackboard text
   through the editor's own molding (Parser.to_zipper is the insertion
   path, one character at a time), read the resulting tiles as a term, and
   check the round trip back to text through the pretty printer.  The
   kernel tests in Test_Blackboard.re cover the checker itself. */

open Alcotest;
open Haz3lcore;
open Language;

/* Parse in the Bb sort and return the Blackboard term. */
let parse_bb = (code: string): Bb.Term.t => {
  let root = Sort.Bb(BbSort.Term);
  switch (Parser.to_zipper(~root, code)) {
  | None => failf("Parser.to_zipper failed for %S", code)
  | Some(z) =>
    switch (
      MakeTerm.go_s(
        root,
        Segment.skel(Dump.to_segment(z, ~root)),
        Dump.to_segment(z, ~root),
      )
    ) {
    | Bb(b) => b
    | other =>
      failf(
        "expected a Blackboard term for %S, got sort %s",
        code,
        Any.show(other),
      )
    }
  };
};

let kernel = (code: string): BbTerm.t =>
  switch (Bb.term_to_kernel(parse_bb(code))) {
  | Ok(t) => t
  | Error({message, _}) =>
    failf("could not read %S as a term: %s", code, message)
  };

let print = (t: Bb.Term.t): string =>
  ExpToSegment.any_to_segment(
    ~settings=ExpToSegment.Settings.of_core(~inline=true, CoreSettings.on),
    Bb(t),
  )
  |> Printer.of_segment(~holes="?", ~refractors=[]);

let tests = (
  "Blackboard editor",
  [
    test_case(
      "a name and the type of types",
      `Quick,
      () => {
        check(bool, "x", true, kernel("x") == BbTerm.Var("x"));
        check(bool, "type", true, kernel("type") == BbTerm.Type);
      },
    ),
    test_case("membership", `Quick, () =>
      check(
        bool,
        "t : T",
        true,
        kernel("t : T") == BbTerm.Mem(Var("t"), Var("T")),
      )
    ),
    test_case("the non-dependent arrow", `Quick, () =>
      check(
        bool,
        "A -> B",
        true,
        kernel("A -> B") == BbTerm.Pi("_", Var("A"), Var("B")),
      )
    ),
    test_case("the dependent arrow is a parenthesized membership", `Quick, () =>
      check(
        bool,
        "(x : A) -> B",
        true,
        kernel("(x : A) -> B") == BbTerm.Pi("x", Var("A"), Var("B")),
      )
    ),
    test_case("the arrow is right-associative", `Quick, () =>
      check(
        bool,
        "A -> B -> C",
        true,
        kernel("A -> B -> C")
        == BbTerm.Pi("_", Var("A"), Pi("_", Var("B"), Var("C"))),
      )
    ),
    test_case(
      "application, curried through an argument list",
      `Quick,
      () => {
        check(
          bool,
          "f(a)",
          true,
          kernel("f(a)") == BbTerm.App(Var("f"), Var("a")),
        );
        check(
          bool,
          "eq(A, a1, a2)",
          true,
          kernel("eq(A, a1, a2)")
          == BbTerm.App(
               App(App(Var("eq"), Var("A")), Var("a1")),
               Var("a2"),
             ),
        );
      },
    ),
    test_case("subtyping, as the paper writes it at l. 234", `Quick, () =>
      check(
        bool,
        "(x : A) -> x : B",
        true,
        kernel("(x : A) -> x : B")
        == BbTerm.Pi("x", Var("A"), Mem(Var("x"), Var("B"))),
      )
    ),
    test_case("a signature entry", `Quick, () =>
      switch (Bb.entry_to_kernel(parse_bb("refl : (A : type) -> A"))) {
      | Ok({name: "refl", ty}) =>
        check(
          bool,
          "the entry's type",
          true,
          ty == BbTerm.Pi("A", Type, Var("A")),
        )
      | Ok({name, _}) => failf("wrong entry name %S", name)
      | Error({message, _}) => failf("not an entry: %s", message)
      }
    ),
    test_case(
      "an assumption block becomes a document",
      `Quick,
      () => {
        let b = parse_bb("assume x : type; y : x by tychk");
        switch (Bb.doc_to_kernel(b)) {
        | Ok([Assume([{name: "x", _}, {name: "y", _}], Some("tychk"))]) =>
          ()
        | Ok(d) => failf("unexpected document: %s", BbTerm.show_doc(d))
        | Error({message, _}) => failf("not a document: %s", message)
        };
      },
    ),
    test_case("a construction block keeps its tactic", `Quick, () =>
      switch (Bb.doc_to_kernel(parse_bb("construct f : type by definition"))) {
      | Ok([Construct([{name: "f", _}], "definition")]) => ()
      | Ok(d) => failf("unexpected document: %s", BbTerm.show_doc(d))
      | Error({message, _}) => failf("not a document: %s", message)
      }
    ),
    test_case(
      "the checker runs on tiles the user typed",
      `Quick,
      () => {
        let b =
          parse_bb(
            "assume eq : (A : type) -> (a1 : A) -> (a2 : A) -> type; bad : B by tychk",
          );
        switch (Bb.doc_to_kernel(b)) {
        | Error({message, _}) => failf("not a document: %s", message)
        | Ok(d) =>
          let errs = BbCheck.all_errors(snd(BbCheck.check_doc(d)));
          check(
            bool,
            "B is unbound, and nothing else is wrong",
            true,
            List.map((l: BbError.located) => (l.entry, l.err), errs)
            == [("bad", BbError.Unbound("B"))],
          );
        };
      },
    ),
    /* Printing is a fixed point: printing what was parsed from printed text
       gives the same text back. */
    test_case("text round-trips through the pretty printer", `Quick, () =>
      List.iter(
        code => {
          let once = print(parse_bb(code));
          let twice = print(parse_bb(once));
          check(
            string,
            Printf.sprintf("%S prints as %S", code, once),
            once,
            twice,
          );
        },
        [
          "type",
          "t : T",
          "A -> B",
          "(x : A) -> B",
          "(x : A) -> x : B",
          "eq(A, a1, a2)",
          "assume x : type by tychk",
          "assume x : type; y : x by tychk; construct f : type by definition",
        ],
      )
    ),
    test_case(
      "a document embedded in an expression",
      `Quick,
      () => {
        let root = Sort.Exp;
        let code = "blackboard assume x : type by tychk end";
        switch (Parser.to_zipper(~root, code)) {
        | None => failf("Parser.to_zipper failed for %S", code)
        | Some(z) =>
          let e = MakeTerm.from_zip_for_sem(z, ~root).term;
          switch (IdTagged.term_of(e)) {
          | BbQuote(b) =>
            switch (Bb.doc_to_kernel(b)) {
            | Ok([Assume([{name: "x", _}], Some("tychk"))]) => ()
            | Ok(d) => failf("unexpected document: %s", BbTerm.show_doc(d))
            | Error({message, _}) => failf("not a document: %s", message)
            }
          | _ => failf("expected a blackboard block, got %s", Exp.show(e))
          };
        };
      },
    ),
  ],
);
