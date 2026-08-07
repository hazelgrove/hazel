open Alcotest;
open Haz3lcore;

/* The fast path's contract: on success, the zipped segment IS the source —
   same tokens, same whitespace, same comments — with molds from
   ExpToSegment. Printing it back must reproduce the input verbatim, and
   the editor's own reader (MakeTerm) must see the same term it would have
   seen through the typing parser. */

let print_seg = seg => Printer.of_segment(~holes="?", ~refractors=[], seg);

let graph_module = {
  let src =
    switch (
      List.find_opt(
        Sys.file_exists,
        [
          "hazel-programs/livelits/graph-editor.hz",
          "../hazel-programs/livelits/graph-editor.hz",
        ],
      )
    ) {
    | Some(p) =>
      let ic = open_in_bin(p);
      let n = in_channel_length(ic);
      let s = really_input_string(ic, n);
      close_in(ic);
      Some(s);
    | None => None
    };
  src;
};

let verbatim = (name, txt) =>
  test_case(name, `Quick, () => {
    switch (FastParse.of_text(~root=Exp, txt)) {
    | None => fail("fast path rejected: " ++ name)
    | Some(seg) =>
      check(
        testable(Fmt.string, String.equal),
        "verbatim roundtrip: " ++ name,
        txt,
        print_seg(seg),
      )
    }
  });

let semantic = (name, txt) =>
  test_case(
    name ++ " (term parity)",
    `Quick,
    () => {
      let fast_term =
        switch (FastParse.of_text(~root=Exp, txt)) {
        | Some(seg) => Some(MakeTerm.go(seg).term)
        | None => None
        };
      let slow_term =
        switch (Parser.to_segment(txt, ~root=Exp)) {
        | Some(seg) => Some(MakeTerm.go(seg).term)
        | None => None
        };
      switch (fast_term, slow_term) {
      | (Some(f), Some(s)) =>
        check(
          bool,
          "MakeTerm reads both segments identically: " ++ name,
          true,
          Language.Equality.(
            equality({
              ...syntactic_settings,
              ignore_parens: false,
            }).
              exp
          )(
            f,
            s,
          ),
        )
      | _ => fail("a parser rejected: " ++ name)
      };
    },
  );

let rejected = (name, txt) =>
  test_case(name ++ " (falls back)", `Quick, () => {
    check(
      bool,
      "fast path bails: " ++ name,
      true,
      FastParse.of_text(~root=Exp, txt) == None,
    )
  });

let tests = (
  "FastParse",
  [
    verbatim("simple binding", "let x = 1 in x + 1"),
    verbatim(
      "multiline with comment",
      "let x = 1 in # a comment #\nlet y = 2 in\n\nx + y",
    ),
    verbatim(
      "module with members",
      "let m = {\n  let a = 1;\n\n  let b = fun x, y -> x + y\n} in m",
    ),
    verbatim(
      "livelit module",
      "let ^p = { let init = 50; let update = fun (m, a) : (Int, Int) -> a } in ^p.update((^p.init, 3))",
    ),
    verbatim("hole", "let x = ? in x"),
    verbatim("binding-chain fragment", "let helper = fun x -> x * 2 in"),
    verbatim(
      "multiline fragment",
      "let a = 1 in\nlet b = fun x, y ->\n  x + y\nin",
    ),
    verbatim("string with tricky content", {|let s = "a # b { c" in s|}),
    verbatim("paren tuple in exp position", "let x = (1, 2) in x"),
    verbatim(
      "type alias keeps aliased-type parens",
      "type Model = ([Int], [(Int, Int)]) in 1",
    ),
    semantic("simple binding", "let x = 1 in x + 1"),
    semantic(
      "type alias with paren tuple type",
      "type T = (Int, Bool) in let x : T = (1, true) in x",
    ),
    semantic("module with members", "let m = { let a = 1; let b = 2 } in m"),
    semantic("case with ctor pats", "case a | Down(x, y) => x | Up => 0 end"),
    rejected("unbalanced brace", "let m = { let a = 1 in m"),
    test_case("projector trigger materializes", `Quick, () => {
      switch (
        FastParse.of_text(
          ~materialize=Triggers.invoked_projector,
          ~root=Exp,
          "let x = ^^slider(50) in x",
        )
      ) {
      | None =>
        fail(
          "trigger should fast-path: "
          ++ Option.value(FastParse.bail_note^, ~default=""),
        )
      | Some(seg) =>
        let has_projector =
          List.exists(
            (p: Piece.t) =>
              switch (p) {
              | Tile({children, _}) =>
                List.exists(
                  List.exists(q =>
                    switch ((q: Piece.t)) {
                    | Projector(_) => true
                    | _ => false
                    }
                  ),
                  children,
                )
              | Projector(_) => true
              | _ => false
              },
            seg,
          );
        check(bool, "projector piece present", true, has_projector);
      }
    }),
    test_case("refractor trigger still bails", `Quick, () => {
      check(
        bool,
        "probe trigger bails",
        true,
        FastParse.of_text(
          ~materialize=Triggers.invoked_projector,
          ~root=Exp,
          "let x = ^^probe(1 + 2) in x",
        )
        == None,
      )
    }),
    test_case("mod root wraps and unwraps", `Quick, () => {
      check(
        bool,
        "member chunk fast-paths at Mod root",
        true,
        FastParse.of_text(~root=Mod, "let x = 1") != None,
      )
    }),
    test_case("graph module chunk: verbatim + fast", `Quick, () => {
      switch (graph_module) {
      | None => () /* corpus unreachable (sandboxed) */
      | Some(src) =>
        /* the module chunk (the realistic agent insert), without the
           ^^livelit uses that trail it */
        let start =
          Str.search_forward(Str.regexp_string("let ^graph"), src, 0);
        let stop =
          Str.search_forward(Str.regexp_string("} in"), src, start) + 4;
        let src = String.sub(src, start, stop - start) ++ " ?";
        let t0 = Sys.time();
        switch (FastParse.of_text(~root=Exp, src)) {
        | None => fail("fast path rejected the graph program")
        | Some(seg) =>
          let ms = (Sys.time() -. t0) *. 1000.;
          Printf.printf("FASTPARSE-PERF: graph program in %.1fms\n", ms);
          check(
            testable(Fmt.string, String.equal),
            "graph module chunk verbatim",
            src,
            print_seg(seg),
          );
          check(bool, "under 250ms", true, ms < 250.);
        };
      }
    }),
  ],
);
