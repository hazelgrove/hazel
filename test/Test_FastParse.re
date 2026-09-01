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
          "hazel-programs/docs/livelits/graph-editor.hz",
          "../hazel-programs/docs/livelits/graph-editor.hz",
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
    | None =>
      fail(
        "fast path rejected: "
        ++ name
        ++ " — "
        ++ Option.value(FastParse.bail_note^, ~default="no note"),
      )
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
    verbatim("hole", "let x = ? in x"),
    test_case(
      "? lands as explicit tile, \xc2\xbf as Grout",
      `Quick,
      () => {
        let has = (pred, seg) => {
          let rec go = s =>
            List.exists(
              (p: Piece.t) =>
                pred(p)
                || (
                  switch (p) {
                  | Tile({children, _}) => List.exists(go, children)
                  | _ => false
                  }
                ),
              s,
            );
          go(seg);
        };
        let is_grout = (p: Piece.t) =>
          switch (p) {
          | Grout(_) => true
          | _ => false
          };
        let is_hole_tile = (p: Piece.t) =>
          switch (p) {
          | Tile({label: ["?"], _}) => true
          | _ => false
          };
        let explicit =
          FastParse.of_text(~root=Exp, "let x = ? in x") |> Option.get;
        check(bool, "? gives a tile", true, has(is_hole_tile, explicit));
        check(bool, "? gives no Grout", false, has(is_grout, explicit));
        let implicit =
          FastParse.of_text(~root=Exp, "let x = \xc2\xbf in x") |> Option.get;
        check(bool, "\xc2\xbf gives Grout", true, has(is_grout, implicit));
        check(
          bool,
          "\xc2\xbf gives no hole tile",
          false,
          has(is_hole_tile, implicit),
        );
      },
    ),
    verbatim("binding-chain fragment", "let helper = fun x -> x * 2 in"),
    verbatim(
      "multiline fragment",
      "let a = 1 in\nlet b = fun x, y ->\n  x + y\nin",
    ),
    verbatim("string with tricky content", {|let s = "a # b { c" in s|}),
    verbatim("paren tuple in exp position", "let x = (1, 2) in x"),
    verbatim(
      "funlet named-function form",
      "let f(x: Int, y: Int): Int = x + y in f(1, 2)",
    ),
    verbatim(
      "funlet module member",
      "let m = {\n  let g(x: Int): Int = x + 1\n} in m.g(2)",
    ),
    semantic("funlet named-function form", "let f(x, y) = x + y in f(1, 2)"),
    verbatim(
      "float literals keep their source spelling",
      "let x = 400.0 in let y = 250. in x +. y *. 2.",
    ),
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
    test_case(
      "fragment load regrouts: pins reprint, save is a fixed point (#2450)",
      `Quick,
      () => {
        let text = "let f = fun x -> x + 1 in\n^^probe(f(1));\n^^probe(f(2));";
        let z = PersistentZipper.from_backup_text(text, ~root=Exp);
        check(int, "two manual pins", 2, List.length(z.refractors.manuals));
        let out = String.trim(MarkerParse.to_text(z));
        check(
          bool,
          "reprint ends with the materialized hole marker",
          true,
          String.ends_with(~suffix="\xc2\xbf", out),
        );
        let z2 = PersistentZipper.from_backup_text(out, ~root=Exp);
        check(
          int,
          "pins survive reload",
          2,
          List.length(z2.refractors.manuals),
        );
        check(
          string,
          "fixed point",
          out,
          String.trim(MarkerParse.to_text(z2)),
        );
      },
    ),
    test_case(
      "refractor pins collected and reprinted",
      `Quick,
      () => {
        let text = "let a = 1 in\n^^probe(a + 1)";
        let z = PersistentZipper.from_backup_text(text, ~root=Exp);
        check(int, "one manual pin", 1, List.length(z.refractors.manuals));
        let (_, entry: ZipperBase.Refractor.entry) =
          List.hd(z.refractors.manuals);
        check(
          bool,
          "pin is a Probe",
          true,
          entry.kind == Language.ProjectorKind.Probe,
        );
        check(
          string,
          "pin reprints as its trigger",
          text,
          String.trim(MarkerParse.to_text(z)),
        );
      },
    ),
    test_case(
      "nested triggers: refractor pinned on a projector",
      `Quick,
      () => {
        let text = "let a = ^^probe(^^slider(50)) in a";
        /* the fast path itself must handle the nest, not the fallback */
        let parsed: FastParse.parsed =
          switch (
            FastParse.parsed_of_text(
              ~materialize=Triggers.invoked_projector,
              ~collect_refractors=true,
              ~root=Exp,
              text,
            )
          ) {
          | Ok(p) => p
          | Error(why) => failwith("nested trigger bailed fast path: " ++ why)
          };
        check(
          int,
          "one refractor collected",
          1,
          List.length(parsed.refractors),
        );
        check(
          bool,
          "wrapped projector materialized",
          true,
          List.exists(
            (p: Piece.t) =>
              switch (p) {
              | Tile({children, _}) =>
                List.exists(
                  List.exists((p: Piece.t) =>
                    switch (p) {
                    | Projector({kind: Slider, _}) => true
                    | _ => false
                    }
                  ),
                  children,
                )
              | _ => false
              },
            parsed.segment,
          ),
        );
        /* end-to-end: the load path pins the probe and reprints verbatim */
        let z = PersistentZipper.from_backup_text(text, ~root=Exp);
        check(int, "one manual pin", 1, List.length(z.refractors.manuals));
        check(
          string,
          "nest reprints as written",
          text,
          String.trim(MarkerParse.to_text(z)),
        );
      },
    ),
    test_case(
      "probe_table renderer option round-trips",
      `Quick,
      () => {
        let text = "let a = 1 in\n^^probe_table(a + 1)";
        let z = PersistentZipper.from_backup_text(text, ~root=Exp);
        let (_, entry: ZipperBase.Refractor.entry) =
          List.hd(z.refractors.manuals);
        check(
          bool,
          "model selects the table renderer",
          true,
          Util.StringUtil.plain_search("table", entry.model, 0) >= 0,
        );
        check(
          string,
          "_table reprints",
          text,
          String.trim(MarkerParse.to_text(z)),
        );
      },
    ),
    test_case(
      "edge whitespace: writer's final newline stripped, blank lines kept",
      `Quick,
      () => {
        /* one final newline = the writer's artifact; the second trailing
           newline and the leading blank line + indent are content */
        let text = "\n  1 + 2\n\n";
        let z = PersistentZipper.from_backup_text(text, ~root=Exp);
        check(
          string,
          "persist round-trips edge whitespace",
          text,
          PersistentZipper.persist(z).backup_text,
        );
        /* the doc-slide unchanged-check compares this print against the
           stored text minus its final newline (ScratchMode.persist) */
        let seg = Zipper.unselect_and_zip(z);
        check(
          string,
          "print equals stored text minus final newline",
          Util.StringUtil.strip_final_newline(text),
          MarkerParse.seg_to_text(~refractors=z.refractors.manuals, seg),
        );
      },
    ),
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
    test_case(
      "text slide loads with caret at the top",
      `Quick,
      () => {
        let z =
          PersistentZipper.unpersist(
            PersistentZipper.of_text("let x = 1 in\nlet y = 2 in\nx + y"),
            ~root=Exp,
          );
        let (before_caret, _) = z.relatives.siblings;
        check(bool, "nothing precedes the caret", true, before_caret == []);
      },
    ),
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
