open Alcotest;
open Haz3lcore;
open Language;

/* Informational statics timing over the bench corpus
   (hazel-programs/bench). Always passes; timings print to the log.
   Run: bash test/run_node.sh test 'BenchStatics' */

let read_file = (path: string): option(string) =>
  switch (open_in_bin(path)) {
  | ic =>
    let n = in_channel_length(ic);
    let s = really_input_string(ic, n);
    close_in(ic);
    Some(s);
  | exception _ => None
  };

let time_statics = (src: string): option(float) =>
  switch (
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root=Exp,
      src,
    )
  ) {
  | None => None
  | Some(seg) =>
    let term = MakeTerm.go(seg).term;
    let t0 = Sys.time();
    let _ =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        term,
      );
    Some((Sys.time() -. t0) *. 1000.);
  };

/* Surgical segment edits, id-preserving — the shape of a real
   one-keystroke change. repl_last prefers the LAST occurrence,
   repl_first the FIRST. */
let rec repl_last =
        (~needle: string, ~repl: string, ps: list(Piece.t))
        : (bool, list(Piece.t)) =>
  switch (ps) {
  | [] => (false, [])
  | [p, ...rest] =>
    let (done_, rest') = repl_last(~needle, ~repl, rest);
    if (done_) {
      (true, [p, ...rest']);
    } else {
      switch (p) {
      | Tile(t) when t.label == [needle] => (
          true,
          [
            Piece.Tile({
              ...t,
              label: [repl],
            }),
            ...rest',
          ],
        )
      | Tile(t) =>
        let (d, kids') =
          repl_last_kids(~needle, ~repl, List.rev(t.children));
        d
          ? (
            true,
            [
              Piece.Tile({
                ...t,
                children: List.rev(kids'),
              }),
              ...rest',
            ],
          )
          : (false, [p, ...rest']);
      | _ => (false, [p, ...rest'])
      };
    };
  }
and repl_last_kids = (~needle, ~repl, kids_rev) =>
  switch (kids_rev) {
  | [] => (false, [])
  | [k, ...rest] =>
    let (d, k') = repl_last(~needle, ~repl, k);
    d
      ? (true, [k', ...rest])
      : {
        let (d2, rest') = repl_last_kids(~needle, ~repl, rest);
        (d2, [k, ...rest']);
      };
  };

let rec repl_first =
        (~needle: string, ~repl: string, ps: list(Piece.t))
        : (bool, list(Piece.t)) =>
  switch (ps) {
  | [] => (false, [])
  | [p, ...rest] =>
    switch (p) {
    | Tile(t) when t.label == [needle] => (
        true,
        [
          Piece.Tile({
            ...t,
            label: [repl],
          }),
          ...rest,
        ],
      )
    | Tile(t) =>
      let (d, kids') = repl_first_kids(~needle, ~repl, t.children);
      if (d) {
        (
          true,
          [
            Piece.Tile({
              ...t,
              children: kids',
            }),
            ...rest,
          ],
        );
      } else {
        let (d2, rest') = repl_first(~needle, ~repl, rest);
        (d2, [p, ...rest']);
      };
    | _ =>
      let (d, rest') = repl_first(~needle, ~repl, rest);
      (d, [p, ...rest']);
    }
  }
and repl_first_kids = (~needle, ~repl, kids) =>
  switch (kids) {
  | [] => (false, [])
  | [k, ...rest] =>
    let (d, k') = repl_first(~needle, ~repl, k);
    d
      ? (true, [k', ...rest])
      : {
        let (d2, rest') = repl_first_kids(~needle, ~repl, rest);
        (d2, [k, ...rest']);
      };
  };

let parse_seg_of = (src: string): Segment.t =>
  switch (
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root=Exp,
      src,
    )
  ) {
  | Some(seg) => seg
  | None => failwith("BENCH: parse failed")
  };

/* Statics.mk memoization probe (informational): the memo is
   Core.Memo.general with POLYMORPHIC hash+equality on
   (ana, ctx, term, probe_ids). Term ids are part of both, so the
   realistic hazard is the EDITOR flow: ids are stable across an edit
   except at the edit site. We model K one-deep-leaf variants of the
   mega program (segment-level tile-label swap, ids preserved): if the
   shallow Hashtbl.hash can't see the changed leaf, every variant
   lands in one bucket and each lookup pays a deep structural compare
   per resident version. Reparse (all-fresh ids) is also probed. */
let memo_probe = (): unit => {
  let path = "hazel-programs/mega/mega-1k.hz";
  let path =
    Sys.file_exists(path) ? path : "../hazel-programs/mega/mega-1k.hz";
  switch (read_file(path)) {
  | None => Printf.printf("MEMOBENCH: corpus unreadable\n")
  | Some(src) =>
    let parse_seg = s =>
      switch (
        FastParse.of_text(
          ~materialize=Triggers.invoked_projector,
          ~collect_refractors=true,
          ~root=Exp,
          s,
        )
      ) {
      | Some(seg) => seg
      | None => failwith("MEMOBENCH: parse failed")
      };
    let seg1 = parse_seg(src);
    let variant = digit => {
      let (found, seg) = repl_last(~needle="9", ~repl=digit, seg1);
      assert(found);
      MakeTerm.go(seg).term;
    };
    let ctx = Builtins.ctx_init(Some(Operators.default_mode));
    let time = (label, f) => {
      let t0 = Sys.time();
      let _ = f();
      Printf.printf(
        "MEMOBENCH %s: %.1fms\n",
        label,
        (Sys.time() -. t0) *. 1000.,
      );
    };
    let term1 = MakeTerm.go(seg1).term;
    let term2 = variant("8"); /* ids shared with term1, one leaf differs */
    let term_reparse = MakeTerm.go(parse_seg(src)).term; /* all-fresh ids */
    Printf.printf(
      "MEMOBENCH shallow-hash collision, id-stable edit: %b\n",
      Hashtbl.hash((ctx, term1)) == Hashtbl.hash((ctx, term2)),
    );
    Printf.printf(
      "MEMOBENCH shallow-hash collision, reparse: %b\n",
      Hashtbl.hash((ctx, term1)) == Hashtbl.hash((ctx, term_reparse)),
    );
    time("hash(term1)", () => Hashtbl.hash(term1));
    time("cold mk(term1)", () => Statics.mk(CoreSettings.on, ctx, term1));
    time("hit mk(term1) same phys", () =>
      Statics.mk(CoreSettings.on, ctx, term1)
    );
    time("miss mk(term2) id-stable variant", () =>
      Statics.mk(CoreSettings.on, ctx, term2)
    );
    time("hit mk(term2) with term1 resident", () =>
      Statics.mk(CoreSettings.on, ctx, term2)
    );
    time("hit mk(term1) with term2 resident", () =>
      Statics.mk(CoreSettings.on, ctx, term1)
    );
    /* pile up more colliding variants, then re-measure hit cost */
    List.iter(
      d => ignore(Statics.mk(CoreSettings.on, ctx, variant(d))),
      ["7", "6", "5", "4"],
    );
    time("hit mk(term1) with 5 variants resident", () =>
      Statics.mk(CoreSettings.on, ctx, term1)
    );
    time("hit mk(term2) with 5 variants resident", () =>
      Statics.mk(CoreSettings.on, ctx, term2)
    );
    /* what does a raw polymorphic compare between colliding keys cost,
       and do id-stable variants physically share subtrees (MakeTerm
       memoization), which would short-circuit it? */
    time("compare(term1, term2)", () => compare(term1, term2));
    time("compare(term1, term_reparse)", () => compare(term1, term_reparse));
    /* worst case: structurally identical, physically distinct — the
       compare must walk the ENTIRE term and return equal */
    let term1_copy = Grammar.map_exp_annotation(x => x, term1);
    time("compare(term1, deep copy) full walk", () =>
      compare(term1, term1_copy)
    );
    Printf.printf(
      "MEMOBENCH deep copy compares equal: %b\n",
      compare(term1, term1_copy) == 0,
    );
  };
};

/* DefStatics (compositional statics) benchmark + parity gate:
   - COLD: engine result must carry the same ERROR ids as whole-program
     Statics.mk (warnings are engine-corrected, counts reported only);
   - INCR non-export edit (deep digit swap): expect 1 item recomputed;
   - INCR export-type edit (first ascription Int->Bool): expect the
     users of that binding to recompute, and parity to hold on the
     edited program too. */
let defstatics_bench = (): unit =>
  List.iter(
    name => {
      let path = "hazel-programs/mega/" ++ name;
      let path =
        Sys.file_exists(path) ? path : "../hazel-programs/mega/" ++ name;
      switch (read_file(path)) {
      | None => Printf.printf("DEFSTATICS %s: <unreadable>\n", name)
      | Some(src) =>
        let settings = CoreSettings.on;
        let ctx = Builtins.ctx_init(Some(Operators.default_mode));
        let seg1 = parse_seg_of(src);
        let term1 = MakeTerm.go(seg1).term;
        let sorted = ids => List.sort_uniq(compare, ids);
        let whole = term => {
          let (map, _) = Statics.mk_unmemoized(settings, ctx, term);
          sorted(Statics.Map.error_ids(map));
        };
        let parity = (label, term, ds) => {
          let w = whole(term);
          let e = sorted(DefStatics.all_error_ids(ds));
          if (w == e) {
            Printf.printf(
              "DEFSTATICS %s %s: parity OK (%d errors)\n",
              name,
              label,
              List.length(w),
            );
          } else {
            Printf.printf(
              "DEFSTATICS %s %s: PARITY MISMATCH whole=%d engine=%d\n",
              name,
              label,
              List.length(w),
              List.length(e),
            );
          };
        };
        let time = (label, f) => {
          let t0 = Sys.time();
          let r = f();
          Printf.printf(
            "DEFSTATICS %s %s: %.0fms\n",
            name,
            label,
            (Sys.time() -. t0) *. 1000.,
          );
          r;
        };
        let ds1 =
          time("engine cold", () => DefStatics.calc(~settings, term1));
        Printf.printf(
          "DEFSTATICS %s items: %d, warnings: %d\n",
          name,
          List.length(ds1.items),
          List.length(DefStatics.all_warning_ids(ds1)),
        );
        parity("cold", term1, ds1);
        /* non-export edit: last digit 9 -> 8, deep in the program */
        let (f2, seg2) = repl_last(~needle="9", ~repl="8", seg1);
        assert(f2);
        let term2 = MakeTerm.go(seg2).term;
        let ds2 =
          time("incr non-export edit", () =>
            DefStatics.calc(~settings, ~prev=ds1, term2)
          );
        Printf.printf(
          "DEFSTATICS %s non-export analyzed: %d items\n",
          name,
          DefStatics.last_analyzed^,
        );
        parity("non-export", term2, ds2);
        /* export-type edit: first ascription Int -> Bool */
        let (f3, seg3) = repl_first(~needle="Int", ~repl="Bool", seg1);
        assert(f3);
        let term3 = MakeTerm.go(seg3).term;
        let ds3 =
          time("incr export-type edit", () =>
            DefStatics.calc(~settings, ~prev=ds1, term3)
          );
        Printf.printf(
          "DEFSTATICS %s export-type analyzed: %d items\n",
          name,
          DefStatics.last_analyzed^,
        );
        parity("export-type", term3, ds3);
        /* CROSS-MODULE cascade: retype the first selfcheck ascription
           (Bool -> String); MetaRunner consumes every selfcheck, so
           downstream items must re-analyze and new errors appear */
        let (f4, seg4) = repl_first(~needle="Bool", ~repl="String", seg1);
        assert(f4);
        let term4 = MakeTerm.go(seg4).term;
        let ds4 =
          time("incr cross-module cascade", () =>
            DefStatics.calc(~settings, ~prev=ds1, term4)
          );
        Printf.printf(
          "DEFSTATICS %s cascade analyzed: %d items\n",
          name,
          DefStatics.last_analyzed^,
        );
        parity("cascade", term4, ds4);
      };
    },
    ["mega-1k.hz", "mega-4k.hz"],
  );

let tests = (
  "BenchStatics",
  [
    test_case(
      "DefStatics compositional (informational)",
      `Quick,
      defstatics_bench,
    ),
    test_case("Statics.mk memoization (informational)", `Quick, memo_probe),
    test_case("corpus statics timing (informational)", `Quick, () =>
      List.iter(
        name => {
          let path = "hazel-programs/bench/" ++ name;
          let path =
            Sys.file_exists(path) ? path : "../hazel-programs/bench/" ++ name;
          switch (read_file(path)) {
          | None => Printf.printf("BENCHSTATICS %s: <unreadable>\n", name)
          | Some(src) =>
            switch (time_statics(src)) {
            | Some(ms) =>
              Printf.printf(
                "BENCHSTATICS %s (%d lines): %.0fms\n",
                name,
                List.length(String.split_on_char('\n', src)),
                ms,
              )
            | None => Printf.printf("BENCHSTATICS %s: <no parse>\n", name)
            }
          };
        },
        ["bench-1k.hz", "bench-2k5.hz", "bench-5k.hz"],
      )
    ),
  ],
);
