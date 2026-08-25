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
    /* replace the LAST tile labeled "9" with [digit], preserving ids
       and every other piece — the shape of a real one-keystroke edit
       deep in the program */
    let rec repl_seg =
            (digit: string, ps: list(Piece.t)): (bool, list(Piece.t)) =>
      switch (ps) {
      | [] => (false, [])
      | [p, ...rest] =>
        let (done_, rest') = repl_seg(digit, rest);
        if (done_) {
          (true, [p, ...rest']);
        } else {
          switch (p) {
          | Tile(t) when t.label == ["9"] => (
              true,
              [
                Piece.Tile({
                  ...t,
                  label: [digit],
                }),
                ...rest',
              ],
            )
          | Tile(t) =>
            let (d, kids') = repl_kids(digit, List.rev(t.children));
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
    and repl_kids = (digit, kids_rev) =>
      switch (kids_rev) {
      | [] => (false, [])
      | [k, ...rest] =>
        let (d, k') = repl_seg(digit, k);
        d
          ? (true, [k', ...rest])
          : {
            let (d2, rest') = repl_kids(digit, rest);
            (d2, [k, ...rest']);
          };
      };
    let seg1 = parse_seg(src);
    let variant = digit => {
      let (found, seg) = repl_seg(digit, seg1);
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

let tests = (
  "BenchStatics",
  [
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
