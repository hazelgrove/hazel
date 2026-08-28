open Alcotest;
open Haz3lcore;
open Language;

/* Microbench: statics cost of a labeled-tuple-heavy module (the
   measured hot item class at mega scale) vs an unlabeled control.
   Prints per-iteration Statics.mk wall time; run under node
   --cpu-prof for attribution.
     bash test/run_node.sh test 'LabelBench' */

let read_file = (path: string): option(string) =>
  switch (open_in_bin(path)) {
  | ic =>
    let n = in_channel_length(ic);
    let s = really_input_string(ic, n);
    close_in(ic);
    Some(s);
  | exception _ => None
  };

let slice_lines = (src: string, lo: int, hi: int): string =>
  String.split_on_char('\n', src)
  |> List.filteri((i, _) => i + 1 >= lo && i + 1 <= hi)
  |> String.concat("\n");

let bench = (label: string, src: string, iters: int) => {
  switch (ParsedCorpus.to_segment(~root=Exp, src)) {
  | None => fail("unparseable: " ++ label)
  | Some(seg) =>
    let term = MakeTerm.go(seg).term;
    let ctx0 = Builtins.ctx_init(Some(Operators.default_mode));
    /* warm */
    let _ = Statics.mk_unmemoized(CoreSettings.on, ctx0, term);
    let t0 = Sys.time();
    for (_ in 1 to iters) {
      ignore(Statics.mk_unmemoized(CoreSettings.on, ctx0, term));
    };
    let dt = (Sys.time() -. t0) /. float_of_int(iters) *. 1000.0;
    Printf.printf("LABELBENCH %s: %.1f ms/iter\n", label, dt);
  };
};

/* the real in-situ path: DefStatics.calc incremental cost for a
   one-item edit on full mega-4k (the sweep measures this at ~1s) */
let insitu = () => {
  let path = "hazel-programs/mega/mega-4k.hz";
  let path = Sys.file_exists(path) ? path : "../" ++ path;
  switch (read_file(path)) {
  | None => fail("corpus unreadable")
  | Some(src) =>
    let parse = txt =>
      FastParse.of_text(
        ~materialize=Triggers.invoked_projector,
        ~collect_refractors=true,
        ~root=Exp,
        txt,
      )
      |> Option.get;
    let seg = parse(src);
    let settings = CoreSettings.on;
    let term = MakeTerm.go(seg).term;
    let t0 = Sys.time();
    let ds0 = DefStatics.calc(~settings, term);
    Printf.printf(
      "INSITU cold calc: %.0f ms, %d items\n",
      (Sys.time() -. t0) *. 1000.0,
      DefStatics.last_analyzed^,
    );
    /* a SURGICAL one-item edit: rewrite the "16" literal tile inside
       SmithWorks to "17" IN PLACE — every id (incl. all binders) is
       preserved, exactly like a real editor edit after the
       remold_regrout identity restore */
    let rec edit_piece = (p: Piece.t): Piece.t =>
      switch (p) {
      | Tile(t) when t.label == ["16"] =>
        Tile({
          ...t,
          id: Id.mk(), /* real edits mint fresh ids for typed pieces */
          label: ["17"],
        })
      | Tile(t) =>
        Tile({
          ...t,
          children: List.map(List.map(edit_piece), t.children),
        })
      | p => p
      };
    let spliced = List.map(edit_piece, seg);
    let term2 = MakeTerm.go(spliced).term;
    let t1 = Sys.time();
    let ds1 = DefStatics.calc(~settings, ~prev=ds0, term2);
    Printf.printf(
      "INSITU incr calc (1-item edit): %.0f ms, %d items analyzed\n",
      (Sys.time() -. t1) *. 1000.0,
      DefStatics.last_analyzed^,
    );
    ignore(ds1);
    /* the FULL init_compositional path (what the browser Force frame
       runs): whole_elab graft + error/warning folds + targets on top
       of calc */
    let t2 = Sys.time();
    let cs0 =
      CachedStatics.init_compositional_term(
        ~settings,
        ~probe_ids=Id.Map.empty,
        term,
      );
    Printf.printf(
      "INSITU cold init_compositional_term: %.0f ms\n",
      (Sys.time() -. t2) *. 1000.0,
    );
    let t3 = Sys.time();
    let cs1 =
      CachedStatics.init_compositional_term(
        ~settings,
        ~probe_ids=Id.Map.empty,
        term2,
      );
    Printf.printf(
      "INSITU incr init_compositional_term: %.0f ms, %d items analyzed\n",
      (Sys.time() -. t3) *. 1000.0,
      DefStatics.last_analyzed^,
    );
    ignore(cs0);
    ignore(cs1);
    check(bool, "ran", true, true);
  };
};

let case = () => {
  let path = "hazel-programs/mega/mega-4k.hz";
  let path = Sys.file_exists(path) ? path : "../" ++ path;
  switch (read_file(path)) {
  | None => fail("corpus unreadable")
  | Some(src) =>
    /* SmithWorks: labeled-tuple Model, the hot class */
    let smith = slice_lines(src, 1906, 1945) ++ "\n1";
    /* Text: comparable-size module with NO labeled tuples */
    let text = slice_lines(src, 4, 30) ++ "\n1";
    bench("module Text (no labels, ~27 lines)", text, 10);
    bench("module SmithWorks (labeled, ~40 lines)", smith, 10);
    check(bool, "ran", true, true);
  };
};

let tests = (
  "LabelBench",
  [
    test_case("labeled module statics", `Quick, case),
    test_case("in-situ incremental calc", `Quick, insitu),
  ],
);
