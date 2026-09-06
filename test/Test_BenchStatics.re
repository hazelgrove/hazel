open Alcotest;
open Haz3lcore;
open Language;

/* Informational statics timing over the bench corpus
   (hazel-programs/bench). Always passes; timings print to the log.
   Run: bash test/run_node.sh test 'BenchStatics' */

let read_file = CorpusUtil.read_file;

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
          /* a print-only mismatch slipped through a whole stage
             (member chains ignored incoming dirty names); ASSERT */
          check(bool, name ++ " " ++ label ++ " error parity", true, w == e);
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
        {
          /* grafted-elaboration parity: evaluating the graft must give
             the same value as evaluating the monolithic elaboration */

          let (_, mono_elab) = Statics.mk_unmemoized(settings, ctx, term1);
          switch (DefStatics.whole_elab(ds1)) {
          | None => Printf.printf("DEFSTATICS %s graft: SHAPE GAP\n", name)
          | Some(graft_elab) =>
            let (v1, _) =
              Evaluator.evaluate(~env=Builtins.env_init, mono_elab);
            let (v2, _) =
              Evaluator.evaluate(~env=Builtins.env_init, graft_elab);
            Printf.printf(
              "DEFSTATICS %s graft-eval parity: %b\n",
              name,
              Exp.fast_equal(v1, v2),
            );
          };
        };
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

/* Slide-load pipeline probe: run each stage of the browser's
   Calculate under whatever stack node was launched with. Chrome's
   renderer stack is ~1MB; run_node.sh uses 8MB — to find what
   overflows in-browser, run this manually WITHOUT --stack-size:
     IDB_STUB=... TEST_JS=... node --require $IDB_STUB $TEST_JS \
       test BenchStatics 3 */
exception Bail;

let load_pipeline_probe = (): unit =>
  List.iter(
    name => {
      let path = "hazel-programs/mega/" ++ name;
      let path =
        Sys.file_exists(path) ? path : "../hazel-programs/mega/" ++ name;
      switch (read_file(path)) {
      | None => Printf.printf("LOADPIPE %s: <unreadable>\n", name)
      | Some(src) =>
        let stage = (label, f) => {
          let t0 = Sys.time();
          switch (f()) {
          | r =>
            Printf.printf(
              "LOADPIPE %s %s: %.0fms\n",
              name,
              label,
              (Sys.time() -. t0) *. 1000.,
            );
            r;
          | exception e =>
            Printf.printf(
              "LOADPIPE %s %s: RAISED %s\n",
              name,
              label,
              Printexc.to_string(e),
            );
            raise(Bail);
          };
        };
        try({
          let seg = stage("parse", () => parse_seg_of(src));
          let z = stage("unzip", () => Zipper.unzip(seg));
          let mt = stage("maketerm", () => MakeTerm.go(seg));
          let ctx = Builtins.ctx_init(Some(Operators.default_mode));
          let (map, elab) =
            switch (Statics.mk_unmemoized(CoreSettings.on, ctx, mt.term)) {
            | r =>
              Printf.printf("LOADPIPE %s statics+elab: ok\n", name);
              r;
            | exception e =>
              Printf.printf(
                "LOADPIPE %s statics+elab: RAISED %s — bisecting by item\n",
                name,
                Printexc.to_string(e),
              );
              let nodes = DefStatics.chain(mt.term);
              let _ =
                List.fold_left(
                  (ctx, node) =>
                    switch (
                      DefStatics.calc_item(
                        ~settings=CoreSettings.on,
                        ~ctx_in=ctx,
                        node,
                      )
                    ) {
                    | it =>
                      Printf.printf(
                        "LOADPIPE %s   item %s: ok\n",
                        name,
                        switch (it.d_exports) {
                        | [e, ..._] => DefStatics.entry_name(e)
                        | [] => "<tail>"
                        },
                      );
                      it.d_ctx_out;
                    | exception e2 =>
                      Printf.printf(
                        "LOADPIPE %s   item OVERFLOWS: %s\n",
                        name,
                        Printexc.to_string(e2),
                      );
                      ctx;
                    },
                  ctx,
                  nodes,
                );
              raise(Bail);
            };
          let _syn = stage("cachedsyntax", () => CachedSyntax.init(z));
          let ei =
            stage("evalinfo", () =>
              EvalInfo.of_info_map(
                ~probe_all=false,
                ~targets=Sample.no_targets,
                map,
              )
            );
          let _ =
            stage("evaluate plain", () =>
              Evaluator.evaluate(~env=Builtins.env_init, elab)
            );
          let _ =
            stage("evaluate w/ eval_info", () =>
              Evaluator.evaluate(~eval_info=ei, ~env=Builtins.env_init, elab)
            );
          ();
        }) {
        | Bail => ()
        };
      };
    },
    ["mega-1k.hz", "mega-2k.hz", "mega-4k.hz"],
  );

/* TEMP: how big/slow was shipping `prev` to the worker? Marshal the
   request payload with and without the incremental cache, for a mega
   program after one evaluation. */
let payload_probe = (): unit => {
  let path = "hazel-programs/mega/mega-1k.hz";
  let path =
    Sys.file_exists(path) ? path : "../hazel-programs/mega/mega-1k.hz";
  switch (read_file(path)) {
  | None => Printf.printf("PAYLOAD: corpus unreadable\n")
  | Some(src) =>
    let seg = parse_seg_of(src);
    let term = MakeTerm.go(seg).term;
    let ctx = Builtins.ctx_init(Some(Operators.default_mode));
    let (map, elab) = Statics.mk_unmemoized(CoreSettings.on, ctx, term);
    let ei =
      EvalInfo.of_info_map(~probe_all=false, ~targets=Sample.no_targets, map);
    let (_, state) =
      Evaluator.evaluate(~eval_info=ei, ~env=Builtins.env_init, elab);
    let prev = state.EvaluatorState.incr_eval;
    let time_size = (label, v) => {
      let t0 = Sys.time();
      let s = Marshal.to_string(v, []);
      Printf.printf(
        "PAYLOAD %s: %d KB, %.1fms to marshal\n",
        label,
        String.length(s) / 1024,
        (Sys.time() -. t0) *. 1000.,
      );
    };
    time_size("expr+eval_info (kept)", (elab, ei));
    time_size("prev cache (no longer shipped)", prev);
    time_size("old full request", (elab, ei, prev));
    /* the #2368 crash shape: big computed VALUES live only in the
       cache (not shared with the elab) */
    let src2 = "let x = range(1, 20000) in length(x)";
    let seg2 = parse_seg_of(src2);
    let term2 = MakeTerm.go(seg2).term;
    let (map2, elab2) = Statics.mk_unmemoized(CoreSettings.on, ctx, term2);
    let ei2 =
      EvalInfo.of_info_map(
        ~probe_all=false,
        ~targets=Sample.no_targets,
        map2,
      );
    let (_, state2) =
      Evaluator.evaluate(~eval_info=ei2, ~env=Builtins.env_init, elab2);
    let prev2 = state2.EvaluatorState.incr_eval;
    time_size("BIG-VALUE expr+eval_info (kept)", (elab2, ei2));
    time_size("BIG-VALUE prev cache (no longer shipped)", prev2);
    time_size("BIG-VALUE old full request", (elab2, ei2, prev2));
  };
};

/* Probe-capture parity: a probe on a fn-body var whose only call site
   is a LATER top-level item must sample under compositional statics +
   grafted elaboration exactly as under monolithic statics. Fresh
   evaluations — no incremental cache — so this isolates capture from
   reuse. */
let probe_capture_parity = (): unit => {
  let settings = CoreSettings.on;
  let ctx = Builtins.ctx_init(Some(Operators.default_mode));
  let src = "let f = fun q -> q + 1 in\nlet z = f(5) in\nz";
  let seg = parse_seg_of(src);
  let term = MakeTerm.go(seg).term;
  let (map0, _) = Statics.mk_unmemoized(settings, ctx, term);
  let q_id =
    Id.Map.fold(
      (id, info, acc) =>
        switch (acc) {
        | Some(_) => acc
        | None =>
          switch (info) {
          | Info.InfoExp({user_term: {term: Var("q"), _}, _}) => Some(id)
          | _ => None
          }
        },
      map0,
      None,
    );
  switch (q_id) {
  | None => Printf.printf("PROBECAP: no Var(q) found\n")
  | Some(q_id) =>
    let probe_ids = Id.Map.singleton(q_id, ());
    let capture_count = (info_map, elab) => {
      let targets =
        CachedStatics.compute_targets(~settings, ~info_map, ~probe_ids);
      let ei = EvalInfo.of_info_map(~probe_all=false, ~targets, info_map);
      let (_, state) =
        Evaluator.evaluate(~eval_info=ei, ~env=Builtins.env_init, elab);
      (
        Id.Map.cardinal(targets),
        Id.Map.cardinal(EvaluatorState.get_probes(state)),
      );
    };
    let (map_m, elab_m) =
      Statics.mk_unmemoized(~probe_ids, settings, ctx, term);
    let (tm, pm) = capture_count(map_m, elab_m);
    Printf.printf("PROBECAP mono: targets=%d captured=%d\n", tm, pm);
    /* probe toggle must be INCREMENTAL: only the item containing the
       toggled id re-analyzes (probe-aware dirtying), and the result
       must still capture like a cold probe-aware run */
    let ds0 = DefStatics.calc(~settings, term);
    let ds = DefStatics.calc(~settings, ~prev=ds0, ~probe_ids, term);
    Printf.printf(
      "PROBECAP toggle-on analyzed: %d of %d items\n",
      DefStatics.last_analyzed^,
      List.length(ds.items),
    );
    check(
      bool,
      "probe toggle re-analyzes a strict subset",
      true,
      DefStatics.last_analyzed^ < List.length(ds.items),
    );
    /* IDEMPOTENCY gate: repeated no-change incremental calcs must not
       grow the patched root infos (the suffix co_ctx patch once read
       its own output back and DOUBLED per calc — exponential memory) */
    let root_co_size = (t: DefStatics.t) =>
      switch (Statics.Map.lookup_exp(Exp.rep_id(term), t.merged)) {
      | Some(info) =>
        /* total USES (per-name entry lists), the thing that doubled */
        CoCtx.to_list(info.co_ctx)
        |> List.fold_left((n, (_, es)) => n + List.length(es), 0)
      | None => (-1)
      };
    let ds_i1 = DefStatics.calc(~settings, ~prev=ds, ~probe_ids, term);
    let ds_i2 = DefStatics.calc(~settings, ~prev=ds_i1, ~probe_ids, term);
    let ds_i3 = DefStatics.calc(~settings, ~prev=ds_i2, ~probe_ids, term);
    Printf.printf(
      "PROBECAP root co_ctx sizes across no-change calcs: %d %d %d\n",
      root_co_size(ds_i1),
      root_co_size(ds_i2),
      root_co_size(ds_i3),
    );
    check(
      int,
      "spine patch idempotent (co_ctx does not grow)",
      root_co_size(ds_i1),
      root_co_size(ds_i3),
    );
    let ds_off = DefStatics.calc(~settings, ~prev=ds, term);
    Printf.printf(
      "PROBECAP toggle-off analyzed: %d\n",
      DefStatics.last_analyzed^,
    );
    switch (Statics.Map.lookup_exp(Exp.rep_id(term), ds_off.merged)) {
    | Some(info) =>
      check(
        bool,
        "toggle-off clears the root witness",
        true,
        SubexpProbeTargets.equal(
          info.probe_targets,
          SubexpProbeTargets.empty,
        ),
      )
    | None => Printf.printf("PROBECAP toggle-off: no root entry\n")
    };
    /* WITNESS parity at the roots: incremental-eval reuse keys on
       InfoExp.probe_targets — stale/empty witnesses mean the cached
       run replays sampleless. Compare mono vs comp at the top root. */
    let witness_at = (label, info_map, id) =>
      switch (Statics.Map.lookup_exp(id, info_map)) {
      | Some(info) =>
        Printf.printf(
          "PROBECAP witness %s: has_q=%b\n",
          label,
          !
            SubexpProbeTargets.equal(
              info.probe_targets,
              SubexpProbeTargets.empty,
            ),
        )
      | None => Printf.printf("PROBECAP witness %s: NO ENTRY\n", label)
      };
    let root_id = Exp.rep_id(term);
    witness_at("mono root", map_m, root_id);
    witness_at("comp root", ds.merged, root_id);
    witness_at("mono q", map_m, q_id);
    witness_at("comp q", ds.merged, q_id);
    switch (DefStatics.whole_elab(ds)) {
    | None => Printf.printf("PROBECAP comp: GRAFT SHAPE GAP\n")
    | Some(elab_c) =>
      let (tc, pc) = capture_count(ds.merged, elab_c);
      Printf.printf("PROBECAP comp: targets=%d captured=%d\n", tc, pc);
      check(bool, "compositional probe capture parity", pm > 0, pc > 0);
    };
  };
};

/* Structural alignment: outline restructure ops (insert / delete /
   move / duplicate a top-level item) must cost the changed item plus
   downstream mentioners of its export names — never a full recompute —
   and must agree with a cold recompute of the same term. Items are
   parsed separately and concatenated so piece ids stay stable across
   recombinations, like real segment surgery. */
let structural_alignment = (): unit => {
  let settings = CoreSettings.on;
  let strip_tail = (seg: Segment.t): Segment.t =>
    switch (List.rev(seg)) {
    | [Piece.Tile(_), ...rest] => List.rev(rest)
    | _ => seg
    };
  let item = txt => strip_tail(parse_seg_of(txt ++ "0"));
  let a = item("let a = 1 in\n");
  let b = item("let b = a + 1 in\n");
  let c = item("let c = b + a in\n");
  let d = item("let d = 7 in\n");
  let tail = parse_seg_of("c + d");
  let term_of = segs => MakeTerm.go(List.concat(segs)).term;
  let base = term_of([a, b, c, d, tail]);
  let ds0 = DefStatics.calc(~settings, base);
  let run = (label, ~expect_analyzed, term) => {
    let ds = DefStatics.calc(~settings, ~prev=ds0, term);
    let analyzed = DefStatics.last_analyzed^;
    let cold = DefStatics.calc(~settings, term);
    let ids = (t: DefStatics.t) =>
      List.map((it: DefStatics.item) => it.d_id, t.items);
    let exports = (t: DefStatics.t) =>
      List.map(
        (it: DefStatics.item) =>
          List.map(DefStatics.entry_name, it.d_exports),
        t.items,
      );
    let errs = (t: DefStatics.t) =>
      List.sort_uniq(compare, DefStatics.all_error_ids(t));
    let warns = (t: DefStatics.t) =>
      List.sort_uniq(compare, DefStatics.all_warning_ids(t));
    check(bool, label ++ ": item ids = cold", true, ids(ds) == ids(cold));
    check(
      bool,
      label ++ ": exports = cold",
      true,
      exports(ds) == exports(cold),
    );
    check(bool, label ++ ": errors = cold", true, errs(ds) == errs(cold));
    check(
      bool,
      label ++ ": warnings = cold",
      true,
      warns(ds) == warns(cold),
    );
    check(int, label ++ ": items analyzed", expect_analyzed, analyzed);
  };
  run("noop", ~expect_analyzed=0, base);
  /* insert a fresh unrelated def: just itself */
  let e = item("let e = 2 in\n");
  run("insert", ~expect_analyzed=1, term_of([a, b, e, c, d, tail]));
  /* delete d: only the tail mentions it */
  run("delete", ~expect_analyzed=1, term_of([a, b, c, tail]));
  /* move b below c: c mentions b, plus b itself (move-in recompute) */
  run("move", ~expect_analyzed=2, term_of([a, c, b, d, tail]));
  /* duplicate a (fresh ids, same name): the copy + mentioners of a */
  let a2 = item("let a = 1 in\n");
  run("duplicate", ~expect_analyzed=3, term_of([a, a2, b, c, d, tail]));
};

/* Incremental MakeTerm parity: the grafted per-item term must carry
   the same chain ids and statics as the monolithic parse, and reuse
   must be per-item (one edited item => one item re-parsed). */
let incr_maketerm_parity = (): unit => {
  let settings = CoreSettings.on;
  let ctx = Builtins.ctx_init(Some(Operators.default_mode));
  let check_prog = (label, seg) => {
    let t_mono = MakeTerm.go(seg).term;
    let t_incr = MakeTerm.Incr.term_of(seg);
    let chain_ids = t => List.map(Exp.rep_id, DefStatics.chain(t));
    check(
      bool,
      label ++ ": chain ids",
      true,
      chain_ids(t_mono) == chain_ids(t_incr),
    );
    let errs = t => {
      let (map, _) = Statics.mk_unmemoized(settings, ctx, t);
      List.sort_uniq(compare, Statics.Map.error_ids(map));
    };
    check(
      bool,
      label ++ ": statics errors",
      true,
      errs(t_mono) == errs(t_incr),
    );
    Printf.printf(
      "INCRMK %s: full term compare equal: %b\n",
      label,
      compare(t_mono, t_incr) == 0,
    );
  };
  check_prog(
    "small",
    parse_seg_of("let a = 1 in\ntest a == 1 end;\nlet b = a + 1 in\nb"),
  );
  let path = "hazel-programs/mega/mega-1k.hz";
  let path =
    Sys.file_exists(path) ? path : "../hazel-programs/mega/mega-1k.hz";
  switch (read_file(path)) {
  | None => Printf.printf("INCRMK: corpus unreadable\n")
  | Some(src) =>
    let seg = parse_seg_of(src);
    check_prog("mega-1k", seg);
    /* reuse: same segment (fresh list, same pieces) => 0 items parsed */
    let seg' = List.map(p => p, seg);
    let _ = MakeTerm.Incr.term_of(seg');
    check(int, "recombination reuse", 0, MakeTerm.Incr.analyzed^);
    /* one-item edit => one item re-parsed */
    let (found, seg2) = repl_last(~needle="9", ~repl="8", seg);
    assert(found);
    let t2 = MakeTerm.Incr.term_of(seg2);
    check(int, "one edit, one item", 1, MakeTerm.Incr.analyzed^);
    let t2_mono = MakeTerm.go(seg2).term;
    check(
      bool,
      "edited: chain ids",
      true,
      List.map(Exp.rep_id, DefStatics.chain(t2_mono))
      == List.map(Exp.rep_id, DefStatics.chain(t2)),
    );
  };
};

/* Incremental StreamCollector parity: drive a real yielding evaluation
   of mega-1k, and at every drained chunk compare the O(program)-walk
   collector against the incremental frontier collector — probes, test
   results, and completion must agree at each step. */
let stream_collector_parity = (): unit => {
  let path = "hazel-programs/mega/mega-1k.hz";
  let path =
    Sys.file_exists(path) ? path : "../hazel-programs/mega/mega-1k.hz";
  switch (read_file(path)) {
  | None => Printf.printf("STREAMINC: corpus unreadable\n")
  | Some(src) =>
    let settings = CoreSettings.on;
    let ctx = Builtins.ctx_init(Some(Operators.default_mode));
    let term = MakeTerm.go(parse_seg_of(src)).term;
    let (info_map, elab) = Statics.mk_unmemoized(settings, ctx, term);
    let eval_info =
      EvalInfo.of_info_map(
        ~probe_all=false,
        ~targets=Sample.no_targets,
        info_map,
      );
    let evaluation =
      Evaluator.start_yielding_evaluation(
        ~eval_info,
        ~env=Builtins.env_init,
        elab,
      );
    let merged = ref(IncrEval.empty_outbox);
    let inc = ref(None);
    let chunks = ref(0);
    let mismatches = ref(0);
    let compare_states = () => {
      let walk = StreamCollector.collect_stream_state(merged^, elab);
      let (inc', fast) =
        StreamCollector.collect_stream_state_inc(~prev=inc^, merged^, elab);
      inc := inc';
      let probes_eq =
        compare(
          EvaluatorState.get_probes(walk),
          EvaluatorState.get_probes(fast),
        )
        == 0;
      let tests_eq =
        compare(
          EvaluatorState.get_tests(walk),
          EvaluatorState.get_tests(fast),
        )
        == 0;
      if (!(probes_eq && tests_eq)) {
        incr(mismatches);
        if (mismatches^ <= 3) {
          let tw = EvaluatorState.get_tests(walk);
          let tf = EvaluatorState.get_tests(fast);
          let rec first_diff = (i, a, b) =>
            switch (a, b) {
            | ([], []) => (-1)
            | ([], _)
            | (_, []) => i
            | ([x, ...a], [y, ...b]) =>
              compare(x, y) == 0 ? first_diff(i + 1, a, b) : i
            };
          Printf.printf(
            "STREAMINC chunk %d MISMATCH probes=%b tests walk=%d fast=%d first_diff=%d\n",
            chunks^,
            probes_eq,
            List.length(tw),
            List.length(tf),
            first_diff(0, tw, tf),
          );
          let digest = l =>
            String.concat(
              " ",
              List.map(
                ((id, reps)) =>
                  String.sub(Id.to_string(id), 0, 4)
                  ++ ":"
                  ++ string_of_int(List.length(reps))
                  ++ TestStatus.show(TestMap.joint_status(reps)),
                l,
              ),
            );
          Printf.printf("  walk: %s\n  fast: %s\n", digest(tw), digest(tf));
          Printf.printf(
            "  sorted_eq=%b\n",
            compare(List.sort(compare, tw), List.sort(compare, tf)) == 0,
          );
        };
      };
    };
    let rec drive = ev =>
      switch (Evaluator.run_yielding_slice(~step_budget=2000, ev)) {
      | Evaluator.EvaluationYielded(ev) =>
        let update = Evaluator.drain_streaming_outbox(ev);
        if (!IncrEval.outbox_is_empty(update)) {
          merged := IncrEval.merge_outbox(update, merged^);
          incr(chunks);
          compare_states();
        };
        drive(ev);
      | Evaluator.EvaluationCompleted((_, final_state)) =>
        let (_, fast) =
          StreamCollector.collect_stream_state_inc(~prev=inc^, merged^, elab);
        Printf.printf(
          "STREAMINC chunks=%d mismatches=%d final tests: stream<=eval %b\n",
          chunks^,
          mismatches^,
          List.length(EvaluatorState.get_tests(fast))
          <= List.length(EvaluatorState.get_tests(final_state)),
        );
      };
    drive(evaluation);
    check(int, "incremental collector parity", 0, mismatches^);
  };
};

let tests = (
  "BenchStatics",
  [
    test_case("stream collector parity", `Quick, stream_collector_parity),
    test_case("probe capture parity", `Quick, probe_capture_parity),
    test_case("structural alignment", `Quick, structural_alignment),
    test_case("incremental MakeTerm parity", `Quick, incr_maketerm_parity),
    test_case("payload probe (informational)", `Quick, payload_probe),
    test_case(
      "DefStatics compositional (informational)",
      `Quick,
      defstatics_bench,
    ),
    test_case("Statics.mk memoization (informational)", `Quick, memo_probe),
    test_case(
      "slide-load pipeline (informational)",
      `Quick,
      load_pipeline_probe,
    ),
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
