/* Timing cases: HAZEL_BENCH=1 bash test/run_node.sh test 'BenchStatics' */
open Alcotest;
open Haz3lcore;
open Language;

/* statics parity gates over the mega corpus, plus print-only timing cases */

let read_file = CorpusUtil.read_file;

/* id-preserving single-token edits at the last / first match of [needle] */
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
      | Tile(t) when Tile.label(t) == [needle] => (
          true,
          [
            Piece.Tile({
              ...t,
              form: Form.Tok(repl),
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
    | Tile(t) when Tile.label(t) == [needle] => (
        true,
        [
          Piece.Tile({
            ...t,
            form: Form.Tok(repl),
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

/* DefStatics error parity with whole-program statics, cold and after
   non-export, export-type and cross-module edits; timings print */
let defstatics_case = (name: string, ()): unit => {
  let path = "hazel-programs/mega/" ++ name;
  let path = Sys.file_exists(path) ? path : "../hazel-programs/mega/" ++ name;
  switch (read_file(path)) {
  | None => fail("DEFSTATICS: corpus unreadable: " ++ name)
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
    let ds1 = time("engine cold", () => DefStatics.calc(~settings, term1));
    Printf.printf(
      "DEFSTATICS %s items: %d, warnings: %d\n",
      name,
      List.length(ds1.items),
      List.length(DefStatics.all_warning_ids(ds1)),
    );
    parity("cold", term1, ds1);
    {
      /* the grafted elaboration evaluates like the monolithic one */

      let (_, mono_elab) = Statics.mk_unmemoized(settings, ctx, term1);
      switch (DefStatics.whole_elab(ds1)) {
      | None => fail(name ++ " graft: shape gap")
      | Some(graft_elab) =>
        let (v1, _) = Evaluator.evaluate(~env=Builtins.env_init, mono_elab);
        let (v2, _) = Evaluator.evaluate(~env=Builtins.env_init, graft_elab);
        check(
          bool,
          name ++ " graft-eval parity",
          true,
          Exp.fast_equal(v1, v2),
        );
      };
    };
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
    /* retypes a selfcheck that MetaRunner consumes downstream */
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
};

/* times each stage of a slide load; to find what overflows Chrome's
   smaller stack, run the test JS under plain node (no --stack-size) */
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

/* a probe in a fn body called only from a later item samples under
   compositional statics as under monolithic (fresh evaluations) */
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
    /* guards against the spine patch re-reading its own output */
    let root_co_size = (t: DefStatics.t) =>
      switch (Statics.Map.lookup_exp(Exp.rep_id(term), t.merged)) {
      | Some(info) =>
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
    /* print-only: eval reuse keys on probe_targets, so a stale root
       witness would replay without samples */
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

/* item insert/delete/move/duplicate re-analyzes just the changed item and
   mentioners of its exports, and matches a cold calc */
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

/* per-item MakeTerm matches the monolithic term and re-parses per item */
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
    check(
      bool,
      label ++ ": full term equal",
      true,
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
  | None => fail("INCRMK: corpus unreadable")
  | Some(src) =>
    let seg = parse_seg_of(src);
    check_prog("mega-1k", seg);
    /* fresh list, same pieces: nothing re-parses */
    let seg' = List.map(p => p, seg);
    let _ = MakeTerm.Incr.term_of(seg');
    check(int, "recombination reuse", 0, MakeTerm.Incr.analyzed^);
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

/* at every chunk of a yielding evaluation, the incremental stream
   collector agrees with the full-walk one */
let stream_collector_parity = (): unit => {
  let path = "hazel-programs/mega/mega-1k.hz";
  let path =
    Sys.file_exists(path) ? path : "../hazel-programs/mega/mega-1k.hz";
  switch (read_file(path)) {
  | None => fail("STREAMINC: corpus unreadable")
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
          "STREAMINC chunks=%d mismatches=%d\n",
          chunks^,
          mismatches^,
        );
        check(
          bool,
          "final streamed tests <= evaluated tests",
          true,
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
    test_case(
      "DefStatics error parity (mega-1k)",
      `Quick,
      defstatics_case("mega-1k.hz"),
    ),
  ]
  @ CorpusUtil.bench_cases([
      test_case(
        "DefStatics compositional timing (mega-4k)",
        `Quick,
        defstatics_case("mega-4k.hz"),
      ),
      test_case(
        "slide-load pipeline (informational)",
        `Quick,
        load_pipeline_probe,
      ),
    ]),
);
