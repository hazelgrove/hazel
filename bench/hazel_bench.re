/* Editor and evaluator timings for the performance report
   (.github/workflows/perf-report.yml, docs/performance.md). Ported from
   David Moon's harness in #2161, without its per-phase hooks.

   Times whole passes of the editor's derived-data pipeline:
   CachedStatics (term construction, statics, elaboration) and then
   CachedSyntax (term construction for display, projector shapes,
   measurement), on generated let-chains of 100 and 500 bindings, in four
   scenarios:

     cold  - a copy of the program with fresh ids, so no memo table has
             seen it: loading a program
     warm  - the same zipper again, every memo table primed: a redraw
     move  - one cursor move after priming: the commonest action
     edit  - one literal edited after priming: the commonest change

   and evaluation of bench/eval-bench.hz as `hazel bench-eval` times it:
   parse and statics once, then the plain (non-incremental) evaluator and
   the incremental one seeding its cache.

   Each benchmark runs --reps times after one untimed run, and the median
   of the reps is reported, in milliseconds, as JSON on stdout. One
   invocation is one round; bench/perf_report.py alternates rounds of two
   builds and compares them.

   Run under node with --expose-gc (a collection before each timed run,
   so one run's garbage is not charged to the next) and a large stack:
     node --expose-gc --stack-size=8192 hazel_bench.bc.js \
       [--reps N] [--filter SUBSTRING] [--eval-bench PATH] [--out FILE] */

open Haz3lcore;
open Language;

let now = (): float =>
  Js_of_ocaml.Js.Unsafe.global##.performance##now()##valueOf
  |> Js_of_ocaml.Js.float_of_number;

let gc = (): unit =>
  if (Js_of_ocaml.Js.Optdef.test(Js_of_ocaml.Js.Unsafe.global##.gc)) {
    Js_of_ocaml.Js.Unsafe.global##gc;
  };

let settings = CoreSettings.on;

/* --- Programs --- */

/* let x0 = 0 in let x1 = x0 + 1 in ... in x{n-1} + 0. The trailing 0 is
   what the edit scenario changes. */
let let_chain = (n: int): string => {
  let buf = Stdlib.Buffer.create(n * 24);
  for (i in 0 to n - 1) {
    Stdlib.Buffer.add_string(
      buf,
      i == 0
        ? "let x0 = 0 in\n"
        : Printf.sprintf("let x%d = x%d + 1 in\n", i, i - 1),
    );
  };
  Stdlib.Buffer.add_string(buf, Printf.sprintf("x%d + 0", n - 1));
  Stdlib.Buffer.contents(buf);
};

/* A zipper with the caret after the whole program, as if just typed. */
let zipper_of = (text: string): Zipper.t =>
  switch (FastParse.of_text(~root=Exp, text)) {
  | Some(seg) =>
    Zipper.unzip(~direction=Right, seg)
    |> Zipper.remold_regrout(Left, ~root=Exp)
  | None => failwith("hazel_bench: cannot parse a generated program")
  };

/* The same program with every id fresh. Every memo table along the
   pipeline is keyed on syntax that includes ids, so this is a program
   none of them has seen, without clearing anything. */
let fresh_copy = (z: Zipper.t): Zipper.t =>
  ZipperBase.MapPiece.go(p => [Segment.IDs.replace_piece(p)], z);

/* --- The pipeline --- */

let statics = (z: Zipper.t): CachedStatics.t =>
  CachedStatics.init(
    ~settings,
    ~is_dynamic_term=true,
    ~stitch=Fun.id,
    ~root=Exp,
    z,
  );

let syntax = (z: Zipper.t, st: CachedStatics.t): CachedSyntax.t =>
  CachedSyntax.mk(
    ~info_map=st.info_map,
    ~dyn_map=Id.Map.empty,
    ~elaborated=Some(st.elaborated),
    z,
  );

let prime = (z: Zipper.t): unit => ignore(syntax(z, statics(z)));

/* One timed pass: (statics ms, syntax ms). */
let timed_pass = (z: Zipper.t): (float, float) => {
  gc();
  let t0 = now();
  let st = statics(z);
  let t1 = now();
  ignore(syntax(z, st));
  let t2 = now();
  (t1 -. t0, t2 -. t1);
};

let perform = (z: Zipper.t, actions: list(Action.t)): Zipper.t =>
  List.fold_left(
    (z, a) =>
      switch (
        Perform.go(
          ~settings,
          ~statics=CachedStatics.empty,
          ~syntax=CachedSyntax.init(z),
          ~root=Exp,
          a,
          {
            zipper: z,
            col_target: None,
          },
        )
      ) {
      | Ok(z) => z
      | Error(_) => failwith("hazel_bench: an editor action failed")
      },
    z,
    actions,
  );

let move: list(Action.t) = [Move(Local(Left, ByChar))];
/* the trailing `+ 0` becomes `+ 1` */
let edit: list(Action.t) = [Destruct(Local(Left, ByChar)), Insert("1")];

/* --- Statistics --- */

let median = (xs: list(float)): float => {
  let a = Array.of_list(xs);
  Array.sort(compare, a);
  let n = Array.length(a);
  n == 0 ? nan : n mod 2 == 1 ? a[n / 2] : (a[n / 2 - 1] +. a[n / 2]) /. 2.0;
};

/* --- Benchmarks --- */

type bench = {
  name: string,
  /* one timed run: its (label, ms) measurements */
  run: unit => list((string, float)),
};

let pipeline_bench = (name: string, setup: unit => Zipper.t): bench => {
  name,
  run: () => {
    let z = setup();
    let (st, sy) = timed_pass(z);
    [("statics", st), ("syntax", sy), ("total", st +. sy)];
  },
};

let program_benches = (label: string, z: Zipper.t): list(bench) => {
  /* warm reuses one zipper, primed here and by the untimed first run */
  let warm = fresh_copy(z);
  [
    pipeline_bench(label ++ "/cold", () => fresh_copy(z)),
    pipeline_bench(label ++ "/warm", () => warm),
    pipeline_bench(
      label ++ "/move",
      () => {
        let z = fresh_copy(z);
        prime(z);
        perform(z, move);
      },
    ),
    pipeline_bench(
      label ++ "/edit",
      () => {
        let z = fresh_copy(z);
        prime(z);
        perform(z, edit);
      },
    ),
  ];
};

let read_file = (path: string): string => {
  let ic = open_in_bin(path);
  let s = really_input_string(ic, in_channel_length(ic));
  close_in(ic);
  s;
};

/* What `hazel bench-eval` does (src/CLI/Cli.re): parse and statics once,
   then time evaluation alone. */
let eval_benches = (path: string): list(bench) => {
  let text = read_file(path) |> Util.StringUtil.strip_final_newline;
  let exp =
    switch (FastParse.of_text(~root=Exp, text)) {
    | Some(seg) => MakeTerm.go(seg).term
    | None => failwith("hazel_bench: cannot parse " ++ path)
    };
  let (info_map, elab) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      exp,
    );
  let eval_info =
    EvalInfo.of_info_map(
      ~probe_all=CoreSettings.on.probe_all,
      ~targets=Id.Map.empty,
      info_map,
    );
  let timed = (label, f) => {
    gc();
    let t0 = now();
    ignore(f());
    [(label, now() -. t0)];
  };
  [
    {
      name: "eval-bench",
      run: () =>
        timed("plain", () => Evaluator.evaluate(~env=Builtins.env_init, elab))
        @ timed("incr", () =>
            Evaluator.evaluate(~eval_info, ~env=Builtins.env_init, elab)
          ),
    },
  ];
};

/* Runs a benchmark reps times after an untimed run; the median of each
   measurement. */
let run_bench = (~reps: int, b: bench): list((string, float)) => {
  ignore(b.run());
  let runs = List.init(reps, _ => b.run());
  List.map(
    ((label, _)) =>
      (
        b.name ++ "/" ++ label,
        median(List.map(run => List.assoc(label, run), runs)),
      ),
    List.hd(runs),
  );
};

let contains = (~sub: string, s: string): bool => {
  let n = String.length(sub);
  let rec go = i =>
    i + n <= String.length(s) && (String.sub(s, i, n) == sub || go(i + 1));
  go(0);
};

let () = {
  let rec parse_args = ((reps, filter, eval_bench, out)) =>
    fun
    | [] => (reps, filter, eval_bench, out)
    | ["--reps", n, ...rest] =>
      parse_args((int_of_string(n), filter, eval_bench, out), rest)
    | ["--filter", f, ...rest] =>
      parse_args((reps, Some(f), eval_bench, out), rest)
    | ["--eval-bench", p, ...rest] =>
      parse_args((reps, filter, Some(p), out), rest)
    | ["--out", p, ...rest] =>
      parse_args((reps, filter, eval_bench, Some(p)), rest)
    | [arg, ..._] => failwith("hazel_bench: unknown argument " ++ arg);
  let (reps, filter, eval_bench, out) =
    parse_args((5, None, None, None), List.tl(Array.to_list(Sys.argv)));
  let benches =
    List.concat_map(
      ((label, n)) => program_benches(label, zipper_of(let_chain(n))),
      [("let100", 100), ("let500", 500)],
    )
    @ (
      switch (eval_bench) {
      | Some(path) => eval_benches(path)
      | None => []
      }
    );
  let benches =
    switch (filter) {
    | Some(sub) => List.filter(b => contains(~sub, b.name), benches)
    | None => benches
    };
  let results =
    List.concat_map(
      b => {
        Printf.eprintf("%s\n%!", b.name);
        run_bench(~reps, b);
      },
      benches,
    );
  let json =
    Yojson.Safe.pretty_to_string(
      `Assoc([
        ("reps", `Int(reps)),
        (
          "results",
          `Assoc(List.map(((name, ms)) => (name, `Float(ms)), results)),
        ),
      ]),
    );
  switch (out) {
  | None => print_endline(json)
  | Some(path) =>
    let oc = open_out_bin(path);
    output_string(oc, json);
    close_out(oc);
  };
};
