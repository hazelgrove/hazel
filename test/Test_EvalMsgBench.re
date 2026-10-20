/* The eval request per pause, whole (as the base posted it) and in
   pieces: after an edit (a literal retyped, its id kept as typing
   does) and its statics, the main thread's cost of building and posting
   the request, the bytes posted, and the worker's cost of taking it in.
   Runs with HAZEL_BENCH=1 only (a release build for real numbers);
   BENCH_DETAIL=1 also prints each pause's pieces */
open Alcotest;
open Haz3lcore;
open Language;

let settings = CoreSettings.on;
let now = Util.JsUtil.precise_timestamp;

/* postMessage's copy of its argument */
let structured_clone: 'a. 'a => 'a =
  x =>
    Js_of_ocaml.Js.Unsafe.fun_call(
      Js_of_ocaml.Js.Unsafe.pure_js_expr(
        "(function (x) { return structuredClone(x); })",
      ),
      [|Js_of_ocaml.Js.Unsafe.inject(x)|],
    );

/* the request as the base posted it: the whole program, every time */
type whole_value = {
  expr: Exp.t,
  eval_info_map: EvalInfo.t,
  prev: int,
  stream: int,
};
type whole =
  | Evaluate(int, list((string, whole_value)));

let post_whole = (st: CachedStatics.t): string =>
  structured_clone(
    Marshal.to_string(
      Evaluate(
        1,
        [
          (
            "",
            {
              expr: st.elaborated,
              eval_info_map:
                EvalInfo.of_info_map(
                  ~probe_all=false,
                  ~targets=st.targets,
                  st.info_map,
                ),
              prev: 0,
              stream: 0,
            },
          ),
        ],
      ),
      [],
    ),
  );

let receive_whole = (bytes: string): unit => {
  let _: whole = Marshal.from_string(bytes, 0);
  ();
};

/* the request in pieces as the client posts it, and where its time
   went: cut, posted_of, encode, clone */
let post_pieces = (st: CachedStatics.t) => {
  let t0 = now();
  let program =
    Web.EvalSplit.program(~probe_all=false, ~targets=st.targets, st);
  let t1 = now();
  let request: WorkerServer.Request.t = {
    request_id: 1,
    batch: [
      (
        "",
        {
          program,
          prev: UseResident,
          stream: Full,
        },
      ),
    ],
  };
  let posted = Web.WorkerClient.posted_of(~full=false, request);
  let t2 = now();
  let encoded = WorkerServer.Active.encode_request(Evaluate(posted));
  let t3 = now();
  let encoded = structured_clone(encoded);
  let t4 = now();
  Web.WorkerClient.hold(request);
  (encoded, (t1 -. t0, t2 -. t1, t3 -. t2, t4 -. t3));
};

let receive_pieces = (encoded: WorkerServer.Active.request): unit =>
  switch (
    WorkerServer.receive(
      switch (WorkerServer.Active.decode_request(encoded)) {
      | Evaluate(p) => p
      | InstallLibrary(_) => Alcotest.fail("a library install")
      },
    )
  ) {
  | Composed(_) => ()
  | _ => fail("the worker didn't compose the pieces")
  };

/* the [n]th [needle] token retyped as [repl], its id kept */
let edit_nth = (~n, ~needle, ~repl, seg: Segment.t): Segment.t => {
  let seen = ref(0);
  let rec go = (seg: Segment.t): Segment.t => {
    let out = List.map(piece, seg);
    List.for_all2((===), seg, out) ? seg : out;
  }
  and piece = (p: Piece.t): Piece.t =>
    switch (p) {
    | Tile(t) when Tile.label(t) == [needle] =>
      let i = seen^;
      incr(seen);
      i == n
        ? Tile({
            ...t,
            form: Form.Tok(repl),
          })
        : p;
    | Tile(t) =>
      let children = List.map(go, t.children);
      List.for_all2((===), children, t.children)
        ? p
        : Tile({
            ...t,
            children,
          });
    | _ => p
    };
  go(seg);
};

let count = (~needle, seg: Segment.t): int => {
  let rec go = seg =>
    List.fold_left(
      (n, p: Piece.t) =>
        switch (p) {
        | Tile(t) when Tile.label(t) == [needle] => n + 1
        | Tile(t) => List.fold_left((n, k) => n + go(k), n, t.children)
        | _ => n
        },
      0,
      seg,
    );
  go(seg);
};

let pct = (xs: list(float), q: float): float => {
  let s = List.sort(compare, xs) |> Array.of_list;
  let n = Array.length(s);
  s[min(n - 1, max(0, int_of_float(ceil(q *. float_of_int(n))) - 1))];
};

let timed = f => {
  let t0 = now();
  let x = f();
  (now() -. t0, x);
};

/* [pauses] edits spread over the program; after each one's statics,
   both requests, in alternating order */
let run = (~name, ~root, ~pauses=12, src: string) => {
  let term_of = seg => MakeTerm.Incr.term_of_root(~root, seg);
  let statics_of = (term: Exp.t) =>
    CachedStatics.of_def_statics(
      ~settings,
      ~probe_ids=Id.Map.empty,
      term,
      DefStatics.calc_auto(~settings, term),
    );
  Hashtbl.reset(WorkerServer.held);
  Hashtbl.reset(Web.WorkerClient.held);
  let seg = CorpusUtil.seg_of(~root, src);
  let st = statics_of(term_of(seg));
  let whole_bytes = String.length(post_whole(st));
  let (first, _) = post_pieces(st);
  receive_pieces(first);
  let size = e =>
    Core.Byte_units.bytes_int_exn(WorkerServer.Active.size_request(e));
  let first_bytes = size(first);
  let rows = ref([]);
  let _: (Segment.t, bool) =
    List.fold_left(
      ((seg, flip), i) => {
        /* three spots (a tenth, half, nine tenths in), alternating */
        let needle = flip ? "2" : "1";
        let n = count(~needle, seg);
        let at = [|n / 10, n / 2, n * 9 / 10|][i mod 3];
        let seg' = edit_nth(~n=at, ~needle, ~repl=flip ? "1" : "2", seg);
        if (seg' === seg) {
          fail("no edit");
        };
        let st = statics_of(term_of(seg'));
        let whole = () => timed(() => post_whole(st));
        let pieces = () => timed(() => post_pieces(st));
        let ((tw, w), (tp, (p, phases))) =
          if (i mod 2 == 0) {
            let w = whole();
            (w, pieces());
          } else {
            let p = pieces();
            (whole(), p);
          };
        let (rw, ()) = timed(() => receive_whole(w));
        let (rp, ()) = timed(() => receive_pieces(p));
        rows := [(tw, tp, phases, rw, rp, size(p)), ...rows^];
        if (Sys.getenv_opt("BENCH_DETAIL") == Some("1")) {
          Printf.printf(
            "  pause %d: posted %d B, encoded %s\n%!",
            i,
            size(p),
            String.concat(
              ", ",
              List.map(EvalPieces.show_key, Web.EvalSplit.encoded^),
            ),
          );
        };
        (seg', i mod 3 == 2 ? !flip : flip);
      },
      (seg, false),
      List.init(pauses, i => i),
    );
  let p50 = f => pct(List.map(f, rows^), 0.5);
  let p90 = f => pct(List.map(f, rows^), 0.9);
  Printf.printf(
    "BENCH %s: main thread per pause, whole p50 %.1f ms (p90 %.1f), %d B | pieces p50 %.1f ms (p90 %.1f; cut %.1f, posted_of %.1f, encode %.1f, clone %.1f), posted p50 %.0f B (max %.0f; the first %d B) | worker: decode the whole p50 %.1f ms, compose the pieces p50 %.1f ms%s\n%!",
    name,
    p50(((tw, _, _, _, _, _)) => tw),
    p90(((tw, _, _, _, _, _)) => tw),
    whole_bytes,
    p50(((_, tp, _, _, _, _)) => tp),
    p90(((_, tp, _, _, _, _)) => tp),
    p50(((_, _, (a, _, _, _), _, _, _)) => a),
    p50(((_, _, (_, b, _, _), _, _, _)) => b),
    p50(((_, _, (_, _, c, _), _, _, _)) => c),
    p50(((_, _, (_, _, _, d), _, _, _)) => d),
    p50(((_, _, _, _, _, b)) => float_of_int(b)),
    pct(List.map(((_, _, _, _, _, b)) => float_of_int(b), rows^), 1.),
    first_bytes,
    p50(((_, _, _, rw, _, _)) => rw),
    p50(((_, _, _, _, rp, _)) => rp),
    switch (Web.EvalSplit.went_whole^) {
    | Some(why) => " WHOLE: " ++ why
    | None => ""
    },
  );
};

let programs = () => [
  ("mega-1k", Sort.Exp, Option.get(CorpusUtil.mega_src("mega-1k.hz"))),
  ("mega-2k", Sort.Exp, Option.get(CorpusUtil.mega_src("mega-2k.hz"))),
  ("mega-4k", Sort.Exp, Option.get(CorpusUtil.mega_src("mega-4k.hz"))),
  (
    "mega-mod-1k",
    Sort.Mod,
    Option.get(CorpusUtil.mega_src("mega-mod-1k.hz")),
  ),
];

let tests = (
  "EvalMsg bench",
  [
    test_case("the request per pause, whole and in pieces", `Slow, () =>
      if (CorpusUtil.bench_enabled) {
        List.iter(
          ((name, root, src)) => run(~name, ~root, src),
          programs(),
        );
      }
    ),
  ],
);
