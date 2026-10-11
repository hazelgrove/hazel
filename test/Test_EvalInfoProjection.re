/* EvalInfo.of_info_map keeps each program's last projection of statics
   and updates it from the statics map's changes instead of rebuilding it,
   so the request that carries it hashes only what changed. Over real
   edits, with per-item statics as the app runs them, it must give the
   same projection as a rebuild and keep the unchanged entries themselves;
   and when the maps share nothing it must still be right. */
open Alcotest;
open Haz3lcore;
open Language;

let settings = {
  ...CoreSettings.off,
  statics: true,
};

let term = (seg: Segment.t): Exp.t => MakeTerm.go(seg).term;

let parse = (src: string): Segment.t =>
  switch (ParsedCorpus.to_segment(~root=Exp, src)) {
  | Some(seg) => seg
  | None => fail("unparseable: " ++ src)
  };

let key = Id.invalid;

let project = (~probe_all=false, info_map) =>
  EvalInfo.of_info_map(~key, ~probe_all, ~targets=Sample.no_targets, info_map).
    statics;

let agrees = (name, ~probe_all=false, got, info_map) =>
  check(
    bool,
    name ++ ": the same as a rebuild",
    true,
    Id.Map.equal(
      EvalInfo.same_entry,
      got,
      EvalInfo.project_all(~probe_all, info_map),
    ),
  );

/* how many of [b]'s entries are [a]'s own */
let kept = (a, b) =>
  Id.Map.fold(
    (k, e, n) =>
      switch (Id.Map.find_opt(k, a)) {
      | Some(e') when e' === e => n + 1
      | _ => n
      },
    b,
    0,
  );

let edit_case = ((name, mk: unit => Test_EditLocality.edit)) =>
  test_case(
    name,
    `Quick,
    () => {
      let e = mk();
      Hashtbl.reset(EvalInfo.slots);
      let ds0 = DefStatics.calc(~settings, term(e.before));
      let ds1 = DefStatics.calc(~settings, ~prev=ds0, term(e.after));
      let p0 = project(ds0.merged);
      let p1 = project(ds1.merged);
      agrees(name, p1, ds1.merged);
      let n = Id.Map.cardinal(p1);
      let k = kept(p0, p1);
      Printf.printf(
        "%s: %d entries, %d kept, %d changed\n",
        name,
        n,
        k,
        n - k,
      );
      check(bool, name ++ ": a real program", true, n > 1000);
      check(bool, name ++ ": keeps most entries", true, k * 10 >= n * 9);
    },
  );

/* mega-1k, as Test_EditLocality edits it */
let edits = [
  ("insert", Test_EditLocality.insert),
  ("delete", Test_EditLocality.delete),
  ("paste", Test_EditLocality.paste),
  ("undo", Test_EditLocality.undo),
  ("restructure", Test_EditLocality.restructure),
  ("agent", Test_EditLocality.agent),
];

/* the maps share nothing: another program under the same key, the same
   program analyzed again from scratch (as monolithic statics does on
   every commit), and a change of probe_all */
let unrelated = () => {
  Hashtbl.reset(EvalInfo.slots);
  let a =
    DefStatics.calc(
      ~settings,
      term(parse("let a = 1 in\nlet b = a + 1 in\nb")),
    ).
      merged;
  let b =
    DefStatics.calc(~settings, term(parse("let f = fun x -> x in\nf(2)"))).
      merged;
  ignore(project(a));
  agrees("another program under the same key", project(b), b);
  let seg =
    switch (CorpusUtil.corpus_seg(~root=Exp, "mega-1k.hz")) {
    | Some(seg) => seg
    | None => fail("corpus unreadable")
    };
  let cold = () => DefStatics.calc(~settings, term(seg)).merged;
  let (m0, m1) = (cold(), cold());
  check(
    bool,
    "statics rebuilt whole: the walk gives up",
    true,
    Option.is_none(
      EvalInfo.project_from(
        ~probe_all=false,
        m0,
        EvalInfo.project_all(~probe_all=false, m0),
        m1,
      ),
    ),
  );
  ignore(project(m0));
  let p1 = project(m1);
  agrees("statics rebuilt whole", p1, m1);
  agrees(
    "probe_all changed",
    ~probe_all=true,
    project(~probe_all=true, m1),
    m1,
  );
  agrees("probe_all changed back", project(m1), m1);
};

/* On this branch the projection also reads the full-capture probes in
   [targets]: an entry holding one gets its own witness. A projection kept
   under one full-capture set must not be updated under another. */
let full_capture = () => {
  Hashtbl.reset(EvalInfo.slots);
  let t = term(parse("let a = 1 in\nlet b = a + 1 in\nb"));
  let rid = Exp.rep_id(t);
  let m =
    DefStatics.calc(~settings, ~probe_ids=Id.Map.singleton(rid, ()), t).
      merged;
  let same = (a: EvalInfo.entry, b: EvalInfo.entry) =>
    a.elab_term === b.elab_term
    && a.co_ctx === b.co_ctx
    && EvalInfo.equal_probe_targets(a.probe_targets, b.probe_targets);
  let full_targets =
    Id.Map.singleton(
      rid,
      {
        ...Sample.empty_capture_spec,
        full: true,
      },
    );
  let plain =
    EvalInfo.of_info_map(
      ~key,
      ~probe_all=false,
      ~targets=Sample.no_targets,
      m,
    ).
      statics;
  let full =
    EvalInfo.of_info_map(~key, ~probe_all=false, ~targets=full_targets, m).
      statics;
  check(
    bool,
    "under a full-capture probe: the same as a rebuild",
    true,
    Id.Map.equal(
      same,
      full,
      EvalInfo.project_all(~probe_all=false, ~full=[rid], m),
    ),
  );
  check(
    bool,
    "and not the plain projection",
    false,
    Id.Map.equal(same, full, plain),
  );
  let back =
    EvalInfo.of_info_map(
      ~key,
      ~probe_all=false,
      ~targets=Sample.no_targets,
      m,
    ).
      statics;
  check(bool, "and back without it", true, Id.Map.equal(same, back, plain));
};

let tests = (
  "EvalInfoProjection",
  List.map(edit_case, edits)
  @ [
    test_case("when the maps share nothing", `Quick, unrelated),
    test_case("under another full-capture probe set", `Quick, full_capture),
  ],
);
