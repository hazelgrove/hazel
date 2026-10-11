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

let tests = (
  "EvalInfoProjection",
  List.map(edit_case, edits)
  @ [test_case("when the maps share nothing", `Quick, unrelated)],
);
