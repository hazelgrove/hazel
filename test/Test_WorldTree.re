open Alcotest;
open Haz3lcore;
open Language;

module P = Test_DefStaticsParity;

/* the world tree: the library, then folders of sealed slides, unloaded
   ones folded empty (WorldTree) */

let mod_seg = src => CorpusUtil.seg_of(~root=Mod, src);

let text = (seg: Segment.t): string =>
  Printer.of_segment(~holes="?", seg) |> String.trim;

let ms = f => {
  let t0 = Sys.time();
  let r = f();
  (r, (Sys.time() -. t0) *. 1000.);
};

let small = () =>
  WorldTree.assemble(
    ~library=mod_seg("let a = 1"),
    [
      (
        ["Documentation", "Basics", "Holes"],
        Some(mod_seg("let x = 1;\ntest x == 1 end;\nx")),
      ),
      (["Documentation", "Basics", "Lists"], None),
      (["Documentation", "Intro"], Some(mod_seg("test true end;\n2"))),
      (["Scratch", "Scratchpad 1"], Some(mod_seg("a + 1"))),
    ],
  )
  |> Option.get;

let holes = ["Documentation", "Basics", "Holes"];
let lists = ["Documentation", "Basics", "Lists"];

let body_text = (path, seg) =>
  switch (WorldTree.body(~slide=WorldTree.slide_id(path), seg)) {
  | Loaded(m) => text(m)
  | Unloaded => "<unloaded>"
  | Gone => "<gone>"
  };

/* the type errors in the slide at [path], and the tests it has when it
   alone runs */
let checked = (~path, seg) => {
  let term = MakeTerm.Incr.term_of_root(~root=Mod, seg);
  let ds =
    DefStatics.calc(
      ~settings=P.settings,
      ~ctx_in=Builtins.ctx_base(Some(Operators.default_mode)),
      term,
    );
  let folders =
    List.init(List.length(path) - 1, i =>
      WorldTree.folder_id(Util.ListUtil.take(i + 1, path))
    );
  let trail = folders @ [WorldTree.slide_id(path)];
  let elab =
    WorldTree.run_elab(~trail, Option.get(DefStatics.whole_elab(ds)));
  let (_, state) =
    Evaluator.evaluate(
      ~eval_info=
        EvalInfo.of_info_map(
          ~probe_all=false,
          ~targets=Id.Map.empty,
          ds.merged,
        ),
      ~env=Builtins.env_for(elab),
      elab,
    );
  (
    {
      /* the slide's own: other slides' errors are theirs */

      let mine =
        switch (WorldTree.body(~slide=WorldTree.slide_id(path), seg)) {
        | Loaded(members) => Segment.ids(members)
        | Unloaded
        | Gone => []
        };
      List.length(
        List.filter(id => List.mem(id, mine), DefStatics.all_error_ids(ds)),
      );
    },
    /* by result: ids differ between parses */
    List.sort(compare, List.map(snd, P.tests_of(state))),
  );
};

let shape = () => {
  let w = small();
  let t = text(w);
  let has = sub => check(bool, sub, true, Test_WorldSlide.contains(t, sub));
  has("module Documentation = {");
  has("module Basics = {");
  has("module Holes : {} = {");
  has("module Scratch = {");
  has("module Scratchpad1 : {} = {");
  check(
    string,
    "a loaded slide's members",
    "let x = 1;\ntest x == 1 end;\nx",
    body_text(holes, w),
  );
  check(string, "an unloaded slide", "<unloaded>", body_text(lists, w));
  check(string, "no such slide", "<gone>", body_text(["Nowhere"], w));
};

let loads = () => {
  let w = small();
  let w' =
    WorldTree.load(
      ~slide=WorldTree.slide_id(lists),
      mod_seg("let l = [a];\nl"),
      w,
    );
  check(string, "loaded", "let l = [a];\nl", body_text(lists, w'));
  check(
    string,
    "the others as they were",
    body_text(holes, w),
    body_text(holes, w'),
  );
};

let runs = () => {
  let w = small();
  check(
    pair(int, list(string)),
    "a slide deep in its folders: its tests alone",
    (0, ["Pass"]),
    checked(~path=holes, w),
  );
  check(
    pair(int, list(string)),
    "another",
    (0, ["Pass"]),
    checked(~path=["Documentation", "Intro"], w),
  );
};

/* every corpus slide in one document with the standard library, a few
   loaded: assembling it costs little, and a loaded slide checks and runs
   as it does alone */
let corpus = () => {
  let library = Test_World.library();
  let slides = CorpusUtil.slide_corpus();
  let doc = "hazel-programs/world/documentation/";
  let loaded = [doc ++ "basic-reference.hz", doc ++ "tables.hz"];
  let place = path => {
    let name = Filename.remove_extension(Filename.basename(path));
    let dir = Filename.basename(Filename.dirname(path));
    ["Documentation", dir, name];
  };
  let entries =
    List.map(
      ((path, root, src)) =>
        (
          place(path),
          List.mem(path, loaded)
            ? World.members_of(~root, CorpusUtil.seg_of(~root, src)) : None,
        ),
      slides,
    );
  let (w, t_assemble) =
    ms(() => Option.get(WorldTree.assemble(~library, entries)));
  let (_, t_check) = ms(() => checked(~path=place(doc ++ "tables.hz"), w));
  print_endline(
    Printf.sprintf(
      "%d slides: assembled in %.0f ms; checked and ran Tables in %.0f ms",
      List.length(entries),
      t_assemble,
      t_check,
    ),
  );
  List.iter(
    path => {
      let (_, root, src) = List.find(((p, _, _)) => p == path, slides);
      let (e, t) =
        Test_MemberForm.checked(~root, CorpusUtil.seg_of(~root, src));
      check(
        pair(int, list(string)),
        path,
        (e, List.sort(compare, List.map(snd, t))),
        checked(~path=place(path), w),
      );
    },
    loaded,
  );
};

let tests = (
  "WorldTree",
  [
    test_case("shape", `Quick, shape),
    test_case("loading a slide", `Quick, loads),
    test_case("a slide runs alone", `Quick, runs),
    test_case("the corpus in one document", `Slow, corpus),
  ],
);
