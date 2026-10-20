open Alcotest;
open Haz3lcore;
open Language;

module P = Test_DefStaticsParity;

/* the world tree's document: the library's items, then a slide as its
   sealed module (World) */

let library_text = () =>
  Option.get(
    Test_ModAudit.read_any([
      "_build/default/src/stdlib/stdlib.hz",
      "../src/stdlib/stdlib.hz",
    ]),
  );

let library = () => CorpusUtil.seg_of(~root=Mod, library_text());

let assembled = (~root=Sort.Exp, ~library, ~name, seg) =>
  Option.bind(World.members_of(~root, seg), members =>
    World.assemble(~library, ~name, members)
  );

/* a test's id, as tests_of shows it, is one of [seg]'s */
let in_seg = (seg: Segment.t) => {
  let ids = Hashtbl.create(256);
  List.iter(id => Hashtbl.replace(ids, Id.show(id), ()), Segment.ids(seg));
  shown => Hashtbl.mem(ids, shown);
};

let round_trip = () => {
  let library = library();
  let members =
    Option.get(
      MemberForm.of_exp_root(
        CorpusUtil.seg_of(~root=Exp, "let x = List.length([1]) in\nx"),
      ),
    );
  let doc =
    Option.get(World.assemble(~library, ~name="Scratchpad 1", members));
  let slide = Option.get(World.slide_id("Scratchpad 1"));
  switch (World.split(~slide, doc)) {
  | None => fail("the slide's module is lost")
  | Some(parts) =>
    check(
      bool,
      "the library's pieces, as they were",
      true,
      List.length(parts.library) == List.length(library)
      && List.for_all2((===), parts.library, library),
    );
    check(
      bool,
      "the slide's members, as they were",
      true,
      List.length(parts.members) == List.length(members)
      && List.for_all2((===), parts.members, members),
    );
  };
  check(
    bool,
    "a new library in place, the slide's module as it was",
    true,
    switch (
      Option.bind(
        World.with_library(~slide, ~library, doc),
        World.split(~slide),
      )
    ) {
    | Some(parts) =>
      let same = (a, b) =>
        List.length(a) == List.length(b) && List.for_all2((===), a, b);
      same(parts.library, library) && same(parts.members, members);
    | None => false
    },
  );
  check(
    option(string),
    "another slide's module isn't this one",
    None,
    Option.map(
      _ => "split",
      World.split(~slide=Option.get(World.slide_id("Other")), doc),
    ),
  );
};

/* the tests [doc] (a world for the slide "Slide") has when only the
   slide's item runs, the library's definitions coming from the bake */
let run_alone = (doc: Segment.t) => {
  let term = MakeTerm.Incr.term_of_root(~root=Mod, doc);
  let (map, elab) =
    Statics.mk_unmemoized(P.settings, DefStatics.ctx0(), term);
  let elab =
    World.run_elab(~slide=Option.get(World.slide_id("Slide")), elab);
  switch (
    Evaluator.evaluate(
      ~eval_info=
        EvalInfo.of_info_map(
          ~probe_all=P.settings.probe_all,
          ~targets=Id.Map.empty,
          map,
        ),
      ~env=Builtins.env_for(elab),
      elab,
    )
  ) {
  | (_, state) => P.tests_of(state)
  | exception _ => [("evaluation", "raised")]
  };
};

/* a corpus slide in the library's world: its own errors and tests, as on
   its own against the baked library */
let corpus = () => {
  let library = library();
  let n = ref(0);
  List.iter(
    ((path, root, src)) => {
      /* one parse: a parse's ids are fresh */
      let seg = CorpusUtil.seg_of(~root, src);
      switch (assembled(~root, ~library, ~name="Slide", seg)) {
      | None => fail(path ++ ": no world document")
      | Some(doc) =>
        incr(n);
        let mine = in_seg(seg);
        let (e0, t0) = Test_MemberForm.checked(~root, seg);
        let (e1, t1) = Test_MemberForm.checked(~root=Mod, doc);
        check(int, path ++ ": type errors", e0, e1);
        check(
          list(pair(string, string)),
          path ++ ": the slide's tests",
          t0,
          List.filter(((id, _)) => mine(id), t1),
        );
        check(
          list(pair(string, string)),
          path ++ ": the slide's tests, run against the bake",
          t0,
          run_alone(doc),
        );
      };
    },
    CorpusUtil.slide_corpus(),
  );
  print_endline(Printf.sprintf("%d slides in the world", n^));
};

let tests = (
  "World",
  [
    test_case(
      "module names",
      `Quick,
      () => {
        check(
          string,
          "spaces",
          "Scratchpad1",
          World.module_name("Scratchpad 1"),
        );
        check(string, "lower", "Basics", World.module_name("basics"));
        check(string, "digit", "Slide2d", World.module_name("2d"));
        check(string, "empty", "Slide", World.module_name("+"));
      },
    ),
    test_case("assemble and split", `Quick, round_trip),
    test_case("corpus slides in the library's world", `Slow, corpus),
  ],
);
