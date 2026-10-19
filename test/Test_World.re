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

let assembled = (~library, ~name, seg) =>
  Option.bind(MemberForm.of_exp_root(seg), members =>
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
  switch (World.split(~name="Scratchpad 1", doc)) {
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
    option(string),
    "another slide's module isn't this one",
    None,
    Option.map(_ => "split", World.split(~name="Other", doc)),
  );
};

/* a corpus slide in the library's world: its own errors and tests, as on
   its own against the baked library */
let corpus = () => {
  let library = library();
  let n = ref(0);
  List.iter(
    ((path, src)) => {
      /* one parse: a parse's ids are fresh */
      let seg = CorpusUtil.seg_of(~root=Exp, src);
      switch (assembled(~library, ~name="Slide", seg)) {
      | None => fail(path ++ ": no world document")
      | Some(doc) =>
        incr(n);
        let mine = in_seg(seg);
        let (e0, t0) = Test_MemberForm.checked(~root=Exp, seg);
        let (e1, t1) = Test_MemberForm.checked(~root=Mod, doc);
        check(int, path ++ ": type errors", e0, e1);
        check(
          list(pair(string, string)),
          path ++ ": the slide's tests",
          t0,
          List.filter(((id, _)) => mine(id), t1),
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
