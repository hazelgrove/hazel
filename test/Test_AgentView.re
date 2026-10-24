open Alcotest;
open Haz3lcore;
open Language;

/* The agent's view (CompositionView): the map and open entries with their
   overlays, as golden strings; the open set by id across a rename; what a
   request records, against what the payload carries */

let settings = CoreSettings.on;

/* a program as the editor reads it: its pieces, zipper and statics */
let program = (~root=Sort.Exp, src: string) => {
  let z = PersistentZipper.from_backup_text(src, ~root);
  let seg = Zipper.unselect_and_zip(z);
  let st =
    CachedStatics.init(
      ~settings,
      ~is_dynamic_term=false,
      ~stitch=x => x,
      ~root,
      z,
    );
  (seg, z, st);
};

/* its run: test results, value and probe samples */
let run = (z: Zipper.t, st: CachedStatics.t) => {
  let targets =
    CachedStatics.compute_targets(
      ~settings,
      ~info_map=st.info_map,
      ~probe_ids=CachedStatics.probe_ids_of_zipper(z),
      (),
    );
  let (value, state) =
    Evaluator.evaluate(
      ~eval_info=EvalInfo.of_targets(targets),
      ~env=Builtins.env_init,
      st.elaborated,
    );
  (
    TestResults.mk_results(EvaluatorState.get_tests(state)),
    value,
    Sample.Map.finalize(EvaluatorState.get_probes(state)),
  );
};

let ids_of = (o: Outline.t, paths: list(string)): list(Id.t) =>
  List.map(
    p =>
      switch (Outline.resolve(o, p)) {
      | Ok(e) => e.id
      | Error(e) => Alcotest.fail(e)
      },
    paths,
  );

let render =
    (~root=Sort.Exp, ~opened=[], ~evaluate=false, ~module_depth=2, src) => {
  let (seg, z, st) = program(~root, src);
  let o = Outline.build(~root, seg);
  let (tests, value, probes) =
    evaluate
      ? {
        let (t, v, p) = run(z, st);
        (Some(t), Some(v), p);
      }
      : (None, None, Sample.Map.empty);
  CompositionView.render(
    ~settings={
      ...CompositionView.default_settings,
      module_depth,
    },
    ~facts={
      info_map: st.info_map,
      error_ids: st.error_ids,
      tests,
      probes,
      refractors: z.refractors.manuals,
      value,
    },
    o,
    ~opened=ids_of(o, opened),
  );
};

let golden = (name, expected, actual) =>
  if (expected != actual) {
    print_endline("== " ++ name ++ " (actual)\n" ++ actual);
    check(string, name, expected, actual);
  };

/* a golden file under test/golden, from the repo root or the test dir;
   HAZEL_GOLDEN=1 writes it instead */
let golden_file = (name: string, actual: string): unit => {
  let path = "test/golden/" ++ name;
  let path = Sys.file_exists(path) ? path : "golden/" ++ name;
  if (Sys.getenv_opt("HAZEL_GOLDEN") == Some("1")) {
    let oc = open_out_bin(path);
    output_string(oc, actual ++ "\n");
    close_out(oc);
  } else {
    switch (CorpusUtil.read_file(path)) {
    | Some(s) => golden(name, Util.StringUtil.strip_final_newline(s), actual)
    | None => Alcotest.fail("no golden " ++ path)
    };
  };
};

let example = {|type Shape = Circle(Float) + Rect(Float, Float) in
let area(s: Shape): Float =
  case s
  | Circle(r) => 3.14 *. r *. r
  | Rect(w, h) => w *. h
  end
in
let total(xs: [Shape]): Float =
  case xs
  | [] => 0.
  | s :: rest => area(s) +. total(rest) +. true
  end
in
module Geo = {
  let perimeter(s: Shape): Float = 0.;
  let scale(k: Float, s: Shape): Shape = s;
  module Units = { let cm = 1.; let m = 100.; let km = 100000. }
} in
hint "total of one circle" test total([Circle(1.)]) == 3.14 end;
total([Rect(2., 3.)])|};

let format_tests = [
  test_case("the map: headers, ⋯ bodies, annotations", `Quick, () =>
    golden(
      "example",
      {|# map · 1 error #
type Shape = Circle(Float) + Rect(Float, Float) in
let area(s: Shape): Float = ⋯ in  # 1 use #
let total(xs: [Shape]): Float = ⋯ in  # 1 error · 3 uses #
module Geo = {
  let perimeter(s: Shape): Float = ⋯;
  let scale(k: Float, s: Shape): Shape = ⋯;
  module Units = { let cm = ⋯; let m = ⋯; let km = ⋯ }
} in
hint "total of one circle" test total([Circle(1.)]) == 3.14 end;
total([Rect(2., 3.)])|},
      render(example),
    )
  ),
  test_case("an open entry follows the map, its error marked", `Quick, () =>
    golden(
      "total open",
      {|# map · 1 error #
type Shape = Circle(Float) + Rect(Float, Float) in
let area(s: Shape): Float = ⋯ in  # 1 use #
let total(xs: [Shape]): Float = ⋯ in  # open below · 1 error · 3 uses #
module Geo = {
  let perimeter(s: Shape): Float = ⋯;
  let scale(k: Float, s: Shape): Shape = ⋯;
  module Units = { let cm = ⋯; let m = ⋯; let km = ⋯ }
} in
hint "total of one circle" test total([Circle(1.)]) == 3.14 end;
total([Rect(2., 3.)])

# open: total #
let total(xs: [Shape]): Float =
  case xs
  | [] => 0.
  | s :: rest => area(s) +. total(rest) +. true
#                                          ^ Expecting type Float but got inconsistent type Bool #
  end
in|},
      render(~opened=["total"], example),
    )
  ),
  test_case("module depth is a setting", `Quick, () =>
    golden(
      "depth 1",
      {|# map · 1 error #
type Shape = Circle(Float) + Rect(Float, Float) in
let area(s: Shape): Float = ⋯ in  # 1 use #
let total(xs: [Shape]): Float = ⋯ in  # 1 error · 3 uses #
module Geo = {
  let perimeter(s: Shape): Float = ⋯;
  let scale(k: Float, s: Shape): Shape = ⋯;
  module Units = { ⋯ }  # 3 members #
} in
hint "total of one circle" test total([Circle(1.)]) == 3.14 end;
total([Rect(2., 3.)])|},
      render(~module_depth=1, example),
    )
  ),
  test_case("test results, the value, probes", `Quick, () =>
    golden(
      "run",
      {|# map · no errors · tests: 1 of 2 pass, 1 fail #
let double(x) = ⋯ in  # open below · 3 uses #
test double(2) == 4 end;  # ✓ #
test ⋯ end;  # ✗ #
double(5)  # ⇒ 10 #

# open: double #
let double(x) =
  let y = ⟦x + x⟧ in     ≡ 4
  y
in|},
      render(
        ~evaluate=true,
        ~opened=["double"],
        {|let double(x) =
  let y = ^^probe(x + x) in
  y
in
test double(2) == 4 end;
test
  double(3)
  == 7
end;
double(5)|},
      ),
    )
  ),
  test_case("a module-rooted program; entries sharing a line", `Quick, () =>
    golden(
      "mod root",
      {|# map · 1 error #
let a = ⋯;  # 3 uses #
module N = { let b = ⋯; test b == 3 end; let c = ⋯ };  # b: 2 uses · c: 1 error · N: open below #
test a == 1 end;  # open below #
print(a);
a + 1

# open: N #
module N = { let b = 2; test b == 3 end; let c = b + true }
#                                                    ^ Expecting type Int but got inconsistent type Bool #

# open: test#1 #
test a == 1 end|},
      render(
        ~root=Mod,
        ~opened=["N", "test#1"],
        {|let a = 1;
module N = { let b = 2; test b == 3 end; let c = b + true };
test a == 1 end;
print(a);
a + 1|},
      ),
    )
  ),
  test_case("nested entries open by path", `Quick, () =>
    golden(
      "nested",
      {|# map · no errors #
let f(x) = ⋯ in  # 1 use #
module Geo = { module Units = { let cm = ⋯; let m = ⋯ } } in  # Units: open below #
f(1)

# open: f/y #
let y = x + 1 in

# open: Geo/Units #
module Units = { let cm = 1; let m = 100 }|},
      render(
        ~opened=["f/y", "Geo/Units"],
        {|let f(x) =
  let y = x + 1 in
  let z = y * 2 in
  z
in
module Geo = { module Units = { let cm = 1; let m = 100 } } in
f(1)|},
      ),
    )
  ),
  test_case("Mega 1k, two entries open", `Quick, () =>
    golden_file(
      "agent-view-mega-1k.txt",
      render(
        ~opened=["GroveNames/grove_name", "CropPlotter"],
        Option.get(CorpusUtil.mega_src("mega-1k.hz")),
      ),
    )
  ),
];

/* the open set keeps its entries by id: a rename keeps them open under
   their new path, and a reload's fresh ids find them by the old path */
let open_set_tests = [
  test_case(
    "an open entry survives a rename",
    `Quick,
    () => {
      let (seg, _, _) = program("let total = 1 in\nlet other = 2 in\ntotal");
      let o = Outline.build(~root=Exp, seg);
      let m =
        switch (
          AgentContext.Update.resolve(
            o,
            AgentContext.Update.Expand(["total"]),
          )
        ) {
        | Ok(r) => AgentContext.Update.update(r, AgentContext.Utils.init())
        | Error(e) => Alcotest.fail(e)
        };
      let (seg', _) =
        CorpusUtil.edit_token(
          ~keep_id=true,
          ~needle="total",
          ~repl="sum",
          seg,
        );
      let o' = Outline.build(~root=Exp, seg');
      let m' = AgentContext.Utils.refresh(o', m);
      check(
        list(string),
        "open under its new name",
        ["sum"],
        List.map((e: AgentContext.Model.entry) => e.path, m'.opened),
      );
      check(
        bool,
        "the same id",
        true,
        AgentContext.Utils.ids(m) == AgentContext.Utils.ids(m'),
      );
    },
  ),
  test_case(
    "fresh ids find an entry by its path",
    `Quick,
    () => {
      let src = "module M = { let f = 1; let g = 2 } in M.f";
      let (seg, _, _) = program(src);
      let m =
        switch (
          AgentContext.Update.resolve(
            Outline.build(~root=Exp, seg),
            Expand(["M/g"]),
          )
        ) {
        | Ok(r) => AgentContext.Update.update(r, AgentContext.Utils.init())
        | Error(e) => Alcotest.fail(e)
        };
      /* parsed again: new ids */
      let (seg', _, _) = program(src);
      let o' = Outline.build(~root=Exp, seg');
      let m' = AgentContext.Utils.refresh(o', m);
      check(int, "still open", 1, List.length(m'.opened));
      check(
        bool,
        "with the new id",
        true,
        AgentContext.Utils.ids(m') == ids_of(o', ["M/g"]),
      );
    },
  ),
  test_case(
    "an unknown path is an error naming the entries",
    `Quick,
    () => {
      let (seg, _, _) = program("let a = 1 in\nlet b = 2 in\na");
      switch (
        AgentContext.Update.resolve(
          Outline.build(~root=Exp, seg),
          Expand(["c"]),
        )
      ) {
      | Ok(_) => Alcotest.fail("resolved")
      | Error(e) =>
        check(
          bool,
          "lists the candidates",
          true,
          Util.StringUtil.plain_search("a, b", e, 0) >= 0,
        )
      };
    },
  ),
  test_case(
    "collapse all closes every entry",
    `Quick,
    () => {
      let (seg, _, _) = program("let a = 1 in\nlet b = 2 in\na");
      let o = Outline.build(~root=Exp, seg);
      let step = (m, a) =>
        switch (AgentContext.Update.resolve(o, a)) {
        | Ok(r) => AgentContext.Update.update(r, m)
        | Error(e) => Alcotest.fail(e)
        };
      let m = step(AgentContext.Utils.init(), Expand(["a", "b"]));
      check(int, "two open", 2, List.length(m.opened));
      let m = step(m, Collapse(["a"]));
      check(int, "one open", 1, List.length(m.opened));
      let m = step(m, CollapseAll);
      check(int, "none open", 0, List.length(m.opened));
    },
  ),
  test_case("the collapse tool's all flag", `Quick, () =>
    switch (
      CompositionUtils.Public.action_of(
        ~tool_name="collapse",
        ~args=`Assoc([("all", `Bool(true))]),
      )
    ) {
    | Action(AgentContextAction(CollapseAll)) => ()
    | _ => Alcotest.fail("not CollapseAll")
    }
  ),
  test_case(
    "a saved open set from before ids reads",
    `Quick,
    () => {
      let m =
        AgentContext.Model.t_of_sexp(
          Sexplib.Sexp.of_string("((expanded_paths (a M/f)))"),
        );
      check(
        list(string),
        "paths kept",
        ["a", "M/f"],
        List.map((e: AgentContext.Model.entry) => e.path, m.opened),
      );
    },
  ),
];

let tests = ("AgentView", format_tests @ open_set_tests);
