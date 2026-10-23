/* The agent's tools at a Mod root read the program per item, as at an Exp
   root: the node map from DefStatics' item chain names every node as the
   whole-program analysis did, and the tool calls it serves are timed */
open Alcotest;
open Haz3lcore;
open Language;
open Util;

/* the node map of a whole-program analysis, as the tools read a Mod root
   before they went per item */
let whole_node_map = (z: Zipper.t): option(HighLevelNodeMap.t) => {
  let (seg, masks) =
    MakeTerm.semantic_segment(~root=Mod, MakeTerm.semantic_source(z));
  let term = MakeTerm.Incr.term_of_root(~masks, ~root=Mod, seg);
  let (info_map, _) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      term,
    );
  HighLevelNodeMap.build(z, info_map);
};

/* each node's path as the agent writes it, `#k` where several share it,
   and the node it resolves to */
let agent_paths = (nm: HighLevelNodeMap.t): list((string, Id.t)) => {
  let named =
    Id.Map.bindings(nm)
    |> List.map(((id, n: HighLevelNodeMap.node)) =>
         (
           id,
           String.concat(
             "/",
             HighLevelNodeMap.id_path_to_name_path(n.path, nm),
           ),
         )
       );
  named
  |> List.map(((id, base)) =>
       switch (
         List.filter_map(((i, b)) => b == base ? Some(i) : None, named)
         |> HighLevelNodeMap.sort_ids_in_program_order(nm)
       ) {
       | [_] => (base, id)
       | ids =>
         let k = Option.get(ListUtil.findi_opt(i => i == id, ids));
         (base ++ "#" ++ string_of_int(fst(k) + 1), id);
       }
     )
  |> List.sort(compare);
};

let parity = (z: Zipper.t) => {
  let get = (what, m) =>
    switch (m) {
    | Some(m) => agent_paths(m)
    | None => fail(what ++ ": no node map")
    };
  let whole = get("whole", whole_node_map(z));
  let items =
    get("per item", CompositionGo.Public.node_map_of(~root=Mod, z));
  check(list(string), "paths", List.map(fst, whole), List.map(fst, items));
  check(
    list(string),
    "nodes",
    List.map(((p, id)) => p ++ " " ++ Id.to_string(id), whole),
    List.map(((p, id)) => p ++ " " ++ Id.to_string(id), items),
  );
};

let of_text = (src: string): Zipper.t =>
  Zipper.unzip(CorpusUtil.seg_of(~root=Mod, src));

/* the forms a Mod root's node map has to name: members lowered from a
   module literal, top-level tests and statements, a sealed module, a
   shadowed name (`#k`), a livelit's members, a let-in under a binding */
let edge_program = "module M = {
  let a = 1;
  test a == 1 end;
  let b = a + 1;
  type T = Int;
  let c : T = b
};
module S : {let x : Int; let y : Int} = {
  let x = 1;
  let y = x + 1
};
let a = 5;
test a == 5 end;
a + 1;
let a = a + 1;
let ^w = { let x = 1; let y = 2 };
let m = { let p = 1; let p = 2 };
let f = fun x -> let inner = x in inner;
let d : M.T = M.a + a + S.y;
d";

let parity_tests = (
  "AgentTools.ModRootParity",
  [
    test_case("the ModRoot program", `Quick, () =>
      parity(of_text(Test_AgentTools.mod_root_program))
    ),
    test_case("lowered, sealed, shadowed and livelit members", `Quick, () =>
      parity(of_text(edge_program))
    ),
    test_case("Mega-Mod 1k", `Slow, () =>
      parity(Zipper.unzip(CorpusUtil.mega_seg(~root=Mod, "mega-mod-1k.hz")))
    ),
    test_case("the module-rooted slides", `Slow, () =>
      List.iter(
        ((_, p)) => parity(PersistentZipper.unpersist(p, ~root=Mod)),
        Docslides.Slides.mod_slides,
      )
    ),
  ],
);

/* tool calls on Mega-Mod 1k with the editor's per-item analysis warm, as
   the app has it: the action and its diff. HAZEL_BENCH=1 only */
let bench = () => {
  let now = JsUtil.precise_timestamp;
  let z = Zipper.unzip(CorpusUtil.mega_seg(~root=Mod, "mega-mod-1k.hz"));
  let syntax = CachedSyntax.init(~root=Mod, z);
  let warm = () =>
    ignore(
      CachedStatics.init_compositional(
        ~settings=CoreSettings.on,
        ~stitch=x => x,
        ~root=Mod,
        z,
      ),
    );
  let run = (name, a: Action.Structural.t) => {
    let once = () => {
      warm();
      let t0 = now();
      let new_z =
        switch (
          Perform.go(
            ~settings=CoreSettings.on,
            ~statics=CachedStatics.empty,
            ~syntax,
            ~root=Mod,
            Structural(a),
            {
              zipper: z,
              col_target: None,
            },
          )
        ) {
        | Ok(z) => z
        | Error(e) => fail(name ++ ": " ++ Action.Failure.show(e))
        };
      let go = now() -. t0;
      let new_syntax = CachedSyntax.init(~root=Mod, new_z);
      let t1 = now();
      ignore(
        CompositionGo.Public.get_diff(
          ~settings=CoreSettings.on,
          ~root=Mod,
          z,
          new_z,
          a,
          ~old_syntax=syntax,
          ~new_syntax,
        ),
      );
      (go, now() -. t1);
    };
    let runs = List.init(7, _ => once()) |> List.tl;
    let median = xs =>
      List.nth(List.sort(compare, xs), List.length(xs) / 2);
    let (go, diff) = (
      median(List.map(fst, runs)),
      median(List.map(snd, runs)),
    );
    Printf.printf(
      "MODROOT-BENCH %s: action %.0f ms, diff %.0f ms, total %.0f ms\n",
      name,
      go,
      diff,
      go +. diff,
    );
  };
  run(
    "update_definition",
    Update(
      Definition,
      "Text/int_to_text",
      "fun n -> if n < 0 then \"-\" ++ go_render(0 - n) else go_render(n)",
    ),
  );
  run("insert_after", Insert(After, "Text/digit", "let zz = 1"));
};

let bench_tests = (
  "AgentTools.ModRootBench",
  [
    test_case("tool calls on Mega-Mod 1k", `Slow, () =>
      if (CorpusUtil.bench_enabled) {
        bench();
      }
    ),
  ],
);

let tests = [parity_tests, bench_tests];
