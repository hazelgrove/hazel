/* The node map from DefStatics' item chain (the canvas reads it per item)
   names every node of a Mod root as the whole-program analysis does */
open Alcotest;
open Haz3lcore;
open Language;
open Util;
open Poly;

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
    |> List.map(~f=((id, n: HighLevelNodeMap.node)) =>
         (
           id,
           String.concat(
             ~sep="/",
             HighLevelNodeMap.id_path_to_name_path(n.path, nm),
           ),
         )
       );
  named
  |> List.map(~f=((id, base)) =>
       switch (
         List.filter_map(~f=((i, b)) => b == base ? Some(i) : None, named)
         |> HighLevelNodeMap.sort_ids_in_program_order(nm)
       ) {
       | [_] => (base, id)
       | ids =>
         let k = Option.value_exn(ListUtil.findi_opt(i => i == id, ids));
         (base ++ "#" ++ string_of_int(fst(k) + 1), id);
       }
     )
  |> List.sort(~compare);
};

let parity = (z: Zipper.t) => {
  let get = (what, m) =>
    switch (m) {
    | Some(m) => agent_paths(m)
    | None => fail(what ++ ": no node map")
    };
  let whole = get("whole", whole_node_map(z));
  let items =
    get(
      "per item",
      HighLevelNodeMap.build_from_items(
        DefStatics.calc_auto(
          ~settings=CoreSettings.on,
          ~probe_ids=CachedStatics.probe_ids_of_zipper(z),
          MakeTerm.semantic_term(~root=Mod, MakeTerm.semantic_source(z)),
        ),
      ),
    );
  check(
    list(string),
    "paths",
    List.map(~f=fst, whole),
    List.map(~f=fst, items),
  );
  check(
    list(string),
    "nodes",
    List.map(~f=((p, id)) => p ++ " " ++ Id.to_string(id), whole),
    List.map(~f=((p, id)) => p ++ " " ++ Id.to_string(id), items),
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
        ~f=((_, p)) => parity(PersistentZipper.unpersist(p, ~root=Mod)),
        Docslides.Slides.mod_slides,
      )
    ),
  ],
);

let tests = [parity_tests];
