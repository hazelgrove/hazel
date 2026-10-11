open Alcotest;
open Haz3lcore;
open Language;

let rec mentions = (tok: string, seg: Segment.t): bool =>
  List.exists(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) =>
        Tile.label(t) == [tok] || List.exists(mentions(tok), t.children)
      | _ => false
      },
    seg,
  );

/* a one-item edit on mega-4k re-analyzes one item and one member */
let insitu = () => {
  let path = "hazel-programs/mega/mega-4k.hz";
  let path = Sys.file_exists(path) ? path : "../" ++ path;
  switch (CorpusUtil.read_file(path)) {
  | None => fail("corpus unreadable")
  | Some(src) =>
    let parse = txt =>
      FastParse.of_text(
        ~materialize=Triggers.invoked_projector,
        ~collect_refractors=true,
        ~root=Exp,
        txt,
      )
      |> Option.get;
    let seg = parse(src);
    let settings = CoreSettings.on;
    let term = MakeTerm.go(seg).term;
    let t0 = Sys.time();
    let ds0 = DefStatics.calc(~settings, term);
    Printf.printf(
      "INSITU cold calc: %.0f ms, %d items\n",
      (Sys.time() -. t0) *. 1000.0,
      DefStatics.last_analyzed^,
    );
    /* only SmithWorks' "16": the literal recurs in other items */
    let edited =
      Segment.top_items(seg)
      |> List.map(item =>
           mentions("SmithWorks", item)
             ? CorpusUtil.edit_token(~needle="16", ~repl="17", item)
             : (item, false)
         );
    check(int, "one item edited", 1, List.length(List.filter(snd, edited)));
    let spliced = List.concat_map(fst, edited);
    let term2 = MakeTerm.go(spliced).term;
    let t1 = Sys.time();
    let ds1 = DefStatics.calc(~settings, ~prev=ds0, term2);
    Printf.printf(
      "INSITU incr calc (1-item edit): %.0f ms, %d items analyzed\n",
      (Sys.time() -. t1) *. 1000.0,
      DefStatics.last_analyzed^,
    );
    check(int, "incr calc: item + 1 member", 2, DefStatics.last_analyzed^);
    ignore(ds1);
    /* the full path the browser runs: calc plus graft, folds and targets */
    let t2 = Sys.time();
    let cs0 =
      CachedStatics.init_compositional_term(
        ~settings,
        ~probe_ids=Id.Map.empty,
        term,
      );
    Printf.printf(
      "INSITU cold init_compositional_term: %.0f ms\n",
      (Sys.time() -. t2) *. 1000.0,
    );
    let t3 = Sys.time();
    let cs1 =
      CachedStatics.init_compositional_term(
        ~settings,
        ~probe_ids=Id.Map.empty,
        term2,
      );
    Printf.printf(
      "INSITU incr init_compositional_term: %.0f ms, %d items analyzed\n",
      (Sys.time() -. t3) *. 1000.0,
      DefStatics.last_analyzed^,
    );
    check(
      int,
      "incr init_compositional_term: item + 1 member",
      2,
      DefStatics.last_analyzed^,
    );
    ignore(cs0);
    ignore(cs1);
  };
};

let tests = (
  "LabelBench",
  [test_case("in-situ incremental calc", `Quick, insitu)],
);
