open Alcotest;
open Haz3lcore;
open Language;

/* Alias-body formers fold into the alias node: `type Pos = (Int, Int)`
 * is ONE node wearing a "()" badge with two component terminals feeding
 * it — no separate "()@Pos" former. Anonymous formers (a tuple written
 * inline in a signature) keep their own Product node. */

let prog = {|type Pos = (Int, Int) in
type Deck = [Pos] in
type Msg = Inc + Move(Pos) in
let step : (Pos, Msg) -> Pos =
fun (p, m) -> p in
step((1, 2), Inc)|};

let graph_of = (code: string): Web.CanvasGraph.t =>
  switch (Parser.to_zipper(~root=Exp, code)) {
  | None => fail("failed to parse program")
  | Some(z) =>
    let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Exp);
    let statics =
      CachedStatics.init_from_term(
        ~settings=CoreSettings.on,
        ~is_dynamic_term=false,
        term,
      );
    Web.CanvasGraph.extract(statics);
  };

let node = (g: Web.CanvasGraph.t, key: string): Web.CanvasGraph.tynode =>
  switch (
    List.find_opt((n: Web.CanvasGraph.tynode) => n.key == key, g.nodes)
  ) {
  | Some(n) => n
  | None => fail("no node " ++ key)
  };
let has = (g: Web.CanvasGraph.t, key: string): bool =>
  List.exists((n: Web.CanvasGraph.tynode) => n.key == key, g.nodes);

let tests = [
  test_case(
    "product alias wears the () badge and owns its parts",
    `Quick,
    () => {
      let g = graph_of(prog);
      let pos = node(g, "Pos");
      check(option(string), "former", Some("()"), pos.former);
      check(int, "two component terminals", 2, List.length(pos.parts));
      check(bool, "no separate former node", false, has(g, "()@Pos"));
      /* the terminals dock at the alias itself */
      List.iter(
        pk =>
          check(
            option(string),
            "part " ++ pk ++ " anchored at Pos",
            Some("Pos"),
            node(g, pk).sat |> Option.map(fst),
          ),
        pos.parts,
      );
    },
  ),
  test_case(
    "list alias wears the [] badge",
    `Quick,
    () => {
      let g = graph_of(prog);
      let deck = node(g, "Deck");
      check(option(string), "former", Some("[]"), deck.former);
      check(list(string), "element feeds it", ["Pos"], deck.parts);
      check(bool, "no separate former node", false, has(g, "[]@Deck"));
    },
  ),
  test_case(
    "sum alias wears the + badge with no parts",
    `Quick,
    () => {
      let g = graph_of(prog);
      let msg = node(g, "Msg");
      check(option(string), "former", Some("+"), msg.former);
      check(int, "no parts", 0, List.length(msg.parts));
    },
  ),
  test_case(
    "anonymous input tuples keep their product node",
    `Quick,
    () => {
      let g = graph_of(prog);
      check(
        bool,
        "a Product node exists for step's (Pos, Msg) input",
        true,
        List.exists(
          (n: Web.CanvasGraph.tynode) =>
            n.kind == Web.CanvasGraph.Product && n.former == None,
          g.nodes,
        ),
      );
    },
  ),
  test_case(
    "a repeated alias name gets a distinct node key",
    `Quick,
    () => {
      let g = graph_of("type Pos = Int in type Pos = Bool in 1");
      let keys =
        List.filter(
          k => String.length(k) >= 3 && String.sub(k, 0, 3) == "Pos",
          List.map((n: Web.CanvasGraph.tynode) => n.key, g.nodes),
        );
      check(list(string), "keys", ["Pos", "Pos#2"], keys);
    },
  ),
];
