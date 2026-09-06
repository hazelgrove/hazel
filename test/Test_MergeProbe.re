open Alcotest;
open Haz3lcore;
open Language;

/* Regression probe: the agent's node map (HighLevelNodeMap.build, which
   walks Info.ancestors to the program's top level) must build for the
   dungeon program at the buffer's end — the modular-editors merge broke
   this in the browser when the tool path was handed a per-item statics
   map (ancestors recorded per item). */
let program = {hz|module Lists = {
  let length : poly A -> [A] -> Int =
    typfun A -> fun xs ->
      case xs
      | [] => 0
      | _ :: tl => 1 + length@<A>(tl)
      end;

  let map : poly A -> poly B -> ((A -> B), [A]) -> [B] =
    typfun A -> typfun B -> fun f, xs ->
      case xs
      | [] => []
      | hd :: tl => f(hd) :: map@<A>@<B>(f, tl)
      end;

  let foldl : poly A -> poly B -> ((B, A) -> B, B, [A]) -> B =
    typfun A -> typfun B -> fun f, acc, xs ->
      case xs
      | [] => acc
      | hd :: tl => foldl@<A>@<B>(f, f(acc, hd), tl)
      end;

  let nth : poly A -> ([A], Int, A) -> A =
    typfun A -> fun xs, i, dflt ->
      case xs
      | [] => dflt
      | hd :: tl =>
        if i <= 0
        then hd
        else nth@<A>(tl, i - 1, dflt)
      end;

  let set_nth : poly A -> ([A], Int, A) -> [A] =
    typfun A -> fun xs, i, v ->
      case xs
      | [] => []
      | hd :: tl =>
        if i <= 0
        then v :: tl
        else hd :: set_nth@<A>(tl, i - 1, v)
      end;

  let range(n: Int): [Int] =
    if n <= 0
    then []
    else range(n - 1) @ [n - 1]
} in

module Pos = {
  type T = (Int, Int);

  type Dir =
    + North
    + South
    + East
    + West;

  let step(d: Dir, p: T): T =
    let (x, y) = p in
    case d
    | North => (x, y - 1)
    | South => (x, y + 1)
    | East => (x + 1, y)
    | West => (x - 1, y)
    end;

  let neighbors(p: T): [T] =
    [step(North, p), step(South, p), step(East, p), step(West, p)];

  let abs_int(n: Int): Int =
    if n < 0
    then 0 - n
    else n;

  let manhattan(a: T, b: T): Int =
    let (ax, ay) = a in
    let (bx, by) = b in
    abs_int(ax - bx) + abs_int(ay - by);

  let eq(a: T, b: T): Bool =
    let (ax, ay) = a in
    let (bx, by) = b in
    ax == bx && ay == by
} in

module Dungeon = {
  type Tile =
    + Floor
    + Wall
    + Door(Bool)
    + Stairs;

  type Room = (Int, Int, [Tile]);

  let width(r: Room): Int =
    let (w, _, _) = r in
    w;

  let height(r: Room): Int =
    let (_, h, _) = r in
    h;

  let in_bounds(r: Room, p: Pos.T): Bool =
    let (w, h, _) = r in
    let (x, y) = p in
    x >= 0 && x < w && y >= 0 && y < h;

  let get(r: Room, p: Pos.T): Tile =
    let (w, _, ts) = r in
    let (x, y) = p in
    if in_bounds(r, p)
    then Lists.nth@<Tile>(ts, y * w + x, Wall)
    else Wall;

  let set(r: Room, p: Pos.T, t: Tile): Room =
    let (w, h, ts) = r in
    let (x, y) = p in
    if in_bounds(r, p)
    then (w, h, Lists.set_nth@<Tile>(ts, y * w + x, t))
    else r;

  let passable(t: Tile): Bool =
    case t
    | Floor => true
    | Stairs => true
    | Door(open) => open
    | Wall => false
    end;

  let can_enter(r: Room, p: Pos.T): Bool =
    passable(get(r, p));

  module Carve = {
    let solid(w: Int, h: Int): Room =
      (w, h, Lists.map@<Int>@<Tile>(fun _ -> Wall, Lists.range(w * h)));

    let on_border(w: Int, h: Int, p: Pos.T): Bool =
      let (x, y) = p in
      x == 0 || y == 0 || x == w - 1 || y == h - 1;

    let rect(w: Int, h: Int): Room =
      let cells = Lists.range(w * h) in
      let tile_at(i: Int): Tile =
        let p = (int_mod(i, w), i / w) in
        if on_border(w, h, p)
        then Wall
        else Floor
      in
      (w, h, Lists.map@<Int>@<Tile>(tile_at, cells));

    let cut_door(r: Room, p: Pos.T, open: Bool): Room =
      case get(r, p)
      | Wall => set(r, p, Door(open))
      | _ => r
      end;

    let place_stairs(r: Room, p: Pos.T): Room =
      case get(r, p)
      | Floor => set(r, p, Stairs)
      | _ => r
      end
  }
} in
test Pos.manhattan((1, 1), (4, 5)) == 7 end;
test Pos.eq(Pos.step(North, (2, 2)), (2, 1)) end;
test Pos.neighbors((0, 0)) == [(0, -1), (0, 1), (1, 0), (-1, 0)] end;
?|hz};

let mk_statics = (z: Zipper.t): StaticsBase.Map.t =>
  fst(
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      MakeTerm.from_zip_for_sem(z, ~root=Exp).term,
    ),
  );

let build_at_end = (code: string): int =>
  switch (Parser.to_zipper(~root=Exp, code)) {
  | None => fail("parse failed")
  | Some(z) =>
    let z = Move.to_end(z);
    let m = mk_statics(z);
    switch (HighLevelNodeMap.build(z, m)) {
    | Some(nm) => Id.Map.cardinal(nm)
    | None => (-1)
    };
  };

/* module member typing probes (mega-corpus regression after the merge) */
let error_count = (code: string): int =>
  switch (Parser.to_zipper(~root=Exp, code)) {
  | None => fail("parse failed")
  | Some(z) =>
    let m = mk_statics(z);
    let errs = StaticsBase.Map.error_ids(m);
    List.iter(e => print_endline("PROBE-ERR " ++ e), ErrorPrint.all(m));
    List.length(errs);
  };

/* dump: top-level term shape + the type recorded for a variable */
let dump_var_type = (code: string, var: string): unit =>
  switch (Parser.to_zipper(~root=Exp, code)) {
  | None => fail("parse failed")
  | Some(z) =>
    let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    let ctor =
      switch (Exp.term_of(term)) {
      | ModuleExp(_) => "ModuleExp"
      | Let(_) => "Let"
      | Seq(_) => "Seq"
      | _ => "other"
      };
    print_endline("DUMP top=" ++ ctor);
    let m = mk_statics(z);
    let found = ref(false);
    Id.Map.iter(
      (_, info) =>
        switch (info) {
        | Info.InfoExp({user_term: {term: Var(v), _}, ty, _}) when v == var =>
          found := true;
          print_endline("DUMP var " ++ v ++ " : " ++ Typ.show(ty));
        | _ => ()
        },
      m,
    );
    if (! found^) {
      print_endline("DUMP var " ++ var ++ " not in map");
    };
    /* the ctx at the Dot: is the module name bound there? */
    Id.Map.iter(
      (_, info) =>
        switch (info) {
        | Info.InfoExp({user_term: {term: Dot(_, _), _}, ctx, _}) =>
          let names =
            Ctx.get_var_entries(ctx)
            |> List.map((v: Ctx.var_entry) => v.name);
          print_endline(
            "DUMP dot-ctx vars="
            ++ String.concat(",", List.filteri((i, _) => i < 8, names))
            ++ " lookup_var("
            ++ var
            ++ ")="
            ++ string_of_bool(Ctx.lookup_var(ctx, var) != None),
          );
        | _ => ()
        },
      m,
    );
  };

let module_probes = [
  test_case(
    "module named Foo (not a builtin constructor), member access", `Quick, () =>
    check(
      int,
      "errors",
      0,
      error_count(
        "module Foo = {\n  let f = fun n -> \"x\"\n} in\nFoo.f(0) == \"x\"",
      ),
    )
  ),
  test_case(
    "dump module form",
    `Quick,
    () => {
      dump_var_type(
        "module Text = {\n  let f = fun n -> \"x\"\n} in\nText.f(0) == \"x\"",
        "Text",
      );
      dump_var_type(
        "let m = {\n  let f = fun n -> \"x\"\n} in\nm.f(0) == \"x\"",
        "m",
      );
    },
  ),
  test_case("labeled tuple projection", `Quick, () =>
    check(int, "errors", 0, error_count("let t = (a=1, b=2) in t.a == 1"))
  ),
  test_case("let-bound module, no member access", `Quick, () =>
    check(
      int,
      "errors",
      0,
      error_count("let m = {\n  let f = fun n -> \"x\"\n} in\n1 == 1"),
    )
  ),
  test_case("let-bound module, member access", `Quick, () =>
    check(
      int,
      "errors",
      0,
      error_count(
        "let m = {\n  let f = fun n -> \"x\"\n} in\nm.f(0) == \"x\"",
      ),
    )
  ),
  test_case("module member, plain fun", `Quick, () =>
    check(
      int,
      "errors",
      0,
      error_count(
        "module Text = {\n  let f = fun n -> \"x\"\n};\ntest Text.f(0) == \"x\" end;\n?",
      ),
    )
  ),
  test_case("module member, ascribed fun", `Quick, () =>
    check(
      int,
      "errors",
      0,
      error_count(
        "module Text = {\n  let f : Int -> String = fun n -> \"x\"\n};\ntest Text.f(0) == \"x\" end;\n?",
      ),
    )
  ),
  test_case("module member used inside the module (ascribed)", `Quick, () =>
    check(
      int,
      "errors",
      0,
      error_count(
        "module Text = {\n  let f : Int -> String = fun n -> \"x\";\n  let g : () -> Bool = fun _ -> f(0) == \"x\"\n};\ntest Text.g() end;\n?",
      ),
    )
  ),
  test_case("let-in module (dev style)", `Quick, () =>
    check(
      int,
      "errors",
      0,
      error_count(
        "module Text = {\n  let f : Int -> String = fun n -> \"x\"\n} in\nText.f(0) == \"x\"",
      ),
    )
  ),
];

/* which sub-terms of an item are absent from the item's own map? */
let rec sub_terms = (e: Exp.t): list(Exp.t) => [
  e,
  ...List.concat_map(
       sub_terms,
       HighLevelNodeMap.Utils.child_expressions_of_exp(e),
     ),
];
let missing_in_item_maps = (ds: DefStatics.t): unit => {
  let rec go = (prefix, items: list(DefStatics.item)) =>
    List.iter(
      (it: DefStatics.item) => {
        let name =
          switch (Id.Map.find_opt(it.d_id, it.d_map)) {
          | Some(info) =>
            try(HighLevelNodeMap.Namer.mk_name(info)) {
            | _ => "?"
            }
          | None => "<root not in map>"
          };
        let missing =
          sub_terms(it.d_node)
          |> List.filter(e => !Id.Map.mem(Exp.rep_id(e), it.d_map));
        if (missing != []) {
          let ctor = (e: Exp.t) =>
            switch (Exp.term_of(e)) {
            | Let(_) => "Let"
            | Fun(_) => "Fun"
            | Ap(_) => "Ap"
            | Var(_) => "Var"
            | Module(_) => "Module"
            | ModuleExp(_) => "ModuleExp"
            | EmptyHole => "Hole"
            | Match(_) => "Match"
            | Tuple(_) => "Tuple"
            | Parens(_) => "Parens"
            | _ => "exp"
            };
          print_endline(
            "ITEMMAP "
            ++ prefix
            ++ name
            ++ ": "
            ++ string_of_int(List.length(missing))
            ++ " missing, first: "
            ++ String.concat(
                 ",",
                 List.filteri((i, _) => i < 4, List.map(ctor, missing)),
               ),
          );
        };
        go(prefix ++ name ++ "/", it.d_members);
      },
      items,
    );
  go("", ds.items);
};

/* parity: node map from the per-item engine == node map from the
   monolithic map (same node ids, same paths) */
let parity = (code: string): (int, int, int) =>
  switch (Parser.to_zipper(~root=Exp, code)) {
  | None => fail("parse failed")
  | Some(z) =>
    let z = Move.to_end(z);
    let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    let mono =
      switch (HighLevelNodeMap.build(z, mk_statics(z))) {
      | Some(nm) => nm
      | None => fail("monolithic node map: None")
      };
    let ds = DefStatics.calc(~settings=CoreSettings.on, term);
    HighLevelNodeMap.items_fallbacks := [];
    missing_in_item_maps(ds);
    let items =
      switch (HighLevelNodeMap.build_from_items(ds)) {
      | Some(nm) => nm
      | None => fail("items node map: None")
      };
    let paths = (nm: HighLevelNodeMap.t) =>
      Id.Map.bindings(nm)
      |> List.map(((id, n: HighLevelNodeMapModel.node)) => (id, n.path));
    let (pm, pi) = (paths(mono), paths(items));
    let name_of = (nm: HighLevelNodeMap.t, id) =>
      switch (Id.Map.find_opt(id, nm)) {
      | Some(n: HighLevelNodeMapModel.node) => n.name
      | None => "?"
      };
    List.iter(
      c => print_endline("PARITY fallback " ++ c),
      HighLevelNodeMap.items_fallbacks^,
    );
    let missing = List.filter(b => !List.mem(b, pi), pm);
    let extra = List.filter(b => !List.mem(b, pm), pi);
    List.iter(
      ((id, _)) =>
        print_endline(
          "PARITY missing-in-items "
          ++ Id.to_string(id)
          ++ " "
          ++ name_of(mono, id),
        ),
      missing,
    );
    List.iter(
      ((id, _)) =>
        print_endline(
          "PARITY extra-in-items "
          ++ Id.to_string(id)
          ++ " "
          ++ name_of(items, id),
        ),
      extra,
    );
    (Id.Map.cardinal(mono), List.length(missing), List.length(extra));
  };
let parity_probes = [
  test_case(
    "node map parity: small module program",
    `Quick,
    () => {
      let (n, missing, extra) =
        parity(
          "module Foo = {\n  let f = fun n -> \"x\";\n  type T = Int\n};\nlet g = fun y -> let h = 1 in y in\ntest Foo.f(0) == \"x\" end;\n?",
        );
      check(bool, "nonempty", true, n > 0);
      check(int, "missing", 0, missing);
      check(int, "extra", 0, extra);
    },
  ),
  test_case(
    "node map parity: dungeon program",
    `Quick,
    () => {
      let (n, missing, extra) = parity(program);
      check(bool, "nonempty", true, n > 0);
      check(int, "missing", 0, missing);
      check(int, "extra", 0, extra);
    },
  ),
];

let tests =
  parity_probes
  @ module_probes
  @ [
    test_case("let-chain program", `Quick, () =>
      check(
        bool,
        "built",
        true,
        build_at_end("let x = 1 in\nlet y = 2 in\n?") >= 0,
      )
    ),
    test_case("small ;-program (module item + test)", `Quick, () =>
      check(
        bool,
        "built",
        true,
        build_at_end("module Pos = {\n  let x = 1\n};\ntest true end;\n?")
        >= 0,
      )
    ),
    test_case("dungeon program", `Quick, () =>
      check(bool, "built", true, build_at_end(program) >= 0)
    ),
  ];
