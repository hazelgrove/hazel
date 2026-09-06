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

let tests = [
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
      build_at_end("module Pos = {\n  let x = 1\n};\ntest true end;\n?") >= 0,
    )
  ),
  test_case("dungeon program", `Quick, () =>
    check(bool, "built", true, build_at_end(program) >= 0)
  ),
];
