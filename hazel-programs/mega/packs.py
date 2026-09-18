"""Synthesized realistic MVU-style modules for the mega corpus
(pack A -> mega-2k, pack A+B -> mega-4k). Written for dev-compatible
Hazel: param-ascribed fun members (see build_mega.defunlet note),
`;`-separated members, no funlet members, no spread patterns, no
string_of_int, string_split(sep, str) arg order, reserved words
avoided (eval/pause/debug/hide/use/fix/test/hint/typfun/poly/
named_fun/rec/end/then/of)."""

PACK_A = []

PACK_A.append(("LunarLife", """module LunarLife = {
  # Conway-style life on a small moonlit grid #
  type Grid = [[Bool]];

  let cell_at = fun (g: Grid, r: Int, c: Int) ->
    if r < 0 || c < 0 || r >= length(g)
    then false
    else
      let row = nth(g, r) in
      if c >= length(row) then false else nth(row, c);

  let neighbors = fun (g: Grid, r: Int, c: Int) ->
    let count_one = fun (dr: Int, dc: Int) ->
      if cell_at(g, r + dr, c + dc) then 1 else 0 in
    count_one(0 - 1, 0 - 1) + count_one(0 - 1, 0) + count_one(0 - 1, 1)
    + count_one(0, 0 - 1) + count_one(0, 1)
    + count_one(1, 0 - 1) + count_one(1, 0) + count_one(1, 1);

  let step_cell = fun (alive: Bool, n: Int) ->
    if alive
    then n == 2 || n == 3
    else n == 3;

  let step = fun (g: Grid) ->
    mapi(g, fun (r, row) ->
      mapi(row, fun (c, alive) ->
        step_cell(alive, neighbors(g, r, c))
      )
    );

  let population = fun (g: Grid) ->
    fold_left(g, fun (acc, row) ->
      acc + fold_left(row, fun (a2, alive) ->
        if alive then a2 + 1 else a2
      , 0)
    , 0);

  # vertical blinker in a 3x3 grid #
  let blinker : Grid = [
    [false, true, false],
    [false, true, false],
    [false, true, false]
  ];

  let selfcheck : () -> Bool = fun _ ->
    let g1 = step(blinker) in
    let g2 = step(g1) in
    population(blinker) == 3
    && population(g1) == 3
    && cell_at(g1, 1, 0) && cell_at(g1, 1, 1) && cell_at(g1, 1, 2)
    && g2 == blinker
} in
"""))

PACK_A.append(("NightMarket", """module NightMarket = {
  # A tiny market: stock, prices, coin ledger #
  type Item = (label = String, price = Int, stock = Int);
  type Model = (items = [Item], coins = Int);
  type Action =
    + Sell(String, Int)
    + Restock(String, Int)
    + Reprice(String, Int);

  let init : Model = (
    items = [
      (label = "lantern", price = 12, stock = 4),
      (label = "rope", price = 3, stock = 10),
      (label = "chalk", price = 1, stock = 25)
    ],
    coins = 0
  );

  let stock_of = fun (m: Model, which: String) ->
    fold_left(m.items, fun (acc, it) ->
      if it.label == which then it.stock else acc
    , 0);

  let earn = fun (m: Model, which: String, n: Int) ->
    fold_left(m.items, fun (acc, it) ->
      if it.label == which then acc + n * it.price else acc
    , 0);

  let update = fun (m: Model, a: Action) ->
    case a
    | Sell(which, n) =>
        if stock_of(m, which) < n
        then m
        else (
          items =
            map(m.items, fun it ->
              if it.label == which
              then (label = it.label, price = it.price, stock = it.stock - n)
              else it),
          coins = m.coins + earn(m, which, n)
        )
    | Restock(which, n) => (
        items =
          map(m.items, fun it ->
            if it.label == which
            then (label = it.label, price = it.price, stock = it.stock + n)
            else it),
        coins = m.coins
      )
    | Reprice(which, p) => (
        items =
          map(m.items, fun it ->
            if it.label == which
            then (label = it.label, price = p, stock = it.stock)
            else it),
        coins = m.coins
      )
    end;

  let run = fun (m: Model, actions: [Action]) ->
    fold_left(actions, fun (acc, a) -> update(acc, a), m);

  let selfcheck : () -> Bool = fun _ ->
    let m1 = run(init, [Sell("rope", 2), Sell("lantern", 1)]) in
    let m2 = run(m1, [Sell("lantern", 99)]) in
    let m3 = run(m2, [Reprice("chalk", 2), Sell("chalk", 5)]) in
    m1.coins == 18
    && stock_of(m1, "rope") == 8
    && m2 == m1
    && m3.coins == 28
    && stock_of(m3, "chalk") == 20
} in
"""))

PACK_A.append(("StarStopwatch", """module StarStopwatch = {
  # Stopwatch with laps, driven by ticks #
  type Model = (running = Bool, elapsed = Int, laps = [Int]);
  type Action =
    + Toggle
    + Tick(Int)
    + Lap
    + Wipe;

  let init : Model = (running = false, elapsed = 0, laps = []);

  let update = fun (m: Model, a: Action) ->
    case a
    | Toggle => (running = !m.running, elapsed = m.elapsed, laps = m.laps)
    | Tick(dt) =>
        if m.running
        then (running = true, elapsed = m.elapsed + dt, laps = m.laps)
        else m
    | Lap => (running = m.running, elapsed = m.elapsed,
              laps = m.elapsed :: m.laps)
    | Wipe => init
    end;

  let run = fun (m: Model, actions: [Action]) ->
    fold_left(actions, fun (acc, a) -> update(acc, a), m);

  let lap_count = fun (m: Model) -> length(m.laps);

  let best_lap = fun (m: Model) ->
    fold_left(m.laps, fun (acc, l) ->
      if acc == 0 || l < acc then l else acc
    , 0);

  let selfcheck : () -> Bool = fun _ ->
    let m1 = run(init, [Tick(5), Toggle, Tick(3), Lap, Tick(4), Lap]) in
    let m2 = run(m1, [Wipe]) in
    m1.elapsed == 7
    && lap_count(m1) == 2
    && best_lap(m1) == 3
    && m2 == init
} in
"""))

PACK_A.append(("OwlPost", """module OwlPost = {
  # Parcel routing between roosts #
  type Parcel = (dest = String, hops = Int);
  type Model = (queue = [Parcel], delivered = Int, lost = Int);
  type Action =
    + Accept(String)
    + Fly(String)
    + Storm;

  let init : Model = (queue = [], delivered = 0, lost = 0);

  let update = fun (m: Model, a: Action) ->
    case a
    | Accept(d) => (
        queue = (dest = d, hops = 0) :: m.queue,
        delivered = m.delivered,
        lost = m.lost
      )
    | Fly(hub) =>
        let arrived =
          filter(m.queue, fun p -> p.dest == hub) in
        let still =
          filter(m.queue, fun p -> !(p.dest == hub)) in
        (
          queue = map(still, fun p -> (dest = p.dest, hops = p.hops + 1)),
          delivered = m.delivered + length(arrived),
          lost = m.lost
        )
    | Storm =>
        let kept = filter(m.queue, fun p -> p.hops < 3) in
        (
          queue = kept,
          delivered = m.delivered,
          lost = m.lost + (length(m.queue) - length(kept))
        )
    end;

  let run = fun (m: Model, actions: [Action]) ->
    fold_left(actions, fun (acc, a) -> update(acc, a), m);

  let selfcheck : () -> Bool = fun _ ->
    let m1 = run(init, [Accept("elm"), Accept("oak"), Fly("oak")]) in
    let m2 = run(m1, [Fly("birch"), Fly("birch"), Fly("birch"), Storm]) in
    m1.delivered == 1
    && length(m1.queue) == 1
    && m2.lost == 1
    && length(m2.queue) == 0
} in
"""))

PACK_A.append(("RuneCipher", """module RuneCipher = {
  # A toy shift cipher over rune indices 0..28 #
  let rune_count : Int = 29;

  let wrap = fun (n: Int) ->
    int_mod(int_mod(n, rune_count) + rune_count, rune_count);

  let encode_one = fun (k: Int, r: Int) -> wrap(r + k);

  let decode_one = fun (k: Int, r: Int) -> wrap(r - k);

  let encode = fun (k: Int, rs: [Int]) ->
    map(rs, fun r -> encode_one(k, r));

  let decode = fun (k: Int, rs: [Int]) ->
    map(rs, fun r -> decode_one(k, r));

  let checksum = fun (rs: [Int]) ->
    fold_left(rs, fun (acc, r) -> int_mod(acc * 31 + r, 9973), 7);

  let selfcheck : () -> Bool = fun _ ->
    let msg = [3, 0, 27, 14, 9, 22] in
    let hidden = encode(11, msg) in
    let opened = decode(11, hidden) in
    opened == msg
    && !(hidden == msg)
    && checksum(msg) == checksum(opened)
    && encode_one(5, 27) == 3
} in
"""))

PACK_A.append(("GladeInventory", """module GladeInventory = {
  # Crafting: consume ingredients, produce goods #
  type Model = (wood = Int, resin = Int, torches = Int, planks = Int);
  type Action =
    + Gather(Int, Int)
    + CraftTorch
    + CraftPlank;

  let init : Model = (wood = 0, resin = 0, torches = 0, planks = 0);

  let update = fun (m: Model, a: Action) ->
    case a
    | Gather(w, r) => (
        wood = m.wood + w, resin = m.resin + r,
        torches = m.torches, planks = m.planks
      )
    | CraftTorch =>
        if m.wood >= 1 && m.resin >= 2
        then (
          wood = m.wood - 1, resin = m.resin - 2,
          torches = m.torches + 1, planks = m.planks
        )
        else m
    | CraftPlank =>
        if m.wood >= 3
        then (
          wood = m.wood - 3, resin = m.resin,
          torches = m.torches, planks = m.planks + 1
        )
        else m
    end;

  let run = fun (m: Model, actions: [Action]) ->
    fold_left(actions, fun (acc, a) -> update(acc, a), m);

  let worth = fun (m: Model) ->
    m.wood + 2 * m.resin + 5 * m.torches + 4 * m.planks;

  let selfcheck : () -> Bool = fun _ ->
    let m1 = run(init, [Gather(5, 4), CraftTorch, CraftTorch]) in
    let m2 = run(m1, [CraftPlank, CraftPlank]) in
    m1.torches == 2
    && m1.wood == 3 && m1.resin == 0
    && m2.planks == 1 && m2.wood == 0
    && worth(m2) == 14
} in
"""))


# ---------------------------------------------------------------------
# Parameterized families: realistic module variants with distinct data
# tables and logic constants; selfcheck expectations are COMPUTED here
# by simulating the same logic, so they provably pass.

def market_module(name, items, sells):
    """items: [(label, price, stock)]; sells: [(label, n)]"""
    stock = {l: s for l, p, s in items}
    price = {l: p for l, p, s in items}
    coins = 0
    for l, n in sells:
        if stock.get(l, 0) >= n:
            stock[l] -= n
            coins += n * price[l]
    first = sells[0][0]
    items_src = ",\n      ".join(
        f'(label = "{l}", price = {p}, stock = {s})' for l, p, s in items)
    sells_src = ", ".join(f'Sell("{l}", {n})' for l, n in sells)
    return (name, f"""module {name} = {{
  # Market variant: distinct stock table and sale ledger #
  type Item = (label = String, price = Int, stock = Int);
  type Model = (items = [Item], coins = Int);
  type Action =
    + Sell(String, Int)
    + Restock(String, Int);

  let init : Model = (
    items = [
      {items_src}
    ],
    coins = 0
  );

  let stock_of = fun (m: Model, which: String) ->
    fold_left(m.items, fun (acc, it) ->
      if it.label == which then it.stock else acc
    , 0);

  let earn = fun (m: Model, which: String, n: Int) ->
    fold_left(m.items, fun (acc, it) ->
      if it.label == which then acc + n * it.price else acc
    , 0);

  let update = fun (m: Model, a: Action) ->
    case a
    | Sell(which, n) =>
        if stock_of(m, which) < n
        then m
        else (
          items =
            map(m.items, fun it ->
              if it.label == which
              then (label = it.label, price = it.price, stock = it.stock - n)
              else it),
          coins = m.coins + earn(m, which, n)
        )
    | Restock(which, n) => (
        items =
          map(m.items, fun it ->
            if it.label == which
            then (label = it.label, price = it.price, stock = it.stock + n)
            else it),
        coins = m.coins
      )
    end;

  let run = fun (m: Model, actions: [Action]) ->
    fold_left(actions, fun (acc, a) -> update(acc, a), m);

  let selfcheck : () -> Bool = fun _ ->
    let done_ = run(init, [{sells_src}]) in
    done_.coins == {coins}
    && stock_of(done_, "{first}") == {stock[first]}
}} in
""")

def cipher_module(name, modulus, key, msg):
    enc = [ (r + key) % modulus for r in msg ]
    def checksum(rs):
        acc = 7
        for r in rs:
            acc = (acc * 31 + r) % 9973
        return acc
    msg_src = ", ".join(str(r) for r in msg)
    return (name, f"""module {name} = {{
  # Shift cipher variant: modulus {modulus}, key {key} #
  let modulus : Int = {modulus};

  let wrap = fun (n: Int) ->
    int_mod(int_mod(n, modulus) + modulus, modulus);

  let encode = fun (k: Int, rs: [Int]) ->
    map(rs, fun r -> wrap(r + k));

  let decode = fun (k: Int, rs: [Int]) ->
    map(rs, fun r -> wrap(r - k));

  let checksum = fun (rs: [Int]) ->
    fold_left(rs, fun (acc, r) -> int_mod(acc * 31 + r, 9973), 7);

  let selfcheck : () -> Bool = fun _ ->
    let msg = [{msg_src}] in
    let hidden = encode({key}, msg) in
    decode({key}, hidden) == msg
    && checksum(msg) == {checksum(msg)}
    && checksum(hidden) == {checksum(enc)}
}} in
""")

def craft_module(name, r1_cost, r1_gain, r2_cost, gathers, crafts1, crafts2):
    a, b = gathers
    wood, resin, one, two = a, b, 0, 0
    for _ in range(crafts1):
        if wood >= r1_cost[0] and resin >= r1_cost[1]:
            wood -= r1_cost[0]; resin -= r1_cost[1]; one += 1
    for _ in range(crafts2):
        if wood >= r2_cost:
            wood -= r2_cost; two += 1
    c1 = ", ".join(["CraftOne"] * crafts1)
    c2 = ", ".join(["CraftTwo"] * crafts2)
    return (name, f"""module {name} = {{
  # Crafting variant: recipe costs ({r1_cost[0]}, {r1_cost[1]}) and {r2_cost} #
  type Model = (wood = Int, resin = Int, ones = Int, twos = Int);
  type Action =
    + Gather(Int, Int)
    + CraftOne
    + CraftTwo;

  let init : Model = (wood = 0, resin = 0, ones = 0, twos = 0);

  let update = fun (m: Model, a: Action) ->
    case a
    | Gather(w, r) => (
        wood = m.wood + w, resin = m.resin + r,
        ones = m.ones, twos = m.twos
      )
    | CraftOne =>
        if m.wood >= {r1_cost[0]} && m.resin >= {r1_cost[1]}
        then (
          wood = m.wood - {r1_cost[0]}, resin = m.resin - {r1_cost[1]},
          ones = m.ones + 1, twos = m.twos
        )
        else m
    | CraftTwo =>
        if m.wood >= {r2_cost}
        then (
          wood = m.wood - {r2_cost}, resin = m.resin,
          ones = m.ones, twos = m.twos + 1
        )
        else m
    end;

  let run = fun (m: Model, actions: [Action]) ->
    fold_left(actions, fun (acc, a) -> update(acc, a), m);

  let selfcheck : () -> Bool = fun _ ->
    let m1 = run(init, [Gather({a}, {b}), {c1}, {c2}]) in
    m1.ones == {one} && m1.twos == {two}
    && m1.wood == {wood} && m1.resin == {resin}
}} in
""")

def tally_module(name, mul, cap, values):
    total, streak = 0, 0
    prev = None
    for v in values:
        if prev is not None and v == prev:
            streak = min(streak + mul, cap)
        else:
            streak = 0
        total += v + streak
        prev = v
    vals_src = ", ".join(f"Score({v})" for v in values)
    return (name, f"""module {name} = {{
  # Streak tally variant: bonus step {mul}, cap {cap} #
  type Model = (total = Int, streak = Int, last = Int, seen = Bool);
  type Action =
    + Score(Int)
    + Clear;

  let init : Model = (total = 0, streak = 0, last = 0, seen = false);

  let update = fun (m: Model, a: Action) ->
    case a
    | Score(v) =>
        let bump =
          if m.seen && v == m.last
          then (if m.streak + {mul} > {cap} then {cap} else m.streak + {mul})
          else 0 in
        (total = m.total + v + bump, streak = bump, last = v, seen = true)
    | Clear => init
    end;

  let run = fun (m: Model, actions: [Action]) ->
    fold_left(actions, fun (acc, a) -> update(acc, a), m);

  let selfcheck : () -> Bool = fun _ ->
    let m1 = run(init, [{vals_src}]) in
    m1.total == {total}
    && m1.streak == {streak}
    && run(m1, [Clear]) == init
}} in
""")

_MARKET_TABLES = [
    ("DriftMarket", [("net", 7, 5), ("hook", 2, 12), ("buoy", 9, 3)],
     [("net", 2), ("hook", 5), ("buoy", 4)]),
    ("EmberMarket", [("coal", 4, 20), ("bellows", 15, 2), ("tongs", 6, 6)],
     [("coal", 8), ("bellows", 1), ("tongs", 2)]),
    ("FableMarket", [("ink", 5, 9), ("quill", 3, 14), ("vellum", 11, 4)],
     [("quill", 6), ("vellum", 2), ("ink", 9), ("ink", 1)]),
    ("HarborMarket", [("salt", 2, 30), ("tar", 6, 8), ("sail", 25, 1)],
     [("salt", 12), ("sail", 1), ("sail", 1), ("tar", 3)]),
    ("MeadowMarket", [("honey", 8, 7), ("wax", 4, 11), ("comb", 13, 2)],
     [("honey", 3), ("wax", 11), ("comb", 2), ("wax", 1)]),
    ("QuarryMarket", [("slate", 5, 16), ("chisel", 9, 4), ("dust", 1, 40)],
     [("slate", 10), ("dust", 25), ("chisel", 4), ("chisel", 1)]),
    ("ReefMarket", [("pearl", 30, 2), ("kelp", 1, 50), ("shell", 3, 18)],
     [("kelp", 20), ("pearl", 1), ("shell", 9)]),
    ("SpireMarket", [("lens", 14, 3), ("brass", 6, 9), ("cord", 2, 22)],
     [("lens", 2), ("brass", 4), ("cord", 15), ("cord", 10)]),
    ("TundraMarket", [("fur", 10, 6), ("sinew", 4, 12), ("bone", 3, 15)],
     [("fur", 2), ("sinew", 6), ("bone", 15), ("bone", 1)]),
    ("ValeMarket", [("herb", 2, 28), ("root", 5, 10), ("bloom", 7, 5)],
     [("herb", 14), ("root", 3), ("bloom", 5), ("bloom", 1)]),
    ("WharfMarket", [("crate", 8, 7), ("winch", 22, 1), ("plank", 4, 13)],
     [("crate", 3), ("winch", 1), ("plank", 8)]),
    ("YarrowMarket", [("seed", 1, 60), ("sprout", 3, 20), ("stalk", 6, 8)],
     [("seed", 33), ("sprout", 12), ("stalk", 8), ("stalk", 2)]),
]

_CIPHER_TABLES = [
    ("AshCipher", 17, 4, [3, 15, 8, 0, 12, 16, 5]),
    ("BrineCipher", 23, 9, [22, 1, 17, 4, 4, 19]),
    ("CinderCipher", 31, 13, [7, 30, 2, 25, 11, 11, 0, 18]),
    ("DuneCipher", 41, 27, [40, 3, 33, 12, 7]),
    ("EchoCipher", 13, 5, [1, 12, 6, 6, 2, 9, 10]),
    ("FrostCipher", 37, 20, [36, 0, 19, 24, 8, 15]),
    ("GaleCipher", 19, 7, [18, 2, 11, 5, 13, 0, 9]),
    ("HollowCipher", 29, 15, [28, 14, 7, 21, 3]),
    ("IrisCipher", 43, 30, [42, 9, 35, 17, 26, 1]),
    ("JuniperCipher", 11, 3, [10, 4, 8, 2, 6, 0, 5, 9]),
    ("KestrelCipher", 47, 18, [46, 22, 5, 39, 12]),
    ("LarkCipher", 53, 41, [52, 30, 11, 44, 6, 25]),
]

_CRAFT_TABLES = [
    ("KilnWorks", (2, 1), 3, 4, (11, 6), 3, 2),
    ("LoomWorks", (1, 3), 2, 5, (9, 10), 3, 2),
    ("MillWorks", (4, 0), 6, 2, (14, 3), 2, 3),
    ("PressWorks", (2, 2), 4, 3, (12, 9), 4, 1),
    ("SmithWorks", (3, 1), 5, 6, (16, 5), 3, 1),
    ("WheelWorks", (1, 1), 2, 4, (8, 7), 5, 1),
    ("AnvilWorks", (5, 2), 8, 3, (21, 9), 3, 2),
    ("BellowsWorks", (2, 4), 3, 7, (13, 17), 3, 1),
    ("CartWorks", (6, 1), 9, 4, (25, 6), 3, 2),
    ("DyeWorks", (1, 5), 2, 2, (7, 21), 4, 2),
    ("ForgeWorks", (3, 3), 6, 5, (18, 14), 4, 1),
    ("GlassWorks", (2, 6), 5, 3, (11, 25), 3, 3),
]

_TALLY_TABLES = [
    ("CairnTally", 3, 12, [5, 5, 5, 2, 2, 9]),
    ("DellTally", 2, 6, [4, 4, 4, 4, 1]),
    ("FenTally", 5, 10, [7, 7, 3, 3, 3, 7]),
    ("GladeTally", 4, 8, [6, 1, 6, 6, 6]),
    ("HeathTally", 1, 4, [9, 9, 9, 9, 9, 2]),
    ("MireTally", 6, 18, [8, 8, 2, 8, 8, 8]),
    ("NookTally", 2, 10, [3, 3, 3, 3, 3, 3, 3]),
    ("OsierTally", 7, 14, [10, 10, 4, 4, 10]),
    ("PondTally", 3, 9, [5, 2, 5, 5, 5, 2, 2]),
    ("QuillTally", 4, 16, [6, 6, 6, 6, 1, 6]),
    ("RushTally", 5, 5, [9, 9, 9, 2, 9, 9]),
    ("SedgeTally", 8, 24, [7, 7, 7, 7, 7]),
]

def _derive(table, i, kind):
    """Rows beyond the hand-written tables: deterministic variants
    with distinct names and shifted constants (still computed here,
    so selfchecks stay provable)."""
    base = table[i % len(table)]
    gen = i // len(table)
    suffix = ["II", "III", "IV", "V"][gen - 1]
    if kind == "market":
        name, items, sells = base
        items = [(l, p + gen, s + gen) for l, p, s in items]
        return (name + suffix, items, sells)
    if kind == "cipher":
        name, modulus, key, msg = base
        return (name + suffix, modulus + 2 * gen,
                key + gen, [m + gen for m in msg])
    if kind == "craft":
        name, r1, g1, r2, gathers, c1, c2 = base
        return (name + suffix, r1, g1, r2 + gen,
                (gathers[0] + 2 * gen, gathers[1] + gen), c1, c2)
    name, mul, cap, values = base
    return (name + suffix, mul + gen, cap + 2 * gen, values)

def _row(table, i, kind):
    return table[i] if i < len(table) else _derive(table, i, kind)

def family_modules(n_each):
    out = []
    for i in range(n_each):
        out.append(market_module(*_row(_MARKET_TABLES, i, "market")))
        out.append(cipher_module(*_row(_CIPHER_TABLES, i, "cipher")))
        out.append(craft_module(*_row(_CRAFT_TABLES, i, "craft")))
        out.append(tally_module(*_row(_TALLY_TABLES, i, "tally")))
    return out
