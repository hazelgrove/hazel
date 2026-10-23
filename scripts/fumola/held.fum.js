// the held versions' setup, for a world of cells (Fumola source as a JS template): the world bound once, as a list and as a map
export const heldSetup = cells => `let wl = ${cells};
func world_held_list(ac, ar, cols, rows, d) {
  switch (step(ac, ar, d, cols, rows)) {
    case (?(c, r)) { switch (find_at(wl, 0, wl.size(), c, r)) { case (?x) { decide(x, opp(d), c, r) }; case null { #Done } } };
    case null { #Done }
  }
};
var wm0 = HashMap.new();
for (x in wl.vals()) { let (m2, _) = HashMap.put(wm0, (x.col, x.row), x); wm0 := m2 };
let wm = wm0;
func world_held_map(ac, ar, cols, rows, d) {
  switch (step(ac, ar, d, cols, rows)) {
    case (?(c, r)) { switch (HashMap.get(wm, (c, r))) { case (?x) { decide(x, opp(d), c, r) }; case null { #Done } } };
    case null { #Done }
  }
}`;
