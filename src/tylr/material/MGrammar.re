// materialized grammar
type t = Mtrl.Sorted.Map.t(Prec.Table.t(Mtrl.Regex.t));

let mtrlize_tiles = (g: Grammar.t): t =>
  Sort.Map.to_seq(g)
  |> Seq.map(((s, tbl)) => {
       let mtrlize_sym =
         Sym.map(Mtrl.tile, ((pad, s)) => (pad, Mtrl.tile(s)));
       let mtbl = Prec.Table.map(Regex.map(mtrlize_sym), tbl);
       (Mtrl.Tile(s), mtbl);
     })
  |> Mtrl.Sorted.Map.of_seq;

// intention here is, given grammar in operator form (no consecutive NTs),
// insert a space-mtrl NT between any consecutive NTs and before/following
// any Ts at the left/right end of each prec level.
// currently assumes input grammar is in alternating form (neither consecutive
// NTs nor Ts) and only inserts space NTs at the ends of prec levels as needed.
let mtrlize_space = (g: t) =>
  g
  |> Mtrl.Sorted.Map.map(
       Prec.Table.map(rgx => {
         let (ls, rs) = RZipper.(enter(~from=L, rgx), enter(~from=R, rgx));
         let exists_t =
           List.exists(
             fun
             | Bound.Root => false
             | Node((sym, _)) => Sym.is_t(sym),
           );
         let spc = Regex.atom(Space.Sym.nt);

         rgx
         |> (exists_t(ls) ? Regex.push(~from=L, spc) : Fun.id)
         |> (exists_t(rs) ? Regex.push(~from=R, spc) : Fun.id);
       }),
     )
  |> Mtrl.Sorted.Map.add(Space, Prec.Table.singleton(Space.Regex.v));

let mtrlize = (g: Grammar.t): t =>
  // grout not explicitly materialized in grammar form,
  // instead generated as needed when baking walks
  g |> mtrlize_tiles |> mtrlize_space;

let v = mtrlize(Grammar.v);
