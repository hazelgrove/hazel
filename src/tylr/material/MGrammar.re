type t = Mtrl.Sort.Map.t(Prec.Table.t(Regex.t(Mtrl.Sym.t)));
// type t = PBNF.t(MSym.t);

let mtrlize_tiles = (g: Grammar.t): t =>
  Sort.Map.to_seq(g)
  |> Seq.map(((s, tbl)) => {
       let mtbl =
         tbl
         |> Prec.Table.map(
              Regex.map(
                fun
                | Sym.T(lbl) => Sym.T(Mtrl.Tile(lbl))
                | NT(s) => NT(Mtrl.Tile(s)),
              ),
            );
       (Mtrl.Tile(s), mtbl);
     })
  |> Mtrl.Sort.Map.of_seq;

let mtrlize_space: t => t =
  Mtrl.Sort.Map.map(
    Prec.Table.map(rgx => {
      let (ls, rs) = RZipper.(enter(~from=L, rgx), enter(~from=R, rgx));
      let exists_t =
        List.exists(
          fun
          | Bound.Root => false
          | Node((sym, _)) => Sym.is_t(sym),
        );
      let spc = Regex.atom(Sym.NT(Mtrl.Space));
      rgx
      |> (exists_t(ls) ? Regex.push(~from=L, spc) : Fun.id)
      |> (exists_t(rs) ? Regex.push(~from=R, spc) : Fun.id);
    }),
  );

// G ::= [Space] <> [Space]
//       ...
//       ([Space] <<)? _ (>< _)* (>> [Space])?

let mtrlize_grout: t => t =
  Mtrl.Sort.Map.map(tbl => {
    open Regex;
    let g_t = atom(Sym.T(Mtrl.Grout));
    let g_nt = atom(Sym.NT(Mtrl.Grout));
    // ideally would let space pass take care of this
    // but gets a lil more complicated with varying end delimiters
    let spc = atom(Sym.NT(Mtrl.Space));
    // todo: add space
    [(None, g_t)]
    @ tbl
    @ [
      (
        None,
        seq([
          opt(seq([spc, g_t])),
          g_nt,
          star(seq([g_t, g_nt])),
          opt(seq([spc, g_t])),
        ]),
      ),
    ];
  });

let mtrlize = (g: Grammar.t): t =>
  g
  |> mtrlize_tiles
  // need to do space before grout
  // bc comment in mtrlize_grout
  |> mtrlize_space
  |> mtrlize_grout;

let v = mtrlize(Grammar.v);
