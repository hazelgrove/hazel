type t = PBNF.t(MSym.t);

let mtrlize_tiles: Grammar.t => t =
  Sort.Map.map(
    Prec.Table.map(
      Regex.map(
        fun
        | Sym.T(lbl) => T(Tile(lbl))
        | NT(s) => NT(Tile(s)),
      ),
    ),
  );

let mtrlize_space: t => t =
  Sort.Map.map(
    Prec.Table.map(rgx => {
      let (ls, rs) = RZipper.(enter(~from=L, rgx), enter(~from=R, rgx));
      let exists_t = List.exists(((sym, _)) => Sym.is_t(sym));
      let spc = Regex.atom(Sym.NT(Space));
      rgx
      |> (exists_t(ls) ? Regex.push(~from=L, spc) : Fun.id)
      |> (exists_t(rs) ? Regex.push(~from=R, spc) : Fun.id);
    }),
  );

let mtrlize_grout = (g: t): t =>
  Sort.Map.map(tbl => {
    open Regex;
    let g_t = atom(Sym.T(Grout));
    let g_nt = atom(Sym.NT(Grout));
    // ideally would let space pass take care of this
    // but gets a lil more complicated with varying end delimiters
    let spc = atom(Sym.NT(Space));
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
