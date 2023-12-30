// module Atom = {
//   type t =
//     | T(EMold.t)
//     | NT(ESort.t);

//   let elab = (a: GAtom.t): t =>
//     switch (GAtom.either(a)) {
//     | Left(m) => T(Tile(m))
//     | Right(s) => NT(Tile(s))
//     };
// };

// type t = Sort.Map.t(Prec.Table.t(Regex.t(Atom.t)));
type t = PBNF.t(Sym.t(EMtrl.t(Label.t), EMtrl.t(Sort.t)));

let elab_tiles = (g: Grammar.t): t =>
  Sort.Map.to_list(g)
  |> List.map(((sort, tbl)) =>
    tbl
    |> Prec.Table.mapi((prec, rgx) =>
      rgx
      |> RZipper.map(zipper => Atom.elab(GAtom.{sort, prec, zipper}))
    )
  )
  |> Sort.Map.of_list;

let elab_grout = (g: t): t =>
  Sort.Map.to_list(g)
  |> List.map(((sort, tbl)) => {
    let r_t = Regex.atom(Atom.T(Grout(sort)));
    let r_nt = Regex.atom(Atom.NT(Grout(sort)));
    [(None, r_t)]
    @ tbl
    @ [(None,
      Regex.(seq([
        opt(r_t),
        r_nt,
        star(seq([
          r_t, r_
        ])),
        opt(atom(Atom.T(Grout(sort))))
      ]))
    )]
  })