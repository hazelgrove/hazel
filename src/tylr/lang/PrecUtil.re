let end_tiles = (side: Dir.t, s: Sort.t): list(Proto.t) =>
  Grammar.v
  |> List.assoc(s)
  |> Precex.end_toks(side)
  |> List.mapi((i, (zs, _)) => (i, zs))
  |> List.concat_map(((i, zs)) =>
    zs
    |> List.map(((label, uz)) => {
      let mold = Mold.{sort: s, prec: i, unzipped: uz};
      Proto.{mold, label};
    })
  );

type cmp =
| Lt
| Eq
| Gt;

type adj = Proto.Map.t(Proto.Map.t(cmp));

let mk_