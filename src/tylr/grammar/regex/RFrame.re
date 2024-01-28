[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('a) =
  | Star_
  | Seq_(Regex.s('a), Regex.s('a))
  | Alt_(Regex.s('a), Regex.s('a));

let zip = (f: t(_), r: Regex.t(_)) =>
  switch (f) {
  | Star_ => Regex.Star(r)
  | Alt_(ls, rs) => Alt(List.rev(ls) @ [r, ...rs])
  | Seq_(ls, rs) => Seq(List.rev(ls) @ [r, ...rs])
  };

let nullable = (side: Dir.t, f: t(_)) =>
  switch (side, f) {
  | (_, Star_ | Alt_(_)) => true
  | (L, Seq_(gs_l, _)) => Regex.nullable(Seq(gs_l))
  | (R, Seq_(_, gs_r)) => Regex.nullable(Seq(gs_r))
  };
