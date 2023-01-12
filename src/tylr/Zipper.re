type t = {
  sel: Selection.t,
  rel: Relatives.t,
};

module Action = {
  type t =
    | Move(Dir.t)
    | Select(Dir.t)
    | Delete(Dir.t)
    | Insert(string);
};

let move = (d: Dir.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let b = Dir.toggle(d);
  if (!Selection.is_empty(z.sel)) {
    let rel = Relatives.push_seg(b, z.sel.seg, z.rel);
    return({rel, sel: Selection.empty});
  } else {
    let+ rel = Relatives.shift_char(d, z.rel);
    {...z, rel};
  };
};

let select = (d: Dir.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let b = Dir.toggle(d);
  if (d == z.sel.foc || Selection.is_empty(z.sel)) {
    let+ (c, rel) = Relatives.pop_char(d, z.rel);
    let sel = Selection.push_char(c, {...sel, foc: d});
    {rel, sel};
  } else {
    // checked for selection empty above
    let (c, sel) = Option.get(Selection.pop_char(z.sel));
    let rel = Relatives.push_char(b, c, z.rel);
    {rel, sel};
  };
};

let delete = (d: Dir.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let+ z = Selection.is_empty(z.sel) ? select(d, z) : return(z);
  let (_, rel) = Relatives.remold(z.rel);
  {rel, sel: Selection.empty};
};

let insert = (s: string, z: t): t => {
  let (lexed, (_, n), rel) = Relatives.lex(s, rel);
  let unmolded =
    lexed |> Aba.map_b(t => Chain.of_tile(Tile.unmolded(t)));
  let rel =
    rel
    |> Relatives.insert(unmolded)
    // restore chars popped off of R-side of relatives when lexing
    |> FunUtil.(repeat(n, force_opt(Relatives.shift_char(R))));
  {rel, sel: Selection.empty};
};

let perform = (a: Action.t, z: t): option(t) =>
  switch (a) {
  | Move(d) => move(d, z)
  | Select(d) => select(d, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
