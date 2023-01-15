open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  sel: Selection.t,
  rel: Relatives.t,
};

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
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
    let rel = Relatives.push_seg(~onto=b, z.sel.seg, z.rel);
    return({rel, sel: Selection.empty});
  } else {
    let+ rel = Relatives.shift_char(~from=d, z.rel);
    {...z, rel};
  };
};

let select = (d: Dir.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let b = Dir.toggle(d);
  if (d == z.sel.foc || Selection.is_empty(z.sel)) {
    let+ (c, rel) = Relatives.pop_char(~from=d, z.rel);
    let sel = Selection.push_char(c, {...z.sel, foc: d});
    {rel, sel};
  } else {
    // checked for selection empty above
    let (c, sel) = Option.get(Selection.pop_char(z.sel));
    let rel = Relatives.push_char(~onto=b, c, z.rel);
    return({rel, sel});
  };
};

let delete = (d: Dir.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let+ z = Selection.is_empty(z.sel) ? select(d, z) : return(z);
  let rel = Relatives.remold_suffix(z.rel);
  {rel, sel: Selection.empty};
};

let insert = (s: string, z: t): t => {
  let (lexed, rel) = Relatives.lex(s, z.rel);
  let unmolded = Segment.of_lexemes(lexed);
  let rel =
    rel
    |> Relatives.insert(unmolded)
    // restore chars popped off of R-side of relatives when lexing
    // |> FunUtil.(repeat(n, force_opt(Relatives.shift_char(~from=R))));
    |> Relatives.unzip;
  {rel, sel: Selection.empty};
};

let perform = (a: Action.t, z: t): option(t) =>
  switch (a) {
  | Move(d) => move(d, z)
  | Select(d) => select(d, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
