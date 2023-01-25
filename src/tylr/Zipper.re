open Sexplib.Std;
open Util;

// todo: document potential same-id token on either side of caret
// l|et x = 1 in x + 1
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  sel: Selection.t,
  rel: Relatives.t,
};

let mk = (~sel=Selection.empty, rel) => {sel, rel};

let init =
  mk(
    Relatives.of_sib(
      Segment.(
        empty,
        of_meld(
          Meld.of_grout(Grout.mk(Mold.mk_operand(Some(Sort.root)))),
        ),
      ),
    ),
  );

let unselect = (d: Dir.t, {sel, rel}: t) =>
  rel
  |> Relatives.map_sib(((pre, suf)) =>
       switch (d) {
       | L => (pre, Segment.(concat([to_suffix(sel.seg), suf])))
       | R => (Segment.(concat([pre, to_prefix(sel.seg)])), suf)
       }
     )
  |> Relatives.assemble
  |> mk;

let zip = (z: t): Meld.Padded.t => {
  let z = unselect(L, z);
  Relatives.zip(z.rel);
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
  OptUtil.Syntax.(
    if (!Selection.is_empty(z.sel)) {
      return(unselect(d, z));
    } else {
      let+ rel = Relatives.shift_char(~from=d, z.rel);
      {...z, rel};
    }
  );
};

let select = (d: Dir.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let b = Dir.toggle(d);
  if (d == z.sel.foc || Selection.is_empty(z.sel)) {
    let+ (c, rel) = Relatives.uncons_char(~from=d, z.rel);
    let sel = Selection.push_char(c, {...z.sel, foc: d});
    mk(~sel, rel);
  } else {
    // checked for selection empty above
    let (c, sel) = Option.get(Selection.pop_char(z.sel));
    let rel = Relatives.cons_lexeme(~onto=b, c, z.rel);
    return(mk(~sel, rel));
  };
};

let delete = (d: Dir.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let+ z = Selection.is_empty(z.sel) ? select(d, z) : return(z);
  let (lexed, rel) = Relatives.relex(z.rel);
  mk(Relatives.insert(lexed, rel));
};

let insert = (s: string, z: t): t => {
  let (lexed, rel) = Relatives.relex(~insert=s, z.rel);
  mk(Relatives.insert(lexed, rel));
};

let perform = (a: Action.t, z: t): option(t) =>
  switch (a) {
  | Move(d) => move(d, z)
  | Select(d) => select(d, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
