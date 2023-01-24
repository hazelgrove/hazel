open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  sel: Selection.t,
  rel: Relatives.t,
};

let mk = (~sel=Selection.empty, rel) => {sel, rel};

let init =
  mk({
    sib:
      Segment.(
        empty,
        of_chain(
          Chain.of_grout(Grout.mk(Mold.mk_operand(Some(Sort.root)))),
        ),
      ),
    anc: [],
  });

let unselect = (d: Dir.t, {sel, rel}: t) => {
  let (pre, suf) = rel.sib;
  let sib =
    switch (d) {
    | L => (pre, Segment.(concat([to_suffix(sel.seg), suf])))
    | R => (Segment.(concat([pre, to_prefix(sel.seg)])), suf)
    };
  mk(Relatives.assemble({...rel, sib}));
};

let zip = (z: t): Chain.Padded.t => {
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
  let (lexed, rel) = Relatives.relex(z.rel);
  let rel = Relatives.insert(lexed, rel);
  {rel, sel: Selection.empty};
};

let insert = (s: string, z: t): t => {
  let (lexed, rel) = Relatives.relex(~insert=s, z.rel);
  let rel = Relatives.insert(lexed, rel);
  {rel, sel: Selection.empty};
};

let perform = (a: Action.t, z: t): option(t) =>
  switch (a) {
  | Move(d) => move(d, z)
  | Select(d) => select(d, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
