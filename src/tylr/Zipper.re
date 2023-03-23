open Sexplib.Std;
open Util;

// todo: document potential same-id token on either side of caret
// l|et x = 1 in x + 1
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  foc: Dir.t,
  sel: Ziggurat.t,
  // todo: rename as ctx
  rel: Stepwell.t,
};

let mk = (~foc=Dir.L, ~sel=Ziggurat.empty, rel) => {foc, sel, rel};

let init =
  mk(
    Stepwell.of_slopes(
      Slopes.mk(
        ~r=
          Slope.Up.of_meld(
            Meld.of_grout(Grout.mk(Mold.mk_operand(Some(Sort.root)))),
          ),
        (),
      ),
    ),
  );

// let unselect = (d: Dir.t, {sel, rel}: t) =>
//   rel |> Stepwell.cons_seg(~onto=Dir.toggle(d), sel.seg) |> mk;
let unselect = (d: Dir.t, {foc: _, sel, rel}: t) =>
  rel |> Stepwell.cons_zigg(~onto=Dir.toggle(d), sel) |> mk;

let zip = (~d=Dir.L, z: t): Meld.t => {
  let z = unselect(d, z);
  Stepwell.zip(z.rel);
};

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Move(Dir.t)
    | Select(Dir.t)
    | Delete(Dir.t)
    | Insert(string);
};

let move = (d: Dir.t, z: t): option(t) =>
  if (!Ziggurat.is_empty(z.sel)) {
    Some(unselect(d, z));
  } else {
    Stepwell.shift_char(~from=d, z.rel)
    |> Option.map(rel => {...z, rel: Stepwell.assemble(rel)});
  };
let rec move_n = (n: int, z: t): option(t) =>
  switch (n) {
  | _ when n < 0 => Option.bind(move(L, z), move_n(n + 1))
  | _ when n > 0 => Option.bind(move(R, z), move_n(n - 1))
  | _zero => Some(z)
  };

// todo: cleanup
let push_sel = (lx: Lexeme.t, foc: Dir.t, sel) =>
  switch (foc) {
  | L => Result.unwrap(Ziggurat.push_lexeme(lx, sel))
  | R => Result.unwrap(Ziggurat.hsup_lexeme(sel, lx))
  };
let pull_sel = (~char=false, foc: Dir.t, sel) =>
  switch (foc) {
  | L => Ziggurat.pull_lexeme(~char, sel)
  | R => Ziggurat.llup_lexeme(~char, sel) |> Option.map(((a, b)) => (b, a))
  };

let select = (d: Dir.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let b = Dir.toggle(d);
  if (d == z.foc || Ziggurat.is_empty(z.sel)) {
    let+ (c, rel) = Stepwell.uncons_char(~from=d, z.rel);
    // let bs = Stepwell.bounds(rel);
    let sel = push_sel(c, d, z.sel);
    mk(~foc=d, ~sel, rel);
  } else {
    // checked for selection empty above
    let (c, sel) = Option.get(pull_sel(~char=true, z.foc, z.sel));
    let rel =
      z.rel |> Stepwell.cons_lexeme(~onto=b, c) |> Stepwell.assemble(~sel);
    return(mk(~sel, rel));
  };
};

let delete = (d: Dir.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let+ z = Ziggurat.is_empty(z.sel) ? select(d, z) : return(z);
  let (lexed, rel) = z.rel |> Stepwell.assemble |> Stepwell.relex;
  // selection dropped
  mk(Stepwell.insert(lexed, rel));
};

let insert = (s: string, z: t): t => {
  print_endline("Zipper.insert");
  let rel = Ziggurat.is_empty(z.sel) ? z.rel : Stepwell.assemble(z.rel);
  let (lexed, rel) = Stepwell.relex(~insert=s, rel);
  // selection (if any) dropped
  mk(Stepwell.insert(lexed, rel));
};

let perform = (a: Action.t, z: t): option(t) =>
  switch (a) {
  | Move(d) => move(d, z)
  | Select(d) => select(d, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
