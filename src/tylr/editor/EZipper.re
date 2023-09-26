open Sexplib.Std;
open Util;

module Focus = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Pointing
    | Selecting(Dir.t, EZiggurat.t);
};

// todo: document potential same-id token on either side of caret
// l|et x = 1 in x + 1
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  foc: Focus.t,
  ctx: EStepwell.t,
};

let mk = (~foc=Focus.Pointing, ctx) => {foc, ctx};

let unselect = (~toward=?, z: t) =>
  switch (z.foc) {
  | Pointing => z
  | Selecting(d, sel) =>
    let onto = Dir.toggle(Option.value(toward, ~default=d));
    mk(Melder.Stepwell.push_zigg(~onto, sel, z.ctx));
  };

module Path = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // top-down
    slots: list(int),
    piece: int,
    offset: int,
  };
  exception Invalid;
};

module Zipped = {
  type t = (Path.t, Slot.p);
};

let rec unzip = (~ctx=empty, (p, slot): Zipped.t): t =>
  switch (slot) {
  | None => mk(ctx)
  | Some(m) =>
    let get = OptUtil.get_or_raise(Path.Invalid);
    switch (p.slots) {
    | [hd, ...slots] =>
      let (slot, bridge) = get(Meld.unzip_slot(hd, m));
      let ctx = link(bridge, ctx);
      unzip(~ctx, ({...p, slots}, slot));
    | [] =>
      let (pre, p, suf) = get(Meld.unzip_piece(p.piece, m));
      switch (Piece.unzip(p.offset, p)) {
      | Ok((l, r)) =>
        let l = Terrace.L.of_prefix(pre, l);
        let r = Terrace.R.of_suffix(r, suf);
        cons_slopes(([l], [r]), well);
      | Error(side) =>
        unzip_slot(side == L ? p.piece : p.piece + 1, m =>
          mk(cons_slopes(Slopes.of_meld(side, m), well))
        )
      };
    };
  };

let zip_up = (_, _) => failwith("todo");
let zip_dn = (_, _) => failwith("todo");
let rec zip_slopes = ((dn, up): Slopes.t, z: Zipped.t) =>
  switch (dn, up) {
  | ([], []) => z
  | ([], [_, ..._] as up) => Slope.Up.zip(z, up)
  | ([_, ..._] as dn, []) => Slope.Dn.zip(dn, z)
  | ([hd_dn, ...tl_dn], [hd_up, ...tl_up]) =>
    let (p, slot) = z;
    let check = b => b ? () : raise(Invalid_argument("Slopes.zip"));
    // expecting no error-correction to take place
    switch (Melder.Wald.cmp(hd_dn.wal, ~slot, hd_up.wal)) {
    | {up: [], dn: [], top} =>
      // eq
      let p = Path.cons(Wald.length(hd_dn.wal), p);
      let slot = Slot.full((hd_dn.mel, top, hd_up.mel));
      zip_slopes((tl_dn, tl_up), (p, slot));
    | {up: [], dn: [hd, ...tl], _} =>
      // lt
      check(tl == []);
      let p = Path.cons(0, p);
      let slot = Slot.full((hd.mel, hd.wal, hd_up.mel));
      zip_slopes((dn, tl_up), slot);
    | {up: [hd, ...tl], _} =>
      // gt
      check(tl == []);
      let p = Path.cons(Wald.length(hd_dn.wal), p);
      let slot = Slot.full((hd_dn.mel, hd.wal, hd.mel));
      zip_slopes((tl_dn, up), slot);
    };
  };
let zip = (z: t): Zipped.t =>
  unselect(z).ctx |> Stepwell.break_bridges |> zip_slopes;

// todo: cleanup
// let push_sel = (lx: Lexeme.t, foc: Dir.t, sel) =>
//   switch (foc) {
//   | L => Result.unwrap(Ziggurat.push_lexeme(lx, sel))
//   | R => Result.unwrap(Ziggurat.hsup_lexeme(sel, lx))
//   };
// let pull_sel = (~char=false, foc: Dir.t, sel) =>
//   switch (foc) {
//   | L => Ziggurat.pull_lexeme(~char, sel)
//   | R => Ziggurat.llup_lexeme(~char, sel) |> Option.map(((a, b)) => (b, a))
//   };
