open Sexplib.Std;
open Util;

module Height = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    head: int, // count of newlines up through leading token of meld
    body: int // count of all other newlines
  };
  let mk = (~head=0, body) => {head, body};
  let zero = mk(0);
  let total = h => h.head + h.body;
  let add = (l, r) => {head: l.head, body: l.body + total(r)};
};
module Width = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    body: int, // count of last-line chars up to trailing whitespace
    foot: int // count of last-line chars in trailing whitespace
  };
  let mk = (~foot=0, body) => {body, foot};
  let zero = mk(0);
  let total = w => w.body + w.foot;
  let add = (l, r) => {body: total(l) + r.body, foot: r.foot};
  let indent = (w: t) => {...w, body: w.body + (w.body > 0 ? 2 : 0)};
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  height: Height.t,
  width: Width.t,
};

let mk = (~height=Height.zero, width) => {height, width};
let zero = mk(Width.zero);
// associative, not commutative
let add = (l: t, r: t) => {
  height: Height.add(l.height, r.height),
  width:
    Width.add(Height.total(r.height) == 0 ? l.width : Width.zero, r.width),
};
let sum = List.fold_left(add, zero);

let indent = (dims: t) => {
  ...dims,
  width: (dims.height.head > 0 ? Width.indent : Fun.id)(dims.width),
};

let of_space = (spc: string) => {
  let lines = String.split_on_char('\n', spc);
  let last = ListUtil.last(lines);
  let height = Height.{head: List.length(lines) - 1, body: 0};
  let width = Width.{body: 0, foot: Utf8.length(last)};
  {height, width};
};

let of_tok = (tok: Token.t) =>
  switch (tok.mtrl) {
  | Space => of_space(tok.text)
  | Grout => mk(Width.mk(1))
  | Tile(_) => mk(Width.mk(Token.length(tok)))
  };

let rec of_cell = (c: Cell.t): t =>
  switch (Cell.get(c)) {
  | None => zero
  | Some(m) => of_meld(m)
  }
and of_meld = (m: Meld.t) =>
  Meld.to_chain(m)
  |> Chain.fold_left(of_cell, (dims, tok, cell) =>
       sum([
         dims,
         of_tok(tok),
         of_cell(cell) |> (Token.indent(tok) ? indent : Fun.id),
       ])
     );
