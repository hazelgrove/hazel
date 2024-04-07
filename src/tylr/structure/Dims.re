open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  height: int, // number of newlines
  width: int // number of characters in last line
};

let mk = (~height=0, width) => {height, width};
let zero = mk(0);

let indent = ({height, width}: t) => {
  height,
  // note: assumes indentation only added to non-empty last line
  width: (height > 0 && width > 0 ? (+)(2) : Fun.id)(width),
};

// associative, not commutative
let add = (l: t, r: t) => {
  height: l.height + r.height,
  width: (r.height == 0 ? (+)(l.width) : Fun.id)(r.width),
};
let sum = List.fold_left(add, zero);

/**
(
  1
)

(
  fun x ->
    x + y
)

 */

let of_tok = (tok: Token.t) =>
  switch (tok.mtrl) {
  | Space =>
    let lines = String.split_on_char('\n', tok.text);
    let last = ListUtil.last(lines);
    mk(~height=List.length(lines) - 1, String.length(last));
  | Grout => mk(1)
  | Tile(_) => mk(Token.length(tok))
  };

let rec of_cell = (c: Cell.t): t =>
  switch (Cell.get(c)) {
  | None => zero
  | Some(m) => of_meld(m)
  }
and of_meld = (m: Meld.t) =>
  m
  |> Meld.fold(of_cell, (dims, tok, cell) =>
       sum([
         dims,
         of_tok(tok),
         of_cell(cell) |> (Token.indent(tok) ? indent : Fun.id),
       ])
     );
