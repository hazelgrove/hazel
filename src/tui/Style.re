/* Terminal cell styling: a tiny subset of SGR attributes, kept as plain
   data so frames are renderer-independent (a future native notty backend
   can interpret the same values). */

[@deriving show({with_path: false})]
type color =
  | Default
  | Ansi256(int);

[@deriving show({with_path: false})]
type t = {
  fg: color,
  bg: color,
  bold: bool,
  dim: bool,
  reverse: bool,
  /* curly underline in the given 256-color; terminals without undercurl
     support degrade to a plain underline (4:3 parses as 4 there) */
  undercurl: option(int),
};

let default: t = {
  fg: Default,
  bg: Default,
  bold: false,
  dim: false,
  reverse: false,
  undercurl: None,
};

let fg = (n: int): t => {
  ...default,
  fg: Ansi256(n),
};
let bold = (s: t): t => {
  ...s,
  bold: true,
};
let dim = (s: t): t => {
  ...s,
  dim: true,
};
let reverse = (s: t): t => {
  ...s,
  reverse: true,
};
let undercurl = (n: int, s: t): t => {
  ...s,
  undercurl: Some(n),
};

/* Full SGR sequence for this style. Leads with a reset so each span is
   self-contained; spans are coarse enough that this costs nothing. */
let sgr = (s: t): string => {
  let attrs =
    ["0"]
    @ (s.bold ? ["1"] : [])
    @ (s.dim ? ["2"] : [])
    @ (s.reverse ? ["7"] : [])
    @ (
      switch (s.fg) {
      | Default => []
      | Ansi256(n) => ["38", "5", string_of_int(n)]
      }
    )
    @ (
      switch (s.bg) {
      | Default => []
      | Ansi256(n) => ["48", "5", string_of_int(n)]
      }
    )
    @ (
      switch (s.undercurl) {
      | None => []
      /* colon sub-parameter syntax: 4:3 = curly underline,
         58:5:n = underline color */
      | Some(n) => ["4:3", "58:5:" ++ string_of_int(n)]
      }
    );
  "\x1b[" ++ String.concat(";", attrs) ++ "m";
};
