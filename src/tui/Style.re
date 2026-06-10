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
};

let default: t = {
  fg: Default,
  bg: Default,
  bold: false,
  dim: false,
  reverse: false,
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
    );
  "\x1b[" ++ String.concat(";", attrs) ++ "m";
};
