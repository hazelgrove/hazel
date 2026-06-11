/* Token class -> terminal style. The class strings are the same ones
   Code.re (web) computes and editor.css colors; the 256-color values
   approximate the oklch hues in src/web/www/style/variables.css. */

let exp = 246; /* --STONE: low-chroma gray-blue */
let pat = 75; /* --PAT: blue, hue ~225 */
let typ = 135; /* --TYP: purple, hue ~300 */
let yellow = 178; /* --Y3: string literals, incomplete shards */
let hole_yellow = 220; /* --Y2: explicit holes */
let green = 108; /* --G2: comments */
let red = 167; /* --R2: error text */

let of_base_cls = (cls: string): Style.t =>
  switch (cls) {
  | "Exp"
  | "Rul" => Style.default /* terminal default fg reads best for code text */
  | "Pat"
  | "TPat"
  | "MPat" => Style.fg(pat)
  | "Typ"
  | "Sig" => Style.fg(typ)
  | "Mod"
  | "Drv" => Style.fg(exp)
  | "string-lit"
  | "incomplete" => Style.fg(yellow)
  | "explicit-hole"
  | "llm-waiting" => Style.bold(Style.fg(hole_yellow))
  | "sort-inconsistent" /* web marks these with a zigzag; color instead */
  | "Any" => Style.fg(red)
  | _ => Style.default
  };

let comment = Style.fg(green);
let error_underline = (s: Style.t) => Style.undercurl(red, s);
/* test result tints: dark backgrounds keep token colors readable */
let test_pass = (s: Style.t): Style.t => {
  ...s,
  bg: Ansi256(22),
}; /* dark green */
let test_fail = (s: Style.t): Style.t => {
  ...s,
  bg: Ansi256(52),
}; /* dark red */
let test_indet = (s: Style.t): Style.t => {
  ...s,
  bg: Ansi256(58),
}; /* dark olive */
let warning_underline = (s: Style.t) => Style.undercurl(yellow, s);
let grout = Style.dim(Style.default);
let line_number = Style.dim(Style.default);
let status_bar = Style.reverse(Style.default);
let status_error = Style.reverse(Style.fg(red));
let result_ok = Style.default;
let result_err = Style.fg(red);
let pane_title = Style.dim(Style.default);
