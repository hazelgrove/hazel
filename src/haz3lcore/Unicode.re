let lam = "λ";
let up_arrow = "↑";
let down_arrow = "↓";
let left_arrow = "←";
let right_arrow = "→";
let nbsp = "\xC2\xA0";
let zwsp = "​";

let typeArrowSym = "→"; // U+2192 "Rightwards Arrow"
let castArrowSym = "⇨";
let castBackArrowSym = "⇦";

let ellipsis = "\xE2\x80\xA6";

// copied from hazel
// NOTE: 30% faster than Camomile
let length = (s: string): int => {
  Util.(StringUtil.length(s));
};
