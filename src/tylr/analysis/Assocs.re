open Util;

// todo: move elsewhere
let get = (s, p): Assoc.t => {
  open OptUtil.Syntax;
  let* s = s;
  let* (_, a) = p < 0 ? None : List.nth_opt(List.assoc(s, v), p);
  a;
};