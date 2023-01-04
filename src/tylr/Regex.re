type t =
  | Char(char)
  | Star(t)
  | Seq(list(t))
  | Alt(list(t));

module Frame = {
  type g = t;
  type t =
    | Star_
    | Seq(list(g), list(g))
    | Alt(list(g), list(g));
  type s = list(t);
};
