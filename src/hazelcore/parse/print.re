open Pretty;

let string_of_layout = (l: Layout.t('a)): string => {
  let rec go = (indent: int, col: int, l: Layout.t('a)): (string, int) =>
    switch (l) {
    | Text(s) => (s, col + String.length(s))
    | Cat(l1, l2) =>
      let (s1, c1) = go(indent, col, l1);
      let (s2, c2) = go(indent, c1, l2);
      (s1 ++ s2, c2);
    | Linebreak => ("\n" ++ String.make(indent, ' '), indent)
    | ExternalLinebreak => (";\n" ++ String.make(indent, ' '), indent)
    | Align(l) => go(col, col, l)
    | Annot(_, l) => go(indent, col, l)
    };
  let (s, _) = go(0, 0, l);
  s;
};
