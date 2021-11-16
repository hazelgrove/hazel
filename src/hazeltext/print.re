open Pretty;
module UHDoc_Exp = UHDoc_Exp.Make(Memo.DummyMemo);

exception NoLayout;

let string_of_layout = (l: Layout.t('a)): string => {
  let rec go = (indent: int, col: int, l: Layout.t('a)): (string, int) =>
    switch (l) {
    | Text(s) => (s, col + String.length(s))
    | Cat(l1, l2) =>
      let (s1, c1) = go(indent, col, l1);
      let (s2, c2) = go(indent, c1, l2);
      (s1 ++ s2, c2);
    | Linebreak => ("\n" ++ String.make(indent, ' '), indent)
    | Align(l) => go(col, col, l)
    | Annot(UHAnnot.HoleLabel(_), _) => ("_?", col + 2)
    | Annot(UHAnnot.ExternalLineBreak, d) =>
      let (s, _) = go(indent, col, d);
      (s ++ ";" ++ String.make(indent, ' '), indent);
    | Annot(_, l) => go(indent, col, l)
    };
  let (s, _) = go(0, 0, l);
  s;
};

let print_exp = (exp: UHExp.t): string => {
  let doc =
    Lazy.force(UHDoc_Exp.mk, ~memoize=true, ~enforce_inline=false, exp);
  let layout = Pretty.LayoutOfDoc.layout_of_doc(~width=100, ~pos=0, doc);
  switch (layout) {
  | Some(layout) => string_of_layout(layout)
  | None => raise(NoLayout)
  };
};
