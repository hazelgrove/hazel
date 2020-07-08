[@deriving sexp]
type t = Pretty.Layout.t(UHAnnot.t);
type with_splices = (t, SpliceMap.t(t));

type pos = {
  indent: int,
  row: int,
  col: int,
};

let fold =
    (
      ~linebreak: 'a,
      ~text: string => 'a,
      ~align: 'a => 'a,
      ~cat: ('a, 'a) => 'a,
      ~annot: (pos, UHAnnot.t, 'a) => 'a,
      l: t,
    ) => {
  let row = ref(0);
  let col = ref(0);
  let rec go = (~indent, l: t) => {
    let go' = go(~indent);
    let pos = {indent, row: row^, col: col^};
    switch (l) {
    | Linebreak =>
      row := row^ + 1;
      col := indent;
      linebreak;
    | Text(s) =>
      col := col^ + StringUtil.utf8_length(s);
      text(s);
    | Align(l) => align(go(~indent=col^, l))
    | Cat(l1, l2) =>
      let a1 = go'(l1);
      let a2 = go'(l2);
      cat(a1, a2);
    | Annot(ann, l) => annot(pos, ann, go'(l))
    };
  };
  go(~indent=0, l);
};
