[@deriving sexp]
type t = Pretty.Layout.t(UHAnnot.t);
type with_splices = (t, SpliceMap.t(t));

type pos = {
  indent: int,
  row: int,
  col: int,
};

let fold:
  (
    ~linebreak: 'a,
    ~text: string => 'a,
    ~align: 'a => 'a,
    ~cat: ('a, 'a) => 'a,
    ~annot: (pos, UHAnnot.t, 'a) => 'a,
    t
  ) =>
  'a;
