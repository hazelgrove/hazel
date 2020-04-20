open Pretty;

[@deriving sexp]
type t = Layout.t(UHAnnot.t);
type with_splices = (t, SpliceMap.t(t));

let has_child: t => bool;

let has_inline_OpenChild: t => bool;

let has_para_OpenChild: t => bool;

type pos = {
  indent: int,
  row: int,
  col: int,
};

// TODO should be possible to make polymorphic over annot
// but was getting confusing type inference error
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

let find_and_decorate_caret:
  (~path: CursorPath.t, with_splices) => option(with_splices);

// TODO rename to current term
let find_and_decorate_cursor:
  (~steps: CursorPath.steps, with_splices) => option(with_splices);

let find_and_decorate_var_use:
  (~steps: CursorPath.steps, with_splices) => option(with_splices);
