open Pretty;

type t = Layout.t(UHAnnot.t);

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
let pos_fold:
  (
    ~col_start: int=?,
    ~linebreak: pos => 'a,
    ~text: (pos, string) => 'a,
    ~align: (pos, 'a) => 'a,
    ~cat: (pos, 'a, 'a) => 'a,
    ~annot: (pos, UHAnnot.t, 'a) => 'a,
    t
  ) =>
  'a;

let find_and_decorate_caret: (~path: CursorPath.t, t) => option(t);

// TODO rename to current term
let find_and_decorate_cursor: (~steps: CursorPath.steps, t) => option(t);

let find_and_decorate_var_use: (~steps: CursorPath.steps, t) => option(t);
