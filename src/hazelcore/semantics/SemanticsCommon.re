open GeneralUtil;

exception FreeVarInPat;

[@deriving sexp]
type inj_side =
  | L
  | R;

let pick_side = (side, l, r) =>
  switch (side) {
  | L => l
  | R => r
  };

[@deriving sexp]
type delim_index = int;
[@deriving sexp]
type op_index = int;
[@deriving sexp]
type char_index = int;
[@deriving sexp]
type child_index = int;

[@deriving sexp]
type side =
  | Before
  | After;

let toggle_side =
  fun
  | Before => After
  | After => Before;
