/* A simple IDVariable generator */
[@deriving sexp]
type t = (MetaVar.t, AssertNumber.t);
let init: t = (0, 0);

let reset_metavar: t => t =
  x => {
    switch (x) {
    | (_, b) => (0, b)
    };
  };
let next_hole: t => (MetaVar.t, t) = ((u, an)) => (u + 1, (u + 1, an));

let next_assert: t => (AssertNumber.t, t) =
  ((u, an)) => (an + 1, (u, an + 1));
