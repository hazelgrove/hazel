[@deriving (sexp, eq, ord)]
type t =
  | NecessarilyComplete
  | NecessarilyIncomplete
  | IndeterminatelyIncomplete;

let join = (cc, cc') =>
  switch (cc, cc') {
  | (IndeterminatelyIncomplete, _)
  | (_, IndeterminatelyIncomplete) => IndeterminatelyIncomplete
  | (NecessarilyIncomplete, _)
  | (_, NecessarilyIncomplete) => NecessarilyIncomplete
  | (NecessarilyComplete, NecessarilyComplete) => NecessarilyComplete
  };

let join_fold =
  List.fold_left((acc, cc) => join(acc, cc), NecessarilyComplete);
