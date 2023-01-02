type t = {
  sib: Siblings.t,
  anc: Ancestors.t,
};

let choose = (in_l: option(Sort.t), out: Sort.t, t: Token.t) => {
  let out_consistent =
    Lang.molds(t) |> List.filter(m => Sort.compare(m.sort, out) <= 0);
  switch (out_consistent) {
  | [] => None
  | [m] => Some(m)
  | [_, _, ..._] =>
    let in_l_consistent =
      out_consistent
      |> List.filter(m =>
           switch (in_l, Mold.expected_sort(L, m)) {
           | (None, Some(_))
           | (Some(_), None) => false
           | (None, None) => true
           | (Some(actual), Some(expected)) =>
             Sort.compare(actual, expected) <= 0
           }
         );
    switch (in_l_consistent) {
    | [] => None
    | [m, ..._] => m // unspecified choice
    };
  };
};

// let ( x|)
// insert =

// look up viable sorts on left side of relatives.
// viable means leq expected sort from some chain on left.
// left only because we want parse always to be consistent
// with a fresh left-to-right transcription.
let mold = (t: Token.t, rel: t): option(Mold.t) => {
  // TODO rename kid_l, only potentially kid of token
  let rec go = (kid_l: option(Sort.t), rel: t) =>
    switch (rel.sib) {
    | ([c, ...pre], suf) =>
      switch (Chain.expected_sort(R, c)) {
      | Some(s) => choose(kid_l, s, t)
      | None => go(Some(Chain.sort(c)), {...rel, sib: (pre, suf)})
      }
    | ([], _) =>
      switch (Ancestors.pop(rel.anc)) {
      | Some((g, anc)) =>
        let sib = Generation.disassemble(g);
        go(kid_l, {sib, anc});
      | None => choose(kid_l, Sort.root, t)
      }
    };
  go(None, rel);
};
