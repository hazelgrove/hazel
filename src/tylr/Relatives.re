type t = {
  sib: Siblings.t,
  anc: Ancestors.t,
};

let choose_cur = (_: t): Dir.t => failwith("todo");

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

let shift_char = (from: Dir.t, rel) => {
  open OptUtil.Syntax;
  let+ (c, rel) = pop_char(rel);
  push_char(Dir.toggle(from), c, rel);
};

// let ( x|)
// insert =

// look up viable sorts on left side of relatives.
// viable means leq expected sort from some chain on left.
// left only because we want parse always to be consistent
// with a fresh left-to-right transcription.
// precond: inputs already lexed such that t is not part
// of a token in rel.
// returns, in addition to the assigned mold, the
// relatives produced by pushing the molded token onto rel,
// which is useful accumulator state to expose when molding
// a sequence of tokens to avoid redoing work.
let mold = (t: Token.t, rel: t): option(Mold.t) => {
  let rec go = (~in_l: option(Sort.t)=?, rel: t) => {
    let (pre, _) = rel.sib;
    switch (Aba.unsnoc(pre)) {
    | Some((pre, c, _)) =>
      let go_next = () =>
        go(~in_l=Chain.sort(c), {...rel, sib: (pre, suf)});
      switch (Chain.expected_sort(R, c)) {
      | None => go_next()
      | Some(out) =>
        switch (choose(in_l, out, t)) {
        | None => go_next()
        | Some(m) => Some(m)
        }
      };
    | None =>
      switch (Ancestors.pop(rel.anc)) {
      | Some((g, anc)) =>
        let sib = Generation.disassemble(g);
        go(kid_l, {sib, anc});
      | None => choose(kid_l, Sort.root, t)
      }
    };
  };
  go(rel);
};

// postcond: returned token empty if nothing to pop
let pop_adj_token = (d: Dir.t, rel: t): (Token.t, t) => failwith("todo");

let lex = (s: string, rel: t): (Aba.t(Space.t, Token.t), (int, int), t) => {
  let (l, rel) = pop_adj_token(L, rel);
  let (r, rel) = pop_adj_token(R, rel);
  let popped_len = Token.(length(l), length(r));
  (LangUtil.lex(l ++ s ++ r), popped_len, rel);
};
