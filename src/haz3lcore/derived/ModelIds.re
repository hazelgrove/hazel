/* (^ticket(721)) The shape of a livelit's model, for a commit that changes
   only its literals: [same_shape_ids] gives a new model the old one's ids,
   [literal_changes] says which literals two models with the same ids differ
   in. Apart from ModelPatch, which patches the statics, so that LivelitProj
   can use it: the statics reach the projectors (CachedStatics,
   ProjectorInit), and the projectors would reach the statics. */
open Util;
open Language;

let keep = (_, x) => x;

let same_class = (a: Atom.t, b: Atom.t): bool =>
  Atom.cls_of_t(a) == Atom.cls_of_t(b);

/* [new_] with [old]'s ids, where the two have the same shape: the same
   constructors, labels, names and lengths, and a literal of the same class at
   each leaf, its value [new_]'s. A subterm of any other kind is kept only if it
   equals [old]'s, and then it is [old]'s. None when the shapes differ: the
   commit keeps [new_]'s fresh ids, and the statics are worked out again. */
let rec same_shape_ids = (old: Exp.t, new_: Exp.t): option(Exp.t) => {
  open OptUtil.Syntax;
  let with_ids = (term: Exp.term): Exp.t => {
    term,
    annotation: {
      ...new_.annotation,
      ids: old.annotation.ids,
    },
  };
  let all2 = (xs: list(Exp.t), ys: list(Exp.t)): option(list(Exp.t)) =>
    List.length(xs) == List.length(ys)
      ? List.fold_right2(
          (x, y, acc) => {
            let* acc = acc;
            let+ z = same_shape_ids(x, y);
            [z, ...acc];
          },
          xs,
          ys,
          Some([]),
        )
      : None;
  switch (old.term, new_.term) {
  | (Atom(a), Atom(b)) when same_class(a, b) => Some(with_ids(Atom(b)))
  | (Parens(x), Parens(y)) =>
    let+ z = same_shape_ids(x, y);
    with_ids(Parens(z));
  | (Tuple(xs), Tuple(ys)) =>
    let+ zs = all2(xs, ys);
    with_ids(Tuple(zs));
  | (ListLit(xs), ListLit(ys)) =>
    let+ zs = all2(xs, ys);
    with_ids(ListLit(zs));
  | (TupLabel(l, x), TupLabel(l', y)) =>
    let* l2 = same_shape_ids(l, l');
    let+ z = same_shape_ids(x, y);
    with_ids(TupLabel(l2, z));
  | (Ap(d, f, x), Ap(d', g, y)) when d == d' =>
    let* f2 = same_shape_ids(f, g);
    let+ z = same_shape_ids(x, y);
    with_ids(Ap(d, f2, z));
  | _ => Exp.fast_equal_with_lexemes(old, new_) ? Some(old) : None
  };
};

/* the literals [new_] gives new values, by id, where [new_] has [old]'s ids and
   shape throughout (as [same_shape_ids] makes it); None where it does not */
let rec literal_changes =
        (old: Exp.t, new_: Exp.t): option(list((Id.t, Exp.t))) => {
  open OptUtil.Syntax;
  let all2 = (xs: list(Exp.t), ys: list(Exp.t)) =>
    List.length(xs) == List.length(ys)
      ? List.fold_right2(
          (x, y, acc) => {
            let* acc = acc;
            let+ c = literal_changes(x, y);
            c @ acc;
          },
          xs,
          ys,
          Some([]),
        )
      : None;
  if (Exp.rep_id(old) != Exp.rep_id(new_)) {
    None;
  } else {
    switch (old.term, new_.term) {
    | (Atom(a), Atom(b)) when same_class(a, b) =>
      Some(Atom.equal(a, b) ? [] : [(Exp.rep_id(new_), new_)])
    | (Parens(x), Parens(y)) => literal_changes(x, y)
    | (Tuple(xs), Tuple(ys))
    | (ListLit(xs), ListLit(ys)) => all2(xs, ys)
    | (TupLabel(l, x), TupLabel(l', y))
    | (Ap(_, l, x), Ap(_, l', y)) => all2([l, x], [l', y])
    | _ => Exp.fast_equal_with_lexemes(old, new_) ? Some([]) : None
    };
  };
};
