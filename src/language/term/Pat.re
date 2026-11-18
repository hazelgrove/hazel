[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | Wild
  | ExplicitNonlabel
  | Atom(Atom.cls)
  | ListLit
  | Constructor
  | Cons
  | Add
  | Var
  | Label
  | TupLabel
  | Tuple
  | Parens
  | Projector
  | ApFunc
  | ApCons
  | Asc;

include TermBase.Pat;

let fast_equal = Equality.syntactic.pat;
let equal = fast_equal;

let rep_id = ({annotation: {ids, _}, _}: t) => {
  List.hd_exn(ids);
};

let term_of: t => TermBase.Pat.term = IdTagged.term_of;

let unwrap: t => (term, term => t) = IdTagged.unwrap;

let fresh: term => t = IdTagged.fresh;

let hole = (tms: list(TermBase.Any.t)): TermBase.Pat.term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
  };

/* Parens and Projector are transparent: elaboration keeps both, so code
   that inspects the head or shape of a pattern must look through them.
   Asc is NOT stripped here; whether an ascription is transparent depends on
   the call site. */
let rec strip_wrappers = (p: t): t =>
  switch (p.term) {
  | Parens(p)
  | Projector(_, p) => strip_wrappers(p)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | ExplicitNonlabel
  | Atom(_)
  | ListLit(_)
  | Constructor(_)
  | Cons(_)
  | Add(_)
  | Var(_)
  | Tuple(_)
  | Label(_)
  | TupLabel(_)
  | Ap(_)
  | Asc(_) => p
  };

let rec ctr_name = (p: Grammar.pat_t('a)): option(Constructor.t) =>
  switch (p.term) {
  | Constructor(name, _) => Some(name)
  | Parens(p)
  | Projector(_, p) => ctr_name(p)
  | Asc(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | ExplicitNonlabel
  | Atom(_)
  | ListLit(_)
  | Cons(_)
  | Add(_)
  | Var(_)
  | Tuple(_)
  | Label(_)
  | TupLabel(_)
  | Ap(_) => None
  };

let cls_of_term: Grammar.pat_term('a) => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | Wild => Wild
  | Atom(c) => Atom(Atom.cls_of_t(c))
  | ListLit(_) => ListLit
  | Constructor(_) => Constructor
  | Cons(_) => Cons
  | Add(_) => Add
  | Var(_) => Var
  | Label(_) => Label
  | ExplicitNonlabel => ExplicitNonlabel
  | TupLabel(_) => TupLabel
  | Tuple(_) => Tuple
  | Parens(_) => Parens
  | Projector(_) => Projector
  | Ap(f, _) => Option.is_some(ctr_name(f)) ? ApCons : ApFunc
  | Asc(_) => Asc;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid pattern"
  | MultiHole => "Broken pattern"
  | EmptyHole => "Empty pattern hole"
  | Wild => "Wildcard"
  | ExplicitNonlabel => "Explicitly unlabeled entry"
  | Atom(Int) => "Number literal"
  | Atom(Float) => "Float literal"
  | Atom(Bool) => "Boolean literal"
  | Atom(String) => "String literal"
  | Atom(Nat) => "Natural number literal"
  | Atom(SInt) => "System integer literal"
  | ListLit => "List literal"
  | Constructor => "Constructor"
  | Cons => "Cons"
  | Add => "Addition pattern"
  | Var => "Variable binding"
  | Label => "Label"
  | TupLabel => "Tuple Item"
  | Tuple => "Tuple"
  | Parens => "Parenthesized pattern"
  | Projector => "Projector"
  | ApCons => "Constructor application"
  | ApFunc => "Function definition"
  | Asc => "Annotation";

let rec is_var = (pat: t): option(Var.t) => {
  switch (pat.term) {
  | Parens(pat)
  | Projector(_, pat)
  | TupLabel(_, pat)
  | Asc(pat, _) => is_var(pat)
  | Var(v) => Some(v)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Atom(_)
  | ListLit(_)
  | Cons(_, _)
  | Add(_, _)
  | Tuple(_)
  | Label(_)
  | ExplicitNonlabel
  | Constructor(_)
  | Ap(_) => None
  };
};

let rec is_tuple_of_vars = (pat: t) =>
  Option.is_some(is_var(pat))
  || (
    switch (pat.term) {
    | Parens(pat)
    | Projector(_, pat)
    | Asc(pat, _)
    | TupLabel(_, pat) => is_tuple_of_vars(pat)
    | Tuple(pats) =>
      pats |> List.for_all(~f=x => x |> is_var |> Option.is_some)
    | Label(_)
    | ExplicitNonlabel
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Atom(_)
    | ListLit(_)
    | Cons(_, _)
    | Add(_, _)
    | Var(_)
    | Constructor(_)
    | Ap(_) => false
    }
  );

let rec get_var = (pat: t) => {
  switch (pat.term) {
  | Parens(pat)
  | Projector(_, pat)
  | TupLabel(_, pat) => get_var(pat)
  | Var(x) => Some(x)
  | Asc(x, _) => get_var(x)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Atom(_)
  | ListLit(_)
  | Cons(_, _)
  | Add(_, _)
  | Label(_)
  | ExplicitNonlabel
  | Tuple(_)
  | Constructor(_)
  | Ap(_) => None
  };
};

let rec get_num_of_vars = (pat: t) =>
  switch (is_var(pat)) {
  | Some(_) => Some(1)
  | None =>
    switch (pat.term) {
    | Parens(pat)
    | Projector(_, pat)
    | Asc(pat, _)
    | TupLabel(_, pat) => get_num_of_vars(pat)
    | Tuple(pats) => is_tuple_of_vars(pat) ? Some(List.length(pats)) : None
    | Label(_)
    | ExplicitNonlabel
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Atom(_)
    | ListLit(_)
    | Cons(_, _)
    | Add(_, _)
    | Var(_)
    | Constructor(_)
    | Ap(_) => None
    }
  };

let rec match_tup_label: t => option((LabeledTuple.label, t)) =
  p =>
    switch (p.term) {
    | Parens(p)
    | Projector(_, p) => match_tup_label(p)
    | TupLabel(plab, p') =>
      switch (strip_wrappers(plab).term) {
      | Label(name) => Some((name, p'))
      | _ => None
      }
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | ExplicitNonlabel
    | Atom(_)
    | ListLit(_)
    | Constructor(_)
    | Cons(_)
    | Add(_)
    | Var(_)
    | Tuple(_)
    | Label(_)
    | Ap(_)
    | Asc(_) => None
    };

let get_label: t => option(LabeledTuple.label) =
  p => match_tup_label(p) |> Option.map(~f=fst);

let rec bindings = (dp: t): Binding.s =>
  switch (dp |> term_of) {
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Invalid(_)
  | Atom(_)
  | Label(_)
  | ExplicitNonlabel
  | Constructor(_) => []
  | Asc(y, _)
  | Parens(y)
  | Projector(_, y)
  | TupLabel(_, y) => bindings(y)
  | Var(name) => [
      {
        name,
        id: rep_id(dp),
      },
    ]
  | Tuple(dps) => List.concat_map(~f=bindings, dps)
  | Cons(dp1, dp2)
  | Add(dp1, dp2) => bindings(dp1) @ bindings(dp2)
  | ListLit(dps) => List.concat_map(~f=bindings, dps)
  | Ap(_, dp1) => bindings(dp1)
  };

let bound_vars = (dp: t): list(Var.t) =>
  dp |> bindings |> List.map(~f=(b: Binding.t) => b.name);

let get_duplicate_bindings = (pat: t) => {
  let bindings = bound_vars(pat);
  List.filter(
    ~f=
      binding => {
        List.length(List.filter(~f=x => String.equal(x, binding), bindings))
        > 1
      },
    bindings,
  );
};

/* Arithmetic for addition patterns `p + c`, where `c` folds to a number
   literal. Elaboration gives `c` the kind of the number it is matched
   against, so on an elaborated pattern `c` says whether it is a Nat. */
module Addition = {
  let plus = (a: Atom.t, b: Atom.t): option(Atom.t) =>
    switch (a, b) {
    | (Int(a), Int(b)) => Some(Int(Bigint.(a + b)))
    | (Nat(a), Nat(b)) => Some(Nat(Bigint.(a + b)))
    | (SInt(a), SInt(b)) => Some(SInt(a + b))
    | _ => None
    };

  /* `None` for a negative Nat: `p + 2` doesn't match 1. */
  let minus = (a: Atom.t, b: Atom.t): option(Atom.t) =>
    switch (a, b) {
    | (Int(a), Int(b)) => Some(Int(Bigint.(a - b)))
    | (Nat(a), Nat(b)) => Bigint.(a >= b) ? Some(Nat(Bigint.(a - b))) : None
    | (SInt(a), SInt(b)) => Some(SInt(a - b))
    | _ => None
    };

  /* The number a pattern of literals and additions stands for. */
  let rec const = (p: t): option(Atom.t) =>
    switch (p.term) {
    | Atom((Int(_) | Nat(_) | SInt(_)) as a) => Some(a)
    | Parens(p)
    | Asc(p, _) => const(p)
    | Add(p1, p2) =>
      switch (const(p1), const(p2)) {
      | (Some(c1), Some(c2)) => plus(c1, c2)
      | _ => None
      }
    | Atom(Float(_) | Bool(_) | String(_))
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | ExplicitNonlabel
    | ListLit(_)
    | Constructor(_)
    | Cons(_)
    | Var(_)
    | Tuple(_)
    | Label(_)
    | TupLabel(_)
    | Projector(_)
    | Ap(_) => None
    };

  /* `p1 + p2` as the operand left to match and the constant to take off
     the number. The right operand is the constant when both are. `None`
     when neither is, which the statics mark. */
  let split = (p1: t, p2: t): option((t, Atom.t)) =>
    switch (const(p2), const(p1)) {
    | (Some(c), _) => Some((p1, c))
    | (None, Some(c)) => Some((p2, c))
    | (None, None) => None
    };
};
