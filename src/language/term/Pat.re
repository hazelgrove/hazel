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
  | Var
  | Label
  | TupLabel
  | Tuple
  | Parens
  | Projector
  | ApFunc
  | ApCons
  | Asc
  | Implicit;

include TermBase.Pat;

let fast_equal = Equality.syntactic.pat;
let equal = fast_equal;

let rep_id = ({annotation: {ids, _}, _}: t) => {
  assert(ids != []);
  List.hd(ids);
};

let term_of: t => TermBase.Pat.term = IdTagged.term_of;

let unwrap: t => (term, term => t) = IdTagged.unwrap;

let fresh: term => t = IdTagged.fresh;

let hole = (tms: list(TermBase.Any.t)): TermBase.Pat.term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
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
  | Var(_) => Var
  | Label(_) => Label
  | ExplicitNonlabel => ExplicitNonlabel
  | TupLabel(_) => TupLabel
  | Tuple(_) => Tuple
  | Parens(_) => Parens
  | Projector(_) => Projector
  | Ap({term: Constructor(_), _}, _) => ApCons
  | Ap(_) => ApFunc
  | Asc(_) => Asc
  | Implicit(_) => Implicit;

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
  | Var => "Variable binding"
  | Label => "Label"
  | TupLabel => "Tuple Item"
  | Tuple => "Tuple"
  | Parens => "Parenthesized pattern"
  | Projector => "Projector"
  | ApCons => "Constructor application"
  | ApFunc => "Function definition"
  | Asc => "Annotation"
  | Implicit => "Implicit module binder";

/* Whether a hole occurs in a binding position of the pattern: one a
   variable could fill. Labels, constructors and annotations are not. */
let rec has_hole_binder = (pat: t): bool =>
  switch (pat.term) {
  | EmptyHole
  | MultiHole(_) => true
  | Parens(pat)
  | Projector(_, pat)
  | Asc(pat, _)
  | TupLabel(_, pat)
  | Ap(_, pat) => has_hole_binder(pat)
  | Tuple(pats)
  | ListLit(pats) => List.exists(has_hole_binder, pats)
  | Cons(p1, p2) => has_hole_binder(p1) || has_hole_binder(p2)
  | Invalid(_)
  | Wild
  | Var(_)
  | Atom(_)
  | Label(_)
  | ExplicitNonlabel
  | Implicit(_)
  | Constructor(_) => false
  };

let rec is_var = (pat: t): option(Var.t) => {
  switch (pat.term) {
  | Parens(pat)
  | Projector(_, pat)
  | TupLabel(_, pat)
  | Asc(pat, _) => is_var(pat)
  | Var(v) => Some(v)
  | Implicit(mp) => MPat.name(mp)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Atom(_)
  | ListLit(_)
  | Cons(_, _)
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
    | Tuple(pats) => pats |> List.for_all(x => x |> is_var |> Option.is_some)
    | Label(_)
    | ExplicitNonlabel
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Atom(_)
    | ListLit(_)
    | Cons(_, _)
    | Var(_)
    | Constructor(_)
    | Implicit(_)
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
  | Implicit(mp) => MPat.name(mp)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Atom(_)
  | ListLit(_)
  | Cons(_, _)
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
    | Var(_)
    | Constructor(_)
    | Implicit(_)
    | Ap(_) => None
    }
  };

let ctr_name = (p: t): option(Constructor.t) =>
  switch (p.term) {
  | Constructor(name, _)
  | Parens({term: Constructor(name, _), _}) => Some(name)
  | _ => None
  };

let rec match_tup_label: t => option((LabeledTuple.label, t)) =
  p =>
    switch (p.term) {
    | Parens(p) => match_tup_label(p)
    | TupLabel(plab, p') =>
      switch (plab.term) {
      | Label(name) => Some((name, p'))
      | _ => None
      }
    | _ => None
    };

let get_label: t => option(LabeledTuple.label) =
  p => match_tup_label(p) |> Option.map(fst);

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
  | Tuple(dps) => List.flatten(List.map(bindings, dps))
  | Cons(dp1, dp2) => bindings(dp1) @ bindings(dp2)
  | ListLit(dps) => List.flatten(List.map(bindings, dps))
  | Ap(_, dp1) => bindings(dp1)
  | Implicit(mp) =>
    switch (MPat.name(mp), MPat.var_id(mp)) {
    | (Some(name), Some(id)) => [
        {
          name,
          id,
        },
      ]
    | _ => []
    }
  };

let bound_vars = (dp: t): list(Var.t) =>
  dp |> bindings |> List.map((b: Binding.t) => b.name);

/* The implicit module binders among a function parameter's components (the
   whole pattern or the items of its tuple), in order. */
let implicit_binders = (p: t): list(Var.t) => {
  let rec strip = (p: t) =>
    switch (term_of(p)) {
    | Parens(p) => strip(p)
    | _ => p
    };
  let of_component = (p: t) =>
    switch (term_of(strip(p))) {
    | Implicit(mp) => MPat.name(mp) |> Option.to_list
    | _ => []
    };
  switch (term_of(strip(p))) {
  | Tuple(ps) => List.concat_map(of_component, ps)
  | _ => of_component(p)
  };
};

/* The pattern a module name pattern denotes (`S : SIG` binds like an
   annotated variable), keeping the MPat's ids so recorded info lands where
   the cursor can find it. */
let rec of_mpat = (mp: MPat.t): t =>
  switch (IdTagged.term_of(mp)) {
  | Var(name) => IdTagged.fast_copy(MPat.rep_id(mp), fresh(Var(name)))
  | Asc(inner, typ) =>
    IdTagged.fast_copy(MPat.rep_id(mp), fresh(Asc(of_mpat(inner), typ)))
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => IdTagged.fast_copy(MPat.rep_id(mp), fresh(Wild))
  };

let get_duplicate_bindings = (pat: t) => {
  let bindings = bound_vars(pat);
  List.filter(
    binding => {List.length(List.filter(x => x == binding, bindings)) > 1},
    bindings,
  );
};
