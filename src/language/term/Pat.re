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
  | Where
  | StdNamed;

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
  | Where(_) => Where
  | StdNamed(_) => StdNamed;

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
  | Where => "Guarded pattern"
  | StdNamed => "Binder with named standardness fact";

let rec is_var = (pat: t): option(Var.t) => {
  switch (pat.term) {
  | Parens(pat)
  | Projector(_, pat)
  | TupLabel(_, pat)
  /* `p where g` binds exactly what `p` binds: transparent here, like
     Asc. The guard is carried for dynamics + proofs, not for binding. */
  | Where(pat, _)
  | StdNamed(pat, _)
  | Asc(pat, _) => is_var(pat)
  | Var(v) => Some(v)
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
    | Where(pat, _)
    | StdNamed(pat, _)
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
    | Ap(_) => false
    }
  );

let rec get_var = (pat: t) => {
  switch (pat.term) {
  | Parens(pat)
  | Projector(_, pat)
  | TupLabel(_, pat) => get_var(pat)
  | Var(x) => Some(x)
  | Asc(x, _)
  | Where(x, _) => get_var(x)
  | StdNamed(x, _) => get_var(x)
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

let rec get_fun_var = (pat: t) => {
  switch (pat.term) {
  | Parens(pat)
  | Projector(_, pat)
  | Where(pat, _)
  | StdNamed(pat, _)
  | TupLabel(_, pat) => get_fun_var(pat)
  | Asc(pat, t1) =>
    if (Typ.is_arrow(t1) || Typ.is_poly(t1)) {
      get_var(pat) |> Option.map(var => var);
    } else {
      None;
    }
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Atom(_)
  | ListLit(_)
  | Cons(_, _)
  | Var(_)
  | Label(_)
  | ExplicitNonlabel
  | Tuple(_)
  | Constructor(_)
  | Ap(_) => None
  };
};

let rec get_bindings = (pat: t) =>
  switch (get_var(pat)) {
  | Some(x) => Some([x])
  | None =>
    switch (pat.term) {
    | Parens(pat)
    | Projector(_, pat)
    | Asc(pat, _)
    | Where(pat, _)
    | StdNamed(pat, _)
    | TupLabel(_, pat) => get_bindings(pat)
    | Tuple(pats) =>
      let vars = pats |> List.map(get_var);
      if (List.exists(Option.is_none, vars)) {
        None;
      } else {
        Some(List.map(Option.get, vars));
      };
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
    | Ap(_) => None
    }
  };

let rec get_num_of_vars = (pat: t) =>
  switch (is_var(pat)) {
  | Some(_) => Some(1)
  | None =>
    switch (pat.term) {
    | Parens(pat)
    | Projector(_, pat)
    | Asc(pat, _)
    | Where(pat, _)
    | StdNamed(pat, _)
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
    | Ap(_) => None
    }
  };

/* Does this pattern carry a `where` guard (docs/prover-obligations.md
   §2.2)? Looks through the transparent wrappers, so
   `((h :: t) where g) : List(Int)` counts.

   Used by the MULTI-ROW coverage positions (`case`, `induction`): a
   guarded arm is not a case split, so it is dropped from the coverage
   matrix rather than counted as covering its constructor. Single-pattern
   binders (`fun` / `let` / `forall`) treat `where` as transparent
   instead: a guard there narrows the binder's domain, it does not
   split a scrutinee. */
let rec is_guarded = (p: t): bool =>
  switch (p |> term_of) {
  | Where(_, _) => true
  | StdNamed(p, _)
  | Parens(p)
  | Projector(_, p)
  | Asc(p, _)
  | TupLabel(_, p) => is_guarded(p)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Atom(_)
  | ListLit(_)
  | Cons(_, _)
  | Var(_)
  | Tuple(_)
  | Label(_)
  | ExplicitNonlabel
  | Constructor(_)
  | Ap(_) => false
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
  | Where(y, _)
  | StdNamed(y, _)
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
  };

let bound_vars = (dp: t): list(Var.t) =>
  dp |> bindings |> List.map((b: Binding.t) => b.name);

let bound_var_ids = (ctx, pat): list(Binding.t) =>
  bound_vars(pat)
  |> List.map(name =>
       switch (Ctx.lookup_var(ctx, name)) {
       | Some({id, _}) =>
         Binding.{
           id,
           name,
         }
       | None => {
           id: Id.invalid,
           name,
         }
       }
     );

let get_duplicate_bindings = (pat: t) => {
  let bindings = bound_vars(pat);
  List.filter(
    binding => {List.length(List.filter(x => x == binding, bindings)) > 1},
    bindings,
  );
};

/* D78 binder (decided "binder"): the typed single-variable shape
   `(x : T !! h)`, parens-transparent, as (x, T, h). Anything else --
   pattern binders, an omitted type -- is an unresolved surface and
   returns None (refused, never guessed). */
let rec std_binder = (p: t): option((Var.t, Typ.t, Var.t)) => {
  let rec strip = (p: t) =>
    switch (p.term) {
    | Parens(p) => strip(p)
    | _ => p
    };
  switch (p.term) {
  | Parens(p) => std_binder(p)
  | StdNamed(b, h) =>
    switch (strip(b).term, strip(h).term) {
    | (Asc(x, ty), Var(h)) =>
      switch (strip(x).term) {
      | Var(x) => Some((x, ty, h))
      | _ => None
      }
    | _ => None
    }
  | _ => None
  };
};
