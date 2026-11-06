[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | Wild
  | Atom(Atom.cls)
  | ListLit
  | Constructor
  | Cons
  | Var
  | Label
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
  | Tuple(_) => Tuple
  | Parens(_) => Parens
  | Projector(_) => Projector
  | Ap({term: Constructor(_), _}, _) => ApCons
  | Ap(_) => ApFunc
  | Asc(_) => Asc;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid pattern"
  | MultiHole => "Broken pattern"
  | EmptyHole => "Empty pattern hole"
  | Wild => "Wildcard"
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
    | Asc(pat, _) => is_tuple_of_vars(pat)
    | Tuple(pats) =>
      pats
      |> List.for_all(entry =>
           entry |> TupleEntry.value |> is_var |> Option.is_some
         )
    | Label(_)
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
  | Projector(_, pat) => get_var(pat)
  | Var(x) => Some(x)
  | Asc(x, _) => get_var(x)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Atom(_)
  | ListLit(_)
  | Cons(_, _)
  | Label(_)
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
    | Asc(pat, _) => get_num_of_vars(pat)
    | Tuple(pats) => is_tuple_of_vars(pat) ? Some(List.length(pats)) : None
    | Label(_)
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

let rec ctr_name = (p: t): option(Constructor.t) =>
  switch (p.term) {
  | Constructor(name, _) => Some(name)
  | Parens(p)
  | Projector(_, p) => ctr_name(p)
  | Asc(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Atom(_)
  | ListLit(_)
  | Cons(_)
  | Var(_)
  | Tuple(_)
  | Label(_)
  | Ap(_) => None
  };

/* The ids p's statics info is stored under: a tuple also owns its
   entries' `=` tiles, as it owns its commas */
let info_ids = (p: t): list(Id.t) =>
  switch (p.term) {
  | Tuple(entries) => IdTagged.ids(p) @ TupleEntry.eq_ids(entries)
  | _ => IdTagged.ids(p)
  };

let rec bindings = (dp: t): Binding.s =>
  switch (dp |> term_of) {
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Invalid(_)
  | Atom(_)
  | Label(_)
  | Constructor(_) => []
  | Asc(y, _)
  | Parens(y)
  | Projector(_, y) => bindings(y)
  | Var(name) => [
      {
        name,
        id: rep_id(dp),
      },
    ]
  | Tuple(dps) =>
    List.concat_map(entry => bindings(TupleEntry.value(entry)), dps)
  | Cons(dp1, dp2) => bindings(dp1) @ bindings(dp2)
  | ListLit(dps) => List.flatten(List.map(bindings, dps))
  | Ap(_, dp1) => bindings(dp1)
  };

let bound_vars = (dp: t): list(Var.t) =>
  dp |> bindings |> List.map((b: Binding.t) => b.name);

let get_duplicate_bindings = (pat: t) => {
  let bindings = bound_vars(pat);
  List.filter(
    binding => {List.length(List.filter(x => x == binding, bindings)) > 1},
    bindings,
  );
};
