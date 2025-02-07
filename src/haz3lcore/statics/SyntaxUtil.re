open Term;

module Exp = {
  /* Strips outer parentheses, unless the
   * innermost outer parenthesis is on a tuple */
  let rec strip_wraps = (e: Exp.t): Exp.t => {
    switch (e.term) {
    | Parens(inner) =>
      switch (inner.term) {
      | Tuple(_) => e
      | _ => strip_wraps(inner)
      }
    | _ => e
    };
  };
  let get_wrap = (term: Exp.t): option(Exp.t) =>
    switch (term) {
    | {term: Parens(term), _} => Some(term)
    | _ => None
    };

  let get_tuple = (term: Exp.t): option(list(Exp.t)) =>
    switch (term) {
    | {term: Tuple(terms), _} => Some(terms)
    | _ => None
    };

  let get_two_tuple = (term: Exp.t): option((Exp.t, Exp.t)) =>
    switch (get_tuple(term)) {
    | Some([term1, term2]) => Some((term1, term2))
    | _ => None
    };

  let get_constructor = (term: Exp.t): option(string) =>
    switch (term) {
    | {term: Constructor(str, _), _} => Some(str)
    | _ => None
    };

  let mk_constructor = (str: string): Exp.t =>
    IdTagged.fresh(
      Constructor(str, Unknown(Internal) |> Typ.temp): Exp.term,
    );

  let mk_tuple = (children: list(Exp.t)): Exp.t =>
    IdTagged.fresh(Tuple(children): Exp.term);

  let mk_listlit = (children: list(Exp.t)): Exp.t =>
    IdTagged.fresh(ListLit(children): Exp.term);

  let mk_wrap = (term: Exp.t): Exp.t =>
    IdTagged.fresh(Parens(term): Exp.term);

  let mk_wrapped_tuple = (ps: list(Exp.t)): Exp.t => mk_wrap(mk_tuple(ps));

  let constr_of_sexp = (sexp: Sexplib.Sexp.t): Exp.t =>
    mk_constructor(Sexplib.Sexp.to_string(sexp));
};

module Pat = {
  let rec strip_wraps = (p: Pat.t): Pat.t => {
    switch (p.term) {
    | Parens(inner) =>
      switch (inner.term) {
      | Tuple(_) => p
      | _ => strip_wraps(inner)
      }
    | _ => p
    };
  };

  let get_wrap = (term: Pat.t): option(Pat.t) =>
    switch (term) {
    | {term: Parens(term), _} => Some(term)
    | _ => None
    };

  let get_tuple = (term: Pat.t): option(list(Pat.t)) =>
    switch (term) {
    | {term: Tuple(terms), _} => Some(terms)
    | _ => None
    };

  let get_two_tuple = (term: Pat.t): option((Pat.t, Pat.t)) =>
    switch (get_tuple(term)) {
    | Some([term1, term2]) => Some((term1, term2))
    | _ => None
    };

  let get_constructor = (term: Pat.t): option(string) => {
    switch (term) {
    | {term: Constructor(str, _), _} => Some(str)
    | {term: Var(str), _} => Some(str)
    | {term: Wild, _} => Some("_")
    | _ => None
    };
  };

  let get_listlit = (term: Pat.t): option(list(Pat.t)) =>
    switch (term) {
    | {term: ListLit(terms), _} => Some(terms)
    | _ => None
    };

  let mk_constructor = (str: string): Pat.t =>
    IdTagged.fresh(
      Constructor(str, Unknown(Internal) |> Typ.temp): Pat.term,
    );

  let mk_tuple = (children: list(Pat.t)): Pat.t =>
    IdTagged.fresh(Tuple(children): Pat.term);

  let mk_listlit = (children: list(Pat.t)): Pat.t =>
    IdTagged.fresh(ListLit(children): Pat.term);

  let mk_wrap = (term: Pat.t): Pat.t =>
    IdTagged.fresh(Parens(term): Pat.term);

  let mk_wrapped_tuple = (ps: list(Pat.t)): Pat.t => mk_wrap(mk_tuple(ps));

  let constr_of_sexp = (sexp: Sexplib.Sexp.t): Pat.t =>
    mk_constructor(Sexplib.Sexp.to_string(sexp));
};
