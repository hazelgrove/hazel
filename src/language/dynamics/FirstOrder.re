/* (^ticket(718), ^report(50)'s next step 7: values as data where they cross) First-order data: a closed value with no
   holes, no functions and no closures (numbers, strings, booleans, lists, tuples, labels, constructors), held without
   the term's annotations: no ids, no secondary, no incomplete tiles. A literal keeps its spelling (lexeme), and a
   constructor its type, so a value made a term again is the same value, but for its ids.

   ^report(47) measured the Witch's picture as data at 48 KB against 142 KB as a term, and its trip 2.5 to 4.3 times
   quicker. This is the first slice: the worker's probe samples carried as data to the page (WorkerServer.Wire), and
   made terms again there. Matthew, 2026-11-08: "a special case of Value ... that's especially efficient to serialize
   for first-order data"; "Values should not have source ids". */
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Atom(Atom.t, option(string))
  | List(list(t))
  | Tuple(list(t))
  | Label(string, t)
  | Ctor(string, option(option(Typ.t)), option(t));

/* the annotation carries nothing but ids (and, for a literal, its spelling) */
let plain = (e: Exp.t, ~lexeme_ok: bool): bool => {
  let a = e.annotation;
  a.secondary == IdTagged.IdTag.empty_secondary
  && a.incomplete == []
  && (lexeme_ok || a.lexeme == None);
};

let rec all = (f, xs) =>
  switch (xs) {
  | [] => Some([])
  | [x, ...rest] =>
    switch (f(x)) {
    | None => None
    | Some(y) =>
      switch (all(f, rest)) {
      | None => None
      | Some(ys) => Some([y, ...ys])
      }
    }
  };

/* None at the first node that is not first-order data (a function, a closure, a hole, a cast, an ascription, a
   parenthesis, an annotation carrying more than ids): such a value stays a term */
let rec of_term = (e: Exp.t): option(t) =>
  switch (e.term) {
  | Atom(a) when plain(e, ~lexeme_ok=true) =>
    Some(Atom(a, e.annotation.lexeme))
  | _ when !plain(e, ~lexeme_ok=false) => None
  | ListLit(xs) => Option.map(xs => List(xs), all(of_term, xs))
  | Tuple(xs) => Option.map(xs => Tuple(xs), all(of_term, xs))
  | TupLabel({term: Label(l), _} as lab, v)
      when plain(lab, ~lexeme_ok=false) =>
    Option.map(v => Label(l, v), of_term(v))
  | Constructor(name, ty) => Some(Ctor(name, ty, None))
  | Ap(Forward, {term: Constructor(name, ty), _} as c, arg)
      when plain(c, ~lexeme_ok=false) =>
    Option.map(a => Ctor(name, ty, Some(a)), of_term(arg))
  | _ => None
  };

/* The same value as a term, each node with a fresh id */
let mk = (term: Exp.term): Exp.t => IdTagged.fresh(term);
let rec to_term = (d: t): Exp.t =>
  switch (d) {
  | Atom(a, lexeme) => {
      term: Atom(a),
      annotation: {
        ...IdTagged.IdTag.fresh(),
        lexeme,
      },
    }
  | List(xs) => mk(ListLit(List.map(to_term, xs)))
  | Tuple(xs) => mk(Tuple(List.map(to_term, xs)))
  | Label(l, v) => mk(TupLabel(mk(Label(l)), to_term(v)))
  | Ctor(name, ty, None) => mk(Constructor(name, ty))
  | Ctor(name, ty, Some(a)) =>
    mk(Ap(Forward, mk(Constructor(name, ty)), to_term(a)))
  };

/* Why a value is not first-order data: the first node of [of_term]'s walk that is not, by its constructor's name (or
   "annotation" for one that carries more than ids); None when it is */
let rec why_not = (e: Exp.t): option(string) =>
  switch (e.term) {
  | Atom(_) when plain(e, ~lexeme_ok=true) => None
  | _ when !plain(e, ~lexeme_ok=false) => Some("annotation")
  | ListLit(xs)
  | Tuple(xs) => List.find_map(why_not, xs)
  | TupLabel({term: Label(_), _}, v) => why_not(v)
  | Constructor(_) => None
  | Ap(Forward, {term: Constructor(_), _}, arg) => why_not(arg)
  | t =>
    let s = Exp.show_term(t);
    Some(
      switch (String.index_opt(s, ' '), String.index_opt(s, '(')) {
      | (Some(i), Some(j)) => String.sub(s, 0, min(i, j))
      | (Some(i), None)
      | (None, Some(i)) => String.sub(s, 0, i)
      | (None, None) => s
      },
    );
  };
