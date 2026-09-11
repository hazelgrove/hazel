open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

/* Blackboard terms in the editor: classes for the cursor inspector, the
   usual id accessors, and the reading of editor terms as terms, signatures
   and documents of the core logic (BbTerm). */

module Term = {
  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
  type cls =
    | Hole
    | Var
    | Type
    | Parens
    | Mem
    | Arrow
    | Ap
    | Tuple
    | Seq
    | Assume
    | Construct;

  let show_cls =
    fun
    | Hole => "Hole"
    | Var => "Name"
    | Type => "The type of types"
    | Parens => "Parentheses"
    | Mem => "Membership"
    | Arrow => "Function type"
    | Ap => "Application"
    | Tuple => "Argument list"
    | Seq => "Sequence"
    | Assume => "Assumption block"
    | Construct => "Construction block";

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = BbTermBase.t;
  type term = BbTermBase.term;

  let rep_id: t => Id.t = IdTagged.rep_id;
  let ids: t => list(Id.t) = IdTagged.ids;
  let term_of: t => term = IdTagged.term_of;
  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Hole(_) => Hole
    | Var(_) => Var
    | Type => Type
    | Parens(_) => Parens
    | Mem(_) => Mem
    | Arrow(_) => Arrow
    | Ap(_) => Ap
    | Tuple(_) => Tuple
    | Seq(_) => Seq
    | Assume(_) => Assume
    | Construct(_) => Construct;

  let cls_of = (t: t): cls => cls_of_term(term_of(t));

  let sort: t => BbSort.t = _ => Term;

  let is_hole = (t: t): bool =>
    switch (term_of(t)) {
    | Hole(_) => true
    | _ => false
    };
};

/* Reading editor terms as kernel terms.  A problem names the node that
   could not be read and says why, in the user's terms. */
[@deriving (show({with_path: false}), sexp, yojson)]
type problem = {
  id: Id.t,
  message: string,
};

let ( let* ) = Result.bind;

let problem = (t: Term.t, message: string) =>
  Error({
    id: Term.rep_id(t),
    message,
  });

/* `(x : A)` used as the domain of an arrow is a binder.  This is the same
   convention as BbParse and BbTerm.to_string: a non-dependent arrow whose
   domain is a membership must be written with a `_` binder. */
let binder_of = (dom: Term.t): option((string, Term.t)) =>
  switch (Term.term_of(dom)) {
  | Parens(inner) =>
    switch (Term.term_of(inner)) {
    | Mem({term: Var(x), _}, a) => Some((x, a))
    | _ => None
    }
  | _ => None
  };

let rec term_to_kernel = (t: Term.t): result(BbTerm.t, problem) =>
  switch (Term.term_of(t)) {
  | Var(x) => Ok(BbTerm.Var(x))
  | Type => Ok(BbTerm.Type)
  | Parens(inner) => term_to_kernel(inner)
  | Mem(a, b) =>
    let* a = term_to_kernel(a);
    let* b = term_to_kernel(b);
    Ok(BbTerm.Mem(a, b));
  | Arrow(dom, cod) =>
    let* cod = term_to_kernel(cod);
    switch (binder_of(dom)) {
    | Some((x, a)) =>
      let* a = term_to_kernel(a);
      Ok(BbTerm.Pi(x, a, cod));
    | None =>
      let* dom = term_to_kernel(dom);
      Ok(BbTerm.Pi("_", dom, cod));
    };
  | Ap(f, arg) =>
    let* f = term_to_kernel(f);
    let args =
      switch (Term.term_of(arg)) {
      | Tuple(args) => args
      | _ => [arg]
      };
    List.fold_left(
      (acc, a) => {
        let* acc = acc;
        let* a = term_to_kernel(a);
        Ok(BbTerm.App(acc, a));
      },
      Ok(f),
      args,
    );
  | Tuple(_) =>
    problem(
      t,
      "a comma-separated list only makes sense as the argument of an application",
    )
  | Seq(_) => problem(t, "a sequence only makes sense inside a block")
  | Assume(_)
  | Construct(_) => problem(t, "a block is not a term")
  | Hole(_) => problem(t, "incomplete term")
  };

let items_of = (t: Term.t): list(Term.t) =>
  switch (Term.term_of(t)) {
  | Seq(items) => items
  | Hole(EmptyHole) => []
  | _ => [t]
  };

/* In a signature entry the colon is a declaration, not a membership: it is
   looser than everything to its right.  The membership tile binds tighter
   than the arrow (as it must inside a type, so that `(x : A) -> x : B`
   reads as the paper writes it), so `f : A -> B` arrives here as
   `Arrow(Mem(f, A), B)` and the spine has to be rotated back. */
let rec rotate_entry = (t: Term.t): option((string, Term.t)) =>
  switch (Term.term_of(t)) {
  | Mem({term: Var(name), _}, ty) => Some((name, ty))
  | Arrow(l, r) =>
    switch (rotate_entry(l)) {
    | Some((name, dom)) =>
      Some((
        name,
        Term.fresh(Arrow(dom, r)) |> IdTagged.fast_copy(Term.rep_id(t)),
      ))
    | None => None
    }
  | _ => None
  };

let entry_to_kernel = (t: Term.t): result(BbTerm.entry, problem) =>
  switch (rotate_entry(t)) {
  | Some((name, ty)) =>
    let* ty = term_to_kernel(ty);
    Ok(
      BbTerm.{
        name,
        ty,
      },
    );
  | None => problem(t, "a signature entry has the form  name : type")
  };

let signature_to_kernel = (t: Term.t): result(BbTerm.signature, problem) =>
  List.fold_right(
    (item, acc) => {
      let* acc = acc;
      let* e = entry_to_kernel(item);
      Ok([e, ...acc]);
    },
    items_of(t),
    Ok([]),
  );

/* A tactic is a name; an empty slot is no tactic. */
let tactic_to_kernel = (t: Term.t): result(option(string), problem) =>
  switch (Term.term_of(t)) {
  | Hole(EmptyHole) => Ok(None)
  | Var(x) => Ok(Some(x))
  | _ => problem(t, "a tactic is a name")
  };

let rec block_to_kernel = (t: Term.t): result(BbTerm.block, problem) =>
  switch (Term.term_of(t)) {
  | Assume(entries, tactic) =>
    let* s = signature_to_kernel(entries);
    let* tac = tactic_to_kernel(tactic);
    Ok(BbTerm.Assume(s, tac));
  | Construct(entries, tactic) =>
    let* s = signature_to_kernel(entries);
    let* tac = tactic_to_kernel(tactic);
    Ok(BbTerm.Construct(s, Option.value(~default="?", tac)));
  | Parens(inner) => block_to_kernel(inner)
  | _ => problem(t, "a document is a sequence of assume and construct blocks")
  };

let doc_to_kernel = (t: Term.t): result(BbTerm.doc, problem) =>
  List.fold_right(
    (item, acc) => {
      let* acc = acc;
      let* b = block_to_kernel(item);
      Ok([b, ...acc]);
    },
    items_of(t),
    Ok([]),
  );
