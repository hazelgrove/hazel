/* The decidable fragment of Blackboard's core logic that the paper assumes
   under the name `tychk` (l. 154-156), minus conversion, since the logic has
   no reduction.  Bidirectional: `synth` finds the type of a term, `is_type`
   checks that a term is a type.  Every step is an instance of a rule of the
   corrected Figure 1 (hyp, type, ap, arrow, in, and in- for hypotheses), so
   whatever passes is derivable; much that is derivable does not pass, and
   the paper says so.

   Two Blackboard-specific points:

   - `(t : T) : type` needs only that T is a type and t has SOME type
     (l. 80), not that t has type T.

   - Because typing is internal, a hypothesis h : (t : T) in the context
     establishes t : T (rules hyp and in-).  The refinement and intersection
     eliminators of Section 4 rely on this. */

open BbTerm;

/* Innermost binding first. */
type ctx = list((string, BbTerm.t));

let lookup = (ctx: ctx, x: string): option(BbTerm.t) =>
  List.assoc_opt(x, ctx);

/* Some hypothesis in the context asserts t : ty. */
let membership_hyp = (ctx: ctx, t: BbTerm.t, ty: BbTerm.t): bool =>
  List.exists(
    ((_, h)) =>
      switch (h) {
      | Mem(t', ty') => alpha_eq(t, t') && alpha_eq(ty, ty')
      | _ => false
      },
    ctx,
  );

/* Some hypothesis in the context asserts t : T for some T. */
let some_membership_hyp = (ctx: ctx, t: BbTerm.t): bool =>
  List.exists(
    ((_, h)) =>
      switch (h) {
      | Mem(t', _) => alpha_eq(t, t')
      | _ => false
      },
    ctx,
  );

/* Errors are collected where subterms are independent, so that several
   slips in one signature entry are all reported. */
type errors = list(BbError.t);

let rec synth = (ctx: ctx, t: BbTerm.t): result(BbTerm.t, errors) =>
  switch (t) {
  | Var(x) =>
    switch (lookup(ctx, x)) {
    | Some(ty) => Ok(ty)
    | None => Error([Unbound(x)])
    }
  | Type => Ok(Type)
  | App(f, a) =>
    switch (synth(ctx, f)) {
    | Error(es) => Error(es)
    | Ok(Pi(x, dom, cod)) =>
      switch (has_type(ctx, a, dom)) {
      | Error(es) => Error(es)
      | Ok(None) => Ok(subst(x, a, cod))
      | Ok(Some(actual)) => Error([ArgumentMismatch(f, a, dom, actual)])
      }
    | Ok(ty) => Error([NotAFunction(f, ty)])
    }
  | Mem(_)
  | Pi(_) =>
    switch (is_type(ctx, t)) {
    | [] => Ok(Type)
    | es => Error(es)
    }
  }

/* Ok(None): t has type `expected`.  Ok(Some(actual)): it has another type.
   Error: t is ill-formed. */
and has_type =
    (ctx: ctx, t: BbTerm.t, expected: BbTerm.t)
    : result(option(BbTerm.t), errors) =>
  if (membership_hyp(ctx, t, expected)) {
    Ok(None);
  } else {
    switch (synth(ctx, t)) {
    | Error(es) => Error(es)
    | Ok(actual) =>
      alpha_eq(actual, expected) ? Ok(None) : Ok(Some(actual))
    };
  }

/* Empty list: t is a type in ctx. */
and is_type = (ctx: ctx, t: BbTerm.t): errors =>
  switch (t) {
  | Type => []
  | Pi(x, a, b) => is_type(ctx, a) @ is_type([(x, a), ...ctx], b)
  | Mem(u, ty) =>
    let has_some_type =
      some_membership_hyp(ctx, u)
        ? []
        : (
          switch (synth(ctx, u)) {
          | Ok(_) => []
          | Error(es) => es
          }
        );
    is_type(ctx, ty) @ has_some_type;
  | Var(_)
  | App(_) =>
    switch (has_type(ctx, t, Type)) {
    | Ok(None) => []
    | Ok(Some(actual)) => [NotAType(t, actual)]
    | Error(es) => es
    }
  };

/* Check a signature entry by entry, each in the context of the ones before
   it (the obligation of an assumption block, l. 149).  An entry with errors
   still enters the context, so later entries can be checked. */
let check_signature = (ctx: ctx, s: signature): (ctx, list(BbError.located)) => {
  let (ctx, errs) =
    List.fold_left(
      ((ctx, errs), {name, ty}) => {
        let here =
          List.map(
            err =>
              BbError.{
                entry: name,
                err,
              },
            is_type(ctx, ty),
          );
        ([(name, ty), ...ctx], List.rev_append(here, errs));
      },
      (ctx, []),
      s,
    );
  (ctx, List.rev(errs));
};

/* What the checker can say about one block. */
[@deriving (show({with_path: false}), eq)]
type report = {
  index: int,
  errors: list(BbError.located),
  /* a construct block's tactic, which nothing discharges yet */
  pending: option(string),
};

let check_doc = (~ctx: ctx=[], d: doc): (ctx, list(report)) => {
  let (ctx, reports, _) =
    List.fold_left(
      ((ctx, reports, index), block) => {
        let (s, pending) =
          switch (block) {
          | Assume(s, _) => (s, None)
          | Construct(s, tactic) => (s, Some(tactic))
          };
        let (ctx, errors) = check_signature(ctx, s);
        (
          ctx,
          [
            {
              index,
              errors,
              pending,
            },
            ...reports,
          ],
          index + 1,
        );
      },
      (ctx, [], 0),
      d,
    );
  (ctx, List.rev(reports));
};

let all_errors = (reports: list(report)): list(BbError.located) =>
  List.concat_map(r => r.errors, reports);
