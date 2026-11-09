open Util;
open Language;

/* The stepper's rows, read off the proof syntax and the checker's
 * ProofMap. Nothing here is stored: the view and the state pass walk the
 * proof again each time.
 *
 * Each step of a `;` chain is a row, showing the goal the step starts
 * from. A chain that ends in a step gets one more row, the step's
 * outgoing goal, where the proof continues. A form's body, an
 * induction's cases and a have's two premises are chains of their own,
 * nested in the form's row. */

[@deriving (show({with_path: false}), sexp, yojson)]
type side =
  | Incoming
  | Outgoing;

/* A step's own row is keyed by the step's id on the incoming side. The
 * row after a chain's last step is keyed by that step's id on the
 * outgoing side. */
[@deriving (show({with_path: false}), sexp, yojson)]
type key = (Id.t, side);

type t = {
  key,
  /* The step the row shows, or for an `End` row the step it follows. */
  step: Proof.t,
  kind,
  /* The checker's entry for `step`; None until the proof is checked. */
  entry: option(ProofMap.entry),
  /* The goal the row shows, as the checker wrote it. None when there is
   * none to show: nothing checked yet, or an earlier step broke. */
  goal: option(Exp.t),
}
and kind =
  /* A written `?` or unparseable text. A step picked here replaces it. */
  | Hole
  /* A step. A step picked here goes in before it. */
  | Step(nested)
  /* Past the chain's last step. A step picked here extends the chain. */
  | End
and nested =
  | Leaf
  /* The body of forall, funext, assume, revert, generalize or alias. */
  | Body(list(t))
  | Cases(list((Pat.t, list(t))))
  | Premises({
      sub: list(t),
      body: list(t),
    });

/* Grouping parens are transparent: `( funext x => ? )` is a funext row. */
let rec unparen = (p: Proof.t): Proof.t =>
  switch (p.term) {
  | Parens(inner) => unparen(inner)
  | _ => p
  };

/* The step a proof starts with, and the rest of its `;` chain. */
let rec split = (p: Proof.t): (Proof.t, option(Proof.t)) => {
  let p = unparen(p);
  switch (p.term) {
  | Seq(head, tail) =>
    switch (unparen(head)) {
    /* Inserting a step before another writes `Seq(Seq(new, old), tail)`
     * until a reparse flattens it, so re-associate it to keep the head a
     * single step. The rebuilt node reuses the inner Seq's ids, which no
     * row reads. */
    | {term: Seq(a, b), _} as inner =>
      split({
        ...p,
        term:
          Seq(
            a,
            {
              ...inner,
              term: Seq(b, tail),
            },
          ),
      })
    | head => (head, Some(tail))
    }
  | _ => (p, None)
  };
};

/* The rows of the chain `proof`. */
let rec chain = (~proof_map: ProofMap.t, proof: Proof.t): list(t) => {
  let (step, tail) = split(proof);
  let entry = ProofMap.lookup(Proof.rep_id(step), proof_map);
  let goal =
    switch (entry) {
    /* After any automatic steps the checker took first. */
    | Some({incoming: Some(incoming), auto_incoming, _}) =>
      Some(List.fold_left((_, (_, e)) => e, incoming, auto_incoming))
    | Some({incoming: None, _})
    | None => None
    };
  let nested = chain(~proof_map);
  let row = kind => {
    key: (Proof.rep_id(step), Incoming),
    step,
    kind,
    entry,
    goal,
  };
  let row =
    switch (step.term) {
    | EmptyHole
    | Invalid(_)
    | MultiHole(_) => row(Hole)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | EvalStep(_)
    | Contradiction(_) => row(Step(Leaf))
    | Forall(_, body)
    | Funext(_, body)
    | Assume(_, _, body)
    | Revert(_, _, body)
    | Generalize(_, body)
    | Alias(_, _, body) => row(Step(Body(nested(body))))
    | Induction(_, _, cases) =>
      row(
        Step(
          Cases(List.map(((pat, body)) => (pat, nested(body)), cases)),
        ),
      )
    | Have(_, sub, body) =>
      row(
        Step(
          Premises({
            sub: nested(sub),
            body: nested(body),
          }),
        ),
      )
    /* `split` leaves neither at the head. */
    | Parens(_)
    | Seq(_, _) => row(Step(Leaf))
    };
  switch (tail, row.kind) {
  | (Some(tail), _) => [row, ...chain(~proof_map, tail)]
  /* A hole at the end already stands for "the proof continues here". */
  | (None, Hole) => [row]
  | (None, Step(_) | End) => [
      row,
      {
        key: (Proof.rep_id(step), Outgoing),
        step,
        kind: End,
        entry,
        goal: Option.bind(entry, (e: ProofMap.entry) => e.outgoing),
      },
    ]
  };
};

/* Every row, nested chains included. */
let rec all = (rows: list(t)): list(t) =>
  List.concat_map(
    row =>
      [
        row,
        ...switch (row.kind) {
           | Step(Body(rows)) => all(rows)
           | Step(Cases(cases)) =>
             List.concat_map(((_, c)) => all(c), cases)
           | Step(Premises({sub, body})) => all(sub) @ all(body)
           | Step(Leaf)
           | Hole
           | End => []
           },
      ],
    rows,
  );

/* The goal a chain's rows reach: its last step's outgoing goal. A
 * trailing hole passes its incoming goal through. */
let reached = (rows: list(t)): option(Exp.t) =>
  switch (ListUtil.last_opt(rows)) {
  | Some({kind: End, goal, _}) => goal
  | Some({kind: Hole | Step(_), entry, _}) =>
    Option.bind(entry, (e: ProofMap.entry) => e.outgoing)
  | None => None
  };

/* The scope a step written at the row is checked in. */
let ctx = (row: t): option(SemanticCtx.t) =>
  Option.map((e: ProofMap.entry) => e.ctx, row.entry);

type row = t;

/* A row's status, from the marks the checker recorded for its step. */
module Status = {
  type t =
    /* Checked, with no marks. Says nothing about whether the goal is
     * proven. */
    | Clean
    /* Clean, but the step's own arguments hold a hole: an induction
     * case's pattern or scrutinee, a forall binder, an axiom target.
     * Holes in nested proofs don't count: each has a row of its own. */
    | Incomplete
    /* At fault: the highest-priority mark. */
    | Broken(ProofMark.t)
    /* An earlier row is at fault, so this one has no incoming goal. */
    | Blocked
    /* Not checked yet. */
    | Pending;

  let of_row = (row: row): t =>
    switch (row.kind, row.entry) {
    /* An `End` row has no step of its own to be at fault. */
    | (End, _) => Clean
    | (Hole | Step(_), None) => Pending
    | (Hole | Step(_), Some({marks, _})) =>
      let blocked = List.mem(ProofMark.MissingIncoming, marks);
      switch (blocked, ProofMark.highest(marks), row.kind) {
      | (true, _, _) => Blocked
      | (false, Some(m), _) => Broken(m)
      | (false, None, Step(_)) when Proof.args_have_hole(row.step) =>
        Incomplete
      | (false, None, _) => Clean
      };
    };

  let broken_mark = (status: t): option(ProofMark.t) =>
    switch (status) {
    | Broken(mark) => Some(mark)
    | Clean
    | Incomplete
    | Blocked
    | Pending => None
    };
};
