open Util;
open Language;

/* The stepper's rows, read off the proof syntax and the checker's
 * ProofMap. Nothing here is stored: the view and the state pass walk the
 * proof again each time.
 *
 * Each step of a `;` chain is a row, showing the goal the step starts
 * from. A chain that ends in a step gets one more row, the step's
 * outgoing goal, where the proof continues. A form's body, an
 * induction's cases, a have's two premises and a focus's steps are
 * chains of their own, nested in the form's row. A focus of one step is
 * drawn as that step, so it nests nothing. */

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
  /* The id in `goal` of the part the step acts on: the occurrence a focus
   * selected, or all of `goal` for a step that rewrites it whole. None
   * for a step that changes the goal's shape or closes it from facts
   * (forall, induction, contradiction, ...). */
  acts_on: option(Id.t),
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
    })
  /* The steps of a focus of more than one step. */
  | Focus(list(t));

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

let is_alias = (step: Proof.t) =>
  switch (step.term) {
  | Alias(_) => true
  | _ => false
  };

/* The one step of a focus drawn as that step's line: a step with no
 * nested rows of its own. */
let single_step = (focus: Proof.t): option(Proof.t) =>
  switch (focus.term) {
  | Focus({body, _}) =>
    let inner = unparen(body);
    switch (inner.term) {
    | AxiomStep(_)
    | AlgebriteStep(_)
    | EvalStep
    | Contradiction(_) => Some(inner)
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | Seq(_)
    | Parens(_)
    | Focus(_)
    | Induction(_)
    | Forall(_)
    | Funext(_)
    | Assume(_)
    | Alias(_)
    | Generalize(_)
    | Revert(_)
    | Have(_) => None
    };
  | _ => None
  };

/* The rows of the chain `proof`. */
let rec chain = (~proof_map: ProofMap.t, proof: Proof.t): list(t) => {
  let (step, tail) = split(proof);
  let lookup = p => ProofMap.lookup(Proof.rep_id(p), proof_map);
  /* A one-step focus's row is its step's: the step's marks and side
   * conditions, unless the focus itself is at fault. */
  let entry =
    switch (lookup(step), Option.bind(single_step(step), lookup)) {
    | (Some(focus), Some(inner)) =>
      Some({
        ...focus,
        marks: focus.marks == [] ? inner.marks : focus.marks,
        obligations: focus.obligations @ inner.obligations,
      })
    | (entry, _) => entry
    };
  let goal =
    switch (entry) {
    /* After any automatic steps the checker took first. */
    | Some({incoming: Some(incoming), auto_incoming, _}) =>
      Some(List.fold_left((_, (_, e)) => e, incoming, auto_incoming))
    | Some({incoming: None, _})
    | None => None
    };
  let nested = chain(~proof_map);
  let acts_on =
    switch (step.term) {
    /* The checker starts a focus's body from the occurrence it selected. */
    | Focus({body, _}) =>
      lookup(body)
      |> Option.bind(_, (e: ProofMap.entry) => e.incoming)
      |> Option.map(Exp.rep_id)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | EvalStep => Option.map(Exp.rep_id, goal)
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | Seq(_)
    | Parens(_)
    | Contradiction(_)
    | Forall(_)
    | Funext(_)
    | Assume(_)
    | Revert(_)
    | Generalize(_)
    | Have(_)
    | Alias(_)
    | Induction(_) => None
    };
  let row = kind => {
    key: (Proof.rep_id(step), Incoming),
    step,
    kind,
    entry,
    goal,
    acts_on,
  };
  let row =
    switch (step.term) {
    | EmptyHole
    | Invalid(_)
    | MultiHole(_) => row(Hole)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | EvalStep
    | Contradiction(_) => row(Step(Leaf))
    | Focus({body, _}) =>
      row(Step(single_step(step) == None ? Focus(nested(body)) : Leaf))
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
  /* `have` and `alias` keep their goal, so their body's own last row is
   * where the proof continues. */
  | (None, Step(Premises(_))) => [row]
  | (None, Step(Body(_))) when is_alias(step) => [row]
  | (None, Step(_) | End) => [
      row,
      {
        key: (Proof.rep_id(step), Outgoing),
        step,
        kind: End,
        entry,
        goal: Option.bind(entry, (e: ProofMap.entry) => e.outgoing),
        acts_on: None,
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
           | Step(Focus(rows)) => all(rows)
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

/* The side conditions a step row shows: the obligations its step
 * incurs. A have's two premises are drawn as its own goals, the sub-proof
 * arm and the goal continuing below it, so it shows none. */
let side_conditions = (row: t): list(Obligation.t) =>
  switch (row.kind, row.entry) {
  | (Step(Premises(_)), _)
  | (Hole | End, _)
  | (Step(_), None) => []
  | (Step(Leaf | Body(_) | Cases(_) | Focus(_)), Some(entry)) =>
    entry.obligations
  };

/* The rows that carry on a step's own goal. `have` and `alias` keep the
 * goal, so their body reads as the rest of the outer chain. */
let continuation = (row: t): option(list(t)) =>
  switch (row.kind) {
  | Step(Premises({body, _})) => Some(body)
  | Step(Body(rows)) when is_alias(row.step) => Some(rows)
  | Step(Leaf | Body(_) | Cases(_) | Focus(_))
  | Hole
  | End => None
  };

/* How a step is drawn between its row and the next. */
type layout =
  /* A line, with the step's lozenge on it. */
  | Line
  /* The line opened into a block: the side conditions shown open, by
   * index into `side_conditions`, then, with `arms`, the goals the step
   * creates. */
  | Block({
      conditions: list(int),
      arms: bool,
    });

/* `expanded` is the indices of the side conditions the user opened. */
let layout = (row: t, ~expanded: list(int)): layout => {
  let count = List.length(side_conditions(row));
  let conditions =
    List.sort_uniq(compare, expanded) |> List.filter(i => i >= 0 && i < count);
  let arms =
    switch (row.kind, continuation(row)) {
    | (Step(Body(_)), Some(_)) => false
    | (Step(Body(_) | Cases(_) | Premises(_) | Focus(_)), _) => true
    | (Step(Leaf) | Hole | End, _) => false
    };
  conditions == [] && !arms
    ? Line
    : Block({
        conditions,
        arms,
      });
};

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
      | (false, None, Step(_))
          when
            Proof.args_have_hole(row.step)
            || Option.fold(
                 ~none=false,
                 ~some=Proof.args_have_hole,
                 single_step(row.step),
               ) =>
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
