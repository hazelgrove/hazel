open Util;
open Language;
open Poly;

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
  /* Whether a step can be written at the row, in its write box: in place
   * of a hole, or after the chain's last step where the proof goes on. */
  continues: bool,
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
    | Cases(_)
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

/* A form proving a goal of its own (forall, assume, a split, ...) passes
 * its goal through until its body closes it, so the proof doesn't go on
 * after it. */
let passes_goal_through = (step: Proof.t): bool =>
  switch (unparen(step).term) {
  | Forall(_)
  | Funext(_)
  | Assume(_)
  | Revert(_)
  | Generalize(_)
  | Cases(_)
  | Induction(_) => true
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | Seq(_)
  | Parens(_)
  | AxiomStep(_)
  | AlgebriteStep(_)
  | EvalStep
  | Focus(_)
  | Contradiction(_)
  | Alias(_)
  | Have(_) => false
  };

/* The rows of the chain `proof`. With `open_end` off, a step can't be
 * written after the chain's last one: the chain is a focus's steps, and
 * more steps follow the focus, so a step there would go between rows. */
let rec chain =
        (~proof_map: ProofMap.t, ~open_end=true, proof: Proof.t): list(t) => {
  let (step, tail) = split(proof);
  let lookup = p => ProofMap.lookup(Proof.rep_id(p), proof_map);
  /* A one-step focus's row is its step's: the step's marks and side
   * conditions, unless the focus itself is at fault. */
  let entry =
    switch (lookup(step), Option.bind(single_step(step), ~f=lookup)) {
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
      Some(
        List.fold_left(~f=(_, (_, e)) => e, ~init=incoming, auto_incoming),
      )
    | Some({incoming: None, _})
    | None => None
    };
  let nested = chain(~proof_map, ~open_end=true);
  /* `have` and `alias` keep their goal, so their body is the rest of this
   * chain. */
  let rest = chain(~proof_map, ~open_end);
  let acts_on =
    switch (step.term) {
    /* The checker starts a focus's body from the occurrence it selected. */
    | Focus({body, _}) =>
      lookup(body)
      |> Option.bind(_, ~f=(e: ProofMap.entry) => e.incoming)
      |> Option.map(~f=Exp.rep_id)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | EvalStep => Option.map(~f=Exp.rep_id, goal)
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
    | Cases(_)
    | Induction(_) => None
    };
  let row = kind => {
    key: (Proof.rep_id(step), Incoming),
    step,
    kind,
    entry,
    goal,
    acts_on,
    continues: kind == Hole,
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
      row(
        Step(
          single_step(step) == None
            ? Focus(
                chain(~proof_map, ~open_end=open_end && tail == None, body),
              )
            : Leaf,
        ),
      )
    | Forall(_, body)
    | Funext(_, body)
    | Assume(_, _, body)
    | Revert(_, _, body)
    | Generalize(_, body) => row(Step(Body(nested(body))))
    | Alias(_, _, body) => row(Step(Body(rest(body))))
    | Cases(_, cases)
    | Induction(_, cases) =>
      row(
        Step(
          Cases(
            List.map(~f=((pat, body)) => (pat, nested(body)), cases),
          ),
        ),
      )
    | Have(_, _, sub, body) =>
      row(
        Step(
          Premises({
            sub: nested(sub),
            body: rest(body),
          }),
        ),
      )
    /* `split` leaves neither at the head. */
    | Parens(_)
    | Seq(_, _) => row(Step(Leaf))
    };
  switch (tail, row.kind) {
  | (Some(tail), _) => [row, ...chain(~proof_map, ~open_end, tail)]
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
        goal: Option.bind(entry, ~f=(e: ProofMap.entry) => e.outgoing),
        acts_on: None,
        continues: open_end && !passes_goal_through(step),
      },
    ]
  };
};

/* The rows of a theorem's proof. A proof that reads Refuted can't go on,
 * as one at `true` can't, so none of its rows continues. */
let of_theorem = (~proof_map: ProofMap.t, proof: Proof.t): list(t) => {
  let rec closed = (rows: list(t)): list(t) =>
    List.map(
      ~f=
        row =>
          {
            ...row,
            continues: false,
            kind:
              switch (row.kind) {
              | Step(Body(rows)) => Step(Body(closed(rows)))
              | Step(Cases(cases)) =>
                Step(
                  Cases(
                    List.map(
                      ~f=((pat, rows)) => (pat, closed(rows)),
                      cases,
                    ),
                  ),
                )
              | Step(Premises({sub, body})) =>
                Step(
                  Premises({
                    sub: closed(sub),
                    body: closed(body),
                  }),
                )
              | Step(Focus(rows)) => Step(Focus(closed(rows)))
              | Step(Leaf)
              | Hole
              | End => row.kind
              },
          },
      rows,
    );
  let rows = chain(~proof_map, proof);
  ProofMap.full_status_of_proof(proof_map, proof) == Refuted
    ? closed(rows) : rows;
};

/* Whether the row shows a goal still to prove: the goal a form passes on
 * after its body, until the body proves it. The last row of a focus's
 * chain is the focused part rewritten, which is no goal. */
let is_unproven = (~proof_map: ProofMap.t, row: t): bool =>
  row.kind == End
  && passes_goal_through(row.step)
  && ProofMap.status_of_proof(proof_map, row.step) != Some(true);

/* The name a split's arm binds that the checker refused, as the goal
 * already uses it: the arm's goal would capture that use, so the arm's
 * steps aren't checked. */
let refused_binding = (split: t, pat: Pat.t): option(Var.t) => {
  let bound = pat |> Pat.bindings |> Binding.variable_names;
  Option.value_map(split.entry, ~default=[], ~f=(e: ProofMap.entry) =>
    e.marks
  )
  |> List.find_map(~f=(mark: ProofMark.t) =>
       switch (mark) {
       | BinderCaptures({var}) when List.mem(bound, var, ~equal=String.equal) =>
         Some(var)
       | _ => None
       }
     );
};

/* Every row, nested chains included. */
let rec all = (rows: list(t)): list(t) =>
  List.concat_map(
    ~f=
      row =>
        [
          row,
          ...switch (row.kind) {
             | Step(Body(rows)) => all(rows)
             | Step(Cases(cases)) =>
               List.concat_map(~f=((_, c)) => all(c), cases)
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
    Option.bind(entry, ~f=(e: ProofMap.entry) => e.outgoing)
  | None => None
  };

/* The scope a step written at the row is checked in. */
let ctx = (row: t): option(SemanticCtx.t) =>
  Option.map(~f=(e: ProofMap.entry) => e.ctx, row.entry);

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
    List.dedup_and_sort(~compare=Int.compare, expanded)
    |> List.filter(~f=i => i >= 0 && i < count);
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
      let blocked =
        List.mem(marks, ProofMark.MissingIncoming, ~equal=Poly.equal);
      switch (blocked, ProofMark.highest(marks), row.kind) {
      | (true, _, _) => Blocked
      | (false, Some(m), _) => Broken(m)
      | (false, None, Step(_))
          when
            Proof.args_have_hole(row.step)
            || Option.value_map(
                 single_step(row.step),
                 ~default=false,
                 ~f=Proof.args_have_hole,
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
