/* The scope a theorem's proof is checked in, at the env `env` where
 * evaluation reached the theorem: the theorems in its namespace, the builtin
 * axioms, and the verdicts of the theorems checked before it. */
let scope =
    (
      ~env: Environment.t(Exp.t),
      ~verdicts: list((Id.t, SemanticCtx.verdict)),
      namespace: ProofStatics.namespace,
    )
    : SemanticCtx.t =>
  SemanticCtx.of_ctx_and_env(
    ~captured=namespace.captured,
    {
      ...Ctx.empty,
      entries:
        List.map(~f=entry => Ctx.TheoremEntry(entry), namespace.theorems)
        @ List.rev_map(
            ~f=entry => Ctx.TheoremEntry(entry),
            Statics.initial_theorems,
          ),
    },
    env,
  )
  |> SemanticCtx.with_verdicts(verdicts);

/* Whether a proof cites the same theorems in either namespace. */
let same = (a: ProofStatics.namespace, b: ProofStatics.namespace): bool =>
  List.equal(Id.equal, a.captured, b.captured)
  && List.equal(
       (x: Ctx.theorem_entry, y: Ctx.theorem_entry) =>
         String.equal(x.name, y.name)
         && Id.equal(x.id, y.id)
         && Option.equal(Exp.fast_equal_with_lexemes, x.prop, y.prop),
       a.theorems,
       b.theorems,
     );
