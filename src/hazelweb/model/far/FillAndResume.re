/* Temporary -- later on try to consolidate these with existing errors */
type error =
  | FailedElaboration;
exception FillAndResumeException(error);

let fill = (e: UHExp.t, u: MetaVar.t, prev_result: Result.t) => {
  /* Get the hole type from the hole context. */
  let (d_result, delta, hci, dr_result) = prev_result;
  let (_, hole_ty, var_ctx) = delta |> MetaVarMap.find(u);
  /* Elaborate the expression in analytic position against the hole type. */
  let (_d, delta) =
    switch (Elaborator_Exp.ana_elab(var_ctx, delta, e, hole_ty)) {
    | Elaborates(d, _ty, delta) => (d, delta)
    | DoesNotElaborate => raise(FillAndResumeException(FailedElaboration))
    };
  /* TODO: Evaluation */
  /* (ec, dr_result) = evaluate(ec, EmptyEnv, d, hci) */
  /* TODO: Postprocessing */
  /* (hci, result) = postprocess(dr) */
  (d_result, delta, hci, dr_result);
};

/* TODO: implement this */
let is_fill_viable = (_old_prog: Program.t, _new_prog: Program.t) => None;
