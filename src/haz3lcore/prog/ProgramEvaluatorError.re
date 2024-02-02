/* TODO: Change names. */
/**
  The type of an evaluation exception, caught from {!Program}.

  This exists mainly because exceptions on a web worker thread are not
  forwarded back to the main thread.
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | /** A caught {!exception:Program.EvalError}. */
    Program_EvalError(
      EvaluatorError.t,
    )
  | /** A caught {!exception:Program.DoesNotElaborate}. */
    Program_DoesNotElaborate;
