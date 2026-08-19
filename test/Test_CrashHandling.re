open Alcotest;
open Web;

/* `CrashHandling` is the app's last line of defence: if a `calculate` pass
 * raises, the user must keep the model they had rather than lose their work,
 * and the failure must be recorded so the UI can offer to revert or rethrow.
 *
 * That fallback used to be inlined in `Update.calculate`, which meant reaching
 * it from a test required building a whole `Logged.Model.t` and provoking a
 * crash inside it. `Update.guard_calculate` is the same code with the model
 * abstracted out, so the behaviour can be checked directly. */

let reset = () => {
  CrashHandling.clear_last_exception();
  CrashHandling.clear_current_exception();
};

let recorded_message = () =>
  switch (CrashHandling.current_exception^) {
  | Some(Calculate(msg)) => Some(msg)
  | Some(Update(_))
  | Some(View(_))
  | None => None
  };

exception Boom;

let tests = (
  "CrashHandling",
  [
    test_case(
      "a successful pass returns its own result",
      `Quick,
      () => {
        reset();
        let result =
          CrashHandling.Update.guard_calculate(~previous=1, () => 2);
        check(int, "result", 2, result);
        check(bool, "nothing recorded", true, recorded_message() == None);
        check(
          bool,
          "no exception stored",
          true,
          CrashHandling.last_exception^ == None,
        );
        reset();
      },
    ),
    /* The property that matters: a crash must not take the previous model with
       it. If this regresses, a single bad frame discards the user's work. */
    test_case(
      "a raising pass falls back to the previous model",
      `Quick,
      () => {
        reset();
        let result =
          CrashHandling.Update.guard_calculate(~previous=1, () =>
            raise(Boom)
          );
        check(int, "previous model is kept", 1, result);
        reset();
      },
    ),
    /* The UI's "revert" and "rethrow" buttons are driven off these two refs, so
       a crash that fails to record leaves the user with no way out. */
    test_case(
      "a raising pass records the failure as Calculate",
      `Quick,
      () => {
        reset();
        let _ =
          CrashHandling.Update.guard_calculate(~previous=0, () =>
            raise(Boom)
          );
        check(
          bool,
          "current_exception is a Calculate",
          true,
          recorded_message() != None,
        );
        check(
          bool,
          "last_exception is stored for rethrow",
          true,
          CrashHandling.last_exception^ != None,
        );
        reset();
      },
    ),
    test_case(
      "clearing resets both refs",
      `Quick,
      () => {
        reset();
        let _ =
          CrashHandling.Update.guard_calculate(~previous=0, () =>
            raise(Boom)
          );
        reset();
        check(
          bool,
          "current_exception cleared",
          true,
          recorded_message() == None,
        );
        check(
          bool,
          "last_exception cleared",
          true,
          CrashHandling.last_exception^ == None,
        );
      },
    ),
  ],
);
