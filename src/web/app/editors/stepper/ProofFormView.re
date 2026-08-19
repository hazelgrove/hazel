open Util;
open Language;

/* Shared rendering for the wrapping proof forms (`assume` / `revert` /
 * `generalize`): the keyword label plus the form's expression argument,
 * read-only, in the same inline-chip wrapper the other step rows use for
 * their arguments (cf. InductionStep's "Induction on:" scrutinee).
 *
 * Read-only on purpose: the expression is a one-shot choice made when the
 * step is inserted, and the authoritative copy lives in the proof text —
 * so it is displayed here, not re-edited (unlike the induction scrutinee,
 * which is edited constantly while cases are built). */
let view_arg =
    (~globals: Globals.t, ~label: string, arg: option(Exp.t))
    : list(WebUtil.Node.t) =>
  switch (arg) {
  | None => []
  | Some(e) => [
      WebUtil.div_c(
        "proof-form-arg",
        [
          WebUtil.Node.text(label),
          WebUtil.div_c(
            "inline-editor-wrapper",
            [
              CodeViewable.view_any(
                ~globals,
                ~settings=
                  Haz3lcore.ExpToSegment.Settings.of_core(
                    ~inline=true,
                    ~fold_fn_bodies=`Text,
                    globals.settings.core,
                  ),
                Exp(e),
              ),
            ],
          ),
        ],
      ),
    ]
  };
