/* Statics for custom (livelit-provided) application forms. The per-kind
   implementations and the tuple-labelling helpers are private.

   `custom_statics_deferred_ap` ignores `~ancestors`, and its one caller
   (Statics.re:1751) passes a unit-typed pun rather than the
   `ancestors_inclusive` every other call in that file uses -- hence the
   unconstrained type here. Worth reconciling; annotating it list(Id.t)
   does not compile today. */

let custom_statics_deferred_ap:
  (
    ~ctx: Ctx.t,
    ~ancestors: 'a,
    ~fn_info: Info.exp,
    Ctx.custom_statics,
    (module StaticsBase.ExpressionStatics),
    StaticsBase.Map.t,
    list(Exp.t),
    ~elab_term: Exp.t
  ) =>
  (Info.exp, Exp.t, StaticsBase.Map.t);

let custom_statics_ap:
  (
    Ctx.custom_statics,
    (module StaticsBase.ExpressionStatics),
    ~annotation: IdTagged.IdTag.t,
    ~fn_info: Info.exp,
    ~ancestors: list(Id.t),
    ~ctx: Ctx.t,
    StaticsBase.Map.t,
    Exp.t
  ) =>
  (Info.exp, Exp.t, StaticsBase.Map.t);
