/* Statics for the builtins whose result type depends on the labels written at
   the call site, and so cannot be given as an ordinary arrow type:
   `project_labels`, `select_labels`, `omit_labels`, `omit_all_labels`,
   `group_by_label` and `to_lvs`, all defined in BuiltinsTupleOperations.
   Each carries a `Ctx.custom_statics` tag on its context entry, and that tag
   selects the rule applied here.

   Both functions take the enclosing statics as a module rather than calling
   Statics directly, because Statics is what calls them. */

let custom_statics_deferred_ap:
  (
    ~ctx: Ctx.t,
    ~ancestors: list(Id.t),
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
