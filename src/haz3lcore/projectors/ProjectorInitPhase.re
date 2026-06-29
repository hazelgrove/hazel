open Util;

/* Runs the optional post-parse `initialize` phase of every projector in a
 * program (see ProjectorBase.Projector.initialize). Each frontend calls this
 * once after parse + statics: the web from its calculate cycle, the CLI from
 * its run/analyze/expand entry points. Projectors that need asynchronous
 * resolution (e.g. CSV fetching a url) report it via their bool return, and
 * the driver invokes [on_complete] only after every such projector has called
 * back. With no async work [on_complete] fires synchronously (the CLI fast
 * path when there are no url projectors).
 *
 * [on_result] receives, per resolved projector, an optional new serialized
 * model (apply like a SetModel) and the resolved expansion (Some exp on
 * success, None on failure). A frontend either substitutes the exp into the
 * program term in place (see [substitute] / the CLI) or lifts it into editor
 * syntax (the web). */
let run =
    (
      ~proj_map: Id.Map.t(Base.projector),
      ~mk_info: Base.projector => ProjectorBase.info,
      ~on_result:
         (
           Id.t,
           ProjectorCore.Kind.t,
           option(string),
           option(Language.Exp.t)
         ) =>
         unit,
      ~on_complete: unit => unit,
    )
    : unit => {
  /* Optimistically count each projector as pending before calling its
   * initializer, so a synchronous callback can't drive the counter negative;
   * undo the count for projectors that report no async work. [on_complete] is
   * gated on [dispatched] so it can only fire after the whole map has been
   * walked, even when some callbacks ran synchronously. */
  let remaining = ref(0);
  let dispatched = ref(false);
  let finish = () =>
    if (dispatched^ && remaining^ == 0) {
      on_complete();
    };
  List.iter(
    ((id, p: Base.projector)) => {
      let (module P) = ProjectorInit.to_module(p.kind);
      switch (P.initialize) {
      | None => ()
      | Some(f) =>
        let info = mk_info(p);
        incr(remaining);
        let started =
          f(
            p.model,
            info,
            ~k=(model_opt, exp_opt) => {
              on_result(id, p.kind, model_opt, exp_opt);
              decr(remaining);
              finish();
            },
          );
        if (!started) {
          decr(remaining);
        };
      };
    },
    Id.Map.bindings(proj_map),
  );
  dispatched := true;
  finish();
};

/* Substitute resolved expansions into a program term, in place: each projector
 * node whose id is in [exps] is replaced by its resolved Exp, exactly where the
 * projector sat — dropping the projector wrapper, since the expansion *is* the
 * meaning for evaluation (an Exp the elaborator/evaluator treats as ordinary
 * syntax). No synthetic bindings, no segment, no re-parse — the payload was
 * built straight as an Exp. Projector ids absent from [exps] (no initialize, or
 * resolution failed) are left untouched for the evaluator to handle as before. */
let substitute =
    (exps: Id.Map.t(Language.Exp.t), term: Language.Exp.t): Language.Exp.t =>
  Language.Exp.map_term(
    ~f_exp=
      (continue, e) =>
        switch (Language.Exp.term_of(e)) {
        | Projector(_) =>
          switch (Id.Map.find_opt(Language.Exp.rep_id(e), exps)) {
          | Some(exp) => exp
          | None => continue(e)
          }
        | _ => continue(e)
        },
    term,
  );
