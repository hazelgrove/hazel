open Util;

/* Resolves every projector in a program once, reporting each one's settled model
 * and its `expand`. After `init` left a placeholder at parse time (see
 * ProjectorBase: init / resolve / update / expand), this runs each projector's
 * `resolve` at most once: if it yields a resolution, run it (it performs the
 * projector's IO), fold the resulting action through `update`, and report the
 * updated model; otherwise report the model as-is.
 *
 * Frontends share the projector methods and differ only in *driving*: the CLI
 * calls [run] once after parse, the web fires per newly-created projector
 * (ProjectorView.run_init_phase). [on_complete] fires once every projector's
 * resolution has finished (synchronously when no async IO is pending — the CLI
 * fast path for local files / seeds). Resolution is one-shot; a projector that
 * wanted another round would need to be re-driven explicitly. */
let run =
    (
      ~proj_map: Id.Map.t(Base.projector),
      ~mk_info: Base.projector => ProjectorBase.info,
      ~on_result:
         (Id.t, ProjectorCore.Kind.t, string, option(Language.Exp.t)) => unit,
      ~on_complete: unit => unit,
    )
    : unit => {
  /* Count each in-flight resolution; [on_complete] is gated on [dispatched] so it
   * can only fire after every projector has been dispatched, even when some IO
   * completed synchronously. */
  let remaining = ref(0);
  let dispatched = ref(false);
  let finish = () =>
    if (dispatched^ && remaining^ == 0) {
      on_complete();
    };
  /* Resolve one projector once, then report its settled model + expansion. */
  let one = (id: Id.t, p: Base.projector) => {
    let info = mk_info(p);
    let (module P) = ProjectorInit.to_module(p.kind);
    let report = model =>
      on_result(id, p.kind, model, P.expand(model, info));
    switch (P.resolve(p.model)) {
    | None => report(p.model)
    | Some(perform) =>
      incr(remaining);
      perform(action => {
        report(P.update(p.model, info, action));
        decr(remaining);
        finish();
      });
    };
  };
  List.iter(
    ((id, p: Base.projector)) => one(id, p),
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
