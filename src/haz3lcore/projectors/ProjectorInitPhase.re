open Util;

/* Resolves every projector in a program to a settled model and its expansion —
 * the second half of instantiating a projector, after `init` left a placeholder
 * at parse time (see ProjectorBase: init / effect / update / expand). For each
 * projector, while `effect(model)` is Some it runs the requested IO (run_io) and
 * folds the result back through `update`, looping until the model settles
 * (effect = None), then reports the final model and `expand(model)`.
 *
 * Frontends share the projector methods + run_io and differ only in *driving*:
 * the CLI calls [resolve] once after parse (this fixpoint loop), the web fires
 * per calculate cycle (ProjectorView.run_init_phase). [on_complete] fires once
 * every projector has settled (synchronously when no async IO is pending — the
 * CLI fast path for local files / seeds). */

/* Interpret an io_request against the frontend-installed hooks. GADT-typed, so
 * each request hands back exactly its own result type (no stringly plumbing): a
 * url fetch -> result(string, string), a seed choice -> int. The actual IO
 * (network, prompt, entropy) lives in the installed UrlFetch / SeedChoose hooks,
 * not here. */
let run_io = (type r, req: ProjectorBase.io_request(r), ~k: r => unit): unit =>
  switch (req) {
  | FetchUrl(url) => UrlFetch.get^(~url, ~on_done=k)
  | ChooseSeed(default) => k(SeedChoose.choose^(~default))
  };

let resolve =
    (
      ~proj_map: Id.Map.t(Base.projector),
      ~mk_info: Base.projector => ProjectorBase.info,
      ~on_result:
         (Id.t, ProjectorCore.Kind.t, string, option(Language.Exp.t)) => unit,
      ~on_complete: unit => unit,
    )
    : unit => {
  /* Count each in-flight effect; [on_complete] is gated on [dispatched] so it
   * can only fire after every projector's first effect has been dispatched, even
   * when some IO completed synchronously. */
  let remaining = ref(0);
  let dispatched = ref(false);
  let finish = () =>
    if (dispatched^ && remaining^ == 0) {
      on_complete();
    };
  /* Drive one projector from a (serialized) model to settled, then report it. */
  let rec drive =
          (
            id: Id.t,
            p: Base.projector,
            info: ProjectorBase.info,
            model: string,
          ) => {
    let (module P) = ProjectorInit.to_module(p.kind);
    switch (P.effect(model)) {
    | None => on_result(id, p.kind, model, P.expand(model, info))
    | Some(Await(req, fold)) =>
      incr(remaining);
      run_io(
        req,
        ~k=result => {
          let model' = P.update(model, info, fold(result));
          decr(remaining);
          drive(id, p, info, model');
          finish();
        },
      );
    };
  };
  List.iter(
    ((id, p: Base.projector)) => drive(id, p, mk_info(p), p.model),
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
