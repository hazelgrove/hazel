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
 * model (apply like a SetModel) and an optional replacement syntax (apply
 * like a SetSyntax). */
let run =
    (
      ~proj_map: Id.Map.t(Base.projector),
      ~mk_info: Base.projector => ProjectorBase.info,
      ~on_result:
         (
           Id.t,
           ProjectorCore.Kind.t,
           option(string),
           option(Base.segment)
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
            ~k=(model_opt, seg_opt) => {
              on_result(id, p.kind, model_opt, seg_opt);
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
