/* The Hazel-source standard library (plans/stdlib.md), checked and
   elaborated at build time by src/stdlib/bake; each entry point installs
   the result at startup. */

/* the baked form: written by bake, read here; Marshal, so main thread,
   worker and baker must come from one build */
type baked = {
  /* of the concatenated source text */
  hash: string,
  /* what the library adds to the context, newest first */
  entries: list(Ctx.entry),
  /* each top-level definition's elaboration, in order, one shared id
     throughout (install re-ids it). Evaluated one at a time, so stack
     depth stays per definition however long the library grows */
  defs: list((Var.t, Exp.t)),
};

/* [blob]: Marshal'd, not base64 */
let decode = (blob: string): baked => Marshal.from_string(blob, 0);

/* the elaborations with fresh ids, as evaluation results get */
let reid = (b: baked): baked => {
  ...b,
  defs: List.map(((x, def)) => (x, Exp.replace_all_ids(def)), b.defs),
};

/* [b] re-id'd */
let install_baked = (b: baked): unit =>
  Builtins.install_library(
    ~entries=b.entries,
    ~env=
      List.fold_left(
        (env, (x, def)) =>
          Environment.extend(env, (x, Evaluator.evaluate_raw(~env, def))),
        Builtins.env_base,
        b.defs,
      ),
  );

let install = (blob: string): unit => install_baked(reid(decode(blob)));
