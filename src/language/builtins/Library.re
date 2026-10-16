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
  /* Marshal of each top-level definition's elaboration, in order, one
     shared id throughout (install re-ids it). Decoded and evaluated only
     when something first evaluates: statics need just the entries */
  defs: string,
};

/* [blob]: Marshal'd, not base64 */
let decode = (blob: string): baked => Marshal.from_string(blob, 0);

/* the definitions evaluated in order, each on its own, so stack depth
   stays per definition however long the library grows; fresh ids, as
   evaluation results get */
let env_of = (defs: string): Environment.t(Exp.t) =>
  List.fold_left(
    (env, (x, def)) =>
      Environment.extend(
        env,
        (x, Evaluator.evaluate_raw(~env, Exp.replace_all_ids(def))),
      ),
    Builtins.env_base,
    Marshal.from_string(defs, 0): list((Var.t, Exp.t)),
  );

let install_baked = (b: baked): unit =>
  Builtins.install_library(~entries=b.entries, ~env=lazy(env_of(b.defs)));

let install = (blob: string): unit => install_baked(decode(blob));
