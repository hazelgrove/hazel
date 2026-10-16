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
  /* its top-level names, in the order elab's tuple holds their values */
  names: list(Var.t),
  /* one shared id throughout: install re-ids it */
  elab: Exp.t,
};

/* [blob]: Marshal'd, not base64 */
let decode = (blob: string): baked => Marshal.from_string(blob, 0);

/* the elab with fresh ids, as evaluation results get */
let reid = (b: baked): baked => {
  ...b,
  elab: Exp.replace_all_ids(b.elab),
};

/* [b] re-id'd */
let install_baked = (b: baked): unit => {
  let values =
    switch (Evaluator.evaluate_raw(~env=Builtins.env_base, b.elab).term) {
    | Tuple(values) when List.length(values) == List.length(b.names) => values
    | _ => failwith("stdlib: the library did not evaluate to its exports")
    };
  let env =
    List.fold_left2(
      (env, name, value) => Environment.extend(env, (name, value)),
      Builtins.env_base,
      b.names,
      values,
    );
  Builtins.install_library(~entries=b.entries, ~env);
};

let install = (blob: string): unit => install_baked(reid(decode(blob)));
