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
let env_of = (defs: string): Environment.t(Exp.t) => {
  let (env, modules) =
    List.fold_left(
      ((env, modules), (x, def)) => {
        let v = Evaluator.evaluate_raw(~env, Exp.replace_all_ids(def));
        (
          Environment.extend(env, (x, v)),
          switch (v.term) {
          | Module([item, ..._]) => [(x, Mod.rep_id(item)), ...modules]
          | _ => modules
          },
        );
      },
      (Builtins.env_base, []),
      Marshal.from_string(defs, 0): list((Var.t, Exp.t)),
    );
  Builtins.library_modules := modules;
  env;
};

let install_baked = (b: baked): unit =>
  Builtins.install_library(~entries=b.entries, ~env=lazy(env_of(b.defs)));

let install = (blob: string): unit => install_baked(decode(blob));

/* [v] with each library module in it named instead, for shipping and
   showing (a sample of `List` is the library's whole List otherwise) */
let name_modules = (v: Exp.t): Exp.t =>
  Builtins.library_modules^ == []
    ? v
    : Exp.map_term(
        ~f_exp=
          (cont, e: Exp.t) =>
            switch (e.term) {
            | Module(_) =>
              switch (Builtins.library_module_name(e)) {
              | Some(x) => {
                  ...e,
                  term: Var(x),
                }
              | None => cont(e)
              }
            | _ => cont(e)
            },
        v,
      );
