/* The Hazel-source standard library (plans/stdlib.md), checked and
   elaborated at build time by src/stdlib/bake; each entry point installs
   the result at startup. */

/* a top-level definition: its elaboration Marshal'd, with its source's
   ids (the editor's: plans/stdlib.md §9b), decoded and evaluated only when
   a program first needs it */
type def = {
  name: Var.t,
  /* the earlier definitions it mentions, by index */
  deps: list(int),
  elab: string,
};

/* the baked form: written by bake, read here; Marshal, so main thread,
   worker and baker must come from one build */
type baked = {
  /* of the concatenated source text */
  hash: string,
  /* Marshal of what the library adds to the context, newest first */
  entries: string,
  /* in order */
  defs: array(def),
};

/* [blob]: Marshal'd, not base64 */
let decode = (blob: string): baked => Marshal.from_string(blob, 0);

/* evaluated on demand, each definition once and against only the
   definitions it mentions; an env per set of names asked for, so a
   program's stays the same object across its runs */
let env_for_of = (defs: array(def)): (list(Var.t) => Environment.t(Exp.t)) => {
  let values = Array.create(~len=Array.length(defs), None);
  let rec extend = (env, i) =>
    Environment.extend(env, (defs[i].name, value(i)))
  and value = i =>
    switch (values[i]) {
    | Some(v) => v
    | None =>
      let env = List.fold(defs[i].deps, ~init=Builtins.env_base, ~f=extend);
      let v =
        Evaluator.evaluate_raw(~env, Marshal.from_string(defs[i].elab, 0));
      switch (v.term) {
      | Module([item, ..._]) =>
        Builtins.library_modules :=
          [(defs[i].name, Mod.rep_id(item)), ...Builtins.library_modules^]
      | _ => ()
      };
      values[i] = Some(v);
      v;
    };
  let last = Stdlib.Hashtbl.create(Array.length(defs));
  Array.iteri(~f=(i, d) => Stdlib.Hashtbl.replace(last, d.name, i), defs);
  let envs = Stdlib.Hashtbl.create(16);
  names => {
    let is = List.filter_map(~f=Stdlib.Hashtbl.find_opt(last), names);
    let is = List.dedup_and_sort(~compare, is);
    switch (Stdlib.Hashtbl.find_opt(envs, is)) {
    | Some(env) => env
    | None =>
      let env = List.fold(is, ~init=Builtins.env_base, ~f=extend);
      /* bounded: programs with distinct name sets keep coming */
      if (Stdlib.Hashtbl.length(envs) > 256) {
        Stdlib.Hashtbl.reset(envs);
      };
      Stdlib.Hashtbl.replace(envs, is, env);
      env;
    };
  };
};

let install_baked = (b: baked): unit =>
  Builtins.install_library(
    ~entries=lazy(Marshal.from_string(b.entries, 0)),
    ~names=Array.to_list(Array.map(~f=d => d.name, b.defs)),
    ~env_for=env_for_of(b.defs),
  );

let install = (blob: string): unit => install_baked(decode(blob));

/* [v] with each library module in it named instead, for shipping and
   showing (a sample of `List` is the library's whole List otherwise) */
let name_modules = (v: Exp.t): Exp.t =>
  List.is_empty(Builtins.library_modules^)
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
