open Language;

/* OutlineTree — the module/definition tree behind the outline sidebar
   (plans/modular-editors.md §1). Walks the program term: top-level
   definitions, module members (recursively), and let-in / type-in
   definitions inside function bodies. Every node carries a jump id. */

type kind =
  | KModule
  | KFn
  | KConst
  | KType;

type node = {
  o_label: string,
  o_kind: kind,
  o_id: option(Id.t),
  o_children: list(node),
};

let rec strip_exp = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(e)
  | Projector(_, e)
  | Filter(_, e) => strip_exp(e)
  | _ => e
  };

/* the binding's display name: first Var through wrappers, incl. the
   funlet head */
let rec pat_name = (p: Pat.t): option(string) =>
  switch (p.term) {
  | Var(x) => Some(x)
  | Parens(p)
  | Asc(p, _)
  | Projector(_, p)
  | TupLabel(_, p) => pat_name(p)
  | Ap(f, _) => pat_name(f)
  | Tuple([p, ..._]) => pat_name(p)
  | _ => None
  };

let rec of_exp = (e: Exp.t): list(node) => {
  let e = strip_exp(e);
  switch (e.term) {
  | Let(pat, def, body) =>
    let entry =
      switch (pat_name(pat)) {
      | Some(name) => [mk_def(~id=Exp.rep_id(e), name, def)]
      | None => []
      };
    entry @ of_exp(body);
  | TyAlias(tpat, _, body) =>
    let entry =
      switch (tpat.term) {
      | Var(name) => [
          {
            o_label: name,
            o_kind: KType,
            o_id: Some(Exp.rep_id(e)),
            o_children: [],
          },
        ]
      | _ => []
      };
    entry @ of_exp(body);
  | ModuleExp(mpat, def, body) =>
    let entry =
      switch (mpat.term) {
      | Var(name) => [mk_def(~id=Exp.rep_id(e), name, def)]
      | _ => []
      };
    entry @ of_exp(body);
  | Seq(_, body) => of_exp(body)
  | _ => []
  };
}

and mk_def = (~id: Id.t, name: string, def: Exp.t): node => {
  let def = strip_exp(def);
  switch (def.term) {
  | Module(items) => {
      o_label: name,
      o_kind: KModule,
      o_id: Some(id),
      o_children: of_mod(items),
    }
  | Fun(_, fbody, _, _)
  | TypFun(_, {term: Fun(_, fbody, _, _), _}, _) => {
      o_label: name,
      o_kind: KFn,
      o_id: Some(id),
      o_children: of_exp(fbody),
    }
  | _ => {
      o_label: name,
      o_kind: KConst,
      o_id: Some(id),
      o_children: of_exp(def),
    }
  };
}

and of_mod = (items: list(Language.Mod.t)): list(node) =>
  List.concat_map(
    (m: Language.Mod.t) =>
      switch (m.term) {
      | ModLet(pat, def) =>
        switch (pat_name(pat)) {
        | Some(name) => [mk_def(~id=Language.Mod.rep_id(m), name, def)]
        | None => []
        }
      | ModType(tpat, _) =>
        switch (tpat.term) {
        | Var(name) => [
            {
              o_label: name,
              o_kind: KType,
              o_id: Some(Language.Mod.rep_id(m)),
              o_children: [],
            },
          ]
        | _ => []
        }
      | ModuleMod(mpat, def) =>
        switch (mpat.term) {
        | Var(name) => [mk_def(~id=Language.Mod.rep_id(m), name, def)]
        | _ => []
        }
      | ModExp(_)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_) => []
      },
    items,
  );

/* memoized on the term's PHYSICAL identity: statics rebuilds the term
   only when the program changes, so between edits (and on every
   render while a focus stack is open) this is a pointer compare —
   the unmemoized walk was O(program) per keystroke */
let cache: ref(option((Exp.t, list(node)))) = ref(None);

let of_term = (e: Exp.t): list(node) =>
  switch (cache^) {
  | Some((prev, tree)) when prev === e => tree
  | _ =>
    let tree = of_exp(e);
    cache := Some((e, tree));
    tree;
  };
