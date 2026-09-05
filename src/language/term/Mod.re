[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | ModLet
  | ModType
  | ModExp
  | ModuleMod
  | ModVal;

include TermBase.Mod;

let fast_equal = Equality.syntactic.mod_;
let equal = fast_equal;

let rep_id: t => Id.t = IdTagged.rep_id;

let fresh: term => t = IdTagged.fresh;

let hole = (tms: list(TermBase.Any.t)): TermBase.Mod.term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
  };

let cls_of_term: Grammar.mod_term('a) => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | ModLet(_, _) => ModLet
  | ModType(_, _) => ModType
  | ModExp(_) => ModExp
  | ModuleMod(_) => ModuleMod
  | ModVal(_, _) => ModVal;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid module"
  | MultiHole => "Broken module"
  | EmptyHole => "Module hole"
  | ModLet => "Let declaration"
  | ModType => "Type declaration"
  | ModExp => "Module expression"
  | ModuleMod => "Module declaration"
  | ModVal => "Module binding";

let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
  };

/* ==================== Evaluated bindings (dynamics) ====================
   A module evaluates item by item; each evaluated binding becomes a ModVal
   item. A module whose items are all ModVal is a module value. */

/* Replace the definition carried by an item (used to plug evaluation
   contexts back in). */
let with_def = (item: t, d: TermBase.Exp.t): t =>
  switch (item.term) {
  | ModLet(p, _) => {
      ...item,
      term: (ModLet(p, d): term),
    }
  | ModuleMod(mp, _) => {
      ...item,
      term: (ModuleMod(mp, d): term),
    }
  | ModExp(_) => {
      ...item,
      term: (ModExp(d): term),
    }
  | ModVal(x, _) => {
      ...item,
      term: (ModVal(x, d): term),
    }
  | ModType(_, _)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => item
  };

let is_value_shape = (items: list(t)): bool =>
  List.for_all(
    (item: t) =>
      switch (item.term) {
      | ModVal(_, _) => true
      | _ => false
      },
    items,
  );

/* Split at the first item that is not yet an evaluated binding. */
let split_pending = (items: list(t)): (list(t), option((t, list(t)))) => {
  let rec go = (prefix: list(t), items: list(t)) =>
    switch (items) {
    | [] => (List.rev(prefix), None)
    | [{term: ModVal(_, _), _} as item, ...rest] =>
      go([item, ...prefix], rest)
    | [item, ...rest] => (List.rev(prefix), Some((item, rest)))
    };
  go([], items);
};

let modval_defs = (items: list(t)): list(TermBase.Exp.t) =>
  List.filter_map(
    (item: t) =>
      switch (item.term) {
      | ModVal(_, d) => Some(d)
      | _ => None
      },
    items,
  );

/* Replace the definitions of the evaluated bindings, in order. */
let with_modval_defs =
    (items: list(t), defs: list(TermBase.Exp.t)): list(t) => {
  let rec go = (items: list(t), defs: list(TermBase.Exp.t)) =>
    switch (items, defs) {
    | ([{term: ModVal(x, _), _} as item, ...items], [d, ...defs]) => [
        {
          ...item,
          term: (ModVal(x, d): term),
        },
        ...go(items, defs),
      ]
    | ([item, ...items], defs) => [item, ...go(items, defs)]
    | ([], _) => []
    };
  go(items, defs);
};

/* The value bound to [name]; the last binding wins. */
let modval_lookup = (items: list(t), name: Var.t): option(TermBase.Exp.t) =>
  List.fold_left(
    (acc, item: t) =>
      switch (item.term) {
      | ModVal(x, d) when x == name => Some(d)
      | _ => acc
      },
    None,
    items,
  );

/* Record an evaluated binding, replacing an earlier binding of the same
   name so a module value has one entry per exported name. */
let add_modval = (items: list(t), name: Var.t, d: TermBase.Exp.t): list(t) =>
  List.filter(
    (item: t) =>
      switch (item.term) {
      | ModVal(x, _) => x != name
      | _ => true
      },
    items,
  )
  @ [fresh(ModVal(name, d))];
