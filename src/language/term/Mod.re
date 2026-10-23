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
   item. */

/* Replace the definition carried by an item. */
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

/* The bindings of a module value: None while any item is still pending. */
let value_bindings =
    (items: list(t)): option(list((Var.t, TermBase.Exp.t))) =>
  items
  |> List.map((item: t) =>
       switch (item.term) {
       | ModVal(x, d) => Some((x, d))
       | _ => None
       }
     )
  |> Util.OptUtil.sequence;

/* Member [name] of a module value. */
let value_member = (items: list(t), name: Var.t): option(TermBase.Exp.t) =>
  Option.bind(
    value_bindings(items),
    List.find_map(((x, d)) => String.equal(x, name) ? Some(d) : None),
  );

/* Member [name] of a module, which may be stuck on an item: its value, if an
   evaluated item binds it and no item from the stuck one on binds it again. */
let member = (items: list(t), name: Var.t): option(TermBase.Exp.t) => {
  let (evaluated, pending) = split_pending(items);
  let rebound =
    switch (pending) {
    | None => false
    | Some((item, rest)) =>
      List.exists(
        (item: t) =>
          switch (item.term) {
          | ModLet(p, _) =>
            List.exists(String.equal(name), Pat.bound_vars(p))
          | _ => false
          },
        [item, ...rest],
      )
    };
  rebound
    ? None
    : List.find_map(
        (item: t) =>
          switch (item.term) {
          | ModVal(x, d) when String.equal(x, name) => Some(d)
          | _ => None
          },
        evaluated,
      );
};

/* Record an evaluated binding at the end, replacing any earlier binding of
   [name]. */
let add_modval = (items: list(t), name: Var.t, d: TermBase.Exp.t): list(t) =>
  List.filter(
    (item: t) =>
      switch (item.term) {
      | ModVal(x, _) => !String.equal(x, name)
      | _ => true
      },
    items,
  )
  @ [fresh(ModVal(name, d))];
