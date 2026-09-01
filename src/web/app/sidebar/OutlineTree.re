open Language;

/* OutlineTree — the module/definition tree behind the outline sidebar
   (plans/modular-editors.md §1). Walks the program term: top-level
   definitions, module members (recursively), let-in / type-in
   definitions inside function bodies, and — at TOP level only —
   semicolon statements (tests grouped into a container) plus the
   trailing expression (the symbolic ⇒ row). Every node carries a
   jump id in the chain-item id domain (DefStatics/Restructure). */

type kind =
  | KModule
  | KFn
  | KConst
  | KType
  | KTest /* one top-level `test … end;` statement */
  | KTests /* container for a contiguous run of tests */
  | KStmt /* any other top-level `…;` statement */
  | KTrail; /* the program's trailing expression */

type node = {
  o_label: string,
  o_kind: kind,
  o_id: option(Id.t),
  /* KTest: the Test term's own id — test-result lookup (o_id is the
     enclosing Seq item, the open/jump/restructure handle) */
  o_test: option(Id.t),
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

/* every BLOCK — the top-level program, a named function's body — is
   an item chain: defs, `…;` statements (tests get status rows), and
   the trailing body. Nested blocks show their ⇒ row only when the
   block actually has other items (a def-less body gets no lone ⇒);
   the top level keeps ⇒ unconditionally (it anchors the result). */
let rec of_exp = (~top=false, e: Exp.t): list(node) => {
  let e = strip_exp(e);
  switch (e.term) {
  | Let(pat, def, body) =>
    let entry =
      switch (pat_name(pat)) {
      | Some(name) => [mk_def(~id=Exp.rep_id(e), name, def)]
      | None => []
      };
    entry @ of_exp(~top, body);
  | TyAlias(tpat, _, body) =>
    let entry =
      switch (tpat.term) {
      | Var(name) => [
          {
            o_label: name,
            o_kind: KType,
            o_id: Some(Exp.rep_id(e)),
            o_test: None,
            o_children: [],
          },
        ]
      | _ => []
      };
    entry @ of_exp(~top, body);
  | ModuleExp(mpat, def, body) =>
    let entry =
      switch (mpat.term) {
      | Var(name) => [mk_def(~id=Exp.rep_id(e), name, def)]
      | _ => []
      };
    entry @ of_exp(~top, body);
  | Seq(e1, body) =>
    let h = strip_exp(e1);
    let entry =
      switch (h.term) {
      | Test(_)
      | HintedTest(_) => [
          {
            o_label: "",
            o_kind: KTest,
            o_id: Some(Exp.rep_id(e)),
            o_test: Some(Exp.rep_id(h)),
            o_children: [],
          },
        ]
      | _ => [
          {
            o_label: "",
            o_kind: KStmt,
            o_id: Some(Exp.rep_id(e)),
            o_test: None,
            o_children: [],
          },
        ]
      };
    entry @ of_exp(~top, body);
  /* a Module ROOT (mod-rooted editors, plans/mod-root.md): its items
     ARE the program's top level — no wrapper row */
  | Module(items) when top => of_mod(items)
  | _ when top => [
      {
        o_label: "",
        o_kind: KTrail,
        o_id: Some(Exp.rep_id(e)),
        o_test: None,
        o_children: [],
      },
    ]
  | _ => []
  };
}

/* a nested block (function body): items plus — only if divided — its
   trailing body as a ⇒ row */
and of_block = (fbody: Exp.t): list(node) => {
  let items = of_exp(fbody);
  switch (items) {
  | [] => []
  | _ =>
    let rec tail_of = (e: Exp.t): Exp.t => {
      let e = strip_exp(e);
      switch (e.term) {
      | Let(_, _, body)
      | TyAlias(_, _, body)
      | ModuleExp(_, _, body)
      | Seq(_, body) => tail_of(body)
      | _ => e
      };
    };
    let tail = tail_of(fbody);
    items
    @ [
      {
        o_label: "",
        o_kind: KTrail,
        o_id: Some(Exp.rep_id(tail)),
        o_test: None,
        o_children: [],
      },
    ];
  };
}

and mk_def = (~id: Id.t, name: string, def: Exp.t): node => {
  let def = strip_exp(def);
  switch (def.term) {
  | Module(items) => {
      o_label: name,
      o_kind: KModule,
      o_id: Some(id),
      o_test: None,
      o_children: of_mod(items),
    }
  | Fun(_, fbody, _, _)
  | TypFun(_, {term: Fun(_, fbody, _, _), _}, _) => {
      o_label: name,
      o_kind: KFn,
      o_id: Some(id),
      o_test: None,
      o_children: of_block(fbody),
    }
  | _ => {
      o_label: name,
      o_kind: KConst,
      o_id: Some(id),
      o_test: None,
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
              o_test: None,
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
      | ModExp(e) =>
        let h = strip_exp(e);
        switch (h.term) {
        | Test(_)
        | HintedTest(_) => [
            {
              o_label: "",
              o_kind: KTest,
              o_id: Some(Language.Mod.rep_id(m)),
              o_test: Some(Exp.rep_id(h)),
              o_children: [],
            },
          ]
        | EmptyHole => []
        | _ => [
            {
              o_label: "",
              o_kind: KStmt,
              o_id: Some(Language.Mod.rep_id(m)),
              o_test: None,
              o_children: [],
            },
          ]
        };
      | Invalid(_)
      | EmptyHole
      | MultiHole(_) => []
      },
    items,
  );

/* group each contiguous run of ≥2 tests (at ANY block level) under a
   container row (aggregate ✓/✗ in the view); singleton tests stay
   flat */
let rec group_tests = (ns: list(node)): list(node) =>
  switch (ns) {
  | [] => []
  | [{o_kind: KTest, _} as t1, {o_kind: KTest, _} as t2, ...rest] =>
    let (run, rest) = take_tests([t2, ...rest], [t1]);
    [
      {
        o_label: "tests",
        o_kind: KTests,
        o_id: None,
        o_test: None,
        o_children: List.rev(run),
      },
      ...group_tests(rest),
    ];
  | [n, ...rest] => [
      {
        ...n,
        o_children: group_tests(n.o_children),
      },
      ...group_tests(rest),
    ]
  }
and take_tests = (ns, acc) =>
  switch (ns) {
  | [{o_kind: KTest, _} as t, ...rest] => take_tests(rest, [t, ...acc])
  | _ => (acc, ns)
  };

/* number the tests in SOURCE order, program-wide */
let number_tests = (ns: list(node)): list(node) => {
  let k = ref(0);
  let label = () => {
    incr(k);
    string_of_int(k^);
  };
  let rec go = (ns: list(node)) =>
    List.map(
      n =>
        switch (n.o_kind) {
        | KTest => {
            ...n,
            o_label: label(),
          }
        | _ => {
            ...n,
            o_children: go(n.o_children),
          }
        },
      ns,
    );
  go(ns);
};

/* memoized on the term's PHYSICAL identity: statics rebuilds the term
   only when the program changes, so between edits (and on every
   render while a focus stack is open) this is a pointer compare —
   the unmemoized walk was O(program) per keystroke */
let cache: ref(option((Exp.t, list(node)))) = ref(None);

let of_term = (e: Exp.t): list(node) =>
  switch (cache^) {
  | Some((prev, tree)) when prev === e => tree
  | _ =>
    let tree = of_exp(~top=true, e) |> group_tests |> number_tests;
    cache := Some((e, tree));
    tree;
  };

/* ancestor labels of the node with id [fid], outermost first — the
   stacked header's qualifier chip (e.g. ["Geo"] for a member of
   module Geo, ["Geo", "area"] for a let nested in a member fn) */
let path_of = (fid: Id.t, e: Exp.t): list(string) => {
  let rec go = (trail, ns: list(node)) =>
    List.fold_left(
      (acc, n) =>
        switch (acc) {
        | Some(_) => acc
        | None =>
          n.o_id == Some(fid)
            ? Some(List.rev(trail))
            : go([n.o_label, ...trail], n.o_children)
        },
      None,
      ns,
    );
  go([], of_term(e)) |> Option.value(~default=[]);
};

/* durable NAME anchor for a row: outline labels root-to-node,
   OCCURRENCE-qualified — labels alone are not unique (duplicate
   definitions, two separated `tests` groups), and first-match
   resolution crossed wires between them. Text-backed persistence
   re-mints ids on every load, so pins save as these paths and
   re-resolve against the loaded outline. */
open Util;
[@deriving (show({with_path: false}), sexp, yojson)]
type path_seg = {
  s_label: string,
  s_occ: int /* index among same-labeled siblings, in tree order */
};
[@deriving (show({with_path: false}), sexp, yojson)]
type path = list(path_seg);

/* pair each node with its occurrence index — the ONE counting
   discipline shared by label_path, resolve_path, and the sidebar's
   collapse paths (diverging counters would cross wires again) */
let with_occurrences = (ns: list(node)): list((node, int)) => {
  let seen: Hashtbl.t(string, int) = Hashtbl.create(8);
  List.map(
    n => {
      let k = Hashtbl.find_opt(seen, n.o_label) |> Option.value(~default=0);
      Hashtbl.replace(seen, n.o_label, k + 1);
      (n, k);
    },
    ns,
  );
};

let label_path = (fid: Id.t, e: Exp.t): option(path) => {
  let rec go = (trail, ns: list((node, int))) =>
    List.fold_left(
      (acc, (n, occ)) => {
        let seg = {
          s_label: n.o_label,
          s_occ: occ,
        };
        switch (acc) {
        | Some(_) => acc
        | None =>
          n.o_id == Some(fid)
            ? Some(List.rev([seg, ...trail]))
            : go([seg, ...trail], with_occurrences(n.o_children))
        };
      },
      None,
      ns,
    );
  go([], with_occurrences(of_term(e)));
};

let resolve_path = (path: path, e: Exp.t): option(Id.t) => {
  let find = (seg: path_seg, ns: list(node)): option(node) =>
    with_occurrences(ns)
    |> List.find_opt(((n, occ)) =>
         n.o_label == seg.s_label && occ == seg.s_occ
       )
    |> Option.map(fst);
  let rec go = (path, ns: list(node)) =>
    switch (path) {
    | [] => None
    | [last] => Option.bind(find(last, ns), n => n.o_id)
    | [hd, ...rest] =>
      switch (find(hd, ns)) {
      | Some(n) => go(rest, n.o_children)
      | None => None
      }
    };
  go(path, of_term(e));
};

/* the kind of the row with id [fid], if any */
let kind_of = (fid: Id.t, e: Exp.t): option(kind) => {
  let rec go = (ns: list(node)) =>
    List.fold_left(
      (acc, n) =>
        switch (acc) {
        | Some(_) => acc
        | None => n.o_id == Some(fid) ? Some(n.o_kind) : go(n.o_children)
        },
      None,
      ns,
    );
  go(of_term(e));
};

/* every id in the SUBTREE rooted at [fid] (excluding fid itself) —
   pinning a parent unpins its pinned descendants */
let descendant_ids = (fid: Id.t, e: Exp.t): list(Id.t) => {
  let rec collect = (ns: list(node)): list(Id.t) =>
    List.concat_map(
      n => Option.to_list(n.o_id) @ collect(n.o_children),
      ns,
    );
  let rec find = (ns: list(node)): option(node) =>
    List.fold_left(
      (acc, n) =>
        switch (acc) {
        | Some(_) => acc
        | None => n.o_id == Some(fid) ? Some(n) : find(n.o_children)
        },
      None,
      ns,
    );
  switch (find(of_term(e))) {
  | Some(n) => collect(n.o_children)
  | None => []
  };
};
