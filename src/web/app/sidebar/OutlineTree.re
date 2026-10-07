open Language;

/* the module/definition tree behind the outline sidebar: definitions,
   module members and nested lets, plus `…;` statements and trailing ⇒
   rows. row ids are chain-item ids (as in DefStatics and ItemEdit) */

type kind =
  | KModule
  | KFn
  | KConst
  | KType
  | KTest /* one `test … end;` statement */
  | KTests /* container for a contiguous run of tests */
  | KStmt /* any other `…;` statement */
  | KTrail; /* a block's trailing expression */

type node = {
  o_label: string,
  o_kind: kind,
  o_id: option(Id.t),
  /* KTest: the Test term's own id, for result lookup (o_id is the
     enclosing item, the open/jump/restructure handle) */
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

/* a block (the program, a function body) is an item chain: defs, `…;`
   statements and the trailing body. a nested block shows its ⇒ row only
   beside other items; the top level always does (it anchors the result) */
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
  /* a Module root: its items are the program's top level, with no
     wrapper row */
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

/* a function body's items, plus its trailing body as a ⇒ row if any */
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

/* each run of ≥2 tests, at any level, goes under a container row; a lone
   test stays flat */
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

/* memoized on the term's physical identity, which statics keeps until
   the program changes */
let cache: Util.Slot.t(Exp.t, list(node)) = Util.Slot.mk();

let of_term = (e: Exp.t): list(node) =>
  Util.Slot.get(cache, e, () =>
    of_exp(~top=true, e) |> group_tests |> number_tests
  );

/* ancestor labels of the node with id [fid], outermost first, for the
   stacked header's qualifier chip (["Geo"] for a member of module Geo) */
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

/* a durable name for a row: its labels root to node, each qualified by
   occurrence (labels repeat). persistence re-mints ids on load, so pins
   save as paths and re-resolve against the loaded outline */
open Util;
[@deriving (show({with_path: false}), sexp, yojson)]
type path_seg = {
  s_label: string,
  s_occ: int /* index among same-labeled siblings, in tree order */
};
[@deriving (show({with_path: false}), sexp, yojson)]
type path = list(path_seg);

/* each row's path segment: its label and its index among same-labeled
   siblings; tests and test groups go by the named row before them
   (`tests@b`), so adding a test elsewhere leaves them put. label_path,
   resolve_path and the sidebar's collapse paths must all count here */
let segs = (ns: list(node)): list((node, path_seg)) => {
  let seen: Hashtbl.t(string, int) = Hashtbl.create(8);
  let prev = ref("");
  List.map(
    n => {
      let l =
        switch (n.o_kind) {
        | KTest => "test@" ++ prev^
        | KTests => "tests@" ++ prev^
        | _ => n.o_label
        };
      let k = Hashtbl.find_opt(seen, l) |> Option.value(~default=0);
      Hashtbl.replace(seen, l, k + 1);
      if (n.o_label != "" && n.o_kind != KTest && n.o_kind != KTests) {
        prev := n.o_label;
      };
      (
        n,
        {
          s_label: l,
          s_occ: k,
        },
      );
    },
    ns,
  );
};

let label_path = (fid: Id.t, e: Exp.t): option(path) => {
  let rec go = (trail, ns: list((node, path_seg))) =>
    List.fold_left(
      (acc, (n, seg)) =>
        switch (acc) {
        | Some(_) => acc
        | None =>
          n.o_id == Some(fid)
            ? Some(List.rev([seg, ...trail]))
            : go([seg, ...trail], segs(n.o_children))
        },
      None,
      ns,
    );
  go([], segs(of_term(e)));
};

let resolve_path = (path: path, e: Exp.t): option(Id.t) => {
  let find = (seg: path_seg, ns: list(node)): option(node) =>
    segs(ns) |> List.find_opt(((_, s)) => s == seg) |> Option.map(fst);
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

/* the headerless rows' ids (⇒ and `;`) */
let headless_row_ids = (e: Exp.t): Id.Map.t(unit) => {
  let rec go = (acc, ns: list(node)) =>
    List.fold_left(
      (acc, n) =>
        go(
          switch (n.o_id) {
          | Some(id) when n.o_kind == KTrail || n.o_kind == KStmt =>
            Id.Map.add(id, (), acc)
          | _ => acc
          },
          n.o_children,
        ),
      acc,
      ns,
    );
  go(Id.Map.empty, of_term(e));
};

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

/* ids below [fid] (not fid itself): pinning a parent unpins its pinned
   descendants */
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

/* ids of the rows from the top level down to [fid], inclusive; rows
   without ids (test containers) are skipped */
let trail_of = (fid: Id.t, e: Exp.t): option(list(Id.t)) => {
  let rec go = (trail, ns: list(node)) =>
    List.fold_left(
      (acc, n) =>
        switch (acc) {
        | Some(_) => acc
        | None =>
          let trail =
            switch (n.o_id) {
            | Some(id) => [id, ...trail]
            | None => trail
            };
          n.o_id == Some(fid)
            ? Some(List.rev(trail)) : go(trail, n.o_children);
        },
      None,
      ns,
    );
  go([], of_term(e));
};

let node_of = (fid: Id.t, e: Exp.t): option(node) => {
  let rec go = (ns: list(node)) =>
    List.fold_left(
      (acc, n) =>
        switch (acc) {
        | Some(_) => acc
        | None => n.o_id == Some(fid) ? Some(n) : go(n.o_children)
        },
      None,
      ns,
    );
  go(of_term(e));
};

/* every row id, memoized on the term like [of_term] */
let row_ids_cache: Slot.t(Exp.t, Id.Map.t(unit)) = Slot.mk();
let row_ids = (e: Exp.t): Id.Map.t(unit) =>
  Slot.get(
    row_ids_cache,
    e,
    () => {
      let rec go = (acc, ns: list(node)) =>
        List.fold_left(
          (acc, n) =>
            go(
              switch (n.o_id) {
              | Some(id) => Id.Map.add(id, (), acc)
              | None => acc
              },
              n.o_children,
            ),
          acc,
          ns,
        );
      go(Id.Map.empty, of_term(e));
    },
  );
