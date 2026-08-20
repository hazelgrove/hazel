open Haz3lcore;

/* CanvasGraph — pure extraction of an architectural graph from a program's
   cached statics: type aliases become nodes, top-level functions become
   edges, values dock to their type's node, tests attach to the functions
   they mention. See plans/agent-canvas.md. */

module Exp = Language.Exp;
module Pat = Language.Pat;
module Typ = Language.Typ;
module Info = Language.Info;
module Ctx = Language.Ctx;
module Atom = Language.Atom;
module TestMap = Language.TestMap;
module TestStatus = Language.TestStatus;
module TestResults = Language.TestResults;
module ConstructorMap = Language.ConstructorMap;

type node_kind =
  | Alias /* top-level type alias */
  | Builtin /* Int, Bool, ... */
  | Derived /* [T], anonymous prods/sums/arrows */
  | Ghost; /* referenced but undefined, or hole */

type tynode = {
  key: string,
  label: string,
  n_id: option(Id.t), /* TyAlias rep_id — jump anchor */
  kind: node_kind,
  ctrs: list(string), /* constructor names when alias body is a sum */
  n_doc: option(string),
  n_err: bool,
  deps: list(string) /* alias keys this node's body references */
};

type test_info = {
  t_id: Id.t,
  status: option(TestStatus.t) /* None = not yet evaluated */
};

type edge = {
  e_name: string,
  e_id: Id.t, /* Let rep_id — jump anchor */
  e_ty: string, /* full pretty type, for tooltip */
  srcs: list(string), /* arg node keys, tuple- and curry-flattened */
  dst: string,
  e_doc: option(string),
  e_err: bool,
  e_hole: bool, /* definition contains a hole => ghost/obligation styling */
  main: bool,
  tests: list(test_info),
};

type value = {
  v_name: string,
  v_id: Id.t,
  v_key: string, /* node key of the value's type */
  v_ty: string,
  v_err: bool,
};

type t = {
  nodes: list(tynode),
  edges: list(edge),
  values: list(value),
  loose_tests: list(test_info),
};

let empty: t = {
  nodes: [],
  edges: [],
  values: [],
  loose_tests: [],
};

/* ---------- spine walk ---------- */

type item =
  | IAlias(Exp.t, Language.TPat.t, Typ.t)
  | ILet(Exp.t, Pat.t, Exp.t)
  | ITest(Exp.t, Exp.t) /* test term, test body */
  | IResult(Exp.t);

let rec strip_exp = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(e)
  | Projector(_, e)
  | Filter(_, e) => strip_exp(e)
  | _ => e
  };

let rec spine = (e: Exp.t): list(item) => {
  let e = strip_exp(e);
  switch (e.term) {
  | Let(pat, def, body) => [ILet(e, pat, def), ...spine(body)]
  | TyAlias(tpat, ty, body) => [IAlias(e, tpat, ty), ...spine(body)]
  | Seq(s, body) =>
    let s = strip_exp(s);
    switch (s.term) {
    | Test(b) => [ITest(s, b), ...spine(body)]
    | HintedTest(b, _) => [ITest(s, b), ...spine(body)]
    | _ => spine(body)
    };
  | Test(b) => [ITest(e, b)]
  | HintedTest(b, _) => [ITest(e, b)]
  | _ => [IResult(e)]
  };
};

/* ---------- small walkers ---------- */

let pat_names = (p: Pat.t): list(string) => {
  let rec go = (p: Pat.t): list(string) =>
    switch (p.term) {
    | Var(x) => [x]
    | Parens(p)
    | Asc(p, _)
    | Projector(_, p)
    | TupLabel(_, p) => go(p)
    | Tuple(ps)
    | ListLit(ps) => List.concat_map(go, ps)
    | Cons(a, b) => go(a) @ go(b)
    | Ap(f, _) => go(f) /* funlet head */
    | _ => []
    };
  go(p);
};

let exp_vars = (e: Exp.t): list(string) => {
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e) => {
          switch (e.term) {
          | Var(x) => acc := [x, ...acc^]
          | _ => ()
          };
          cont(e);
        },
      e,
    );
  List.rev(acc^);
};

let exp_has_hole = (e: Exp.t): bool => {
  let found = ref(false);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e) => {
          switch (e.term) {
          | EmptyHole
          | MultiHole(_) => found := true
          | _ => ()
          };
          cont(e);
        },
      e,
    );
  found^;
};

let rec unwrap_ty = (ty: Typ.t): Typ.t =>
  switch (ty.term) {
  | Parens(t)
  | Projector(_, t)
  | TupLabel(_, t) => unwrap_ty(t)
  | _ => ty
  };

let ty_vars = (ty: Typ.t): list(string) => {
  let acc = ref([]);
  let _ =
    Typ.map_term(
      ~f_typ=
        (cont, t: Typ.t) => {
          switch (t.term) {
          | Var(x) => acc := [x, ...acc^]
          | _ => ()
          };
          cont(t);
        },
      ty,
    );
  List.rev(acc^);
};

/* uncurry + untuple: (A, B) -> C -> D  =>  ([A, B, C], D) */
let rec flatten_arrow = (ty: Typ.t): (list(Typ.t), Typ.t) => {
  let ty = unwrap_ty(ty);
  switch (ty.term) {
  | Arrow(a, b) =>
    let args =
      switch (unwrap_ty(a).term) {
      | Prod(ts) => List.map(unwrap_ty, ts)
      | _ => [unwrap_ty(a)]
      };
    let (rest, ret) = flatten_arrow(b);
    (args @ rest, ret);
  | Rec(_, t)
  | Poly(_, t) => flatten_arrow(t)
  | _ => ([], ty)
  };
};

let atom_name = (cls: Atom.cls): string =>
  switch (cls) {
  | Int => "Int"
  | SInt => "SInt"
  | Nat => "Nat"
  | Float => "Float"
  | Bool => "Bool"
  | String => "String"
  };

let truncate = (n: int, s: string): string =>
  String.length(s) > n ? String.sub(s, 0, n - 1) ++ "…" : s;

/* Node key + display kind for a type. ~anchor uniquifies holes so distinct
   unknowns don't merge into one false hub. */
let ty_node_key = (~anchor: string, ty: Typ.t): (string, string, node_kind) => {
  let ty = unwrap_ty(ty);
  switch (ty.term) {
  | Var(name) => (name, name, Alias)
  | Atom(cls) =>
    let n = atom_name(cls);
    (n, n, Builtin);
  | List(t) =>
    let inner =
      switch (unwrap_ty(t).term) {
      | Var(n) => n
      | Atom(cls) => atom_name(cls)
      | _ => truncate(12, Typ.pretty_print(unwrap_ty(t)))
      };
    ("[" ++ inner ++ "]", "[" ++ inner ++ "]", Derived);
  | Unknown(_) => ("?" ++ anchor, "?", Ghost)
  | _ =>
    let label = truncate(20, Typ.pretty_print(ty));
    (label, label, Derived);
  };
};

let ctr_names = (ty: Typ.t): list(string) =>
  switch (unwrap_ty(ty).term) {
  | Sum(cmap) =>
    List.filter_map(
      (variant: ConstructorMap.variant(Typ.t)) =>
        switch (variant) {
        | Variant(ctr, _, _) => Some(ctr)
        | _ => None
        },
      cmap,
    )
  | _ => []
  };

/* Comments in the before-run of a term's secondary. Safe for let/type heads:
   they are provably their own first printed token (see IdTagged TRAP note). */
let doc_of = (e_annotation: Language.IdTagged.IdTag.t): option(string) => {
  let (before, _) = e_annotation.secondary;
  let comments =
    List.filter_map(
      (s: Language.Secondary.t) =>
        switch (s.content) {
        | Comment(c) =>
          let c = c |> String.split_on_char('#') |> String.concat("");
          Some(String.trim(c));
        | Whitespace(_) => None
        },
      before,
    );
  switch (comments) {
  | [] => None
  | cs => Some(String.concat(" ", cs))
  };
};

/* ---------- error attribution ---------- */

/* Which of [roots] (disjoint def/typ/test subtree rep_ids) owns [err_id]? */
let err_owner =
    (~info_map: Language.Statics.Map.t, ~roots: list(Id.t), err_id: Id.t)
    : option(Id.t) => {
  let root_set = roots;
  let is_root = id => List.exists(r => Id.compare(r, id) == 0, root_set);
  if (is_root(err_id)) {
    Some(err_id);
  } else {
    switch (Id.Map.find_opt(err_id, info_map)) {
    | Some(info) => List.find_opt(is_root, Info.ancestors_of(info))
    | None => None
    };
  };
};

/* ---------- assembly ---------- */

let extract =
    (~test_results: option(TestResults.t)=?, statics: CachedStatics.t): t => {
  let info_map = statics.info_map;
  let items = spine(statics.term);

  /* Type environment at the program's final expression: every top-level
     binding appears there as a VarEntry with its (ascription-respecting)
     type — one lookup instead of per-binding statics spelunking. */
  let result_ctx: option(Ctx.t) =
    List.find_map(
      fun
      | IResult(e) =>
        switch (Id.Map.find_opt(Exp.rep_id(e), info_map)) {
        | Some(info) => Some(Info.ctx_of(info))
        | None => None
        }
      | _ => None,
      items,
    );
  let lookup_type = (name: string): option(Typ.t) =>
    switch (result_ctx) {
    | Some(ctx) =>
      Ctx.lookup_var(ctx, name) |> Option.map((v: Ctx.var_entry) => v.typ)
    | None => None
    };

  /* Error ownership roots: the disjoint payload subtree of each item. */
  let item_root = (item: item): option(Id.t) =>
    switch (item) {
    | IAlias(_, _, ty) => Some(Typ.rep_id(ty))
    | ILet(_, _, def) => Some(Exp.rep_id(def))
    | ITest(_, body) => Some(Exp.rep_id(body))
    | IResult(_) => None
    };
  let roots = List.filter_map(item_root, items);
  let err_roots: list(Id.t) =
    statics.error_ids
    |> List.filter_map(err_owner(~info_map, ~roots))
    |> List.sort_uniq(Id.compare);
  let root_has_err = (root: option(Id.t)): bool =>
    switch (root) {
    | Some(r) => List.exists(e => Id.compare(e, r) == 0, err_roots)
    | None => false
    };

  /* Aliases, in program order. */
  let alias_nodes: list(tynode) =
    List.filter_map(
      fun
      | IAlias(term, tpat, ty) => {
          let name =
            switch (tpat.term) {
            | Var(n) => n
            | _ => "?"
            };
          let is_hole =
            switch (unwrap_ty(ty).term) {
            | Unknown(_) => true
            | _ => false
            };
          Some({
            key: name,
            label: name,
            n_id: Some(Exp.rep_id(term)),
            kind: is_hole ? Ghost : Alias,
            ctrs: ctr_names(ty),
            n_doc: doc_of(term.annotation),
            n_err: root_has_err(Some(Typ.rep_id(ty))),
            deps: List.sort_uniq(compare, ty_vars(ty)),
          });
        }
      | _ => None,
      items,
    );
  let alias_keys = List.map(n => n.key, alias_nodes);

  /* Bindings: an edge if the ctx type is arrow-ish, else a value. */
  let bindings: list((string, Id.t, Typ.t, option(string), bool, bool)) =
    List.concat_map(
      fun
      | ILet(term, pat, def) => {
          let doc = doc_of(term.annotation);
          let err = root_has_err(Some(Exp.rep_id(def)));
          let hole = exp_has_hole(def);
          pat_names(pat)
          |> List.filter_map(name =>
               lookup_type(name)
               |> Option.map(ty =>
                    (name, Exp.rep_id(term), ty, doc, err, hole)
                  )
             );
        }
      | _ => [],
      items,
    );

  /* Reference counts across all definition bodies + result, for main-ness. */
  let all_uses: list(string) =
    List.concat_map(
      fun
      | ILet(_, _, def) => exp_vars(def)
      | IResult(e) => exp_vars(e)
      | _ => [],
      items,
    );
  let use_count = (name: string): int =>
    List.length(List.filter(u => u == name, all_uses));

  let (edges_raw, values) =
    List.fold_left(
      ((es, vs), (name, id, ty, doc, err, hole)) => {
        let (args, ret) = flatten_arrow(ty);
        switch (args) {
        | [] =>
          let (v_key, _, _) = ty_node_key(~anchor=Id.to_string(id), ty);
          (
            es,
            vs
            @ [
              {
                v_name: name,
                v_id: id,
                v_key,
                v_ty: Typ.pretty_print(ty),
                v_err: err,
              },
            ],
          );
        | _ =>
          let anchor = Id.to_string(id);
          let srcs =
            List.map(
              a => {
                let (k, _, _) = ty_node_key(~anchor, a);
                k;
              },
              args,
            );
          let (dst, _, _) = ty_node_key(~anchor, ret);
          (
            es
            @ [
              {
                e_name: name,
                e_id: id,
                e_ty: Typ.pretty_print(ty),
                srcs,
                dst,
                e_doc: doc,
                e_err: err,
                e_hole: hole,
                main: use_count(name) >= 2,
                tests: [],
              },
            ],
            vs,
          );
        };
      },
      ([], []),
      bindings,
    );

  /* Tests: attach to the first top-level function mentioned in the body. */
  let edge_names = List.map(e => e.e_name, edges_raw);
  let status_of = (id: Id.t): option(TestStatus.t) =>
    switch (test_results) {
    | Some(tr) =>
      TestMap.lookup(id, tr.test_map) |> Option.map(TestMap.joint_status)
    | None => None
    };
  let (edge_tests, loose_tests) =
    List.fold_left(
      ((et, lt), item) =>
        switch (item) {
        | ITest(term, body) =>
          let t_id = Exp.rep_id(term);
          let info = {
            t_id,
            status: status_of(t_id),
          };
          switch (
            exp_vars(body) |> List.find_opt(v => List.mem(v, edge_names))
          ) {
          | Some(target) => ([(target, info), ...et], lt)
          | None => (et, lt @ [info])
          };
        | _ => (et, lt)
        },
      ([], []),
      items,
    );
  let edges =
    List.map(
      e =>
        {
          ...e,
          tests:
            edge_tests
            |> List.filter(((name, _)) => name == e.e_name)
            |> List.rev_map(snd),
        },
      edges_raw,
    );

  /* Nodes: aliases first (program order), then any endpoint/value type not
     yet present, in first-use order. Alias references without a definition
     become ghosts. */
  let referenced_keys: list((string, (string, node_kind))) =
    List.concat_map(e => e.srcs @ [e.dst], edges)
    @ List.map(v => v.v_key, values)
    |> List.map(k => {
         let kind =
           if (List.mem(k, alias_keys)) {
             Alias;
           } else if (String.length(k) > 0 && k.[0] == '?') {
             Ghost;
           } else if (List.mem(
                        k,
                        ["Int", "SInt", "Nat", "Float", "Bool", "String"],
                      )) {
             Builtin;
           } else if (String.length(k) > 0 && k.[0] == '[') {
             Derived;
           } else {
             Ghost;
                  /* named but not defined here (Use import / tyvar) */
           };
         let label =
           String.length(k) > 0 && k.[0] == '?' ? "?" : truncate(20, k);
         (k, (label, kind));
       });
  let extra_nodes =
    List.fold_left(
      (acc, (k, (label, kind))) =>
        List.mem(k, alias_keys)
        || List.exists((n: tynode) => n.key == k, acc)
          ? acc
          : acc
            @ [
              {
                key: k,
                label,
                n_id: None,
                kind,
                ctrs: [],
                n_doc: None,
                n_err: false,
                deps: [],
              },
            ],
      [],
      referenced_keys,
    );

  {
    nodes: alias_nodes @ extra_nodes,
    edges,
    values,
    loose_tests,
  };
};
