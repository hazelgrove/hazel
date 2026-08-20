open Haz3lcore;

/* CanvasGraph — pure extraction of an architectural graph from a program's
   cached statics: type aliases become nodes, top-level functions become
   edges, values dock to their type's node, tests attach to the functions
   they mention. Multi-argument functions get an explicit Product node fed
   by formation lines from the component types (shared between functions
   with the same input tuple). Builtin types (Int, String, ...) duplicate
   per use-site as small satellite terminals — like ground symbols in a
   circuit diagram — so they never act as hubs. See plans/agent-canvas.md. */

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
  | Derived /* [T], anonymous sums/arrows */
  | Ghost /* referenced but undefined, or hole */
  | Product; /* tuple-formation node for a multi-arg input */

type tynode = {
  key: string,
  label: string,
  n_id: option(Id.t), /* TyAlias rep_id — jump anchor */
  kind: node_kind,
  ctrs: list(string), /* constructor names when alias body is a sum */
  n_ty: option(string), /* alias body pretty — a second name for matching */
  n_doc: option(string),
  n_err: bool,
  deps: list(string), /* node keys this node's body/components reference */
  parts: list(string), /* Product: component node keys (formation lines) */
  sat: option((string, bool)) /* satellite: (anchor node key, output side) */
};

type test_info = {
  t_id: Id.t,
  status: option(TestStatus.t) /* None = not yet evaluated */
};

type edge = {
  e_name: string,
  e_id: Id.t, /* Let rep_id — jump anchor */
  e_ty: string, /* full pretty type, for tooltip */
  e_src: string, /* input node key (component, product, or satellite) */
  dst: string,
  e_doc: option(string),
  e_err: bool,
  e_hole: bool, /* definition contains a hole => ghost/obligation styling */
  main: bool,
  tests: list(test_info),
  /* probe-sample anchors: parameter pattern ids (aligned with the
     flattened input components) and the function's body id */
  e_arg_ids: list(Id.t),
  e_out_id: option(Id.t),
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
  /* rep id of the first test / the result expression — canvas-authored
     function stubs paste just before it (after all definitions) */
  insert_anchor: option(Id.t),
  /* the last top-level binding (name, term rep id): canvas-authored
     stubs insert_after it (pathless insert_after goes after the trailing
     expression, which the statics guard rightly rejects). The id lets the
     insert path be resolved unambiguously when the name is shadowed by a
     nested binding. */
  last_def: option((string, Id.t)),
};

let empty: t = {
  nodes: [],
  edges: [],
  values: [],
  loose_tests: [],
  insert_anchor: None,
  last_def: None,
};

let mk_node =
    (
      ~n_id=None,
      ~ctrs=[],
      ~n_ty=None,
      ~n_doc=None,
      ~n_err=false,
      ~deps=[],
      ~parts=[],
      ~sat=None,
      ~kind,
      ~label,
      key,
    )
    : tynode => {
  key,
  label,
  n_id,
  kind,
  ctrs,
  n_ty,
  n_doc,
  n_err,
  deps,
  parts,
  sat,
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

/* pretty-print with redundant Parens stripped everywhere, so signatures
   read `(Model, Msg) -> Model` rather than `((Model, Msg)) -> Model` and
   type strings compare stably regardless of source parenthesization */
let pretty_ty = (ty: Typ.t): string => {
  let rec strip = (t: Typ.t): Typ.t =>
    switch (t.term) {
    | Parens(inner) => strip(inner)
    | _ => t
    };
  ty
  |> Typ.map_term(~f_typ=(cont, t) => cont(strip(t)))
  |> strip
  |> Typ.pretty_print;
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

/* Shared label + kind for a type (holes uniquified by ~anchor so distinct
   unknowns never merge into one false hub). */
let ty_ref = (~anchor: string, ty: Typ.t): (string, node_kind) => {
  let ty = unwrap_ty(ty);
  switch (ty.term) {
  | Var(name) => (name, Alias)
  | Atom(cls) => (atom_name(cls), Builtin)
  | List(t) =>
    let inner =
      switch (unwrap_ty(t).term) {
      | Var(n) => n
      | Atom(cls) => atom_name(cls)
      | _ => truncate(12, Typ.pretty_print(unwrap_ty(t)))
      };
    ("[" ++ inner ++ "]", Derived);
  | Unknown(_) => ("?" ++ anchor, Ghost)
  | _ => (truncate(20, Typ.pretty_print(ty)), Derived)
  };
};

let display_label = (key: string): string =>
  String.length(key) > 0 && key.[0] == '?' ? "?" : truncate(20, key);

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

/* ---------- function anatomy (probe-sample anchors) ---------- */

let rec strip_pat = (p: Pat.t): Pat.t =>
  switch (p.term) {
  | Parens(p)
  | Asc(p, _)
  | TupLabel(_, p)
  | Projector(_, p) => strip_pat(p)
  | _ => p
  };

/* Parameter pattern ids (tuple components flattened, matching
   flatten_arrow's input slots) and the body id of a function definition.
   Probe samples at a pattern id carry the bound value; at the body id,
   the return value. */
let fun_anatomy = (def: Exp.t): (list(Id.t), option(Id.t)) => {
  let pat_comps = (p: Pat.t): list(Id.t) => {
    let p = strip_pat(p);
    switch (p.term) {
    | Tuple(ps) => List.map(p => Pat.rep_id(strip_pat(p)), ps)
    | _ => [Pat.rep_id(p)]
    };
  };
  let rec go = (acc, e: Exp.t): (list(Id.t), option(Id.t)) => {
    let e = strip_exp(e);
    switch (e.term) {
    | Fun(p, body, _, _) => go(acc @ pat_comps(p), body)
    | _ => (acc, Some(Exp.rep_id(e)))
    };
  };
  switch (strip_exp(def).term) {
  | Fun(_) => go([], def)
  | _ => ([], None)
  };
};

/* ---------- test attribution ---------- */

/* Which top-level function is a test's subject? A test that mentions
   exactly one is lexically a unit test of it. With several, take the head
   of the DEEPEST application — the innermost call is the one being driven;
   outer calls massage its output for the assertion (in
   `count(update(...)) == 1` the subject is `update`). */
let test_subject = (~edge_names: list(string), body: Exp.t): option(string) => {
  let rec head_of = (e: Exp.t): option(string) =>
    switch (strip_exp(e).term) {
    | Var(x) => Some(x)
    | Ap(_, f, _) => head_of(f)
    | _ => None
    };
  let hits: ref(list((string, int))) = ref([]);
  let depth = ref(0);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e) => {
          switch (e.term) {
          | Ap(_, f, _) =>
            switch (head_of(f)) {
            | Some(x) when List.mem(x, edge_names) =>
              hits := [(x, depth^), ...hits^]
            | _ => ()
            };
            incr(depth);
            let r = cont(e);
            decr(depth);
            r;
          | _ => cont(e)
          }
        },
      body,
    );
  let names = hits^ |> List.map(fst) |> List.sort_uniq(compare);
  switch (names) {
  | [] =>
    /* no applications: fall back to any bare mention */
    exp_vars(body) |> List.find_opt(v => List.mem(v, edge_names))
  | [x] => Some(x)
  | _ =>
    hits^
    |> List.fold_left(
         (best, (x, d)) =>
           switch (best) {
           | Some((_, bd)) when bd >= d => best
           | _ => Some((x, d))
           },
         None,
       )
    |> Option.map(fst)
  };
};

/* ---------- error attribution ---------- */

/* Which of [roots] (disjoint def/typ/test subtree rep_ids) owns [err_id]? */
let err_owner =
    (~info_map: Language.Statics.Map.t, ~roots: list(Id.t), err_id: Id.t)
    : option(Id.t) => {
  let is_root = id => List.exists(r => Id.compare(r, id) == 0, roots);
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
          Some(
            mk_node(
              ~n_id=Some(Exp.rep_id(term)),
              ~kind=is_hole ? Ghost : Alias,
              ~ctrs=ctr_names(ty),
              ~n_ty=is_hole ? None : Some(pretty_ty(ty)),
              ~n_doc=doc_of(term.annotation),
              ~n_err=root_has_err(Some(Typ.rep_id(ty))),
              ~deps=List.sort_uniq(compare, ty_vars(ty)),
              ~label=name,
              name,
            ),
          );
        }
      | _ => None,
      items,
    );
  let alias_keys = List.map((n: tynode) => n.key, alias_nodes);

  /* Non-alias nodes accumulate as edges/values reference them. */
  let extras: ref(list(tynode)) = ref([]);
  let have = (key: string): bool =>
    List.mem(key, alias_keys)
    || List.exists((n: tynode) => n.key == key, extras^);
  let ensure = (n: tynode): unit =>
    if (!have(n.key)) {
      extras := extras^ @ [n];
    };

  /* A grid node for a type reference (alias/derived/ghost — never dup'd). */
  let ensure_grid = (key: string, kind: node_kind): unit => {
    let kind =
      switch (kind) {
      | Alias when !List.mem(key, alias_keys) => Ghost /* undefined name */
      | k => k
      };
    ensure(mk_node(~kind, ~label=display_label(key), key));
  };

  /* A builtin terminal duplicated per use-site, docked to [anchor_key]. */
  let ensure_sat =
      (~anchor_key: string, ~output: bool, ~dup: string, label: string)
      : string => {
    let key = label ++ "@" ++ dup;
    ensure(
      mk_node(~kind=Builtin, ~sat=Some((anchor_key, output)), ~label, key),
    );
    key;
  };

  /* Bindings, phase 1: names, types, metadata. */
  let bindings:
    list(
      (
        string,
        Id.t,
        Typ.t,
        option(string),
        bool,
        bool,
        (list(Id.t), option(Id.t)),
      ),
    ) =
    List.concat_map(
      fun
      | ILet(term, pat, def) => {
          let doc = doc_of(term.annotation);
          let err = root_has_err(Some(Exp.rep_id(def)));
          let hole = exp_has_hole(def);
          let anatomy = fun_anatomy(def);
          pat_names(pat)
          |> List.filter_map(name =>
               lookup_type(name)
               |> Option.map(ty =>
                    (name, Exp.rep_id(term), ty, doc, err, hole, anatomy)
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

  /* Bindings, phase 2: materialize nodes and edges/values. */
  let (edges_raw, values) =
    List.fold_left(
      ((es, vs), (name, id, ty, doc, err, hole, (arg_ids, out_id))) => {
        let (args, ret) = flatten_arrow(ty);
        switch (args) {
        | [] =>
          let (v_key, v_kind) = ty_ref(~anchor=name, ty);
          ensure_grid(v_key, v_kind);
          (
            es,
            vs
            @ [
              {
                v_name: name,
                v_id: id,
                v_key,
                v_ty: pretty_ty(ty),
                v_err: err,
              },
            ],
          );
        | _ =>
          /* input node: single component, or a shared Product */
          let comp_refs =
            List.mapi(
              (i, a) => ty_ref(~anchor=name ++ string_of_int(i), a),
              args,
            );
          let (ret_key, ret_kind) = ty_ref(~anchor=name ++ "r", ret);
          let input_key =
            switch (comp_refs) {
            | [(k, Builtin)] when ret_kind != Builtin =>
              /* single builtin arg: terminal docked to the result node */
              ensure_grid(ret_key, ret_kind);
              ensure_sat(~anchor_key=ret_key, ~output=false, ~dup=name, k);
            | [(k, kind)] =>
              ensure_grid(k, kind);
              k;
            | comps =>
              let product_key =
                "("
                ++ String.concat(
                     ", ",
                     List.map(((k, _)) => display_label(k), comps),
                   )
                ++ ")";
              let part_keys =
                List.map(
                  ((k, kind)) =>
                    switch (kind) {
                    | Builtin =>
                      ensure_sat(
                        ~anchor_key=product_key,
                        ~output=false,
                        ~dup=product_key,
                        k,
                      )
                    | _ =>
                      ensure_grid(k, kind);
                      k;
                    },
                  comps,
                );
              ensure(
                mk_node(
                  ~kind=Product,
                  ~label="",
                  ~parts=part_keys,
                  ~deps=
                    List.filter_map(
                      ((k, kind)) =>
                        switch (kind) {
                        | Builtin => None
                        | _ => Some(k)
                        },
                      comps,
                    ),
                  product_key,
                ),
              );
              product_key;
            };
          /* result node: builtin results dock to the input node */
          let dst_key =
            switch (ret_kind) {
            | Builtin =>
              ensure_sat(
                ~anchor_key=input_key,
                ~output=true,
                ~dup=name,
                ret_key,
              )
            | _ =>
              ensure_grid(ret_key, ret_kind);
              ret_key;
            };
          (
            es
            @ [
              {
                e_name: name,
                e_id: id,
                e_ty: pretty_ty(ty),
                e_src: input_key,
                dst: dst_key,
                e_doc: doc,
                e_err: err,
                e_hole: hole,
                main: use_count(name) >= 2,
                tests: [],
                e_arg_ids: arg_ids,
                e_out_id: out_id,
              },
            ],
            vs,
          );
        };
      },
      ([], []),
      bindings,
    );

  /* Tests: attach to their lexical subject (see test_subject). */
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
          switch (test_subject(~edge_names, body)) {
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

  let insert_anchor =
    List.find_map(
      fun
      | ITest(term, _) => Some(Exp.rep_id(term))
      | IResult(e) => Some(Exp.rep_id(e))
      | _ => None,
      items,
    );
  let last_def =
    List.fold_left(
      (acc, item) =>
        switch (item) {
        | IAlias(term, tpat, _) =>
          switch (tpat.term) {
          | Var(n) => Some((n, Exp.rep_id(term)))
          | _ => acc
          }
        | ILet(term, pat, _) =>
          switch (pat_names(pat)) {
          | [n, ..._] => Some((n, Exp.rep_id(term)))
          | [] => acc
          }
        | _ => acc
        },
      None,
      items,
    );
  {
    nodes: alias_nodes @ extras^,
    edges,
    values,
    loose_tests,
    insert_anchor,
    last_def,
  };
};
