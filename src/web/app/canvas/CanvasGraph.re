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
  /* enclosing module chain, outermost first ([] = top level) */
  m_path: list(string),
  n_id: option(Id.t), /* TyAlias rep_id — jump anchor */
  kind: node_kind,
  ctrs: list(string), /* constructor names when alias body is a sum */
  n_ty: option(string), /* alias body pretty — a second name for matching */
  n_doc: option(string),
  n_err: bool,
  deps: list(string), /* node keys this node's body/components reference */
  /* deps that are displayed through former nodes instead of plain dep
     links (they still participate in layer ordering) */
  hidden_deps: list(string),
  parts: list(string), /* Product / folded alias: component node keys (formation lines) */
  sat: option((string, bool)), /* satellite: (anchor node key, output side) */
  /* glyph of the type former an alias body IS ("()", "[]", "+"): the
     former is folded into the alias node — one object, drawn once */
  former: option(string),
};

type test_info = {
  t_id: Id.t,
  status: option(TestStatus.t) /* None = not yet evaluated */
};

type edge = {
  e_name: string, /* path-qualified, unique (dom ids, focus keys) */
  e_label: string, /* bare display name */
  m_path: list(string),
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
  /* one id per parameter: the UNSPLIT pattern (see fun_anatomy) */
  e_whole_ids: list(Id.t),
  e_out_id: option(Id.t),
  /* names of OTHER top-level bindings this definition references
     (call/constant dependencies; the hover fan) */
  e_deps: list(string),
};

type value = {
  v_name: string, /* path-qualified, unique */
  v_label: string, /* bare display name */
  m_path: list(string),
  v_id: Id.t,
  v_key: string, /* node key of the value's type */
  v_ty: string,
  v_err: bool,
  v_def: Exp.t /* the definition, for the info panel */
};

type t = {
  nodes: list(tynode),
  edges: list(edge),
  values: list(value),
  loose_tests: list(test_info),
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
  last_def: None,
};

let mk_node =
    (
      ~n_id=None,
      ~m_path=[],
      ~ctrs=[],
      ~n_ty=None,
      ~n_doc=None,
      ~n_err=false,
      ~deps=[],
      ~hidden_deps=[],
      ~parts=[],
      ~sat=None,
      ~former=None,
      ~kind,
      ~label,
      key,
    )
    : tynode => {
  key,
  label,
  m_path,
  n_id,
  kind,
  ctrs,
  n_ty,
  n_doc,
  n_err,
  deps,
  hidden_deps,
  parts,
  sat,
  former,
};

/* ---------- spine walk ---------- */

/* anchor id + doc comment travel with the item so module members (whose
   wrapper is a Mod.t, not an Exp.t) flow through the same pipeline */
type item =
  | IAlias(option(Id.t), option(string), Language.TPat.t, Typ.t)
  | ILet(option(Id.t), option(string), Pat.t, Exp.t)
  | ITest(Exp.t, Exp.t) /* test term, test body */
  | IResult(Exp.t);

let rec strip_exp = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(e)
  | Projector(_, e)
  | Filter(_, e) => strip_exp(e)
  | _ => e
  };

/* doc_of is defined below; forward-declare the shape we need here */
let doc_of_fwd: ref(Language.IdTagged.IdTag.t => option(string)) =
  ref(_ => None);

/* walk a module body: members surface as path-tagged items */
let rec mod_members =
        (path: list(string), ms: list(Language.Mod.t))
        : list((list(string), item)) =>
  List.concat_map(
    (m: Language.Mod.t) =>
      switch (m.term) {
      | ModLet(pat, def) =>
        let entry = (
          path,
          ILet(
            Some(Language.Mod.rep_id(m)),
            doc_of_fwd^(m.annotation),
            pat,
            def,
          ),
        );
        /* `let m = { … }` members are modules too */
        switch (strip_exp(def).term, pat.term) {
        | (Module(items), Var(name)) => [
            entry,
            ...mod_members(path @ [name], items),
          ]
        | _ => [entry]
        };
      | ModType(tp, ty) => [
          (
            path,
            IAlias(
              Some(Language.Mod.rep_id(m)),
              doc_of_fwd^(m.annotation),
              tp,
              ty,
            ),
          ),
        ]
      | ModuleMod(mpat, def) =>
        switch (mpat.term) {
        | Var(name) =>
          let entry = (
            path,
            ILet(
              Some(Language.Mod.rep_id(m)),
              doc_of_fwd^(m.annotation),
              {
                term: Var(name),
                annotation: mpat.annotation,
              },
              def,
            ),
          );
          switch (strip_exp(def).term) {
          | Module(items) => [entry, ...mod_members(path @ [name], items)]
          | _ => [entry]
          };
        | _ => []
        }
      | ModExp(_)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_) => []
      },
    ms,
  )

and spine = (e: Exp.t): list((list(string), item)) => {
  let e = strip_exp(e);
  let top = it => ([], it);
  switch (e.term) {
  | Let(pat, def, body) =>
    let entry =
      top(ILet(Some(Exp.rep_id(e)), doc_of_fwd^(e.annotation), pat, def));
    /* `let m = { … }` binds a module without module syntax (the livelit
       idiom): its members join the stream like any module's */
    let members =
      switch (strip_exp(def).term, pat.term) {
      | (Module(items), Var(name)) => mod_members([name], items)
      | _ => []
      };
    [entry] @ members @ spine(body);
  /* module M = {...} binds like a let whose type is a Sig ({} former);
     its members join the stream under the module's path */
  | ModuleExp(mpat, def, body) =>
    switch (mpat.term) {
    | Var(name) =>
      let entry =
        top(
          ILet(
            Some(Exp.rep_id(e)),
            doc_of_fwd^(e.annotation),
            {
              term: Var(name),
              annotation: mpat.annotation,
            },
            def,
          ),
        );
      let members =
        switch (strip_exp(def).term) {
        | Module(items) => mod_members([name], items)
        | _ => []
        };
      [entry] @ members @ spine(body);
    | _ => spine(body)
    }
  | TyAlias(tpat, ty, body) => [
      top(
        IAlias(Some(Exp.rep_id(e)), doc_of_fwd^(e.annotation), tpat, ty),
      ),
      ...spine(body),
    ]
  | Seq(s, body) =>
    let s = strip_exp(s);
    switch (s.term) {
    | Test(b) => [top(ITest(s, b)), ...spine(body)]
    | HintedTest(b, _) => [top(ITest(s, b)), ...spine(body)]
    | _ => spine(body)
    };
  | Test(b) => [top(ITest(e, b))]
  | HintedTest(b, _) => [top(ITest(e, b))]
  | _ => [top(IResult(e))]
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
  /* module values: a {} former, styled like the ()/[] delimiters */
  | Sig(_) => ("{}" ++ "@" ++ anchor, Product)
  | _ => (truncate(20, Typ.pretty_print(ty)), Derived)
  };
};

let display_label = (key: string): string =>
  if (String.length(key) > 0 && key.[0] == '?') {
    "?";
  } else if (String.length(key) >= 2 && String.sub(key, 0, 2) == "{}") {
    "{}";
  } else {
    truncate(20, key);
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
doc_of_fwd := doc_of;

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
   the return value. Two definition shapes: explicit `fun` chains on the
   rhs, and funlet form (`let f(x: A, y: B): C = body`) where the params
   live in the let pattern's Ap argument and the rhs IS the body. */
let pat_comps = (p: Pat.t): list(Id.t) => {
  let p = strip_pat(p);
  switch (p.term) {
  | Tuple(ps) => List.map(p => Pat.rep_id(strip_pat(p)), ps)
  | _ => [Pat.rep_id(p)]
  };
};

/* Innermost expression a body evaluates through: descend let/seq headers
   so the output anchor is a term that completes with the return value
   (a `let` header itself never carries a sample). */
let rec body_anchor = (e: Exp.t): Id.t => {
  let e = strip_exp(e);
  switch (e.term) {
  | Let(_, _, body)
  | Seq(_, body) => body_anchor(body)
  | _ => Exp.rep_id(e)
  };
};

let pat_whole = (p: Pat.t): Id.t => Pat.rep_id(strip_pat(p));

/* (flattened component ids, whole-pattern id per param, body anchor).
   The whole-pattern ids matter when a tuple pattern destructures a
   NAMED alias (flip = fun (s, r) with flip : Card -> Card): the strip
   shows ONE Card slot, and its well must anchor at the whole pattern —
   anchoring at the first component showed bare Suits. */
let fun_anatomy =
    (~pat: Pat.t, def: Exp.t): (list(Id.t), list(Id.t), option(Id.t)) => {
  let rec go =
          ((acc_c, acc_w), e: Exp.t)
          : (list(Id.t), list(Id.t), option(Id.t)) => {
    let e = strip_exp(e);
    switch (e.term) {
    | Fun(p, body, _, _) =>
      go((acc_c @ pat_comps(p), acc_w @ [pat_whole(p)]), body)
    | _ => (acc_c, acc_w, Some(body_anchor(e)))
    };
  };
  switch (strip_exp(def).term) {
  | Fun(_) => go(([], []), def)
  | _ =>
    switch (strip_pat(pat).term) {
    | Ap(_, arg) => (
        pat_comps(arg),
        [pat_whole(arg)],
        Some(body_anchor(def)),
      )
    | _ => ([], [], None)
    }
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

/* path-qualified key: Geo.Dist.manhattan */
let qname = (path: list(string), name: string): string =>
  String.concat(".", path @ [name]);

/* scan a module's (statics-expanded, labeled-product) type for a member */
let rec sig_members = (t: Typ.t): list((string, Typ.t)) =>
  switch (t.term) {
  | Parens(t)
  | Projector(_, t) => sig_members(t)
  | Prod(xs) => List.concat_map(sig_members, xs)
  | TupLabel({term: Label(l), _}, t) => [(l, t)]
  | _ => []
  };

/* ---------- assembly ---------- */

/* a repeated definition (a second `type Pos`) would give two nodes one
   key, and the vdom two keyed children with one key — which breaks its
   child patching (insertBefore of nothing). Later duplicates get `#2`,
   `#3`…; references by key resolve to the first, as in HighLevelNodeMap. */
let uniquify_keys = (nodes: list(tynode)): list(tynode) => {
  let seen = Hashtbl.create(16);
  List.map(
    (n: tynode) => {
      let count =
        Option.value(Hashtbl.find_opt(seen, n.key), ~default=0) + 1;
      Hashtbl.replace(seen, n.key, count);
      count == 1
        ? n
        : {
          ...n,
          key: n.key ++ "#" ++ string_of_int(count),
        };
    },
    nodes,
  );
};

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
      | ([], IResult(e)) =>
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
  /* member types come from the enclosing module's labeled-product type,
     walked down the path */
  let lookup_path_type = (path: list(string), name: string): option(Typ.t) =>
    switch (path) {
    | [] => lookup_type(name)
    | [root, ...rest] =>
      List.fold_left(
        (acc, seg) =>
          Option.bind(acc, mty => List.assoc_opt(seg, sig_members(mty))),
        lookup_type(root),
        rest @ [name],
      )
    };
  /* internal type aliases, per path — references to them from member
     signatures resolve to the qualified node (nearest enclosing scope) */
  let internal_aliases: list((list(string), string)) =
    List.filter_map(
      fun
      | (path, IAlias(_, _, tpat, _)) when path != [] =>
        switch (tpat.term) {
        | Var(n) => Some((path, n))
        | _ => None
        }
      | _ => None,
      items,
    );
  let rec resolve_internal = (path: list(string), k: string): option(string) =>
    if (List.mem((path, k), internal_aliases)) {
      Some(qname(path, k));
    } else {
      switch (List.rev(path)) {
      | [] => None
      | [_, ...rev_parent] => resolve_internal(List.rev(rev_parent), k)
      };
    };
  /* qualified ty_ref: internal alias names map to their qualified keys */
  let ty_ref_at =
      (~path: list(string), ~anchor: string, ty: Typ.t): (string, node_kind) => {
    let (k, kind) = ty_ref(~anchor, ty);
    switch (kind) {
    | Alias
    | Ghost =>
      switch (resolve_internal(path, k)) {
      | Some(q) => (q, kind)
      | None => (k, kind)
      }
    | _ => (k, kind)
    };
  };

  /* Error ownership roots: the disjoint payload subtree of each item. */
  let item_root = (item: item): option(Id.t) =>
    switch (item) {
    | IAlias(_, _, _, ty) => Some(Typ.rep_id(ty))
    | ILet(_, _, _, def) => Some(Exp.rep_id(def))
    | ITest(_, body) => Some(Exp.rep_id(body))
    | IResult(_) => None
    };
  let roots = List.filter_map(((_, it)) => item_root(it), items);
  let err_roots: list(Id.t) =
    statics.error_ids
    |> List.filter_map(err_owner(~info_map, ~roots))
    |> List.sort_uniq(Id.compare);
  let root_has_err = (root: option(Id.t)): bool =>
    switch (root) {
    | Some(r) => List.exists(e => Id.compare(e, r) == 0, err_roots)
    | None => false
    };

  /* Aliases, in program order (module-internal ones get qualified keys,
     bare labels, and their module path). */
  let alias_nodes: list(tynode) =
    List.filter_map(
      fun
      | (path, IAlias(anchor, doc, tpat, ty)) => {
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
          let deps =
            List.sort_uniq(compare, ty_vars(ty))
            |> List.map(d =>
                 Option.value(resolve_internal(path, d), ~default=d)
               );
          Some(
            mk_node(
              ~n_id=anchor,
              ~m_path=path,
              ~kind=is_hole ? Ghost : Alias,
              ~ctrs=ctr_names(ty),
              ~n_ty=is_hole ? None : Some(pretty_ty(ty)),
              ~n_doc=doc,
              ~n_err=root_has_err(Some(Typ.rep_id(ty))),
              ~deps,
              ~label=name,
              path == [] ? name : qname(path, name),
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
  let ensure_grid =
      (~hole_path: list(string)=[], key: string, kind: node_kind): unit => {
    let kind =
      switch (kind) {
      | Alias when !List.mem(key, alias_keys) => Ghost /* undefined name */
      | k => k
      };
    /* anchored holes ("?<anchor>") are per-use unique, so an
       unannotated member's unknowns belong to that member's module —
       they dock at the former and the hull contains them */
    let m_path = String.length(key) > 0 && key.[0] == '?' ? hole_path : [];
    ensure(mk_node(~kind, ~m_path, ~label=display_label(key), key));
  };

  /* A builtin terminal duplicated per use-site, docked to [anchor_key]. */
  let ensure_sat =
      (
        ~m_path: list(string)=[],
        ~anchor_key: string,
        ~output: bool,
        ~dup: string,
        label: string,
      )
      : string => {
    let key = label ++ "@" ++ dup;
    ensure(
      mk_node(
        ~kind=Builtin,
        ~m_path,
        ~sat=Some((anchor_key, output)),
        ~label,
        key,
      ),
    );
    key;
  };

  /* ---- alias-body former folding ----
     An alias whose body is a tuple / list / sum IS that former: one
     object with a name. Its components feed the alias node directly
     (formation lines) and the node wears the former's glyph — no
     separate "()" / "[]" node. The replaced deps go to hidden_deps
     (still ordering the columns, no longer drawn). Anonymous formers
     (a tuple written inline in a signature) keep their own nodes. */
  let expanded_aliases: Hashtbl.t(string, unit) = Hashtbl.create(8);
  let alias_former: Hashtbl.t(string, (string, list(string))) =
    Hashtbl.create(8);
  let resolve_former_comp = (~path, ~alias_key, ~anchor, comp: Typ.t): string => {
    let (k, kind) = ty_ref_at(~path, ~anchor, comp);
    switch (kind) {
    | Builtin =>
      /* dup by the per-component anchor: (Int, Int) = two terminals */
      ensure_sat(~anchor_key=alias_key, ~output=false, ~dup=anchor, k)
    | _ =>
      ensure_grid(~hole_path=path, k, kind);
      k;
    };
  };
  List.iter(
    fun
    | (path, IAlias(_, _, tpat, ty)) => {
        let name =
          switch (tpat.term) {
          | Var(n) => n
          | _ => "?"
          };
        let key = path == [] ? name : qname(path, name);
        switch (unwrap_ty(ty).term) {
        | Prod(comps) when List.length(comps) > 1 =>
          let parts =
            List.mapi(
              (i, c) =>
                resolve_former_comp(
                  ~path,
                  ~alias_key=key,
                  ~anchor=key ++ "c" ++ string_of_int(i),
                  c,
                ),
              comps,
            );
          Hashtbl.replace(alias_former, key, ("()", parts));
          Hashtbl.replace(expanded_aliases, key, ());
        | List(el) =>
          let part =
            resolve_former_comp(
              ~path,
              ~alias_key=key,
              ~anchor=key ++ "el",
              el,
            );
          Hashtbl.replace(alias_former, key, ("[]", [part]));
          Hashtbl.replace(expanded_aliases, key, ());
        | Sum(_) => Hashtbl.replace(alias_former, key, ("+", []))
        | _ => ()
        };
      }
    | _ => (),
    items,
  );

  /* Bindings, phase 1: names, types, metadata. */
  let bindings:
    list(
      (
        list(string),
        string,
        Id.t,
        Typ.t,
        option(string),
        bool,
        bool,
        bool,
        (list(Id.t), list(Id.t), option(Id.t)),
        list(string),
        Exp.t,
      ),
    ) =
    List.concat_map(
      fun
      | (path, ILet(anchor, doc, pat, def)) => {
          let err = root_has_err(Some(Exp.rep_id(def)));
          let hole = exp_has_hole(def);
          /* module literals get a {} former node instead of their
             statics-expanded labeled-product type */
          let is_mod =
            switch (strip_exp(def).term) {
            | Module(_) => true
            | _ => false
            };
          let anatomy = fun_anatomy(~pat, def);
          /* Member types come from the DEFINITION'S OWN statics entry:
             it is typed where module-local aliases are still in scope,
             so (per the whnf invariant) names like Tally/Model survive.
             The module's labeled-product type is only a fallback — the
             sig-builder must expand local aliases (they cannot escape
             into the module's exported type), which is exactly where
             names died before. */
          let def_ty = (): option(Typ.t) =>
            switch (Id.Map.find_opt(Exp.rep_id(def), info_map)) {
            | Some(InfoExp(e)) => Some(Language.Info.exp_ty(e))
            | _ => None
            };
          /* def_ty types the whole definition: only primary when the
             pattern binds exactly that one name plainly (tuple pats
             split it; funlet heads' defs are just the body) */
          let rec plain_var = (p: Pat.t): bool =>
            switch (p.term) {
            | Var(_) => true
            | Parens(p)
            | Asc(p, _)
            | Projector(_, p) => plain_var(p)
            | _ => false
            };
          /* an ascribed name is the author's word: `start : World` docks at
             World even though the definition's own type is the joined,
             alias-free tuple (which made a structural Derived node) */
          let rec asc_of = (p: Pat.t): option(Typ.t) =>
            switch (p.term) {
            | Asc(_, ann) => Some(ann)
            | Parens(p)
            | Projector(_, p) => asc_of(p)
            | _ => None
            };
          let member_ty = (name: string): option(Typ.t) => {
            let looked = () =>
              path == [] ? lookup_type(name) : lookup_path_type(path, name);
            switch (plain_var(pat) ? asc_of(pat) : None) {
            | Some(ann) => Some(ann)
            | None =>
              plain_var(pat)
                ? switch (def_ty()) {
                  | Some(ty) => Some(ty)
                  | None => looked()
                  }
                : (
                  switch (looked()) {
                  | Some(ty) => Some(ty)
                  | None => def_ty()
                  }
                )
            };
          };
          pat_names(pat)
          |> List.filter_map(name =>
               member_ty(name)
               |> Option.map(ty =>
                    (
                      path,
                      name,
                      Option.value(anchor, ~default=Exp.rep_id(def)),
                      ty,
                      doc,
                      err,
                      hole,
                      is_mod,
                      anatomy,
                      List.sort_uniq(compare, exp_vars(def)),
                      def,
                    )
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
      | (_, ILet(_, _, _, def)) => exp_vars(def)
      | (_, IResult(e)) => exp_vars(e)
      | _ => [],
      items,
    );
  let use_count = (name: string): int =>
    List.length(List.filter(u => u == name, all_uses));

  let binding_names =
    List.map(((_, n, _, _, _, _, _, _, _, _, _)) => n, bindings);
  /* Bindings, phase 2: materialize nodes and edges/values. */
  let (edges_raw, values) =
    List.fold_left(
      (
        (es, vs),
        (
          path,
          name,
          id,
          ty,
          doc,
          err,
          hole,
          is_mod,
          (arg_ids, whole_ids, out_id),
          dvars,
          def,
        ),
      ) => {
        let qn = path == [] ? name : qname(path, name);
        let (args, ret) = flatten_arrow(ty);
        switch (args) {
        | [] =>
          let (v_key, v_kind) =
            if (is_mod) {
              ("{}" ++ "@" ++ qn, Product);
            } else if (path != []) {
              (
                /* member constants orbit their module's former node: the
                   hull contains them regardless of their value's type */
                "{}" ++ "@" ++ String.concat(".", path),
                Product,
              );
            } else {
              ty_ref_at(~path, ~anchor=qn, ty);
            };
          if (is_mod) {
            /* the module node: treat the module as having an implicit
               module TYPE of the same name — the node is labeled like a
               type node; its info panel shows the module value */
            ensure(
              mk_node(
                ~kind=Product,
                ~m_path=path @ [name],
                ~n_ty=Some(pretty_ty(ty)),
                ~label=name,
                v_key,
              ),
            );
          } else {
            ensure_grid(~hole_path=path, v_key, v_kind);
          };
          (
            es,
            vs
            @ [
              {
                v_name: qn,
                v_label: name,
                m_path: path,
                v_id: id,
                v_key,
                v_ty: pretty_ty(ty),
                v_err: err,
                v_def: def,
              },
            ],
          );
        | _ =>
          /* input node: single component, or a shared Product */
          let comp_refs =
            List.mapi(
              (i, a) => ty_ref_at(~path, ~anchor=qn ++ string_of_int(i), a),
              args,
            );
          let (ret_key, ret_kind) = ty_ref_at(~path, ~anchor=qn ++ "r", ret);
          let input_key =
            switch (comp_refs) {
            | [(k, Builtin)] when ret_kind != Builtin =>
              /* single builtin arg: terminal docked to the result node */
              ensure_grid(~hole_path=path, ret_key, ret_kind);
              ensure_sat(
                ~m_path=path,
                ~anchor_key=ret_key,
                ~output=false,
                ~dup=qn,
                k,
              );
            | [(k, kind)] =>
              ensure_grid(~hole_path=path, k, kind);
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
                List.mapi(
                  (ci, (k, kind)) =>
                    switch (kind) {
                    | Builtin =>
                      /* per-component dup: (Int, Int) must yield TWO
                         Int terminals, not a deduped one */
                      ensure_sat(
                        ~m_path=path,
                        ~anchor_key=product_key,
                        ~output=false,
                        ~dup=product_key ++ "c" ++ string_of_int(ci),
                        k,
                      )
                    | _ =>
                      ensure_grid(~hole_path=path, k, kind);
                      k;
                    },
                  comps,
                );
              ensure(
                mk_node(
                  ~kind=Product,
                  ~label="()",
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
                ~m_path=path,
                ~anchor_key=input_key,
                ~output=true,
                ~dup=qn,
                ret_key,
              )
            | _ =>
              ensure_grid(~hole_path=path, ret_key, ret_kind);
              ret_key;
            };
          (
            es
            @ [
              {
                e_name: qn,
                e_label: name,
                m_path: path,
                e_id: id,
                e_ty: pretty_ty(ty),
                e_src: input_key,
                dst: dst_key,
                e_doc: doc,
                e_err: err,
                e_hole: hole,
                main: path == [] && use_count(name) >= 2,
                tests: [],
                e_arg_ids: arg_ids,
                e_whole_ids: whole_ids,
                e_out_id: out_id,
                e_deps:
                  dvars
                  |> List.filter(v => v != name && List.mem(v, binding_names)),
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
      ((et, lt), (_, item)) =>
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

  let last_def =
    List.fold_left(
      (acc, (path, item)) =>
        path != []
          ? acc
          : (
            switch (item) {
            | IAlias(Some(id), _, tpat, _) =>
              switch (tpat.term) {
              | Var(n) => Some((n, id))
              | _ => acc
              }
            | ILet(Some(id), _, pat, _) =>
              switch (pat_names(pat)) {
              | [n, ..._] => Some((n, id))
              | [] => acc
              }
            | _ => acc
            }
          ),
      None,
      items,
    );
  let alias_nodes =
    List.map(
      (n: tynode) => {
        let n =
          Hashtbl.mem(expanded_aliases, n.key)
            ? {
              ...n,
              hidden_deps: n.deps,
            }
            : n;
        switch (Hashtbl.find_opt(alias_former, n.key)) {
        | Some((glyph, parts)) => {
            ...n,
            former: Some(glyph),
            parts,
          }
        | None => n
        };
      },
      alias_nodes,
    );
  {
    nodes: uniquify_keys(alias_nodes @ extras^),
    edges,
    values,
    loose_tests,
    last_def,
  };
};
