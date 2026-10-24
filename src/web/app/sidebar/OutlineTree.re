open Language;

/* the module/definition tree behind the outline sidebar: definitions,
   module members and nested lets, plus `…;` statements, `use … in` and
   trailing ⇒ rows. row ids are chain-item ids (as in ItemEdit) */

type kind =
  | KModule
  | KFn
  | KConst
  | KType
  | KTest /* one `test … end;` statement */
  | KTests /* container for a contiguous run of tests */
  | KStmt /* any other `…;` statement */
  | KUse /* `use X in`: no name, no cell; scopes over the rows below it */
  | KTrail; /* a block's trailing expression */

type node = {
  o_label: string,
  o_kind: kind,
  o_id: option(Id.t),
  /* KTest: the Test term's own id, for result lookup (o_id is the
     enclosing item, the open/jump/restructure handle) */
  o_test: option(Id.t),
  /* a named row's binding token, and its sort (Pat, TPat or MPat) */
  o_binder: option((Id.t, Sort.t)),
  o_children: list(node),
};

let rec strip_exp = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(e)
  | Projector(_, e)
  | Filter(_, e) => strip_exp(e)
  | _ => e
  };

/* the binding's name and its token: the first Var through wrappers,
   incl. the funlet head */
let rec pat_var = (p: Pat.t): option((string, Id.t)) =>
  switch (p.term) {
  | Var(x) => Some((x, Pat.rep_id(p)))
  | Parens(p)
  | Asc(p, _)
  | Projector(_, p)
  | TupLabel(_, p) => pat_var(p)
  | Ap(f, _) => pat_var(f)
  | Tuple([p, ..._]) => pat_var(p)
  | _ => None
  };

let row = (~id=?, ~label="", ~test=?, ~binder=?, ~kids=[], kind: kind): node => {
  o_label: label,
  o_kind: kind,
  o_id: id,
  o_test: test,
  o_binder: binder,
  o_children: kids,
};

let type_row = (id: Id.t, tp: TPat.t): list(node) =>
  switch (tp.term) {
  | Var(name) => [
      row(~id, ~label=name, ~binder=(TPat.rep_id(tp), Sort.TPat), KType),
    ]
  | _ => []
  };

/* a `…;` statement: a test, or any other */
let stmt_row = (id: Id.t, e: Exp.t): list(node) => {
  let h = strip_exp(e);
  switch (h.term) {
  | Test(_)
  | HintedTest(_) => [row(~id, ~test=Exp.rep_id(h), KTest)]
  | _ => [row(~id, KStmt)]
  };
};

let use_row = (id: Id.t, typ: Typ.t): node =>
  row(~id, ~label="use " ++ Typ.pretty_print(typ), KUse);

/* a block (the program, a function body) is an item chain: defs, `use`s,
   `…;` statements and the trailing body. a nested block shows its ⇒ row
   only beside other items; the top level always does (it anchors the
   result) */
let rec of_exp = (~top=false, e: Exp.t): list(node) => {
  let e = strip_exp(e);
  switch (e.term) {
  | Let(pat, def, body) =>
    let_row(Exp.rep_id(e), pat, def) @ of_exp(~top, body)
  | TyAlias(tpat, _, body) =>
    type_row(Exp.rep_id(e), tpat) @ of_exp(~top, body)
  | ModuleExp(mpat, def, body) =>
    module_row(Exp.rep_id(e), mpat, def) @ of_exp(~top, body)
  | Use(typ, body) => [use_row(Exp.rep_id(e), typ), ...of_exp(~top, body)]
  | Seq(e1, body) => stmt_row(Exp.rep_id(e), e1) @ of_exp(~top, body)
  /* a Module root: its items are the program's top level, with no
     wrapper row */
  | Module(items) when top => of_mod(items)
  | _ when top => [row(~id=Exp.rep_id(e), KTrail)]
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
      | Use(_, body)
      | Seq(_, body) => tail_of(body)
      | _ => e
      };
    };
    items @ [row(~id=Exp.rep_id(tail_of(fbody)), KTrail)];
  };
}

and mk_def =
    (~id: Id.t, ~binder: (Id.t, Sort.t), name: string, def: Exp.t): node => {
  let def = strip_exp(def);
  let (kind, kids) =
    switch (def.term) {
    | Module(items) => (KModule, of_mod(items))
    | Fun(_, fbody, _, _)
    | TypFun(_, {term: Fun(_, fbody, _, _), _}, _) => (
        KFn,
        of_block(fbody),
      )
    | _ => (KConst, of_exp(def))
    };
  row(~id, ~label=name, ~binder, ~kids, kind);
}

and let_row = (id: Id.t, pat: Pat.t, def: Exp.t): list(node) =>
  switch (pat_var(pat)) {
  /* `let f(x) = …` keeps its parameters in the pattern: [def] is the
     function's body, as `fun x -> …`'s is */
  | Some((name, binder)) when Option.is_some(FunctionSugar.detect(pat)) => [
      row(
        ~id,
        ~label=name,
        ~binder=(binder, Sort.Pat),
        ~kids=of_block(def),
        KFn,
      ),
    ]
  | Some((name, binder)) => [
      mk_def(~id, ~binder=(binder, Sort.Pat), name, def),
    ]
  | None => []
  }

/* `module M = …`, or `module M : S = …`: the name under its signature.
   A module whatever its definition, an alias (`module Db = Base`) too */
and module_row = (id: Id.t, mpat: MPat.t, def: Exp.t): list(node) =>
  switch (mpat.term) {
  | Var(name) => [
      {
        ...mk_def(~id, ~binder=(MPat.rep_id(mpat), Sort.MPat), name, def),
        o_kind: KModule,
      },
    ]
  | Asc(inner, _) => module_row(id, inner, def)
  | _ => []
  }

and of_mod = (items: list(Mod.t)): list(node) =>
  List.concat_map(
    (m: Mod.t) =>
      switch (m.term) {
      | ModLet(pat, def) => let_row(Mod.rep_id(m), pat, def)
      | ModType(tpat, _) => type_row(Mod.rep_id(m), tpat)
      | ModuleMod(mpat, def) => module_row(Mod.rep_id(m), mpat, def)
      | ModExp(e) =>
        switch (strip_exp(e).term) {
        | EmptyHole => []
        | _ => stmt_row(Mod.rep_id(m), e)
        }
      /* an evaluated binding: a module value, never source */
      | ModVal(_)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_) => []
      },
    items,
  );

/* ---- rows from the program's outline (Haz3lcore.Outline) ---- */

module O = Haz3lcore.Outline;
module Piece = Haz3lcore.Piece;
module Tile = Haz3lcore.Tile;
module Form = Haz3lcore.Form;
module Printer = Haz3lcore.Printer;

/* a let's row name and token: its pattern's leading variable, through
   parentheses, projectors, a function head, an ascription, a label and a
   tuple's first part */
let rec lead_var = (pat: Haz3lcore.Segment.t): option((string, Id.t)) => {
  let c = O.content(pat);
  let rec first = (ps: list(Piece.t)) =>
    switch (ps) {
    | [Tile(t), ..._] when Tile.is_comma(t) => []
    | [p, ...ps] => [p, ...first(ps)]
    | [] => []
    };
  let first = first(c);
  switch (first) {
  | [Tile({children: [inner], _} as t)] when Tile.label(t) == ["(", ")"] =>
    lead_var(inner)
  | [Projector(pr)] => lead_var([pr.syntax])
  | [Tile(v), ...rest] =>
    switch (Tile.single_token(v), rest) {
    | (Some(x), [Tile(lab), ...inner]) when Tile.is_tuple_label_eq(lab) =>
      Token.is_var(x) ? lead_var(inner) : None
    | (Some(x), [])
    | (Some(x), [Tile(_), ..._]) when Token.is_var(x) => Some((x, v.id))
    | _ => None
    }
  | _ => None
  };
};

/* the rows of a block's entries; the term walk stopped at a form that
   isn't a binding (a theorem) or a let under a function head, which it
   read as the block's result */
let rec rows_of = (~top: bool, ~heads=false, es: list(O.entry)): list(node) =>
  switch (es) {
  | [] => []
  | [e, ...es] =>
    switch (opaque(~heads, e)) {
    | Some(id) => top ? [row(~id, KTrail)] : []
    | None => row_of(~top, e) @ rows_of(~top, es)
    }
  }

/* [heads]: [e] is a function body's first entry, after the function's
   own `fun x ->` */
and opaque = (~heads, e: O.entry): option(Id.t) =>
  switch (e.kind, O.head(e.lv, e.k)) {
  | (Def, Some(h)) when Tile.token(h, 0) == "theorem" => Some(h.id)
  /* `fun x ->` before it, in its slice, but a function body's own head */
  | (Def, Some(_)) when !heads && !O.in_block(e) =>
    let sp = e.lv.spans[e.k];
    let pre =
      Array.to_list(Array.sub(e.lv.pieces, sp.lo, fst(sp.core) - sp.lo));
    switch (O.content(pre)) {
    | [p, ..._] => Some(Piece.id(p))
    | [] => None
    };
  | _ => None
  }

and row_of = (~top: bool, e: O.entry): list(node) => {
  let id = e.id;
  let at_mod = e.lv.sort == Sort.Mod;
  switch (e.kind) {
  | Def =>
    switch (O.parts(e.lv, e.k)) {
    | None => []
    | Some((pat, def)) =>
      switch (lead_var(pat)) {
      | None => []
      | Some((name, tok)) =>
        let binder = (tok, Sort.Pat);
        let below = below(e, def);
        let (kind, kids) =
          if (e.fn) {
            (KFn, block_rows(below));
          } else if (e.literal) {
            (KModule, rows_of(~top=false, below));
          } else if (starts_abstraction(def)) {
            (KConst, []);
          } else {
            (KConst, rows_of(~top=false, below));
          };
        [row(~id, ~label=name, ~binder, ~kids, kind)];
      }
    }
  | Type =>
    switch (e.names) {
    | [(name, tok), ..._] => [
        row(~id, ~label=name, ~binder=(tok, Sort.TPat), KType),
      ]
    | [] => []
    }
  | Module =>
    switch (e.names) {
    | [(name, tok), ..._] => [
        row(
          ~id,
          ~label=name,
          ~binder=(tok, Sort.MPat),
          ~kids=rows_of(~top=false, e.children),
          KModule,
        ),
      ]
    | [] => []
    }
  | Test => [row(~id, ~test=?e.test, KTest)]
  | Stmt when at_mod && hole_only(e) => []
  | Stmt => [row(~id, KStmt)]
  | Use =>
    switch (use_type(e)) {
    | Some(label) => [row(~id, ~label, KUse)]
    | None => []
    }
  /* a Mod root's result is a member, as the parser reads it */
  | Tail when at_mod =>
    switch (O.test_tile(e.lv, e.k), test_member(e)) {
    | (Some(test), true) => [row(~id, ~test, KTest)]
    | _ => hole_only(e) ? [] : [row(~id, KStmt)]
    }
  | Tail =>
    switch (wrapped(O.core(e.lv, e.k))) {
    | Some(inner) =>
      switch (O.build(~root=Sort.Exp, inner).entries) {
      | [{kind: Tail, _}] => top ? [row(~id, KTrail)] : []
      | es => rows_of(~top, es)
      }
    | None => top ? [row(~id, KTrail)] : []
    }
  };
}

/* a definition that is a type abstraction (not over one function) */
and starts_abstraction = (def: Haz3lcore.Segment.t): bool =>
  switch (O.content(Option.value(wrapped(def), ~default=def))) {
  | [p, ..._] => O.head_label(p) == Some(["typfun", "->"])
  | [] => false
  }

/* a definition's or result's expression inside parentheses or a
   projector (a fold), which the spine doesn't open: the term walk read
   through them */
and wrapped = (seg: Haz3lcore.Segment.t): option(Haz3lcore.Segment.t) => {
  let rec strip = (seg: Haz3lcore.Segment.t) =>
    switch (O.content(seg)) {
    | [Tile({children: [inner], _} as t)]
        when Tile.label(t) == ["(", ")"] =>
      strip(inner)
    | [Projector(pr)] => strip([pr.syntax])
    | _ => seg
    };
  switch (O.content(seg)) {
  | [Tile({children: [_], _} as t)] when Tile.label(t) == ["(", ")"] =>
    Some(strip(seg))
  | [Projector(_)] => Some(strip(seg))
  | _ => None
  };
}

/* an entry's children as the term walk saw them */
and below = (e: O.entry, def: Haz3lcore.Segment.t): list(O.entry) =>
  switch (wrapped(def)) {
  | Some(inner) => O.build(~root=Sort.Exp, inner).entries
  | None => e.children
  }

/* a function body's rows, and its result's if it has others */
and block_rows = (es: list(O.entry)): list(node) =>
  switch (rows_of(~top=false, ~heads=true, es)) {
  | [] => []
  | rows =>
    switch (List.find_opt((e: O.entry) => e.kind == Tail, es)) {
    | Some(t) => rows @ [row(~id=t.id, KTrail)]
    | None => rows
    }
  }

and hole_only = (e: O.entry): bool =>
  switch (O.content(O.core(e.lv, e.k))) {
  | []
  | [Grout(_)] => true
  | _ => false
  }

and test_member = (e: O.entry): bool =>
  switch (O.content(O.core(e.lv, e.k))) {
  | [Tile(t)] =>
    t.form == Form.Compound(Test) || t.form == Form.Compound(HintedTest)
  | _ => false
  }

/* `use M in`'s row label; none for a filter */
and use_type = (e: O.entry): option(string) =>
  switch (O.content(O.core(e.lv, e.k))) {
  | [Tile({children: [typ], _} as t), ..._] when Tile.token(t, 0) == "use" =>
    Some(
      "use "
      ++ (
        Printer.of_segment(~holes="?", typ)
        |> String.split_on_char('\n')
        |> List.map(String.trim)
        |> String.concat(" ")
        |> String.trim
      ),
    )
  | _ => None
  };

/* each run of ≥2 tests, at any level, goes under a container row; a lone
   test stays flat */
let rec group_tests = (ns: list(node)): list(node) =>
  switch (ns) {
  | [] => []
  | [{o_kind: KTest, _} as t1, {o_kind: KTest, _} as t2, ...rest] =>
    let (run, rest) = take_tests([t2, ...rest], [t1]);
    [
      row(~label="tests", ~kids=List.rev(run), KTests),
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

/* the rows of a program's outline */
let of_outline = (o: O.t): list(node) =>
  rows_of(~top=true, o.entries) |> group_tests |> number_tests;

/* memoized on the term's physical identity, which statics keeps until
   the program changes */
let cache: Util.Slot.t(Exp.t, list(node)) = Util.Slot.mk();

let of_term = (e: Exp.t): list(node) =>
  Util.Slot.get(cache, e, () =>
    of_exp(~top=true, e) |> group_tests |> number_tests
  );

/* every row of [ns], each before its children */
let rec flatten = (ns: list(node)): list(node) =>
  List.concat_map(n => [n, ...flatten(n.o_children)], ns);

/* their ids, in that order */
let collect = (ns: list(node)): list(Id.t) =>
  List.filter_map(n => n.o_id, flatten(ns));

/* the rows [errors] badge, among [rows]: each error's deepest row in its
   item, and the rows holding it past that, modules included, whose
   roll-up badge shows while they are collapsed */
let error_rows =
    (~info_map: Id.Map.t(Info.t), ~rows: list(Id.t), errors: list(Id.t))
    : (list(Id.t), list(Id.t)) => {
  let in_rows = id => List.mem(id, rows);
  List.fold_left(
    ((direct, roll), err) => {
      let (own, held) =
        switch (Id.Map.find_opt(err, info_map)) {
        | Some(info) => (
            [err, ...Info.within_item(~info_map, info)],
            [err, ...Info.containers(~info_map, info)],
          )
        | None => ([err], [err])
        };
      let holders = List.filter(in_rows, held);
      switch (List.filter(in_rows, own)) {
      | [] => (direct, holders @ roll)
      | [deepest, ..._] => (
          [deepest, ...direct],
          List.filter(id => !Id.equal(id, deepest), holders) @ roll,
        )
      };
    },
    ([], []),
    errors,
  );
};

/* the row with id [fid], and the rows above it outermost first */
let find = (fid: Id.t, e: Exp.t): option((list(node), node)) => {
  let rec go = (above, ns: list(node)) =>
    List.find_map(
      n =>
        n.o_id == Some(fid)
          ? Some((List.rev(above), n)) : go([n, ...above], n.o_children),
      ns,
    );
  go([], of_term(e));
};

let node_of = (fid: Id.t, e: Exp.t): option(node) =>
  Option.map(snd, find(fid, e));

let kind_of = (fid: Id.t, e: Exp.t): option(kind) =>
  Option.map(n => n.o_kind, node_of(fid, e));

/* ancestor labels of the node with id [fid], outermost first, for the
   stacked header's qualifier chip (["Geo"] for a member of module Geo) */
let path_of = (fid: Id.t, e: Exp.t): list(string) =>
  switch (find(fid, e)) {
  | Some((above, _)) => List.map(n => n.o_label, above)
  | None => []
  };

/* ids of the rows from the top level down to [fid], inclusive; rows
   without ids (test containers) are skipped */
let trail_of = (fid: Id.t, e: Exp.t): option(list(Id.t)) =>
  Option.map(
    ((above, n)) => List.filter_map(n => n.o_id, above @ [n]),
    find(fid, e),
  );

/* ids below [fid] (not fid itself): pinning a parent unpins its pinned
   descendants */
let descendant_ids = (fid: Id.t, e: Exp.t): list(Id.t) =>
  switch (node_of(fid, e)) {
  | Some(n) => collect(n.o_children)
  | None => []
  };

/* the row that takes [fid]'s place once it's deleted: the next sibling,
   else the one before, else its parent */
let successor = (fid: Id.t, e: Exp.t): option(Id.t) =>
  Option.bind(
    find(fid, e),
    ((above, _)) => {
      let siblings =
        switch (Util.ListUtil.last_opt(above)) {
        | Some(parent) => parent.o_children
        | None => of_term(e)
        };
      let rec split = (before, xs: list(node)) =>
        switch (xs) {
        | [n, ...rest] when n.o_id == Some(fid) => (before, rest)
        | [n, ...rest] => split([n, ...before], rest)
        | [] => (before, [])
        };
      let (before, after) = split([], siblings);
      let first_id = List.find_map((n: node) => n.o_id);
      switch (first_id(after), first_id(before)) {
      | (Some(id), _)
      | (None, Some(id)) => Some(id)
      | (None, None) => first_id(List.rev(above))
      };
    },
  );

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
   resolve_path and the sidebar's fold paths must all count here */
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
      switch (n.o_kind) {
      | KTest
      | KTests
      | KUse => ()
      | _ when n.o_label == "" => ()
      | _ => prev := n.o_label
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

/* [fid]'s row and the rows above it, outermost first, each with its
   path segment */
let find_path = (fid: Id.t, e: Exp.t): option(list((node, path_seg))) =>
  Option.map(
    ((above, n)) =>
      snd(
        List.fold_left_map(
          (siblings, m) =>
            (m.o_children, (m, List.assq(m, segs(siblings)))),
          of_term(e),
          above @ [n],
        ),
      ),
    find(fid, e),
  );

let label_path = (fid: Id.t, e: Exp.t): option(path) =>
  Option.map(List.map(snd), find_path(fid, e));

/* the run of tests holding row [fid], by path */
let run_of = (fid: Id.t, e: Exp.t): option(path) =>
  switch (Option.map(List.rev, find_path(fid, e))) {
  | Some([_, ({o_kind: KTests, _}, _), ..._] as up) =>
    Some(List.rev_map(snd, List.tl(up)))
  | _ => None
  };

/* the folds chosen by hand, by path: each row's last choice (true:
   folded). A row without one takes its kind's default */
[@deriving (show({with_path: false}), sexp, yojson)]
type folds = list((path, bool));

/* a run of tests starts folded, every other row open */
let folded_by_default = (k: kind): bool => k == KTests;

let folded = (~folds: folds, path: path, k: kind): bool =>
  switch (List.assoc_opt(path, folds)) {
  | Some(f) => f
  | None => folded_by_default(k)
  };

/* [folds] with row [path]'s choice made */
let choose = (path: path, f: bool, folds: folds): folds => [
  (path, f),
  ...List.remove_assoc(path, folds),
];

/* row [fid] shows its children: it is a row, and not folded */
let expanded = (~folds: folds, fid: Id.t, e: Exp.t): bool =>
  switch (find_path(fid, e)) {
  | Some(chain) =>
    let (n, _) = ListUtil.last(chain);
    !folded(~folds, List.map(snd, chain), n.o_kind);
  | None => false
  };

/* the row at [path], if it's there still */
let node_at = (path: path, e: Exp.t): option(node) => {
  let find = (seg: path_seg, ns: list(node)): option(node) =>
    segs(ns) |> List.find_opt(((_, s)) => s == seg) |> Option.map(fst);
  let rec go = (path, ns: list(node)) =>
    switch (path) {
    | [] => None
    | [last] => find(last, ns)
    | [hd, ...rest] =>
      Option.bind(find(hd, ns), n => go(rest, n.o_children))
    };
  go(path, of_term(e));
};

let resolve_path = (path: path, e: Exp.t): option(Id.t) =>
  Option.bind(node_at(path, e), n => n.o_id);

/* row [path]'s fold as [e] shows it, flipped; none once it's gone */
let flipped = (~folds: folds, path: path, e: Exp.t): option(bool) =>
  Option.map(
    (n: node) => !folded(~folds, path, n.o_kind),
    node_at(path, e),
  );

/* the headerless rows' ids (⇒ and `;`) */
let headless_row_ids = (e: Exp.t): Id.Map.t(unit) =>
  List.fold_left(
    (acc, n) =>
      switch (n.o_id) {
      | Some(id) when n.o_kind == KTrail || n.o_kind == KStmt =>
        Id.Map.add(id, (), acc)
      | _ => acc
      },
    Id.Map.empty,
    flatten(of_term(e)),
  );

/* every row id, memoized on the term like [of_term] */
let row_ids_cache: Slot.t(Exp.t, Id.Map.t(unit)) = Slot.mk();
let row_ids = (e: Exp.t): Id.Map.t(unit) =>
  Slot.get(row_ids_cache, e, () =>
    List.fold_left(
      (acc, id) => Id.Map.add(id, (), acc),
      Id.Map.empty,
      collect(of_term(e)),
    )
  );

/* the row holding [id]: its own, else its nearest ancestor's in its item */
let enclosing_row =
    (~info_map: Statics.Map.t, id: Id.t, e: Exp.t): option(Id.t) =>
  switch (Id.Map.find_opt(id, info_map)) {
  | Some(info) =>
    let rows = row_ids(e);
    Id.Map.mem(id, rows)
      ? Some(id)
      : Info.find_within_item(~info_map, info, x => Id.Map.mem(x, rows));
  | None => None
  };
