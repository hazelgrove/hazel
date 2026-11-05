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
  /* KTest: its hint (`hint "…" test … end`), the row's name */
  o_hint: option(string),
  /* a named row's binding token, and its sort (Pat, TPat or MPat) */
  o_binder: option((Id.t, Sort.t)),
  o_children: list(node),
};

let row =
    (~id=?, ~label="", ~test=?, ~hint=?, ~binder=?, ~kids=[], kind: kind)
    : node => {
  o_label: label,
  o_kind: kind,
  o_id: id,
  o_test: test,
  o_hint: hint,
  o_binder: binder,
  o_children: kids,
};

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
  | [Projector(pr)] => lead_var(pr.syntax)
  | [Tile(v), ...rest] =>
    switch (Tile.single_token(v), rest) {
    | (Some(x), [Tile(lab), ...inner]) when Tile.is_tuple_label_eq(lab) =>
      Token.is_var(x) ? lead_var(inner) : None
    | (Some(x), [])
    | (Some(x), [Tile(_), ..._]) when Haz3lcore.EntryPath.is_binder(x) =>
      Some((x, v.id))
    | _ => None
    }
  | _ => None
  };
};

/* each entry's rows, while it's the same entry (Outline keeps an
   unchanged entry's object) */
let entry_memo: Util.Swept.t((Id.t, bool), (O.entry, list(node))) =
  Util.Swept.mk();

/* the rows of a block's entries; a form that isn't a binding (a theorem)
   or a let under a function head ends the block, as its result */
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

and row_of = (~top: bool, e: O.entry): list(node) =>
  switch (Util.Swept.find_opt(entry_memo, (e.anchor, top))) {
  | Some((e', rows)) when e' === e => rows
  | _ =>
    let rows = entry_rows(~top, e);
    Util.Swept.replace(entry_memo, (e.anchor, top), (e, rows));
    rows;
  }

and entry_rows = (~top: bool, e: O.entry): list(node) => {
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
  | Test => [row(~id, ~test=?e.test, ~hint=?named(e.hint), KTest)]
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
    | (Some(test), true) =>
      let (c0, c1) = e.lv.spans[e.k].core;
      let hint =
        Option.join(Haz3lcore.EntryPath.test_of(e.lv, c0, c1)) |> named;
      [row(~id, ~test, ~hint?, KTest)];
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
   projector (a fold), which the spine doesn't open: the rows read
   through them */
and wrapped = (seg: Haz3lcore.Segment.t): option(Haz3lcore.Segment.t) => {
  let rec strip = (seg: Haz3lcore.Segment.t) =>
    switch (O.content(seg)) {
    | [Tile({children: [inner], _} as t)]
        when Tile.label(t) == ["(", ")"] =>
      strip(inner)
    | [Projector(pr)] => strip(pr.syntax)
    | _ => seg
    };
  switch (O.content(seg)) {
  | [Tile({children: [_], _} as t)] when Tile.label(t) == ["(", ")"] =>
    Some(strip(seg))
  | [Projector(_)] => Some(strip(seg))
  | _ => None
  };
}

/* an entry's children as rows */
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
  | [Tile(t)] => Tile.is_explicit_hole(t)
  | _ => false
  }

/* a hint that names its test: an empty one doesn't */
and named = (hint: option(string)): option(string) =>
  switch (hint) {
  | Some("") => None
  | h => h
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

/* each run of tests, at any level, a lone test's too, goes under a
   container row at its tests' level */
let rec group_tests = (ns: list(node)): list(node) =>
  switch (ns) {
  | [] => []
  | [{o_kind: KTest, _} as t, ...rest] =>
    let (run, rest) = take_tests(rest, [t]);
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

/* name the tests: by hint, else by number in SOURCE order, program-wide
   (a hinted test counts, so a hint added leaves the others' numbers) */
let name_tests = (ns: list(node)): list(node) => {
  let k = ref(0);
  let rec go = (ns: list(node)) =>
    List.map(
      n =>
        switch (n.o_kind) {
        | KTest =>
          incr(k);
          {
            ...n,
            o_label: Option.value(n.o_hint, ~default=string_of_int(k^)),
          };
        | _ => {
            ...n,
            o_children: go(n.o_children),
          }
        },
      ns,
    );
  go(ns);
};

/* the item a row's outline ops act on: its own, or a run of one's test */
let item_of = (n: node): option(Id.t) =>
  switch (n.o_id, n.o_kind, n.o_children) {
  | (None, KTests, [t]) => t.o_id
  | (id, _, _) => id
  };

/* the rows of a program's outline */
let build = (o: O.t): list(node) => {
  Util.Swept.next_pass(entry_memo);
  rows_of(~top=true, o.entries) |> group_tests |> name_tests;
};

/* the outline the rows are of: a program's (Program.outline) */
type t = O.t;

/* memoized on the outline, which is kept while the program's pieces are;
   a few, as programs take turns */
let cache: ref(list((t, list(node)))) = ref([]);
let rows = (o: t): list(node) =>
  switch (List.find_opt(((o', _)) => o' === o, cache^)) {
  | Some((_, r)) => r
  | None =>
    let r = build(o);
    cache := [(o, r), ...Util.ListUtil.take(3, cache^)];
    r;
  };

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
let find = (fid: Id.t, o: t): option((list(node), node)) => {
  let rec go = (above, ns: list(node)) =>
    List.find_map(
      n =>
        n.o_id == Some(fid)
          ? Some((List.rev(above), n)) : go([n, ...above], n.o_children),
      ns,
    );
  go([], rows(o));
};

let node_of = (fid: Id.t, o: t): option(node) =>
  Option.map(snd, find(fid, o));

let kind_of = (fid: Id.t, o: t): option(kind) =>
  Option.map(n => n.o_kind, node_of(fid, o));

/* ancestor labels of the node with id [fid], outermost first, for the
   stacked header's qualifier chip (["Geo"] for a member of module Geo) */
let path_of = (fid: Id.t, o: t): list(string) =>
  switch (find(fid, o)) {
  | Some((above, _)) => List.map(n => n.o_label, above)
  | None => []
  };

/* ids of the rows from the top level down to [fid], inclusive; rows
   without ids (test containers) are skipped */
let trail_of = (fid: Id.t, o: t): option(list(Id.t)) =>
  Option.map(
    ((above, n)) => List.filter_map(n => n.o_id, above @ [n]),
    find(fid, o),
  );

/* ids below [fid] (not fid itself): pinning a parent unpins its pinned
   descendants */
let descendant_ids = (fid: Id.t, o: t): list(Id.t) =>
  switch (node_of(fid, o)) {
  | Some(n) => collect(n.o_children)
  | None => []
  };

/* the row that takes [fid]'s place once it's deleted: the next sibling,
   else the one before, else its parent. A run's only test goes with its
   run, which then takes the run's place */
let successor = (fid: Id.t, o: t): option(Id.t) =>
  Option.bind(
    find(fid, o),
    ((above, n)) => {
      let (above, n) =
        switch (Util.ListUtil.split_last_opt(above)) {
        | Some((up, {o_kind: KTests, o_children: [_], _} as run)) => (
            up,
            run,
          )
        | _ => (above, n)
        };
      let siblings =
        switch (Util.ListUtil.last_opt(above)) {
        | Some(parent) => parent.o_children
        | None => rows(o)
        };
      let rec split = (before, xs: list(node)) =>
        switch (xs) {
        | [m, ...rest] when m === n => (before, rest)
        | [m, ...rest] => split([m, ...before], rest)
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
let find_path = (fid: Id.t, o: t): option(list((node, path_seg))) =>
  Option.map(
    ((above, n)) =>
      snd(
        List.fold_left_map(
          (siblings, m) =>
            (m.o_children, (m, List.assq(m, segs(siblings)))),
          rows(o),
          above @ [n],
        ),
      ),
    find(fid, o),
  );

let label_path = (fid: Id.t, o: t): option(path) =>
  Option.map(List.map(snd), find_path(fid, o));

/* the run of tests holding row [fid], by path */
let run_of = (fid: Id.t, o: t): option(path) =>
  switch (Option.map(List.rev, find_path(fid, o))) {
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
let expanded = (~folds: folds, fid: Id.t, o: t): bool =>
  switch (find_path(fid, o)) {
  | Some(chain) =>
    let (n, _) = ListUtil.last(chain);
    !folded(~folds, List.map(snd, chain), n.o_kind);
  | None => false
  };

/* the row at [path], if it's there still */
let node_at = (path: path, o: t): option(node) => {
  let find = (seg: path_seg, ns: list(node)): option(node) =>
    segs(ns) |> List.find_opt(((_, s)) => s == seg) |> Option.map(fst);
  let rec go = (path, ns: list(node)) =>
    switch (path) {
    | [] => None
    | [last] => find(last, ns)
    | [hd, ...rest] =>
      Option.bind(find(hd, ns), n => go(rest, n.o_children))
    };
  go(path, rows(o));
};

let resolve_path = (path: path, o: t): option(Id.t) =>
  Option.bind(node_at(path, o), n => n.o_id);

/* row [path]'s fold as [e] shows it, flipped; none once it's gone */
let flipped = (~folds: folds, path: path, o: t): option(bool) =>
  Option.map(
    (n: node) => !folded(~folds, path, n.o_kind),
    node_at(path, o),
  );

/* the headerless rows' ids (⇒ and `;`) */
let headless_row_ids = (o: t): Id.Map.t(unit) =>
  List.fold_left(
    (acc, n) =>
      switch (n.o_id) {
      | Some(id) when n.o_kind == KTrail || n.o_kind == KStmt =>
        Id.Map.add(id, (), acc)
      | _ => acc
      },
    Id.Map.empty,
    flatten(rows(o)),
  );

/* every row id, memoized on the outline like [rows] */
let row_ids_cache: Slot.t(t, Id.Map.t(unit)) = Slot.mk();
let row_ids = (o: t): Id.Map.t(unit) =>
  Slot.get(row_ids_cache, o, () =>
    List.fold_left(
      (acc, id) => Id.Map.add(id, (), acc),
      Id.Map.empty,
      collect(rows(o)),
    )
  );

/* a definition that is a livelit's use, ^^livelit(...): one with a GUI */
let rec is_livelit_use = (e: Exp.t): bool =>
  switch (e.term) {
  | Parens(e) => is_livelit_use(e)
  | Projector(pd, _) => pd.kind == ProjectorKind.Livelit
  | _ => false
  };

/* the rows whose definition is a livelit's use, in program order, at
   every level: what the outline's UX mode shows unless told otherwise.
   Memoized on the term like [row_ids]. */
let livelit_rows_cache: Slot.t(Exp.t, list(Id.t)) = Slot.mk();
let livelit_row_ids = (e: Exp.t): list(Id.t) =>
  Slot.get(
    livelit_rows_cache,
    e,
    () => {
      let rec exp = (acc, e: Exp.t) =>
        switch (e.term) {
        | Parens(e)
        | Filter(_, e) => exp(acc, e)
        | Projector(pd, e) =>
          pd.kind == ProjectorKind.Livelit ? acc : exp(acc, e)
        | Let(_, def, body) =>
          exp(
            is_livelit_use(def) ? [Exp.rep_id(e), ...acc] : exp(acc, def),
            body,
          )
        | ModuleExp(_, def, body) => exp(exp(acc, def), body)
        | TyAlias(_, _, body)
        | Seq(_, body) => exp(acc, body)
        | Module(items) => List.fold_left(item, acc, items)
        | _ => acc
        }
      and item = (acc, m: Mod.t) =>
        switch (m.term) {
        | ModLet(_, def) =>
          is_livelit_use(def) ? [Mod.rep_id(m), ...acc] : exp(acc, def)
        | ModuleMod(_, def) => exp(acc, def)
        | _ => acc
        };
      List.rev(exp([], e));
    },
  );
/* the row holding [id]: its own, else its nearest ancestor's in its item */
let enclosing_row = (~info_map: Statics.Map.t, id: Id.t, o: t): option(Id.t) =>
  switch (Id.Map.find_opt(id, info_map)) {
  | Some(info) =>
    let rows = row_ids(o);
    Id.Map.mem(id, rows)
      ? Some(id)
      : Info.find_within_item(~info_map, info, x => Id.Map.mem(x, rows));
  | None => None
  };
