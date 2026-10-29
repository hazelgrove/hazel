open Alcotest;
open Haz3lcore;
open Language;

/* every outline row opens as its own item: a ⇒ or `;` row as a headless
   cell holding just its expression or statement, a definition under its
   pattern, a test run as one cell; and joining gives back the program,
   with its own pieces. A `use` row opens none. What a cell should hold
   comes from the parsed term (each term's span in its level), not from
   the cell code under test */

module Divided = Web.Divided;
module Focus = Web.ScratchFocus;
module O = Web.OutlineTree;
module V = Web.SlideView;

let text_of = (seg: Segment.t): string =>
  seg |> Zipper.unzip |> MarkerParse.to_text;

/* edge whitespace and comments off: a statement's cell keeps the comments
   above it, which no term spans */
let trim = (seg: Segment.t): Segment.t => {
  let rec drop =
    fun
    | [p, ...rest] when Piece.is_secondary(p) => drop(rest)
    | ps => ps;
  seg |> drop |> List.rev |> drop |> List.rev;
};

/* what a row's cell holds, as the rep ids of terms */
type want =
  /* its expression or statement, with the parentheses or projector a ⇒
     row's id reads through */
  | Headless(Id.t)
  /* a pattern over a definition: a let, type or module, or a theorem */
  | Def(Id.t, Id.t)
  /* a module literal under its name: its members, if braced */
  | Module(Id.t, Id.t)
  /* no cell: a `use` */
  | Shut;

/* a let's name: the first Var through wrappers, as rows are named */
let rec pat_var = (p: Pat.t): option(string) =>
  switch (p.term) {
  | Var(x) => Some(x)
  | Parens(p)
  | Asc(p, _)
  | Projector(_, p)
  | TupLabel(_, p) => pat_var(p)
  | Ap(f, _) => pat_var(f)
  | Tuple([p, ..._]) => pat_var(p)
  | _ => None
  };

/* the outline's rows, read from the term, with each row's terms */
let wants = (term: Exp.t): Id.Map.t(want) => {
  let out = ref(Id.Map.empty);
  let add = (id, w) => out := Id.Map.add(id, w, out^);
  let rec strip = (e: Exp.t): Exp.t =>
    switch (e.term) {
    | Parens(e)
    | Projector(_, e)
    | Filter(_, e) => strip(e)
    | _ => e
    };
  /* a block's ⇒: a theorem's cell holds its statement */
  let tail = (e: Exp.t) =>
    switch (strip(e).term) {
    | Theorem(pat, def, _) => Def(Pat.rep_id(pat), Exp.rep_id(def))
    | _ => Headless(Exp.rep_id(e))
    };
  let def_row = (id, header, def: Exp.t) =>
    add(
      id,
      switch (strip(def).term) {
      | Module(_) => Module(header, Exp.rep_id(def))
      | _ => Def(header, Exp.rep_id(def))
      },
    );
  /* the rows at this level, counted: a block shows its ⇒ beside items */
  let rec of_exp = (~top, e: Exp.t): int => {
    let s = strip(e);
    switch (s.term) {
    | Let(pat, def, body) =>
      let n = let_row(Exp.rep_id(s), pat, def);
      n + of_exp(~top, body);
    | TyAlias(tpat, typ, body) =>
      let n = type_row(Exp.rep_id(s), tpat, typ);
      n + of_exp(~top, body);
    | ModuleExp(mpat, def, body) =>
      let n = module_row(Exp.rep_id(s), mpat, def);
      n + of_exp(~top, body);
    | Use(_, body) =>
      add(Exp.rep_id(s), Shut);
      1 + of_exp(~top, body);
    | Seq(e1, body) =>
      add(Exp.rep_id(s), Headless(Exp.rep_id(e1)));
      1 + of_exp(~top, body);
    | Module(items) when top => of_mod(items)
    | _ when top =>
      add(Exp.rep_id(s), tail(e));
      1;
    | _ => 0
    };
  }
  and of_block = (fbody: Exp.t): unit => {
    /* a curried function's body is under its last lambda:
       `fun m -> fun a -> let with_drag = … in …` */
    let rec peel = (e: Exp.t): Exp.t =>
      switch (strip(e).term) {
      | Fun(_, b, _, _) => peel(b)
      | _ => e
      };
    let fbody = peel(fbody);
    if (of_exp(~top=false, fbody) > 0) {
      let rec last = (e: Exp.t): Exp.t =>
        switch (strip(e).term) {
        | Let(_, _, body)
        | TyAlias(_, _, body)
        | ModuleExp(_, _, body)
        | Use(_, body)
        | Seq(_, body) => last(body)
        | _ => e
        };
      let e = last(fbody);
      add(Exp.rep_id(strip(e)), tail(e));
    };
  }
  and def_kids = (def: Exp.t): unit =>
    switch (strip(def).term) {
    | Module(items) => ignore(of_mod(items))
    | Fun(_, fbody, _, _)
    | TypFun(_, {term: Fun(_, fbody, _, _), _}, _) => of_block(fbody)
    | _ => ignore(of_exp(~top=false, strip(def)))
    }
  and let_row = (id, pat: Pat.t, def: Exp.t): int =>
    switch (pat_var(pat)) {
    | Some(_) =>
      def_row(id, Pat.rep_id(pat), def);
      /* `let f(x) = …`: [def] is the function's body */
      Option.is_some(FunctionSugar.detect(pat))
        ? of_block(def) : def_kids(def);
      1;
    | None => 0
    }
  and type_row = (id, tpat: TPat.t, typ: Typ.t): int =>
    switch (tpat.term) {
    | Var(_) =>
      add(id, Def(TPat.rep_id(tpat), Typ.rep_id(typ)));
      1;
    | _ => 0
    }
  /* a name, maybe under a signature; the header holds the whole
     pattern */
  and module_row = (id, mpat: MPat.t, def: Exp.t): int => {
    let rec named = (mp: MPat.t) =>
      switch (mp.term) {
      | Var(_) => true
      | Asc(inner, _) => named(inner)
      | _ => false
      };
    if (named(mpat)) {
      def_row(id, MPat.rep_id(mpat), def);
      def_kids(def);
      1;
    } else {
      0;
    };
  }
  and of_mod = (items: list(Mod.t)): int =>
    List.fold_left(
      (n, m: Mod.t) =>
        n
        + (
          switch (m.term) {
          | ModLet(pat, def) => let_row(Mod.rep_id(m), pat, def)
          | ModType(tpat, typ) => type_row(Mod.rep_id(m), tpat, typ)
          | ModuleMod(mpat, def) => module_row(Mod.rep_id(m), mpat, def)
          | ModExp(e) =>
            switch (strip(e).term) {
            | EmptyHole => 0
            | _ =>
              add(Mod.rep_id(m), Headless(Exp.rep_id(e)));
              1;
            }
          | ModVal(_)
          | Invalid(_)
          | EmptyHole
          | MultiHole(_) => 0
          }
        ),
      0,
      items,
    );
  ignore(of_exp(~top=true, term));
  out^;
};

/* the ids under a projector: its syntax is the projector's, no cell's */
let projected_ids = (seg: Segment.t): Id.Map.t(unit) => {
  let out = ref(Id.Map.empty);
  let rec go = (~under, seg: Segment.t) =>
    List.iter(
      (p: Piece.t) => {
        if (under) {
          out := Id.Map.add(Piece.id(p), (), out^);
        };
        switch (p) {
        | Tile(t) => List.iter(go(~under), t.children)
        | Projector(pr) => go(~under=true, pr.syntax)
        | _ => ()
        };
      },
      seg,
    );
  go(~under=false, seg);
  out^;
};

let kind_name = (k: O.kind): string =>
  switch (k) {
  | KTrail => {js|⇒|js}
  | KStmt => ";"
  | KUse => "use"
  | KTest => "test"
  | KTests => "tests"
  | KModule => "module"
  | KFn => "fn"
  | KConst => "const"
  | KType => "type"
  };

let is_def = (e: Web.ScratchCell.t) =>
  switch (e.e_kind) {
  | Def(_) => true
  | Members
  | Headless(_)
  | Run(_) => false
  };

type survey = {
  rows: int,
  /* rows under a projector, which open no cell: its syntax isn't cut */
  projected: int,
  broken: list(string),
};

/* open each row's cell (every [sample]th) from one editor, as the outline
   does */
let survey = (~root=Sort.Exp, ~sample=1, ~name: string, src: string): survey => {
  let seg =
    switch (CorpusUtil.parse(~root, src)) {
    | Some(seg) => seg
    | None => Option.get(CorpusUtil.typed_seg(~root, src))
    };
  let term = MakeTerm.Incr.term_of_root(~root, seg);
  let outline = CorpusUtil.outline_of(~root, seg);
  let info_map = DefStatics.calc(~settings=CoreSettings.on, term).merged;
  let data =
    (root == Sort.Mod ? MakeTerm.go_mod_root(seg) : MakeTerm.go(seg)).
      term_data;
  let wants = wants(term);
  let projected = projected_ids(seg);
  let (text, ids) = (text_of(seg), Segment.ids(seg));
  /* one editor for every row: making it measures the whole program */
  let whole = Focus.cell_of_seg(~root, seg);
  /* a term's level and its range there */
  let range = (id: Id.t): (Segment.t, int, int) =>
    switch (Id.Map.find_opt(id, data)) {
    | Some({base_seg, skel, _}) =>
      let (l, r) = Skel.range(skel);
      (base_seg, l, r + 1);
    | None => failwith(name ++ ": no term " ++ Id.show(id))
    };
  let span = id => {
    let (level, l, r) = range(id);
    Util.ListUtil.slice(l, r - l, level);
  };
  let braced = (s: Segment.t) =>
    switch (s) {
    | [Tile({children: [kid], _} as t)] when Tile.label(t) == ["{", "}"] =>
      Some(kid)
    | _ => None
    };
  let rows = ref(0);
  let n_projected = ref(0);
  let broken = ref([]);
  /* the outline's verdict, from the cell code in one walk */
  let refused =
    Focus.refusals(
      ~root,
      ~want=id => Id.Map.mem(id, O.row_ids(outline)),
      seg,
    );
  let note = (row, whys) =>
    if (whys != []) {
      broken :=
        [
          Printf.sprintf(
            "%s: %s: %s",
            name,
            row,
            String.concat("; ", List.rev(whys)),
          ),
          ...broken^,
        ];
    };
  /* a row that opens no cell: the outline gets why, and a pin drops */
  let check_shut = (~row, ~id, ~pin) => {
    incr(rows);
    let whys = ref([]);
    let fail = why => whys := [why, ...whys^];
    if (Id.Map.find_opt(id, refused) != Some(Focus.Directive)) {
      fail("not refused as a use");
    };
    switch (V.realize(~info_map, ~outline, pin, Whole(whole))) {
    | exception e => fail("raised " ++ Printexc.to_string(e))
    | (v, Whole(_)) when v.pins == [] => ()
    | _ => fail("opened")
    };
    note(row, whys^);
  };
  let check_row = (~row, ~id, ~pin, ~expect, ~header, ~body) => {
    incr(rows);
    let whys = ref([]);
    let fail = why => whys := [why, ...whys^];
    let under =
      List.exists(id => Id.Map.mem(id, projected), Segment.ids(body));
    switch (Id.Map.find_opt(id, refused), under) {
    | (None, false)
    | (Some(Focus.Projected), true) => ()
    | (Some(Projected), false) => fail("refused as under a projector")
    | (Some(Unfinished), _) => fail("refused as unfinished")
    | (Some(Directive), _) => fail("refused as a use")
    | (None, true) => fail("not refused under a projector")
    };
    switch (V.realize(~info_map, ~outline, pin, Whole(whole))) {
    | exception e => fail("raised " ++ Printexc.to_string(e))
    | (v, Whole(_)) when under && v.pins == [] => incr(n_projected)
    | (_, Whole(_)) => fail("didn't open")
    | (_, Divided(_)) when under => fail("opened under a projector")
    | (_, Divided(d)) =>
      switch (Divided.cells(d)) {
      | [e] =>
        if (!expect(e)) {
          fail("opened as " ++ Web.ScratchCell.show_kind(e.e_kind));
        };
        let same = (what, got, want) =>
          if (text_of(trim(got)) != text_of(trim(want))) {
            fail(
              Printf.sprintf(
                "%s holds\n%s\n--- not\n%s",
                what,
                text_of(trim(got)),
                text_of(trim(want)),
              ),
            );
          } else if (Segment.ids(trim(got)) != Segment.ids(trim(want))) {
            fail(what ++ " holds other pieces");
          };
        Option.iter(h => same("header", Focus.header_text(e), h), header);
        same("body", Focus.body_text(e), body);
      | cells => fail(Printf.sprintf("%d cells", List.length(cells)))
      };
      if (!Segment.ptr_eq(Divided.document(d), seg)) {
        fail("not the program's own pieces");
      };
      let joined = Focus.zip_of_cell(Divided.join(~prev=whole, d));
      if (text_of(joined) != text) {
        fail("join changed the text");
      } else if (Segment.ids(joined) != ids) {
        fail("join changed the ids");
      };
    };
    note(row, whys^);
  };
  List.iter(
    (n: O.node) =>
      switch (n.o_id, n.o_kind) {
      | (Some(id), kind) =>
        let row = n.o_label ++ " " ++ kind_name(kind);
        let pin = V.pin(~outline, id, V.init);
        switch (Id.Map.find_opt(id, wants)) {
        | None => failwith(name ++ ": no term for " ++ row)
        | Some(Shut) => check_shut(~row, ~id, ~pin)
        | Some(Headless(b)) =>
          let sym = kind == KTrail ? {js|⇒|js} : ";";
          check_row(
            ~row,
            ~id,
            ~pin,
            ~expect=e => e.e_kind == Headless(sym),
            ~header=None,
            ~body=span(b),
          );
        | Some(Def(h, b)) =>
          check_row(
            ~row,
            ~id,
            ~pin,
            ~expect=is_def,
            ~header=Some(span(h)),
            ~body=span(b),
          )
        | Some(Module(h, b)) =>
          switch (braced(span(b))) {
          | Some(members) =>
            check_row(
              ~row,
              ~id,
              ~pin,
              ~expect=e => e.e_kind == Members,
              ~header=Some(span(h)),
              ~body=members,
            )
          /* `{}` has no members to show: its definition */
          | None =>
            check_row(
              ~row,
              ~id,
              ~pin,
              ~expect=is_def,
              ~header=Some(span(h)),
              ~body=span(b),
            )
          }
        };
      /* a tests container opens its run as one cell */
      | (None, KTests) =>
        let kids = List.filter_map((k: O.node) => k.o_id, n.o_children);
        let stmt = id =>
          switch (Id.Map.find_opt(id, wants)) {
          | Some(Headless(b)) => range(b)
          | _ => failwith(name ++ ": a test row without its statement")
          };
        let (level, l, _) = stmt(List.hd(kids));
        let (_, _, r) = stmt(Util.ListUtil.last(kids));
        check_row(
          ~row="tests",
          ~id=List.hd(kids),
          ~pin=V.pin(~outline, ~run=true, List.hd(kids), V.init),
          ~expect=
            e =>
              switch (e.e_kind) {
              | Run(ids) => List.for_all(k => List.mem(k, ids), kids)
              | Def(_)
              | Members
              | Headless(_) => false
              },
          ~header=None,
          ~body=Util.ListUtil.slice(l, r - l, level),
        );
      | (None, _) => ()
      },
    List.filteri((i, _) => i mod sample == 0, O.flatten(O.rows(outline))),
  );
  /* and every item the walk expects has its row */
  let shown = O.row_ids(outline);
  Id.Map.iter(
    (id, _) =>
      if (!Id.Map.mem(id, shown)) {
        note("a term " ++ Id.show(id), ["has no row"]);
      },
    wants,
  );
  {
    rows: rows^,
    projected: n_projected^,
    broken: List.rev(broken^),
  };
};

let report = (s: survey) =>
  List.iter(b => Printf.printf("BROKEN %s\n", b), s.broken);

/* shapes the corpus has, and some it lacks */
let shapes = [
  (
    "a ⇒ that is an if, its else holding lets",
    "let f = fun x ->\n  let c = x in\n  if c > 0\n  then 1\n  else\n    let d = c in\n    d\nin\nf(1)",
    0,
  ),
  (
    "the same in a module member",
    "module M = {\n  let f = fun x ->\n    let c = x in\n    if c > 0 then 1 else let d = c in d;\n  let g = 2\n} in\nM.g",
    0,
  ),
  (
    "a ⇒ in parentheses",
    "module M = {\n  let f = fun x ->\n    let a = x in\n    (a, a);\n  let g = 2\n} in\nM.f(1)",
    0,
  ),
  ("a ⇒ under a projector", "let a = 1 in\n^^fold(\"hidden\")", 0),
  ("rows inside a projector", "let a = 1 in\n^^fold((let b = 2 in\nb))", 2),
  ("an if's else taking a `;`", "let c = true in\nif c then 1 else 2;\n3", 0),
  ("a block in parentheses", "let a = 1 in\n(let b = 2 in\nb)", 0),
  (
    "a module's lone statement",
    "module M = {\n  test 1 == 1 end\n} in\n0",
    0,
  ),
  ("a theorem, by its statement", "theorem t = 1 + 1 == 2 in\n0", 0),
  ("a test run", "let a = 1 in\ntest a == 1 end;\ntest a > 0 end;\na", 0),
  (
    "a ⇒ that is a do-block, after a let",
    "let f = fun x ->\n  let a = x in\n  do b <- a in\n  let c = b in\n  c\nin\nf(1)",
    0,
  ),
  (
    "a run of hinted tests",
    "let a = 1 in\nhint \"one\"\ntest a == 1 end;\nhint \"two\"\ntest a > 0 end;\na",
    0,
  ),
  ("a use first", "use Nat in\nlet a = 1 in\na + 2", 0),
  (
    "a use among definitions and tests",
    "let a = 1 in\nuse Nat in\nlet b = a + 2 in\ntest b == 3 end;\nb",
    0,
  ),
  (
    "a use in a function body",
    "let f = fun x ->\n  use Nat in\n  let y = x + 1 in\n  y * 2\nin\nf(1)",
    0,
  ),
  (
    "a use in a member's body",
    "module M = {\n  let x =\n    use Nat in\n    1 + 2;\n  let y = 2\n} in\nM.x",
    0,
  ),
  (
    "a module with a signature",
    "type S = {\n  let x : Int\n} in\nmodule M : S = {\n  let x = 1;\n  let y = 2\n} in\nM.x",
    0,
  ),
];

let shape_rows = () => {
  let surveys = List.map(((name, src, _)) => survey(~name, src), shapes);
  List.iter(report, surveys);
  List.iter2(
    ((name, _, projected), s) => {
      check(int, name ++ ": broken rows", 0, List.length(s.broken));
      check(int, name ++ ": rows under a projector", projected, s.projected);
    },
    shapes,
    surveys,
  );
};

/* every slide's rows, and every fourth of mega-1k's and mega-mod-1k's */
let corpus = () => {
  let programs =
    List.map(
      ((path, src)) => (path, Sort.Exp, 1, src),
      CorpusUtil.slide_corpus(),
    )
    @ List.map(
        ((file, root)) =>
          (file, root, 4, Option.get(CorpusUtil.mega_src(file))),
        [("mega-1k.hz", Sort.Exp), ("mega-mod-1k.hz", Sort.Mod)],
      );
  let (rows, projected, broken) =
    List.fold_left(
      ((rows, projected, broken), (name, root, sample, src)) => {
        let s = survey(~root, ~sample, ~name, src);
        report(s);
        (
          rows + s.rows,
          projected + s.projected,
          broken + List.length(s.broken),
        );
      },
      (0, 0, 0),
      programs,
    );
  Printf.printf(
    "HeadlessRows: %d programs, %d rows, %d under a projector, %d broken\n",
    List.length(programs),
    rows,
    projected,
    broken,
  );
  check(int, "broken rows", 0, broken);
};

let tests = (
  "HeadlessRows",
  [
    test_case("shapes", `Quick, shape_rows),
    test_case("corpus rows", `Slow, corpus),
  ],
);
