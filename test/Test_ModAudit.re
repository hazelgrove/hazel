open Util;
open Alcotest;
open Haz3lcore;
open Language;

module O = Web.OutlineTree;

/* The module-editing audit (plans/module-editing-audit.md): editing inside
   module bodies against the same editing in a let-in chain. Minutes per
   corpus, so only with MODAUDIT=1; MODAUDIT_ONLY=<substring> picks corpora
   and probes, MODAUDIT_LOG=1 prints every miss, MODAUDIT_SAMPLE=n samples
   the mega programs. The numbers print; nothing here gates CI. */

let enabled = Sys.getenv_opt("MODAUDIT") != None;
let verbose = Sys.getenv_opt("MODAUDIT_LOG") != None;
let selected = (name: string): bool =>
  switch (Sys.getenv_opt("MODAUDIT_ONLY")) {
  | None => true
  | Some(s) => CorpusUtil.contains(s, name)
  };

/* the editor's own editing settings; no evaluation */
let settings: CoreSettings.t = {
  ...CoreSettings.on,
  dynamics: false,
  elaborate: false,
  assist: false,
};

/* tokens in order, whitespace and grout aside, shards in place */
let rec tokens = (seg: Segment.t): list(string) =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Secondary(_)
      | Grout(_)
      | Projector(_) => []
      | Tile(t) =>
        let rec interleave = (ls, chs) =>
          switch (ls, chs) {
          | ([], _) => []
          | ([l, ...ls], []) => [l, ...interleave(ls, [])]
          | ([l, ...ls], [c, ...chs]) =>
            [l, ...tokens(c)] @ interleave(ls, chs)
          };
        interleave(Tile.effective_label(t), t.children);
      },
    seg,
  );

let load = (~root: Sort.t, src: string): Zipper.t =>
  Zipper.unzip(CorpusUtil.seg_of(~root, src));

/* ---- what the editor shows for a program: outline rows, each with its
   binder's type and the errors it holds ---- */

type fp = {
  kind: O.kind,
  ty: string,
  errs: list(string),
};

type snap = {
  term: Exp.t,
  source: Segment.t,
  completed: Segment.t,
  /* the display's completion (quiver chips, Tab) */
  display: Segment.t,
  /* label path -> fingerprint, document order */
  rows: list((string, fp)),
  ids: list((string, Id.t)),
  info_map: Statics.Map.t,
};

let key_of = (p: O.path): string =>
  String.concat(
    "/",
    List.map(
      (s: O.path_seg) =>
        (s.s_label == "" ? "⇒" : s.s_label)
        ++ (s.s_occ > 0 ? "#" ++ string_of_int(s.s_occ) : ""),
      p,
    ),
  );

let kind_name: O.kind => string =
  fun
  | KModule => "module"
  | KFn => "fn"
  | KConst => "const"
  | KType => "type"
  | KTest => "test"
  | KTests => "tests"
  | KStmt => "stmt"
  | KUse => "use"
  | KTrail => "trail";

/* a mark's constructor, and its first argument when short: `Free "x"` */
let mark_names = (info: Info.t): list(string) =>
  List.map(
    m => {
      let s =
        Mark.show(m)
        |> String.map(c => c == '\n' ? ' ' : c)
        |> Str.global_replace(Str.regexp("  +"), " ")
        |> String.trim;
      let s =
        String.length(s) > 0 && s.[0] == '('
          ? String.sub(s, 1, String.length(s) - 1) : s;
      switch (Str.search_forward(Str.regexp("[ (]*[{]"), s, 0)) {
      | i => String.sub(s, 0, i)
      | exception Not_found =>
        String.length(s) > 40 ? String.sub(s, 0, 40) : s
      };
    },
    Info.marks_of(info),
  );

let snapshot = (~root: Sort.t, z: Zipper.t): snap => {
  let source = MakeTerm.semantic_source(z);
  let (completed, masks) = MakeTerm.semantic_segment(~root, source);
  let display = CanonicalCompletion.for_editor(~root, source).completed_seg;
  let term = MakeTerm.Incr.term_of_root(~masks, ~root, completed);
  let st =
    CachedStatics.init_compositional_term(
      ~settings,
      ~probe_ids=Id.Map.empty,
      term,
    );
  let info_map = st.info_map;
  let nodes = O.flatten(O.of_term(term));
  let ids =
    List.filter_map(
      (n: O.node) =>
        switch (n.o_id) {
        | Some(id) =>
          Option.map(p => (key_of(p), id), O.label_path(id, term))
        | None => None
        },
      nodes,
    );
  let key_of_id = id =>
    List.find_map(((k, i)) => i == id ? Some(k) : None, ids);
  let errs: Hashtbl.t(string, list(string)) = Hashtbl.create(16);
  List.iter(
    eid => {
      let k =
        switch (O.enclosing_row(~info_map, eid, term)) {
        | Some(r) => Option.value(key_of_id(r), ~default="<?>")
        | None => "<top>"
        };
      let names =
        switch (Id.Map.find_opt(eid, info_map)) {
        | Some(info) => mark_names(info)
        | None => ["?"]
        };
      Hashtbl.replace(
        errs,
        k,
        names @ Option.value(Hashtbl.find_opt(errs, k), ~default=[]),
      );
    },
    st.error_ids,
  );
  let rows =
    List.filter_map(
      (n: O.node) =>
        switch (n.o_id) {
        | None => None
        | Some(id) =>
          switch (key_of_id(id)) {
          | None => None
          | Some(k) =>
            let ty =
              switch (n.o_binder) {
              | Some((bid, Sort.Pat)) =>
                switch (Id.Map.find_opt(bid, info_map)) {
                | Some(InfoPat(p)) => Typ.pretty_print(p.ty)
                | _ => "?"
                }
              | _ => ""
              };
            Some((
              k,
              {
                kind: n.o_kind,
                ty,
                errs:
                  List.sort(
                    compare,
                    Option.value(Hashtbl.find_opt(errs, k), ~default=[]),
                  ),
              },
            ));
          }
        },
      nodes,
    );
  let top = Option.value(Hashtbl.find_opt(errs, "<top>"), ~default=[]);
  let rows =
    top == []
      ? rows
      : rows
        @ [
          (
            "<top>",
            {
              kind: KStmt,
              ty: "",
              errs: List.sort(compare, top),
            },
          ),
        ];
  {
    term,
    source,
    completed,
    display,
    rows,
    ids,
    info_map,
  };
};

/* ---- where an edit lands: the innermost row whose text holds a point ---- */

let origin_of = (measured: Measured.t, id: Id.t): option(Point.t) =>
  Option.map(
    (m: Measured.measurement) => m.origin,
    Measured.find_by_id(id, measured),
  );

let le = (a: Point.t, b: Point.t) =>
  a.row < b.row || a.row == b.row && a.col <= b.col;

let depth = (k: string): int => List.length(String.split_on_char('/', k));

/* rows in document order: the deepest row starting at or before [pt]
   whose later siblings and ancestors' siblings start after it */
let row_at = (~measured, snap: snap, pt: Point.t): option(string) => {
  let stack = ref([]);
  List.iter(
    ((k, id)) =>
      switch (origin_of(measured, id)) {
      | Some(o) when le(o, pt) =>
        let d = depth(k);
        stack := [k, ...List.filter(k' => depth(k') < d, stack^)];
      | _ => ()
      },
    snap.ids,
  );
  switch (stack^) {
  | [k, ..._] => Some(k)
  | [] => None
  };
};

let is_prefix = (a: string, b: string): bool =>
  a == b
  || String.length(a) < String.length(b)
  && String.sub(b, 0, String.length(a) + 1) == a
  ++ "/";

/* rows an edit at row [k] may change: it, its ancestors and descendants */
let near = (k: option(string), row: string): bool =>
  switch (k) {
  | None => false
  | Some(k) => is_prefix(row, k) || is_prefix(k, row)
  };

type ripple = {
  gone: list(string),
  added: list(string),
  retyped: list((string, string, string)),
  errs: list((string, list(string), list(string))),
};

let no_ripple = {
  gone: [],
  added: [],
  retyped: [],
  errs: [],
};

/* the rows away from [k] that read differently in [after] */
let ripple = (~k: option(string), before: snap, after: snap): ripple => {
  let far = List.filter(((r, _)) => !near(k, r) && r != "<top>");
  let b = far(before.rows);
  let a = far(after.rows);
  List.fold_left(
    (acc, (r, fb: fp)) =>
      switch (List.assoc_opt(r, a)) {
      | None => {
          ...acc,
          gone: acc.gone @ [r],
        }
      | Some(fa) =>
        let acc =
          fa.ty != fb.ty
            ? {
              ...acc,
              retyped: acc.retyped @ [(r, fb.ty, fa.ty)],
            }
            : acc;
        fa.errs != fb.errs
          ? {
            ...acc,
            errs: acc.errs @ [(r, fb.errs, fa.errs)],
          }
          : acc;
      },
    {
      ...no_ripple,
      added:
        List.filter_map(
          ((r, _)) => List.mem_assoc(r, b) ? None : Some(r),
          a,
        ),
    },
    b,
  );
};

let quiet = (r: ripple) =>
  r.gone == [] && r.added == [] && r.retyped == [] && r.errs == [];

let show_ripple = (r: ripple): string =>
  String.concat(
    "; ",
    (r.gone == [] ? [] : ["gone " ++ String.concat(",", r.gone)])
    @ (r.added == [] ? [] : ["new " ++ String.concat(",", r.added)])
    @ List.map(
        ((row, b, a)) => Printf.sprintf("%s: %s => %s", row, b, a),
        r.retyped,
      )
    @ List.map(
        ((row, b, a)) =>
          Printf.sprintf(
            "%s errs [%s] => [%s]",
            row,
            String.concat(",", b),
            String.concat(",", a),
          ),
        r.errs,
      ),
  );

/* ---- edits through the real pipeline ---- */

let perform =
    (~root, ~syntax, ~statics, z: Zipper.t, a: Action.t): option(Zipper.t) =>
  switch (
    Perform.go(
      ~settings,
      ~statics,
      ~syntax,
      ~root,
      a,
      {
        zipper: z,
        col_target: None,
      },
    )
  ) {
  | Ok(z) => Some(z)
  | Error(_) => None
  | exception _ => None
  };

let statics_of = (~root, z: Zipper.t): CachedStatics.t =>
  CachedStatics.init_compositional(
    ~settings,
    ~stitch=Fun.id,
    ~root,
    ~probe_ids=Id.Map.empty,
    z,
  );

/* the text of the edited program, around a point */
let text_of = (z: Zipper.t): string =>
  Printer.of_zipper(~holes="?", ~indent="", z);

let excerpt = (~around: int, text: string): string => {
  let lines = String.split_on_char('\n', text);
  let n = List.length(lines);
  let lo = max(0, around - 2);
  let hi = min(n - 1, around + 2);
  List.filteri((i, _) => i >= lo && i <= hi, lines)
  |> String.concat(" ⏎ ");
};

/* ---- the deletion sweep ---- */

type target = {
  tok: Token.t,
  kind: string, /* the token, its form, its sort */
  head: string, /* its form's first token */
  sort: Sort.t,
  tile: Id.t,
  pt: Point.t, /* just after the token */
  ctx: string /* what the token's item sits in */
};

/* what a tile's item sits in: a module literal's body, the top level of a
   Mod-rooted program, or an expression's let chain */
let ctx_name = (~root: Sort.t, in_braces: bool): string =>
  in_braces ? "module-body" : root == Sort.Mod ? "mod-top" : "exp-top";

let is_braces = (t: Tile.t) =>
  Tile.label(t) == ["{", "}"] && Tile.mold(t).out == Sort.Exp;

/* every shard of every multi-shard tile, and every `;` and type arrow */
let targets =
    (~root: Sort.t, measured: Measured.t, z: Zipper.t): list(target) => {
  let acc = ref([]);
  let rec walk = (~in_braces, sg: Segment.t) =>
    List.iter(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          List.iter(walk(~in_braces=in_braces || is_braces(t)), t.children);
          let multi = Tile.arity(t) > 1;
          let sep = Tile.label(t) == [";"] || Tile.label(t) == ["->"];
          if (multi || sep) {
            let sort = Tile.mold(t).out;
            let form =
              String.concat("…", Tile.label(t)) ++ "@" ++ Sort.show(sort);
            switch (Measured.find_shards_by_id(t.id, measured)) {
            | Some(shards) =>
              List.iter(
                ((i, m: Measured.measurement)) => {
                  let tok = Tile.token(t, i);
                  acc :=
                    [
                      {
                        tok,
                        kind: tok ++ " of " ++ form,
                        head: Tile.token(t, 0),
                        sort,
                        tile: t.id,
                        pt: m.last,
                        ctx: ctx_name(~root, in_braces),
                      },
                      ...acc^,
                    ];
                },
                shards,
              )
            | None => ()
            };
          };
        | _ => ()
        },
      sg,
    );
  walk(~in_braces=false, Zipper.unselect_and_zip(z));
  List.rev(acc^);
};

type outcome = {
  t: target,
  row: option(string),
  own: bool, /* the token is its row's own: a definition's head, its `in` */
  restored: bool,
  destroyed: bool,
  display_differs: bool,
  ripple,
  text: string,
  /* the completion semantics read, around the edit */
  reading: string,
};

let ok = (o: outcome) => o.restored && quiet(o.ripple);

/* the kind with module and let-in spellings identified: a member's `;` and
   a chain definition's `in` both separate items */
let norm = (o: outcome): string =>
  switch (o.t.tok) {
  | ";" when o.t.sort == Sort.Mod || o.own => "item-sep"
  | "in" when o.own => "item-sep"
  | tok => (o.own ? "head " : "") ++ tok ++ " of " ++ o.t.head
  };

let delete_at =
    (~root, ~syntax, ~statics, z0: Zipper.t, t: target): option(Zipper.t) => {
  let acts =
    [Action.Move(Point(t.pt, None))]
    @ List.init(Token.length(t.tok), _ =>
        Action.Destruct(Local(Left, ByChar))
      );
  List.fold_left(
    (z, a) => Option.bind(z, z => perform(~root, ~syntax, ~statics, z, a)),
    Some(z0),
    acts,
  );
};

/* did the deletion take exactly the token? */
let intact = (original, mutated, tok): bool => {
  let rec go = (pre, rest) =>
    switch (rest) {
    | [] => false
    | [x, ...tl] =>
      x == tok && List.rev_append(pre, tl) == mutated || go([x, ...pre], tl)
    };
  go([], original);
};

let sweep_deletions =
    (~root: Sort.t, ~sample=1, name: string, src: string): list(outcome) => {
  let z0 = load(~root, src);
  let syntax = CachedSyntax.init(~root, z0);
  let statics = statics_of(~root, z0);
  let base = snapshot(~root, z0);
  let original = tokens(Zipper.unselect_and_zip(z0));
  if (tokens(base.completed) != original) {
    print_endline(
      name ++ ": the unedited program does not complete to itself",
    );
  };
  let ts = targets(~root, syntax.measured, z0);
  /* a sample keeps whole rows, the same rows in programs sharing them */
  let ts =
    sample == 1
      ? ts
      : List.filter(
          (t: target) =>
            Hashtbl.hash(
              Option.value(
                row_at(~measured=syntax.measured, base, t.pt),
                ~default="-",
              ),
            )
            mod sample == 0,
          ts,
        );
  List.filter_map(
    (t: target) =>
      switch (delete_at(~root, ~syntax, ~statics, z0, t)) {
      | None => None
      | Some(z) =>
        let mutated = tokens(Zipper.unselect_and_zip(~erase_buffer=true, z));
        let destroyed = !intact(original, mutated, t.tok);
        let s = snapshot(~root, z);
        let row = row_at(~measured=syntax.measured, base, t.pt);
        let own =
          switch (row) {
          | Some(k) => List.assoc_opt(k, base.ids) == Some(t.tile)
          | None => false
          };
        Some({
          t,
          row,
          own,
          restored: tokens(s.completed) == original,
          destroyed,
          display_differs: tokens(s.display) != tokens(s.completed),
          ripple: ripple(~k=row, base, s),
          text: excerpt(~around=t.pt.row, text_of(z)),
          reading:
            excerpt(~around=t.pt.row, CorpusUtil.text_of(s.completed)),
        });
      },
    ts,
  );
};

let show_outcome = (name: string, o: outcome): string =>
  Printf.sprintf(
    "[%s] %s %s at %d:%d in %s%s%s%s :: %s%s",
    name,
    o.t.ctx,
    o.t.kind,
    o.t.pt.row,
    o.t.pt.col,
    Option.value(o.row, ~default="-"),
    o.restored ? "" : " UNRESTORED",
    o.display_differs ? " DISPLAY≠SEMANTICS" : "",
    quiet(o.ripple) ? "" : " RIPPLE{" ++ show_ripple(o.ripple) ++ "}",
    o.text,
    o.restored ? "" : "\n        reads as: " ++ o.reading,
  );

/* tallies by (context, kind) */
type tally = {
  mutable n: int,
  mutable restored: int,
  mutable quiet: int,
  mutable display: int,
  mutable destroyed: int,
};

let new_tally = () => {
  n: 0,
  restored: 0,
  quiet: 0,
  display: 0,
  destroyed: 0,
};

let report = (name: string, outs: list(outcome)) => {
  let tbl: Hashtbl.t((string, string), tally) = Hashtbl.create(32);
  List.iter(
    (o: outcome) => {
      let key = (o.t.ctx, norm(o));
      let t =
        switch (Hashtbl.find_opt(tbl, key)) {
        | Some(t) => t
        | None =>
          let t = new_tally();
          Hashtbl.replace(tbl, key, t);
          t;
        };
      t.n = t.n + 1;
      if (o.restored) {
        t.restored = t.restored + 1;
      };
      if (quiet(o.ripple)) {
        t.quiet = t.quiet + 1;
      };
      if (o.display_differs) {
        t.display = t.display + 1;
      };
      if (o.destroyed) {
        t.destroyed = t.destroyed + 1;
      };
      if (verbose && (!ok(o) || o.display_differs)) {
        print_endline(show_outcome(name, o));
      };
    },
    outs,
  );
  let rows =
    Hashtbl.fold((k, v, acc) => [(k, v), ...acc], tbl, [])
    |> List.sort(compare);
  Printf.printf(
    "== %s: %d deletions ==\n%-12s %-24s %5s %8s %8s %8s %8s\n",
    name,
    List.length(outs),
    "context",
    "delimiter",
    "n",
    "restored",
    "quiet",
    "display≠",
    "destroy",
  );
  List.iter(
    (((ctx, kind), t)) =>
      Printf.printf(
        "%-12s %-24s %5d %8d %8d %8d %8d\n",
        ctx,
        kind,
        t.n,
        t.restored,
        t.quiet,
        t.display,
        t.destroyed,
      ),
    rows,
  );
};

/* ---- twins: one module's members as a let-in chain, at a Mod-rooted top
   level, and as a module literal in an expression ---- */

type twins = {
  mod_lit: string, /* as written: a module literal at a Mod-rooted top */
  mod_top: string, /* its members at a Mod-rooted top level */
  exp_lit: string, /* the literal in an expression: `… in 0` */
  exp_chain: string /* its members as a let-in chain: `… in 0` */
};

let dedent = (line: string): string =>
  String.length(line) >= 2 && String.sub(line, 0, 2) == "  "
    ? String.sub(line, 2, String.length(line) - 2) : line;

/* [src]'s first top-level module literal, which must open its line's end
   and close at a line's start */
let twins_of = (src: string): option(twins) => {
  let z = load(~root=Mod, src);
  let measured = CachedSyntax.init(~root=Mod, z).measured;
  let seg = Zipper.unselect_and_zip(z);
  let lit =
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) when is_braces(t) => Some(t)
        | _ => None
        },
      seg,
    );
  switch (
    lit,
    Option.map(
      (t: Tile.t) => Measured.find_shards_by_id(t.id, measured),
      lit,
    ),
  ) {
  | (Some(lt), Some(Some([(_, op), (_, cl)])))
      when List.length(lt.children) == 1 =>
    let body = List.hd(lt.children);
    let lines = Array.of_list(String.split_on_char('\n', src));
    let is_def = (p: Piece.t) =>
      switch (p) {
      | Tile(t) => List.mem(Tile.token(t, 0), ["let", "type", "module"])
      | _ => false
      };
    /* each member-ending `;`: its point, and whether a definition ends */
    let rec seps = (first: option(Piece.t), sg: Segment.t) =>
      switch (sg) {
      | [] => ([], first)
      | [Piece.Tile(t) as p, ...rest] when Tile.is_semi(t) =>
        let (more, last) = seps(None, rest);
        (
          [
            (
              Option.map(
                (m: Measured.measurement) => m.origin,
                Measured.find_by_id(t.id, measured),
              ),
              Option.fold(~none=false, ~some=is_def, first),
            ),
            ...more,
          ],
          last,
        )
        |> (
          r => {
            ignore(p);
            r;
          }
        );
      | [Piece.Secondary(_), ...rest] => seps(first, rest)
      | [p, ...rest] => seps(first == None ? Some(p) : first, rest)
      };
    let (ss, last_first) = seps(None, body);
    let chain = Array.copy(lines);
    let ok = ref(true);
    List.iter(
      ((pt: option(Point.t), def)) =>
        switch (pt) {
        | Some(pt)
            when
              pt.row < Array.length(chain)
              && pt.col < String.length(chain[pt.row])
              && chain[pt.row].[pt.col] == ';' =>
          if (def) {
            let l = chain[pt.row];
            chain[pt.row] =
              String.sub(l, 0, pt.col)
              ++ " in"
              ++ String.sub(l, pt.col + 1, String.length(l) - pt.col - 1);
          }
        | _ => ok := false
        },
      /* right to left within a line keeps earlier columns put */
      List.rev(ss),
    );
    let inner = (a: array(string)) =>
      Array.to_list(
        Array.sub(a, op.origin.row + 1, cl.origin.row - op.origin.row - 1),
      )
      |> List.map(dedent);
    let body_lines = inner(lines);
    let chain_lines = inner(chain);
    /* the last member, if a definition, takes an `in` too */
    let chain_lines =
      Option.fold(~none=false, ~some=is_def, last_first)
        ? {
          let rev = List.rev(chain_lines);
          let rec add = (l: list(string)) =>
            switch (l) {
            | [x, ...tl] when String.trim(x) == "" => [x, ...add(tl)]
            | [x, ...tl] => [x ++ " in", ...tl]
            | [] => []
            };
          List.rev(add(rev));
        }
        : chain_lines;
    let upto_close =
      Array.to_list(Array.sub(lines, 0, cl.origin.row))
      @ [String.sub(lines[cl.origin.row], 0, cl.origin.col + 1)]
      |> String.concat("\n");
    ! ok^
      ? None
      : Some({
          mod_lit: src,
          mod_top: String.concat("\n", body_lines),
          exp_lit: upto_close ++ " in\n0",
          exp_chain: String.concat("\n", chain_lines) ++ "\n0",
        });
  | _ => None
  };
};

/* the same delimiter across variants: its row (the module's own row
   prefix stripped), its identified kind, and its index among those */
let align = (~strip: option(string), outs: list(outcome)) => {
  let seen = Hashtbl.create(64);
  List.map(
    (o: outcome) => {
      let row = Option.value(o.row, ~default="-");
      let row =
        switch (strip) {
        | Some(p) when is_prefix(p, row) =>
          row == p
            ? "<shell>"
            : String.sub(
                row,
                String.length(p) + 1,
                String.length(row) - String.length(p) - 1,
              )
        | _ => row
        };
      let k = (row, norm(o));
      let i = Option.value(Hashtbl.find_opt(seen, k), ~default=0);
      Hashtbl.replace(seen, k, i + 1);
      ((row, norm(o), i), o);
    },
    outs,
  );
};

/* against the let-in control: the module variant's misses where the
   control is fine (module-specific), and misses both share */
let compare_to_control =
    (
      ~control: list(((string, string, int), outcome)),
      name: string,
      variant: list(((string, string, int), outcome)),
    ) => {
  let specific = ref([]);
  let shared = ref(0);
  let better = ref(0);
  let aligned = ref(0);
  List.iter(
    ((k, o: outcome)) =>
      switch (List.assoc_opt(k, control)) {
      | None => ()
      | Some(c) =>
        incr(aligned);
        switch (ok(c), ok(o)) {
        | (true, false) => specific := [(k, c, o), ...specific^]
        | (false, false) => incr(shared)
        | (false, true) => incr(better)
        | (true, true) => ()
        };
      },
    variant,
  );
  let specific = List.rev(specific^);
  Printf.printf(
    "-- %s vs let-in: %d aligned, %d worse in the module, %d shared misses, %d better --\n",
    name,
    aligned^,
    List.length(specific),
    shared^,
    better^,
  );
  let by_kind = Hashtbl.create(16);
  List.iter(
    (((_, kind, _), _, _)) =>
      Hashtbl.replace(
        by_kind,
        kind,
        1 + Option.value(Hashtbl.find_opt(by_kind, kind), ~default=0),
      ),
    specific,
  );
  Hashtbl.iter(
    (k, n) => Printf.printf("   worse: %-24s %d\n", k, n),
    by_kind,
  );
  List.iter(
    (((row, kind, i), c: outcome, o: outcome)) =>
      Printf.printf(
        "   WORSE %s %s #%d\n     module: %s\n     let-in: %s\n",
        row,
        kind,
        i,
        show_outcome(name, o),
        show_outcome("control", c),
      ),
    specific,
  );
};

/* ---- corpora ---- */

let counter_mod = "module Counter = {\n  let start = 0;\n  let next = start + 1;\n  let after = next + 1\n};\nCounter.after";

let read_any = (paths: list(string)): option(string) =>
  List.find_map(CorpusUtil.read_file, paths);

/* the standard library's sources, when this tree has them (stdlib
   branches), else the audit's copies */
let stdlib = (name: string): option(string) =>
  read_any([
    "hazel-programs/stdlib/" ++ name,
    "../hazel-programs/stdlib/" ++ name,
    "test/modaudit/" ++ name,
    "../test/modaudit/" ++ name,
  ]);

let module_sources: list((string, unit => option(string))) = [
  ("counter", () => Some(counter_mod)),
  ("Option", () => stdlib("Option.hz")),
  ("List", () => stdlib("List.hz")),
];

let twin_sweep = ((name, src)) =>
  switch (Option.bind(src(), twins_of)) {
  | None => print_endline(name ++ ": no twins")
  | Some(tw) =>
    let modname =
      switch (
        Str.search_forward(
          Str.regexp("module \\([A-Za-z_]+\\)"),
          tw.mod_lit,
          0,
        )
      ) {
      | _ => Some(Str.matched_group(1, tw.mod_lit))
      | exception Not_found => None
      };
    let run = (label, root, text) => {
      let outs = sweep_deletions(~root, name ++ " " ++ label, text);
      report(name ++ " " ++ label, outs);
      outs;
    };
    let control =
      align(~strip=None, run("let-in chain", Sort.Exp, tw.exp_chain));
    let lit =
      align(~strip=modname, run("literal@Mod", Sort.Mod, tw.mod_lit));
    let top = align(~strip=None, run("members@Mod", Sort.Mod, tw.mod_top));
    let elit =
      align(~strip=modname, run("literal@Exp", Sort.Exp, tw.exp_lit));
    compare_to_control(~control, name ++ " literal@Mod", lit);
    compare_to_control(~control, name ++ " members@Mod", top);
    compare_to_control(~control, name ++ " literal@Exp", elit);
  };

/* MODAUDIT_SAMPLE=n: one row in n (whole rows, so programs sharing rows
   sample the same ones) */
let sample =
  switch (Option.bind(Sys.getenv_opt("MODAUDIT_SAMPLE"), int_of_string_opt)) {
  | Some(n) when n > 0 => n
  | _ => 1
  };

/* the same modules at an expression's top (`… in`) and at a Mod-rooted
   one (`…;`): only the top level differs */
let mega_sweep = () => {
  let run = (label, root, file) =>
    switch (CorpusUtil.mega_src(file)) {
    | None =>
      print_endline(file ++ ": missing");
      [];
    | Some(src) =>
      let outs = sweep_deletions(~root, ~sample, label, src);
      report(label, outs);
      align(~strip=None, outs);
    };
  let control = run("mega-1k", Sort.Exp, "mega-1k.hz");
  let variant = run("mega-mod-1k", Sort.Mod, "mega-mod-1k.hz");
  compare_to_control(~control, "mega-mod-1k", variant);
};

let deletions = () =>
  if (!enabled) {
    print_endline("ModAudit deletions: skipped (MODAUDIT=1 runs it)");
  } else {
    List.iter(
      ((name, _) as s) => selected(name) ? twin_sweep(s) : (),
      module_sources,
    );
    if (selected("mega")) {
      mega_sweep();
    };
    /* the Modules slide: literals, signatures, nesting, aliases, in one
       expression */
    if (selected("slide")) {
      switch (
        read_any([
          "hazel-programs/docs/reference/modules.hz",
          "../hazel-programs/docs/reference/modules.hz",
        ])
      ) {
      | Some(src) =>
        report(
          "Modules slide",
          sweep_deletions(~root=Exp, ~sample, "Modules slide", src),
        )
      | None => print_endline("modules.hz: missing")
      };
    };
  };

/* ---- the typing sweep: a new item typed key by key, every state checked
   against the program before it ---- */

/* the rows the new item may change: those holding the caret */
let typing_ripple =
    (~scope: list(string), ~fresh: string, before: snap, after: snap) => {
  let keep = ((r, _)) => r != "<top>" && !List.mem(r, scope);
  let b = List.filter(keep, before.rows);
  let a = List.filter(keep, after.rows);
  let is_fresh = (r: string) => {
    let parts = String.split_on_char('/', r);
    let last = Util.ListUtil.last(parts);
    /* the new row, its rows, a test's numbering */
    List.exists(p => p == fresh, parts)
    || last == "⇒"
    || CorpusUtil.contains("test@", last)
    || CorpusUtil.contains("tests@", last);
  };
  List.fold_left(
    (acc, (r, fb: fp)) =>
      switch (List.assoc_opt(r, a)) {
      | None => {
          ...acc,
          gone: acc.gone @ [r],
        }
      | Some(fa) =>
        let acc =
          fa.ty != fb.ty
            ? {
              ...acc,
              retyped: acc.retyped @ [(r, fb.ty, fa.ty)],
            }
            : acc;
        fa.errs != fb.errs
          ? {
            ...acc,
            errs: acc.errs @ [(r, fb.errs, fa.errs)],
          }
          : acc;
      },
    {
      ...no_ripple,
      added:
        List.filter_map(
          ((r, _)) =>
            List.mem_assoc(r, b) || is_fresh(r) ? None : Some(r),
          a,
        ),
    },
    b,
  );
};

type step =
  | Key(string)
  | Left;

let keys = (s: string): list(step) =>
  List.map(c => Key(c), Token.to_list(s));

type typed = {
  site: string,
  item: string,
  /* each disturbing state: the keys so far, what moved */
  bad: list((string, ripple, string)),
  /* the final state's rows unchanged, and a definition item's row a
     definition's */
  final_ok: bool,
  reads_def: bool,
  states: int,
  /* the keys, and under each `.` (quiet), `X` (disturbed), `!` (refused) */
  typed: string,
  marks: string,
};

/* [steps] from [z], each state snapshot against [base]: the disturbing
   states (keys so far, the ripple, the reading around [around]), the
   state count, whether the last state is quiet, the keys and marks, and
   the last state's snapshot */
let run_steps =
    (
      ~root,
      ~syntax,
      ~statics,
      ~scope,
      ~fresh,
      ~around: int,
      base: snap,
      z: Zipper.t,
      steps: list(step),
    )
    : (
        list((string, ripple, string)),
        int,
        bool,
        string,
        string,
        option(snap),
      ) => {
  let typed = Stdlib.Buffer.create(16);
  let marks = Stdlib.Buffer.create(16);
  let (bad, n, _, last_quiet, last) =
    List.fold_left(
      ((bad, n, z, last_quiet, last), st) =>
        switch (z) {
        | None => (bad, n, None, last_quiet, last)
        | Some(z) =>
          let a =
            switch (st) {
            | Key(c) =>
              Stdlib.Buffer.add_string(typed, c == "\n" ? "⏎" : c);
              Action.Insert(c);
            | Left =>
              Stdlib.Buffer.add_string(typed, "←");
              Action.Move(Local(Left, ByChar));
            };
          switch (perform(~root, ~syntax, ~statics, z, a)) {
          | None =>
            Stdlib.Buffer.add_string(marks, "!");
            (bad, n, Some(z), last_quiet, last);
          | Some(z') =>
            let s = snapshot(~root, z');
            let r = typing_ripple(~scope, ~fresh, base, s);
            let bad =
              quiet(r)
                ? bad
                : bad
                  @ [
                    (
                      Stdlib.Buffer.contents(typed),
                      r,
                      excerpt(~around, CorpusUtil.text_of(s.completed)),
                    ),
                  ];
            /* one mark per key, under it; `d`/`D`: the display's
               completion reads differently from the semantics' */
            let differs = tokens(s.display) != tokens(s.completed);
            Stdlib.Buffer.add_string(
              marks,
              switch (quiet(r), differs) {
              | (true, false) => "."
              | (false, false) => "X"
              | (true, true) => "d"
              | (false, true) => "D"
              },
            );
            (bad, n + 1, Some(z'), quiet(r), Some(s));
          };
        },
      ([], 0, Some(z), true, None),
      steps,
    );
  (
    bad,
    n,
    last_quiet,
    Stdlib.Buffer.contents(typed),
    Stdlib.Buffer.contents(marks),
    last,
  );
};

/* a definition item reads as one: its row is a definition's, not a
   statement's (a test is a statement) */
let reads_as_def = (~fresh: string, last: option(snap)): bool =>
  fresh == "test"
  || (
    switch (last) {
    | Some(s) =>
      List.exists(
        ((k, fp: fp)) =>
          Util.ListUtil.last(String.split_on_char('/', k)) == fresh
          && fp.kind != KStmt,
        s.rows,
      )
    | None => false
    }
  );

/* the items typed, as a member and as a chain definition */
let typed_items: list((string, string, string, string)) = [
  /* name, fresh row label, member spelling, let-in spelling */
  ("let", "q", "let q = 1", "let q = 1 in"),
  ("fun", "q", "let q = fun x -> x + 1", "let q = fun x -> x + 1 in"),
  ("paren", "q", "let q = (1 + 2)", "let q = (1 + 2) in"),
  (
    "case",
    "q",
    "let q = case 1 | 1 => 2 | _ => 3 end",
    "let q = case 1 | 1 => 2 | _ => 3 end in",
  ),
  ("type", "Q", "type Q = Int", "type Q = Int in"),
  ("module", "N", "module N = { let z = 2 }", "module N = { let z = 2 } in"),
  ("test", "test", "test 1 == 1 end", "test 1 == 1 end;"),
];

/* the end of a piece's last token */
let piece_end = (measured: Measured.t, p: Piece.t): option(Point.t) =>
  switch (p) {
  | Tile(t) =>
    Option.bind(Measured.find_shards_by_id(t.id, measured), shards =>
      Option.map(
        ((_, m: Measured.measurement)) => m.last,
        Util.ListUtil.last_opt(shards),
      )
    )
  | Grout(_)
  | Projector(_) =>
    Option.map(
      (m: Measured.measurement) => m.last,
      Measured.find_by_id(Piece.id(p), measured),
    )
  | Secondary(_) => None
  };

/* each item of a level: where its first token starts and its last ends
   (its `;` or `in` included) */
let item_spans =
    (measured: Measured.t, level: Segment.t): list((Point.t, Point.t)) =>
  List.filter_map(
    (item: Segment.t) => {
      let solid = List.filter(p => !Piece.is_secondary(p), item);
      switch (solid, Util.ListUtil.last_opt(solid)) {
      | ([first, ..._], Some(last)) =>
        switch (
          Option.map(
            (m: Measured.measurement) => m.origin,
            Measured.find_by_id(Piece.id(first), measured),
          ),
          piece_end(measured, last),
        ) {
        | (Some(o), Some(e)) => Some((o, e))
        | _ => None
        }
      | _ => None
      };
    },
    Segment.top_items(level),
  );

type site = {
  label: string,
  at: Point.t,
  /* the steps before and after the item's keys */
  pre: list(step),
  post: list(step),
};

/* where a new item goes among a level's items: before the first, after
   each, and after the last. [chain]: let-in spelling (its last item is the
   body, which keeps its place). [opener]: after a module literal's `{`.
   [open_end]: the last member has no `;`, so a member also goes after it
   without one, on a line of its own */
let sites =
    (
      ~chain: bool,
      ~opener: option(Point.t),
      ~open_end=false,
      spans: list((Point.t, Point.t)),
    )
    : list(site) => {
  let spans = chain ? List.rev(List.tl(List.rev(spans))) : spans;
  let sep = chain ? [] : keys(";");
  let n = List.length(spans);
  let top =
    switch (opener, spans) {
    | (Some(pt), _) => [
        {
          label: "top",
          at: pt,
          pre: keys("\n"),
          post: sep,
        },
      ]
    | (None, [(o, _), ..._]) => [
        {
          label: "top",
          at: {
            ...o,
            col: 0,
          },
          pre: keys("\n") @ [Left],
          post: sep,
        },
      ]
    | (None, []) => []
    };
  top
  @ List.mapi(
      (i, (_, e: Point.t)) =>
        i == n - 1 && !chain
          ? {
            label: "end",
            at: e,
            pre: keys(";\n"),
            post: [],
          }
          : {
            label: i == n - 1 ? "end" : "after #" ++ string_of_int(i),
            at: e,
            pre: keys("\n"),
            post: sep,
          },
      spans,
    )
  @ (
    switch (open_end, Util.ListUtil.last_opt(spans)) {
    | (true, Some((_, e))) => [
        {
          label: "end, no ;",
          at: e,
          pre: keys("\n"),
          post: [],
        },
      ]
    | _ => []
    }
  );
};

/* the first module literal, depth first; [inner]: the first one holding
   no other */
let rec find_braces = (~inner=false, seg: Segment.t): option(Tile.t) =>
  List.find_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) when is_braces(t) =>
        switch (
          inner ? List.find_map(find_braces(~inner), t.children) : None
        ) {
        | Some(_) as deeper => deeper
        | None => Some(t)
        }
      | Tile(t) => List.find_map(find_braces(~inner), t.children)
      | _ => None
      },
    seg,
  );

/* every item typed at every site of [level] (a module literal's body, or a
   top level), each state checked. [scope]: the rows holding the level */
let sweep_typing =
    (
      ~root: Sort.t,
      ~chain: bool,
      ~lit: bool,
      ~inner=false,
      ~sample=1,
      name: string,
      src: string,
    )
    : list(typed) => {
  let z0 = load(~root, src);
  let syntax = CachedSyntax.init(~root, z0);
  let statics = statics_of(~root, z0);
  let base = snapshot(~root, z0);
  let seg = Zipper.unselect_and_zip(z0);
  let (level, opener, scope) =
    switch (lit) {
    | false => (seg, None, [])
    | true =>
      switch (find_braces(~inner, seg)) {
      | Some(t) =>
        let op =
          Option.bind(
            Measured.find_shards_by_id(t.id, syntax.measured), shards =>
            Option.map(
              ((_, m: Measured.measurement)) => m.last,
              Util.ListUtil.hd_opt(shards),
            )
          );
        /* the rows holding the literal: the first top-level row, and for
           an inner literal the first module row in it */
        let scope =
          switch (base.ids) {
          | [(k, _), ..._] when inner => [
              k,
              ...List.filter_map(
                   ((r, fp: fp)) =>
                     fp.kind == KModule && is_prefix(k, r) && r != k
                       ? Some(r) : None,
                   base.rows,
                 )
                 |> Util.ListUtil.take(1),
            ]
          | [(k, _), ..._] => [k]
          | [] => []
          };
        (List.hd(t.children), op, scope);
      | None => (seg, None, [])
      }
    };
  let spans = item_spans(syntax.measured, level);
  let open_end =
    !chain
    && (
      switch (List.rev(List.filter(p => !Piece.is_secondary(p), level))) {
      | [p, ..._] => !Piece.is_semi(p)
      | [] => false
      }
    );
  List.concat_map(
    ((item, fresh, member, letin)) => {
      let text = chain ? letin : member;
      sites(~chain, ~opener, ~open_end, spans)
      |> List.filteri((i, _) =>
           i mod sample == 0 || i >= List.length(spans)
         )
      |> List.filter_map((st: site) =>
           switch (
             perform(~root, ~syntax, ~statics, z0, Move(Point(st.at, None)))
           ) {
           | None => None
           | Some(z) =>
             let steps = st.pre @ keys(text) @ st.post;
             let (bad, n, last_quiet, typed, marks, last) =
               run_steps(
                 ~root,
                 ~syntax,
                 ~statics,
                 ~scope,
                 ~fresh,
                 ~around=st.at.row,
                 base,
                 z,
                 steps,
               );
             Some({
               site: st.label,
               item,
               bad,
               final_ok: last_quiet && reads_as_def(~fresh, last),
               reads_def: reads_as_def(~fresh, last),
               states: n,
               typed,
               marks,
             });
           }
         );
    },
    typed_items,
  )
  |> (
    r => {
      ignore(name);
      r;
    }
  );
};

let typing_report = (name: string, outs: list(typed)) => {
  let disturbed =
    List.filter(
      (t: typed) =>
        t.bad != []
        || String.contains(t.marks, 'd')
        || String.contains(t.marks, 'D'),
      outs,
    );
  Printf.printf(
    "== %s: %d items typed, %d disturbed another row at some key, %d at the end ==\n",
    name,
    List.length(outs),
    List.length(disturbed),
    List.length(List.filter((t: typed) => !t.final_ok, outs)),
  );
  List.iter(
    (t: typed) => {
      let (first, r, reading) =
        switch (t.bad) {
        | [b, ..._] => b
        | [] => ("", no_ripple, "(display differs only)")
        };
      Printf.printf(
        "   %-6s at %-10s %s\n   %-6s    %-10s %s\n      first after \"%s\": %s\n      reads: %s\n",
        t.item,
        t.site,
        t.typed,
        "",
        "",
        t.marks,
        first,
        show_ripple(r),
        reading,
      );
    },
    disturbed,
  );
  List.iter(
    (t: typed) =>
      Printf.printf(
        "   %-6s at %-10s %s\n      at the end: %s\n",
        t.item,
        t.site,
        t.typed,
        t.reads_def ? "a row disturbed" : "reads as a statement",
      ),
    List.filter((t: typed) => !t.final_ok, outs),
  );
};

let typing_sweep = ((name, src)) =>
  switch (Option.bind(src(), twins_of)) {
  | None => print_endline(name ++ ": no twins")
  | Some(tw) =>
    let run = (label, root, ~chain, ~lit, text) =>
      typing_report(
        name ++ " " ++ label,
        sweep_typing(~root, ~chain, ~lit, name ++ " " ++ label, text),
      );
    run("let-in chain", Sort.Exp, ~chain=true, ~lit=false, tw.exp_chain);
    run("literal@Mod", Sort.Mod, ~chain=false, ~lit=true, tw.mod_lit);
    run("members@Mod", Sort.Mod, ~chain=false, ~lit=false, tw.mod_top);
    run("literal@Exp", Sort.Exp, ~chain=false, ~lit=true, tw.exp_lit);
  };

/* a module inside a module, typed into the inner one */
let nested_src = "module Outer = {\n  module Inner = {\n    type T = Bool;\n    let value = true\n  };\n  let w = 1\n};\nOuter.w";

let typing = () =>
  if (!enabled) {
    print_endline("ModAudit typing: skipped (MODAUDIT=1 runs it)");
  } else {
    List.iter(
      ((name, _) as s) => selected(name) ? typing_sweep(s) : (),
      module_sources,
    );
    if (selected("nested")) {
      typing_report(
        "nested (inner literal)",
        sweep_typing(
          ~root=Mod,
          ~chain=false,
          ~lit=true,
          ~inner=true,
          "nested",
          nested_src,
        ),
      );
    };
  };

/* ---- outline ops (Alt-arrows, Tab/⇧Tab, duplicate, delete, new) on
   every row, by their laws: an op and its inverse restore the text, a
   result reads back as written and leaves no unfinished form ---- */

module IE = Web.ItemEdit;

let edit_ctx = (~root: Sort.t, seg: Segment.t): IE.ctx => {
  let term = MakeTerm.Incr.term_of_root(~root, seg);
  {
    mod_root: root == Sort.Mod,
    term,
    info_map: lazy(DefStatics.calc(~settings=CoreSettings.on, term).merged),
    is_open: _ => true,
  };
};

let op_name: Web.OutlineEdit.def_op => string =
  fun
  | NewBelow => "new"
  | NewTypeBelow => "new type"
  | NewModuleBelow => "new module"
  | NewTestBelow => "new test"
  | NewTestAbove => "new test above"
  | Duplicate => "duplicate"
  | MoveUp => "Alt↑"
  | MoveDown => "Alt↓"
  | Indent => "Tab"
  | Outdent => "⇧Tab"
  | Delete => "delete";

let labels = (term: Exp.t): list(string) =>
  O.flatten(O.of_term(term))
  |> List.filter_map((n: O.node) =>
       n.o_id == None || n.o_kind == KTrail ? None : Some(n.o_label)
     )
  |> List.sort(compare);

let rows_of = (term: Exp.t): list((string, Id.t)) =>
  O.flatten(O.of_term(term))
  |> List.filter_map((n: O.node) =>
       switch (n.o_id) {
       | Some(id) when n.o_kind != KTrail && n.o_kind != KTests =>
         Option.map(p => (key_of(p), id), O.label_path(id, term))
       | _ => None
       }
     );

let op = (~root, o, fid, seg) =>
  IE.edit(edit_ctx(~root, seg), Op(o, fid), seg);

/* where two texts first differ: the lines there, each side */
let first_diff = (a: string, b: string): string => {
  let la = Array.of_list(String.split_on_char('\n', a));
  let lb = Array.of_list(String.split_on_char('\n', b));
  let n = min(Array.length(la), Array.length(lb));
  let rec go = i =>
    i >= n
      ? Printf.sprintf(
          "line %d: %d lines vs %d",
          i,
          Array.length(la),
          Array.length(lb),
        )
      : la[i] != lb[i]
          ? Printf.sprintf(
              "line %d: %S => %S%s",
              i,
              la[i],
              lb[i],
              i + 1 < n
                ? Printf.sprintf(" (next %S => %S)", la[i + 1], lb[i + 1])
                : "",
            )
          : go(i + 1);
  go(0);
};

/* each op's laws, at row [k] */
let op_laws =
    (~root: Sort.t, seg: Segment.t, (k: string, fid: Id.t))
    : list((string, string)) => {
  let text0 = CorpusUtil.text_of(seg);
  let term0 = MakeTerm.Incr.term_of_root(~root, seg);
  let n0 = List.length(labels(term0));
  let reads_back = (seg': Segment.t): list(string) => {
    let txt = CorpusUtil.text_of(seg');
    let unfinished = Segment.incomplete_tiles_deep(seg') != [];
    let back =
      switch (CorpusUtil.parse(~root, txt)) {
      | Some(s2) when tokens(s2) == tokens(seg') => None
      | Some(s2) =>
        Some(
          "reads back as "
          ++ String.concat(" ", tokens(s2))
          ++ " vs "
          ++ String.concat(" ", tokens(seg')),
        )
      | None when String.trim(txt) == "" => None
      | None => Some("unparseable: " ++ txt)
      };
    (unfinished ? ["leaves an unfinished form"] : [])
    @ (
      switch (back) {
      | None => []
      | Some(why) =>
        let why =
          String.length(why) > 300 ? String.sub(why, 0, 300) ++ "…" : why;
        ["does not read back as written: " ++ why];
      }
    );
  };
  let inverse = (o1, o2) => {
    (
      switch (op(~root, o1, fid, seg)) {
      | Error(_) => []
      | Ok((seg1, target)) =>
        let laws = reads_back(seg1);
        let term1 = MakeTerm.Incr.term_of_root(~root, seg1);
        let laws =
          laws
          @ (
            labels(term1) == labels(term0)
              ? [] : ["changes the rows' names"]
          );
        /* the same row, after the op: where the op says, else by its path,
           else the one row of its name */
        let rows1 = rows_of(term1);
        let last = (k: string) =>
          Util.ListUtil.last(String.split_on_char('/', k));
        let found =
          switch (target, List.assoc_opt(k, rows1)) {
          | (Some(id), _) when List.exists(((_, i)) => i == id, rows1) =>
            Some(id)
          | (_, Some(id)) => Some(id)
          | _ =>
            switch (List.filter(((k1, _)) => last(k1) == last(k), rows1)) {
            | [(_, id)] => Some(id)
            | _ => None
            }
          };
        switch (found) {
        | None => laws @ ["loses the row"]
        | Some(fid1) =>
          switch (op(~root, o2, fid1, seg1)) {
          | Error(why) => laws @ [op_name(o2) ++ " back refused: " ++ why]
          | Ok((seg2, _)) =>
            let text2 = CorpusUtil.text_of(seg2);
            text2 == text0
              ? laws
              : laws
                @ [
                  op_name(o2)
                  ++ " back changes the text: "
                  ++ first_diff(text0, text2),
                ];
          }
        };
      }
    )
    |> List.map(l => (op_name(o1), l));
  };
  let count = (o, delta) =>
    switch (op(~root, o, fid, seg)) {
    | Error(_) => []
    | Ok((seg1, _)) =>
      let term1 = MakeTerm.Incr.term_of_root(~root, seg1);
      let n1 = List.length(labels(term1));
      reads_back(seg1)
      @ (delta(n1 - n0) ? [] : [Printf.sprintf("rows %d -> %d", n0, n1)])
      |> List.map(l => (op_name(o), l));
    };
  inverse(MoveUp, MoveDown)
  @ inverse(MoveDown, MoveUp)
  @ inverse(Indent, Outdent)
  @ count(Duplicate, d => d >= 1)
  @ count(Delete, d => d <= (-1))
  @ count(NewBelow, d => d == 1)
  @ count(NewTestBelow, d => d >= 0);
};

let ops_sweep = (~root: Sort.t, ~sample=1, name: string, src: string) => {
  let seg = CorpusUtil.seg_of(~root, src);
  let term = MakeTerm.Incr.term_of_root(~root, seg);
  let rows = rows_of(term) |> List.filteri((i, _) => i mod sample == 0);
  let bad =
    List.concat_map(
      ((k, _) as row) =>
        op_laws(~root, seg, row) |> List.map(((o, l)) => (k, o, l)),
      rows,
    );
  Printf.printf(
    "== %s: %d rows, %d law breaks ==\n",
    name,
    List.length(rows),
    List.length(bad),
  );
  List.iter(
    ((k, o, l)) => Printf.printf("   %-28s %-10s %s\n", k, o, l),
    bad,
  );
};

let ops = () =>
  if (!enabled) {
    print_endline("ModAudit ops: skipped (MODAUDIT=1 runs it)");
  } else {
    List.iter(
      ((name, src)) =>
        if (selected(name)) {
          switch (Option.bind(src(), twins_of)) {
          | None => print_endline(name ++ ": no twins")
          | Some(tw) =>
            ops_sweep(~root=Exp, name ++ " let-in chain", tw.exp_chain);
            ops_sweep(~root=Mod, name ++ " literal@Mod", tw.mod_lit);
            ops_sweep(~root=Mod, name ++ " members@Mod", tw.mod_top);
            ops_sweep(~root=Exp, name ++ " literal@Exp", tw.exp_lit);
          };
        },
      module_sources,
    );
    /* doc comments and blank lines between items */
    List.iter(
      ((name, root, src)) =>
        selected(name) ? ops_sweep(~root, name, src) : (),
      [
        (
          "doc comments (module)",
          Sort.Mod,
          "module M = {\n  let init = (1, 2);\n\n  # doc for f #\n  let f = fun x -> x;\n\n  # doc for g #\n  let g = 2\n};\nM.f",
        ),
        (
          "doc comments (let-in)",
          Sort.Exp,
          "let init = (1, 2) in\n\n# doc for f #\nlet f = fun x -> x in\n\n# doc for g #\nlet g = 2 in\nf",
        ),
        /* blank lines holding indentation, as a paste leaves them */
        (
          "doc comments (pasted)",
          Sort.Mod,
          "module M = {\n  let init = (1, 2);\n  \n  # doc for f #\n  let f = fun x -> x;\n  \n  # doc for g #\n  let g = 2\n};\nM.f",
        ),
      ],
    );
    /* one row in 3 unless MODAUDIT_SAMPLE says */
    let sample = Sys.getenv_opt("MODAUDIT_SAMPLE") == None ? 3 : sample;
    List.iter(
      ((name, root, file)) =>
        if (selected(name)) {
          switch (CorpusUtil.mega_src(file)) {
          | Some(src) => ops_sweep(~root, ~sample, name, src)
          | None => print_endline(name ++ ": missing")
          };
        },
      [
        ("mega-1k", Sort.Exp, "mega-1k.hz"),
        ("mega-mod-1k", Sort.Mod, "mega-mod-1k.hz"),
      ],
    );
  };

/* ---- feature probes: one behaviour in a module against the same in a
   let-in chain, printed side by side ---- */

/* the point of the [k]-th occurrence of [needle], [off] columns in */
let point_of = (~k=0, ~off=0, src: string, needle: string): option(Point.t) => {
  let lines = String.split_on_char('\n', src);
  let rec find = (row, k, lines) =>
    switch (lines) {
    | [] => None
    | [l, ...rest] =>
      let rec cols = (from, k) =>
        switch (Str.search_forward(Str.regexp_string(needle), l, from)) {
        | c when k == 0 => `Found(c)
        | c => cols(c + 1, k - 1)
        | exception Not_found => `Left(k)
        };
      switch (cols(0, k)) {
      | `Found(c) =>
        Some(
          {
            row,
            col: c + off,
          }: Point.t,
        )
      | `Left(k) => find(row + 1, k, rest)
      };
    };
  find(0, k, lines);
};

/* an action's result, or why it failed */
let perform_why =
    (~root, ~syntax, ~statics, z: Zipper.t, a: Action.t)
    : result(Zipper.t, string) =>
  switch (
    Perform.go(
      ~settings,
      ~statics,
      ~syntax,
      ~root,
      a,
      {
        zipper: z,
        col_target: None,
      },
    )
  ) {
  | Ok(z) => Ok(z)
  | Error(f) => Error(Action.Failure.show(f))
  | exception e => Error("raised " ++ Printexc.to_string(e))
  };

/* [src] loaded, the caret at [needle] (+[off]), then [acts] performed;
   with the loaded program's statics */
let act_st =
    (~root, ~k=0, ~off=1, src: string, needle: string, acts: list(Action.t))
    : (result(Zipper.t, string), CachedStatics.t) => {
  let z0 = load(~root, src);
  let statics = statics_of(~root, z0);
  switch (point_of(~k, ~off, src, needle)) {
  | None => (Error("no " ++ needle), statics)
  | Some(pt) => (
      List.fold_left(
        (z, a) =>
          Stdlib.Result.bind(z, z =>
            perform_why(
              ~root,
              ~syntax=CachedSyntax.init(~root, z),
              ~statics,
              z,
              a,
            )
          ),
        Ok(z0),
        [Action.Move(Point(pt, None)), ...acts],
      ),
      statics,
    )
  };
};

let act = (~root, ~k=0, ~off=1, src, needle, acts): option(Zipper.t) =>
  Stdlib.Result.to_option(fst(act_st(~root, ~k, ~off, src, needle, acts)));

/* leading spaces shown, lines joined */
let show_lines = (lines: list(string)): string =>
  lines
  |> List.map(l => {
       let n = String.length(l) - String.length(StringUtil.trim_leading(l));
       String.concat("", List.init(n, _ => "·"))
       ++ String.sub(l, n, String.length(l) - n);
     })
  |> String.concat(" ⏎ ");

let caret_at = (~root, z: Zipper.t): Point.t =>
  Zipper.Caret.point(CachedSyntax.init(~root, z).measured, z);

/* the line and column the caret lands on, and the text there */
let landing = (~root, src: string, z: option(Zipper.t)): string =>
  switch (z) {
  | None => "refused"
  | Some(z) =>
    let pt = caret_at(~root, z);
    let line =
      switch (List.nth_opt(String.split_on_char('\n', src), pt.row)) {
      | Some(l) => String.trim(l)
      | None => "?"
      };
    Printf.sprintf("%d:%d %S", pt.row, pt.col, line);
  };

type probe = {
  what: string,
  root: Sort.t,
  src: string,
  run: (Sort.t, string) => string,
  expect: string,
};

/* F12 from the [k]-th [needle] */
let f12 = (~k=0, ~off=1, needle, root, src) =>
  switch (
    fst(
      act_st(
        ~root,
        ~k,
        ~off,
        src,
        needle,
        [Move(Goal(BindingSiteOfIndicatedVar))],
      ),
    )
  ) {
  | Error(why) => why
  | Ok(z) => landing(~root, src, Some(z))
  };

/* the indicated term's class and type, as the inspector reads them (the
   cursor's info: CodeWithStatics.get_cursor_info) */
let inspect = (~k=0, ~off=1, needle, root, src) => {
  switch (act_st(~root, ~k, ~off, src, needle, [])) {
  | (Error(why), _) => why
  | (Ok(z), st) =>
    switch (
      Indicated.ci_of(z, st.info_map)
      |> Option.map(Info.as_member_reference(~info_map=st.info_map))
    ) {
    | None => "no info"
    | Some(InfoExp(e)) =>
      Info.cls_label(InfoExp(e))
      ++ " : "
      ++ Typ.pretty_print(e.ty)
      ++ (
        e.marks == []
          ? "" : " ERR " ++ String.concat(",", mark_names(InfoExp(e)))
      )
    | Some(InfoPat(p)) =>
      Info.cls_label(InfoPat(p))
      ++ " : "
      ++ Typ.pretty_print(p.ty)
      ++ (
        p.marks == []
          ? "" : " ERR " ++ String.concat(",", mark_names(InfoPat(p)))
      )
    | Some(info) => Info.cls_label(info)
    }
  };
};

/* the program after [acts] at the caret, the lines from the caret's */
let text_after = (~k=0, ~off=1, ~lines=3, needle, acts, root, src) =>
  switch (fst(act_st(~root, ~k, ~off, src, needle, acts))) {
  | Error(why) => why
  | Ok(z) =>
    let row =
      Option.fold(
        ~none=0,
        ~some=(p: Point.t) => p.row,
        point_of(~k, src, needle),
      );
    let all = String.split_on_char('\n', text_of(z));
    List.filteri((i, _) => i >= row && i < row + lines, all) |> show_lines;
  };

/* a structural (agent) edit's result, whole */
let agent = (a: Action.Structural.t, root, src) => {
  let z0 = load(~root, src);
  switch (
    perform_why(
      ~root,
      ~syntax=CachedSyntax.init(~root, z0),
      ~statics=statics_of(~root, z0),
      z0,
      Structural(a),
    )
  ) {
  | Error(why) => why
  | Ok(z) =>
    String.split_on_char('\n', text_of(z))
    |> List.map(String.trim)
    |> List.filter(l => l != "")
    |> String.concat(" ⏎ ")
  };
};

/* TyDi's suggestion with the caret just after [needle] */
let tydi = (~k=0, needle, root, src) =>
  switch (act_st(~root, ~k, ~off=String.length(needle), src, needle, [])) {
  | (Error(why), _) => why
  | (Ok(z), st) =>
    let ci = Indicated.ci_for_completion(z, st.info_map);
    switch (
      Option.bind(TyDi.set_buffer(~root, ~ci, z), TyDi.get_unparsed_buffer)
    ) {
    | Some(b) => "suggests " ++ b
    | None => "no suggestion"
    };
  };

/* the program's errors, each as its token and mark */
let errors_of = (root, src) => {
  let z = load(~root, src);
  let st = statics_of(~root, z);
  let seg = Zipper.unselect_and_zip(z);
  let rec tok_of = (id, seg: Segment.t) =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) when t.id == id =>
          Some(String.concat("…", Tile.label(t)))
        | Tile(t) => List.find_map(tok_of(id), t.children)
        | Grout(g) when g.id == id => Some("hole")
        | _ => None
        },
      seg,
    );
  st.error_ids
  |> List.map(id =>
       Option.value(tok_of(id, seg), ~default="?")
       ++ ":"
       ++ String.concat(
            ",",
            switch (Id.Map.find_opt(id, st.info_map)) {
            | Some(i) => mark_names(i)
            | None => []
            },
          )
     )
  |> List.sort(compare)
  |> String.concat("; ");
};

/* the rows and their errors after pasting [clip] at [needle] */
let paste_rows = (~k=0, ~off=1, needle, clip, root, src) =>
  switch (fst(act_st(~root, ~k, ~off, src, needle, [Paste(clip)]))) {
  | Error(why) => why
  | Ok(z) =>
    let s = snapshot(~root, z);
    let rows =
      s.rows
      |> List.map(((r, fp: fp)) =>
           r
           ++ (fp.errs == [] ? "" : "(" ++ String.concat(",", fp.errs) ++ ")")
         )
      |> String.concat(" ");
    rows ++ " :: " ++ show_lines(String.split_on_char('\n', text_of(z)));
  };

/* the selection after Cmd+D [n] times from [needle] */
let grow = (~n, ~k=0, ~off=1, needle, root, src) =>
  switch (
    fst(
      act_st(
        ~root,
        ~k,
        ~off,
        src,
        needle,
        List.init(n, _ => Action.Select(Term(Current))),
      ),
    )
  ) {
  | Error(why) => why
  | Ok(z) =>
    Printer.of_segment(~holes="?", ~refractors=[], z.selection.content)
    |> String.split_on_char('\n')
    |> List.map(String.trim)
    |> String.concat(" ⏎ ")
  };

/* outline row [label]'s kind */
let row_kind = (label, root, src) => {
  let term =
    MakeTerm.Incr.term_of_root(~root, CorpusUtil.seg_of(~root, src));
  switch (O.kind_of(CorpusUtil.row_by_label(term, label), term)) {
  | Some(k) => kind_name(k)
  | None => "no row"
  };
};

/* an outline op on row [label]'s result, whole */
let outline_op = (o: Web.OutlineEdit.def_op, label: string, root, src) => {
  let seg = CorpusUtil.seg_of(~root, src);
  let term = MakeTerm.Incr.term_of_root(~root, seg);
  switch (op(~root, o, CorpusUtil.row_by_label(term, label), seg)) {
  | Error(why) => why
  | Ok((seg1, _)) =>
    show_lines(String.split_on_char('\n', CorpusUtil.text_of(seg1)))
  };
};

let run_probes = (name: string, ps: list(probe)) => {
  let bad = ref(0);
  List.iter(
    (p: probe) => {
      let got =
        switch (p.run(p.root, p.src)) {
        | s => s
        | exception e => "raised " ++ Printexc.to_string(e)
        };
      let ok = got == p.expect;
      if (!ok) {
        incr(bad);
      };
      Printf.printf(
        "%s %-52s\n      got:    %s\n%s",
        ok ? "  ok  " : "  DIFF",
        p.what ++ " [" ++ Sort.show(p.root) ++ "]",
        got,
        ok ? "" : "      expect: " ++ p.expect ++ "\n",
      );
    },
    ps,
  );
  Printf.printf("== %s: %d of %d differ ==\n", name, bad^, List.length(ps));
};

let m_src = "module M = {\n  let a = 1;\n  let b = a + 1;\n  type T = Int;\n  let c : T = b\n};\nlet a = 5;\nlet d : M.T = M.a + a;\nd";
let e_src = "module M = {\n  let a = 1;\n  let b = a + 1;\n  type T = Int;\n  let c : T = b\n} in\nlet a = 5 in\nlet d : M.T = M.a + a in\nd";
let chain_src = "let a = 1 in\nlet b = a + 1 in\ntype T = Int in\nlet c : T = b in\nc";

let probes = (): list(probe) => [
  {
    /* go to definition */

    what: "F12 on a member's use of an earlier member",
    root: Mod,
    src: m_src,
    run: f12("a + 1"),
    expect: "1:6 \"let a = 1;\"",
  },
  {
    what: "F12 on a member's use of an earlier member",
    root: Exp,
    src: e_src,
    run: f12("a + 1"),
    expect: "1:6 \"let a = 1;\"",
  },
  {
    what: "F12 on a chain use (control)",
    root: Exp,
    src: chain_src,
    run: f12("a + 1"),
    expect: "0:4 \"let a = 1 in\"",
  },
  {
    what: "F12 on M.a's a (a top-level a also bound)",
    root: Mod,
    src: m_src,
    run: f12(~off=2, "M.a"),
    expect: "1:6 \"let a = 1;\"",
  },
  {
    what: "F12 on M.a's a (a top-level a also bound)",
    root: Exp,
    src: e_src,
    run: f12(~off=2, "M.a"),
    expect: "1:6 \"let a = 1;\"",
  },
  {
    what: "F12 from a member to a definition outside its module",
    root: Mod,
    src: "let base = 1;\nmodule M = {\n  let a = base + 1\n};\nM.a",
    run: f12("base + 1"),
    expect: "0:4 \"let base = 1;\"",
  },
  {
    what: "F12 from a member to a definition outside its module",
    root: Exp,
    src: "let base = 1 in\nmodule M = {\n  let a = base + 1\n} in\nM.a",
    run: f12("base + 1"),
    expect: "0:4 \"let base = 1 in\"",
  },
  {
    what: "F12 on M.a's M",
    root: Mod,
    src: m_src,
    run: f12(~off=0, "M.a"),
    expect: "0:7 \"module M = {\"",
  },
  {
    what: "F12 on M.T's T in a type",
    root: Mod,
    src: m_src,
    run: f12(~off=2, "M.T"),
    expect: "3:7 \"type T = Int;\"",
  },
  {
    what: "F12 on a member's use of a type member",
    root: Mod,
    src: m_src,
    run: f12(~off=0, "T = b"),
    expect: "3:7 \"type T = Int;\"",
  },
  {
    /* the inspector */

    what: "inspector on M.a's a",
    root: Mod,
    src: m_src,
    run: inspect(~off=2, "M.a"),
    expect: "Variable reference : Int",
  },
  {
    what: "inspector on M.a's a",
    root: Exp,
    src: e_src,
    run: inspect(~off=2, "M.a"),
    expect: "Variable reference : Int",
  },
  {
    what: "inspector on a member's binder",
    root: Mod,
    src: m_src,
    run: inspect(~off=0, "b = a"),
    expect: "Variable binding : Int",
  },
  {
    what: "inspector on a chain binder (control)",
    root: Exp,
    src: chain_src,
    run: inspect(~off=0, "b = a"),
    expect: "Variable binding : Int",
  },
  {
    /* Enter at a member's end: the new line's indentation */

    what: "Enter after a member's `;`",
    root: Mod,
    src: m_src,
    run: text_after(~off=10, ~lines=3, "let a = 1;", [Insert("\n")]),
    expect: "··let a = 1; ⏎ ·· ⏎ ··let b = a + 1;",
  },
  {
    what: "Enter after `{`",
    root: Mod,
    src: m_src,
    run: text_after(~off=12, ~lines=3, "module M = {", [Insert("\n")]),
    expect: "module M = { ⏎ ·· ⏎ ··let a = 1;",
  },
  {
    what: "Enter after the last member (no `;`)",
    root: Mod,
    src: m_src,
    run: text_after(~off=15, ~lines=3, "let c : T = b", [Insert("\n")]),
    expect: "··let c : T = b ⏎ ·· ⏎ };",
  },
  {
    what: "Enter after a nested module's `{`",
    root: Mod,
    src: nested_src,
    run: text_after(~off=16, ~lines=3, "module Inner = {", [Insert("\n")]),
    expect: "··module Inner = { ⏎ ···· ⏎ ····type T = Bool;",
  },
  {
    what: "Enter after a nested member's `;`",
    root: Mod,
    src: nested_src,
    run: text_after(~off=14, ~lines=3, "type T = Bool;", [Insert("\n")]),
    expect: "····type T = Bool; ⏎ ···· ⏎ ····let value = true",
  },
  {
    what: "Enter after a nested module's last member",
    root: Mod,
    src: nested_src,
    run: text_after(~off=16, ~lines=3, "let value = true", [Insert("\n")]),
    expect: "····let value = true ⏎ ···· ⏎ ··};",
  },
  {
    what: "Enter after a chain definition's `in` (control)",
    root: Exp,
    src: chain_src,
    run: text_after(~off=12, ~lines=3, "let a = 1 in", [Insert("\n")]),
    expect: "let a = 1 in ⏎  ⏎ let b = a + 1 in",
  },
  {
    what: "Enter after a member's `=`",
    root: Mod,
    src: m_src,
    run: text_after(~off=7, ~lines=3, "let b = a + 1", [Insert("\n")]),
    expect: "··let b = ⏎ ·····a + 1; ⏎ ··type T = Int;",
  },
  {
    what: "Enter after a chain definition's `=` (control)",
    root: Exp,
    src: chain_src,
    run: text_after(~off=7, ~lines=3, "let b = a + 1", [Insert("\n")]),
    expect: "let b = ⏎ ···a + 1 in ⏎ type T = Int in",
  },
  {
    /* Cmd+D grows a member to its module, as a definition to its body */

    what: "Cmd+D x4 from a member's definition",
    root: Mod,
    src: m_src,
    run: grow(~n=4, ~off=0, "a + 1"),
    expect: "{ ⏎ let a = 1; ⏎ let b = a + 1; ⏎ type T = Int; ⏎ let c : T = b ⏎ }",
  },
  {
    what: "Cmd+D x4 from a member's definition",
    root: Exp,
    src: e_src,
    run: grow(~n=4, ~off=0, "a + 1"),
    expect: "{ ⏎ let a = 1; ⏎ let b = a + 1; ⏎ type T = Int; ⏎ let c : T = b ⏎ }",
  },
  {
    what: "Cmd+D x5 from a member's definition",
    root: Exp,
    src: e_src,
    run: grow(~n=5, ~off=0, "a + 1"),
    expect: "module M = { ⏎ let a = 1; ⏎ let b = a + 1; ⏎ type T = Int; ⏎ let c : T = b ⏎ } in",
  },
  {
    what: "Cmd+D x4 from a chain definition (control)",
    root: Exp,
    src: chain_src,
    run: grow(~n=4, ~off=0, "a + 1"),
    expect: "let b = a + 1 in ⏎ type T = Int in ⏎ let c : T = b in ⏎ c",
  },
  {
    /* the outline */

    what: "outline: a module alias's row",
    root: Mod,
    src: "module Base = {\n  type Id = Int\n};\nmodule App = {\n  module Db = Base;\n  type UserId = Db.Id\n};\nlet uid : App.Db.Id = 42;\nuid",
    run: row_kind("Db"),
    expect: "module",
  },
  {
    /* the formatter */

    what: "Format(Indent) leaves a formatted module as is",
    root: Mod,
    src: m_src,
    run: text_after(~off=0, ~lines=20, "module M", [Format(Indent)]),
    expect: show_lines(String.split_on_char('\n', m_src)),
  },
  {
    what: "Format(Spacing) leaves a formatted module as is",
    root: Mod,
    src: m_src,
    run: text_after(~off=0, ~lines=20, "module M", [Format(Spacing)]),
    expect: show_lines(String.split_on_char('\n', m_src)),
  },
  {
    what: "Format(Pretty) leaves a formatted module as is",
    root: Mod,
    src: m_src,
    run: text_after(~off=0, ~lines=20, "module M", [Format(Pretty)]),
    expect: show_lines(String.split_on_char('\n', m_src)),
  },
  {
    what: "Format(Pretty) leaves a formatted chain as is (control)",
    root: Exp,
    src: chain_src,
    run: text_after(~off=0, ~lines=20, "let a", [Format(Pretty)]),
    expect: show_lines(String.split_on_char('\n', chain_src)),
  },
  {
    /* the assistant's edits on member paths */

    what: "agent update_definition M/b",
    root: Mod,
    src: m_src,
    run: agent(Update(Definition, "M/b", "a + 2")),
    expect: "module M = { ⏎ let a = 1; ⏎ let b = a + 2; ⏎ type T = Int; ⏎ let c : T = b ⏎ }; ⏎ let a = 5; ⏎ let d : M.T = M.a + a; ⏎ d",
  },
  {
    what: "agent update_definition M/b",
    root: Exp,
    src: e_src,
    run: agent(Update(Definition, "M/b", "a + 2")),
    expect: "module M = { ⏎ let a = 1; ⏎ let b = a + 2; ⏎ type T = Int; ⏎ let c : T = b ⏎ } in ⏎ let a = 5 in ⏎ let d : M.T = M.a + a in ⏎ d",
  },
  {
    what: "agent update_definition of a top-level member",
    root: Mod,
    src: m_src,
    run: agent(Update(Definition, "a", "6")),
    expect: "module M = { ⏎ let a = 1; ⏎ let b = a + 1; ⏎ type T = Int; ⏎ let c : T = b ⏎ }; ⏎ let a = 6; ⏎ let d : M.T = M.a + a; ⏎ d",
  },
  {
    what: "agent insert_after M/a",
    root: Mod,
    src: m_src,
    run: agent(Insert(After, "M/a", "let z = 3")),
    expect: "module M = { ⏎ let a = 1; ⏎ let z = 3; ⏎ let b = a + 1; ⏎ type T = Int; ⏎ let c : T = b ⏎ }; ⏎ let a = 5; ⏎ let d : M.T = M.a + a; ⏎ d",
  },
  {
    what: "agent insert_after M/a",
    root: Exp,
    src: e_src,
    run: agent(Insert(After, "M/a", "let z = 3")),
    expect: "module M = { ⏎ let a = 1; ⏎ let z = 3; ⏎ let b = a + 1; ⏎ type T = Int; ⏎ let c : T = b ⏎ } in ⏎ let a = 5 in ⏎ let d : M.T = M.a + a in ⏎ d",
  },
  {
    what: "agent insert_after the last member M/c",
    root: Mod,
    src: m_src,
    run: agent(Insert(After, "M/c", "let z = 3")),
    expect: "module M = { ⏎ let a = 1; ⏎ let b = a + 1; ⏎ type T = Int; ⏎ let c : T = b; ⏎ let z = 3 ⏎ }; ⏎ let a = 5; ⏎ let d : M.T = M.a + a; ⏎ d",
  },
  {
    what: "agent insert_test after M/b",
    root: Mod,
    src: m_src,
    run: agent(InsertTest(Some("M/b"), "b == 2")),
    expect: "module M = { ⏎ let a = 1; ⏎ let b = a + 1; ⏎ test b == 2 end; ⏎ type T = Int; ⏎ let c : T = b ⏎ }; ⏎ let a = 5; ⏎ let d : M.T = M.a + a; ⏎ d",
  },
  {
    what: "agent insert_test after M/b",
    root: Exp,
    src: e_src,
    run: agent(InsertTest(Some("M/b"), "b == 2")),
    expect: "module M = { ⏎ let a = 1; ⏎ let b = a + 1; ⏎ test b == 2 end; ⏎ type T = Int; ⏎ let c : T = b ⏎ } in ⏎ let a = 5 in ⏎ let d : M.T = M.a + a in ⏎ d",
  },
  {
    what: "agent delete M/b",
    root: Mod,
    src: m_src,
    run: agent(Delete(BindingClause, "M/b")),
    expect: "module M = { ⏎ let a = 1; ⏎ type T = Int; ⏎ let c : T = b ⏎ }; ⏎ let a = 5; ⏎ let d : M.T = M.a + a; ⏎ d",
  },
  {
    what: "agent delete M/b",
    root: Exp,
    src: e_src,
    run: agent(Delete(BindingClause, "M/b")),
    expect: "module M = { ⏎ let a = 1; ⏎ type T = Int; ⏎ let c : T = b ⏎ } in ⏎ let a = 5 in ⏎ let d : M.T = M.a + a in ⏎ d",
  },
  {
    what: "agent update_pattern M/a renames M.a",
    root: Mod,
    src: m_src,
    run: agent(Update(Pattern, "M/a", "aa")),
    expect: "module M = { ⏎ let aa = 1; ⏎ let b = aa + 1; ⏎ type T = Int; ⏎ let c : T = b ⏎ }; ⏎ let a = 5; ⏎ let d : M.T = M.aa + a; ⏎ d",
  },
  {
    what: "agent update_pattern M/a renames M.a",
    root: Exp,
    src: e_src,
    run: agent(Update(Pattern, "M/a", "aa")),
    expect: "module M = { ⏎ let aa = 1; ⏎ let b = aa + 1; ⏎ type T = Int; ⏎ let c : T = b ⏎ } in ⏎ let a = 5 in ⏎ let d : M.T = M.aa + a in ⏎ d",
  },
  {
    /* TyDi: dot completion through a module (FastParse-loaded) */

    what: "TyDi: M.sta (a member through its module)",
    root: Mod,
    src: "module M = {\n  let start = 0\n};\nlet q = M.sta;\nq",
    run: tydi("M.sta"),
    expect: "suggests rt",
  },
  {
    what: "TyDi: M.sta (a member through its module)",
    root: Exp,
    src: "module M = {\n  let start = 0\n} in\nlet q = M.sta in\nq",
    run: tydi("M.sta"),
    expect: "suggests rt",
  },
  {
    what: "TyDi: m.sta (a labeled tuple, control)",
    root: Exp,
    src: "let m = (start = 0) in\nlet q = m.sta in\nq",
    run: tydi("m.sta"),
    expect: "suggests rt",
  },
  {
    /* TyDi as its own tests drive it: typed, the caret at ¦ */

    what: "TyDi typed: a member's partial use",
    root: Exp,
    src: "module M = {\n  let start = 0;\n  let q = sta¦\n} in\nM.q",
    run: (_, src) =>
      Option.fold(
        ~none="no suggestion",
        ~some=b => "suggests " ++ b,
        Test_TyDi.tydi_suggest(src),
      ),
    expect: "suggests rt",
  },
  {
    what: "TyDi typed: a chain's partial use (control)",
    root: Exp,
    src: "let start = 0 in\nlet q = sta¦ in\nq",
    run: (_, src) =>
      Option.fold(
        ~none="no suggestion",
        ~some=b => "suggests " ++ b,
        Test_TyDi.tydi_suggest(src),
      ),
    expect: "suggests rt",
  },
  {
    what: "TyDi typed: a member's partial use, last member",
    root: Exp,
    src: "module M = {\n  let start = 0;\n  let q = sta¦\n} in\nM.q",
    run: (_, src) =>
      Option.fold(
        ~none="no suggestion",
        ~some=b => "suggests " ++ b,
        Test_TyDi.tydi_suggest(src),
      ),
    expect: "suggests rt",
  },
  {
    /* errors */

    what: "errors: unbound, mismatch in members",
    root: Mod,
    src: "module M = {\n  let a = 1;\n  let b = a + zz;\n  let c : Int = true\n};\nM.a",
    run: errors_of,
    expect: "true:ExpectationMismatch; zz:Free \"zz\")",
  },
  {
    what: "errors: unbound, mismatch in a chain (control)",
    root: Exp,
    src: "let a = 1 in\nlet b = a + zz in\nlet c : Int = true in\na",
    run: errors_of,
    expect: "true:ExpectationMismatch; zz:Free \"zz\")",
  },
  {
    what: "errors: a member used before its definition",
    root: Mod,
    src: "module M = {\n  let b = a + 1;\n  let a = 1\n};\nM.a",
    run: errors_of,
    expect: "a:Free \"a\")",
  },
  {
    what: "errors: a duplicate member name",
    root: Mod,
    src: "module M = {\n  let a = 1;\n  let a = 2\n};\nM.a",
    run: errors_of,
    expect: "",
  },
  {
    /* paste */

    what: "paste a member after a member's `;`",
    root: Mod,
    src: "module M = {\n  let a = 1;\n  let b = 2\n};\nM.a",
    run: paste_rows(~off=10, "let a = 1;", "\nlet z = 3;"),
    expect: "M M/a M/z M/b ⇒ :: module M = { ⏎ ··let a = 1; ⏎ ··let z = 3; ⏎ ··let b = 2 ⏎ }; ⏎ M.a",
  },
  {
    what: "paste two members after a member's `;`",
    root: Mod,
    src: "module M = {\n  let a = 1;\n  let b = 2\n};\nM.a",
    run: paste_rows(~off=10, "let a = 1;", "\nlet y = 3;\nlet z = 4;"),
    expect: "M M/a M/y M/z M/b ⇒ :: module M = { ⏎ ··let a = 1; ⏎ ··let y = 3; ⏎ ··let z = 4; ⏎ ··let b = 2 ⏎ }; ⏎ M.a",
  },
  {
    what: "paste a partial member (`let z =`) after a member's `;`",
    root: Mod,
    src: "module M = {\n  let a = 1;\n  let b = 2\n};\nM.a",
    run: paste_rows(~off=10, "let a = 1;", "\nlet z ="),
    expect: "M M/a M/z M/b ⇒ :: module M = { ⏎ ··let a = 1; ⏎ ··let z = ⏎ ··let b = 2 ⏎ }; ⏎ M.a",
  },
  {
    what: "paste a partial definition (`let z =`) after a chain's `in` (control)",
    root: Exp,
    src: "let a = 1 in\nlet b = 2 in\na",
    run: paste_rows(~off=12, "let a = 1 in", "\nlet z ="),
    expect: "a z b ⇒ :: let a = 1 in ⏎ let z = ⏎ let b = 2 in ⏎ a",
  },
  {
    what: "paste a member at the end (after the last, `;`-less member)",
    root: Mod,
    src: "module M = {\n  let a = 1;\n  let b = 2\n};\nM.a",
    run: paste_rows(~off=9, "let b = 2", ";\nlet z = 3"),
    expect: "M M/a M/b M/z ⇒ :: module M = { ⏎ ··let a = 1; ⏎ ··let b = 2; ⏎ ··let z = 3 ⏎ }; ⏎ M.a",
  },
  {
    /* outline ops on an unfinished member that swallowed the one below */

    what: "delete an unfinished member's row (`let q =` above `next`)",
    root: Mod,
    src: "module Counter = {\n  let start = 0;\n  let q =\n  let next = start + 1;\n  let after = next + 1\n};\nCounter.after",
    run: outline_op(Delete, "q"),
    expect: "it can't go here",
  },
  {
    what: "move an unfinished member's row down",
    root: Mod,
    src: "module Counter = {\n  let start = 0;\n  let q =\n  let next = start + 1;\n  let after = next + 1\n};\nCounter.after",
    run: outline_op(MoveDown, "q"),
    expect: "it can't move there",
  },
  {
    what: "duplicate an unfinished member's row",
    root: Mod,
    src: "module Counter = {\n  let start = 0;\n  let q =\n  let next = start + 1;\n  let after = next + 1\n};\nCounter.after",
    run: outline_op(Duplicate, "q"),
    expect: "it can't go here",
  },
  {
    /* outline Tab with a doc comment above the item */

    what: "Tab on a doc-commented member",
    root: Mod,
    src: "module M = {\n  let init = (1, 2);\n\n  # doc for f #\n  let f = fun x -> x\n};\nM.f",
    run: outline_op(Indent, "f"),
    expect: "module M = { ⏎ ··let init = ⏎ ····# doc for f # ⏎ ····let f = fun x -> x in ⏎ ····(1, 2) ⏎ }; ⏎ M.f",
  },
  {
    what: "Tab on a doc-commented definition (control)",
    root: Exp,
    src: "let init = (1, 2) in\n\n# doc for f #\nlet f = fun x -> x in\nf",
    run: outline_op(Indent, "f"),
    expect: "let init = ⏎ ··# doc for f # ⏎ ··let f = fun x -> x in ⏎ ··(1, 2) in ⏎ f",
  },
  {
    what: "Tab on a module member below a module (into it)",
    root: Mod,
    src: "module A = {\n  let a = 1\n};\n\n# doc for b #\nlet b = 2;\nA.a",
    run: outline_op(Indent, "b"),
    expect: "module A = { ⏎ ··let a = 1; ⏎  ⏎ ··# doc for b # ⏎ ··let b = 2 ⏎ }; ⏎ A.a",
  },
];

let features = () =>
  if (!enabled) {
    print_endline("ModAudit features: skipped (MODAUDIT=1 runs it)");
  } else {
    run_probes("feature probes", probes());
  };

/* ---- minimal repros: delete the [k]-th [tok] delimiter of a small
   program and print how completion reads it ---- */

let repro = (~root: Sort.t, ~k=0, src: string, tok: string): string => {
  let z0 = load(~root, src);
  let syntax = CachedSyntax.init(~root, z0);
  let statics = statics_of(~root, z0);
  let base = snapshot(~root, z0);
  let ts =
    List.filter(
      (t: target) => t.tok == tok,
      targets(~root, syntax.measured, z0),
    );
  switch (List.nth_opt(ts, k)) {
  | None => "no " ++ tok
  | Some(t) =>
    switch (delete_at(~root, ~syntax, ~statics, z0, t)) {
    | None => "refused"
    | Some(z) =>
      let s = snapshot(~root, z);
      let row = row_at(~measured=syntax.measured, base, t.pt);
      let r = ripple(~k=row, base, s);
      /* MODAUDIT_PAT=1: the first definition's pattern as the term has it */
      if (Sys.getenv_opt("MODAUDIT_PAT") != None) {
        let rec first_pat = (e: Exp.t) =>
          switch (e.term) {
          | Let(p, _, _) => Some(Pat.show(p))
          | ModuleExp(_, d, _) => first_pat(d)
          | Module(items) =>
            List.find_map(
              (m: Mod.t) =>
                switch (m.term) {
                | ModLet(p, _) => Some(Pat.show(p))
                | _ => None
                },
              items,
            )
          | _ => None
          };
        print_endline(
          "      pattern: "
          ++ (
            Option.value(first_pat(s.term), ~default="?")
            |> String.map(c => c == '\n' ? ' ' : c)
            |> Str.global_replace(Str.regexp("  +"), " ")
          ),
        );
      };
      Printf.sprintf(
        "%s\n      reads as: %s%s",
        show_lines(String.split_on_char('\n', text_of(z))),
        show_lines(
          String.split_on_char('\n', CorpusUtil.text_of(s.completed)),
        ),
        quiet(r) ? "" : "\n      ripple: " ++ show_ripple(r),
      );
    }
  };
};

let repros: list((string, Sort.t, string, string, int)) = [
  /* (what, root, program, token, occurrence) */
  (
    "case deleted in a member's fun body",
    Mod,
    "let f = fun x ->\n  case x\n  | 1 => 2\n  | _ => 3\n  end;\nlet g = 1;\ng",
    "case",
    0,
  ),
  (
    "case deleted in a chain fun body (control)",
    Exp,
    "let f = fun x ->\n  case x\n  | 1 => 2\n  | _ => 3\n  end in\nlet g = 1 in\ng",
    "case",
    0,
  ),
  (
    "case deleted, one-line member",
    Mod,
    "let f = fun x -> case x | 1 => 2 | _ => 3 end;\nlet g = 1;\ng",
    "case",
    0,
  ),
  (
    "case deleted, one-line chain (control)",
    Exp,
    "let f = fun x -> case x | 1 => 2 | _ => 3 end in\nlet g = 1 in\ng",
    "case",
    0,
  ),
  (
    "( deleted in a member",
    Mod,
    "let f = fun x -> x;\nlet g = f(1 + 2);\ng",
    "(",
    0,
  ),
  (
    "( deleted in a chain (control)",
    Exp,
    "let f = fun x -> x in\nlet g = f(1 + 2) in\ng",
    "(",
    0,
  ),
  (
    "( deleted in a member, parens",
    Mod,
    "let g = (1 + 2) * 3;\nlet h = 1;\nh",
    "(",
    0,
  ),
  (
    "( deleted in a chain, parens (control)",
    Exp,
    "let g = (1 + 2) * 3 in\nlet h = 1 in\nh",
    "(",
    0,
  ),
  (
    "[ deleted in a member",
    Mod,
    "let xs = [1, 2, 3];\nlet g = 1;\ng",
    "[",
    0,
  ),
  (
    "[ deleted in a chain (control)",
    Exp,
    "let xs = [1, 2, 3] in\nlet g = 1 in\ng",
    "[",
    0,
  ),
  (
    "if deleted in a member",
    Mod,
    "let f = fun x ->\n  if x then 1 else 2;\nlet g = 1;\ng",
    "if",
    0,
  ),
  (
    "if deleted in a chain (control)",
    Exp,
    "let f = fun x ->\n  if x then 1 else 2 in\nlet g = 1 in\ng",
    "if",
    0,
  ),
  (
    "fun deleted in a member",
    Mod,
    "let f = fun x -> x + 1;\nlet g = 1;\ng",
    "fun",
    0,
  ),
  (
    "fun deleted in a chain (control)",
    Exp,
    "let f = fun x -> x + 1 in\nlet g = 1 in\ng",
    "fun",
    0,
  ),
  (
    "; deleted before the last member",
    Mod,
    "let a = 1;\nlet b = a + 1;\nlet c = b + 1",
    ";",
    1,
  ),
  (
    "; deleted before a middle member",
    Mod,
    "let a = 1;\nlet b = a + 1;\nlet c = b + 1;\nc",
    ";",
    0,
  ),
  (
    "in deleted before the last definition (control)",
    Exp,
    "let a = 1 in\nlet b = a + 1 in\nlet c = b + 1 in\nc",
    "in",
    1,
  ),
  (
    "let deleted, member used below (Text)",
    Mod,
    "module Text = {\n  # Digits #\n  let digit : Int -> String = fun d ->\n    case d\n    | 0 => \"0\" | _ => \"9\"\n    end;\n\n  let go_render : Int -> String = fun k ->\n    digit(k)\n};\nText.go_render(1)",
    "let",
    0,
  ),
  (
    "let deleted, member used below, no comment",
    Mod,
    "module Text = {\n  let digit : Int -> String = fun d ->\n    case d\n    | 0 => \"0\" | _ => \"9\"\n    end;\n\n  let go_render : Int -> String = fun k ->\n    digit(k)\n};\nText.go_render(1)",
    "let",
    0,
  ),
  (
    "let deleted, member used below, one line",
    Mod,
    "module Text = {\n  let digit = fun d -> d;\n  let go_render = fun k -> digit(k)\n};\nText.go_render(1)",
    "let",
    0,
  ),
  (
    "let deleted, chain def used below (control)",
    Exp,
    "let digit : Int -> String = fun d ->\n  case d\n  | 0 => \"0\" | _ => \"9\"\n  end in\n\nlet go_render : Int -> String = fun k ->\n  digit(k) in\ngo_render(1)",
    "let",
    0,
  ),
  (
    "module deleted: a signed module",
    Exp,
    "module M : {let x : Int} = {let x = 1} in\nlet y = M.x in\ny",
    "module",
    0,
  ),
  (
    "module deleted: an unsigned module",
    Exp,
    "module M = {let x = 1} in\nlet y = M.x in\ny",
    "module",
    0,
  ),
  (
    "let deleted v1: annotated one-liner (control)",
    Exp,
    "let digit : Int -> Int = fun d -> d in\nlet go = fun k -> digit(k) in\ngo(1)",
    "let",
    0,
  ),
  (
    "let deleted v2: unannotated two-liner (control)",
    Exp,
    "let digit = fun d ->\n  d in\nlet go = fun k -> digit(k) in\ngo(1)",
    "let",
    0,
  ),
  (
    "let deleted v3: annotated two-liner (control)",
    Exp,
    "let digit : Int -> Int = fun d ->\n  d in\nlet go = fun k -> digit(k) in\ngo(1)",
    "let",
    0,
  ),
  (
    "let deleted v4: plain value two-liner (control)",
    Exp,
    "let digit =\n  1 in\nlet go = digit + 1 in\ngo",
    "let",
    0,
  ),
  (
    "let deleted v5: plain value one-liner (control)",
    Exp,
    "let digit = 1 in\nlet go = digit + 1 in\ngo",
    "let",
    0,
  ),
  (
    "} deleted, module literal at a Mod top",
    Mod,
    "module M = {\n  let a = 1;\n  let b = 2\n};\nM.a",
    "}",
    0,
  ),
  (
    "{ deleted",
    Mod,
    "module M = {\n  let a = 1;\n  let b = 2\n};\nM.a",
    "{",
    0,
  ),
  (
    "} deleted",
    Exp,
    "module M = {\n  let a = 1;\n  let b = 2\n} in\nM.a",
    "}",
    0,
  ),
];

/* a paste of [n] definitions into a module body, at a Mod-rooted top
   level and into a let-in chain: milliseconds, and the path taken */
let paste_timing = (n: int) => {
  let block = (sep: string) =>
    String.concat(
      "",
      List.init(n, i => Printf.sprintf("\nlet x%d = %d + 1%s", i, i, sep)),
    );
  let cases = [
    (
      "module body",
      Sort.Mod,
      "module M = {\n  let a = 1;\n  let b = 2\n};\nM.a",
      "let a = 1;",
      block(";"),
    ),
    (
      "Mod-rooted top",
      Sort.Mod,
      "let a = 1;\nlet b = 2;\na",
      "let a = 1;",
      block(";"),
    ),
    (
      "let-in chain",
      Sort.Exp,
      "let a = 1 in\nlet b = 2 in\na",
      "let a = 1 in",
      block(" in"),
    ),
  ];
  List.iter(
    ((what, root, src, needle, clip)) => {
      let t0 = Sys.time();
      let r =
        fst(
          act_st(
            ~root,
            ~off=String.length(needle),
            src,
            needle,
            [Paste(clip)],
          ),
        );
      let ms = (Sys.time() -. t0) *. 1000.;
      Printf.printf(
        "   paste %d definitions into a %s: %.0f ms (%s)\n",
        n,
        what,
        ms,
        switch (r) {
        | Ok(z) =>
          let rows =
            rows_of(
              MakeTerm.Incr.term_of_root(~root, Zipper.unselect_and_zip(z)),
            );
          string_of_int(List.length(rows))
          ++ " rows after"
          ++ (verbose ? ": " ++ String.concat(" ", List.map(fst, rows)) : "");
        | Error(why) => why
        },
      );
    },
    cases,
  );
};

let run_repros = () =>
  if (!enabled) {
    print_endline("ModAudit repros: skipped (MODAUDIT=1 runs it)");
  } else {
    if (selected("paste")) {
      paste_timing(5);
      paste_timing(20);
      paste_timing(80);
    };
    List.iter(
      ((what, root, src, tok, k)) =>
        if (selected(what)) {
          Printf.printf(
            "-- %s [%s]: delete %s\n      %s\n",
            what,
            Sort.show(root),
            tok,
            switch (repro(~root, ~k, src, tok)) {
            | s => s
            | exception e => "raised " ++ Printexc.to_string(e)
            },
          );
        },
      repros,
    );
    ();
  };

let tests = (
  "ModAudit",
  [
    test_case("deletion sweep", `Slow, deletions),
    test_case("typing sweep", `Slow, typing),
    test_case("outline ops", `Slow, ops),
    test_case("feature probes", `Slow, features),
    test_case("repros", `Slow, run_repros),
  ],
);
