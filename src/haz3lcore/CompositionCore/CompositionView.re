open Util;
module Trivia = Secondary;
open Language;

/* The agent's view of the program (plans/editing-in-the-large.html, "What
   the agent sees"): a code map, the program with its definitions' bodies
   spliced to ⋯ and annotations in comments, then the entries the agent
   has open, in full, each under a comment naming its path. One printer,
   with overlays for errors, test results and probe values. It is rendered
   text: nothing is placed in the program. */

module O = Outline;
module S = ItemSpans;

type settings = {
  /* modules list their members this many levels down; deeper ones show
     as `{ ⋯ }` */
  module_depth: int,
  /* type definitions in full in the map, else spliced as other bodies */
  types_in_full: bool,
};

let default_settings = {
  module_depth: 2,
  types_in_full: true,
};

/* what the overlays read; any may be missing */
type facts = {
  info_map: Statics.Map.t,
  error_ids: list(Id.t),
  tests: option(TestResults.t),
  probes: Sample.Map.t,
  refractors: Refractors.RefractorList.t,
  /* the program's value, shown on its result */
  value: option(Exp.t),
};

let no_facts = {
  info_map: Id.Map.empty,
  error_ids: [],
  tests: None,
  probes: Sample.Map.empty,
  refractors: [],
  value: None,
};

let elided = {js|⋯|js};

/* ---- printing ---- */

let projector_to_segment = Triggers.projector_to_invoke;

/* [seg] with projectors as their invocations, so lines and columns
   measure as they print */
let invoked = (seg: Segment.t): Segment.t =>
  ZipperBase.MapPiece.of_segment(Printer.remove_projector, seg);

let print_segment = (segment: Segment.t): string =>
  Printer.of_segment(~holes="?", ~projector_to_segment, segment);

let print_zipper = (z: Zipper.t): string =>
  Printer.of_zipper(~holes="?", ~concave_holes="~", ~projector_to_segment, z);

/* text and a row per id, of a segment printed with probed terms
   bracketed */
let print_measured =
    (~refractors, seg: Segment.t)
    : (array(string), Id.t => option(Measured.measurement)) => {
  let seg = invoked(seg);
  let text =
    Printer.of_segment(
      ~holes="?",
      ~concave_holes="~",
      ~refractors,
      ~refractor_seg_to_seg=Triggers.refractor_seg_to_seg_text,
      seg,
    );
  let measured = lazy(Measured.of_segment(seg, Id.Map.empty, Id.Map.empty));
  /* only what it holds: the rest is elided or elsewhere */
  let held =
    lazy(
      List.fold_left(
        (m, id) => Id.Map.add(id, (), m),
        Id.Map.empty,
        Segment.ids(seg),
      )
    );
  (
    Array.of_list(String.split_on_char('\n', text)),
    id =>
      Id.Map.mem(id, Lazy.force(held))
        ? Measured.find_by_id(id, Lazy.force(measured)) : None,
  );
};

/* text safe inside a `# … #` comment */
let comment_safe = (s: string): string =>
  s
  |> String.map(c => c == '\n' ? ' ' : c)
  |> StringUtil.replace(StringUtil.regexp("#"), _, {js|♯|js});

let comment = (parts: list(string)): string =>
  switch (List.filter(p => p != "", parts)) {
  | [] => ""
  | parts => "# " ++ String.concat({js| · |js}, parts) ++ " #"
  };

/* a path's last element */
let last_elem = (path: string): string =>
  switch (String.rindex_opt(path, '/')) {
  | Some(i) => String.sub(path, i + 1, String.length(path) - i - 1)
  | None => path
  };

let plural = (n: int, one: string, many: string): string =>
  string_of_int(n) ++ " " ++ (n == 1 ? one : many);

/* ---- facts by entry ---- */

let test_status = (facts: facts, test: Id.t): option(TestStatus.t) =>
  Option.bind(facts.tests, (tr: TestResults.t) =>
    TestMap.lookup(test, tr.test_map) |> Option.map(TestMap.joint_status)
  );

let status_mark = (s: TestStatus.t): string =>
  switch (s) {
  | Pass => {js|✓|js}
  | Fail => {js|✗|js}
  | Indet => "indeterminate"
  };

let error_message = (facts: facts, id: Id.t): string =>
  switch (Id.Map.find_opt(id, facts.info_map)) {
  | Some(info) =>
    comment_safe(ErrorPrint.string_of_marks(info, Info.marks_of(info)))
  | None => "error"
  };

let probe_values = (facts: facts, ids: list(Id.t)): string =>
  ProbeText.format_probe_values(~window=Single, ~probe_map=facts.probes, ids);

/* ---- the map ---- */

let elision = (): Piece.t =>
  Piece.mk_tile((Form.Tok(elided), Sort.Any), []);
let space = (): Piece.t => Piece.Secondary(Trivia.mk_space(Id.mk()));

/* a kid replaced by ` ⋯ `, and the ⋯'s id */
let elide_kid = (): (Segment.t, Id.t) => {
  let e = elision();
  ([space(), e, space()], Piece.id(e));
};

let rec spans_lines = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) => List.exists(List.exists(spans_lines), t.children)
  | _ => Piece.is_linebreak(p)
  };

/* tile [t] with its last child replaced */
let with_last_kid = (t: Tile.t, kid: Segment.t): Tile.t => {
  let (init, _) = ListUtil.split_last(t.children);
  {
    ...t,
    children: init @ [kid],
  };
};

/* an annotation: a line, found by an id's measurement, and its parts */
type note = {
  /* the entry's name, when several notes share a line */
  label: string,
  at: Id.t,
  /* at the measurement's last row, else its first */
  last: bool,
  parts: list(string),
};

type ctx = {
  settings,
  facts,
  opened: Id.Map.t(unit),
  errors: Id.Map.t(list(Id.t)),
  uses: Id.Map.t(int),
};

let rec subtree_errors = (ctx: ctx, e: O.entry): int =>
  List.length(Option.value(Id.Map.find_opt(e.id, ctx.errors), ~default=[]))
  + List.fold_left((n, c) => n + subtree_errors(ctx, c), 0, e.children);

let own_errors = (ctx: ctx, e: O.entry): int =>
  List.length(Option.value(Id.Map.find_opt(e.id, ctx.errors), ~default=[]));

let uses_of = (ctx: ctx, e: O.entry): option(int) =>
  switch (e.kind, e.names) {
  | (Def, [(_, tok), ..._]) =>
    Some(Option.value(Id.Map.find_opt(tok, ctx.uses), ~default=0))
  | _ => None
  };

/* the parts every map line carries for its entry: open, errors */
let common_parts = (ctx: ctx, e: O.entry, ~errors: int): list(string) => [
  Id.Map.mem(e.id, ctx.opened) ? "open below" : "",
  errors > 0 ? plural(errors, "error", "errors") : "",
];

let first_content = (seg: Segment.t): option(Id.t) =>
  switch (O.content(seg)) {
  | [p, ..._] => Some(Piece.id(p))
  | [] => None
  };
let last_content = (seg: Segment.t): option(Id.t) =>
  first_content(List.rev(seg));

/* level [lv]'s pieces with its entries' bodies elided, [depth] module
   levels down, and the notes for their lines */
let rec map_level =
        (ctx: ctx, ~depth: int, lv: S.level, es: list(O.entry))
        : (Segment.t, list(note)) => {
  let arr = Array.copy(lv.pieces);
  /* replaced runs of the level's pieces: [a, b) by a segment */
  let runs = ref([]);
  let notes = ref([]);
  let note = (~last=false, ~label="", at, parts) =>
    notes :=
      [
        {
          label,
          at,
          last,
          parts,
        },
        ...notes^,
      ];
  List.iter(
    (e: O.entry) => {
      let k = e.k;
      let sp = lv.spans[k];
      let (c0, c1) = sp.core;
      let head = first_content(O.core(lv, k));
      let note = note(~label=O.path_string(e) |> last_elem);
      let tail_end = last_content(O.core(lv, k));
      let elide_def = () =>
        switch (S.site_of(lv, k)) {
        | Some(Kid(h, t)) =>
          let (kid, mark) = elide_kid();
          arr[h] = Tile(with_last_kid(t, kid));
          Some(mark);
        | Some(Run(_, a, b)) when a < b =>
          let e = elision();
          runs := [(a, b, [space(), e]), ...runs^];
          Some(Piece.id(e));
        | _ => None
        };
      switch (e.kind) {
      | Def =>
        let mark = elide_def();
        let uses =
          switch (uses_of(ctx, e)) {
          | Some(n) when n > 0 => plural(n, "use", "uses")
          | _ => ""
          };
        Option.iter(
          at =>
            note(
              at,
              common_parts(ctx, e, ~errors=subtree_errors(ctx, e)) @ [uses],
            ),
          mark,
        );
      | Type when !ctx.settings.types_in_full =>
        Option.iter(
          at =>
            note(at, common_parts(ctx, e, ~errors=subtree_errors(ctx, e))),
          elide_def(),
        )
      | Type =>
        Option.iter(
          at =>
            note(at, common_parts(ctx, e, ~errors=subtree_errors(ctx, e))),
          head,
        )
      | Module when e.literal && depth < ctx.settings.module_depth =>
        /* its members, one level down */
        switch (S.members(lv, k), S.site_of(lv, k)) {
        | (Some(m), Some(site)) =>
          let (members, below) =
            map_level(ctx, ~depth=depth + 1, m, e.children);
          notes := below @ notes^;
          replace_brace(arr, lv, site, members);
          Option.iter(
            at =>
              note(at, common_parts(ctx, e, ~errors=own_errors(ctx, e))),
            head,
          );
        | _ => ()
        }
      | Module when e.literal =>
        switch (S.site_of(lv, k)) {
        | Some(site) =>
          let (kid, mark) = elide_kid();
          replace_brace(arr, lv, site, kid);
          note(
            mark,
            common_parts(ctx, e, ~errors=subtree_errors(ctx, e))
            @ [plural(List.length(e.children), "member", "members")],
          );
        | None => ()
        }
      | Module =>
        Option.iter(
          at =>
            note(at, common_parts(ctx, e, ~errors=subtree_errors(ctx, e))),
          head,
        )
      | Test =>
        /* `test ⋯ end`: the test tile's body */
        let status =
          switch (Option.bind(e.test, test_status(ctx.facts))) {
          | Some(s) => status_mark(s)
          | None => ""
          };
        switch (S.only_content(lv.pieces, c0, c1)) {
        | Some(i) =>
          switch (arr[i]) {
          | Tile({children: [_, ..._], _} as t) =>
            /* a one-line test is its own best summary */
            if (spans_lines(arr[i])) {
              let (kid, _) = elide_kid();
              arr[i] = Tile(with_last_kid(t, kid));
            };
            note(
              t.id,
              common_parts(ctx, e, ~errors=subtree_errors(ctx, e))
              @ [status],
            );
          | _ => ()
          }
        | None => ()
        };
      | Tail =>
        let value =
          switch (ctx.facts.value) {
          | Some(v) when lv.owner == Top =>
            {js|⇒ |js} ++ comment_safe(ProbeText.format_value(v))
          | _ => ""
          };
        Option.iter(
          at =>
            note(
              ~last=true,
              at,
              common_parts(ctx, e, ~errors=subtree_errors(ctx, e)) @ [value],
            ),
          tail_end,
        );
      | Stmt
      | Use =>
        Option.iter(
          at =>
            note(
              ~last=true,
              at,
              common_parts(ctx, e, ~errors=subtree_errors(ctx, e)),
            ),
          tail_end,
        )
      };
    },
    es,
  );
  /* the level's pieces with the runs replaced */
  let runs = List.sort(((a, _, _), (b, _, _)) => compare(a, b), runs^);
  let rec splice = (i, runs) =>
    if (i >= Array.length(arr)) {
      [];
    } else {
      switch (runs) {
      | [(a, b, seg), ...rest] when a == i => seg @ splice(b, rest)
      | _ => [arr[i], ...splice(i + 1, runs)]
      };
    };
  (splice(0, runs), notes^);
}

/* a module literal's brace, in its site, given new members */
and replace_brace =
    (arr: array(Piece.t), lv: S.level, site: S.site, members: Segment.t)
    : unit =>
  switch (S.brace_of(lv.pieces, site)) {
  | Some((m, brace)) =>
    let brace' =
      Piece.Tile({
        ...brace,
        children: [members],
      });
    switch (site) {
    | Kid(h, t) =>
      let kid = ListUtil.last(t.children);
      arr[h] = Tile(with_last_kid(t, ListUtil.put_nth(m, brace', kid)));
    | Run(_) => arr[m] = brace'
    };
  | None => ()
  };

/* [lines] with each note's parts at the end of its line */
let annotate =
    (
      lines: array(string),
      find: Id.t => option(Measured.measurement),
      notes: list(note),
    )
    : array(string) => {
  let by_row = Hashtbl.create(16);
  List.iter(
    n =>
      switch (List.filter(p => p != "", n.parts), find(n.at)) {
      | (_, None) => ()
      | (parts, Some(m)) =>
        let row = n.last ? m.last.row : m.origin.row;
        Hashtbl.replace(
          by_row,
          row,
          Option.value(Hashtbl.find_opt(by_row, row), ~default=[])
          @ [(n.label, parts)],
        );
      },
    List.rev(notes),
  );
  /* entries sharing a line name theirs */
  Array.mapi(
    (row, line) =>
      switch (Hashtbl.find_opt(by_row, row)) {
      | Some([(_, parts)]) when parts != [] =>
        line ++ "  " ++ comment(parts)
      | Some([_, _, ..._] as several) =>
        switch (
          List.filter_map(
            ((label, parts)) =>
              parts == []
                ? None : Some(label ++ ": " ++ String.concat(", ", parts)),
            several,
          )
        ) {
        | [] => line
        | parts => line ++ "  " ++ comment(parts)
        }
      | _ => line
      },
    lines,
  );
};

/* the probed ids among [find]'s, by the row their values go on: the
   last row of the probed term */
let probe_rows =
    (facts: facts, find: Id.t => option(Measured.measurement))
    : IntMap.t(list(Id.t)) =>
  List.fold_right(
    ((id, entry: Refractors.entry), acc) =>
      entry.kind != Probe
        ? acc
        : (
          switch (find(id)) {
          | Some(m) =>
            IntMap.update(
              m.last.row,
              l => Some([id, ...Option.value(l, ~default=[])]),
              acc,
            )
          | None => acc
          }
        ),
    facts.refractors,
    IntMap.empty,
  );

let with_probes = (facts, find, lines: array(string)): array(string) => {
  let rows = probe_rows(facts, find);
  Array.mapi(
    (row, line) =>
      switch (IntMap.find_opt(row, rows)) {
      | Some(ids) => line ++ probe_values(facts, ids)
      | None => line
      },
    lines,
  );
};

let strip_trailing_blank = (lines: list(string)): list(string) => {
  let rec drop = l =>
    switch (l) {
    | [s, ...rest] when String.trim(s) == "" => drop(rest)
    | _ => l
    };
  List.rev(drop(List.rev(drop(lines))));
};

let summary = (ctx: ctx): string => {
  let errors = List.length(ctx.facts.error_ids);
  let tests =
    switch (ctx.facts.tests) {
    | Some(tr) when tr.total > 0 =>
      Printf.sprintf(
        "tests: %d of %d pass%s",
        tr.passing,
        tr.total,
        tr.failing > 0 ? Printf.sprintf(", %d fail", tr.failing) : "",
      )
    | _ => ""
    };
  comment([
    "map",
    errors > 0 ? plural(errors, "error", "errors") : "no errors",
    tests,
  ]);
};

let map_text = (ctx: ctx, o: O.t): string => {
  let top = S.top(~root=o.root, o.seg);
  let (seg, notes) = map_level(ctx, ~depth=0, top, o.entries);
  let (lines, find) = print_measured(~refractors=ctx.facts.refractors, seg);
  let lines = annotate(lines, find, notes) |> with_probes(ctx.facts, find);
  String.concat(
    "\n",
    [summary(ctx), ...strip_trailing_blank(Array.to_list(lines))],
  );
};

/* ---- open entries ---- */

/* an entry in full: its core, later lines dedented to its first line's
   column, errors marked under their lines, probe values and test results
   at line ends */
let entry_text = (ctx: ctx, e: O.entry): string => {
  let base = e.lv.spans[e.k].base_col;
  let seg = O.core(e.lv, e.k);
  let (lines, find) = print_measured(~refractors=ctx.facts.refractors, seg);
  let dedent = (row, line) =>
    row == 0
      ? line
      : {
        let n = String.length(line);
        let rec spaces = i =>
          i < n && i < base && line.[i] == ' ' ? spaces(i + 1) : i;
        let k = spaces(0);
        String.sub(line, k, n - k);
      };
  let shift = row => row == 0 ? 0 : base;
  /* the tests below, at their lines */
  let rec tests = (e: O.entry) =>
    (
      switch (e.test) {
      | Some(t) =>
        switch (test_status(ctx.facts, t)) {
        | Some(s) => [
            {
              label: "",
              at: t,
              last: true,
              parts: [status_mark(s)],
            },
          ]
        | None => []
        }
      | None => []
      }
    )
    @ List.concat_map(tests, e.children);
  let lines =
    annotate(lines, find, tests(e)) |> with_probes(ctx.facts, find);
  let rec errs = (e: O.entry) =>
    Option.value(Id.Map.find_opt(e.id, ctx.errors), ~default=[])
    @ List.concat_map(errs, e.children);
  /* caret lines under each row, left to right */
  let carets = Hashtbl.create(4);
  let at = err =>
    switch (find(err)) {
    | Some(m) => (m.origin.row, max(0, m.origin.col - shift(m.origin.row)))
    | None => (0, 0)
    };
  List.iter(
    err => {
      let msg = error_message(ctx.facts, err);
      let (row, col) = at(err);
      let line =
        col <= 0
          ? "#^ " ++ msg ++ " #"
          : "#" ++ String.make(col - 1, ' ') ++ "^ " ++ msg ++ " #";
      Hashtbl.replace(
        carets,
        row,
        Option.value(Hashtbl.find_opt(carets, row), ~default=[]) @ [line],
      );
    },
    List.stable_sort((a, b) => compare(at(a), at(b)), errs(e)),
  );
  Array.to_list(lines)
  |> List.mapi((row, line) =>
       [
         dedent(row, line),
         ...Option.value(Hashtbl.find_opt(carets, row), ~default=[]),
       ]
     )
  |> List.concat
  |> String.concat("\n");
};

/* the open entries, outermost first in program order; an entry inside
   another open one shows in it */
let open_entries = (o: O.t, opened: Id.Map.t(unit)): list(O.entry) => {
  let rec go = (es: list(O.entry)) =>
    List.concat_map(
      (e: O.entry) => Id.Map.mem(e.id, opened) ? [e] : go(e.children),
      es,
    );
  go(o.entries);
};

let mk_ctx = (~settings, ~facts, o: O.t, opened: list(Id.t)): ctx => {
  settings,
  facts,
  opened:
    List.fold_left((m, id) => Id.Map.add(id, (), m), Id.Map.empty, opened),
  errors: O.errors_by_entry(o, ~info_map=facts.info_map, facts.error_ids),
  uses: O.uses(o, ~info_map=facts.info_map),
};

/* the view: the map, then each open entry */
let render =
    (
      ~settings=default_settings,
      ~facts=no_facts,
      o: O.t,
      ~opened: list(Id.t),
    )
    : string => {
  let ctx = mk_ctx(~settings, ~facts, o, opened);
  String.concat(
    "\n\n",
    [
      map_text(ctx, o),
      ...List.map(
           (e: O.entry) =>
             comment(["open: " ++ O.path_string(e)])
             ++ "\n"
             ++ entry_text(ctx, e),
           open_entries(o, ctx.opened),
         ),
    ],
  );
};

module Public = {
  let print_segment = print_segment;
  let print_zipper = print_zipper;
  let render = render;
};
