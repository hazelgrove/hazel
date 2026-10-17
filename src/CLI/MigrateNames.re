/* `hazel migrate`: renames uses of library names in program text, by
   scope (plans/stdlib.md §5). A table of `let old = New.path;` lines says
   what moved. A variable named `old` is rewritten only where it resolves
   to the library's binding of that name, or to nothing; a program's own
   `map` or `init` stays. Everything else in the file stays byte for
   byte: each rewrite replaces one identifier token of the source. */
open Haz3lcore;
open Language;

type renames = list((string, string));

/* `let old = New.path;` lines; other lines (comments) are skipped */
let renames_of_text = (text: string): renames =>
  Stdlib.String.split_on_char('\n', text)
  |> List.filter_map(line =>
       switch (
         Stdlib.String.split_on_char(' ', Stdlib.String.trim(line))
         |> List.filter(w => w != "")
       ) {
       | ["let", old, "=", target] =>
         let n = Stdlib.String.length(target);
         Some((
           old,
           n > 0 && target.[n - 1] == ';'
             ? Stdlib.String.sub(target, 0, n - 1) : target,
         ));
       | _ => None
       }
     );

/* What becomes of each variable named in the table: Some(new name) or
   None (it is the program's own), keyed by its tile id */
type plan = {
  decisions: Id.Map.t(option(string)),
  problems: list(string),
};

/* A variable is renamed when it resolves to the library's binder of its
   name, or is unbound, and its new home module is the library's there. */
let plan = (~renames: renames, term: Exp.t): plan => {
  let library_id = name =>
    List.find_map(
      fun
      | Ctx.VarEntry(v) when v.name == name => Some(v.id)
      | _ => None,
      Lazy.force(Builtins.library_entries^),
    );
  let (info_map, _) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      term,
    );
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      switch (info) {
      | InfoExp({
          user_term: {term: Var(name), _} as user,
          ctx,
          label_sort,
          _,
        })
          when !label_sort && id == Exp.rep_id(user) =>
        switch (List.assoc_opt(name, renames)) {
        | None => acc
        | Some(target) =>
          let resolves_to_library =
            switch (Ctx.lookup_var(ctx, name)) {
            | None => true
            | Some(v) => Some(v.id) == library_id(name)
            };
          let home = List.hd(Stdlib.String.split_on_char('.', target));
          let home_visible =
            switch (Ctx.lookup_var(ctx, home)) {
            | Some(v) => Some(v.id) == library_id(home)
            | None => false
            };
          let decide = d => {
            ...acc,
            decisions: Id.Map.add(id, d, acc.decisions),
          };
          if (!resolves_to_library) {
            decide(None);
          } else if (!home_visible) {
            {
              ...decide(None),
              problems: [
                Printf.sprintf(
                  "%s stays: %s is shadowed where it is used",
                  name,
                  home,
                ),
                ...acc.problems,
              ],
            };
          } else {
            decide(Some(target));
          };
        }
      | _ => acc
      },
    info_map,
    {
      decisions: Id.Map.empty,
      problems: [],
    },
  );
};

/* The alignment keeps word-like tokens only (names, keywords, true, _),
   which the lexer and the parsed segment spell alike. */
let wordish = (s: string): bool =>
  s != ""
  && (
    switch (s.[0]) {
    | 'a' .. 'z'
    | '_'
    | '$' => true
    | _ => false
    }
  );

/* The source's word tokens with their byte offsets, from the lexer the
   fast parser uses */
let source_words = (text: string): option(list((int, string))) => {
  MenhirParser.Lexer.reset_delims();
  let lexbuf = Lexing.from_string(text);
  let rec go = acc =>
    switch (MenhirParser.Lexer.token(lexbuf)) {
    | MenhirParser.Parser.EOF => Some(List.rev(acc))
    | _ =>
      let s = Lexing.lexeme(lexbuf);
      go(wordish(s) ? [(Lexing.lexeme_start(lexbuf), s), ...acc] : acc);
    | exception _ => None
    };
  go([]);
};

/* The segment's word tokens in text order, each with its tile's id */
let rec segment_words = (seg: Segment.t): list((string, Id.t)) =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) =>
        let label = Tile.label(t);
        let shard = i => {
          let s = List.nth(label, i);
          wordish(s) ? [(s, t.id)] : [];
        };
        switch (t.shards) {
        | [] => []
        | [first, ...rest] =>
          shard(first)
          @ List.concat(
              List.map2(
                (child, i) => segment_words(child) @ shard(i),
                t.children,
                rest,
              ),
            )
        };
      | Projector(pr) => segment_words([pr.syntax])
      | Grout(_)
      | Secondary(_) => []
      },
    seg,
  );

/* One rewrite: the byte offset of an old name in the text */
type edit = {
  start: int,
  old: string,
  new_: string,
};

/* Places [renamed] (tile id → new name) in [text], the text [z] was
   parsed from: the lexer's word tokens and the segment's must agree one
   for one, or nothing is placed. */
let edits = (~renamed: Id.Map.t(string), ~text: string, z: Zipper.t) => {
  let words = segment_words(Zipper.unselect_and_zip(~erase_buffer=true, z));
  switch (source_words(text)) {
  | None => Error("the lexer rejects the text")
  | Some(src) when List.length(src) != List.length(words) =>
    Error(
      Printf.sprintf(
        "%d words in the source but %d in the parse",
        List.length(src),
        List.length(words),
      ),
    )
  | Some(src) =>
    List.fold_left2(
      (acc, (start, s), (w, id)) =>
        switch (acc) {
        | Error(_) => acc
        | Ok(_) when s != w =>
          Error(
            Printf.sprintf(
              "at byte %d the source has '%s', the parse '%s'",
              start,
              s,
              w,
            ),
          )
        | Ok(es) =>
          switch (Id.Map.find_opt(id, renamed)) {
          | Some(new_) =>
            Ok([
              {
                start,
                old: s,
                new_,
              },
              ...es,
            ])
          | None => acc
          }
        },
      Ok([]),
      src,
      words,
    )
    |> Result.map(List.rev)
  };
};

/* [edits] (sorted) applied to [text], whose offsets they index */
let apply = (edits: list(edit), text: string): string => {
  let buf = Stdlib.Buffer.create(Stdlib.String.length(text) + 64);
  let pos =
    List.fold_left(
      (pos, e) => {
        Stdlib.Buffer.add_string(
          buf,
          Stdlib.String.sub(text, pos, e.start - pos),
        );
        Stdlib.Buffer.add_string(buf, e.new_);
        e.start + Stdlib.String.length(e.old);
      },
      0,
      edits,
    );
  Stdlib.Buffer.add_string(
    buf,
    Stdlib.String.sub(text, pos, Stdlib.String.length(text) - pos),
  );
  Stdlib.Buffer.contents(buf);
};

let parse = (text: string): option(Zipper.t) =>
  PersistentZipper.parse_text(~source="migrate", ~root=Exp, text);

let term_of = (z: Zipper.t): Exp.t =>
  MakeTerm.from_zip_for_sem(z, ~root=Exp).term;

/* What a program does, to compare before and after: its static errors
   (messages only, since ids and positions move), its result and its test
   statuses */
let behavior = (term: Exp.t): string => {
  let (info_map, _) = Run.statics_and_elab(term);
  let errors =
    Id.Map.fold(
      (id, info, acc) =>
        id == Info.id_of(info)
          ? switch (Info.marks_of(info)) {
            | [] => acc
            | marks => [ErrorPrint.string_of_marks(info, marks), ...acc]
            }
          : acc,
      info_map,
      [],
    )
    |> List.sort(compare);
  let run =
    switch (Run.evaluate_with_tests(term)) {
    | (result, tests) =>
      Print.print(result)
      ++ "\ntests: "
      ++ Stdlib.String.concat(
           " ",
           List.map(TestStatus.to_string, tests.statuses),
         )
    | exception e => "raised " ++ Printexc.to_string(e)
    };
  Stdlib.String.concat("\n", ["errors:", ...errors] @ [run]);
};

type outcome = {
  text: string,
  /* at the input text's offsets */
  edits: list(edit),
  problems: list(string),
  /* the behavior before and after, when asked to verify and it moved */
  changed: option((string, string)),
};

/* A unit of text to rewrite: one Exp program, parsed from [body], whose
   offsets [to_file] maps back to the file's */
type part = {
  label: string,
  body: string,
  to_file: int => int,
  zipper: Zipper.t,
};

/* Rewrites [parts] of [text] after checking [runs] (terms built from the
   parts' terms); each run decides for the tiles it contains, and runs
   that share a tile (a lesson's tests) must decide alike. */
let rewrite =
    (~renames, ~text, ~unparsed, ~parts: list(part), ~runs: list(Exp.t))
    : outcome => {
  let plans = List.map(plan(~renames), runs);
  let (renamed, disagreements) =
    List.fold_left(
      ((acc, bad), p: plan) =>
        Id.Map.fold(
          (id, d, (acc, bad)) =>
            switch (Id.Map.find_opt(id, acc)) {
            | Some(d') when d' != d => (acc, Id.Set.add(id, bad))
            | _ => (Id.Map.add(id, d, acc), bad)
            },
          p.decisions,
          (acc, bad),
        ),
      (Id.Map.empty, Id.Set.empty),
      plans,
    );
  let renamed =
    Id.Map.filter_map(
      (id, d) => Id.Set.mem(id, disagreements) ? None : d,
      renamed,
    );
  let problems =
    unparsed
    @ List.concat_map((p: plan) => p.problems, plans)
    @ (
      Id.Set.is_empty(disagreements)
        ? []
        : [
          Printf.sprintf(
            "%d names left: the code and the solution bind them differently",
            Id.Set.cardinal(disagreements),
          ),
        ]
    );
  let placed =
    List.map(
      p =>
        switch (edits(~renamed, ~text=p.body, p.zipper)) {
        | Ok(es) =>
          Ok(
            List.map(
              e =>
                {
                  ...e,
                  start: p.to_file(e.start),
                },
              es,
            ),
          )
        | Error(why) => Error(p.label ++ ": " ++ why)
        },
      parts,
    );
  switch (
    List.find_map(
      fun
      | Error(why) => Some(why)
      | Ok(_) => None,
      placed,
    )
  ) {
  | Some(why) => {
      text,
      edits: [],
      problems: ["cannot place rewrites: " ++ why, ...problems],
      changed: None,
    }
  | None =>
    let es =
      List.concat_map(Result.get_ok, placed)
      |> List.sort((a, b) => compare(a.start, b.start));
    {
      text: apply(es, text),
      edits: es,
      problems,
      changed: None,
    };
  };
};

/* [o] with [behaviors] of the text before and after, when they differ */
let verified =
    (~verify, ~before: string, o: outcome, behaviors: string => string) => {
  ...o,
  changed:
    !verify || o.edits == []
      ? None
      : {
        let (b, a) = (behaviors(before), behaviors(o.text));
        b == a ? None : Some((b, a));
      },
};

let program = (~renames, ~verify, text: string): outcome =>
  switch (parse(text)) {
  | None => {
      text,
      edits: [],
      problems: ["does not parse"],
      changed: None,
    }
  | Some(zipper) =>
    let o =
      rewrite(
        ~renames,
        ~text,
        ~unparsed=[],
        ~parts=[
          {
            label: "program",
            body: text,
            to_file: k => k,
            zipper,
          },
        ],
        ~runs=[term_of(zipper)],
      );
    verified(~verify, ~before=text, o, text =>
      switch (parse(text)) {
      | Some(z) => behavior(term_of(z))
      | None => "does not parse"
      }
    );
  };

/* Tutorial.stitch_term: the tests see the code's bindings, or only
   `answer` under the wrapper flag */
let stitch = (~wrapper: bool, code: Exp.t, tests: Exp.t): Exp.t =>
  Web.EditorUtil.append_exp(
    wrapper
      ? Let(Var("answer") |> Pat.fresh, code, EmptyHole |> Exp.fresh)
        |> Exp.fresh
      : code,
    tests,
  );

let is_space = c =>
  c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\012';

/* [lines] (file offset, text) as one text, newline terminated and
   trimmed as TutorialText trims a section, with a map from its offsets
   back to the file's */
let text_of_lines =
    (lines: list((int, string))): option((string, int => int)) => {
  let body =
    Stdlib.String.concat("", List.map(((_, l)) => l ++ "\n", lines));
  let n = Stdlib.String.length(body);
  let rec lead = i => i < n && is_space(body.[i]) ? lead(i + 1) : i;
  let lead = lead(0);
  let to_file = k => {
    let k = k + lead;
    let rec go = (base, ls) =>
      switch (ls) {
      | [] => failwith("offset past the text")
      | [(offset, l), ...rest] =>
        let len = Stdlib.String.length(l) + 1;
        k < base + len ? offset + (k - base) : go(base + len, rest);
      };
    go(0, lines);
  };
  switch (Stdlib.String.trim(body)) {
  | "" => None
  | trimmed => Some((trimmed, to_file))
  };
};

/* The text's lines, each with its offset */
let lines_of = (content: string): list((int, string)) =>
  Stdlib.String.split_on_char('\n', content)
  |> List.fold_left(
       ((offset, acc), line) =>
         (
           offset + Stdlib.String.length(line) + 1,
           [(offset, line), ...acc],
         ),
       (0, []),
     )
  |> snd
  |> List.rev;

/* A lesson's lines, each with the section it falls in (marker lines
   dropped) */
let tagged_lines =
    (content: string): list((Web.TutorialText.marker, (int, string))) =>
  List.fold_left(
    ((cur, acc), (offset, line)) =>
      switch (Web.TutorialText.marker_of_line(line)) {
      | Some(m) => (m, acc)
      | None => (cur, [(cur, (offset, line)), ...acc])
      },
    (Web.TutorialText.Code, []),
    lines_of(content),
  )
  |> snd
  |> List.rev;

/* The bodies of the Hazel code fences in markdown [lines]: ```hazel,
   ```hazelnoeval and ```hazelnostatics render as checked cells
   (ExplainThis) */
let fences = (lines: list((int, string))): list(list((int, string))) => {
  let opens = l =>
    switch (Stdlib.String.trim(l)) {
    | "```hazel"
    | "```hazelnoeval"
    | "```hazelnostatics" => true
    | _ => false
    };
  let (_, rev) =
    List.fold_left(
      ((cur, acc), (offset, l)) =>
        switch (cur) {
        | None => (opens(l) ? Some([]) : None, acc)
        | Some(body) when Stdlib.String.trim(l) == "```" => (
            None,
            [List.rev(body), ...acc],
          )
        | Some(body) => (Some([(offset, l), ...body]), acc)
        },
      (None, []),
      lines,
    );
  List.rev(rev);
};

let fence_parts = (lines): list((string, string, int => int)) =>
  List.filter_map(
    fence =>
      Option.map(
        ((body, to_file)) => ("a ```hazel fence", body, to_file),
        text_of_lines(fence),
      ),
    fences(lines),
  );

/* Parsed parts, and a problem for each that does not parse */
let parse_parts = (texts: list((string, string, int => int))) => (
  List.filter_map(
    ((label, body, to_file)) =>
      Option.map(
        zipper =>
          {
            label,
            body,
            to_file,
            zipper,
          },
        parse(body),
      ),
    texts,
  ),
  List.filter_map(
    ((label, body, _)) =>
      parse(body) == None ? Some(label ++ " does not parse") : None,
    texts,
  ),
);

/* A lesson (.hzt) rewrites its @code, @test and @solution sections,
   checked as the tutorial checks them (the tests after the code, and
   after the solution), and the Hazel fences in its prose sections, each
   a program of its own */
let lesson = (~renames, ~verify, content: string): outcome => {
  let wrapper = Web.TutorialText.parse_sections(content).wrapper;
  let tagged = tagged_lines(content);
  let section_lines = (keep: Web.TutorialText.marker => bool) =>
    List.filter_map(((m, l)) => keep(m) ? Some(l) : None, tagged);
  let code_like: Web.TutorialText.marker => bool =
    fun
    | Code
    | Test
    | Solution => true
    | _ => false;
  let sections =
    List.filter_map(
      ((label, m)) =>
        Option.map(
          ((body, to_file)) => (label, body, to_file),
          text_of_lines(section_lines(Web.TutorialText.equal_marker(m))),
        ),
      [
        ("@code", Web.TutorialText.Code),
        ("@test", Test),
        ("@solution", Solution),
      ],
    );
  let (code_parts, unparsed) = parse_parts(sections);
  let (fence_parsed, unparsed_fences) =
    parse_parts(fence_parts(section_lines(m => !code_like(m))));
  let term = label =>
    List.find_map(
      p => p.label == label ? Some(term_of(p.zipper)) : None,
      code_parts,
    );
  let tests =
    Option.value(
      term("@test"),
      ~default=term_of(Option.get(parse("test true end"))),
    );
  let runs =
    List.filter_map(
      code => Option.map(c => stitch(~wrapper, c, tests), term(code)),
      ["@code", "@solution"],
    )
    @ List.map(p => term_of(p.zipper), fence_parsed);
  let o =
    rewrite(
      ~renames,
      ~text=content,
      ~unparsed=unparsed @ unparsed_fences,
      ~parts=code_parts @ fence_parsed,
      ~runs,
    );
  let behaviors = content => {
    let s = Web.TutorialText.parse_sections(content);
    let term = text =>
      Option.map(term_of, parse(Stdlib.String.trim(text)))
      |> Option.value(~default=EmptyHole |> Exp.fresh);
    let tests = term(s.test == "" ? "test true end" : s.test);
    let prose =
      List.filter_map(
        ((m, l)) => code_like(m) ? None : Some(l),
        tagged_lines(content),
      );
    List.map(
      code => behavior(stitch(~wrapper, term(code), tests)),
      [s.code, ...Option.to_list(s.solution)],
    )
    @ List.map(
        ((_, body, _)) => behavior(term(body)),
        fence_parts(prose),
      )
    |> Stdlib.String.concat("\n--\n");
  };
  verified(~verify, ~before=content, o, behaviors);
};

/* A markdown file rewrites its Hazel fences, each a program */
let markdown = (~renames, ~verify, content: string): outcome => {
  let (parts, unparsed) = parse_parts(fence_parts(lines_of(content)));
  let o =
    rewrite(
      ~renames,
      ~text=content,
      ~unparsed,
      ~parts,
      ~runs=List.map(p => term_of(p.zipper), parts),
    );
  let behaviors = content =>
    lines_of(content)
    |> fence_parts
    |> List.map(((_, body, _)) =>
         Option.map(z => behavior(term_of(z)), parse(body))
         |> Option.value(~default="does not parse")
       )
    |> Stdlib.String.concat("\n--\n");
  verified(~verify, ~before=content, o, behaviors);
};

let report = (path: string, o: outcome): unit => {
  prerr_endline(
    Printf.sprintf(
      "%s: %d rewrite%s%s",
      path,
      List.length(o.edits),
      List.length(o.edits) == 1 ? "" : "s",
      o.changed == None ? "" : ", BEHAVIOR CHANGED",
    ),
  );
  List.iter(p => prerr_endline("  problem: " ++ p), o.problems);
  switch (o.changed) {
  | Some((before, after)) =>
    prerr_endline("  before:\n" ++ before ++ "\n  after:\n" ++ after)
  | None => ()
  };
};

let read = path =>
  path == "-"
    ? In_channel.input_all(stdin)
    : In_channel.with_open_bin(path, In_channel.input_all);

/* `hazel migrate --renames T [--write] [--verify] FILE...`: a .hzt file
   is a lesson, anything else one program; `-` reads stdin and prints the
   rewrite. With --json, each FILE holds a JSON list of programs, and a
   list of {text, edits, problems} goes to stdout or -o. */
let run =
    (
      renames_path: string,
      write: bool,
      verify: bool,
      json: bool,
      output: option(string),
      paths: list(string),
    )
    : unit => {
  let renames = renames_of_text(read(renames_path));
  List.iter(
    path =>
      if (json) {
        let programs =
          switch (Yojson.Safe.from_string(read(path))) {
          | `List(items) =>
            List.map(
              fun
              | `String(s) => s
              | _ => failwith("expected a JSON list of strings"),
              items,
            )
          | _ => failwith("expected a JSON list of strings")
          };
        let result = text => {
          let o =
            try(program(~renames, ~verify, text)) {
            | e => {
                text,
                edits: [],
                problems: ["raised " ++ Printexc.to_string(e)],
                changed: None,
              }
            };
          let changed =
            switch (o.changed) {
            | Some((b, a)) => ["BEHAVIOR CHANGED\n" ++ b ++ "\n=>\n" ++ a]
            | None => []
            };
          `Assoc([
            ("text", `String(o.text)),
            (
              "edits",
              `List(
                List.map(
                  e =>
                    `List([
                      `Int(e.start),
                      `String(e.old),
                      `String(e.new_),
                    ]),
                  o.edits,
                ),
              ),
            ),
            (
              "problems",
              `List(List.map(p => `String(p), o.problems @ changed)),
            ),
          ]);
        };
        let out = Yojson.Safe.to_string(`List(List.map(result, programs)));
        switch (output) {
        | Some(file) =>
          Out_channel.with_open_bin(file, oc =>
            Out_channel.output_string(oc, out)
          )
        | None => print_endline(out)
        };
      } else {
        let text = read(path);
        let o =
          try(
            if (Filename.check_suffix(path, ".hzt")) {
              lesson(~renames, ~verify, text);
            } else if (Filename.check_suffix(path, ".md")) {
              markdown(~renames, ~verify, text);
            } else {
              program(~renames, ~verify, text);
            }
          ) {
          | e => {
              text,
              edits: [],
              problems: ["raised " ++ Printexc.to_string(e)],
              changed: None,
            }
          };
        report(path, o);
        if (path == "-") {
          print_string(o.text);
        } else if (write && o.text != text) {
          Out_channel.with_open_bin(path, oc =>
            Out_channel.output_string(oc, o.text)
          );
        };
      },
    paths,
  );
};
