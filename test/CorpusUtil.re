open Haz3lcore;

/* helpers for tests over the mega corpus (hazel-programs/mega); paths
   resolve from the repo root (run_node.sh) or one level down (dune) */

let read_file = (path: string): option(string) =>
  switch (open_in_bin(path)) {
  | ic =>
    let n = in_channel_length(ic);
    let s = really_input_string(ic, n);
    close_in(ic);
    Some(s);
  | exception _ => None
  };

/* timing-only cases register only under HAZEL_BENCH=1 */
let bench_enabled = Sys.getenv_opt("HAZEL_BENCH") == Some("1");
let bench_cases = cases => bench_enabled ? cases : [];

let mega_path = (name: string): string => {
  let path = "hazel-programs/mega/" ++ name;
  Sys.file_exists(path) ? path : "../hazel-programs/mega/" ++ name;
};

let mega_src = (name: string): option(string) =>
  read_file(mega_path(name));

let parse = (~root: Sort.t=Exp, src: string): option(Segment.t) =>
  FastParse.of_text(
    ~materialize=Triggers.invoked_projector,
    ~collect_refractors=true,
    ~root,
    src,
  );

let corpus_seg = (~root: Sort.t=Exp, name: string): option(Segment.t) =>
  Option.bind(mega_src(name), parse(~root));

/* every .hz program under hazel-programs outside mega/, as loaded:
   (path, text) */
let slide_corpus = (): list((string, string)) => {
  let rec find = (dir: string): list(string) =>
    switch (Sys.readdir(dir)) {
    | entries =>
      entries
      |> Array.to_list
      |> List.concat_map(entry => {
           let path = Filename.concat(dir, entry);
           switch (Sys.is_directory(path)) {
           | true => entry == "mega" ? [] : find(path)
           | false => Filename.check_suffix(entry, ".hz") ? [path] : []
           | exception _ => []
           };
         })
    | exception _ => []
    };
  let root =
    Sys.file_exists("hazel-programs")
      ? "hazel-programs" : "../hazel-programs";
  find(root)
  |> List.sort(compare)
  |> List.filter_map(path =>
       Option.map(
         src =>
           (
             path,
             src
             |> Util.StringUtil.trim_leading
             |> Util.StringUtil.strip_final_newline,
           ),
         read_file(path),
       )
     );
};

/* mid-typing states: unfinished forms, missing operands, open delimiters */
let incomplete_programs = [
  ("empty definition", "let x = in\nx"),
  ("unfinished last def", "let a = 1 in\nlet b ="),
  ("fun without a body", "let f = fun x -> in\nf(1)"),
  ("missing operand", "let a = 1 in\nlet b = a +\nin b"),
  ("unfinished case", "let a = 1 in\ncase a\n| 1 =>"),
  ("open paren", "let t = (1,\n2 in t"),
  ("open list", "let xs = [1, 2 in xs"),
  ("empty type", "type t = in 1"),
  ("bare keyword", "let a = 1 in\nlet"),
  ("unfinished member", "module M = {\n  let x = 1;\n  let y =\n} in M.x"),
  ("if without else", "let a = 1 in\nif a == 1 then 2"),
  ("def without body", "let a = 1 in\nlet b = 2"),
  /* body holes under use/theorem, or off the binding-form spine */
  ("use", "use X in\nzz"),
  ("theorem", "theorem t = 1 in\nzz"),
  ("operand above a let", "let a = 1 in\nx\nlet b = 2 in\nzz"),
  ("let under fun", "fun x ->\nlet y = x in\nzz"),
  ("let under else", "if true then 1 else\nlet y = 2 in\ny"),
  ("let under operator", "1 +\nlet y = 2 in\ny"),
];

/* as if typed: the text parser keeps unfinished forms and grout */
let typed_seg = (~root: Sort.t=Exp, src: string): option(Segment.t) =>
  Option.map(Zipper.unselect_and_zip, MarkerParse.of_text(~root, src));

/* retoken every [needle] tile to [repl] (fresh id), leaving untouched
   pieces physically shared as a real edit does; returns whether found */
let rec edit_token =
        (~needle: string, ~repl: string, seg: Segment.t): (Segment.t, bool) => {
  let piece = (p: Piece.t): (Piece.t, bool) =>
    switch (p) {
    | Tile(t) when Tile.label(t) == [needle] => (
        Tile({
          ...t,
          id: Id.mk(),
          form: Form.Tok(repl),
        }),
        true,
      )
    | Tile(t) =>
      let (children, changed) =
        List.fold_right(
          (seg, (segs, ch)) => {
            let (seg', ch') = edit_token(~needle, ~repl, seg);
            ([seg', ...segs], ch || ch');
          },
          t.children,
          ([], false),
        );
      changed
        ? (
          Tile({
            ...t,
            children,
          }),
          true,
        )
        : (p, false);
    | p => (p, false)
    };
  let (pieces, changed) =
    List.fold_right(
      (p, (ps, ch)) => {
        let (p', ch') = piece(p);
        ([p', ...ps], ch || ch');
      },
      seg,
      ([], false),
    );
  changed ? (pieces, true) : (seg, false);
};

let sorted_ids = (ids: list(Id.t)): list(string) =>
  List.sort_uniq(compare, List.map(Id.to_string, ids));

/* mega-scale slides skip the super-linear gates (typing parse, roundtrip);
   substring match, as names carry folder prefixes ("Perf / Mega 1k") */
let mega_scale = (name: string): bool => {
  let sub = "Mega";
  let (nl, sl) = (String.length(name), String.length(sub));
  let rec go = i =>
    i + sl <= nl && (String.sub(name, i, sl) == sub || go(i + 1));
  go(0);
};
